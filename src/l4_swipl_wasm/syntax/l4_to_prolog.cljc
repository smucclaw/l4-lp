(ns l4-swipl-wasm.syntax.l4-to-prolog 
  (:require [clojure.edn :as edn]
            [l4-swipl-wasm.syntax.symbol-db :as symbol-db]
            [meander.epsilon :as m]
            [meander.strategy.epsilon :as r]
            [tupelo.core :refer [it->]]
            [tupelo.string :as str]))

(def ^:private l4-mixfix->prolog-prefix
  "Parses and transforms L4 mixfix predicate application into prefix form in
   Prolog."
  (r/pipe
   (r/rewrite
   ;; Partition all the elements into args and non-args.
   ;; Non-args are later mashed together into an atom representing the predicate.
    (m/app #(group-by symbol-db/is-l4-symbol? %)
           {true (m/some (m/and [_] ?non-arg))
            false (m/some ?args)})
    {:non-args ?non-arg :args ?args}

    ((m/or (m/and (m/or (m/symbol "_") (m/symbol "var" _)
                        (m/pred number?)
                        (m/pred seqable?))
                  !args)
           !non-args)
     ..1)
    {:non-args [!non-args ...] :args [!args ...]})

   ;; Convert the non-args into a valid atom representing the predicate.
   (r/match
    {:non-args [?non-arg] :args ?args}
     {:pred (symbol ?non-arg) :args ?args}

     {:non-args ?non-args :args ?args}
     {:pred (it-> ?non-args (str/join "_" it) (str "'" it "'") (symbol it))
      :args ?args})

   ;; Convert the predicate and list of arguments to prefix form.
   (r/rewrite
    {:pred ?pred :args []} ?pred
    {:pred ?pred :args [!args ... ?arg]}
    (?pred ~(symbol "(") & (!args ~(symbol ",") ... ?arg) ~(symbol ")")))))

(def ^:private l4-ast->prolog-ast
  "This function transforms the AST of an individual L4 rule or goal to the
   Prolog AST.
   
   Semantically its behaviour is axiomatised via an equational theory whose
   primary construct is the interpretation function ⟦.⟧ which maps the L4 term
   algebra to that of Prolog.

   For the implementation, we:
   1. Define a term rewriting system (TRS) that orients the equational theory
      from left to right.
   2. Traverse nodes in the L4 AST in a top-down manner, using the TRS to
      rewrite and transform each node."
  (r/top-down
   (r/rewrite
    ;; --------------------------------------------------
    ;; ⟦DECIDE ?head₀ ... ?headₘ IF ?body₀ ... ?bodyₙ⟧ =
    ;;   ⟦(:- (?head₀ ... ?headₘ) (?body₀ ... ?bodyₙ))⟧
    (DECIDE . !head ..1 IF . !body ..1)
    ((~(symbol ":-") (!head ...) (!body ...)))

    ;; -------------------------------------------------
    ;; ⟦DECIDE ?head₀ ... ?headₙ⟧ = ⟦(?head₀ ... ?headₙ)⟧
    (DECIDE . !head ..1) ((!head ...))

      ;;  ?op ∈ math-list-ops     ?lhsᵢ ∉ math-list-ops
      ;; ---------------------------------------------------
      ;; ⟦(?lhs₀ ... lhsₘ IS THE ?op OF ?rhs₀ ... ?rhsₙ)⟧ =
      ;;   ⟦(?op (?rhs₀ ... ?rhsₘ) (?lhs₀ ... ?lhsₙ))⟧
    (. !lhs ..1 IS THE (m/pred #{'MIN 'MAX 'PRODUCT 'SUM} ?op) OF . !rhs ..1)
    ((?op (!rhs ...) (!lhs ...)))

      ;;  ?op ∈ math-list-ops     ?lhsᵢ ∉ math-list-ops
      ;; -----------------------------------------------------
      ;; ⟦(?xs IS THE LIST OF ALL ?x SUCH THAT ?φ₀ ... ?φₙ)⟧ =
      ;;   ⟦(findall ?x (?φ₀ ... ?φₙ) ?xs)⟧
    (?xs IS THE LIST OF ALL ?x SUCH THAT & ?φ)
    ((findall ?x ?φ ?xs))

    ;; Auxiliary stuff for parsing predicate applications that are presented
    ;; in mixfix form.
    ;; TODO: Formalise the semantics of this operation.
    (m/and
     ;; Restrict mixfix parsing to seqs where there is > 1 item present,
     ;; because otherwise there is no need for this.
     (_ _ & _)
     ;; The next 2 clauses restricts mixfix parsing to ignore BoolStructs,
     ;; ie things like (... AND ... AND ...).
     (m/gather (m/pred #{'AND 'OR}) ?count) (m/guard (zero? ?count))
     ?predicate-application)
    ~(l4-mixfix->prolog-prefix ?predicate-application)

    ;; ---------------------------------------
    ;;  ⟦[?x₀ ... ?xₙ]⟧ = [⟦?x₀⟧ , ... , ⟦?xₙ⟧]
    [!xs ... !x] [!xs ~(symbol ",") ... !x]

    ;; ?var-name = (symbol "var" ?var-name')
    ;; -----------------------------------------------------
    ;;   ⟦?var-name⟧ = (symbol (str/capitalize ?var-name'))
    (m/symbol "var" ?var-name) ~(-> ?var-name str/capitalize symbol)

    ;; --------------------------------------------------------
    ;;   ⟦?l4-symbol⟧ = l4-symbol->prolog-symbol(?prolog-symbol)
    (m/app symbol-db/l4-symbol->prolog-symbol (m/some ?prolog-symbol))
    ?prolog-symbol

      ;; ?x ∈ atom ∪ ℝ ∪ string
      ;; -----------------------
      ;;       ⟦?x⟧ = ?x
    ?x ?x)))

(def ^:private ->l4-ast
  (r/match
   (m/or (m/pred string? (m/app edn/read-string ?l4-program-ast))
         ?l4-program-ast)
    ?l4-program-ast))

(defn- remove-all-spaces [s]
  (str/replace s #" " ""))

(defn l4->prolog-str [l4-goal]
  (-> l4-goal ->l4-ast l4-ast->prolog-ast str remove-all-spaces))

(defn l4-program->prolog-program-str [l4-program]
  (it-> l4-program
        (->l4-ast it)
        (eduction (map l4-ast->prolog-ast) it)
        (eduction (interpose ".\n") it)
        (apply str it)
        (str it ".")
        (remove-all-spaces it)))