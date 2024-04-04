(ns l4-lp.syntax.l4-to-prolog
  (:require [clojure.edn :as edn]
            [l4-lp.syntax.mixfix-parser :refer [l4-mixfix->prolog-prefix]]
            [l4-lp.syntax.symbol-db :as symbol-db]
            [l4-lp.syntax.arithmetic :as arithmetic]
            [meander.epsilon :as m]
            [meander.strategy.epsilon :as r]
            [tupelo.core :refer [it->]]
            [tupelo.string :as str]))

(def ^:private ->l4-ast
  "Transforms EDN strings representing L4 programs into Clojure data."
  (let [parens-if-needed
        (r/match
         (m/re #"^(\(.*\)|\[.*\])$" [?edn-str]) ?edn-str
         ?edn-str (str "(" ?edn-str ")"))]
    (r/match
     (m/or (m/pred string?
                   (m/app #(-> % parens-if-needed edn/read-string)
                          ?l4-program-ast))
           ?l4-program-ast)
      ?l4-program-ast)))

(def ^:private l4-ast->seq-of-rules
  (r/rewrite
   (m/with
    [%rule (m/and (m/or (m/seqable GIVEN . _ ..1 DECIDE _ & _)
                        (m/seqable GIVETH . _ ..1 DECIDE _ & _)
                        (m/seqable DECIDE _ & _))
                  !rules)
     %rules (m/or (m/seqable & %rule & %rules)
                  (m/seqable %rule & %rules)
                  (m/seqable & %rule)
                  (m/seqable %rule))]
    %rules)
   (!rules ...)))

(def ^:private ->seq-of-rules
  (r/pipe ->l4-ast l4-ast->seq-of-rules))

(def ^:private l4-rule->prolog-rule
  "This function transforms the AST of an individual L4 rule or goal to the
   Prolog AST.

   Formally, we specify its behaviour via an interpretation function ⟦.⟧ mapping
   from the L4 term algebra to that of Prolog, which we define via recursive
   equations.

   For the implementation, we:
   1. Define a term rewriting system (TRS) that orients the equations 
      defining ⟦.⟧ from left to right.
   2. Traverse nodes in the L4 AST in a top-down manner, using the TRS to
      rewrite and transform each node."
  (r/top-down
   (r/rewrite
    (GIVETH !giveths ..1 DECIDE & ?horn-clause)
    ((GIVEN & (!giveths ...) DECIDE & ?horn-clause))

    (m/or (!xs ..1 GIVETH . !xs ..1)
          (!xs ..1 OTHERWISE))
    ((!xs ...))

    (GIVEN
     . (m/with [%givens (m/symbol nil !givens)]
               (m/or %givens (m/seqable %givens & _))) ..1
     DECIDE & ?horn-clause)
    ((GIVEN #{^& (!givens ...)} DECIDE & ?horn-clause))

    ;; ?symbol ∈ ?givens
    ;; ⊢ (symbol nil ?symbol) ⇓ ?symbol'
    ;; ⊢ (symbol "var" ?symbol) ⇓ ?var
    ;; (?C, λx. throw (cont C) x) ⊢ (?C ?var) ⇓ ?e
    ;; --------------------------------------------------------------------
    ;; ⟦(GIVEN ?givens ... C[?symbol'] ...)⟧ = ⟦(GIVEN ?givens ... ?e ...)⟧
    ;;
    ;; Here, C[.] denotes contexts defined in the obvious way, ie:
    ;;   C ::= [.] | (C ... C) | [C ... C] | #{C ... C} | {C C,..., C C}
    (GIVEN (m/and #{?symbol ^& _} ?givens) & (m/$ ?C (m/symbol nil ?symbol)))
    ((GIVEN ?givens ~(?C (symbol "var" ?symbol))))

    (GIVEN _ & ?horn-clause) ?horn-clause

    ;; --------------------------------------------------
    ;; ⟦(DECIDE ?head₀ ... ?headₘ IF ?body₀ ... ?bodyₙ)⟧ =
    ;;   ⟦(:- (?head₀ ... ?headₘ) (?body₀ ... ?bodyₙ))⟧
    (DECIDE . !head ..1 (m/pred #{'IF 'WHEN 'WHERE}) . !body ..1)
    ((~(symbol ":-") (!head ...) (!body ...)))

    ;; -------------------------------------------------
    ;; ⟦(DECIDE ?head₀ ... ?headₙ)⟧ = ⟦(?head₀ ... ?headₙ)⟧
    (DECIDE . !head ..1) ((!head ...))

    (m/with
     [%bool-op (m/pred #{'AND 'OR} !bool-op)
      %xs (m/or (m/seqable !xs ..1 %bool-op & %xs)
                (m/seqable !xs ..1 %bool-op))]
     (m/seqable & %xs . !x ..1))

    ((!xs ...) !bool-op ... (!x ...))

    ;; ?op ∈ {MIN MAX PRODUCT SUM}
    ;; ?comparison ∈ {'IS 'EQUALS '= '== '< '<= '=< '> '>=}
    ;; ⊢ symbol? ?arg ∨ ∀ x ∈ ?arg, symbol? x ∨ number? x
    ;; ?var is a fresh variable
    ;; (?C, λx. throw (cont C) x) ⊢ (?C ?var) ⇓ ?rhs
    ;; --------------------------------------------------------------------------------------
    ;; ⟦(?lhs ?comparison C[(?op ?arg)]⟧ = ((?var IS ?op OF ?arg) AND (?lhs ?comparison ?rhs))⟧

    ;; ?op ∈ {MIN MAX PRODUCT SUM}
    ;; ?comparison ∈ {'IS 'EQUALS '= '== '< '<= '=< '> '>=}
    ;; ⊢ symbol? ?arg ∨ ∀ x ∈ ?arg, symbol? x ∨ number? x
    ;; ?var is a fresh variable
    ;; (?C, λx. throw (cont C) x) ⊢ (?C ?var) ⇓ ?lhs
    ;; --------------------------------------------------------------------------------------
    ;; ⟦(C[(?op ?arg)] ?comparison ?rhs⟧ = ((?var IS ?op OF ?arg) AND (?lhs ?comparison ?rhs))⟧
    (m/let [?seq-of-symbols-and-nums
            #(every? (some-fn symbol? number?) %)

            ?vec-of-symbols-and-nums
            (every-pred vector? ?seq-of-symbols-and-nums)

            ?fresh-var (delay (gensym "Var__"))]
      (m/with
       [%has-nested-arithmetic-expr
        (m/$ ?C ((m/pred #{'MIN 'MAX 'PRODUCT 'SUM} ?op)
                 & (m/or
                    (m/seqable (m/or (m/and (m/symbol _) ?arg)
                                     (m/pred ?vec-of-symbols-and-nums ?arg)))
                    (m/pred ?seq-of-symbols-and-nums
                            (m/app #(into [] %) ?arg)))))
        %comparison
        (m/pred #{'IS 'EQUALS '= '== '< '<= '=< '> '>=} ?comparison)]

       (m/or  (m/and (& %has-nested-arithmetic-expr %comparison & ?rhs)
                     (m/let [?lhs (?C @?fresh-var)]))
              (m/and (& ?lhs %comparison & %has-nested-arithmetic-expr)
                     (m/let [?rhs (?C @?fresh-var)])))))

    ((~ @?fresh-var IS ~(symbol "ARITHMETIC-OP" ?op) ?arg)
     AND (?lhs ?comparison ?rhs))

    ;;  ∀ 0 ≤ i ≤ n, ?lhsᵢ ∉ {MIN MAX PRODUCT SUM}
    ;;  ?op ∈ {MIN MAX PRODUCT SUM}
    ;; ---------------------------------------------------
    ;; ⟦(?lhs₀ ... ?lhsₘ IS ?op OF ?rhs₀ ... ?rhsₙ)⟧ =
    ;;   ⟦(?op (?rhs₀ ... ?rhsₘ) (?lhs₀ ... ?lhsₙ))⟧
    (. !lhs ..1 IS (m/symbol "ARITHMETIC-OP" ?op) . !rhs ..1)
    ((?op (!rhs ...) (!lhs ...)))

    ;;  ∀ 0 ≤ i ≤ n - 1, ?elementᵢ ≠ IS ∧ ?elementᵢ₊₁ ≠ IN
    ;; ---------------------------------------------------------------------
    ;; ⟦(?element₀ ... ?elementₘ IS IN ?collection₀ ... ?collectionₙ)⟧ =
    ;;   ⟦(is_in (?element₀ ... ?elementₘ) (?collection₀ ... ?collectionₙ))⟧
    (. !element ..1 IS IN . !collection ..1)
    ((is_in (!element ...) (!collection ...)))

    ;;  ∀ 0 ≤ i ≤ n - 2, ?elementᵢ ≠ IS ∧ ?elementᵢ₊₁ ≠ NOT  ?elementᵢ₊₂ ≠ IN
    ;; ---------------------------------------------------------------------
    ;; ⟦(?element₀ ... ?elementₘ IS IN ?collection₀ ... ?collectionₙ)⟧ =
    ;;   ⟦(is_in (?element₀ ... ?elementₘ) (?collection₀ ... ?collectionₙ))⟧
    (. !element ..1 IS NOT IN . !collection ..1)
    (NOT ((!element ...) IS IN (!collection ...)))

    ;;  ∀ 0 ≤ i ≤ n - 1, ?elementᵢ ≠ IS ∧ ?elementᵢ₊₁ ≠ NOT
    ;;  ?collection₀ ≠ IN
    ;; ---------------------------------------------------------------------
    ;; ⟦(?element₀ ... ?elementₘ IS NOT ?collection₀ ... ?collectionₙ)⟧ =
    ;;   ⟦(NOT ((?element₀ ... ?elementₘ) IS (?collection₀ ... ?collectionₙ))⟧
    (. !lhs ..1 IS NOT . !rhs ..1)
    (NOT ((!lhs ...) IS (!rhs ...)))

    ;;  ∀ 0 ≤ i ≤ n, ?dateᵢ ≠ IS
    ;; ----------------------------------------
    ;; ⟦(?date₀ ... ?dateₙ IS A VALID DATE)⟧ =
    ;;   ⟦(is_valid_date (?date₀ ... ?dateₘ))⟧
    (. !date ..1 IS A VALID DATE)
    ((is_valid_date (!date ...)))

    ;;  ?number ∈ ℕ
    ;;  ?unit ∈ {DAY DAYS WEEK WEEKS MONTH MONTHS YEAR YEARS}
    ;; ---------------------------------------------------------
    ;; ⟦(?date₀ + ?number ?unit IS ?date₁)⟧ =
    ;;   ⟦(date_add_duration ?date₀ (?unit ?number) ?date₁)⟧

    ;;  ?number ∈ ℕ
    ;;  ?unit ∈ {DAY DAYS WEEK WEEKS MONTH MONTHS YEAR YEARS}
    ;; ---------------------------------------------------------
    ;; ⟦(?date₀ - ?number ?unit IS ?date₁)⟧ =
    ;;   ⟦(date_minus_duration ?date₀ (?unit ?number) ?date₁)⟧
    (. ?date-0
       (m/or (m/and + (m/let [?pred 'date_add_duration]))
             (m/and - (m/let [?pred 'date_minus_duration])))
       ?number
       (m/pred #{'DAY 'DAYS 'WEEK 'WEEKS 'MONTH 'MONTHS 'YEAR 'YEARS} ?unit)
       IS ?date-1)
    ((?pred ?date-0 (?unit ?number) ?date-1))

    ;;  ∀ 0 ≤ i ≤ m - 1, ?dateᵢ ≠ IS ∧ ?dateᵢ₊₁ ≠ WITHIN
    ;;  ∀ 0 ≤ j ≤ n, ?numberⱼ ∉ {DAY DAYS WEEK WEEKS MONTH MONTHS YEAR YEARS} 
    ;;  ?unit ∈ {DAY DAYS WEEK WEEKS MONTH MONTHS YEAR YEARS}
    ;; ------------------------------------------------------------------------------------
    ;; ⟦(?date₀ ... ?dateₘ IS WITHIN ?number₀ ... ?numberₙ ?unit OF ?date'₀ ... ?date'ᵣ)⟧ =
    ;;   ⟦(date_is_within_duration_of_date
    ;;     (?date₀ ... ?dateₘ) (?number₀ ... ?numberₙ) (?date'₀ ... ?date'ᵣ))⟧
    (. !date-0 ..1 IS WITHIN . !number ..1
       (m/pred #{'DAY 'DAYS 'WEEK 'WEEKS 'MONTH 'MONTHS 'YEAR 'YEARS} ?unit)
       OF . !date-1 ..1)
    ((date_is_within_duration_of_date
      (!date-0 ...) (?unit (!number ...)) (!date-1 ...)))

    ;;  ∀ 0 ≤ i ≤ m, ?yearᵢ ∉ {-}           ∀ 0 ≤ j ≤ n, ?monthⱼ ∉ {-}
    ;; -----------------------------------------------------------------------
    ;; ⟦(?year₀ ... ?yearₘ - ?month₀ ... ?monthₙ - ?day₀ ... ?dayᵣ)⟧ =
    ;;   ⟦(date (?year₀ ... ?yearₘ) (?month₀ ... ?monthₙ) (?day₀ ... ?dayᵣ))⟧
    (. !year ..1 - . !month ..1 - . !day ..1)
    ((date (!year ...) (!month ...) (!day ...)))

    ;;  ?op ∈ math-list-ops     ?lhsᵢ ∉ math-list-ops
    ;; -----------------------------------------------------
    ;; ⟦(?xs IS THE LIST OF ALL ?x SUCH THAT ?φ₀ ... ?φₙ)⟧ =
    ;;   ⟦(findall ?x (?φ₀ ... ?φₙ) ?xs)⟧
    (?xs IS THE LIST OF ALL ?x SUCH THAT & ?φ)
    ((findall ?x ?φ ?xs))

    ;; ---------------------------------------
    ;;  ⟦[?x₀ ... ?xₙ]⟧ = [⟦?x₀⟧ , ... , ⟦?xₙ⟧]
    [!xs ... !x] [!xs ~(symbol ",") ... !x]

    ;; Auxiliary stuff for parsing predicate applications that are presented
    ;; in mixfix form.
    ;; TODO: Formalise the semantics of this operation.
    (m/and
     ;; Restrict mixfix parsing to seqs where there is > 1 item present,
     ;; because otherwise there is no need for this.
     (_ _ & _)
     ;; The next 2 clauses restrict mixfix parsing to ignore BoolStructs,
     ;; ie things like (... AND ... AND ...).
     ;; (m/gather (m/pred #{'AND 'OR}) ?count) (m/guard (zero? ?count))
     ?predicate-application)
    ~(l4-mixfix->prolog-prefix ?predicate-application)

    ;; ---------------------------------------
    ;;  ⟦[?x₀ ... ?xₙ]⟧ = [⟦?x₀⟧ , ... , ⟦?xₙ⟧]
    [!xs ... ?x] [!xs ~(symbol ",") ... ?x]

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

(defn- remove-all-spaces [s]
  (str/replace s #" " ""))

(defn l4->prolog-str
  "Given an L4 rule/fact/goal, transpiles it into a string representing a Prolog
   rule/fact/goal.

   The input can either be an EDN string or Clojure data."
  [l4-goal]
  (-> l4-goal ->l4-ast l4-rule->prolog-rule list str remove-all-spaces))

(defn l4-program->prolog-program-str
  "Given an L4 program, transpiles it into a string representing a Prolog
   program.

   The input can either be an EDN string or Clojure data."
  [l4-program]
  (it-> l4-program
        (->seq-of-rules it)
        (eduction (map l4-rule->prolog-rule) it)
        (eduction (interpose ".\n") it)
        (apply str it)
        (str it ".")
        (remove-all-spaces it)))