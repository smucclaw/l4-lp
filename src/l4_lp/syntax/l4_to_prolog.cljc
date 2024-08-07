(ns l4-lp.syntax.l4-to-prolog
  (:require #?(:cljs [cljs-bean.core :as bean])
            [clojure.edn :as edn]
            [l4-lp.syntax.mixfix-parser :refer [l4-mixfix->prolog-prefix]]
            [l4-lp.syntax.symbol-db :as symbol-db]
            [meander.epsilon :as m]
            [meander.strategy.epsilon :as r]
            [tupelo.string :as str]))

(defn- l4->parse-tree
  "Parses strings representing L4 programs into a parse tree."
  [l4-program]
  (let [parens-if-needed
        (r/match
         (m/re #"^\(.*\)$" ?edn-str-with-parens) ?edn-str-with-parens
         ?edn-str (str "(" ?edn-str ")"))]

    (if (string? l4-program)
      (-> l4-program str/trim parens-if-needed edn/read-string)
      l4-program)))

(def ^:private l4-parse-tree->parse-tree-of-rules
  "Identify rules (ie Horn clauses) in the raw parse tree and restructure the
   tree so that each rule is grouped into its own subtree."
  (r/rewrite
   (m/with
    [%horn-clause ((m/pred '#{DECIDE QUERY}) _ & _)
     %rule (m/and (m/or ((m/pred '#{GIVEN GIVETH}) . _ ..1 & %horn-clause)
                        %horn-clause)
                  !rules)
     %rules (m/or (& %rule & %rules) (%rule & %rules) (& %rule) (%rule))]
    %rules)

   (!rules ...)))

(def ^:private l4->parse-tree-of-rules
  "Parses an L4 program into a parse tree, with each subtree of the root node
   representing the parse tree of an L4 rule (which represents a Horn clause)."
  (r/pipe l4->parse-tree l4-parse-tree->parse-tree-of-rules))

(def ^:private time-units
  (let [singular-time-units '#{DAY WEEK MONTH YEAR}

        unit->unit+plural
        (r/match
         (m/and (m/symbol ?time-unit-str) ?time-unit)
          [?time-unit (symbol (str ?time-unit-str "S"))])]

    (->> singular-time-units
         (eduction (mapcat unit->unit+plural))
         (into #{}))))

(def ^:private l4-rule->prolog-rule
  "Transpiles and desugars the parse tree of an L4 rule (which represents a
   Horn clause) into (an S-exp representation of) Prolog / Datalog, which we
   use as our intermediate AST.

   Formally, this is specified as an equational theory axiomatising a
   interpretation function ⟦.⟧ mapping from the L4 term algebra to that of
   Prolog.

   For the implementation, we:
   1. Define a term rewriting system (TRS) that orients the equational theory
      from left to right.

      Each rewrite rule in the TRS defined here is accompanied by a comment
      axiomatisating it as an equation in the equational theory.

   2. Traverse the L4 rule (viewed as a tree) in a top-down manner, using the
      TRS to rewrite and transform each node.

   Note that:
   - Our denotational semantics relies on:
     - A standard big step semantics with first-class continuations.
       This is used to axiomatise some of our equations / rewrite rules which
       manipulate nested terms and their contexts (captured as continuations),
       like the rules which recursively:
       - traverse the head and body of an L4 rule to identify symbols which
         appear in the GIVEN and GIVETH clauses, so that they can be labelled as
         variables.
       - flatten nested function applications into a conjunction of Prolog terms.

     - Standard Datalog semantics.
       This is used to axiomatise a rewrite rule that transpiles L4 symbols to
       Prolog ones by looking up in an embedded Datalog database.
       (Datalog is used here because we want a bidirectional map that can convert
       Prolog symbols back into L4 when we post-process results from Prolog engines.)

   - The resulting S-exp AST is also valid Prolog code because each S-exp
     in the AST is also a Prolog M-exp. For instance, an S-exp of the form
     ( p '( x₀ ', ... ', xₙ ') ) is used to represent the Prolog term
     p(x₀, ..., xₙ)

   - Prolog is a universal input format for Horn clause solvers like
     Z3, Prolog, Datalog and ASP, so that the resulting S-exp AST can be
     readily fed into such tools for static analysis and execution."
  (r/top-down
   (r/rewrite
    ;; -----------------------------------------------
    ;; ⟦(GIVETH ?x₀ ... ?xₙ)⟧ = ⟦((GIVEN ?x₀ ... ?xₙ))⟧
    (GIVETH . !xs ..1) ((GIVEN & (!xs ...)))

    ;; --------------------------------------------
    ;; ⟦(?x₀ ... ?xₙ OTHERWISE)⟧ = ⟦((?x₀ ... ?xₙ))⟧
    (!xs ..1 OTHERWISE) ((!xs ...))

    ;; TODO: Document semantics.
    (GIVEN
     . (m/with [%var (m/symbol nil !vars)]
               (m/or (m/pred '#{GIVEN GIVETH}) %var (m/seqable %var & _)))
     ..1
     (m/pred #{'DECIDE 'QUERY} ?decide-query) & ?horn-clause)
    ((GIVEN #{^& (!vars ...)} ?decide-query & ?horn-clause))

    ;; ⊢ ?symbol ∈ ?givens
    ;; ⊢ (symbol nil ?symbol) ⇓ ?symbol'
    ;; ⊢ (symbol "var" ?symbol) ⇓ ?var
    ;; (?C, λx. throw (cont C) x) ⊢ (?C ?var) ⇓ ?e
    ;; ----------------------------------------------------------------------
    ;; ⟦(GIVEN ?givens ... C[?symbol'] ...)⟧ = ⟦((GIVEN ?givens ... ?e ...))⟧
    ;;
    ;; Here, C[.] denotes contexts defined in the obvious way, ie:
    ;;   C ::= [.] | (C ... C) | [C ... C] | #{C ... C} | {C C,..., C C}
    (GIVEN (m/and #{?symbol ^& _} ?givens) & (m/$ ?C (m/symbol nil ?symbol)))
    ((GIVEN ?givens ~(?C (symbol "var" ?symbol))))

    (GIVEN _ & ?horn-clause) ?horn-clause

    ;; ?op ∈ {IF WHEN WHERE}
    ;; ---------------------------------------------------
    ;; ⟦(DECIDE ?head₀ ... ?headₘ ?op ?body₀ ... ?bodyₙ)⟧ =
    ;;   ⟦(:- (?head₀ ... ?headₘ) (?body₀ ... ?bodyₙ))⟧
    (DECIDE . !head ..1 (m/pred '#{IF WHEN WHERE}) . !body ..1)
    ((~(symbol ":-") (!head ...) (!body ...)))

    ;; -------------------------------------------------
    ;; ⟦(DECIDE ?head₀ ... ?headₙ)⟧ = ⟦(?head₀ ... ?headₙ)⟧
    (DECIDE . !head ..1) ((!head ...))

    (QUERY . !query ..1) (QUERY ((!query ...)))

    ;; TODO: Formalise BoolStruct parser + transpiler.
    (m/with [%bool-op (m/pred '#{AND OR UNLESS} !bool-op)
             %conjunct-disjunct (!xs ..1 %bool-op)
             %xs (m/or (& %conjunct-disjunct & %xs) %conjunct-disjunct)]
            (& %xs . !x ..1))

    ((!xs ...) !bool-op ... (!x ...))

    ;; ?op ∈ {MIN MAX PRODUCT SUM}
    ;; ?comparison ∈ {IS EQUALS = == < <= =< > >=}
    ;; ⊢ symbol? ?arg ∨ ∀ x ∈ ?arg, symbol? x ∨ number? x
    ;; ?var is a fresh variable
    ;; (?C, λx. throw (cont C) x) ⊢ (?C ?var) ⇓ ?rhs
    ;; -----------------------------------------------------------------------------------
    ;; ⟦(?lhs ?comparison C[(?op ?arg)]⟧ = ⟦((?op ?arg ?var) AND (?lhs ?comparison ?rhs))⟧

    ;; ?op ∈ {MIN MAX PRODUCT SUM}
    ;; ?comparison ∈ {IS EQUALS = == < <= =< > >=}
    ;; ⊢ symbol? ?arg ∨ ∀ x ∈ ?arg, symbol? x ∨ number? x
    ;; ?var is a fresh variable
    ;; (?C, λx. throw (cont C) x) ⊢ (?C ?var) ⇓ ?lhs
    ;; -----------------------------------------------------------------------------------
    ;; ⟦(C[(?op ?arg)] ?comparison ?rhs⟧ = ⟦((?op ?arg ?var) AND (?lhs ?comparison ?rhs))⟧
    (m/let [?coll-of-symbols-and-nums
            #(every? (some-fn symbol? number?) %)

            ?vec-of-symbols-and-nums
            (every-pred vector? ?coll-of-symbols-and-nums)

            ?fresh-var (delay (->> (gensym "var__") str (symbol "var")))]
      (m/with
       [%has-nested-arithmetic-expr
        (m/$ ?C ((m/pred '#{MIN MAX PRODUCT SUM MINUS DIVIDE} ?op)
                 & (m/or
                    ((m/or (m/and (m/symbol _) ?arg)
                           (m/pred ?vec-of-symbols-and-nums ?arg)))
                    (m/pred ?coll-of-symbols-and-nums
                            (m/app #(into [] %) ?arg)))))
        %comparison
        (m/pred '#{IS EQUALS = == < <= =< > >=} ?comparison)]

       (m/or  (m/and (& %has-nested-arithmetic-expr %comparison & ?rhs)
                     (m/let [?lhs (?C @?fresh-var)]))
              (m/and (& ?lhs %comparison & %has-nested-arithmetic-expr)
                     (m/let [?rhs (?C @?fresh-var)])))))

    ((?op ?arg ~(deref ?fresh-var)) AND (?lhs ?comparison ?rhs))

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

    ;; ------------------------------------------------------------------
    ;; ⟦(NOT ?element₀ ... ?elementₙ)⟧ = ⟦(NOT (?element₀ ... ?elementₙ))⟧
    (NOT . !xs ..1) (NOT (!xs ...))

    ;;  ∀ 0 ≤ i ≤ n, ?dateᵢ ≠ IS
    ;; ----------------------------------------
    ;; ⟦(?date₀ ... ?dateₙ IS A VALID DATE)⟧ =
    ;;   ⟦(is_valid_date (?date₀ ... ?dateₘ))⟧
    (. !date ..1 IS A VALID DATE)
    ((is_valid_date (!date ...)))

    ;;  ?number ∈ ℕ
    ;;  ?time-unit ∈ {DAY DAYS WEEK WEEKS MONTH MONTHS YEAR YEARS}
    ;; -----------------------------------------------------------
    ;; ⟦(?date₀ + ?number ?time-unit IS ?date₁)⟧ =
    ;;   ⟦(date_add_duration ?date₀ (?time-unit ?number) ?date₁)⟧

    ;;  ?number ∈ ℕ
    ;;  ?time-unit ∈ {DAY DAYS WEEK WEEKS MONTH MONTHS YEAR YEARS}
    ;; -----------------------------------------------------------
    ;; ⟦(?date₀ - ?number ?time-unit IS ?date₁)⟧ =
    ;;   ⟦(date_minus_duration ?date₀ (?time-unit ?number) ?date₁)⟧
    (. ?date-0
       (m/or (m/and + (m/let [?pred 'date_add_duration]))
             (m/and - (m/let [?pred 'date_minus_duration])))
       ?number (m/pred ~time-units ?time-unit) IS ?date-1)
    ((?pred ?date-0 (?time-unit ?number) ?date-1))

    ;;  ∀ 0 ≤ i ≤ m - 1, ?dateᵢ ≠ IS ∧ ?dateᵢ₊₁ ≠ WITHIN
    ;;  ∀ 0 ≤ j ≤ n, ?numberⱼ ∉ {DAY DAYS WEEK WEEKS MONTH MONTHS YEAR YEARS} 
    ;;  ?unit ∈ {DAY DAYS WEEK WEEKS MONTH MONTHS YEAR YEARS}
    ;; ------------------------------------------------------------------------------------
    ;; ⟦(?date₀ ... ?dateₘ IS WITHIN ?number₀ ... ?numberₙ ?unit OF ?date'₀ ... ?date'ᵣ)⟧ =
    ;;   ⟦(date_is_within_duration_of_date
    ;;     (?date₀ ... ?dateₘ) (?number₀ ... ?numberₙ) (?date'₀ ... ?date'ᵣ))⟧
    (. !date-0 ..1 IS WITHIN . !number ..1
       (m/pred ~time-units ?time-unit) OF . !date-1 ..1)
    ((date_is_within_duration_of_date
      (!date-0 ...) (?time-unit (!number ...)) (!date-1 ...)))

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
    ;;
    ;; We restrict mixfix parsing to seqs where there is > 1 item present,
    ;; because otherwise there is no need for this.
    (_ _ & _ :as ?pred-app) ~(l4-mixfix->prolog-prefix ?pred-app)

    ;; --------------------------------------
    ;;  ⟦[?x₀ ... ?xₙ]⟧ = [⟦?x₀⟧ , ... , ⟦?xₙ⟧]
    [!xs ... ?x] [!xs ~(symbol ",") ... ?x]

    ;; ⊢ (symbol "var" ?var-name') ⇓ ?var-name
    ;; ⊢ (symbol (capitalize ?var-name')) ⇓ ?var-name''
    ;; ---------------------------------------------------
    ;;  ⟦?var-name⟧ = ?var-name''
    (m/symbol "var" ?var-name) ~(-> ?var-name str/capitalize symbol)

    ;;  ⊢ is_l4_symbol? ?x
    ;;  ⊢ (l4-symbol->prolog-symbol ?x) ⇓ ?prolog-symbol
    ;; --------------------------------------------------
    ;;  ⟦?x⟧ = ?prolog-symbol
    (m/app symbol-db/l4-symbol->prolog-symbol (m/some ?prolog-symbol))
    ?prolog-symbol

    ;;  ⊢ atomic ?x ∧ ¬ is_l4_symbol? ?x
    ;; ----------------------------------
    ;;           ⟦?x⟧ = ?x
    ?x ?x)))

(defn- remove-all-spaces [s]
  (str/replace s #" " ""))

(defn l4->prolog-program+queries
  "Given an L4 program, parses and transforms it into a map where:
   - :queries is a vector of Prolog queries (as strings).
   - :program is the Prolog program (as a string).

   The input can either be an EDN string or Clojure data."
  [l4-program]
  (let [prolog-rules->prolog-program-rules+queries
        (r/rewrite
         (m/seqable (m/or (m/$ (QUERY & !queries)) !rules) ...)
         {:queries (!queries ...)
          :program-rules (!rules ...)})

        prolog-rules->prolog-str
        (fn [ending-str prolog-rules]
          (->> prolog-rules
               (eduction (mapcat (fn [prolog-rule]
                                   [(into () prolog-rule) ending-str])))
               (apply str)
               remove-all-spaces))

        prolog-program-rules+queries->prolog-program+queries-str
        (r/match
         {:queries ?queries :program-rules ?program-rules}
          {:queries (->> ?queries (mapv #(prolog-rules->prolog-str "" %)))
           :program (->> ?program-rules (prolog-rules->prolog-str ".\n"))})]

    (->> l4-program
         l4->parse-tree-of-rules
         (eduction (map l4-rule->prolog-rule))
         prolog-rules->prolog-program-rules+queries
         prolog-program-rules+queries->prolog-program+queries-str)))

#?(:cljs
   (defn l4->prolog-program+queries-js [l4-program]
     (-> l4-program
         l4->prolog-program+queries
         bean/->js))
   :clj
   (defn l4->prolog-program+queries-java [l4-program]
     (-> l4-program
         l4->prolog-program+queries
         java.util.Collections/unmodifiableMap)))