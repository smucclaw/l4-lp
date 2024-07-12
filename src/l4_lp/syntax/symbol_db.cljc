(ns l4-lp.syntax.symbol-db
  (:require [datascript.core :as ds]
            [meander.epsilon :as m]
            [meander.strategy.epsilon :as r]
            [net.cgrand.xforms :as xforms]
            [tupelo.string :as str]))

(def ^:private l4-prolog-symbol-pairs
  "Ordered pairs defining a binary relation between L4 and Prolog symbols,
   used as a bidirectional map for translating symbols from L4 to Prolog and
   vice versa.

   For convenience, we allow both elements in the pair to be sequences.
   In such cases, each element in the left item (resp right item) is related to
   each element of the right item.

   Internally, each pair is represented as a Datascript datom, and we
   axiomatise a binary relation called l4-prolog-symbol via a Datalog rule in
   terms of these datoms to lookup and translate from L4 to Prolog and vice
   versa."
  '#{[[IS EQUALS = ==] [eq is]]
     [AND ","]
     [OR ";"]
     [NOT [not_ "\\+"]]
     [UNLESS unless]
     [TRUE "true"]
     [FALSE ["false" fail]]
     [< lt]
     [[<= =<] [leq =<]]
     [> gt]
     [>= geq]
     ;; [+ +]
     ;; [- -]
     ;; [* *]
     ;; [/ /]
     ;; [** **]
     ;; https://swi-prolog.discourse.group/t/why-is-predicate-traced-when-using-trace-predicate-but-not-when-using-trace-then-predicate/3231
     ;; [findall [findall findall_loop]]
     [[DAYS DAY] days]
     [[WEEKS WEEK] weeks]
     [[MONTHS MONTH] months]
     [[YEARS YEAR] years]
     ["IS IN" is_in]
     [SUM [sum_list_ sum_list]]
     [PRODUCT product_list]
     [MINUS minus_list]
     [DIVIDE divide_list]
     [MIN [min_list_ min_list]]
     [MAX [max_list_ max_list]]})

(def ^:private symbol-db-conn
  (ds/create-conn))

;; Populate the database with the ordered pairs in l4-prolog-symbol-pairs
;; so that they can be used to derive l4-prolog-symbol.
(let [->coll-of-symbols
      (r/match
       (m/pred coll? ?coll) (mapv symbol ?coll)
       ?x [(symbol ?x)])]

  (->> l4-prolog-symbol-pairs
       (eduction
        (xforms/for [[l4-symbols prolog-symbols] %
                     l4-symbol (->coll-of-symbols l4-symbols)
                     prolog-symbol (->coll-of-symbols prolog-symbols)]
          {:l4-symbol l4-symbol :prolog-symbol prolog-symbol}))
       (ds/transact! symbol-db-conn)))

(def ^:private l4-prolog-symbol-rule
  "Datalog rule encoding the l4-prolog-symbol relation:
     ∀ ?l4-symbol ?prolog-symbol,
       l4-prolog-symbol(?l4-symbol, ?prolog-symbol) ←
         ∃ ?entity,
           rdf(?entity, l4-symbol, ?l4-symbol) ∧
           rdf(?entity, prolog-symbol, ?prolog-symbol)"
  '[[(l4-prolog-symbol ?l4-symbol ?prolog-symbol)
     [?entity :l4-symbol ?l4-symbol]
     [?entity :prolog-symbol ?prolog-symbol]]])

(defn is-wildcard-symbol? [sym]
  (and (simple-symbol? sym)
       (str/starts-with? "_" (str sym))))

(defn is-l4-symbol?
  "Checks if a symbol is a valid L4 symbol."
  [sym]
  (and (not (is-wildcard-symbol? sym))
       (some? (ds/q '[:find ?l4-symbol .
                      :in $ ?l4-symbol
                      :where [_ :l4-symbol ?l4-symbol]]
                    @symbol-db-conn
                    sym))))

(defn l4-symbol->prolog-symbol
  "Given a L4 symbol, returns an appropriate symbol representing a Prolog
   functor.

   Technically, this function is the result of transforming the l4-prolog-symbol
   relation into a function in its first argument via a Choice function that
   picks the first ?prolog-symbol such that the following Datalog query
   succeeds:
     ⊢ ∃ ?l4-symbol, l4-prolog-symbol(?l4-symbol, ?prolog-symbol)."
  [l4-symbol]
  (if (is-wildcard-symbol? l4-symbol)
    l4-symbol
    (ds/q '[:find ?prolog-symbol .
            :in $ % ?l4-symbol
            :where (l4-prolog-symbol ?l4-symbol ?prolog-symbol)]
          @symbol-db-conn
          l4-prolog-symbol-rule
          l4-symbol)))

(defn prolog-symbol->l4-symbol
  "Given a symbol representing a Prolog functor, returns an appropriate L4
   symbol."
  [prolog-symbol]
  (ds/q '[:find ?l4-symbol .
          :in $ % ?prolog-symbol
          :where (l4-prolog-symbol ?l4-symbol ?prolog-symbol)]
        @symbol-db-conn
        l4-prolog-symbol-rule
        prolog-symbol))