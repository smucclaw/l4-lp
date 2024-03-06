(ns l4-lp.syntax.symbol-db
  (:require [datascript.core :as ds]
            [meander.epsilon :as m]
            [meander.strategy.epsilon :as r]
            [net.cgrand.xforms :as xforms]))

(def l4-lp-symbol-pairs
  "Ordered pairs defining a binary relation between L4 and Prolog symbols,
   used as a bidirectional map for translating symbols from L4 to Prolog and
   vice versa.

   For convenience, we allow both elements in the pair to be sequences.
   In such cases, each element in the left item (resp right item) is related to
   each element of the right item.

   Internally, each pair is represented as a Datascript datom, and we
   axiomatise a binary relation called l4-lp-symbol via a Datalog rule in terms
   of these datoms to lookup and translate from L4 to Prolog and vice versa."
  #{[['IS '= '==] ['eq 'is]]
    ['AND ","]
    ['OR ";"]
    ['NOT ['not "\\+"]]
    ['< 'lt]
    [['<= '=<] ['lte '=<]]
    ['> 'gt]
    ['>= 'gte]
    ;; ['+ '+]
    ;; ['- '-]
    ;; ['* '*]
    ;; ['/ '/]
    ;; ['** '**]
    [['DAYS 'DAY] 'days]
    [['WEEKS 'WEEK] 'weeks]
    [['MONTHS 'MONTH] 'months]
    [['YEARS 'YEAR] 'years]
    ['SUM ['sum_list_ 'sum_list]]
    ['PRODUCT 'product_list]
    ['MIN ['min_list_ 'min_list]]
    ['MAX ['max_list_ 'max_list]]})

(def ^:private symbol-db-conn
  (ds/create-conn))

;; Populate the database with the ordered pairs in l4-lp-symbol-pairs
;; so that they can be used to derive l4-lp-symbol.
(->> l4-lp-symbol-pairs
     (eduction
      (xforms/for
       [[l4-symbols prolog-symbols] %
        :let [->coll-of-symbols
              (r/match
               (m/pred coll? ?coll) (mapv symbol ?coll)
               ?x [(symbol ?x)])]

        l4-symbol (->coll-of-symbols l4-symbols)
        prolog-symbol (->coll-of-symbols prolog-symbols)]

        {:l4-symbol l4-symbol :prolog-symbol prolog-symbol}))
     (ds/transact! symbol-db-conn))

(def ^:private l4-lp-symbol-rule
  "Datalog rule encoding the l4-lp-symbol relation:
     ∀ ?l4-symbol ?prolog-symbol,
       l4-lp-symbol(?l4-symbol, ?prolog-symbol) ←
         ∃ ?entity,
           rdf(?entity, l4-symbol, ?l4-symbol) ∧
           rdf(?entity, prolog-symbol, ?prolog-symbol)"
  '[[(l4-lp-symbol ?l4-symbol ?prolog-symbol)
     [?entity :l4-symbol ?l4-symbol]
     [?entity :prolog-symbol ?prolog-symbol]]])

(defn is-l4-symbol?
  "Given an input x, check if x is a valid L4 symbol."
  [x]
  (->> x
       (ds/q '[:find ?l4-symbol .
               :in $ ?l4-symbol
               :where [_ :l4-symbol ?l4-symbol]]
             @symbol-db-conn)
       some?))

(defn l4-symbol->prolog-symbol
  "Given a L4 symbol, returns an appropriate symbol representing a Prolog
   functor.

   Technically, this function is the result of transforming the l4-lp-symbol
   relation into a function in its first argument via a Choice function that
   picks the first ?prolog-symbol such that the following Datalog query
   succeeds:
     ⊢ ∃ ?l4-symbol, l4-lp-symbol(?l4-symbol, ?prolog-symbol)."
  [l4-symbol]
  (ds/q '[:find ?prolog-symbol .
          :in $ % ?l4-symbol
          :where (l4-lp-symbol ?l4-symbol ?prolog-symbol)]
        @symbol-db-conn
        l4-lp-symbol-rule
        l4-symbol))

(defn prolog-symbol->l4-symbol
  "Given a symbol representing a Prolog functor, returns an appropriate L4
   symbol."
  [prolog-symbol]
  (ds/q '[:find ?l4-symbol .
          :in $ % ?prolog-symbol
          :where (l4-lp-symbol ?l4-symbol ?prolog-symbol)]
        @symbol-db-conn
        l4-lp-symbol-rule
        prolog-symbol))