(ns l4-lp.syntax.mixfix-parser 
  (:require [l4-lp.syntax.symbol-db :as symbol-db]
            [meander.epsilon :as m]
            [meander.strategy.epsilon :as r]
            [tupelo.core :refer [it->]]
            [tupelo.string :as str]))

(def ^:private partition-args-non-args
  "Partition all the elements into args and non-args.
   Non-args will be mashed together into an atom representing the predicate."
  (let [is-var-symbol? #(and (symbol? %) (= (namespace %) "var"))
        is-arg? (some-fn number? coll? is-var-symbol?
                         symbol-db/is-wildcard-symbol?)]
    (r/rewrite
     (m/app #(group-by symbol-db/is-l4-symbol? %)
            {true (m/some [?l4-symbol])
             false (m/some ?args)})
     {:l4-symbol ?l4-symbol :args ?args}

     ((m/or (m/pred is-arg? !args) !non-args) ..1)
     {:non-args [!non-args ...] :args [!args ...]})))

(def ^:private non-args+args->pred-atom+args
   "Convert the non-args into a valid atom representing the predicate."
  (r/match
   {:l4-symbol (m/some ?l4-symbol) :args ?args}
    {:pred ?l4-symbol :args ?args}

    {:non-args (m/some ?non-args) :args ?args}
    {:pred (it-> ?non-args
                 (str/join "_" it)
                 (str/replace it "'" "\\'")
                 (str "'" it "'")
                 (symbol it))
     :args ?args}))

(def ^:private pred-atom+args->prolog-prefix-ast
  "Convert the predicate and sequence of arguments to prefix form."
  (r/rewrite
   {:pred ?pred :args []} ?pred

   {:pred ?pred :args [!args ... ?arg]}
   (?pred ~(symbol "(") & (!args ~(symbol ",") ... ?arg) ~(symbol ")"))))

(def l4-mixfix->prolog-prefix
  "Parses and transforms L4 mixfix predicate application into prefix form in
   Prolog."
  (r/pipe partition-args-non-args
          non-args+args->pred-atom+args
          pred-atom+args->prolog-prefix-ast))