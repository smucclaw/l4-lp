(ns l4-swipl-wasm.syntax.swipl-to-clj 
  (:require [applied-science.js-interop :as jsi]
            [l4-swipl-wasm.syntax.symbol-db :as symbol-db]
            [meander.epsilon :as m]
            [meander.strategy.epsilon :as r]))

;; https://swi-prolog.discourse.group/t/clojure-clojurescript-with-swi-prolog/5399/4
;; https://github.com/SWI-Prolog/roadmap/issues/43

(def ^:private swipl-js-data->clj
  (r/top-down
   (r/rewrite
    #js {:$t (m/some "s") :v (m/some ?str)} ?str
    #js {:$t (m/some "v") :v (m/some ?var-id)} ~(symbol "var" ?var-id)

    #js [!xs ...] [!xs ...]

    #js {:$t (m/some "t")
         :functor (m/some ":")
         ":" (m/some #js [_ (m/cata ?term)])}
    ?term

    (m/and #js {:$t (m/some "t") :functor (m/some ?functor)}
           (m/app #(jsi/get % ?functor) #js [!args ...]))
    (~(symbol ?functor) & [!args ...])

    (m/app symbol-db/prolog-symbol->l4-symbol
           (m/some ?l4-symbol))
    ?l4-symbol

    (m/pred string? ?str) ~(symbol ?str)

    ?term ?term)))