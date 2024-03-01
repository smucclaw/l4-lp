(ns l4-swipl-wasm.swipl-wasm-js.swipl-js-to-clj 
  (:require [applied-science.js-interop :as jsi]
            [cljs-bean.core :as bean]
            [l4-swipl-wasm.syntax.symbol-db :as symbol-db]
            [meander.epsilon :as m]
            [meander.strategy.epsilon :as r]
            [tupelo.string :as str]))

;; https://swi-prolog.discourse.group/t/clojure-clojurescript-with-swi-prolog/5399/4
;; https://github.com/SWI-Prolog/roadmap/issues/43

(def ^:private swipl-data->clj
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

(def swipl-query-result->bindings
  (r/pipe
   (r/match
    (m/app bean/bean
           {:$tag (m/some "bindings")
            :success (m/some true)
            & ?bindings})
     ?bindings
   _ {})

   (r/repeat
    (r/rewrite
     {(m/keyword ?var-id) (m/some ?swipl-data) & ?bindings}
     {~(symbol "var" (str/lower-case ?var-id))
      ~(swipl-data->clj ?swipl-data)
      & ?bindings}))))

(def swipl-stack-frame->clj
  (r/match
   #js {:parent_goal (m/some ?parent-goal)
        :current_goal (m/some ?current-goal)
        :port (m/some ?port)
        :recursion_depth (m/some ?recursion-depth)}
    {:parent-goal (swipl-data->clj ?parent-goal)
     :current-goal (swipl-data->clj ?current-goal)
     :port (swipl-data->clj ?port)
     :recursion-depth ?recursion-depth}))