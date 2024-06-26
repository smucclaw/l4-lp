(ns l4-lp.swipl.js.common.swipl-js-to-clj 
  (:require [applied-science.js-interop :as jsi]
            [cljs-bean.core :as bean]
            [l4-lp.syntax.symbol-db :as symbol-db]
            [meander.epsilon :as m]
            [meander.strategy.epsilon :as r]
            [net.cgrand.xforms :as xforms]
            [tupelo.string :as str]))

;; https://swi-prolog.discourse.group/t/clojure-clojurescript-with-swi-prolog/5399/4

(def ^:private swipl-data->clj
  "Transforms Javascript data output by the SWIPL wasm interpreter into Clj
   data.

   The format of the Javascript data is described here:
   https://swi-prolog.discourse.group/t/swi-prolog-in-the-browser-using-wasm/5650#calling-between-javascript-and-prolog-5"
  (r/top-down
   (r/rewrite
    #js {:$t "s" :v (m/some ?str)} ?str
    #js {:$t "v" :v (m/some ?var-id)} ~(symbol "var" ?var-id)

    #js [!xs ...] [!xs ...]

    #js {:$t "t"
         :functor ":"
         ":" #js [_ (m/cata ?term)]}
    ?term

    (m/and #js {:$t "t" :functor (m/some ?functor)}
           (m/app #(jsi/get % ?functor) #js [!args ...]))
    (~(symbol ?functor) & [!args ...])

    (m/app symbol-db/prolog-symbol->l4-symbol
           (m/some ?l4-symbol))
    ?l4-symbol

    (m/pred string? ?str) ~(symbol ?str)

    ?term ?term)))

(defn swipl-query-result->bindings
  "Given a SWIPL wasm query result, extract the bindings map and transform it
   back into Clojure data."
  [swipl-query-result]
  (let [query-result->bindings
        (r/match
         (m/app bean/bean {:$tag "bindings" :success true & ?bindings})
          ?bindings
          _ {})

        js-bindings->clj
        (r/match
         [(m/keyword ?var-id) ?swipl-data]
          [(symbol "var" (str/lower-case ?var-id))
           (swipl-data->clj ?swipl-data)])]

    (->> swipl-query-result
         query-result->bindings
         (eduction (map js-bindings->clj))
         (into {}))))

(defn swipl-stack-frame->clj
  "Given a stack frame logged by the SWIPL interpreter in wasm, transform it
   into Clojure data."
  [swipl-stack-frame]
  (m/match swipl-stack-frame
    #js {:parent_goal (m/some ?parent-goal)
         :current_goal (m/some ?current-goal)
         :port (m/some ?port)
         :recursion_depth (m/some ?recursion-depth)}
    {:parent-goal (swipl-data->clj ?parent-goal)
     :current-goal (swipl-data->clj ?current-goal)
     :port (swipl-data->clj ?port)
     :recursion-depth ?recursion-depth}))

(defn swipl-stack-trace->clj [swipl-stack-trace]
  (->> swipl-stack-trace
       (eduction (map swipl-stack-frame->clj))
       (eduction (xforms/by-key :recursion-depth (xforms/into [])))
       (into {})))

(defn swipl-stack-trace->js [swipl-stack-trace]
  (-> swipl-stack-trace
      swipl-stack-trace->clj
      bean/->js))

;; (defn swipl-stack-frame->edn-str [swipl-stack-frame]
;;   (-> swipl-stack-frame swipl-stack-frame->clj str))