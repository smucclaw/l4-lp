(ns l4-lp.web-editor.guifier
  (:require ["https://cdn.jsdelivr.net/npm/guifier@1.0.24/dist/Guifier.js$default"
             :as Guifier]
            [applied-science.js-interop :as jsi]
            [cljs-bean.core :as bean]
            [l4-lp.swipl.js.wasm-query :as swipl-wasm-query]
            [l4-lp.syntax.l4-to-prolog :as l4->prolog]))

(def ^:private guifier-opts
  #js {:data #js []
       :dataType "js"
       :elementSelector "#guifier"
       :withoutContainer true
       :readOnlyMode true})

(def ^:private guifier
  (atom nil))

(defn- init-guifier-if-needed! []
  (swap! guifier (some-fn identity #(new Guifier guifier-opts))))

(defn query-and-trace-and-guifier! [l4-program]
  (init-guifier-if-needed!)

  (let [prolog-program+queries
        (-> l4-program l4->prolog/l4->prolog-program+queries)
        guifier-query-results #js []]

    (jsi/call js/console :log
              "Transpiled program and queries:\n"
              (bean/->js prolog-program+queries))

    ;; Execute queries sequentially and update guifier GUI as each query
    ;; executes.
    (swipl-wasm-query/query-and-trace!
     prolog-program+queries
     (fn [result]
       (->> result bean/->js (jsi/call guifier-query-results :push))
       (jsi/call @guifier :setData guifier-query-results "js")))))