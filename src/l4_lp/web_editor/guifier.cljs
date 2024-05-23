(ns l4-lp.web-editor.guifier
  (:require ["https://cdn.jsdelivr.net/npm/guifier@1.0.24/dist/Guifier.js$default"
             :as Guifier]
            ["@mui/material/Box$default" :as Box]
            [applied-science.js-interop :as jsi]
            [cljs-bean.core :as bean]
            [l4-lp.swipl.js.wasm-query :as swipl-wasm-query]
            [l4-lp.syntax.l4-to-prolog :as l4->prolog]
            [uix.core :as uix]))

(defn- init-guifier! [guifier-elt-id]
  (new Guifier
       #js {:data #js []
            :dataType "js"
            :elementSelector (str "#" guifier-elt-id)
            :withoutContainer true
            :readOnlyMode true}))

(uix/defui Guifier [{:keys [ref max-height]}]
  (let [elt-id (str `guifier#)]
    (uix/use-effect
     (fn [] (swap! ref (some-fn identity #(init-guifier! elt-id)))))
    (uix/$ Box {:id elt-id :max-height max-height :overflow :auto})))

(defn query-trace-and-update-guifier! [guifier l4-program]
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
       (jsi/call guifier :setData guifier-query-results "js")))))