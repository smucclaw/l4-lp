(ns l4-lp.web-editor.guifier
  (:require [applied-science.js-interop :as jsi]
            [l4-lp.swipl.js.wasm-query :as swipl-wasm-query]
            [l4-lp.syntax.l4-to-prolog :as l4->prolog]
            [promesa.core :as prom]
            ["https://cdn.jsdelivr.net/npm/guifier@1.0.24/dist/Guifier.js$default"
             :as Guifier]))

(def ^:private guifier-element-id
  "guifier")

(defn query-and-trace-and-guifier! [l4-program]
  (-> js/document
      (jsi/call :getElementById guifier-element-id)
      (jsi/assoc! :innerHTML ""))

  (prom/let
   [{program :program-str
     query :query-str}
    (-> l4-program l4->prolog/l4-program->prolog-query+program-str)

    _ (jsi/call js/console :log "Transpiled program: " program)
    _ (jsi/call js/console :log "Transpiled query: " query)

    stack-trace (swipl-wasm-query/query-and-trace-js! program query)]
    (Guifier. #js {:data stack-trace
                   :dataType "js"
                   :elementSelector (str "#" guifier-element-id)
                   :withoutContainer true
                   :readOnlyMode true})))