(ns l4-lp.web-editor.guifier
  (:require ["https://cdn.jsdelivr.net/npm/guifier@1.0.24/dist/Guifier.js$default"
             :as Guifier]
            [applied-science.js-interop :as jsi]
            [cljs-bean.core :as bean]
            [l4-lp.swipl.js.wasm-query :as swipl-wasm-query]
            [l4-lp.syntax.l4-to-prolog :as l4->prolog]
            [promesa.core :as prom]))

(def ^:private guifier-element-id
  "guifier")

(defn query-and-trace-and-guifier! [l4-program]
  (-> js/document
      (jsi/call :getElementById guifier-element-id)
      (jsi/assoc! :innerHTML ""))

  (prom/let
   [{program :program queries :queries :as prolog-program+queries}
    (-> l4-program l4->prolog/l4->prolog-program+queries)

    _ (println "Transpiled program: " program)
    _ (println "Transpiled queries: " (bean/->js queries))

    stack-trace (swipl-wasm-query/query-and-trace-js! prolog-program+queries)]
    (Guifier. #js {:data stack-trace
                   :dataType "js"
                   :elementSelector (str "#" guifier-element-id)
                   :withoutContainer true
                   :readOnlyMode true})))