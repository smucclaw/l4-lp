(ns l4-lp.web-editor.query-output
  (:require ["@mui/icons-material/ExpandMore$default" :as ExpandMoreIcon]
            ["@mui/icons-material/Send$default" :as SendIcon]
            ["@mui/lab/LoadingButton$default" :as LoadingButton]
            ["@mui/material/Accordion$default" :as Accordion]
            ["@mui/material/AccordionDetails$default" :as AccordionDetails]
            ["@mui/material/AccordionSummary$default" :as AccordionSummary]
            ["@mui/material/Box$default" :as Box]
            ["@mui/material/Typography$default" :as Typography]
            ["https://cdn.jsdelivr.net/npm/guifier@1.0.24/dist/Guifier.js$default"
             :as Guifier]
            [applied-science.js-interop :as jsi]
            [cljs-bean.core :as bean]
            [l4-lp.swipl.js.wasm-query :as swipl-wasm-query]
            [l4-lp.syntax.l4-to-prolog :as l4->prolog]
            [l4-lp.web-editor.utils :refer [loading-bar]]
            [promesa.core :as prom]
            [uix.core :as uix]))

(defn- init-guifier!
  ([guifier-elt-id]
   (init-guifier! guifier-elt-id nil))
  
  ([guifier-elt-id js-data]
   (new Guifier
        #js {:data js-data
             :dataType "js"
             :elementSelector (->> guifier-elt-id
                                   (jsi/call js/CSS :escape)
                                   (str "#"))
             :withoutContainer true
             :readOnlyMode true})))

(uix/defui guifier
  [{:keys [data box-props]}]

  (let [elt-id (str "guifier" (uix/use-id))
        guifier-ref (uix/use-ref)]

    (uix/use-effect
     #(swap! guifier-ref
             (fn [guifier]
               (let [js-data (bean/->js data)]
                 (if guifier
                   (do (jsi/call guifier :setData js-data "js")
                       guifier)
                   (init-guifier! elt-id js-data))))))

    (uix/$ Box (assoc box-props :id elt-id))))

(uix/defui prolog-program-and-queries-comp
  [{prolog-program-and-queries :data}]
  (and
   (not-empty prolog-program-and-queries)
   (uix/$
    loading-bar
    (uix/$ Accordion
           (uix/$ AccordionSummary {:expand-icon (uix/$ ExpandMoreIcon)}
                  (uix/$ Typography {:variant :h6}
                         "Transpiled Prolog program and queries"))
           (uix/$ AccordionDetails
                  (uix/$ guifier {:data prolog-program-and-queries}))))))

(uix/defui query-results-comp
  [{query-results :data}]

  (let [indexed-query-result->comp
        (fn [index result]
          (uix/$ Accordion {:key [index result]}
                 (uix/$ AccordionSummary
                        {:expand-icon (uix/$ ExpandMoreIcon)
                         :aria-controls :panel-content}
                        (uix/$ Typography {:variant :h6}
                               (str "Query " (inc index))))
                 (uix/$ AccordionDetails
                        (uix/$ guifier {:data result}))))]
    (uix/$ Box #_{:mt 4}
           (uix/$ Typography {:m 2 :variant :h4} "Query Results")
           (uix/$ Box
                  (->> query-results
                       (map-indexed indexed-query-result->comp))))))

(uix/defui query-button-and-output
  [{:keys [max-height cm-editor-ref]}]

  (let [[queries-running? set-queries-running!] (uix/use-state false)
        [prolog-program-and-queries-stateful set-prolog!] (uix/use-state nil)
        [query-results-stateful set-query-results!] (uix/use-state [])

        cm-editor->query-trace-and-update-guifier!
        (fn []
          (set-queries-running! true)
          (set-query-results! [])
          (set-prolog! nil)

          (let [cm-editor-doc (jsi/get-in @cm-editor-ref [:view :state :doc])
                prolog-program-and-queries
                (-> cm-editor-doc str l4->prolog/l4->prolog-program+queries)]
            (prom/do
              (set-prolog! prolog-program-and-queries)

              (swipl-wasm-query/query-and-trace!
               prolog-program-and-queries
               (fn [result] (set-query-results! #(conj % result))))

              (set-queries-running! false))))]
    (uix/$ Box
           (uix/$ LoadingButton
                  {:sx #js {:ml 5 :mb 2}
                   :loading queries-running?
                   :variant :contained
                   :size :large
                   :end-icon (uix/$ SendIcon)
                   :on-click cm-editor->query-trace-and-update-guifier!}
                  "Run Queries")
           (uix/$ Box {:max-height max-height
                       :overflow :auto}
                  (uix/$ prolog-program-and-queries-comp
                         {:data prolog-program-and-queries-stateful})
                  (uix/$ query-results-comp
                         {:data query-results-stateful})))))