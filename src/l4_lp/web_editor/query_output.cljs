(ns l4-lp.web-editor.query-output
  (:require ["@mui/icons-material/ExpandMore$default" :as ExpandMoreIcon]
            ["@mui/material/Accordion$default" :as Accordion]
            ["@mui/material/AccordionDetails$default" :as AccordionDetails]
            ["@mui/material/AccordionSummary$default" :as AccordionSummary]
            ["@mui/material/Box$default" :as Box]
            ["@mui/material/Typography$default" :as Typography]
            ["https://cdn.jsdelivr.net/npm/guifier@1.0.24/dist/Guifier.js$default"
             :as Guifier]
            [applied-science.js-interop :as jsi]
            [cljs-bean.core :as bean]
            [l4-lp.web-editor.utils :refer [suspense-loading-bar]]
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
    suspense-loading-bar
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
    (uix/$ Box (->> query-results
                    (map-indexed indexed-query-result->comp)))))

(uix/defui query-output
  [{:keys [max-height
           prolog-program-and-queries
           query-results]}]
  (uix/$ Box
         (uix/$ Typography {:mb 2 :variant :h4} "Query Results")

         (uix/$ Box {:max-height max-height
                     :overflow :auto}
                (uix/$ Box {:ml 2 :mr 2 :mb 2}
                       (uix/$ prolog-program-and-queries-comp
                              {:data prolog-program-and-queries}))
                (uix/$ query-results-comp
                       {:data query-results}))))