(ns l4-lp.ide.ui.query.output.core
  (:require ["@mui/icons-material/ExpandMore$default" :as ExpandMoreIcon]
            ["@mui/material/Accordion$default" :as Accordion]
            ["@mui/material/AccordionDetails$default" :as AccordionDetails]
            ["@mui/material/AccordionSummary$default" :as AccordionSummary]
            ["@mui/material/Box$default" :as Box]
            ["@mui/material/Typography$default" :as Typography]
            [l4-lp.ide.ui.query.output.guifier :refer [guifier]]
            [l4-lp.ide.ui.utils :refer [suspense-loading-bar]]
            [uix.core :as uix]))

(uix/defui ^:private transpiled-prolog
  [{:keys [data]}]
  (and
   (some? data)
   (uix/$
    suspense-loading-bar
    (uix/$ Accordion
           (uix/$ AccordionSummary {:expand-icon (uix/$ ExpandMoreIcon)}
                  (uix/$ Typography {:variant :h6}
                         "Transpiled Prolog program and queries"))
           (uix/$ AccordionDetails
                  (uix/$ guifier {:data data}))))))

(uix/defui ^:private query-results
  [{:keys [data]}]
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
    (->> data
         (eduction (map-indexed indexed-query-result->comp))
         to-array)))

(uix/defui query-output
  [{max-height :max-height
    transpiled-prolog-data :transpiled-prolog
    query-results-data :query-results}]
  (uix/$ Box
         (uix/$ Typography {:ml 2 :mr 2 :mb 2 :variant :h4}
                "Query Results")

         (uix/$ Box {:max-height max-height
                     :overflow :auto}
                (uix/$ Box {:ml 2 :mr 2 :mb 2}
                       (uix/$ transpiled-prolog
                              {:data transpiled-prolog-data}))
                (uix/$ query-results {:data query-results-data}))))