(ns l4-lp.ide.ui.ide-instrs 
  (:require ["@mui/icons-material/ExpandMore$default" :as ExpandMoreIcon]
            ["@mui/material/Accordion$default" :as Accordion]
            ["@mui/material/AccordionDetails$default" :as AccordionDetails]
            ["@mui/material/AccordionSummary$default" :as AccordionSummary]
            ["@mui/material/Typography$default" :as Typography]
            [l4-lp.ide.ui.utils :refer [suspense-loading-bar
                                        use-cached-fetch-as-text!]]
            [uix.core :as uix]))

(def ^:private ide-instrs-url
  "ide_usage_instructions.txt")

(uix/defui ide-instrs
  [{:keys [max-text-width sx]}]
  (let [ide-instrs-text (use-cached-fetch-as-text! ide-instrs-url)]
    (uix/$ Accordion {:sx sx}
           (uix/$ AccordionSummary
                  {:expand-icon (uix/$ ExpandMoreIcon)
                   :aria-controls :panel-content
                   :id :web-editor-instrs}
                  (uix/$ Typography {:variant :h6} "Usage instructions"))
           (uix/$ AccordionDetails
                  (uix/$ suspense-loading-bar
                         (uix/$ Typography {:max-width max-text-width
                                            :variant :body1
                                            :white-space :pre-line}
                                ide-instrs-text))))))