(ns l4-lp.ide.core
  (:require ["@mui/icons-material/Send$default" :as SendIcon]
            ["@mui/material/Box$default" :as Box]
            ["@mui/material/Unstable_Grid2$default" :as Grid]
            ["react-dom" :as react-dom]
            [applied-science.js-interop :as jsi]
            [l4-lp.ide.ui.editor :refer [editor]]
            [l4-lp.ide.ui.ide-instrs :refer [ide-instrs]]
            [l4-lp.ide.ui.query.button :refer [query-button]]
            [l4-lp.ide.ui.query.output :refer [query-output]]
            [l4-lp.ide.ui.top-bar :as top-bar]
            [uix.core :as uix]
            [uix.dom :as uix-dom]))

(def ^:private ide-preamble-url
 "ide_preamble.edn")

(def ^:private ide-instrs-url
  "ide_usage_instructions.txt")

(def ^:private ide-app-id
  "ide-app")

(uix/defui mui-fonts
  "Component to load fonts for Material UI."
  []
  (uix/$
   :div
   (react-dom/preconnect "https://fonts.googleapis.com")
   (react-dom/preconnect "https://fonts.gstatic.com"
                         #js {:crossOrigin "anonymous"})
   (uix/$ :link {:rel :stylesheet
                 :href "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;500;700&display=swap"})))

(uix/defui ide-app []
  (let [cm-editor-ref (uix/use-ref)
        [prolog-program-and-queries set-prolog!] (uix/use-state nil)
        [query-results set-query-results!] (uix/use-state nil)]
    (uix/$ Box
           (uix/$ :title "L4 web editor")
           (uix/$ mui-fonts)

           (uix/$ top-bar/top-bar
                  (uix/$ query-button
                         {:cm-editor-ref cm-editor-ref
                          :set-prolog! set-prolog!
                          :set-query-results! set-query-results!
                          :button-props {:sx #js {:ml 3}
                                         :variant :contained
                                         :color :inherit
                                         :size :medium
                                         :end-icon (uix/$ SendIcon)}}
                         "Run Queries"))

           (uix/$ Grid {:container true
                        :spacing 2}
                  (uix/$ Grid {:ml 2 :mr 2}
                         (uix/$ Box
                                (uix/$ ide-instrs
                                       {:max-text-width :md
                                        :ide-instrs-url ide-instrs-url})
                                (uix/$ Box {:mt 2}))
                         (uix/$ editor
                                {:max-height :85vh
                                 :font-size :14pt
                                 :ref cm-editor-ref
                                 :ide-preamble-url ide-preamble-url}))
                  (uix/$ Grid {:mt 2 :ml 2 :mr 2}
                         (uix/$ query-output
                                {:max-height :85vh
                                 :prolog-program-and-queries prolog-program-and-queries
                                 :query-results query-results}))))))

(defn- render-ide-app! []
  (let [app-root
        (-> js/document
            (jsi/call :getElementById ide-app-id)
            uix-dom/create-root)]
    (uix-dom/render-root (uix/$ ide-app) app-root)))

(defn start! []
  (println "Starting..."))

(defn stop! []
  (println "Stopping..."))

(defn init! []
  (render-ide-app!)
  (start!))