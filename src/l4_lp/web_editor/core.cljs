(ns l4-lp.web-editor.core
  (:require ["@mui/icons-material/Send$default" :as SendIcon]
            ["@mui/material/Box$default" :as Box]
            ["@mui/material/Unstable_Grid2$default" :as Grid]
            ["react-dom" :as react-dom]
            [applied-science.js-interop :as jsi]
            [l4-lp.web-editor.editor :as editor]
            [l4-lp.web-editor.query-button :as query-button]
            [l4-lp.web-editor.query-output :as query-output]
            [l4-lp.web-editor.top-bar :as top-bar]
            [uix.core :as uix]
            [uix.dom :as uix-dom]))

(def ^:private editor-preamble-url
 "web_editor_preamble.edn")

(def ^:private editor-instrs-url
  "web_editor_instructions.txt")

(def ^:private web-editor-app-id
  "web-editor-app")

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

(uix/defui web-editor-app []
  (let [cm-editor-ref (uix/use-ref)
        [prolog-program-and-queries set-prolog!] (uix/use-state nil)
        [query-results set-query-results!] (uix/use-state [])]
    (uix/$ Box
           (uix/$ :title "L4 web editor")
           (uix/$ mui-fonts)

           (uix/$ top-bar/top-bar
                  (uix/$ query-button/query-button
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
                         (uix/$ editor/editor
                                {:max-editor-height :85vh
                                 :max-editor-instrs-width :sm
                                 :editor-font-size :14pt
                                 :editor-ref cm-editor-ref
                                 :editor-preamble-url editor-preamble-url
                                 :editor-instrs-url editor-instrs-url}))
                  (uix/$ Grid {:mt 2 :ml 2 :mr 2}
                         (uix/$ query-output/query-output
                                {:max-height :85vh
                                 :prolog-program-and-queries prolog-program-and-queries
                                 :query-results query-results}))))))

(defn- render-web-editor-app! []
  (let [app-root
        (-> js/document
            (jsi/call :getElementById web-editor-app-id)
            uix-dom/create-root)]
    (uix-dom/render-root (uix/$ web-editor-app) app-root)))

(defn start! []
  (println "Starting..."))

(defn stop! []
  (println "Stopping..."))

(defn init! []
  (render-web-editor-app!)
  (start!))