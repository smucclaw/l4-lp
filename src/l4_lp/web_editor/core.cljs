(ns l4-lp.web-editor.core
  (:require ["@mui/material/Box$default" :as Box]
            ["@mui/material/Link$default" :as Link]
            ["@mui/material/Typography$default" :as Typography]
            ["@mui/material/Unstable_Grid2$default" :as Grid]
            ["react-dom" :as react-dom]
            [applied-science.js-interop :as jsi]
            [l4-lp.web-editor.codemirror-editor :refer [cm-editor-and-instrs]]
            [l4-lp.web-editor.query-output :refer [query-button-and-output]]
            [uix.core :as uix]
            [uix.dom :as uix-dom]))

(def ^:private editor-preamble-url
 "web_editor_preamble.edn")

(def ^:private editor-instrs-url
  "web_editor_instructions.txt")

(def ^:private web-editor-app-id
  "web-editor-app")

(uix/defui web-editor-app []
  (uix/$
   Box
   (uix/$ :title "L4 web editor")

   ;; Load fonts for MUI components.
   (react-dom/preconnect "https://fonts.googleapis.com")
   (react-dom/preconnect "https://fonts.gstatic.com"
                         #js {:crossOrigin "anonymous"})
   (uix/$ :link {:rel :stylesheet
                 :href "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;500;700&display=swap"})

   (uix/$ Box {:ml 2 :mr 2}
          (uix/$ Typography {:variant :h3 :gutter-bottom true} "L4 web editor")

          (uix/$ Link {:href "https://github.com/smucclaw/l4-lp"
                       :underline :hover
                       :variant :h6}
                 "Click here to visit the project on GitHub!"))

   (let [cm-editor-ref (uix/use-ref)]
     (uix/$ Grid {:container true}
            (uix/$ Grid {:m 2}
                   (uix/$ cm-editor-and-instrs
                          {:cm-editor-ref cm-editor-ref
                           :editor-preamble-url editor-preamble-url
                           :editor-instrs-url editor-instrs-url}))
            (uix/$ Grid {:ml 2 :mr 2}
                   (uix/$ query-button-and-output
                          {:cm-editor-ref cm-editor-ref}))))))

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