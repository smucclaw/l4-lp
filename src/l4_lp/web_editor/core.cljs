(ns l4-lp.web-editor.core
  (:require ["@mui/icons-material/Send$default" :as SendIcon]
            ["@mui/material/Box$default" :as Box]
            ["@mui/material/Button$default" :as Button]
            ["@mui/material/Link$default" :as Link]
            ["@mui/material/Typography$default" :as Typography]
            ["@mui/material/Unstable_Grid2$default" :as Grid]
            [applied-science.js-interop :as jsi]
            [l4-lp.web-editor.codemirror-editor :as cm-editor]
            [l4-lp.web-editor.guifier :as guifier]
            [l4-lp.web-editor.utils :refer [fetch-text-from-url-and-do!]]
            [tupelo.core :refer [it->]]
            [uix.core :as uix]
            [uix.dom :as dom]))

(def ^:private web-editor-preamble-url
 "web_editor_preamble.edn")

(def ^:private web-editor-instrs-url
  "web_editor_instructions.txt")

(def ^:private web-editor-app-id
  "web-editor-app")

(uix/defui ^:private WebEditor []
  (let [[web-editor-instrs set-web-editor-instrs!] (uix/use-state nil)

        guifier-ref (uix/use-ref)
        cm-editor-ref (uix/use-ref)

        cm-editor->query-trace-and-update-guifier!
        #(let [cm-editor @cm-editor-ref
               guifier @guifier-ref]
           (it-> cm-editor
                 (jsi/get-in it [:view :state :doc])
                 (str it)
                 (guifier/query-trace-and-update-guifier! guifier it)))]

    (uix/use-effect
     #(fetch-text-from-url-and-do! web-editor-instrs-url
                                   set-web-editor-instrs!))

    (uix/$
     Box
     (uix/$ :title "L4 web editor")
     (uix/$ :link
            {:rel "preconnect"
             :href "https://fonts.googleapis.com"})
     (uix/$ :link
            {:rel "preconnect"
             :href "https://fonts.gstatic.com"
             :cross-origin "anonymous"})
     (uix/$ :link
            {:rel "stylesheet"
             :href "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;500;700&display=swap"})

     (uix/$
      Grid {:container true}
      (uix/$ Grid
             (uix/$ Box {:sx #js {:m 2}}
                    (uix/$ Typography {:variant :h2 :gutter-bottom true}
                           "L4 web editor")

                    (uix/$ Link {:href "https://github.com/smucclaw/l4-lp"
                                 :underline :hover
                                 :variant :h5
                                 :gutter-bottom true}
                           "Click here to visit the project on GitHub!")

                    (uix/$ Typography {:mt 2 :mb 2
                                       :max-width :md
                                       :variant :body1
                                       :gutter-bottom true}
                           web-editor-instrs)

                    (uix/$ cm-editor/CodeMirrorEditor
                           {:ref cm-editor-ref
                            :max-height :70vh
                            :font-size :14pt
                            :editor-preamble-url web-editor-preamble-url})))

      (uix/$ Grid
             (uix/$ Box {:sx #js {:m 2}}
                    (uix/$ Typography {:variant :h4 :gutter-bottom true}
                           "Query results")

                    (uix/$ Button {:variant :contained
                                   :size :large
                                   :end-icon (uix/$ SendIcon)
                                   :on-click
                                   cm-editor->query-trace-and-update-guifier!}
                           "Run Queries")

                    (uix/$ guifier/Guifier
                           {:ref guifier-ref
                            :max-height :100vh})))))))

(defn- render-react-web-editor-app! []
  (let [app-root
        (-> js/document
            (jsi/call :getElementById web-editor-app-id)
            dom/create-root)]
    (dom/render-root (uix/$ WebEditor) app-root)))

(defn start! []
  (println "Starting..."))

(defn stop! []
  (println "Stopping..."))

(defn init! []
  (render-react-web-editor-app!)
  (start!))