(ns l4-lp.web-editor.core
  (:require ["@mui/material/Box$default" :as Box]
            ["@mui/material/Unstable_Grid2$default" :as Grid]
            ["@mui/material/Link$default" :as Link]
            ["@mui/material/Typography$default" :as Typography]
            [applied-science.js-interop :as jsi]
            [l4-lp.web-editor.codemirror-editor :as cm-editor]
            [uix.core :as uix]
            [uix.dom :as dom]))

(def ^:private web-editor-preamble-url
 "web_editor_preamble.edn")

(def ^:private web-editor-app-id
  "web-editor-app")

(uix/defui ^:private web-editor-component []
  (let [editor-elt-ref (uix/use-ref)]
    (uix/use-effect
     #(cm-editor/bind-editor!
       {:editor-preamble-url web-editor-preamble-url
        :editor-elt @editor-elt-ref :guifier-elt-id "guifier"}))
    (uix/$
     Box
     (uix/$ :title "L4 web editor")

     (uix/$ :link
            {:rel "preconnect"
             :href "https://fonts.googleapis.com"})
     (uix/$ :link
            {:rel "preconnect"
             :href "https://fonts.gstatic.com"
             :cross-origin true})
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
                    (uix/$ Typography {:mt 2
                                       :max-width :md
                                       :variant :body1
                                       :gutter-bottom true}
                           "Enter an L4 program below, and then press M-Enter to evaluate all queries.
When the evaluation completes, an execution trace will appear below the input window.
Note that the whole pipeline, from parsing and transpilation to evaluation and
processing of traces, runs directly in the browser."))

             (uix/$ Box {:sx #js {:m 2} :id "editor" :ref editor-elt-ref}))

      (uix/$ Grid
             (uix/$ Box {:sx #js {:m 2}}
                    (uix/$ Typography {:variant :h3 :gutter-bottom true}
                           "Query results"))
             (uix/$ Box {:xs #js {:m 2} :id "guifier"}))))))

(defn- render-react-web-editor-app! []
  (let [app-root
        (-> js/document
            (jsi/call :getElementById web-editor-app-id)
            dom/create-root)]
    (dom/render-root (uix/$ web-editor-component) app-root)))

(defn start! []
  (println "Starting..."))

(defn stop! []
  (println "Stopping..."))

(defn init! []
  (render-react-web-editor-app!)
  (start!))