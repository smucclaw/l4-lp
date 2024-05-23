(ns l4-lp.web-editor.core
  (:require [applied-science.js-interop :as jsi]
            [l4-lp.web-editor.codemirror-editor :as cm-editor]
            [l4-lp.web-editor.guifier :as guifier]
            [uix.core :as uix]
            [uix.dom :as dom]))

(def ^:private web-editor-preamble-url
 "web_editor_preamble.edn")

(def ^:private web-editor-app-id
  "web-editor-app")

(uix/defui ^:private web-editor-component []
  (let [editor-elt-ref (uix/use-ref)]
    (uix/use-effect
     (fn []
       (guifier/init-guifier-if-needed!)
       (cm-editor/bind-editor! @editor-elt-ref web-editor-preamble-url)))
    (uix/$
     :div
     (uix/$ :link
            {:rel "stylesheet"
             :href "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css"
             :integrity "sha384-QWTKZyjpPEjISv5WaRU9OFeRpok6YctnYmDr5pNlyT2bRjXh0JMhjY6hW+ALEwIH"
             :crossOrigin "anonymous"})

     (uix/$ :title "L4 web editor")
     (uix/$ :h1 "L4 web editor")

     (uix/$ :a {:href "https://github.com/smucclaw/l4-lp"}
            "Click here to visit the project on GitHub!")

     (uix/$ :br)
     (uix/$ :br)

     (uix/$ :div {:id "editor" :ref editor-elt-ref})

     (uix/$ :br)

     (uix/$ :h2 "Query results")
     (uix/$ :div {:id "guifier"}))))

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