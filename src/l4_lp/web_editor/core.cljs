(ns l4-lp.web-editor.core
  (:require [applied-science.js-interop :as jsi]
            [l4-lp.web-editor.codemirror-editor :as cm-editor]
            [l4-lp.web-editor.guifier :as guifier]
            [uix.core :as uix]
            [uix.dom :as dom]))

(def ^:private initial-editor-text
  ";; Enter an L4 program here, and then press M-Enter to evaluate all queries.
;; When the evaluation completes, an execution trace will appear below the input window.
;; Note that the whole pipeline, from parsing and transpilation to evaluation and
;; processing of traces, runs directly in the browser.

GIVETH x
QUERY MIN (MINUS 0 x) (SUM [1, 2, 3, -12]) 2 < PRODUCT (MAX (DIVIDE 12 3) 4 -1) 19 x

GIVETH x y
QUERY SUM x y IS 0
AND MINUS x y IS 0

GIVEN (x IS A Number)
DECIDE x is between 0 and 10 or is 100
IF 0 <= x AND x <= 10
OR x IS MAX 100 -20

DECIDE (2023 - 1 - 10) is a date

GIVEN (xs IS A LIST OF Number)
GIVETH x
DECIDE b of x and xs
WHERE x IS SUM xs

DECIDE b of 0 and _ OTHERWISE")

(def ^:private web-editor-app-id
  "web-editor-app")

(uix/defui ^:private web-editor-component []
  (let [editor-elt-ref (uix/use-ref)]
    (uix/use-effect
     (fn []
       (cm-editor/bind-editor! @editor-elt-ref initial-editor-text)
       (guifier/init-guifier-if-needed!))
     [])

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