(ns l4-lp.main
  (:require [applied-science.js-interop :as jsi]
            [hoplon.core :as h]
            [hoplon.goog]
            [javelin.core :as hj]
            [l4-lp.swipl.js.wasm-query :as swipl-wasm-query]
            [l4-lp.syntax.l4-to-prolog :as l4->prolog]
            [promesa.core :as prom]
            [shadow.esm :refer [dynamic-import]]))

(hj/defc l4-program
  "DECIDE p
IF q AND r

DECIDE q
WHEN 3 IS SUM 0 1 (MIN (SUM 0 3) 2)

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

(hj/defc l4-query "q")

(def ^:private guifier-cdn-url
  "https://cdn.jsdelivr.net/npm/guifier@1.0.24/dist/Guifier.js")

(def guifier-constructor
  (atom nil))

(defn query-and-trace-and-guifier! []
  (prom/let
   [l4-program @l4-program
    l4-query @l4-query

    guifier-div (jsi/call js/document :getElementById "guifier")
    _ (jsi/assoc! guifier-div :innerHTML "")

    program (-> l4-program l4->prolog/l4-program->prolog-program-str)
    _ (jsi/call js/console :log "Transpiled program: " program)

    query (-> l4-query l4->prolog/l4->prolog-str)
    _ (jsi/call js/console :log "Transpiled query: " query)

    stack-trace (swipl-wasm-query/query-and-trace-js! program query)

    Guifier @guifier-constructor]
    (Guifier. #js {:data stack-trace
                   :dataType "js"
                   :elementSelector "#guifier"
                   :withoutContainer true
                   :readOnlyMode true})))

(h/defelem html [_attrs _children]
  (h/div
   (h/link :rel "stylesheet"
           :href "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css"
           :integrity "sha384-QWTKZyjpPEjISv5WaRU9OFeRpok6YctnYmDr5pNlyT2bRjXh0JMhjY6hW+ALEwIH"
           :crossorigin "anonymous")

   (h/title "L4 web editor")
   (h/h1 "L4 web editor")

   (h/div :class "form-group"
          (h/label :for "l4-program"
                   :class "col-sm-1 control-label"
                   (h/b "L4 Program"))
          (h/textarea :class "form-control"
                      :id "l4-program"
                      :rows 7
                      :value l4-program
                      :change #(reset! l4-program @%)))

   (h/div :class "form-group"
          (h/label :for "l4-query"
                   :class "col-sm-1 control-label"
                   (h/b "Query"))
          (h/input :class "form-control"
                   :id "l4-query"
                   :type "text"
                   :value l4-query
                   :change #(reset! l4-query @%))
          (h/button :class "btn btn-primary"
                    :click #(query-and-trace-and-guifier!)
                    (h/text "Run query")))

   (h/div (h/b "Trace")
          (h/div :id "guifier"))))

(defn mount-components! []
  (-> js/document
      (jsi/call :getElementById "app")
      (jsi/call :replaceChildren (html))))

(defn start []
  (mount-components!)
  (jsi/call js/console :log "Starting..."))

(defn stop []
  (jsi/call js/console :log "Stopping..."))

(defn init []
  (prom/let [guifier-mod (dynamic-import guifier-cdn-url)
             Guifier (jsi/get guifier-mod :default)]
    (reset! guifier-constructor Guifier)
    (start)))