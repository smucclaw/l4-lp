(ns l4-lp.ide.ui.query.button 
  (:require ["@mui/lab/LoadingButton$default" :as LoadingButton]
            [applied-science.js-interop :as jsi]
            [l4-lp.swipl.js.wasm-query :as swipl-wasm-query]
            [l4-lp.syntax.l4-to-prolog :as l4->prolog]
            [promesa.core :as prom]
            [uix.core :as uix]))

;; TODO:
;; Currently, the query-fn function below defined in the button component
;; executes both the backend parse -> transpile -> execute pipeline, along with
;; various operations to update the UI state accordingly.
;; This is not ideal for the following reasons:
;; 1. It complects the backend pipeline along with frontend UI operations in
;;    this UI component.
;; 2. It executes the backend pipeline in the main UI thread, thus blocking
;;    the thread and rendering the UI unresponsive when input programs are
;;    processed in the pipeline.
;;
;; Ideally, the backend pipeline should be separated out into a web worker
;; that runs in a separate background thread, and communicates with this
;; component via message passing.
;; In this way, executing the pipeline will no longer block the UI, thus
;; improving responsiveness and solving (2).
;; Moreover, this resolves (1) as the backend pipeline logic will be abstracted
;; away via message passing. 
;;
;; Next steps:
;; - Study the web worker API and in particular:
;;   - How to spawn them in CLJS via the facilities provided by shadow-cljs.
;;   - How to send messages between the parent UI thread and child web worker.
;; - Design and implement an API based on findings to the above.

(uix/defui query-button
  [{:keys [cm-editor-ref
           set-transpiled-prolog! set-query-results!
           button-props children]}]
  (let [[queries-running? set-queries-running!] (uix/use-state false)
        query-fn
        (fn []
          (set-queries-running! true)
          (set-query-results! [])
          (set-transpiled-prolog! nil)

          (let [cm-editor-doc (jsi/get-in @cm-editor-ref [:view :state :doc])
                prolog-program-and-queries
                (-> cm-editor-doc str l4->prolog/l4->prolog-program+queries)]
            (prom/do
              (set-transpiled-prolog! prolog-program-and-queries)

              (swipl-wasm-query/query-and-trace!
               prolog-program-and-queries
               (fn [result] (set-query-results! #(conj % result))))

              (set-queries-running! false))))]
    (uix/$ LoadingButton
           (merge button-props
                  {:loading queries-running?
                   :on-click query-fn})
           children)))