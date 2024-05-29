(ns l4-lp.ide.ui.query.button 
  (:require ["@mui/lab/LoadingButton$default" :as LoadingButton]
            [applied-science.js-interop :as jsi]
            [l4-lp.ide.query.core :refer [init-worker!
                                          transpile-and-query-on-worker!]]
            [uix.core :as uix]))

(uix/defui query-button
  [{:keys [cm-editor-ref
           on-click
           on-transpiled-prolog on-query-result
           button-props children]}]
  (let [[queries-running? set-queries-running!] (uix/use-state false)
        query-worker-ref (uix/use-ref)
        query-fn!
        (fn []
          (set-queries-running! true)

          (let [cm-editor-doc (jsi/get-in @cm-editor-ref [:view :state :doc])
                query-worker @query-worker-ref]
            (on-click)

            (transpile-and-query-on-worker!
             :l4-program (str cm-editor-doc)
             :worker query-worker
             :on-transpiled-prolog on-transpiled-prolog
             :on-query-result on-query-result
             :on-done #(set-queries-running! false))))]

    (uix/use-effect
     (fn []
       (reset! query-worker-ref (init-worker!))
       #(jsi/call @query-worker-ref :terminate))
     [])

    (uix/$ LoadingButton
           (merge button-props
                  {:loading queries-running?
                   :on-click query-fn!})
           children)))