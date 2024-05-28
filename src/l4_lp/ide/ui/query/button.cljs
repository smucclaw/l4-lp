(ns l4-lp.ide.ui.query.button 
  (:require ["@mui/lab/LoadingButton$default" :as LoadingButton]
            [applied-science.js-interop :as jsi]
            [l4-lp.ide.query.core :refer [transpile-and-query!]]
            [uix.core :as uix]))

(uix/defui query-button
  [{:keys [cm-editor-ref
           on-click
           on-transpiled-prolog on-query-result
           button-props children]}]
  (let [[queries-running? set-queries-running!] (uix/use-state false)
        query-fn!
        (fn []
          (set-queries-running! true)
          (on-click)

          (let [l4-program (-> @cm-editor-ref
                               (jsi/get-in [:view :state :doc])
                               str)]
            (transpile-and-query!
             l4-program
             :on-transpiled-prolog on-transpiled-prolog
             :on-query-result on-query-result
             :on-done
             #(set-queries-running! false))))]
    (uix/$ LoadingButton
           (merge button-props
                  {:loading queries-running? :on-click query-fn!})
           children)))