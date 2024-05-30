(ns l4-lp.ide.ui.query.button 
  (:require ["@mui/lab/LoadingButton$default" :as LoadingButton]
            [applied-science.js-interop :as jsi]
            [l4-lp.ide.query.core :refer [worker-js-url
                                          transpile-and-query-on-worker!]]
            [l4-lp.ide.ui.utils :refer [use-web-worker!]]
            [uix.core :as uix]))

(uix/defui query-button
  [{:keys [cm-editor-ref
           on-click
           on-transpiled-prolog on-query-result
           button-props children]}]
  (let [[loading? set-loading!] (uix/use-state true)
        [query-worker query-worker-ready?] (use-web-worker! worker-js-url)
        query-fn!
        (fn []
          (set-loading! true)

          (let [cm-editor-doc (jsi/get-in @cm-editor-ref
                                          [:view :state :doc])
                query-worker @query-worker]
            (on-click)

            (transpile-and-query-on-worker!
             :l4-program (str cm-editor-doc)
             :worker query-worker
             :on-transpiled-prolog on-transpiled-prolog
             :on-query-result on-query-result
             :on-done #(set-loading! false))))]

    (uix/use-effect
     #(when query-worker-ready?
        (set-loading! false))
     [query-worker-ready?])

    (uix/$ LoadingButton
           (merge button-props
                  {:loading loading?
                   :on-click query-fn!})
           children)))