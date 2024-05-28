(ns l4-lp.ide.ui.query.button 
  (:require ["@mui/lab/LoadingButton$default" :as LoadingButton]
            [applied-science.js-interop :as jsi]
            [lambdaisland.uri :as uri]
            [meander.epsilon :as m]
            [uix.core :as uix]))

(def ^:private swipl-prelude-qlf-url
  (let [app-url (jsi/get-in js/window [:location :origin])]
    (str (uri/join app-url "./resources/swipl/prelude.qlf"))))

(uix/defui query-button
  [{:keys [cm-editor-ref
           set-transpiled-prolog! set-query-results!
           button-props children]}]
  (let [[queries-running? set-queries-running!] (uix/use-state false)
        query-fn!
        (fn []
          (set-queries-running! true)
          (set-query-results! [])
          (set-transpiled-prolog! nil)

          (let [l4-program (-> @cm-editor-ref
                               (jsi/get-in [:view :state :doc])
                               str)
                query-worker (new js/Worker "/js/l4_ide/query_worker.js"
                                  #js {:type "module"})]
            (jsi/assoc!
             query-worker :onmessage
             (jsi/fn [^:js {:keys [data]}]
               (m/match data
                 #js {:tag (m/some "transpiled-prolog")
                      :payload (m/some ?transpiled-prolog)}
                 (set-transpiled-prolog! ?transpiled-prolog)

                 #js {:tag (m/some "query-result")
                      :payload (m/some ?query-result)}
                 (set-query-results! #(conj % ?query-result))

                 #js {:tag (m/some "done")}
                 (set-queries-running! false))))

            (jsi/call query-worker :postMessage
                      #js {:l4-program l4-program
                           :swipl-prelude-qlf-url swipl-prelude-qlf-url}))

          #_(let [cm-editor-doc (jsi/get-in @cm-editor-ref [:view :state :doc])
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
                  {:loading queries-running? :on-click query-fn!})
           children)))