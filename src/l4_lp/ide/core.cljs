(ns l4-lp.ide.core 
  (:require ["@mui/material/Box$default" :as Box]
            [applied-science.js-interop :as jsi]
            [l4-lp.ide.browser-backend.core :as backend]
            [l4-lp.ide.ui.core :as ui]
            [l4-lp.ide.ui.utils :refer [render-app! use-web-worker!]]
            [promesa.core :as prom]
            [promesa.exec.csp :as csp]
            [uix.core :as uix]))

(def ^:private ide-app-id
  "ide-app")

(uix/defui ^:private ide-app []
  (let [cm-editor-ref (uix/use-ref)

        {query-worker-state :worker-state
         post-to-query-worker! :post-to-worker
         query-output-chan :output-chan}
        (use-web-worker! backend/worker-js-url)

        [swipl-prelude-url-sent? set-swipl-prelude-url-sent!]
        (uix/use-state false)

        [transpiled-prolog set-transpiled-prolog!] (uix/use-state nil)
        [query-results set-query-results!] (uix/use-state [])]

    (uix/use-effect
     #(when (and (not swipl-prelude-url-sent?)
                 (= query-worker-state :ready))
        (post-to-query-worker! backend/swipl-prelude-url-data)
        (set-swipl-prelude-url-sent! true))
     [query-worker-state swipl-prelude-url-sent? post-to-query-worker!])

    (uix/use-effect
     #(prom/let [query-output-chan query-output-chan
                 data (csp/take query-output-chan)]
        (backend/on-data-from-worker
         data
         :on-transpiled-prolog set-transpiled-prolog!
         :on-query-result
         (fn [result]
           (set-query-results! (fn [results]
                                 (conj results result))))))
     [query-output-chan])

    (uix/$ Box
           (uix/$ :title "L4 IDE")
           (uix/$ ui/mui-fonts)

           (uix/$ ui/top-bar-and-query-button
                  {:button-loading? (and swipl-prelude-url-sent?
                                         (not= query-worker-state :ready))
                   :on-button-click
                   (fn []
                     (set-query-results! [])
                     (set-transpiled-prolog! nil)

                     (-> @cm-editor-ref
                         (jsi/get-in [:view :state :doc])
                         str
                         backend/l4-program->worker-data
                         post-to-query-worker!))})

           (uix/$ ui/ide-grid
                  {:cm-editor-ref cm-editor-ref
                   :transpiled-prolog transpiled-prolog
                   :query-results query-results}))))


(defn start! []
  (println "Starting..."))

(defn stop! []
  (println "Stopping..."))

(defn init! []
  (render-app! ide-app-id (uix/$ ide-app))
  (start!))