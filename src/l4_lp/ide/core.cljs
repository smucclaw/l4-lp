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

        [query-worker-state post-js-query! query-output-chan]
        (use-web-worker! backend/worker-js-url)

        [transpiled-prolog set-transpiled-prolog!] (uix/use-state nil)
        [query-results set-query-results!] (uix/use-state [])]

    (uix/use-effect
     #(prom/chain query-output-chan
                  csp/take
                  (fn [data]
                    (backend/on-data-from-worker
                     data
                     :on-transpiled-prolog set-transpiled-prolog!
                     :on-query-result
                     (fn [result]
                       (set-query-results! (fn [results]
                                             (conj results result)))))))
     [query-output-chan])

    (uix/$ Box
           (uix/$ :title "L4 IDE")
           (uix/$ ui/mui-fonts)

           (uix/$ ui/top-bar-and-query-button
                  {:button-loading? (not= query-worker-state :ready)
                   :on-button-click
                   (fn []
                     (set-query-results! [])
                     (set-transpiled-prolog! nil)

                     (-> @cm-editor-ref
                         (jsi/get-in [:view :state :doc])
                         str
                         backend/l4-program->worker-data
                         post-js-query!))})

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