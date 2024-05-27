(ns l4-lp.ide.ui.core 
  (:require ["@mui/icons-material/Send$default" :as SendIcon]
            ["@mui/material/Box$default" :as Box]
            ["@mui/material/Unstable_Grid2$default" :as Grid]
            ["react-dom" :as react-dom]
            [applied-science.js-interop :as jsi]
            [l4-lp.ide.ui.editor :refer [editor]]
            [l4-lp.ide.ui.ide-instrs :refer [ide-instrs]]
            [l4-lp.ide.ui.query.button :refer [query-button]]
            [l4-lp.ide.ui.query.output.core :refer [query-output]]
            [l4-lp.ide.ui.top-bar :refer [top-bar]]
            [uix.core :as uix]
            [uix.dom :as uix-dom]))

(uix/defui ^:private mui-fonts
  "Component to load fonts for Material UI."
  []
  (uix/$ :div
         (react-dom/preconnect "https://fonts.googleapis.com")
         (react-dom/preconnect "https://fonts.gstatic.com"
                               #js {:crossOrigin "anonymous"})
         (uix/$ :link {:rel :stylesheet
                       :href "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;500;700&display=swap"})))

(uix/defui ^:private top-bar-and-query-button
  [{:keys [cm-editor-ref set-transpiled-prolog! set-query-results!]}]
  (uix/$ top-bar
         (uix/$ query-button
                {:cm-editor-ref cm-editor-ref
                 :set-transpiled-prolog! set-transpiled-prolog!
                 :set-query-results! set-query-results!
                 :button-props {:sx #js {:ml 3}
                                :variant :contained
                                :color :inherit
                                :size :medium
                                :end-icon (uix/$ SendIcon)}}
                "Run Queries")))

(uix/defui ^:private ide-grid
  [{:keys [cm-editor-ref transpiled-prolog query-results]}]
  (uix/$ Grid {:container true}
         (uix/$ Grid {:ml 2 :mr 2}
                (uix/$ ide-instrs {:sx #js {:mb 2} :max-text-width :md})
                (uix/$ editor
                       {:max-height :85vh
                        :font-size :14pt
                        :ref cm-editor-ref}))
         (uix/$ Grid {:mt 2 :ml 2 :mr 2}
                (uix/$ query-output
                       {:max-height :85vh
                        :transpiled-prolog transpiled-prolog
                        :query-results query-results}))))

(uix/defui ^:private ide-app []
  (let [cm-editor-ref (uix/use-ref)
        [transpiled-prolog set-transpiled-prolog!] (uix/use-state nil)
        [query-results set-query-results!] (uix/use-state nil)]
    (uix/$ Box
           (uix/$ :title "L4 IDE")
           (uix/$ mui-fonts)

           (uix/$ top-bar-and-query-button
                  {:cm-editor-ref cm-editor-ref
                   :set-transpiled-prolog! set-transpiled-prolog!
                   :set-query-results! set-query-results!})

           (uix/$ ide-grid
                  {:cm-editor-ref cm-editor-ref
                   :transpiled-prolog transpiled-prolog
                   :query-results query-results}))))

(defn render-ide-app! [ide-app-id]
  (let [app-root
        (-> js/document
            (jsi/call :getElementById ide-app-id)
            uix-dom/create-root)]
    (uix-dom/render-root (uix/$ ide-app) app-root)))