(ns l4-lp.web-editor.core
  (:require ["@mui/icons-material/GitHub$default" :as GitHubIcon]
            ["@mui/icons-material/Send$default" :as SendIcon]
            ["@mui/lab/LoadingButton$default" :as LoadingButton]
            ["@mui/material/AppBar$default" :as AppBar]
            ["@mui/material/Box$default" :as Box]
            ["@mui/material/Link$default" :as Link]
            ["@mui/material/Toolbar$default" :as Toolbar]
            ["@mui/material/Typography$default" :as Typography]
            ["@mui/material/Unstable_Grid2$default" :as Grid]
            ["react-dom" :as react-dom]
            [applied-science.js-interop :as jsi]
            [l4-lp.swipl.js.wasm-query :as swipl-wasm-query]
            [l4-lp.syntax.l4-to-prolog :as l4->prolog]
            [l4-lp.web-editor.editor :as editor]
            [l4-lp.web-editor.query-output :as query-output]
            [promesa.core :as prom]
            [uix.core :as uix]
            [uix.dom :as uix-dom]))

(def ^:private editor-preamble-url
 "web_editor_preamble.edn")

(def ^:private editor-instrs-url
  "web_editor_instructions.txt")

(def ^:private web-editor-app-id
  "web-editor-app")

(uix/defui top-bar
  [{:keys [queries-running? cm-editor-ref query-fn]}]
  (uix/$ Box {:flex-grow 1}
         (uix/$
          AppBar {:position :fixed}
          (uix/$ Toolbar {:variant :dense}
                 (uix/$ Link {:color :inherit
                              :href "https://github.com/smucclaw/l4-lp"}
                        (uix/$ GitHubIcon))
                 (uix/$ Typography {:mt 1 :ml 5 :mr 5
                                    :variant :h5
                                    :gutter-bottom true}
                        "L4 web editor")
                 (uix/$ LoadingButton
                        {:sx #js {:ml 3}
                         :loading queries-running?
                         :variant :contained
                         :color :inherit
                         :size :medium
                         :end-icon (uix/$ SendIcon)
                         :on-click #(query-fn cm-editor-ref)}
                        "Run Queries")))
         (uix/$ Toolbar)))

(uix/defui mui-fonts
  "Component to load fonts for Material UI."
  []
  (uix/$
   :div
   (react-dom/preconnect "https://fonts.googleapis.com")
   (react-dom/preconnect "https://fonts.gstatic.com"
                         #js {:crossOrigin "anonymous"})
   (uix/$ :link {:rel :stylesheet
                 :href "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;500;700&display=swap"})))

(uix/defui web-editor-app []
  (let [cm-editor-ref (uix/use-ref)
        [queries-running? set-queries-running!] (uix/use-state false)
        [prolog-program-and-queries set-prolog!] (uix/use-state nil)
        [query-results set-query-results!] (uix/use-state [])

        query-fn
        (fn [cm-editor-ref]
          (set-queries-running! true)
          (set-query-results! [])
          (set-prolog! nil)

          (let [cm-editor-doc (jsi/get-in @cm-editor-ref [:view :state :doc])
                prolog-program-and-queries
                (-> cm-editor-doc str l4->prolog/l4->prolog-program+queries)]
            (prom/do
              (set-prolog! prolog-program-and-queries)

              (swipl-wasm-query/query-and-trace!
               prolog-program-and-queries
               (fn [result] (set-query-results! #(conj % result))))

              (set-queries-running! false))))]
    (uix/$ Box
           (uix/$ :title "L4 web editor")
           (uix/$ mui-fonts)

           (uix/$ top-bar
                  {:cm-editor-ref cm-editor-ref
                   :queries-running? queries-running?
                   :query-fn query-fn})

           (uix/$ Grid {:container true
                        :spacing 2}
                  (uix/$ Grid {:ml 2 :mr 2}
                         (uix/$ editor/editor
                                {:max-editor-height :85vh
                                 :editor-ref cm-editor-ref
                                 :editor-preamble-url editor-preamble-url
                                 :editor-instrs-url editor-instrs-url}))
                  (uix/$ Grid {:mt 2 :ml 2 :mr 2}
                         (uix/$ query-output/query-output
                                {:max-height :85vh
                                 :prolog-program-and-queries prolog-program-and-queries
                                 :query-results query-results}))))))

(defn- render-web-editor-app! []
  (let [app-root
        (-> js/document
            (jsi/call :getElementById web-editor-app-id)
            uix-dom/create-root)]
    (uix-dom/render-root (uix/$ web-editor-app) app-root)))

(defn start! []
  (println "Starting..."))

(defn stop! []
  (println "Stopping..."))

(defn init! []
  (render-web-editor-app!)
  (start!))