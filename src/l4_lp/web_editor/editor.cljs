(ns l4-lp.web-editor.editor
  (:require ["@codemirror/view" :as cm-view]
            ["@mui/icons-material/ExpandMore$default" :as ExpandMoreIcon]
            ["@mui/icons-material/Send$default" :as SendIcon]
            ["@mui/lab/LoadingButton$default" :as LoadingButton]
            ["@mui/material/Accordion$default" :as Accordion]
            ["@mui/material/AccordionDetails$default" :as AccordionDetails]
            ["@mui/material/AccordionSummary$default" :as AccordionSummary]
            ["@mui/material/Box$default" :as Box]
            ["@mui/material/Typography$default" :as Typography]
            ["@mui/material/Unstable_Grid2$default" :as Grid]
            ["@nextjournal/clojure-mode" :as cm-clj]
            ["@uiw/codemirror-theme-solarized" :as cm-solarized]
            ["@uiw/react-codemirror$default" :as CodeMirror]
            [applied-science.js-interop :as jsi]
            [l4-lp.web-editor.utils :refer [fetch-as-text!
                                            suspense-loading-bar]]
            [promesa.core :as prom]
            [uix.core :as uix]))

;; https://blog.jakubholy.net/2023/interactive-code-snippets-fulcro/
;; https://github.com/nextjournal/clojure-mode

(defn- set-editor-text! [editor-view text]
  (let [editor-doc-length (jsi/get-in editor-view [:state :doc :length])]
    (jsi/call editor-view :dispatch
              #js {:changes #js {:from 0
                                 :to editor-doc-length
                                 :insert text}})))

(uix/defui cm-editor
  [{:keys [ref editor-preamble-text max-height font-size]}]
  (let [theme (jsi/call cm-view/EditorView :theme
                        #js {:& #js {:font-size (name font-size)}
                             :.cm-content #js {:font-family "Lucida Console"}
                             :.cm-scroller #js {:overflow "auto"}})
        exts #js [theme
                  (jsi/call cm-view/keymap :of cm-clj/complete_keymap)
                  cm-clj/default_extensions]]

    (uix/$ CodeMirror
           {:theme cm-solarized/solarizedLight
            :extensions exts
            :basic-setup true
            :max-height max-height
            :ref ref
            :on-create-editor
            (fn [editor-view _editor-state]
              (-> editor-preamble-text
                  (prom/then #(set-editor-text! editor-view %))))})))

(uix/defui editor-instrs
  [{:keys [editor-instrs-url
           max-text-width]}]
  (uix/$ Accordion
         (uix/$ AccordionSummary
                {:expand-icon (uix/$ ExpandMoreIcon)
                 :aria-controls :panel-content
                 :id :web-editor-instrs}
                (uix/$ Typography {:variant :h6} "Usage instructions"))
         (uix/$ AccordionDetails
                (uix/$ suspense-loading-bar
                       (uix/$ Typography {:max-width max-text-width
                                          :variant :body1
                                          :white-space :pre-line}
                              (fetch-as-text! editor-instrs-url))))))

(uix/defui editor
  [{:keys [max-editor-height
           editor-ref
           editor-instrs-url editor-preamble-url]}]
  (uix/$ Box
         (uix/$ editor-instrs
                {:max-text-width :sm
                 :editor-instrs-url editor-instrs-url})
         (uix/$ Box {:mt 2}
                (uix/$ suspense-loading-bar
                       (uix/$ cm-editor
                              {:ref editor-ref
                               :max-height max-editor-height
                               :font-size :14pt
                               :editor-preamble-text
                               (fetch-as-text! editor-preamble-url)})))))