(ns l4-lp.web-editor.codemirror-editor
  (:require ["@codemirror/view" :as cm-view]
            ["@mui/icons-material/ExpandMore$default" :as ExpandMoreIcon]
            ["@mui/material/Accordion$default" :as Accordion]
            ["@mui/material/AccordionDetails$default" :as AccordionDetails]
            ["@mui/material/AccordionSummary$default" :as AccordionSummary]
            ["@mui/material/Box$default" :as Box]
            ["@mui/material/Typography$default" :as Typography]
            ["@nextjournal/clojure-mode" :as cm-clj]
            ["@uiw/codemirror-theme-solarized" :as cm-solarized]
            ["@uiw/react-codemirror$default" :as CodeMirror]
            [applied-science.js-interop :as jsi]
            [l4-lp.web-editor.utils :refer [fetch-as-text! loading-bar]]
            [promesa.core :as prom]
            [uix.core :as uix]))

;; https://blog.jakubholy.net/2023/interactive-code-snippets-fulcro/
;; https://github.com/nextjournal/clojure-mode

(defn- set-editor-text! [editor-view text]
  (let [editor-doc-length (jsi/get-in editor-view [:state :doc :length])]
    (jsi/call editor-view :dispatch
              #js {:changes #js {:from 0 :to editor-doc-length :insert text}})))

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

(uix/defui cm-editor-and-instrs
  [{:keys [cm-editor-ref max-editor-height
           editor-instrs-url editor-preamble-url]}]

  (uix/$ Box
         (uix/$
          Box {:ml 2 :mr 2 :mb 2}
          (uix/$ Accordion
                 (uix/$ AccordionSummary
                        {:expand-icon (uix/$ ExpandMoreIcon)
                         :aria-controls :web-editor-instrs-control
                         :id :web-editor-instrs}
                        (uix/$ Typography {:variant :h6} "Usage instructions"))
                 (uix/$ AccordionDetails
                        (uix/$ Typography {:max-width :md
                                           :variant :body1
                                           :white-space :pre-line}
                               (uix/$ loading-bar
                                      (fetch-as-text! editor-instrs-url))))))
         (uix/$ loading-bar
                (uix/$ cm-editor
                       {:ref cm-editor-ref
                        :max-height max-editor-height
                        :font-size :14pt
                        :editor-preamble-text
                        (fetch-as-text! editor-preamble-url)}))))