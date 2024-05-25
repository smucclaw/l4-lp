(ns l4-lp.web-editor.editor
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
            [l4-lp.web-editor.utils :refer [memoised-fetch-as-text!
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

(defn- exts [font-size]
  (let [theme (jsi/call cm-view/EditorView :theme
                        #js {:& #js {:font-size (name font-size)}
                             :.cm-content #js {:font-family "Lucida Console"}
                             :.cm-scroller #js {:overflow "auto"}})]
    #js [theme
         (jsi/call cm-view/keymap :of cm-clj/complete_keymap)
         cm-clj/default_extensions]))

(uix/defui cm-editor
  [{:keys [ref editor-preamble-text max-height font-size]}]
  (let [exts (uix/use-memo #(exts font-size) [font-size])

        ;; https://github.com/uiwjs/react-codemirror/issues/314
        #_ref-callback-fn
        #_(uix/use-callback
         #(jsi/let [^:js {:keys [editor state view] :as cm-editor} %]
            (when (and editor state view)
              (prom/then editor-preamble-text
                         (partial set-editor-text! view))
              (reset! ref cm-editor))

            [ref editor-preamble-text]))]

    (uix/$ CodeMirror
           {:theme cm-solarized/solarizedLight
            :extensions exts
            :basic-setup true
            :max-height max-height
            :ref ref #_ref-callback-fn
            :on-create-editor
            (fn [editor-view _editor-state]
              (prom/then editor-preamble-text
                         #(set-editor-text! editor-view %)))})))

(uix/defui editor-instrs
  [{:keys [editor-instrs-text
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
                              editor-instrs-text)))))

(uix/defui editor
  [{:keys [max-editor-height
           editor-ref
           editor-instrs-url editor-preamble-url]}]
  (let [editor-instrs-text
        (memoised-fetch-as-text! editor-instrs-url)
        editor-preamble-text
        (memoised-fetch-as-text! editor-preamble-url)]
    (uix/$ Box
           (uix/$ editor-instrs
                  {:max-text-width :sm
                   :editor-instrs-text editor-instrs-text})
           (uix/$ Box {:mt 2}
                  (uix/$ suspense-loading-bar
                         (uix/$ cm-editor
                                {:ref editor-ref
                                 :max-height max-editor-height
                                 :font-size :14pt
                                 :editor-preamble-text
                                 editor-preamble-text}))))))