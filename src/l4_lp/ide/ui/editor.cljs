(ns l4-lp.ide.ui.editor
  (:require ["@codemirror/view" :as cm-view]
            ["@nextjournal/clojure-mode" :as cm-clj]
            ["@uiw/codemirror-theme-solarized" :as cm-solarized]
            ["@uiw/react-codemirror$default" :as CodeMirror]
            [applied-science.js-interop :as jsi]
            [l4-lp.ide.ui.utils :refer [memoised-fetch-as-text!
                                        suspense-loading-bar]]
            [promesa.core :as prom]
            [uix.core :as uix]))

;; https://blog.jakubholy.net/2023/interactive-code-snippets-fulcro/
;; https://github.com/nextjournal/clojure-mode

(def ^:private sample-program-url
 "ide_sample_program.edn")

(defn- set-editor-text! [editor-view text]
  (let [editor-doc-length (jsi/get-in editor-view [:state :doc :length])]
    (jsi/call editor-view :dispatch
              #js {:changes #js {:from 0 :to editor-doc-length
                                 :insert text}})))

(defn- exts [font-size]
  (let [theme (jsi/call cm-view/EditorView :theme
                        #js {:& #js {:font-size (name font-size)}
                             :.cm-content #js {:font-family "Lucida Console"}
                             :.cm-scroller #js {:overflow "auto"}})]
    #js [theme
         (jsi/call cm-view/keymap :of cm-clj/complete_keymap)
         cm-clj/default_extensions]))

(uix/defui editor
  [{:keys [ref max-height font-size]}]
  (let [preamble-text (memoised-fetch-as-text! sample-program-url)
        exts (uix/use-memo #(exts font-size) [font-size])

        ;; https://github.com/uiwjs/react-codemirror/issues/314
        #_ref-callback-fn
        #_(uix/use-callback
           #(jsi/let [^:js {:keys [editor state view] :as cm-editor} %]
              (when (and editor state view)
                (prom/then editor-preamble-text
                           (partial set-editor-text! view))
                (reset! ref cm-editor))

              [ref editor-preamble-text]))]

    (uix/$ suspense-loading-bar
           (uix/$ CodeMirror
                  {:theme cm-solarized/solarizedLight
                   :extensions exts
                   :basic-setup true
                   :max-height max-height
                   :ref ref #_ref-callback-fn
                   :on-create-editor
                   (fn [editor-view _editor-state]
                     (prom/bind preamble-text
                                #(set-editor-text! editor-view %)))}))))