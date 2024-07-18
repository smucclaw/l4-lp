(ns l4-lp.ide.ui.editor
  (:require ["@codemirror/view" :as cm-view]
            ["@nextjournal/clojure-mode" :as cm-clj]
            ["@uiw/codemirror-theme-solarized" :as cm-solarized]
            ["@uiw/react-codemirror$default" :as CodeMirror]
            [applied-science.js-interop :as jsi]
            [l4-lp.ide.ui.utils :refer [use-fetch-as-text!]]
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
  (let [{sample-program-text :text sample-program-fetched? :fetched?}
        (use-fetch-as-text! sample-program-url)
         exts (uix/use-memo #(exts font-size) [font-size])

        ;; https://github.com/uiwjs/react-codemirror/issues/314
         editor-callback-ref
         (uix/use-callback
          #(jsi/let [^:js {:keys [editor state view] :as cm-editor} %]
             (when (and editor state view sample-program-fetched?)
               (set-editor-text! view sample-program-text)
               (reset! ref cm-editor)))

          [ref sample-program-text sample-program-fetched?])]

    (uix/$ CodeMirror
           {:theme cm-solarized/solarizedLight
            :extensions exts
            :basic-setup true
            :max-height max-height
            :ref editor-callback-ref
            :value ";; Loading sample program..."})))