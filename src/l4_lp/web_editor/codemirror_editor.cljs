(ns l4-lp.web-editor.codemirror-editor
  (:require ["@codemirror/view" :as cm-view]
            ["@nextjournal/clojure-mode" :as cm-clj]
            ["@uiw/codemirror-theme-solarized" :as cm-solarized]
            ["@uiw/react-codemirror$default" :as CodeMirror]
            [applied-science.js-interop :as jsi]
            [l4-lp.web-editor.utils :refer [fetch-text-from-url-and-do!]]
            [uix.core :as uix]))

;; https://blog.jakubholy.net/2023/interactive-code-snippets-fulcro/
;; https://github.com/nextjournal/clojure-mode

(def ^:private theme
  (jsi/call cm-view/EditorView :theme
            #js {:& #js {:fontSize "14pt"
                         :max-height "70vh"}
                 :.cm-content #js {:fontFamily "Lucida Console"}
                 :.cm-scroller #js {:overflow "auto"}}))

(def ^:private exts
  #js [theme
       (jsi/call cm-view/keymap :of cm-clj/complete_keymap)
       cm-clj/default_extensions])

(defn- set-editor-text! [editor-view text]
  (jsi/call editor-view :dispatch
            #js {:changes #js {:from 0
                               :to (jsi/get-in editor-view
                                               [:state :doc :length])
                               :insert text}}))

(uix/defui CodeMirrorEditor
  [{:keys [ref editor-preamble-url]}]
  (uix/$ CodeMirror
         {:theme cm-solarized/solarizedLight
          :extensions exts
          :basicSetup true
          :ref ref
          :on-create-editor
          (fn [editor-view _editor-state]
            (fetch-text-from-url-and-do!
             editor-preamble-url
             #(set-editor-text! editor-view %)))}))