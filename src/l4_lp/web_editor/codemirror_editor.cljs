(ns l4-lp.web-editor.codemirror-editor
  (:require ["@codemirror/view" :as cm-view]
            ["@nextjournal/clojure-mode" :as cm-clj]
            ["@uiw/codemirror-theme-solarized" :as cm-solarized]
            ["@uiw/react-codemirror$default" :as CodeMirror]
            [applied-science.js-interop :as jsi]
            [l4-lp.web-editor.utils :refer [fetch-text-from-url-and-then!]]
            [uix.core :as uix]))

;; https://blog.jakubholy.net/2023/interactive-code-snippets-fulcro/
;; https://github.com/nextjournal/clojure-mode

(defn- set-editor-text! [editor-view text]
  (jsi/call editor-view :dispatch
            #js {:changes #js {:from 0
                               :to (jsi/get-in editor-view
                                               [:state :doc :length])
                               :insert text}}))

(uix/defui cm-editor
  [{:keys [ref editor-preamble-url max-height font-size]}]
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
              (fetch-text-from-url-and-then!
               editor-preamble-url
               #(set-editor-text! editor-view %)))})))