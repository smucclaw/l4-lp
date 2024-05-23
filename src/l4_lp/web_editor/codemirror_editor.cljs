(ns l4-lp.web-editor.codemirror-editor
  (:require ["@codemirror/commands" :as cm-cmds]
            ["@codemirror/language" :as cm-lang]
            ["@codemirror/state" :as cm-state]
            ["@codemirror/view" :as cm-view]
            ["@nextjournal/clojure-mode" :as cm-clj]
            ["@uiw/codemirror-theme-solarized" :as cm-solarized]
            [applied-science.js-interop :as jsi]
            [l4-lp.web-editor.guifier :refer [query-and-trace-and-guifier!]]
            [lambdaisland.fetch :as fetch]
            [promesa.core :as prom]))

;; https://blog.jakubholy.net/2023/interactive-code-snippets-fulcro/
;; https://github.com/nextjournal/clojure-mode

(defn ^:private eval-query! [cm-editor-view]
  (-> cm-editor-view
      (jsi/get-in [:state :doc])
      str
      query-and-trace-and-guifier!)
  true)

(def ^:private eval-query-extension
  (jsi/call cm-view/keymap :of
            #js [#js {:key "Mod-Enter" :run eval-query!}]))

(def ^:private theme
  (jsi/call cm-view/EditorView :theme
            #js {:& #js {:fontSize "14pt"}
                 :.cm-content #js {:fontFamily "Lucida Console"}}))

(def ^:private extensions
  #js [cm-solarized/solarizedLight
       theme
       eval-query-extension
       (cm-cmds/history)
       (cm-lang/syntaxHighlighting cm-lang/defaultHighlightStyle)
       (cm-view/drawSelection)
       (cm-view/lineNumbers)
       (jsi/call-in cm-state/EditorState [:allowMultipleSelections :of] true)
       (jsi/call cm-view/keymap :of cm-clj/complete_keymap)
       (jsi/call cm-view/keymap :of cm-cmds/historyKeymap)
       cm-clj/default_extensions])

(defn bind-editor! [editor-elt preamble-url]
  (prom/let
   [{preamble-text :body} (fetch/get preamble-url {:content-type :text})
    state (jsi/call cm-state/EditorState
                    :create
                    #js {:doc preamble-text :extensions extensions})]
    (new cm-view/EditorView #js {:parent editor-elt :state state})))