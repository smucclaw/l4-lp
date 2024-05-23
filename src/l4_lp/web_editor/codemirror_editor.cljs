(ns l4-lp.web-editor.codemirror-editor
  (:require ["@codemirror/commands" :as cm-cmds]
            ["@codemirror/language" :as cm-lang]
            ["@codemirror/state" :as cm-state]
            ["@codemirror/view" :as cm-view]
            ["@nextjournal/clojure-mode" :as cm-clj]
            ["@uiw/codemirror-theme-solarized" :as cm-solarized]
            [applied-science.js-interop :as jsi]
            [lambdaisland.fetch :as fetch]
            [promesa.core :as prom]
            [tupelo.core :refer [it->]]
            [l4-lp.web-editor.guifier :as guifier]))

;; https://blog.jakubholy.net/2023/interactive-code-snippets-fulcro/
;; https://github.com/nextjournal/clojure-mode

(defn- eval-and-trace-query! [guifier cm-editor-view]
  (it-> cm-editor-view
      (jsi/get-in it [:state :doc])
      (str it)
      (guifier/query-and-trace-and-guifier! guifier it))
  true)

(defn- eval-and-trace-query-ext [guifier]
  (jsi/call cm-view/keymap :of
            #js [#js {:key "Mod-Enter"
                      :run #(eval-and-trace-query! guifier %)}]))

(def ^:private theme
  (jsi/call cm-view/EditorView :theme
            #js {:& #js {:fontSize "14pt"}
                 :.cm-content #js {:fontFamily "Lucida Console"}}))

(defn- exts [guifier]
  #js [cm-solarized/solarizedLight
       theme
       (eval-and-trace-query-ext guifier)
       (cm-cmds/history)
       (cm-lang/syntaxHighlighting cm-lang/defaultHighlightStyle)
       (cm-view/drawSelection)
       (cm-view/lineNumbers)
       (jsi/call-in cm-state/EditorState [:allowMultipleSelections :of] true)
       (jsi/call cm-view/keymap :of cm-clj/complete_keymap)
       (jsi/call cm-view/keymap :of cm-cmds/historyKeymap)
       cm-clj/default_extensions])

(defn bind-editor!
  [{:keys [editor-preamble-url editor-elt guifier-elt-id]}]
  (prom/let
   [guifier (guifier/init-guifier! guifier-elt-id)
    {preamble-text :body} (fetch/get editor-preamble-url {:content-type :text})
    state (jsi/call cm-state/EditorState
                    :create
                    #js {:doc preamble-text :extensions (exts guifier)})]
    (new cm-view/EditorView #js {:parent editor-elt :state state})))