(ns l4-lp.webeditor
  (:require ["@nextjournal/clojure-mode"
             :refer [default_extensions, complete_keymap]]
            ["@codemirror/view" :refer [EditorView drawSelection keymap]]
            ["@codemirror/state" :refer [EditorState]]
            [applied-science.js-interop :as jsi]
            [cljs-bean.core :as bean]))

;; https://blog.jakubholy.net/2023/interactive-code-snippets-fulcro/
;; https://github.com/nextjournal/clojure-mode

(def extensions
  (->> default_extensions
       (cons (jsi/call keymap :of complete_keymap))
       bean/->js))

(def state
  (jsi/call EditorState
            :create #js {:doc "some clojure code"
                         :extensions extensions}))

(def editor-element
  (jsi/call js/document :querySelector "#editor"))

(def editor
  (EditorView. #js {:state state
                    :parent editor-element
                    :extensions extensions}))

(defn start []
  (jsi/call js/console :log "Starting..."))

(defn stop []
  (jsi/call js/console :log "Stopping..."))

(defn init []
  (start))