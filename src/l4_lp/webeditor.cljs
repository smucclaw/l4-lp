(ns l4-lp.webeditor
  (:require ["@nextjournal/clojure-mode" :as cm-clj]
            ["@codemirror/view" :as cm-view]
            ["@codemirror/state" :as cm-state]
            [applied-science.js-interop :as jsi]
            [cljs-bean.core :as bean]))

;; https://blog.jakubholy.net/2023/interactive-code-snippets-fulcro/
;; https://github.com/nextjournal/clojure-mode

(def extensions
  (->> cm-clj/default_extensions
       (cons (jsi/call cm-view/keymap :of cm-clj/complete_keymap))
       bean/->js))

(def state
  (jsi/call cm-state/EditorState
            :create #js {:doc "some clojure code"
                         :extensions extensions}))

(def editor-element
  (jsi/call js/document :querySelector "#editor"))

(def editor
  (cm-view/EditorView.
   #js {:state state
        :parent editor-element
        :extensions extensions}))

(defn start []
  (jsi/call js/console :log "Starting..."))

(defn stop []
  (jsi/call js/console :log "Stopping..."))

(defn init []
  (start))