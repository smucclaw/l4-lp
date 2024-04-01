(ns l4-lp.webeditor
  (:require ["@codemirror/commands" :as cm-cmds]
            ["@codemirror/language" :as cm-lang]
            ["@codemirror/state" :as cm-state]
            ["@codemirror/view" :as cm-view]
            ["@nextjournal/clojure-mode" :as cm-clj]
            [applied-science.js-interop :as jsi]
            [l4-lp.swipl.js.wasm-query :as swipl-wasm-query]
            [l4-lp.syntax.l4-to-prolog :as l4->prolog]
            [promesa.core :as prom]))

;; https://blog.jakubholy.net/2023/interactive-code-snippets-fulcro/
;; https://github.com/nextjournal/clojure-mode

(def extensions
  #js [(cm-cmds/history)
       (cm-lang/syntaxHighlighting cm-lang/defaultHighlightStyle)
       (cm-view/drawSelection)
       (cm-view/lineNumbers)
       (jsi/call-in cm-state/EditorState [:allowMultipleSelections :of] true)
       (jsi/call cm-view/keymap :of cm-clj/complete_keymap)
       (jsi/call cm-view/keymap :of cm-cmds/historyKeymap)
       cm-clj/default_extensions])

(def ^:private guifier-cdn-url
  "https://cdn.jsdelivr.net/npm/guifier@1.0.24/dist/Guifier.js")

(def guifier-constructor
  (atom nil))

#_(defn query-and-trace-and-guifier! []
  (prom/let
   [l4-program @l4-program
    l4-query @l4-query

    guifier-div (jsi/call js/document :getElementById "guifier")
    _ (jsi/assoc! guifier-div :innerHTML "")

    program (-> l4-program l4->prolog/l4-program->prolog-program-str)
    _ (jsi/call js/console :log "Transpiled program: " program)

    query (-> l4-query l4->prolog/l4->prolog-str)
    _ (jsi/call js/console :log "Transpiled query: " query)

    stack-trace (swipl-wasm-query/query-and-trace-js! program query)

    Guifier @guifier-constructor]
    (Guifier. #js {:data stack-trace
                   :dataType "js"
                   :elementSelector "#guifier"
                   :withoutContainer true
                   :readOnlyMode true})))

(def eval-extension)

(defn bind-editor! [editor-elt guifier-elt code]
  (let [state (jsi/call cm-state/EditorState
                       :create
                       #js {:doc code
                            :extensions extensions})]
    (cm-view/EditorView.
     #js {:parent editor-elt
          :state state})))

#_(defn output-panel-extension [result-atom]
  (let [dom (js/document.createElement "div")]
    (add-watch result-atom :output-panel
               (fn [_ _ _ result]
                 (if (::error result)
                   (do
                     (jsi/call-in dom [:classList :add] "error")
                     (jsi/assoc!
                      dom :textContent (str "ERROR: " (::error result))))
                   (do
                     (jsi/call-in dom [:classList :remove] "error")
                     (jsi/assoc! dom :textContent (str ";; => " (pr-str result)))))))
    (set! (.-className dom) "cm-output-panel")
    (.of cm-view/showPanel (fn [_] #js {:dom dom}))))

(def editor-element
  (jsi/call js/document :querySelector "#editor"))

(defn start []
  (jsi/call js/console :log "Starting..."))

(defn stop []
  (jsi/call js/console :log "Stopping..."))

(defn init []
  (bind-editor! editor-element nil "nil")
  (start))