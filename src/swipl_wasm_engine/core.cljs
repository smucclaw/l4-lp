(ns swipl-wasm-engine.core
  (:require [applied-science.js-interop :as jsi]
            [cljs-bean.core :as bean]
            [promesa.core :as prom]
            [shadow.esm :refer [dynamic-import]]))

(def ^:private swipl-wasm-cdn-url
  "https://SWI-Prolog.github.io/npm-swipl-wasm/3/7/0/dynamic-import.js")

(def ^:private prelude-qlf-url
  "resources/swipl/prelude.qlf")

;; https://swi-prolog.discourse.group/t/clojure-clojurescript-with-swi-prolog/5399/4
;; https://github.com/SWI-Prolog/roadmap/issues/43

(defn- fn->functional-interface [func]
  (new #(this-as this (jsi/assoc! this :apply func))))

(defn eval-and-trace [program-str goal-str]
  (prom/let
   [^js swipl-mod (dynamic-import swipl-wasm-cdn-url)
    swipl (-> swipl-mod
              (jsi/get :SWIPL)
              (new (bean/->js {:arguments ["-q"]})))

    stack-trace (transient [])

   ;; Ugly hack to get swipl wasm working on nodejs.
   ;; The issue is that it fails to load prolog and qlf files on nodejs via Prolog.consult
   ;; with following error:
   ;; ERROR: JavaScript: ReferenceError: window is not defined
   ;; To solve this, we assign a global window object to an empty object just so
   ;; that it's defined.
    _ (when-not (or (exists? js/window)
                    (jsi/get js/globalThis :window))
        (jsi/assoc! js/globalThis :window #js {}))

    _ (jsi/call-in swipl [:prolog :consult] prelude-qlf-url)

    ;; Need to wrap functions by an opaque, non-plain JS object with a method
    ;; that runs the original function.
    ;; This is because if we pass in a plain object, or plain lambda, swipl-wasm
    ;; treats them both as plain objects and tries to recursively convert it to
    ;; a Prolog dictionary, which fails.
    log-stack-frame-callback
    (-> (partial conj! stack-trace) fn->functional-interface)

    assert-callback-fn-query
    (jsi/call-in swipl [:prolog :query]
                 "asserta((js_log_stack_frame_callback(Func) :- !))"
                 #js {:Func log-stack-frame-callback})
    _ (jsi/call assert-callback-fn-query :once)

    _ (jsi/call-in swipl [:prolog :load_string]
                   "log_stack_frame(StackFrame) :- !,
                      js_log_stack_frame_callback(Func),
                      _ := Func.apply(StackFrame).")

    _ (jsi/call-in swipl [:prolog :load_string] program-str)

    query (jsi/call-in swipl [:prolog :query]
                       "eval_and_trace(Goal)" #js {:Goal goal-str})

    _ (jsi/call query :once)]

    ;; (jsi/call js/console :log "Loaded Swipl Mod: " swipl-mod)
    ;; (jsi/call js/console :log "SWIPL: " swipl)
    
    (-> stack-trace persistent! bean/->js)))