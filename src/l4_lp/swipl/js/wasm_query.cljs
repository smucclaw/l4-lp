(ns l4-lp.swipl.js.wasm-query 
  (:require [applied-science.js-interop :as jsi]
            [cljs-bean.core :as bean]
            [l4-lp.swipl.js.common.swipl-js-to-clj :as swipl-js->clj]
            [promesa.core :as prom]
            ["https://SWI-Prolog.github.io/npm-swipl-wasm/3/7/11/dynamic-import.js"
             :rename {SWIPL Swipl}]))

(def ^:private prelude-qlf-url
  "resources/swipl/prelude.qlf")

(defn- fn->non-plain-obj [f]
  (new #(this-as this (jsi/assoc! this :apply f))))

;; TODO: Document and clean up this function.
(defn query-and-trace! [program goal]
  (prom/let
   [swipl (Swipl. #js {:arguments #js ["-q"]})

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
    (fn->non-plain-obj #(conj! stack-trace %))

    assert-callback-fn-query
    (jsi/call-in swipl [:prolog :query]
                 "asserta(js_log_stack_frame_callback(Func))"
                 #js {:Func log-stack-frame-callback})
    _ (jsi/call assert-callback-fn-query :once)

    ;; This rule invokes the callback function (wrapped as an opaque object)
    ;; from SWI Prolog running in wasm.
    ;; Note that any rule containing the special _ := _ assignment operator
    ;; CANNOT be pre-compiled and loaded in a qlf or buried under an "assert".
    ;; Doing so results in a runtime error.
    _ (jsi/call-in swipl [:prolog :load_string]
                   "log_stack_frame(StackFrame) =>
                      js_log_stack_frame_callback(Func),
                      _ := Func.apply(StackFrame).")

    _ (jsi/call-in swipl [:prolog :load_string] program)

    query (jsi/call-in swipl [:prolog :query]
                       ;; "once_trace_all(Goal)" #js {:Goal goal}
                       (str "once_trace_all(" goal ")"))

    query-result (jsi/call query :once)]

    {:trace (-> stack-trace
                persistent!
                swipl-js->clj/swipl-stack-trace->clj)
     :bindings (-> query-result
                   swipl-js->clj/swipl-query-result->bindings)}))

(defn query-and-trace-js! [program goal]
  (->> (query-and-trace! program goal)
       (prom/map bean/->js)))