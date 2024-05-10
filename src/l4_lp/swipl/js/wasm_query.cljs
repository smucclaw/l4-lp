(ns l4-lp.swipl.js.wasm-query 
  (:require ["https://SWI-Prolog.github.io/npm-swipl-wasm/3/7/11/dynamic-import.js"
             :rename {SWIPL Swipl}]
            [applied-science.js-interop :as jsi]
            [cljs-bean.core :as bean]
            [l4-lp.swipl.js.common.swipl-js-to-clj :as swipl-js->clj]
            [promesa.core :as prom]
            [tupelo.core :refer [it->]]))

(def ^:private prelude-qlf-url
  "resources/swipl/prelude.qlf")

(defn- fn->non-plain-obj [f]
  (new #(this-as this (jsi/assoc! this :apply f))))

;; TODO: Document and clean up this function.
(defn query-and-trace! [{program :program queries :queries}]
  (prom/let
   [swipl (Swipl. #js {:arguments #js ["-q"]})

    stack-trace (atom nil)

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
    (fn->non-plain-obj #(conj! @stack-trace %))

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

    run-query! (fn [query]
                 (reset! stack-trace (transient []))
                 (let [swipl-query (jsi/call-in swipl [:prolog :query]
                                                (str "once_trace_all(" query ")"))
                       query-result (jsi/call swipl-query :once)]
                   {:query query
                    :trace (-> @stack-trace
                               persistent!
                               swipl-js->clj/swipl-stack-trace->clj)
                    :bindings (-> query-result
                                  swipl-js->clj/swipl-query-result->bindings)}))]

    (->> queries (eduction (map run-query!)) prom/all)))

(defn query-and-trace-js! [prolog-program+query]
  (->> prolog-program+query
       bean/->clj
       query-and-trace!
       (prom/map bean/->js)))