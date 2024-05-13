(ns l4-lp.swipl.js.wasm-query 
  (:require ["https://SWI-Prolog.github.io/npm-swipl-wasm/3/7/11/dynamic-import.js"
             :rename {SWIPL Swipl}]
            [applied-science.js-interop :as jsi]
            [cljs-bean.core :as bean]
            [l4-lp.swipl.js.common.swipl-js-to-clj :as swipl-js->clj]
            [l4-lp.utils.promise.monad :as prom-m]
            [promesa.core :as prom]
            [tupelo.core :refer [it->]]))

(def ^:private prelude-qlf-url
  "resources/swipl/prelude.qlf")

(defn- fn->non-plain-obj [f]
  (new #(this-as this (jsi/assoc! this :apply f))))

(defn- run-swipl-query! [swipl query]
  (prom/let
   [stack-trace (transient [])

    ;; Need to wrap functions by an opaque, non-plain JS object with a method
    ;; that runs the original function.
    ;; This is because if we pass in a plain object, or plain lambda, swipl-wasm
    ;; treats them both as plain objects and tries to recursively convert it to
    ;; a Prolog dictionary, which fails.
    log-stack-frame-callback
    (fn->non-plain-obj #(conj! stack-trace %))

    swipl-call-once (fn [& args]
                      (it-> swipl
                            (apply jsi/call-in it [:prolog :query] args)
                            (jsi/call it :once)))

    _ (swipl-call-once "asserta(js_log_stack_frame_callback(Func))"
                       #js {:Func log-stack-frame-callback})

    query-result (swipl-call-once (str "once_trace_all(" query ")"))

    _ (swipl-call-once "retractall(js_log_stack_frame_callback(_))")]

    {:query query
     :bindings (-> query-result
                   swipl-js->clj/swipl-query-result->bindings)
     :trace (-> stack-trace
                persistent!
                swipl-js->clj/swipl-stack-trace->clj)}))

(defn query-and-trace!
  ([prolog-program+queries]
   (query-and-trace! prolog-program+queries identity))

  ([{program :program queries :queries} query-result-callback]
   (prom/let
    [swipl (Swipl. #js {:arguments #js ["-q"]})

    ;; This rule invokes the callback function (wrapped as an opaque object)
    ;; from SWI Prolog running in wasm.
    ;; Note that any rule containing the special _ := _ assignment operator
    ;; CANNOT be pre-compiled and loaded in a qlf or buried under an "assert".
    ;; Doing so results in a runtime error.
     _ (jsi/call-in swipl [:prolog :load_string]
                    "log_stack_frame(StackFrame) =>
                      js_log_stack_frame_callback(Func),
                      _ := Func.apply(StackFrame).")

     _ (jsi/call-in swipl [:prolog :consult] prelude-qlf-url)
     _ (jsi/call-in swipl [:prolog :load_string] program)]

     (->> queries
          (prom-m/traverse
           (prom-m/>=> #(run-swipl-query! swipl %)
                       query-result-callback))))))

(defn query-and-trace-js! [prolog-program+queries]
  (->> prolog-program+queries
       bean/->clj
       query-and-trace!
       (prom/map bean/->js)))