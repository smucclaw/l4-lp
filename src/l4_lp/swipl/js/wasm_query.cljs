(ns l4-lp.swipl.js.wasm-query 
  (:require ["https://SWI-Prolog.github.io/npm-swipl-wasm/4/0/4/dynamic-import.js"
             :rename {SWIPL Swipl}]
            [applied-science.js-interop :as jsi]
            [cljs-bean.core :as bean]
            [l4-lp.swipl.js.common.swipl-js-to-clj :as swipl-js->clj]
            [l4-lp.utils.promise.monad :as prom-m]
            [promesa.core :as prom]
            [tupelo.core :refer [it->]]))

(def swipl-prelude-qlf-url
  "./resources/swipl/prelude.qlf")

(defn init-swipl! [swipl-prelude-qlf-url]
  (prom/let
   [swipl (new Swipl #js {:arguments #js ["-q"]})
    _ (jsi/call-in swipl [:prolog :consult] swipl-prelude-qlf-url)
    ;; Note that any rule containing the special _ := _ assignment operator
    ;; CANNOT be pre-compiled and loaded in a qlf or buried under an "assert".
    ;; Doing so results in a runtime error.
    _ (jsi/call-in swipl [:prolog :load_string]
                   "log_stack_frame(StackFrame) =>
                      stack_trace(StackTrace),
                      _ := StackTrace.log_stack_frame(StackFrame).")]
    swipl))

(defn- swipl-query-once
  [swipl & args]
  (it-> swipl
        (apply jsi/call-in it [:prolog :query] args)
        (jsi/call it :once)))

(defn- run-swipl-query! [swipl query]
  (prom/let
   [stack-trace #js []
    ;; Wrap the stack-trace in a non-plain JS object.
    ;; Note that if we pass in a plain object, or plain lambda, swipl-wasm
    ;; treats them both as plain objects and tries to recursively convert it to
    ;; a Prolog dictionary, which fails.
    stack-trace-obj
    (new #(this-as this
                   (jsi/assoc! this :log_stack_frame
                               (partial jsi/call stack-trace :push))))
    query-result
    (swipl-query-once swipl
                      (str "query_and_trace(StackTrace," query ")")
                      #js {:StackTrace stack-trace-obj})]
    {:query query
     :bindings (-> query-result swipl-js->clj/swipl-query-result->bindings)
     :trace (-> stack-trace swipl-js->clj/swipl-stack-trace->clj)}))

(defn query-and-trace!
  [{program :program queries :queries}
   & {:keys [swipl on-query-result]
      :or {on-query-result identity}}]
  (prom/let
   [swipl (if swipl
            swipl
            (init-swipl! swipl-prelude-qlf-url))
    ;; Using the same file id "id" for all programs that we load ensures that
    ;; they are reconsulted, so that earlier programs that were loaded into the
    ;; swipl interpreter get unloaded.
    ;; https://swi-prolog.discourse.group/t/using-the-wasm-version/6379/2
    _ (jsi/call-in swipl [:prolog :load_string] program "id")]
    (->> queries
         (prom-m/traverse
          (prom-m/>=> #(run-swipl-query! swipl %)
                      on-query-result)))))

(defn query-and-trace-js! [prolog-program+queries]
  (->> prolog-program+queries
       bean/->clj
       query-and-trace!
       (prom/map bean/->js)))