(ns l4-lp.swipl.js.janus-py-nodejs-query
  (:require ["pythonia" :refer [python]]
            [applied-science.js-interop :as jsi]
            [cljs-bean.core :as bean]
            [l4-lp.swipl.js.common.swipl-js-to-clj :as swipl-js->clj]
            [l4-lp.utils.promise.monad :as prom-m]
            [l4-lp.utils.python :as python-utils]
            [promesa.core :as prom]))

(def ^:private py-query-mod
  (atom nil))

;; Note that all ^js hints below are needed.
;; Otherwise, the Closure compiler mangles up the objects' field names, leading
;; to property access errors.

(defn init-swipl-engine! [l4-lp-py-dir]
  (when-not @py-query-mod
    (let [^js py-query-mod' (python l4-lp-py-dir)]
      (->> py-query-mod'
           (prom/map #(jsi/call ^js % .-init_swipl_engine))
           (prom/map (fn [_] (reset! py-query-mod py-query-mod')))))))

(defn query-and-trace!
  ([prolog-program+queries]
   (query-and-trace! prolog-program+queries identity))

  ([prolog-program+queries query-result-callback]
   (prom/let [^js py-query-mod @py-query-mod]
     (when py-query-mod
       (prom/let
        [program+queries (bean/->js prolog-program+queries)

         query-results-py-iter
         (jsi/call ^js py-query-mod .-query_and_trace_sync program+queries)]

         (->> query-results-py-iter
              (python-utils/traverse-py-iter
               (prom-m/>=>
                bean/bean
                #(update % :stack_trace swipl-js->clj/swipl-stack-trace->clj)
                query-result-callback))))))))

(defn query-and-trace-js! [prolog-program+queries]
  (->> prolog-program+queries
       query-and-trace!
       (prom/map bean/->js)))