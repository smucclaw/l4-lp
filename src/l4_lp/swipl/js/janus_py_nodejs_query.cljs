(ns l4-lp.swipl.js.janus-py-nodejs-query
  (:require ["pythonia" :refer [python]]
            [applied-science.js-interop :as jsi]
            [cljs-bean.core :as bean]
            [l4-lp.swipl.js.common.swipl-js-to-clj :as swipl-js->clj]
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

(defn query-and-trace! [prolog-program+queries]
  (prom/let [^js py-query-mod @py-query-mod]
    (when py-query-mod
      (prom/let
       [program+queries (-> prolog-program+queries bean/->js)

        query-results-py-ref
        (jsi/call py-query-mod .-query_and_trace_sync program+queries)
        query-results
        (jsi/call query-results-py-ref .-valueOf)]

        (->> query-results
             (mapv
              #(-> % bean/bean
                   (update :stack_trace swipl-js->clj/swipl-stack-trace->clj))))))))

(defn query-and-trace-js! [prolog-program+queries]
  (->> prolog-program+queries
       query-and-trace!
       (prom/map bean/->js)))