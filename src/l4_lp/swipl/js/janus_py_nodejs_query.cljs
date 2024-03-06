(ns l4-lp.swipl.js.janus-py-nodejs-query
  (:require ["pythonia" :refer [python]]
            [applied-science.js-interop :as jsi]
            [l4-lp.swipl.js.common.swipl-js-to-clj :as swipl-js->clj]
            [promesa.core :as prom]))

(def ^:private py-query-mod
  (atom nil))

(defn init-swipl-engine! [l4-lp-py-dir]
  (when-not @py-query-mod
    (let [^js py-query-mod' (python l4-lp-py-dir)]
      (->> py-query-mod'
           (prom/map #(jsi/call ^js % .-init_swipl_engine))
           (prom/map (fn [_] (reset! py-query-mod py-query-mod')))))))

(defn query-and-trace! [program goal]
  (prom/let [^js py-query-mod @py-query-mod]
    (when py-query-mod
      (prom/let
       [^js stack-trace-py-ref
        (jsi/call py-query-mod .-query_and_trace_sync program goal)

        ^js stack-trace
        (jsi/call stack-trace-py-ref .-valueOf)]
        (->> stack-trace
             seq
             (eduction (map swipl-js->clj/swipl-stack-frame->clj))
             (into []))))))