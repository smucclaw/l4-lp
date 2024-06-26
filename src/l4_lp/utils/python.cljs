(ns l4-lp.utils.python
  (:require [applied-science.js-interop :as jsi]
            [l4-lp.utils.promise.monad :as prom-m]
            [promesa.core :as prom]))

(defn traverse-py-iter
  "Asynchronously traverses a Python iterator."
  [f ^js py-iter]
  (let [next-fn
        #(->> (prom/do (jsi/call py-iter .-__next__))
              (prom/mapcat (fn [^js item-proxy] 
                             (jsi/call item-proxy .-valueOf)))
              (prom/map (fn [item] {:item item :rest true}))
              (prom/merr (constantly (prom/resolved {:done? true}))))]
    (prom-m/traverse f true :next-fn next-fn)))

(defn py-iter->vec [py-iter]
  (traverse-py-iter identity py-iter))