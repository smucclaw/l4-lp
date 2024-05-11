(ns l4-lp.utils.python
  (:require [applied-science.js-interop :as jsi]
            [meander.epsilon :as m]
            [promesa.core :as prom]))

(defn traverse-py-iter
  "Asynchronously traverses a Python iterator."
  [f ^js py-iter]
  (prom/loop [results (transient [])]
    (prom/let [item (-> (prom/do (jsi/call py-iter .-__next__))
                        (prom/then #(jsi/call ^js % .-valueOf))
                        (prom/catch (constantly :done)))]
      (m/match item
        :done (persistent! results)
        _ (prom/let [result (f item)]
            (prom/recur (conj! results result)))))))

(defn py-iter->vec [py-iter]
  (traverse-py-iter identity py-iter))