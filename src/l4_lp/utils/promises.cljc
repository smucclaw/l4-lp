(ns l4-lp.utils.promises 
  (:require [meander.epsilon :as m]
            [promesa.core :as prom]))

(defn traverse-promises
  "Traverse a seqable of promise monads (in an eager, tail-recursive manner)."
  [f promises]
  (prom/loop [promises promises
              results (transient [])]
    (m/match promises
      (m/seqable) (persistent! results)

      (m/seqable ?promise & ?promises)
      (prom/let [result (f ?promise)]
        (prom/recur ?promises (conj! results result))))))