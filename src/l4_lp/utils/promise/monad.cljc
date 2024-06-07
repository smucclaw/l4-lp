(ns l4-lp.utils.promise.monad
  (:refer-clojure :exclude [sequence])
  (:require [meander.epsilon :as m]
            [meander.strategy.epsilon :as r]
            [promesa.core :as prom]))

(def ^:private default-traverse-next-fn
  (r/match
   (m/seqable ?item & ?rest) {:item ?item :rest ?rest}
   _ {:done? true}))

(defn traverse
  "Traverse a collection using a promise returning function, ie:
   traverse :: (a -> PromiseT m b) -> [b] -> PromiseT m [b]

   Note that:
   - Using a side-effecting function, ie m = IO, enables one to stream
     visible effects to the outside world (like a GUI) as and when promises
     complete, without blocking and waiting for all results to complete first.

   - This function loops indefinitely if the input collection is an
     infinitely long lazy seq.

     More generally, any lawful implementation of traverse must necessarily be
     unproductive, ie return ⊥, on an infinite input containing any datatype
     generalising Maybe (a trivial adversarial argument yields a contradiction:
     if we lazily yield coll₀ through collᵢ is a Just, then the adversary can set
     collᵢ₊₁ to Nothing, so that our traversal violates traversable laws).
     The same argument shows that that one cannot lazily stream the results of
     such a traversal even over a finite input."
  [f coll
   & {:keys [next-fn return-coll]
      :or {next-fn default-traverse-next-fn
           return-coll []}}]

  (prom/loop [next (next-fn coll)
              return-coll (transient return-coll)]
    (m/match next
      {:done? true} (persistent! return-coll)

      {:item (m/some ?item) :rest (m/some ?rest)}
      (prom/let [result (f ?item)]
        (prom/recur (next-fn ?rest) (conj! return-coll result))))))

(defn >=>
  "Variadic Kleisli composition of promise monadic functions."
  ([] identity)
  ([f & args] #(apply prom/chain % f args)))