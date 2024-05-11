(ns l4-lp.utils.promise
  (:refer-clojure :exclude [sequence])
  (:require [meander.epsilon :as m]
            [promesa.core :as prom]))

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
  [f coll]
  (prom/loop [coll coll
              results (transient [])]
    (m/match coll
      (m/seqable ?item & ?coll)
      (prom/let [result (f ?item)]
        (prom/recur ?coll (conj! results result)))

      _ (persistent! results))))

(defn sequence
  "Traverse a collection of promises using the identity function, ie:
   sequence :: [PromiseT m a] -> PromiseT m [a]"
  [promises]
  (traverse identity promises))

(defn >=>
  "Variadic Kleisli composition of promise monadic functions."
  ([] identity)
  ([f & args] #(apply prom/chain % f args)))