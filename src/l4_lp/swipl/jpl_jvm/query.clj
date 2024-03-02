(ns l4-lp.swipl.jpl-jvm.query
 (:require [meander.epsilon :as m]
           [promesa.core :as prom]
           [tupelo.core :refer [it->]])
  (:import [org.jpl7 Atom Compound Query Term Variable]
           [org.jpl7.fli Prolog]))

;; https://github.com/SWI-Prolog/packages-swipy/blob/feff32f02a82363df86483f1b9448ccc16d5ac1b/janus/janus.py#L334
;; https://github.com/SWI-Prolog/packages-swipy/blob/feff32f02a82363df86483f1b9448ccc16d5ac1b/janus/janus.pl#L1234
(defn consult [file & {:keys [data module]
                       :or {data ""
                            module "user"}}]
    (->> [file data module]
         (eduction (map #(Atom. %)))
         into-array
         (Query. "jpl_consult")
         (.oneSolution)))

;; https://jpl7.org/TutorialMultithreaded
;; https://github.com/SWI-Prolog/packages-jpl/tree/2c6cd0abd5ef6d46e4a78e49c55774db3a17f162/src/examples/java/thread

(defn query! []
  (it-> (prom/promise (Prolog/create_engine))
        (prom/map
         (fn [_]
           (-> "member(X, [1, 2, 3])"
               Term/textToTerm
               (Query.)
               (.oneSolution)))
         it)
        (prom/finally it (fn [_ _] (Prolog/destroy_engine)))
        (deref it)
        (println it)))