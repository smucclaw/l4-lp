(ns l4-lp.swipl.jpl-jvm.query
  (:require [meander.epsilon :as m]
            [promesa.core :as prom]
            [promesa.exec :as promx]
            [tupelo.core :refer [it->]])
  (:import [org.jpl7 Atom Compound Query Term Variable]
           [org.jpl7.fli Prolog]))

;; https://jpl7.org/TutorialMultithreaded
;; https://github.com/SWI-Prolog/packages-jpl/tree/2c6cd0abd5ef6d46e4a78e49c55774db3a17f162/src/examples/java/thread

(def ^:private swipl-query-executor
  (atom nil))

(defn init!
  [& {:keys [query-executor]
      :or {query-executor (promx/vthread-per-task-executor)}}]

  (reset! swipl-query-executor query-executor)

  (it-> "public/resources/swipl/prelude.qlf"
        (str "consult(user:'" it "')")
        (Query. it)
        (.oneSolution it)))

(defn query! [program goal]
  (promx/submit!
   @swipl-query-executor
   (fn []
     (Prolog/create_engine)

     ;; Load Prolog program.
     (->> ["program" (str program) "user"]
          (eduction (map #(Atom. %)))
          into-array
          (Query. "load_from_string")
          (.oneSolution))

     (let [soln (-> goal (Query.) (.oneSolution))]
       (Prolog/destroy_engine)
       soln))))