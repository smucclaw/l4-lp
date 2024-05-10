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

(defn init-swipl-engine!
  [& {:keys [query-executor]
      :or {query-executor (promx/vthread-per-task-executor)}}]

  (reset! swipl-query-executor query-executor)

  (it-> "public/resources/swipl/prelude.qlf"
        (str "consult(user:'" it "')")
        (Query. it)
        (.oneSolution it)))

(defn query! [{program :program queries :queries}]
  (let [;; Query to load the Prolog program.
        load-program
        (->> ["program" program "user"]
             (eduction (map #(Atom. %)))
             into-array
             (Query. "load_from_string"))
        queries (->> queries (eduction (map #(Query. (str %)))))]

    (promx/submit!
     @swipl-query-executor
     (fn []
       (Prolog/create_engine)
       (.oneSolution load-program)
       (let [query-results (->> queries (mapv #(.oneSolution %)))]
         (Prolog/destroy_engine)
         query-results)))))