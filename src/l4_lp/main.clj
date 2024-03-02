(ns l4-lp.main
  (:require ;; [libpython-clj2.python :as py]
 ;; [libpython-clj2.require :refer [require-python]]
            [l4-lp.core :as l4-lp] ;; [libpython-clj2.python :as py]
            [l4-lp.swipl.jpl-jvm.query :refer [query!]])
  (:gen-class) 
  (:import [org.jpl7 Query]))

;; (def janus
;;   (py/import-module "janus_swi"))

(defn -main [& args]
  (let [program
        (-> "[(DECIDE p of var/xs and var/x IF
               (var/xs IS THE LIST OF ALL var/y SUCH THAT q holds for var/y)
               AND (var/x IS THE SUM OF var/xs))
              (DECIDE q holds for 1)
              (DECIDE q holds for 2)]"
            l4-lp/l4-program->prolog-program-str)

        goal (-> "(p of var/xs and var/x)"
                 l4-lp/l4->prolog-str)]

    (println "Program: " program)
    (println "Goal: " goal)

    (-> "0 = 0" (Query.) (.oneSolution))

    (println @(query!))

    ;; (py/get-attr janus :attach_engine)
    ;; (py/call-attr janus :consult "public/resources/swipl/prelude.qlf")
    ;; (py/call-attr janus :consult "program" program)

    ;; (it-> goal
    ;;       (py/call-attr janus :query_once it)
    ;;       ;; (get it "Xs")
    ;;       ;; (type it)
    ;;       (println it))

    ;; (py/get-attr janus :detach_engine)
    
    (System/exit 0)))