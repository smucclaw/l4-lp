(ns l4-lp.main
  (:require [l4-lp.syntax.l4-to-prolog :as l4->prolog]
            [l4-lp.swipl.jpl-jvm.query :as l4-lp-jpl-query]
            [promesa.core :as prom])
  (:gen-class))

;; [libpython-clj2.python :as py]
;; [libpython-clj2.require :refer [require-python]]

;; (def janus
;;   (py/import-module "janus_swi"))

(defn -main [& _args]
  (let [{program :program query :query :as prolog-program+query}
        (-> "GIVETH xs x
             QUERY p of xs and x

             GIVEN (xs IS A LIST OF Number)
                   (x IS A Number)
                   y
             DECIDE p of xs and x
             IF xs IS THE LIST OF ALL y SUCH THAT q holds for y
             AND x IS SUM xs

             DECIDE q holds for 1
             DECIDE q holds for 2"
            l4->prolog/l4->prolog-program+query)]

    (println "Program: " program)
    (println "Query: " query)

    (l4-lp-jpl-query/init-swipl-engine!)
    @(->> (l4-lp-jpl-query/query! prolog-program+query) (prom/map prn))

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