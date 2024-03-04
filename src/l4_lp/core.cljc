(ns l4-lp.core
  (:require #?(:cljs [applied-science.js-interop :as jsi])
            #?(:cljs [l4-lp.swipl.js.wasm-query :as swipl-wasm-js-query])
            [l4-lp.syntax.l4-to-prolog :as l4->prolog]
            #?(:cljs [promesa.core :as prom])))

(def l4->prolog-str
  l4->prolog/l4->prolog-str)

(def l4-program->prolog-program-str
  l4->prolog/l4-program->prolog-program-str)

#?(:cljs
   (def query-and-trace
     swipl-wasm-js-query/query-and-trace))

;; #?(:cljs
;;    (prom/let
;;     [program
;;      ['(DECIDE
;;         the sum of the list of all elements satisfying q,
;;         say var/xs, is var/z, which is strictly between 0 and 10
;;      ;; IF (NOT ((var/x <= 0) OR (var/x >= 10)))
;;         IF (var/z > 0)
;;         AND (var/z < 10)
;;         AND (var/xs IS THE LIST OF ALL var/x SUCH THAT
;;                     q holds for var/x)
;;         AND ((var/y + 1) IS 0)
;;         AND ((var/z + (-1 - var/y)) IS THE SUM OF var/xs))

;;       '(DECIDE q holds for 0)
;;       '(DECIDE q holds for 1)
;;       '(DECIDE q holds for 2)

;;       '(DECIDE
;;         var/x and var/y are solutions
;;         IF (((var/x ** 3) + (var/y ** 3)) IS 0)
;;         AND (((var/x ** 2) + (var/y ** 2)) IS 1)
;;         #_AND #_(var/x IS var/y))]

;;      goal '(the sum of the list of all elements satisfying q,
;;                 say var/xs, is var/z which is strictly between _ and _)
;;   ;; goal '(var/x and var/y are solutions)

;;      program' (l4-program->prolog-program-str program)
;;      goal' (l4->prolog-str goal)

;;      _ (jsi/call js/console :log "Input L4 program:\n" program)
;;      _ (jsi/call js/console :log "Input L4 goal:\n" goal)

;;      _ (jsi/call js/console :log "Transpiled Prolog program:\n" program')
;;      _ (jsi/call js/console :log "Transpiled Prolog goal:\n" goal')

;;      output (query-and-trace program' goal')]
;;      (jsi/call js/console :log "Output:\n" output))

;;    #_(->>
;;       "[(DECIDE p of var/x IF ((var/x ** 1) IS 0))
;;    (DECIDE q holds for 1)
;;    (DECIDE q holds for 2)]"

;;       l4->prolog/l4-program->prolog-program-str

;;       (jsi/call js/console :log)))