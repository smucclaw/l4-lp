(ns l4-lp.core
  (:require #?(:cljs [applied-science.js-interop :as jsi])
            #?(:cljs [l4-lp.swipl.js.wasm-query :as swipl-wasm-js-query])
            [l4-lp.syntax.l4-to-prolog :as l4->prolog]
            #?(:cljs [promesa.core :as prom])))

;; (def l4->prolog-str
;;   l4->prolog/l4->prolog-str)

;; (def l4-program->prolog-program-str
;;   l4->prolog/l4-program->prolog-program-str)

;; #?(:cljs
;;    (def query-and-trace
;;      swipl-wasm-js-query/query-and-trace))

#?(:cljs
   (prom/let
    [program
     ['(DECIDE p of var/xs and var/x and var/z
              IF (var/xs IS THE LIST OF ALL var/y SUCH THAT q holds for var/y)
              AND (var/ys IS THE LIST OF ALL var/y SUCH THAT r holds for var/y)
              AND (r holds for var/z)
              AND (var/x IS (SUM var/xs))
              AND (var/y IS (SUM var/ys))
              AND (var/y > 0))

      '(DECIDE q holds for 0)
      '(DECIDE q holds for 1)

      '(DECIDE r holds for var/z
              IF (var/z IS 3)
              OR (var/z IS 4))]

     goal '(p of var/xs and var/x and 4)

     _ (jsi/call js/console :log "Input L4 program:\n" program)
     _ (jsi/call js/console :log "Input L4 goal:\n" goal)

     program' (l4->prolog/l4-program->prolog-program-str program)
     goal' (l4->prolog/l4->prolog-str goal)

     _ (jsi/call js/console :log "Transpiled Prolog program:\n" program')
     _ (jsi/call js/console :log "Transpiled Prolog goal:\n" goal')

     output (swipl-wasm-js-query/query-and-trace! program' goal')]
     (jsi/call js/console :log "Output:\n" output))

   #_(->>
      "[(DECIDE p of var/x IF ((var/x ** 1) IS 0))
   (DECIDE q holds for 1)
   (DECIDE q holds for 2)]"

      l4->prolog/l4-program->prolog-program-str

      (jsi/call js/console :log)))