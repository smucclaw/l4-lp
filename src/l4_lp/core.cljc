(ns l4-lp.core
  (:require #?(:cljs [l4-lp.swipl.js.wasm-query :as swipl-wasm-js-query])
            [l4-lp.syntax.l4-to-prolog :as l4->prolog]
            #?(:cljs [promesa.core :as prom])))

;; (def l4->prolog-str
;;   l4->prolog/l4->prolog-str)

;; (def l4-program->prolog-program-str
;;   l4->prolog/l4-program->prolog-program-str)

;; #?(:cljs
;;    (def query-and-trace
;;      swipl-wasm-js-query/query-and-trace))