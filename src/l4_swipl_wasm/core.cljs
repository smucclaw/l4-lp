(ns l4-swipl-wasm.core
  (:require [applied-science.js-interop :as jsi]
            [cljs-bean.core :as bean]
            [l4-swipl-wasm.syntax.swipl-to-clj :as swipl-js->clj]
            [l4-swipl-wasm.syntax.l4-to-prolog :as l4->prolog]
            [meander.epsilon :as m]
            [meander.strategy.epsilon :as r]
            [promesa.core :as prom]
            [shadow.esm :refer [dynamic-import]]
            [tupelo.string :as str]))

(def ^:private swipl-wasm-cdn-url
  "https://SWI-Prolog.github.io/npm-swipl-wasm/3/7/0/dynamic-import.js")

(def ^:private prelude-qlf-url
  "resources/swipl/prelude.qlf")

;; https://swi-prolog.discourse.group/t/clojure-clojurescript-with-swi-prolog/5399/4
;; https://github.com/SWI-Prolog/roadmap/issues/43

(def ^:private swipl-result->bindings
  (r/pipe
   (r/match
    (m/app bean/bean
           {:$tag (m/some "bindings")
            :success (m/some true)
            & ?bindings})
     ?bindings
   _ {})

   (r/repeat
    (r/rewrite
     {(m/keyword ?var-id) (m/some ?swipl-data) & ?bindings}
     {~(symbol "var" (str/lower-case ?var-id))
      ~(swipl-js->clj/swipl-js-data->clj ?swipl-data)
      & ?bindings}))))

(def ^:private stack-frame->clj
  (r/match
   #js {:parent_goal (m/some ?parent-goal)
        :current_goal (m/some ?current-goal)
        :port (m/some ?port)
        :recursion_depth (m/some ?recursion-depth)}
    {:parent-goal (swipl-js->clj/swipl-js-data->clj ?parent-goal)
     :current-goal (swipl-js->clj/swipl-js-data->clj ?current-goal)
     :port (swipl-js->clj/swipl-js-data->clj ?port)
     :recursion-depth ?recursion-depth}))

(defn- fn->functional-interface [func]
  (new #(this-as this (jsi/assoc! this :apply func))))

(defn eval-and-trace [program goal]
  (prom/let
   [^js swipl-mod (dynamic-import swipl-wasm-cdn-url)
    swipl (-> swipl-mod
              (jsi/get :SWIPL)
              (new #js {:arguments #js ["-q"]}))

    stack-trace (transient [])

   ;; Ugly hack to get swipl wasm working on nodejs.
   ;; The issue is that it fails to load prolog and qlf files on nodejs via Prolog.consult
   ;; with following error:
   ;; ERROR: JavaScript: ReferenceError: window is not defined
   ;; To solve this, we assign a global window object to an empty object just so
   ;; that it's defined.
    _ (when-not (or (exists? js/window)
                    (jsi/get js/globalThis :window))
        (jsi/assoc! js/globalThis :window #js {}))

    _ (jsi/call-in swipl [:prolog :consult] prelude-qlf-url)

    ;; Need to wrap functions by an opaque, non-plain JS object with a method
    ;; that runs the original function.
    ;; This is because if we pass in a plain object, or plain lambda, swipl-wasm
    ;; treats them both as plain objects and tries to recursively convert it to
    ;; a Prolog dictionary, which fails.
    log-stack-frame-callback
    (fn->functional-interface
     #(->> % stack-frame->clj (conj! stack-trace)))

    assert-callback-fn-query
    (jsi/call-in swipl [:prolog :query]
                 "asserta((js_log_stack_frame_callback(Func) :- !))"
                 #js {:Func log-stack-frame-callback})
    _ (jsi/call assert-callback-fn-query :once)

    ;; This rule invokes the callback function (wrapped as an opaque object)
    ;; from SWI Prolog running in wasm.
    ;; Note that any rule containing the special _ := _ assignment operator
    ;; CANNOT be pre-compiled and loaded in a qlf or buried under an "assert".
    ;; Doing so results in a runtime error.
    _ (jsi/call-in swipl [:prolog :load_string]
                   "log_stack_frame(StackFrame) :- !,
                      js_log_stack_frame_callback(Func),
                      _ := Func.apply(StackFrame).")

    _ (jsi/call-in swipl [:prolog :load_string] program)

    query (jsi/call-in swipl [:prolog :query]
                       ;; "eval_and_trace(Goal)" #js {:Goal goal}
                       (str "eval_and_trace(" goal ")"))

    result (jsi/call query :once)]

    ;; (jsi/call js/console :log "Loaded Swipl Mod: " swipl-mod)
    ;; (jsi/call js/console :log "SWIPL: " swipl)

    {:trace (-> stack-trace persistent!)
     :bindings (-> result swipl-result->bindings)}))

;; TODO:
;; Can use dicts to model objects and method calls.
;; See comment here: https://www.swi-prolog.org/pldoc/man?predicate=is/2
;; https://www.swi-prolog.org/pldoc/man?section=bidicts
;; Also look at records: https://www.swi-prolog.org/pldoc/man?section=record

(prom/let
 [program
  ['(DECIDE
     the sum of the list of all elements satisfying q,
     say var/xs, is var/z, which is strictly between 0 and 10
     ;; IF (NOT ((var/x <= 0) OR (var/x >= 10)))
     IF (var/z > 0)
     AND (var/z < 10)
     AND (var/xs IS THE LIST OF ALL var/x SUCH THAT
                 q holds for var/x)
     AND ((var/y + 1) IS 0)
     AND ((var/z + (-1 - var/y)) IS THE SUM OF var/xs))

   '(DECIDE q holds for 0)
   '(DECIDE q holds for 1)
   '(DECIDE q holds for 2)

   '(DECIDE
     var/x and var/y are solutions
     IF (((var/x ** 3) + (var/y ** 3)) IS 0)
     AND (((var/x ** 2) + (var/y ** 2)) IS 1)
     #_AND #_(var/x IS var/y))]

  goal '(the sum of the list of all elements satisfying q,
         say var/xs, is var/z which is strictly between _ and _)
  ;; goal '(var/x and var/y are solutions)

  program' (l4->prolog/l4-program->prolog-program-str program)
  goal' (l4->prolog/l4->prolog-str goal)

  _ (jsi/call js/console :log "Input L4 program:\n" program)
  _ (jsi/call js/console :log "Input L4 goal:\n" goal)

  _ (jsi/call js/console :log "Transpiled Prolog program:\n" program')
  _ (jsi/call js/console :log "Transpiled Prolog goal:\n" goal')

  output (eval-and-trace program' goal')]
  (jsi/call js/console :log "Output:\n" output))

#_(->>
 "[(DECIDE p of var/x IF ((var/x ** 1) IS 0))
   (DECIDE q holds for 1)
   (DECIDE q holds for 2)]"

 l4->prolog/l4-program->prolog-program-str

 (jsi/call js/console :log))