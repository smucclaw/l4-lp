(ns l4-swipl-wasm.core
  (:require [applied-science.js-interop :as jsi]
            [clojure.string :as str]
            [meander.epsilon :as m]
            [meander.strategy.epsilon :as r]
            [promesa.core :as prom]
            [shadow.esm :refer [dynamic-import]]
            [tupelo.core :refer [it->]]))

(def ^:private swipl-wasm-cdn-url
  "https://SWI-Prolog.github.io/npm-swipl-wasm/3/7/0/dynamic-import.js")

(def ^:private prelude-qlf-url
  "resources/swipl/prelude.qlf")

;; https://swi-prolog.discourse.group/t/clojure-clojurescript-with-swi-prolog/5399/4
;; https://github.com/SWI-Prolog/roadmap/issues/43

(def ^:private swipl-data->clj
  (r/top-down
   (r/rewrite
    #js {:$t (m/some "s") :v (m/some ?str)} ?str
    #js {:$t (m/some "v") :v (m/some ?var-id)} ~(symbol "var" ?var-id)

    #js [!xs ...] [!xs ...]

    #js {:$t (m/some "t")
         :functor (m/some ":")
         ":" (m/some #js [_ (m/cata ?term)])}
    ?term

    (m/and
     #js {:$t (m/some "t") :functor (m/some ?functor)}
     (m/app #(jsi/get % ?functor) #js [!args ...]))
    (~(symbol ?functor) & [!args ...])

    =< <=

    (m/symbol ";") or
    (m/symbol ",") and

    ?term ?term)))

(def ^:private clj->swipl
  (let [turnstile (symbol ":-")
        comma (symbol ",")
        _dot (symbol ".")
        semicolon (symbol ";")
        open-brace (symbol "(")
        close-brace (symbol ")")
        open-sq-brace (symbol "[")
        close-sq-brace (symbol "]")
        infix-ops #{'+ '- '* '/ '< '<= '= '> '>=}
        math-list-ops #{'MIN 'MAX 'PRODUCT 'SUM}]
    ;; Transpiler from L4 to SWI Prolog is formalised as a denotational
    ;; semantics.
    ;; We axiomatise this via a (first-order) equational theory whose primary
    ;; construct is the transpilation function ⟦.⟧ which maps from the Natural4
    ;; term algebra to that of SWI Prolog.
    (r/top-down
     (r/rewrite
      ;; ------------------------------------------------
      ;; ⟦DECIDE ?head₀ ... ?headₘ IF ?body₀ ... ?bodyₙ⟧ =
      ;;   ⟦(:- (?head₀ ... ?headₘ) (?body₀ ... ?bodyₙ))⟧
      (DECIDE . !head ..1 IF . !body ..1)
      ((~turnstile (!head ...) (!body ...)))

      ;; -------------------------------------------------
      ;; ⟦DECIDE ?head₀ ... ?headₙ⟧ = ⟦(?head₀ ... ?headₙ)⟧
      (DECIDE . !head ..1) ((!head ...))

      ;; TODO: (& [!conjuncts ... ?conjunct] AND ...) nil

      ;; ---------
      ;; ⟦AND⟧ = ,
      AND ~comma

      ;; ---------
      ;; ⟦OR⟧ = ;
      OR ~semicolon

      ;; -----------
      ;; ⟦NOT⟧ = not
      NOT not

      ;; ------------------
      ;; ⟦SUM⟧ = sum_list_
      SUM sum_list_

      ;; ------------------------
      ;; ⟦PRODUCT⟧ = product_list
      PRODUCT product_list

      ;; ------------------------
      ;; ⟦MIN⟧ = min_list_
      MIN min_list_

      ;; ------------------------
      ;; ⟦MAX⟧ = max_list_
      MAX max_list_

      ;; ---------
      ;; ⟦<=⟧ = =<
      <= =<

      ;;  ?op ∈ math-list-ops     ?lhsᵢ ∉ math-list-ops
      ;; ---------------------------------------------------
      ;; ⟦(?lhs₀ ... lhsₘ IS THE ?op OF ?rhs₀ ... ?rhsₙ)⟧ =
      ;;   ⟦(?op (?rhs₀ ... ?rhsₘ) (?lhs₀ ... ?lhsₙ))⟧
      (. !lhs ..1 IS THE (m/pred math-list-ops ?op) OF . !rhs ..1)
      ((?op (!rhs ...) (!lhs ...)))

      ;;  ?op ∈ math-list-ops     ?lhsᵢ ∉ math-list-ops
      ;; -----------------------------------------------------
      ;; ⟦(?xs IS THE LIST OF ALL ?x SUCH THAT ?φ₀ ... ?φₙ)⟧ =
      ;;   ⟦(findall ?x (?φ₀ ... ?φₙ) ?xs)⟧
      (?xs IS THE LIST OF ALL ?x SUCH THAT & ?phi-x)
      ((findall ?x ?phi-x ?xs))

      ;;  ?op ∈ infix-ops              ?lhsᵢ ∉ infix-ops
      ;; -------------------------------------------------
      ;; ⟦(?lhs₀ ... lhsₘ ?op ?rhs₀ ... ?rhsₙ)⟧ =
      ;;   ⟦(?op (?lhs₀ ... ?lhsₘ) (?rhs₀ ... ?rhsₙ))⟧
      (m/and (!lhs ..1 (m/pred infix-ops ?op) . !rhs ..1))
      ((?op (!lhs ...) (!rhs ...)))

      ;; ?pred ∈ symbol
      ;; ----------------------------------------------------------
      ;;  ⟦(?pred ?arg₀ ... ?argₙ)⟧ = ⟦?pred⟧(⟦?arg₀⟧ , ... , ⟦?argₙ⟧)
      ((m/and (m/symbol _) ?pred) . !args ... ?arg)
      (?pred ~open-brace & (!args ~comma ... ?arg) ~close-brace)

      ;; ---------------------------------------
      ;;  ⟦[?x₀ ... ?xₙ]⟧ = [⟦?x₀⟧ , ... , ⟦?xₙ⟧]
      [!xs ... !x] (~open-sq-brace & (!xs ~comma ... !x) ~close-sq-brace)

      ;; ?var-name = (symbol "var" ?var-name')
      ;; -----------------------------------------------------
      ;;   ⟦?var-name⟧ = (symbol (str/capitalize ?var-name'))
      (m/symbol "var" ?var-name) ~(-> ?var-name str/capitalize symbol)

      ;; ?x ∈ atom ∪ ℝ ∪ string 
      ;; --------------------
      ;; ⟦?x⟧ = ?x
      ?x ?x))))

(def ^:private stack-frame->clj
  (r/match
   #js {:parent_goal (m/some ?parent-goal)
        :current_goal (m/some ?current-goal)
        :port (m/some ?port)
        :recursion_depth (m/some ?recursion-depth)}
    {:parent-goal (swipl-data->clj ?parent-goal)
     :current-goal (swipl-data->clj ?current-goal)
     :port (swipl-data->clj ?port)
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

    _ (jsi/call query :once)]

    ;; (jsi/call js/console :log "Loaded Swipl Mod: " swipl-mod)
    ;; (jsi/call js/console :log "SWIPL: " swipl)

    (-> stack-trace persistent!)))

;; TODO:
;; Can use dicts to model objects and method calls.
;; See comment here: https://www.swi-prolog.org/pldoc/man?predicate=is/2
;; https://www.swi-prolog.org/pldoc/man?section=bidicts
;; Also look at records: https://www.swi-prolog.org/pldoc/man?section=record

(prom/let
 [program
  ['(DECIDE
     p var/xs var/x
     IF (var/xs IS THE LIST OF ALL var/x SUCH THAT q var/x)
     AND (var/x IS THE SUM OF var/xs)
     AND (NOT ((var/x <= 0) OR (var/x >= 10))))

   '(DECIDE q 0)
   '(DECIDE q 1)
   '(DECIDE q 2)]

  goal '(p var/xs var/x)

  program' (it-> program
                 (eduction (map #(-> % clj->swipl (str ".\n"))) it)
                 (apply str it)
                 (str/replace it #" " ""))
  goal' (-> goal clj->swipl str (str/replace #" " ""))

  _ (jsi/call js/console :log "Input L4 program:\n" program)
  _ (jsi/call js/console :log "Input L4 goal:\n" goal)

  _ (jsi/call js/console :log "Transpiled Prolog program:\n" program')
  _ (jsi/call js/console :log "Transpiled Prolog goal:\n" goal')

  trace (eval-and-trace program' goal')]
  (jsi/call js/console :log "Trace:\n" trace))