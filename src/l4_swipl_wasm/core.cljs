(ns l4-swipl-wasm.core
  (:require [applied-science.js-interop :as jsi]
            [cljs-bean.core :as bean]
            [tupelo.string :as str]
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

    (m/and #js {:$t (m/some "t") :functor (m/some ?functor)}
           (m/app #(jsi/get % ?functor) #js [!args ...]))
    (~(symbol ?functor) & [!args ...])

    IS is
    lt <
    =< <=
    lte <=
    gt >
    gte >=

    (m/symbol ";") or
    (m/symbol ",") and

    ?term ?term)))

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
      ~(swipl-data->clj ?swipl-data)
      & ?bindings}))))

(def ^:private mixfix->prefix
  (r/pipe
   (r/rewrite
   ;; Partition all the elements into args and non-args.
   ;; Non-args are later mashed together into an atom representing the predicate.
    ((m/or (m/and (m/or (m/symbol "_")
                        (m/symbol "var" _)
                        (m/pred number?)
                        (m/pred seq?)
                        (m/pred vector?))
                  !args)
           !non-args)
     ..1)
    {:non-args [!non-args ...] :args [!args ...]})

   ;; Convert the non-args into a valid atom representing the predicate.
   (r/match
    {:non-args ?non-args :args ?args}
     {:pred (it-> ?non-args (str/join "_" it) (str "'" it "'") (symbol it))
      :args ?args})

   ;; Convert the predicate and list of arguments to prefix form.
   (r/rewrite
    {:pred ?pred :args []} ?pred
    {:pred ?pred :args [!args ... ?arg]}
    (?pred ~(symbol "(") & (!args ~(symbol ",") ... ?arg) ~(symbol ")")))))

(def ^:private clj->swipl
  (let [turnstile (symbol ":-")
        comma (symbol ",")
        _dot (symbol ".")
        semicolon (symbol ";")
        open-brace (symbol "(")
        close-brace (symbol ")")
        _open-sq-brace (symbol "[")
        _close-sq-brace (symbol "]")
        infix-ops #{'+ '- '* '/ '< '<= '= '> '>= '=< 'IS '**}
        math-list-ops #{'MIN 'MAX 'PRODUCT 'SUM}]
    ;; Here we formalise a denotational semantics for the transpiler from L4 to
    ;; SWI Prolog, and implement it.
    ;; We axiomatise our semantics via a (first-order) equational theory whose
    ;; primary construct is the interpretation function ⟦.⟧ which maps the
    ;; Natural4 term algebra to that of SWI Prolog.
    ;; This is implemented via a top-down traversal of the Natural4
    ;; AST, transforming each node via Meander term rewriting
    ;; rules that orient the equational theory from left to right.
    (r/top-down
     (r/rewrite
      ;; --------------------------------------------------
      ;; ⟦DECIDE ?head₀ ... ?headₘ IF ?body₀ ... ?bodyₙ⟧ =
      ;;   ⟦(:- (?head₀ ... ?headₘ) (?body₀ ... ?bodyₙ))⟧
      (DECIDE . !head ..1 IF . !body ..1)
      ((~turnstile (!head ...) (!body ...)))

      ;; -------------------------------------------------
      ;; ⟦DECIDE ?head₀ ... ?headₙ⟧ = ⟦(?head₀ ... ?headₙ)⟧
      (DECIDE . !head ..1) ((!head ...))

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

      ;; Auxiliary stuff for parsing predicate applications that are presented
      ;; in mixfix form.
      ;; TODO: Formalise the semantics of this operation.
      (m/and
       ;; Restrict mixfix parsing to seqs where there is > 1 item present,
       ;; because otherwise there is no need for this.
       (_ _ & _)
       ;; The next 2 clauses restricts mixfix parsing to ignore AST nodes that
       ;; contain L4 keywords.
       ;; Such nodes include BoolStructs like (... AND ... AND ...).
       (m/gather (m/or (m/pred #(-> % str str/uppercase?))
                       (m/pred infix-ops))
                 ?count)
       (m/guard (zero? ?count))
       ?predicate-application)
      ~(mixfix->prefix ?predicate-application)

      ;; ?pred ∈ symbol
      ;; -----------------------------------------------------------
      ;;  ⟦(?pred ?arg₀ ... ?argₙ)⟧ = ⟦?pred⟧(⟦?arg₀⟧ , ... , ⟦?argₙ⟧)
      ((m/and (m/symbol _) ?pred) . !args ... ?arg)
      (?pred ~open-brace & (!args ~comma ... ?arg) ~close-brace)

      ;; ---------------------------------------
      ;;  ⟦[?x₀ ... ?xₙ]⟧ = [⟦?x₀⟧ , ... , ⟦?xₙ⟧]
      [!xs ... !x] [!xs ~(symbol ",") ... !x]

      ;; ?var-name = (symbol "var" ?var-name')
      ;; -----------------------------------------------------
      ;;   ⟦?var-name⟧ = (symbol (str/capitalize ?var-name'))
      (m/symbol "var" ?var-name) ~(-> ?var-name str/capitalize symbol)

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

      ;; ------------------
      ;; ⟦MIN⟧ = min_list_
      MIN min_list_

      ;; -----------------
      ;; ⟦MAX⟧ = max_list_
      MAX max_list_

      ;; -----------
      ;; ⟦IS⟧ = 'IS'
      IS ~(symbol "'IS'")

      < ~(symbol "lt")
      <= ~(symbol "leq")
      =< ~(symbol "leq")

      > ~(symbol "gt")
      >= ~(symbol "geq")

      ;; ~(symbol "**") pow

      ;; ?x ∈ atom ∪ ℝ ∪ string
      ;; -----------------------
      ;;       ⟦?x⟧ = ?x
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

  program' (it-> program
                 (eduction (map #(-> % clj->swipl (str ".\n"))) it)
                 (apply str it)
                 (str/replace it #" " ""))
  goal' (-> goal clj->swipl str (str/replace #" " ""))

  _ (jsi/call js/console :log "Input L4 program:\n" program)
  _ (jsi/call js/console :log "Input L4 goal:\n" goal)

  _ (jsi/call js/console :log "Transpiled Prolog program:\n" program')
  _ (jsi/call js/console :log "Transpiled Prolog goal:\n" goal')

  output (eval-and-trace program' goal')]
  (jsi/call js/console :log "Output:\n" output))