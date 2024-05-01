# l4-lp overview

This project formalises and implements an execution pipeline for L4, a legal
DSL for the law, along with an accompanying web-based IDE and libraries that
integrate L4 with various general-purpose programming languages.

For context, this evolved from an older pipeline involving
[logical-english-client](https://github.com/smucclaw/logical-english-client)
which utilised [Logical English](https://github.com/smucclaw/LogicalEnglish)
to transpile L4 to Prolog and generate execution traces.

Try out the web IDE and interpreter [here](https://smucclaw.github.io/l4-lp/)!

More specifically, it contains:
- An equational theory equipping L4 constitutive rules with a denotational
  semantics in terms of Horn clauses in the Prolog term algebra
  (ie. Prolog's concrete syntax),
  implemented as a parser and transpiler in Clojure / Clojurescript.
  For more details, see
  [this section](#denotational-semantics-and-accompanying-parser-and-transpiler).

- A SWI-Prolog based rule engine runtime and
  [custom Prolog predicates](public/resources/swipl/prelude.pl)
  for executing transpiled L4 specifications, and obtaining execution traces.

  Note that while SWI-Prolog was chosen for convenience
  (details on why that is [here](#swi-prolog-based-rule-engine-runtime)),
  Prolog's syntax is
  commonly adopted for other related Horn clause based formalisms, like
  Datalog (eg. Nemo), ASP (eg. Clingo and scasp) and SMT-based
  [Constrained Horn Clause (CHC)](https://chc-comp.github.io/)
  solvers, so that if one prefers say SMT solvers,
  [Z3's CHC and Datalog support](https://microsoft.github.io/z3guide/docs/fixedpoints/intro/)
  can be also used to implement an executable runtime for the output of the L4
  &rarr; Prolog transpiler.
  Of course, one can also implement their own Horn clause execution engine.

  Currently, other such backends are planned but not implemented yet.

- Wrapper libraries that integrate L4 with the following languages/runtimes so
  that one can author and execute L4 from them:
  | Language | Status | Example usage |
  | -------- | ------ | ------------- |
  | Clojure / Clojurescript | :heavy_check_mark: | |
  | Java / JVM | in progress | |
  | Browser JS ESM | :heavy_check_mark: | [here](public/index.js) |
  | CommonJS NodeJS | :heavy_check_mark: | [here](public/node_example_usage.js) |
  | Python | :heavy_check_mark: | [here](src/l4_lp_py/example_usage.py) |

  Note that all these share the same Clojure + SWI-Prolog pipeline under
  the hood so that they have the _exact same_ functionality, execution
  behaviour and semantics.

- A web-based IDE and pipeline that uses the browser JS ESM library to
  parse, transpile and execute L4 completely in the browser
  (yes, the whole pipeline runs in the browser and does not involve any backend
  server).

See [this section](#some-details-and-discussion) for more details about the
semantics and pipeline implemented in this project.

# Dependencies

- Java
- [Clojure](https://clojure.org/guides/install_clojure)
- [Babashka](https://github.com/babashka/babashka)
- [pnpm](https://pnpm.io/installation)
- [SWI-Prolog](https://www.swi-prolog.org/)

This project is developed with JDK LTS 21, nodejs LTS 20, and SWI-Prolog 9.3.5.

# Usage
## Setup
```shell
  # Install npm dependencies.
  pnpm install

  # Pre-compile our SWI-Prolog runtime library to qlf files.
  pnpm build:swipl-qlfs
```

## Running the web IDE demo
- Make sure to [setup](#setup) the project first.
- Run the following command to start a local dev server in the `public` directory:

  ```shell
    pnpm start
  ```

- Navigate to <http://localhost:8000> in your browser.

  The webpage contains some instructions on how to use the IDE.

### Compiling optimised versions of the libraries and web IDE

Run the following command:

```shell
  pnpm build:all
```

# Some details and discussion
## Denotational semantics and accompanying parser and transpiler

  L4's denotational semantics is given as an equational theory from
  term algebra (ie concrete syntax) of L4 to that of Prolog.
  This is primarily documented and implemented by the
  `l4-rule->prolog-rule` in
  [l4_to_prolog.cljc](src/l4_lp/syntax/l4_to_prolog.cljc)

Points to note:

1. The `l4-rule->prolog-rule` function uses
   [Meander](https://github.com/noprompt/meander)
   to implement a term rewriting system which orients the equational theory
   from left to right.
   Each rewrite rule has an accompanying comment above it which describes
   the equation it implements.

2. We rewrite L4's concrete syntax directly into that of Prolog, so that
   Prolog's concrete syntax is essentially our intermediate representation.
   We _do not_ have any fancy data types or separate form of abstract
   syntax in our transpilation pipeline.

3. We chose to use Prolog as our intermediate reprensentation as it is
   extremely convenient to manipulate and work with, which is the case because:

   1. The concrete syntax of L4's constitutive rules is a variation of
      Prolog-style Horn clauses, containing additional syntactic sugar.

   2. Prolog is homoiconic, with a concrete syntax that is very close to
      s-expressions.

   3. The transpiler and most of the rest of the project is implemented in
      Clojure / Clojurescript which is a lisp, and hence convenient for
      manipulating s-expressions.

   4. As mentioned in the [overview](#l4-lp-overview),
      Prolog's concrete syntax is adopted and used by many formalisms and tools
      based on Horn clauses, so that the output of such a transpiler can be
      readily fed into any execution engine of one's choice
      (provided it has runtime libraries supporting L4's syntactic constructs).
      This improves L4's interoperability with various other downstream
      formalisms and tools.

4. We transform nested function applications into predicative syntax, eg:
   ```
   MIN (SUM 0 1 (PRODUCT 1 2)) 3 < MINUS 2 1
   ```
   gets expanded to something like:
   ```
   product_list([1, 2], Var__63),
   sum_list(([0, 1, Var__63]), Var__64),
   min_list(([Var__64, 3]), Var__65),
   minus_list(([2, 1]), Var__66),
   lt(Var__65, Var__66)
   ```
  
   The idea is that during the recursive transformation of a term,
   whenever we find a nested function application, we:
   1. Capture its evaluation context in a continuation.
   2. Generate a fresh variable of the form `Var__N`.
   3. Throw the fresh variable to the continuation.
   4. Lift the function application from its nested context up to the top
      most term.
   5. Convert the function application into a predicate application, using
      the fresh variable as the output variable.
    
   Note that we rely heavily on Meander's context-sensitive rewriting
   features
   (like the [$ macro](https://cljdoc.org/d/meander/epsilon/0.0.421/doc/operator-overview#subtree-search-))
   to conveniently manipulate nested terms and their evaluation contexts.

## SWI-Prolog based rule engine runtime
We implement a rule engine runtime in SWI-Prolog,
along with some custom Prolog predicates,
in order to execute L4 specifications that have been transpiled to our
intermediate representation, ie the concrete syntax of Prolog.

As mentioned [previously](#l4-lp-overview), we could have built a runtime
using any other Horn clause based formalism,
but for now, we chose Prolog, and SWI-Prolog in particular, as it is
extremely convenient for the following reasons:

1. Prolog is a backward-chaining rule engine for Horn clauses
   (or at least it can be used as one, because it uses SLD-resolution),
   and has powerful meta-programming functionality.

   This means that to implement a backward chaining rule engine in Prolog
   with custom behaviour,
   one can simply utilise meta-programming to customise the existing behaviour
   of the Prolog interpreter to suit their needs, rather than have to implement
   everything like the backward chaining and unification mechanism themselves.

   For instance, we can easily implement
   [meta-predicates](https://github.com/smucclaw/l4-lp/blob/aba0a7c15fe9b2e57fc9992a97c73f9dbea48b98/public/resources/swipl/prelude.pl#L40)
   that utilise SWI-Prolog's meta-programming APIs to hook into the Prolog interpreter
   and log execution traces.
   This can also be used to pass callbacks into the interpreter for an
   interactive Q&A expert system.
   Without meta-programming, we would have to implement a whole new interpreter
   ourselves just to get such functionality.

2. Prolog has good library support, including constraint solving and
   date libraries.
   
   This lets us easily solve planning problems on top of just executing
   programs, and integrate date logic reasoning.

3. SWI-Prolog has language bindings to various languages like
   JS (including browser JS via WASM), Python and Java.

   This lets us, without much additional effort, recycle the same
   codebase and runtime to implement JS, Python and Java libraries that
   integrate L4 with them, and even execute in the browser.
   In addition, this ensures that the execution behaviour and semantics across
   all these libraries is exactly the same.