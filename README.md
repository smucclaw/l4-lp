# l4-lp (L4 Logic Program) overview

This project formalises a semantics for L4, a legal DSL for the law, and
implements a rule engine execution pipeline, along with various language
binding libraries for interacting with L4 and the pipeline.
Among these is a browser JS ESM library which is used to implement an
IDE that parses, transpiles and executes L4, as well as visualise execution
traces _completely in the browser_.

Try out our IDE and rule engine [here](https://smucclaw.github.io/l4-lp/)!

For context, this evolved from an older pipeline involving
[logical-english-client](https://github.com/smucclaw/logical-english-client)
which utilised [Logical English](https://github.com/smucclaw/LogicalEnglish)
to transpile L4 to Prolog and generate execution traces.

More precisely, this project contains:
- A denotational semantics for L4 constitutive rules,
  formalised as an equational theory
  mapping L4 rules into Horn clauses in the Prolog term algebra
  (ie. Prolog's concrete syntax),
  implemented as a parser and transpiler in Clojure / Clojurescript.
  
  Note that we use such Horn clauses as our intermediate representation
  because for one, they are syntactically close to L4, and are convenient to
  work with.
  Moreover, Prolog / Datalog Horn clauses have well-understood
  semantics, and are widely adopted as the standard format for
  (and hence highly interoperable with) many rule engines and static analysis 
  tools
  (including SMT based solvers like
  [Z3](https://microsoft.github.io/z3guide/docs/fixedpoints/intro/)).
  See [this section](#denotational-semantics-and-accompanying-parser-and-transpiler)
  for more details.

- A SWI-Prolog based rule engine runtime, accompanied by
  [custom Prolog predicates](public/resources/swipl/prelude.pl)
  for executing transpiled L4 specifications, and obtaining execution traces.

  Note that SWI-Prolog was chosen for convenience
  (details on why that is [here](#swi-prolog-based-rule-engine-runtime)),
  and that one can easily build a runtime for our Prolog intermediate
  representation using Z3 for instance.
  Currently, integrating other such backends is planned but not worked on yet.

- Language binding libraries that allow other languages to interact with L4 and
  its execution pipeline:
  | Language | Library status | Example usage |
  | -------- | ------ | ------------- |
  | Clojure / JVM | In progress | [JVM main.clj](src/l4_lp/main.clj) |
  | Clojurescript / ESM in browser | :heavy_check_mark: | [index.js](public/index.js) |
  | Clojurescript / CommonJS on NodeJS | :heavy_check_mark: | [node_example_usage.js](public/node_example_usage.js) |
  | Python | :heavy_check_mark: | [example_usage.py](src/l4_lp_py/example_usage.py) |

  Note that all these bind to the same Clojure + SWI-Prolog code base under the
  hood so that they have the _exact same_ functionality, behaviour and
  semantics.
  See [here](_) for more details.

- An in-browser IDE powered by [CodeMirror](https://codemirror.net/)
  and the browser ESM library to parse, transpile, execute L4 and visualise
  execution traces (via [guifier](https://github.com/maliknajjar/guifier))
  completely in the browser.

See [this section](#details-and-discussion) for more details about the
semantics and pipeline implemented in this project.

# Dependencies

- Java
- [Clojure](https://clojure.org/guides/install_clojure)
- [Babashka](https://github.com/babashka/babashka)
- [pnpm](https://pnpm.io/installation)
- [SWI-Prolog](https://www.swi-prolog.org/)

This project is developed with:
- JDK LTS 21
- Nodejs LTS 20
- SWI-Prolog 9.3.6

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

## Compiling optimised versions of the language binding libraries and web IDE

Run the following command:

```shell
  pnpm build:all
```

# Details and discussion
## Denotational semantics and accompanying parser and transpiler

L4's denotational semantics is given as an equational theory from
the term algebra (ie concrete syntax) of L4 to that of Prolog.
This is primarily documented and implemented by the
`l4-rule->prolog-rule` in
[l4_to_prolog.cljc](src/l4_lp/syntax/l4_to_prolog.cljc).

### Points to note

1. We use
   [Meander](https://github.com/noprompt/meander)
   to implement a term rewriting system which orients the equational theory
   from left to right.

   Each rewrite rule has an accompanying comment above it which describes
   the equation it implements.

1. We rewrite L4's concrete syntax directly into that of Prolog,
   so that Prolog's concrete syntax is essentially our intermediate
   representation.
   We _do not_ have any fancy data types or separate form of abstract
   syntax in our transpilation pipeline.

2. Prolog syntax makes for an extremely convenient intermediate representation
   to work with because:
   1. The concrete syntax of L4's constitutive rules is a variation of
      Prolog-style Horn clauses, containing additional syntactic sugar.

   2. Prolog is homoiconic, with a concrete syntax that is very close to
      s-expressions.

   3. The transpiler and most of the rest of the project is implemented in
      Clojure / Clojurescript which is a lisp, and hence convenient for
      manipulating s-expressions.

   4. As mentioned in the [overview](#l4-lp-overview),
      such an intermediate representation is a rather standard one adopted by
      many other downstream formalisms and tools that are
      based on Horn clauses.
      These tools utilise relatively similar semantics
      (which as mentioned, are well-understood),
      and can readily consume our intermediate representation.

      They include:
      - Prolog engines, like
        [SWI](https://www.swi-prolog.org/),
        [Ciao](https://ciao-lang.org/)
        and
        [Scryer](https://www.scryer.pl/).

      - Datalog, like
        [Souffle](https://souffle-lang.github.io/index.html)
        (popular tool for static analysis)
        and
        [Nemo](https://github.com/knowsys/nemo)
        (high performance rule engine which can run in the browser)
  
      - ASP, like
        [sCASP](https://github.com/SWI-Prolog/sCASP)
        and
        [Clingo](https://github.com/potassco/clingo)
        (these are good for counterfactual explanations and abduction)
  
      - SMT-based Constrained Horn Clause (CHC) solvers, like
        [Z3](https://microsoft.github.io/z3guide/docs/fixedpoints/intro/)
        and
        [Eldarica](https://github.com/uuverifiers/eldarica)

    In other words, adopting Prolog syntax as our intermediate representation
    keeps our parser and transpiler really lean (ie. under 300 lines of code),
    and also
    allows us to equip L4 with standard, well-understood Horn clause based
    semantics, and
    facilitates the integration of a wide variety of tools and their
    functionality (which we hope to achieve eventually).

3. We transform nested function applications into predicative syntax, eg:
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

As mentioned [previously](#l4-lp-overview),
we could have built a runtime
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

2. SWI-Prolog has good library support, including constraint solving and
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

## Language binding libraries for L4
We provide libraries for Clojure, Java, JS and Python with language bindings
to L4, which expose programmatic APIs for these languages to interop with
L4 and call various
parts of the
Clojure parser &rarr;
Clojure transpiler &rarr;
SWI Prolog rule engine
pipeline, and interop with L4.

This is easy because:
- Our parser and transpiler are written entirely in Clojure, which compiles to
  and interops bidirectionally with _both_ JVM bytecode and JS.
  
  This means that just by writing our parser &rarr; transpiler pipeline in
  Clojure, we get to integrate it with Java and JS for free.

- [JsPyBridge](https://github.com/extremeheat/JSPyBridge)
  offers almost seamless bidirectional interop between JS and Python.

  This lets us integrate our Clojure parser &rarr; Clojure transpiler with
  Python as well, for free, because we can interop
  Clojure &harr; JS &harr; Python

- SWI-Prolog has good bindings and bidirectional interop with
  [JS via WASM](https://github.com/SWI-Prolog/npm-swipl-wasm),
  [Python](https://github.com/SWI-Prolog/packages-swipy)
  and
  [Java](https://jpl7.org/).

  Using this, we wrote some glue code in Clojure and Python to connect the
  output of the Clojure transpiler to the SWI Prolog rule engine
  to complete the whole pipeline.