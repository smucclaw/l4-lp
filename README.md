# l4-lp

Experiments with:
- Using term rewriting via [Meander](https://github.com/noprompt/meander) to
  transpile a textual version of L4 constitutive rules and predicates into Prolog.
- Using [SWI-Prolog running on WASM](https://github.com/SWI-Prolog/npm-swipl-wasm) 
  and its interop with Javascript to evaluate the transpiled Prolog and
  generate execution traces.

Try it out [here](https://smucclaw.github.io/l4-lp/)!

For context, this evolved from an older pipeline involving
[logical-english-client](https://github.com/smucclaw/logical-english-client)
which utilised [Logical English](https://github.com/smucclaw/LogicalEnglish)
to transpile L4 to Prolog and generate execution traces.

## Dependencies

- java
- [pnpm](https://pnpm.io/installation)

This project is developed with JDK LTS 21 and nodejs LTS 20.12.0.

## Usage
### Setup
```shell
  pnpm install
```

### Running the demo
Currently, this project transpiles a sample L4 program to Prolog, and then
evaluates it using SWI-Prolog (running in WASM), capturing stack traces
as JS objects and transforming them back into Clojure forms.

- Make sure to [setup](#setup) the project first.
- Also, follow the instructions [here](https://github.com/binaryage/cljs-devtools/blob/master/docs/installation.md)
  to enable custom formatters in your browser
  (so that the Clojure data structures which are logged in the console are readable).

- Run the following command to start a local dev server in the `public` directory:

  ```shell
    pnpm start
  ```

- Go to <http://localhost:8000> in your browser and check your console.
  You should see the output of the L4 &rarr; transpiler, and the stack trace
  obtained from SWI-Prolog. 


- If you edit and save a file, it will recompile the code and reload the
  browser to show the updated version.

### Compile an optimised version of the library

Run the following command:

```shell
  pnpm build:prod
```

This compiles an optimised, production-ready version of the library to
`public/js/main.js`.