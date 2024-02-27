# l4-swipl-wasm

Experiments with:
- Transpiling a textual version of L4 constitutive rules and predicates into
  Prolog.
- Using [SWI-Prolog running on WASM](https://github.com/SWI-Prolog/npm-swipl-wasm) 
  and its interop with Javascript to generate stack traces from the interpreter.

## Dependencies

- java
- [pnpm](https://pnpm.io/installation)

This project is developed with JDK LTS 21 and nodejs LTS 20.11.1.

## Usage
### Setup
```shell
  pnpm install
```

### Running the demo
- Make sure to [setup](#setup) the project first.

- Run the following command to start a local dev server in the `public` directory:

  ```shell
    pnpm start
  ```

- Go to <http://localhost:8000> in your browser.
  You should see a justification tree visualised by
  [guifier](https://guifier.com/).

- If you edit and save a file, it will recompile the code and reload the
  browser to show the updated version.

### Compile an optimised version of the library

Run the following command:

```shell
  pnpm build:prod
```

This compiles an optimised, production-ready version of the library to
`public/js/main.js`.