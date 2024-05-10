import {
  l4_to_prolog_program_and_queries,
  query_and_trace
} from "./js/main.js";

const program_edn = await fetch("program.edn").then(resp => resp.text());

const prolog_program_and_query = l4_to_prolog_program_and_query(program_edn);
const { program, query } = prolog_program_and_query;

console.log("Transpiled program: ", program);
console.log("Transpiled queries: ", queries);

const query_results = await query_and_trace(prolog_program_and_queries); 

const { default: Guifier } = await import(
  "https://cdn.jsdelivr.net/npm/guifier@1.0.24/dist/Guifier.js"
);

// console.log("Stack trace: ", stack_trace);

const _guifier = new Guifier({
  data: query_results,
  dataType: "js",
  elementSelector: "#guifier",
  withoutContainer: true,
  readOnlyMode: true
});

// const program = `
//   person(adam).
//   person(bob).

//   parent(bob, adam).
//   parent(charlie, bob).

//   grandparent(X, Y) :-
//     parent(X, Z),
//     parent(Z, Y).
// `;

// const goal = 'grandparent(X, adam)';

// const trace = await query_and_trace(program, goal);
// console.log("Trace: ", trace);