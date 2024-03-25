import {
  l4_to_prolog_str,
  l4_program_to_prolog_program_str,
  query_and_trace
}
from "./js/main.js";

let program = `
[(DECIDE p
  IF (q AND r))

 (DECIDE (var/x is between 0 and 10 or is 100)
  IF (((0.0 <= var/x) AND (var/x <= 10.0)) OR (var/x IS 100.0)))

 (DECIDE ((2023 - 1 - 10) is a date))]
`;

let goal = "(var/d is a date)";

program = l4_program_to_prolog_program_str(program);
goal = l4_to_prolog_str(goal);

console.log("Transpiled program: ", program);
console.log("Transpiled goal: ", goal);

const stack_trace = await query_and_trace(program, goal); 

const { default: Guifier } = await import(
  "https://cdn.jsdelivr.net/npm/guifier@1.0.24/dist/Guifier.js"
);

// console.log("Stack trace: ", stack_trace);

const _guifier = new Guifier({
  data: stack_trace,
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