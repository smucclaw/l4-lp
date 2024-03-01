import {
  l4_to_prolog_str,
  l4_program_to_prolog_program_str,
  query_and_trace
}
from "./js/main.js";

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