import {
  l4_to_prolog_str,
  l4_program_to_prolog_program_str,
  query_and_trace
}
from "./js/main.js";

let program = `
  [(DECIDE p of var/xs and var/x and var/z
  IF (var/xs IS THE LIST OF ALL var/y SUCH THAT q holds for var/y)
  AND (var/ys IS THE LIST OF ALL var/y SUCH THAT r holds for var/y)
  AND (r holds for var/z)
  AND (var/x IS THE SUM OF var/xs)
  AND (var/y IS THE SUM OF var/ys)
  AND (var/y > 0))

  (DECIDE q holds for 0)
  (DECIDE q holds for 1)

  (DECIDE r holds for var/z
  IF (var/z IS 3)
  OR (var/z IS 4))]
`;

let goal = "(p of var/xs and var/x and 4)";

program = l4_program_to_prolog_program_str(program);
goal = l4_to_prolog_str(goal);

console.log("Transpiled program: ", program);
console.log("Transpiled goal: ", goal);

const stack_trace = await query_and_trace(program, goal); 

console.log("Stack trace: ", stack_trace);

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