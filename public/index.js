import { eval_and_trace } from "./js/main.js";

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

// const trace = await eval_and_trace(program, goal);
// console.log("Trace: ", trace);