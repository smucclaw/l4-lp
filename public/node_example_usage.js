const path = require('node:path');

const l4_lp = require('./js/l4_lp_nodejs_lib.js');

async function main() {
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

  program = l4_lp.l4_program_edn_str_to_prolog_program_str(program);
  goal = l4_lp.l4_edn_str_to_prolog_str(goal);

  console.log([program, goal]);

  await l4_lp.init_swipl_engine(
    path.join('..', '..', 'src', 'l4_lp_py', 'swipl', '_query.py')
  );

  stack_trace = await l4_lp.query_and_trace(program, goal);
  console.log(`Stack trace: ${JSON.stringify(stack_trace, null, 2)}`);
}

main();