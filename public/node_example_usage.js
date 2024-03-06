const path = require('node:path');

const l4_lp = require('./js/l4_lp_nodejs_lib.js');

async function main() {
  let program = `
    [(DECIDE p of var/xs and var/x
    IF (var/xs IS THE LIST OF ALL var/y SUCH THAT q holds for var/y)
    AND (var/x IS THE SUM OF var/xs))

    (q holds for 0)
    (q holds for 1)]
  `;

  let goal = "(p of var/xs and var/x)";

  program = l4_lp.l4_program_edn_str_to_prolog_program_str(program);
  goal = l4_lp.l4_edn_str_to_prolog_str(goal);

  console.log([program, goal]);

  await l4_lp.init_swipl_engine(
    path.join('..', '..', 'src', 'l4_lp_py', 'swipl', '_query.py')
  );

  trace = await l4_lp.query_and_trace(program, goal);
  console.log(`Stack trace: ${trace}`);
}

main();