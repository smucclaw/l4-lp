const path = require('node:path');

const l4_lp = require('./js/l4_lp_nodejs_lib.js');

async function main() {
  const l4_program = `
DECIDE p
IF q AND r

QUERY q

DECIDE q
WHEN 3 IS SUM 0 1 (MIN (SUM 0 3) 2)

GIVEN (x IS A Number)
DECIDE x is between 0 and 10 or is 100
IF 0 <= x AND x <= 10
OR x IS MAX 100 -20

DECIDE (2023 - 1 - 10) is a date

GIVEN x (xs IS A LIST OF Number)
DECIDE x is the sum of xs
WHERE x IS SUM xs
  `;

  const prolog_program_and_queries = l4_lp.l4_to_prolog_program_and_queries(l4_program);
  const { program, query } = prolog_program_and_queries;

  console.log([program, query]);

  await l4_lp.init_swipl_engine(
    path.join('..', '..', 'src', 'l4_lp_py', 'swipl', '_query.py')
  );

  query_results = await l4_lp.query_and_trace(prolog_program_and_queries);
  console.log(`Query results: ${JSON.stringify(query_results, null, 2)}`);
}

main();