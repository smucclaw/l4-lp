import asyncio
import datetime

import pout

from l4_lp_py.syntax.edsl import And, Or, Fact, Rule, Query, Is, Date, l4_edsl_to_prolog_program_and_queries
from swipl.query import init_swipl_engine, query_and_trace

# l4_program = '''
# DECIDE p
# IF q AND r

# QUERY q

# DECIDE q
# WHEN 3 IS SUM 0 1 (MIN (SUM 0 3) 2)

# GIVEN (x IS A Number)
# DECIDE x is between 0 and 10 or is 100
# IF 0 <= x AND x <= 10
# OR x IS MAX 100 -20

# DECIDE (2023 - 1 - 10) is a date

# GIVEN x (xs IS A LIST OF Number)
# DECIDE x is the sum of xs
# WHERE x IS SUM xs
# '''

l4_program = (
  Rule(
    givens = [], giveths = [],
    head = 'p',
    body = Is(lhs = ('MIN', 0, 1, 2), rhs = ('SUM', [1, -1]))
  ),
  Query(
    givens = [], giveths = [],
    query = 'p'
  ),
  Fact(
    [], [],
    datetime.date(2024, 1, 1), 'is a date'
  )
)

prolog_program_and_queries = l4_edsl_to_prolog_program_and_queries(l4_program)
pout.v(prolog_program_and_queries)

init_swipl_engine()

query_results = query_and_trace(prolog_program_and_queries)

for result in query_results:
  pout.v(result)