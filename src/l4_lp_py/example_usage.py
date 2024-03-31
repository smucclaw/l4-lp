import asyncio
import datetime

import pout

from syntax.dsl import And, Or, Fact, Rule, Date, Var, l4_to_edn_str
from syntax.l4_to_prolog import l4_to_prolog_str, l4_program_to_prolog_str
from swipl.query import init_swipl_engine, query_and_trace

program = '''
DECIDE p
IF q AND r

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
'''

goal = 'q'

program = l4_program_to_prolog_str(program)
goal = l4_to_prolog_str(goal)

pout.v(program)

init_swipl_engine()

stack_trace = asyncio.run(query_and_trace(program, goal))

pout.v(stack_trace)

# program = [
#   Rule(
#     ['x', 'xs', 'y', 'ys', 'z', 'date'],
#     ('p of', 'xs', 'and', 'x', 'and', 'z', 'and', 'date'),
#     And(
#       ('xs', 'IS THE LIST OF ALL', 'y', 'SUCH THAT', 'q holds for', 'y'),

#       ('ys', 'IS THE LIST OF ALL', 'y', 'SUCH THAT', 'r holds for', 'y'),

#       ('r holds for', 'z'),

#       ('x', 'IS', 'SUM', 'xs'),
      
#       ('y', 'IS', ('SUM', 'ys')),

#       ('y', '>', 0),

#       ('date', 'IS WITHIN', 3, 'DAYS', 'OF', datetime.date(2024, 1, 20))
#     )
#   ),

#   Fact('q holds for', 0),
#   Fact('q holds for', 1),

#   Rule(
#     ['z'],
#     ('r holds for', 'z'),
#     # ('member', Var('z'), [3, 4])
#     Or(('z', 'IS', 3), ('z', 'IS', 4))
#   ),

#   # Fact('test', ('p', [0, 1]))
# ]

# goal = Fact(
#   'p of', Var('xs'), 'and', Var('x'), 'and', 4, 'and',
#   datetime.date(2024, 1, 21)
# )

# goal = Fact(
#   And(
#     (Var('date'), 'IS A VALID DATE'),
#     (datetime.date(2024, 1, 20), '+', 3, 'DAYS', 'IS', Var('date'))
#   )
# )