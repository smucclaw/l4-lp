import asyncio
import datetime

import pout

from syntax.dsl import And, Or, Var, Fact, Rule, Date, l4_to_edn_str
from syntax.l4_to_prolog import l4_to_prolog_str, l4_program_to_prolog_str
from swipl.query import init_swipl_engine, query_and_trace

program = [
  Rule(
    ('p of', Var('xs'), 'and', Var('x'), 'and', Var('z')),
    And(
      (Var('xs'), 'IS THE LIST OF ALL', Var('y'), 'SUCH THAT', 'q holds for', Var('y')),

      (Var('ys'), 'IS THE LIST OF ALL', Var('y'), 'SUCH THAT', 'r holds for', Var('y')),

      ('r holds for', Var('z')),

      (Var('x'), 'IS THE SUM OF', Var('xs')),
      
      (Var('y'), 'IS THE SUM OF', Var('ys')),

      (Var('y'), '>', 0)
    )
  ),

  Fact('q holds for', 0),
  Fact('q holds for', 1),

  Rule(
    ('r holds for', Var('z')),
    # ('member', Var('z'), [3, 4])
    Or((Var('z'), 'IS', 3), (Var('z'), 'IS', 4))
  ),

  # Fact('test', ('p', [0, 1]))
]

goal = Fact('p of', Var('xs'), 'and', Var('x'), 'and', 4)

# goal = Fact(
#   And(
#     (Var('date'), 'IS A VALID DATE'),
#     (datetime.date(2024, 1, 20), '+', 3, 'DAYS', 'IS', Var('date'))
#   )
# )

program = l4_program_to_prolog_str(program)
goal = l4_to_prolog_str(goal)

init_swipl_engine()

stack_trace = asyncio.run(query_and_trace(program, goal))

pout.vs(stack_trace)