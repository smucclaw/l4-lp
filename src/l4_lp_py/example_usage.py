import asyncio
import datetime
import pprint

from syntax.dsl import And, Or, Var, Fact, Rule, Date, l4_to_edn_str
from syntax.l4_to_prolog import l4_to_prolog_str, l4_program_to_prolog_str
from swipl.query import init_swipl_engine, query_and_trace

program = [
  Rule(
    ('p of', Var('xs'), 'and', Var('x')),
    And(
      (Var('xs'), 'IS THE LIST OF ALL', Var('y'), 'SUCH THAT', 'q holds for', Var('y')),

      (Var('x'), 'IS THE SUM OF', Var('xs'))
    )
  ),
  Fact('q holds for', 0),
  Fact('q holds for', 1),

  # Fact('test', ('p', [0, 1]))
]

# goal = Fact('p of', Var('xs'), 'and', Var('x'))

goal = Fact(
  And(
    (Var('date'), 'IS A VALID DATE'),
    (datetime.date(2024, 1, 20), '+', 3, 'DAYS', 'IS', Var('date'))
  )
)

program = l4_program_to_prolog_str(program)
goal = l4_to_prolog_str(goal)

init_swipl_engine()

trace = asyncio.run(query_and_trace(program, goal))

print('Stack trace:')
pprint.pprint(trace)