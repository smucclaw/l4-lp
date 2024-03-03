from collections.abc import Sequence
from dataclasses import dataclass
from typing import Any, Optional

import cytoolz.itertoolz as it

from javascript import require, globalThis
# from edn_format import Symbol
import edn_format as edn_format
import janus_swi as janus

@dataclass
class Var:
  name: str

@dataclass
class Rule:
  head: Any
  body: Optional[Any]

@dataclass(init = False)
class Fact:
  head: Any

  def __init__(self, *head):
    self.head = head

@dataclass(init = False)
class And:
  conjuncts: Sequence[Any]

  def __init__(self, *conjuncts):
    self.conjuncts = conjuncts

@dataclass(init = False)
class Or:
  disjuncts: Sequence[Any]

  def __init__(self, *disjuncts):
    self.dijuncts = disjuncts

def transpile_to_edn(ast_node):
  match ast_node:
    case str() as s:
      return edn_format.Symbol(s)

    case Var(name):
      return transpile_to_edn(f'var/{name}')

    case Fact(head):
      return transpile_to_edn(Rule(head, None))

    case Rule(head, body):
      body = ('IF', body) if body else ()
      return transpile_to_edn(('DECIDE', *head, *body))
    
    case tuple() as tuple_node:
      return tuple(map(transpile_to_edn, tuple_node))

    case list() as list_node:
      return list(map(transpile_to_edn, list_node))

    case And(conjuncts):
      return tuple(map(transpile_to_edn, it.interpose('AND', conjuncts)))

    case Or(disjuncts):
      return tuple(map(transpile_to_edn, it.interpose('OR', disjuncts)))
 
    case ast_node:
      return ast_node

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

goal = Fact('p of', Var('xs'), 'and', Var('x'))
# goal = And(
#   Fact('test', Var('x')),
#   Fact('py_transform_compound_term', Var('x'), Var('y'))
# )

program = edn_format.dumps(transpile_to_edn(program))
goal = edn_format.dumps(transpile_to_edn(goal))

l4_lp = require("./public/js/node_lib.js")

program = l4_lp.l4_program_to_prolog_program_str(program)
goal = l4_lp.l4_to_prolog_str(goal)

print(f'Transpiled program: {program}')
print(f'Transpiled goal: {goal}')

janus.consult("public/resources/swipl/py_prelude.qlf")

class StackTrace:
  def __init__(self):
    self.stack_trace = []

  def log_stack_frame(self, stack_frame):
    stack_frame_edn_str = l4_lp.swipl_stack_frame_to_edn_str(stack_frame)
    stack_frame_edn = edn_format.loads(stack_frame_edn_str)
    self.stack_trace.append(stack_frame_edn)

def query_and_trace(program, goal):
  janus.attach_engine()
  janus.consult('program', program)

  stack_trace = StackTrace()

  janus.query_once(
    'asserta(py_stack_trace(PyStackTrace))',
    {'PyStackTrace': stack_trace}
  )

  try:
    result = janus.query_once(goal)
  except janus.PrologException as e:
    print(f'Error: {e}')
  else:
    print(f'Result: {result}')
  finally:
    janus.detach_engine()
    return stack_trace.stack_trace
  
stack_trace = query_and_trace(program, goal)

print(f'Trace: {stack_trace}')