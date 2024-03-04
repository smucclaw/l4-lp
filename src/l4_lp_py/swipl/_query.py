from pathlib import Path

import cytoolz.functoolz as ft
import janus_swi as janus

class _StackTrace:
  def __init__(self):
    self.stack_trace = []

  def log_stack_frame(self, stack_frame):
    self.stack_trace.append(stack_frame)

def init_swipl_engine():
  path = ft.pipe(
    Path(__file__).parent / 'prolog' / 'prelude.qlf',
    lambda path: path.resolve()
  )
  janus.consult(f'{path}')

def query_and_trace(program, goal):
  janus.attach_engine()
  janus.consult('program', program)

  stack_trace = _StackTrace()

  janus.query_once(
    'asserta(py_stack_trace(PyStackTrace))',
    {'PyStackTrace': stack_trace}
  )

  goal = f'eval_and_trace({goal})'

  try:
    janus.query_once(goal)
  except janus.PrologException as e:
    print(f'Error: {e}')
  
  janus.detach_engine()
  return stack_trace.stack_trace