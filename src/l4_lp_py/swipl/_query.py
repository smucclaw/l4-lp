import asyncio
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

# https://www.swi-prolog.org/pldoc/man?section=janus-threads
# https://swi-prolog.discourse.group/t/janus-and-swish/7142/7

def _query_and_trace(program, goal):
  janus.attach_engine()
  janus.consult('program', program)

  stack_trace = _StackTrace()

  janus.query_once(
    'asserta(py_stack_trace(PyStackTrace))',
    {'PyStackTrace': stack_trace}
  )

  goal = f'once_trace_all({goal})'

  try:
    janus.query_once(goal)
  except Exception as _domain_error:
    # print(f'Error: {domain_error}')
    pass
  
  janus.detach_engine()
  return stack_trace.stack_trace

async def query_and_trace(program, goal):
  return await asyncio.to_thread(_query_and_trace, program, goal)

def query_and_trace_sync(program, goal):
  return asyncio.run(query_and_trace(program, goal))