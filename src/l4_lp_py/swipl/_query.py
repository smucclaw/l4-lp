import asyncio
from pathlib import Path

import cytoolz.functoolz as ft
import janus_swi as janus

class _StackTrace:
  def __init__(self):
    self.stack_trace = []

  def log_stack_frame(self, stack_frame):
    self.stack_trace.append(stack_frame)

  def get(self):
    return self.stack_trace

def init_swipl_engine():
  path = ft.pipe(
    Path(__file__).parent / 'prolog' / 'prelude.qlf',
    lambda path: path.resolve()
  )
  janus.consult(f'{path}')

# https://www.swi-prolog.org/pldoc/man?section=janus-threads
# https://swi-prolog.discourse.group/t/janus-and-swish/7142/7

def _query_and_trace(prolog_program_and_queries):
  match prolog_program_and_queries:
    case {'program': program, 'queries': queries}:
      janus.attach_engine()
      janus.consult('program', program)

      for query in queries:
        stack_trace = _StackTrace()

        janus.query_once(
          f'query_and_trace(StackTrace, {query})',
          {'StackTrace': stack_trace}
        )

        yield {'query': query, 'stack_trace': stack_trace.get()}

      janus.detach_engine()

async def query_and_trace(prolog_program_and_queries):
  return await asyncio.to_thread(_query_and_trace, prolog_program_and_queries)

def query_and_trace_sync(prolog_program_and_queries):
  yield from asyncio.run(query_and_trace(prolog_program_and_queries))