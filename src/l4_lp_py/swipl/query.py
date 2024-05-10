import cytoolz.functoolz as ft
# import edn_format as edn

from . import _query
from _l4_lp_nodejs_lib import _l4_lp

def init_swipl_engine():
  return _query.init_swipl_engine()

async def query_and_trace(prolog_program_and_query):
  query_results = await _query.query_and_trace(prolog_program_and_query)
  for result in query_results:
    result['stack_trace'] = ft.pipe(
      result['stack_trace'],
      _l4_lp.swipl_stack_trace_to_js,
      lambda x: x.valueOf()
    )
  return query_results

  # for index, stack_frame in enumerate(stack_trace):
  #   stack_trace[index] = ft.pipe(
  #     stack_frame, _l4_lp.swipl_stack_frame_to_edn_str, edn.loads
  #   )

  # return stack_trace