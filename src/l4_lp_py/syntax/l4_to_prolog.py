import cytoolz.functoolz as ft

from . import edsl
from _l4_lp_nodejs_lib import _l4_lp

def l4_to_prolog_program_and_queries(l4):
  match l4:
    case str():
      return _l4_lp.l4_to_prolog_program_and_queries(l4).valueOf()
    case _:
      return ft.pipe(l4, edsl.l4_to_edn_str, l4_to_prolog_program_and_queries)

# def l4_program_to_prolog_str(l4_program):
#   match l4_program:
#     case str():
#       return _l4_lp.l4_program_edn_str_to_prolog_program_str(l4_program)
#     case _:
#       return ft.pipe(
#         l4_program,
#         dsl.l4_to_edn_str,
#         l4_program_to_prolog_str
#       )