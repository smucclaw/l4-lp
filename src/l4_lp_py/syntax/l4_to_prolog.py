import cytoolz.functoolz as ft

from . import dsl
from _l4_lp_nodejs_lib import _l4_lp

def l4_to_prolog_program_and_query(l4):
  match l4:
    case str():
      return _l4_lp.l4_to_prolog_program_and_query(l4)
    case _:
      return ft.pipe(l4, dsl.l4_to_edn_str, l4_to_prolog_program_and_query)

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