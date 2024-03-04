from collections.abc import Sequence
from dataclasses import dataclass
from typing import Any, Optional

import cytoolz.functoolz as ft

# from edn_format import Symbol
import edn_format as edn
# import janus_swi as janus

from . import dsl
from _l4_lp_nodejs_lib import _l4_lp

def l4_to_prolog_str(l4):
  match l4:
    case str():
      return _l4_lp.l4_edn_str_to_prolog_str(l4)
    case _:
      return ft.pipe(l4, dsl.l4_to_edn_str, l4_to_prolog_str)

def l4_program_to_prolog_str(l4_program):
  match l4_program:
    case str():
      return _l4_lp.l4_program_edn_str_to_prolog_program_str(l4_program)
    case _:
      return ft.pipe(
        l4_program,
        dsl.l4_to_edn_str,
        l4_program_to_prolog_str
      )