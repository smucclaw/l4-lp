from collections.abc import Sequence
from dataclasses import dataclass
import datetime
from typing import Any, Optional

import cytoolz.functoolz as ft
import cytoolz.itertoolz as it
import edn_format as edn

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

@dataclass
class Date:
  year: Any
  month: Any
  day: Any

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

def l4_to_edn(l4_program):
  match l4_program:
    case str() as s:
      return edn.Symbol(s)

    case Var(name):
      return l4_to_edn(f'var/{name}')

    case Fact(head):
      return l4_to_edn(Rule(head, None))

    case Rule(head, body):
      body = ('IF', body) if body else ()
      return l4_to_edn(('DECIDE', *head, *body))

    case Date(year, month, day):
      return l4_to_edn((year, '-', month, '-', day))

    case datetime.date(year = year, month = month, day = day):
      return l4_to_edn(Date(year, month, day))

    case tuple() as tuple_node:
      return tuple(map(l4_to_edn, tuple_node))

    case list() as list_node:
      return list(map(l4_to_edn, list_node))

    case And(conjuncts):
      return tuple(map(l4_to_edn, it.interpose('AND', conjuncts)))

    case Or(disjuncts):
      return tuple(map(l4_to_edn, it.interpose('OR', disjuncts)))
 
    case ast_node:
      return ast_node

def l4_to_edn_str(l4_program):
  return ft.pipe(l4_program, l4_to_edn, edn.dumps)