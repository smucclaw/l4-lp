from collections.abc import Sequence
from dataclasses import dataclass
import datetime
from typing import Any, Optional

import cytoolz.functoolz as ft
import cytoolz.itertoolz as it
import edn_format as edn

from l4_lp_py.syntax.l4_to_prolog import l4_to_prolog_program_and_queries

@dataclass
class Rule:
  givens: Sequence[Any]
  giveths: Sequence[Any]
  head: Any
  body: Optional[Any]

@dataclass
class Fact:
  givens: Sequence[Any]
  giveths: Sequence[Any]
  fact: Any

@dataclass
class Query:
  givens: Sequence[Any]
  giveths: Sequence[Any]
  query: Any

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
    self.disjuncts = disjuncts

@dataclass
class Is:
  lhs: Sequence[Any]
  rhs: Sequence[Any]

def l4_to_edn(l4_program):
  match l4_program:
    case str() as s:
      return edn.Symbol(s)

    case Fact(givens, giveths, fact):
      return l4_to_edn(Rule(givens, giveths, fact, None))

    case Query(givens, giveths, query):
      givens = ('GIVEN', givens) if givens else ()
      giveths = ('GIVETH', giveths) if giveths else ()
      return l4_to_edn((*givens, *giveths, 'QUERY', *query))

    case Rule(givens, giveths, head, body):
      givens = ('GIVEN', givens) if givens else ()
      giveths = ('GIVETH', giveths) if giveths else ()
      body = ('IF', body) if body else ()
      return l4_to_edn((*givens, *giveths, 'DECIDE', *head, *body))

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

    case Is(lhs, rhs):
      return tuple(map(l4_to_edn, (*lhs, 'IS', *rhs)))
 
    case ast_node:
      return ast_node

def l4_edsl_to_edn_str(l4_program):
  return ft.pipe(l4_program, l4_to_edn, edn.dumps)

def l4_edsl_to_prolog_program_and_queries(l4_program):
  return ft.pipe(
    l4_program, l4_edsl_to_edn_str, l4_to_prolog_program_and_queries
  )