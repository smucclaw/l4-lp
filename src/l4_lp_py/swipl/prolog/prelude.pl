:- use_module(library(janus)).

:- ['../../../../public/resources/swipl/prelude'].

:- dynamic stack_trace/1.

log_stack_frame(StackFrame) =>
  py_transform_compound_term(StackFrame, TransformedStackFrame),
  stack_trace(StackTrace),
  py_call(StackTrace:log_stack_frame(TransformedStackFrame)).

py_transform_compound_term(InString, OutTaggedDict), string(InString) =>
  OutTaggedDict = py{'$t': s, v: InString}.

py_transform_compound_term(InAtom, OutAtom), atomic(InAtom) =>
  OutAtom = InAtom.

py_transform_compound_term(InVar, OutDict), var(InVar) =>
  term_string(InVar, InVarString),
  atom_string(InVarAtom, InVarString),
  OutDict = py{'$t': v, v: InVarAtom}.

py_transform_compound_term(InTerm, OutTerm), is_list(InTerm) =>
  maplist(py_transform_compound_term, InTerm, OutTerm).

py_transform_compound_term(InDict, OutDict), is_dict(InDict) =>
  dict_pairs(InDict, Tag, InPairs),
  pairs_keys_values(InPairs, InKeys, InVals),
  maplist(py_transform_compound_term, InKeys, OutKeys),
  maplist(py_transform_compound_term, InVals, OutVals),
  pairs_keys_values(OutPairs, OutKeys, OutVals),
  dict_pairs(OutDict, Tag, OutPairs).

py_transform_compound_term(InCompoundTerm, OutDict) =>
  InCompoundTerm =.. [Functor | Args],
  maplist(py_transform_compound_term, Args, OutArgs),
  dict_pairs(OutDict, py, ['$t'-t, functor-Functor, Functor-OutArgs]).