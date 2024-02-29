% https://swi-prolog.discourse.group/t/tool-for-drawing-sld-trees-in-prolog/4737

% https://www.swi-prolog.org/pldoc/man?section=debugoverview
% https://swi-prolog.discourse.group/t/swi-prolog-in-the-browser-using-wasm/5650
% https://www.swi-prolog.org/pldoc/man?section=wasm

:- use_module(library(macros)).
:- use_module(library(dicts)).

:- use_module(library(clpBNR)).
:- use_module(library(date_time)).

% :- use_module(library(clpq)).

% :- [library(date_time), library(clpBNR)].

% :- set_prolog_flag(gc, off).

:- leash(-all).
:- visible(+all).

:- dynamic log_stack_frame/1.

prolog_trace_interception(Port, Frame, _Choice, continue) :-
  prolog_frame_attribute(Frame, goal, Current_goal),
  prolog_frame_attribute(Frame, level, Recursion_level),

  prolog_frame_attribute(Frame, parent_goal(Parent_frame), Current_goal),
  prolog_frame_attribute(Parent_frame, goal, Parent_goal),

  StackFrame = stack_frame{
    current_goal:Current_goal,
    parent_goal:Parent_goal,
    port:Port,
    recursion_depth:Recursion_level
  },

  log_stack_frame(StackFrame).

#define(
  toggle_tracing(Toggle, Goal, Toggle0),
  (Toggle, ((Goal, !, Toggle0) ; (Toggle0, fail)))
).

eval_and_trace(Goal) :-
  #toggle_tracing(trace, call(Goal), notrace).

% https://www.swi-prolog.org/pldoc/man?predicate=op/3
:- op(800, xfx, 'IS').
:- op(800, xfx, 'IS_').

product_list([], Result) => Result 'IS' 0.
product_list([X], Result) => Result 'IS' X.
product_list([X | Xs], Result) =>
  product_list(Xs, Result0),
  Result 'IS' X * Result0.

sum_list_([], Result) => Result 'IS' 0.
sum_list_([X | Xs], Result) =>
  sum_list_(Xs, Result0),
  Result 'IS' X + Result0.

min_list_([], Result) => Result 'IS' inf.
min_list_([X | Xs], Result) =>
  min_list_(Xs, Result0),
  Result 'IS' min(X, Result0).

max_list_([], Result) => Result 'IS' -inf.
max_list_([X | Xs], Result) =>
  max_list_(Xs, Result0),
  Result 'IS' max(X, Result0).

% This first uses constraint solving via clpBNR for unifying arithmetic
% expressions, modulo the theory of reals.
% If that fails, then we know that at least one of the terms that we're trying
% to unify is compound, with a functor that is not in the signature of the
% first order theory of reals.
% In this case, we do the following (which is a variant of unification
% with an occurs check, just modulo the theory of reals):
% 1. Syntactically unify the functor, ie modulo the empty theory.
% 2. Recursively unify the arguments, modulo the theory of reals.
%
% If this is too slow, we can replace the replace the constraint based
% unification with arithmetic evaluation via "is", or the unify with occurs
% check with plain old unification.
X 'IS' Y :-
  notrace,
  catch(
    ({X == Y, Y == X}, solve([X, Y])),
    _,
    (X 'IS_' Y, trace)
  ), !,
  trace.

X 'IS' Y :- #toggle_tracing(notrace, X 'IS_' Y, trace).

X 'IS_' Y :-
  X =.. [Functor | X_args],
  Y =.. [Functor | Y_args],
  maplist('IS', X_args, Y_args).

% X 'IS' Y :-
  % catch(X is Y, _, fail), !

% X 'IS' Y :-
%   catch(Y is X, _, fail), !.

% X 'IS' Y :-
%   catch((Z is X, Z is Y), _, fail), !.

% X 'IS' Y :-
  % notrace, ((unify_with_occurs_check(X, Y), !, trace); trace).

#define(
  arithmetic_comparison(Comparison),
  #toggle_tracing(notrace, {Comparison}, trace)
).

% As with 'IS', we use constraint solving via clpBNR for handling arithmetic
% comparisons.
% If this is slow, try reverting to plain old arithmetic comparisons.
lt(X, Y) :- #arithmetic_comparison(X < Y).
leq(X, Y) :- #arithmetic_comparison(X =< Y).

% '<='(X, Y) :- X =< Y.

gt(X, Y) :- #arithmetic_comparison(X > Y).
geq(X, Y) :- #arithmetic_comparison(X >= Y).
