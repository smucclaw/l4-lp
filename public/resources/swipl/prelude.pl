% https://swi-prolog.discourse.group/t/tool-for-drawing-sld-trees-in-prolog/4737

% https://www.swi-prolog.org/pldoc/man?section=debugoverview
% https://swi-prolog.discourse.group/t/swi-prolog-in-the-browser-using-wasm/5650
% https://www.swi-prolog.org/pldoc/man?section=wasm

:- use_module(library(macros)).
:- use_module(library(dicts)).

% :- use_module(library(clpq)).

:- [library(clpBNR)].
:- [library(date_time)].

% :- set_prolog_flag(gc, off).

% https://github.com/SWI-Prolog/packages-swipy/blob/feff32f02a82363df86483f1b9448ccc16d5ac1b/janus/janus.py#L334
% https://github.com/SWI-Prolog/packages-swipy/blob/feff32f02a82363df86483f1b9448ccc16d5ac1b/janus/janus.pl#L1234
load_from_string(File, Data, Module) =>
  setup_call_cleanup(
    open_string(Data, In),
    load_files(Module:File, [stream(In)]),
    close(In)
  ). 

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

eval_and_trace(Goal) :-
  setup_call_cleanup(trace, call(Goal), notrace).

% https://www.swi-prolog.org/pldoc/man?predicate=op/3
:- op(700, xfx, eq).
:- op(700, xfx, lt).
:- op(700, xfx, leq).
:- op(700, xfx, gt).
:- op(700, xfx, geq).

product_list([], Result) => Result eq 0.
product_list([X], Result) => Result eq X.
product_list([X | Xs], Result) =>
  product_list(Xs, Result0),
  Result eq X * Result0.

sum_list_([], Result) => Result eq 0.
sum_list_([X | Xs], Result) =>
  sum_list_(Xs, Result0),
  Result eq X + Result0.

min_list_([], Result) => Result eq inf.
min_list_([X | Xs], Result) =>
  min_list_(Xs, Result0),
  Result eq min(X, Result0).

max_list_([], Result) => Result eq -inf.
max_list_([X | Xs], Result) =>
  max_list_(Xs, Result0),
  Result eq max(X, Result0).

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
%
% Is there a better way to do this kind of unification?
% Perhaps SWI Prolog provides some meta-level hooks that we can use to tweak
% the standard unification procedure to be modulo the theory of reals.
X eq Y :-
  setup_call_cleanup(
    notrace,
    catch(({X == Y, Y == X}, solve([X, Y])), _, fail),
    trace
  ).

% Optimisation for when X and Y are both lists. In that case, just use maplist
% to unify all their arguments, instead of using the next clause to split apart
% the terms into functors and args.
X eq Y :-
  setup_call_cleanup(notrace, maplist(eq, X, Y), trace), !.

X eq Y :- setup_call_cleanup(
  notrace, (
    X =.. [Functor | X_args],
    Y =.. [Functor | Y_args],
    maplist(eq, X_args, Y_args)
  ),
  trace
).

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
  setup_call_cleanup(notrace, {Comparison}, trace)
).

% As with 'IS', we use constraint solving via clpBNR for handling arithmetic
% comparisons.
% If this is slow, try reverting to plain old arithmetic comparisons.
X lt Y :- #arithmetic_comparison(X < Y).
X leq Y :- #arithmetic_comparison(X =< Y).

% '<='(X, Y) :- X =< Y.

X gt Y :- #arithmetic_comparison(X > Y).
X geq Y :- #arithmetic_comparison(X >= Y).
