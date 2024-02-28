% https://swi-prolog.discourse.group/t/tool-for-drawing-sld-trees-in-prolog/4737

% https://www.swi-prolog.org/pldoc/man?section=debugoverview
% https://swi-prolog.discourse.group/t/swi-prolog-in-the-browser-using-wasm/5650
% https://www.swi-prolog.org/pldoc/man?section=wasm

:- use_module(library(dicts)).
% :- use_module(library(clpq)).

:- [library(date_time), library(clpBNR)].

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

eval_and_trace(Goal) :-
  trace,
  ((call(Goal), !, notrace, nodebug) ; (notrace, nodebug, fail)).

% Judgement forms for semantics:
% 1. Γ ⊢ expr ⇓ val
%   Under context Γ, expr big-step evaluates to val 
% 2. Γ ⊢ p
%   Under context Γ, p holds.

% https://www.swi-prolog.org/pldoc/man?predicate=op/3
:- op(800, xfx, 'IS').

product_list([], 0) :- !.
product_list([X], X) :- !.
product_list([X | Xs], Result) :- !,
  product_list(Xs, Result0),
  Result 'IS' X * Result0.

sum_list_([], 0) :- !.
sum_list_([X | Xs], Result) :- !,
  sum_list_(Xs, Result0),
  Result 'IS' X + Result0.

min_list_([], inf) :- !.
min_list_([X | Xs], Result) :- !,
  min_list_(Xs, Result0),
  Result 'IS' min(X, Result0).

max_list_([], -inf) :- !.
max_list_([X | Xs], Result) :- !,
  max_list_(Xs, Result0),
  Result 'IS' max(X, Result0).

X 'IS' Y :-
  % This first uses constraint solving via clpBNR for unifying arithmetic
  % expressions, modulo the theory of reals.
  % If that fails, we revert to syntatic unification, ie modulo the empty
  % theory.
  %
  % If this is too slow, we can replace the replace the constraint based
  % unification with arithmetic evaluation via "is", or the unify with occurs
  % check with plain old unification.
  notrace,
  catch({X == Y}, _, (unify_with_occurs_check(X, Y), trace)),
  trace.

  % catch(X is Y, _, fail), !

% 'IS'(X, Y) :-
%   catch(Y is X, _, fail), !.

% 'IS'(X, Y) :-
%   catch((Z is X, Z is Y), _, fail), !.

  % notrace, ((unify_with_occurs_check(X, Y), !, trace); trace).

% As with 'IS', we use constraint solving via clpBNR for handling arithmetic
% comparisons.
% If this is slow, try reverting to plain old arithmetic comparisons.
lt(X, Y) :- notrace, (({X < Y}, !, trace) ; (trace, fail)).
leq(X, Y) :- notrace, (({X =< Y}, !, trace) ; (trace, fail)).

% '<='(X, Y) :- X =< Y.

gt(X, Y) :- notrace, (({X > Y}, !, trace) ; (trace, fail)).
geq(X, Y) :- notrace, (({X >= Y}, !, trace) ; (trace, fail)).

  % trace,
  % (
  %     call(Goal),
  %     fail,
  %     !
  % ;
  %     notrace,
  %     nodebug
  % ).

% run0 :-
%   trace,
%   (
%       mother(_,_),
%       fail,
%       !
%   ;
%       notrace,
%       nodebug
%   ).

% female(pam).
% female(liz).
% female(pat).
% female(ann).

% male(tom).
% male(bob).
% male(jim).

% parent(pam,bob).
% parent(tom,bob).
% parent(tom,liz).
% parent(bob,ann).
% parent(bob,pat).
% parent(pat,jim).
% parent(bob,peter).
% parent(peter,jim).

% mother(X,Y) :-
%   parent(X,Y),
%   female(X).

% grandmother(X,Z) :-
%   mother(X,Y),
%   parent(Y,Z).
