% https://swi-prolog.discourse.group/t/tool-for-drawing-sld-trees-in-prolog/4737

% https://www.swi-prolog.org/pldoc/man?section=debugoverview
% https://swi-prolog.discourse.group/t/swi-prolog-in-the-browser-using-wasm/5650
% https://www.swi-prolog.org/pldoc/man?section=wasm

:- [library(date_time), library(dicts)].

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
  ((call(Goal), !, notrace, nodebug) ; notrace, nodebug).

product_list([], 0) :- !.
product_list([X], X) :- !.
product_list([X | Xs], A) :- !,
  product_list(Xs, B),
  A is X * B.

sum_list_([], 0) :- !.
sum_list_([X | Xs], A) :- !,
  sum_list_(Xs, B),
  A is X + B.

min_list_([], inf) :- !.
min_list_([X | Xs], A) :- !,
  min_list_(Xs, B),
  A is min(X, B).

max_list_([], -inf) :- !.
max_list_([X | Xs], A) :- !,
  max_list_(Xs, B),
  A is max(X, B).

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
