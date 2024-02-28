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

% Judgement forms for semantics:
% 1. Γ ⊢ expr ⇓ val
%   Under context Γ, expr big-step evaluates to val 
% 2. Γ ⊢ p
%   Under context Γ, p holds.

% ------------------------
% Γ ⊢ product_list [] ⇓ 0
product_list([], 0) :- !.

% -------------------------
% Γ ⊢ product_list [X] ⇓ X 
product_list([X], X) :- !.

% Γ ⊢ product_list [Y | T] ⇓ Result0
% Γ ⊢ X * Result0 ⇓ Result
% -------------------------------------
% Γ ⊢ product_list [X, Y | T] ⇓ Result 
product_list([X | Xs], Result) :- !,
  product_list(Xs, Result0),
  Result is X * Result0.

% Γ ⊢ sum_list_ [] ⇓ 0
sum_list_([], 0) :- !.

% Γ ⊢ sum_list_ T ⇓ Result0
% Γ ⊢ X + Result0 ⇓ Result
% ------------------------------
% Γ ⊢ sum_list_ [X | T] ⇓ Result 
sum_list_([X | Xs], Result) :- !,
  sum_list_(Xs, Result0),
  Result is X + Result0.

% ----------------------
% Γ ⊢ min_list_ [] ⇓ ∞
min_list_([], inf) :- !.

% Γ ⊢ min_list_ T ⇓ Result0
% Γ ⊢ min(X, Result0) ⇓ Result
% ------------------------------
% Γ ⊢ min_list_ [X | T] ⇓ Result 
min_list_([X | Xs], Result) :- !,
  min_list_(Xs, Result0),
  Result is min(X, Result0).

% ----------------------
% Γ ⊢ max_list_ [] ⇓ -∞
max_list_([], -inf) :- !.

% Γ ⊢ max_list_ T ⇓ Result0
% Γ ⊢ max(X, Result0) ⇓ Result
% ------------------------------
% Γ ⊢ max_list_ [X | T] ⇓ Result 
max_list_([X | Xs], Result) :- !,
  max_list_(Xs, Result0),
  Result is max(X, Result0).

% ---------------
%   Γ ⊢ X IS X
'IS'(X, X) :- !.

%  (ℝ, Γ) ⊨ X = Y    (Abuse of notation because we're technically working with)
% ----------------     order-sorted algebras)
%  Γ ⊢ X IS Y
'IS'(X, Y) :-
  % is/2 throws a type error if the 2nd argument is not numeric.
  % This catches the type error and forces the predicate to fail if that happens.
  catch((Z is X, Z is Y), _, fail).

% Γ ⊢ X =< Y     (ie. %  (ℝ, Γ) ⊨ X <= Y)
% -------------------
% Γ ⊢ X <= Y
'<='(X, Y) :- X =< Y.

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
