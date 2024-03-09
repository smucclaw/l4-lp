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

% The more stack frames that we trace, and the more data that we capture in
% each stack frame, the slower the interpreter will run, and the more work
% will need to be done afterwards to post-process and filter through the stack
% stack trace.
% What info do we really need to capture?
% - Maybe we can just trace Exit and Fail ports. Not sure about Redo.
%   See https://www.swi-prolog.org/pldoc/man?section=debugoverview
% - Maybe we don't need the recursion level.

:- leash(-all).
:- visible(-all), visible([+exit, +fail]).

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

% #define(
%   toggle_tracing(Toggle0, Goal, Toggle1),
%   (Toggle0, setup_call_cleanup(true, call(Goal), Toggle1))
% ).

once_trace_all(Goal) =>
  setup_call_cleanup(trace, once(Goal), notrace).

% https://www.swi-prolog.org/pldoc/man?predicate=op/3
:- op(700, xfx, eq).
:- op(700, xfx, eq_).
:- op(700, xfx, neq).
:- op(700, xfx, neq_).
:- op(700, xfx, lt).
:- op(700, xfx, leq).
:- op(700, xfx, gt).
:- op(700, xfx, geq).

is_in(X, [Y | _]) :- X eq Y.
is_in(X, [_ | Xs]) :- is_in(X, Xs).

product_list([], Result) => Result eq 0.
product_list([X], Result) => Result eq X.
product_list([X | Xs], Result) =>
  product_list(Xs, Result0),
  Result eq X * Result0.

sum_list_([], Result) => Result eq 0.
sum_list_([X | Xs], Result) =>
  sum_list_(Xs, Result0),
  Result eq X + Result0.

min_(X, Y, Result), X leq Y => Result eq X.
min_(_, Y, Result) => Result eq Y.

max_(X, Y, Result), X geq Y => Result eq X.
max_(_, Y, Result) => Result eq Y.

min_by(P, X, Y, Result) :-
  call(P, X, X0),
  call(P, Y, Y0),
  (
    X0 leq Y0 -> Result eq X ; Result eq Y
  ).

max_by(P, X, Y, Result) :-
  call(P, X, X0),
  call(P, Y, Y0),
  (
    X0 geq Y0 -> Result eq X ; Result eq Y
  ).

min_list_([], Result) => Result eq inf.
min_list_([X | Xs], Result) =>
  min_list_(Xs, Result0),
  min_(X, Result0, Result).
  % Result eq min(X, Result0).

max_list_([], Result) => Result eq -inf.
max_list_([X | Xs], Result) =>
  max_list_(Xs, Result0),
  max_(X, Result0, Result).
  % Result eq max(X, Result0).

% TODO:
% Implement min_by and max_by in a way that works for predicates,
% functions/methods, and attribute projects on dicts.

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
X eq Y :- notrace(X eq_ Y).

X eq_ Y :-
  catch(({X == Y, Y == X}, solve([X, Y])), _, fail), !.

% Optimisation for when X and Y are both lists. In that case, just use maplist
% to unify all their arguments, instead of using the next clause to split apart
% the terms into functors and args.
X eq_ Y :- maplist(eq_, X, Y), !.

X eq_ Y :-
  catch(
    (
      X =.. [Functor | X_args],
      Y =.. [Functor | Y_args],
      maplist(eq_, X_args, Y_args)
    ),
  _, fail).

% X 'IS' Y :-
  % catch(X is Y, _, fail), !

% X 'IS' Y :-
%   catch(Y is X, _, fail), !.

% X 'IS' Y :-
%   catch((Z is X, Z is Y), _, fail), !.

% X 'IS' Y :-
  % notrace, ((unify_with_occurs_check(X, Y), !, trace); trace).

X neq Y :- notrace(X neq_ Y).

X neq_ Y :-
  catch({~(X == Y)}, _, fail), !.

X neq_ Y :-
  maplist(neq_, X, Y), !.

% This is not really ideal because we still have:
% ?- X neq_ b(0), X = b(0 + 0).
% X = b(0+0).
%
% Maybe need to use CHR or something else to control unification.
X neq_ Y :-
  catch(
    (
      X =.. [X_functor | X_args],
      Y =.. [Y_functor | Y_args],
      (dif(X_functor, Y_functor) -> true ; maplist(neq_, X_args, Y_args))
    ),
  _, fail), !.

X neq_ Y :- dif(X, Y).

not_((X , Y)) => not_(X) ; not_(Y).
not_((X ; Y)) => not_(X) , not_(Y).

not_(X lt Y) => X geq Y.
not_(X leq Y) => X gt Y.

not_(X gt Y) => X leq Y.
not_(X geq Y) => X lt Y.

not_(X eq Y) => X neq Y.

% not_(X eq Y) => dif(X, Y).

% Ideally we want to macro-expand P to a matching body and push the negation
% into that.
not_(P) => \+ P.

% test(X) :- 0 leq X, X leq 10.

#define(
  arithmetic_comparison(Comparison),
  notrace(catch({Comparison}, _, fail))
).

% As with 'IS', we use constraint solving via clpBNR for handling arithmetic
% comparisons.
% If this is slow, try reverting to plain old arithmetic comparisons.
X lt Y :- #arithmetic_comparison(X < Y), !.
X lt Y :- date_is_before_date(X, Y).

% [years(Y0), months(M0), days(D0)] lt [years(Y1), months(M1), days(D1)] :-
%   maplist(lt, [Y0, M0, D0], [Y1, M1, D1]).

X leq Y :- #arithmetic_comparison(X =< Y), !.
X leq Y :- date_is_before_or_eq_date(X, Y).

% '<='(X, Y) :- X =< Y.

X gt Y :- #arithmetic_comparison(X > Y), !.
X gt Y :- date_is_after_date(X, Y).

X geq Y :- #arithmetic_comparison(X >= Y), !.
X geq Y :- date_is_after_or_eq_date(X, Y).

is_valid_date(date(Year, Month, Day)) :- notrace(
  (
    [Year, Month, Day]::integer, {
      Year > 0,
      1 =< Month, Month =< 12,
      1 =< Day, Day =< 31,
      (
        ((Month == 4) or (Month == 6) or (Month == 9) or (Month == 11)) ->
          (Day =< 30)
      ),
      ((Month == 2) -> (Day =< 29)),
      (
        ((Month == 2) and (Day == 29)) ->
          (integer(Year / 4) and (integer(Year / 100) -> integer(Year / 400)))
      )
    }
  )
).

% is_valid_unit(days) :- !.
% is_valid_unit(weeks) :- !.
% is_valid_unit(months) :- !.
% is_valid_unit(years) :- !.

% is_valid_duration_with_unit_number(Duration, Unit, Number) :-
%   Number::integer, {Number >= 0},
%   is_valid_unit(Unit),
%   Duration =.. [Unit, Number].

% is_valid_duration(Duration) :-
%   is_valid_duration_with_unit_number(Duration, _, _).

% #define(
%   wrap_date_goal(Date0, Date1, Goal),
%   setup_call_cleanup(
%     notrace,
%     (is_valid_date(Date0), is_valid_date(Date1), Goal), 
%     trace
%   )
% ).

% #define(
%   wrap_date_duration_goal(Date0, Date1, Duration, Goal),
%   #wrap_date_goal(Date0, Date1, (is_valid_duration(Duration), Goal))
% ).

date_add_duration(Date0, Duration, Date1) =>
  % #wrap_date_duration_goal(
  %   Date0, Date1, Duration,
  notrace(date_add(Date0, Duration, Date1)).
  % ).

date_minus_duration(Date0, Duration, Date1) =>
  % #wrap_date_goal(
  %   Date0, Date1,
  %  (
      % is_valid_duration_with_unit_number(Duration, Unit, Number),
  notrace(
    (
      Duration =.. [Unit, Number],
      Negated_duration =.. [Unit, -Number],
      date_add(Date0, Negated_duration, Date1)
    )
  ).
  %  )
  % ).

% date_minus_date(Date0, Date1, Duration) =>
%   #wrap_date_duration_goal(
%     Date0, Date1, Duration,
%     date_difference(Date0, Date1, Duration)
%   ).

#define(
  wrap_date_compare(Date0, Op, Date1),
  notrace(date_compare(Date0, Op, Date1))
).

date_is_before_date(Date0, Date1) =>
  #wrap_date_compare(Date0, <, Date1).

date_is_before_or_eq_date(Date0, Date1) =>
  #wrap_date_compare(Date0, =<, Date1).

date_is_after_date(Date0, Date1) =>
  #wrap_date_compare(Date0, >, Date1).

date_is_after_or_eq_date(Date0, Date1) =>
  #wrap_date_compare(Date0, >=, Date1).

date_is_within_duration_of_date(Date0, Duration, Date1),
  date_is_before_or_eq_date(Date0, Date1) =>
    date_add_duration(Date0, Duration, Date),
    date_is_before_or_eq_date(Date1, Date).

date_is_within_duration_of_date(Date0, Duration, Date1) =>
  date_minus_duration(Date0, Duration, Date),
  date_is_after_or_eq_date(Date1, Date).