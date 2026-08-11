% Ported to Go from BinProlog (github.com/ptarau/binprolog, src/lib.pl, src/extra.pl and related sources), Copyright (C) Paul Tarau, licensed under Apache-2.0.
% This file has been modified: non-ISO predicates are renamed and missing ISO predicates are added.

dynamic((PI1, PI2)) :- !,
  dynamic(PI1),
  dynamic(PI2).
dynamic([PI|PIs]) :- !,
  dynamic(PI),
  dynamic(PIs).
dynamic([]) :- !.
dynamic(PI) :- '$dynamic'(PI).

'$cut_to'('$cut').

! :- !.

A, B :- A, B.

If -> Then :- If, !, Then.

If -> Then; Else :- !, '$if'(If, Then, Else).
A; B :- A; B.

'$or'(A, _) :- A.
'$or'(_, B) :- B.

'$if'(If, Then, _) :- If, !, Then.
'$if'(_, _, Else) :- Else.

catch(Goal, Ball, Do) :-
  '$get_neck_cut'(Choice),
  '$cont'(Cont),
  Goal,
  '$to_catch'(Ball, Do, Choice, Cont).
catch(_, _, _) :- fail.                 % '$get_neck_cut'(Choice) above requires this.

% $to_catch/4 is a catch marker and does nothing per se.
% catch/3 sneaks it into the continuation chain and throw/1 finds it for recovery.
'$to_catch'(_, _, _, _).

X = X.

unify_with_occurs_check(X, Y) :-
  X = Y,
  acyclic_term(X).

X \= Y :- \+(X = Y).

atomic(X) :-
  nonvar(X),
  \+compound(X).

nonvar(X) :- \+var(X).

number(X) :- integer(X); float(X).

callable(X) :- atom(X); compound(X).

X @=< Y :- compare(=, X, Y).
X @=< Y :- compare(<, X, Y).

X == Y :- compare(=, X, Y).

X \== Y :- \+(X == Y).

X @< Y :- compare(<, X, Y).

X @> Y :- compare(>, X, Y).

X @>= Y :- compare(>, X, Y).
X @>= Y :- compare(=, X, Y).

R is E :- R is E.

'$expr'(E, _) :- var(E), !,
  throw(error(instantiation_error, '$expr'/2)).
'$expr'(E, R) :- atomic(E), !,
  E = R.
'$expr'(E, R) :- E =.. [Op, E1, E2], !,
  '$atom_concat'($, Op, NewOp),
  '$expr'(E1, X1),
  '$expr'(E2, X2),
  G =.. [NewOp, X1, X2, R],
  G.
'$expr'(E, R) :- E =.. [Op, E1],
  '$atom_concat'($, Op, NewOp),
  '$expr'(E1, X1),
  G =.. [NewOp, X1, R],
  G.

X =:= Y :- X =:= Y.
X =\= Y :- X =\= Y.
X < Y :- X < Y.
X =< Y :- X =< Y.
X > Y :- X > Y.
X >= Y :- X >= Y.

sort([], []).
sort(List, Sorted) :-
  setof(X, '$member'(X, List), Sorted).

'$member'(X, [X|_]).
'$member'(X, [_|Xs]) :- '$member'(X, Xs).

retractall(Head) :-
  retract((Head :- _)),
  fail.
retractall(_).

% stub
write(Term).

% stub
number_chars(_, _) :- throw(error(syntax_error(_), _)).

\+(G) :- G, !, fail.
\+(_).

once(G) :- call(G), !.
