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

open(Source_sink, Mode, Stream) :-
  open(Source_sink, Mode, Stream, []).

close(S_or_a) :-
  close(S_or_a, []).

flush_output :-
  current_output(S),
  flush_output(S).

at_end_of_stream :-
  current_input(S),
  stream_property(S, end_of_stream(E)),
  !,
  (E = at ; E = past).

at_end_of_stream(S_or_a) :-
  ( atom(S_or_a) ->
    stream_property(S, alias(S_or_a))
  ; S = S_or_a
  ),
  stream_property(S, end_of_stream(E)),
  !,
  (E = at ; E = past).

get_char(Char) :-
  current_input(S),
  get_char(S, Char).

get_code(Code) :-
  current_input(S),
  get_code(S, Code).

peek_char(Char) :-
  current_input(S),
  peek_char(S, Char).

peek_code(Code) :-
  current_input(S),
  peek_code(S, Code).

put_char(Char) :-
  current_output(S),
  put_char(S, Char).

put_code(Code) :-
  current_output(S),
  put_code(S, Code).

nl :-
  current_output(S),
  nl(S).

nl(S) :-
  put_char(S, '\n').

get_byte(Byte) :-
  current_input(S),
  get_byte(S, Byte).

peek_byte(Byte) :-
  current_input(S),
  peek_byte(S, Byte).

put_byte(Byte) :-
  current_output(S),
  put_byte(S, Byte).

read_term(Term, Options) :-
  current_input(S),
  read_term(S, Term, Options).

read(Term) :-
  current_input(S),
  read_term(S, Term, []).

read(S, Term) :-
  read_term(S, Term, []).

write_term(Term, Options) :-
  current_output(S),
  write_term(S, Term, Options).

write(Term) :-
  current_output(S),
  write_term(S, Term, [numbervars(true)]).

write(S, Term) :-
  write_term(S, Term, [numbervars(true)]).

writeq(Term) :-
  current_output(S),
  write_term(S, Term, [quoted(true), numbervars(true)]).

writeq(S, Term) :-
  write_term(S, Term, [quoted(true), numbervars(true)]).

write_canonical(Term) :-
  current_output(S),
  write_term(S, Term, [quoted(true), ignore_ops(true)]).

write_canonical(S, Term) :-
  write_term(S, Term, [quoted(true), ignore_ops(true)]).

% stub
number_chars(_, _) :- throw(error(syntax_error(_), _)).

\+(G) :- G, !, fail.
\+(_).

once(G) :- call(G), !.
