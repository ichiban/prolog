/*
 *  bootstrap script
 */

% operators
:-(op(1200, xfx, [:-, -->])).
:-(op(1200, fx, [:-, ?-])).
:-(op(1105, xfy, '|')).
:-(op(1100, xfy, ;)).
:-(op(1050, xfy, ->)).
:-(op(1000, xfy, ',')).
:-(op(900, fy, \+)).
:-(op(700, xfx, [=, \=])).
:-(op(700, xfx, [==, \==, @<, @=<, @>, @>=])).
:-(op(700, xfx, =..)).
:-(op(700, xfx, [is, =:=, =\=, <, =<, >, >=])).
:-(op(600, xfy, :)).
:-(op(500, yfx, [+, -, /\, \/])).
:-(op(400, yfx, [*, /, //, div, rem, mod, <<, >>])).
:-(op(200, xfx, **)).
:-(op(200, xfy, ^)).
:-(op(200, fy, [+, -, \])).

% true/fail

true.

fail :- \+true.

% conjunction/disjunction and if-then-else

P, Q :- call((P, Q)).

If -> Then; _ :- If, !, Then.
_ -> _; Else :- !, Else.
P; Q :- call((P; Q)).

If -> Then :- If, !, Then.

% cut

! :- !.

% logic and control

once(P) :- P, !.

% not unifiable

X \= Y :- \+(X = Y).

% term comparison

X == Y :- compare(=, X, Y).
X \== Y :- \+(X == Y).
X @< Y :- compare(<, X, Y).
X @=< Y :- compare(=, X, Y).
X @=< Y :- compare(<, X, Y).
X @> Y :- compare(>, X, Y).
X @>= Y :- compare(>, X, Y).
X @>= Y :- compare(=, X, Y).

nonvar(X) :- \+var(X).

number(X) :- float(X).
number(X) :- integer(X).

callable(X) :- atom(X).
callable(X) :- compound(X).

ground(X) :- term_variables(X, []).

atomic(X) :- nonvar(X), \+compound(X).

open(Filename, Mode, Stream) :- open(Filename, Mode, Stream, []).

close(Stream) :- close(Stream, []).

flush_output :- current_output(S), flush_output(S).

write_term(Term, Options) :- current_output(S), write_term(S, Term, Options).

write(Stream, Term) :- write_term(Stream, Term, [numbervars(true)]).

write(Term) :- current_output(S), write(S, Term).

write_canonical(Stream, Term) :- write_term(Stream, Term, [quoted(true), ignore_ops(true)]).

write_canonical(Term) :- current_output(S), write_canonical(S, Term).

writeq(Stream, Term) :- write_term(Stream, Term, [quoted(true), numbervars(true)]).

writeq(Term) :- current_output(S), writeq(S, Term).

nl(Stream) :- write_term(Stream, '\n', []).

nl :- current_output(S), nl(S).

put_byte(Byte) :- current_output(S), put_byte(S, Byte).

put_code(Code) :- current_output(S), put_code(S, Code).

put_char(Stream, Char) :- char_code(Char, Code), put_code(Stream, Code).

put_char(Char) :- current_output(S), put_char(S, Char).

read_term(Term, Options) :- current_input(S), read_term(S, Term, Options).

read(Stream, Term) :- read_term(Stream, Term, []).

read(Term) :- current_input(S), read(S, Term).

get_byte(Byte) :- current_input(S), get_byte(S, Byte).

get_char(Char) :- current_input(S), get_char(S, Char).

get_code(Stream, Code) :-
  get_char(Stream, Char),
  (Char = end_of_file -> Code = -1; char_code(Char, Code)).

get_code(Code) :- current_input(S), get_code(S, Code).

peek_byte(Byte) :- current_input(S), peek_byte(S, Byte).

peek_char(Char) :- current_input(S), peek_char(S, Char).

peek_code(Stream, Code) :-
  peek_char(Stream, Char),
  (Char = end_of_file -> Code = -1; char_code(Char, Code)).

peek_code(Code) :- current_input(S), peek_code(S, Code).

halt :- halt(0).

at_end_of_stream(Stream) :-
  stream_property(Stream, end_of_stream(E)), !,
  (E = at; E = past).

at_end_of_stream :- current_input(S), at_end_of_stream(S).

retractall(Head) :-
   retract((Head :- _)),
   fail.
retractall(_).

%%%% non-ISO predicates

false :- fail.

member(X, [X|_]).
member(X, [_|Xs]) :- member(X, Xs).

[H|T] :- consult([H|T]).

phrase(GRBody, S0) :- phrase(GRBody, S0, []).

select(E, [E|Xs], Xs).
select(E, [X|Xs], [X|Ys]) :-
   select(E, Xs, Ys).

maplist(_Cont_1, []).
maplist(Cont_1, [E1|E1s]) :-
   call(Cont_1, E1),
   maplist(Cont_1, E1s).

maplist(_Cont_2, [], []).
maplist(Cont_2, [E1|E1s], [E2|E2s]) :-
   call(Cont_2, E1, E2),
   maplist(Cont_2, E1s, E2s).

maplist(_Cont_3, [], [], []).
maplist(Cont_3, [E1|E1s], [E2|E2s], [E3|E3s]) :-
   call(Cont_3, E1, E2, E3),
   maplist(Cont_3, E1s, E2s, E3s).

maplist(_Cont_4, [], [], [], []).
maplist(Cont_4, [E1|E1s], [E2|E2s], [E3|E3s], [E4|E4s]) :-
   call(Cont_4, E1, E2, E3, E4),
   maplist(Cont_4, E1s, E2s, E3s, E4s).

maplist(_Cont_5, [], [], [], [], []).
maplist(Cont_5, [E1|E1s], [E2|E2s], [E3|E3s], [E4|E4s], [E5|E5s]) :-
   call(Cont_5, E1, E2, E3, E4, E5),
   maplist(Cont_5, E1s, E2s, E3s, E4s, E5s).

maplist(_Cont_6, [], [], [], [], [], []).
maplist(Cont_6, [E1|E1s], [E2|E2s], [E3|E3s], [E4|E4s], [E5|E5s], [E6|E6s]) :-
   call(Cont_6, E1, E2, E3, E4, E5, E6),
   maplist(Cont_6, E1s, E2s, E3s, E4s, E5s, E6s).

maplist(_Cont_7, [], [], [], [], [], [], []).
maplist(Cont_7, [E1|E1s], [E2|E2s], [E3|E3s], [E4|E4s], [E5|E5s], [E6|E6s], [E7|E7s]) :-
   call(Cont_7, E1, E2, E3, E4, E5, E6, E7),
   maplist(Cont_7, E1s, E2s, E3s, E4s, E5s, E6s, E7s).
