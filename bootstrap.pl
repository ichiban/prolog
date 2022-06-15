/*
 *  bootstrap script
 */

% operators
:-(op(1200, xfx, :-)).
:-(op(1200, xfx, -->)).
:-(op(1200, fx, :-)).
:-(op(1200, fx, ?-)).
:-(op(1105, xfy, '|')).
:-(op(1100, xfy, ;)).
:-(op(1050, xfy, ->)).
:-(op(1000, xfy, ',')).
:-(op(900, fy, \+)).
:-(op(700, xfx, =)).
:-(op(700, xfx, \=)).
:-(op(700, xfx, ==)).
:-(op(700, xfx, \==)).
:-(op(700, xfx, @<)).
:-(op(700, xfx, @=<)).
:-(op(700, xfx, @>)).
:-(op(700, xfx, @>=)).
:-(op(700, xfx, is)).
:-(op(700, xfx, =:=)).
:-(op(700, xfx, =\=)).
:-(op(700, xfx, <)).
:-(op(700, xfx, =<)).
:-(op(700, xfx, =\=)).
:-(op(700, xfx, >)).
:-(op(700, xfx, >=)).
:-(op(700, xfx, =..)).
:-(op(500, yfx, +)).
:-(op(500, yfx, -)).
:-(op(500, yfx, /\)).
:-(op(500, yfx, \/)).
:-(op(400, yfx, *)).
:-(op(400, yfx, /)).
:-(op(400, yfx, //)).
:-(op(400, yfx, div)).
:-(op(400, yfx, rem)).
:-(op(400, yfx, mod)).
:-(op(400, yfx, <<)).
:-(op(400, yfx, >>)).
:-(op(200, xfx, **)).
:-(op(200, xfy, ^)).
:-(op(200, fy, \)).
:-(op(200, fy, +)).
:-(op(200, fy, -)).
:-(op(100, xfx, @)).
:-(op(50, xfx, :)).

% true/fail

:- built_in(true/0).
true.

:- built_in(fail/0).
fail :- \+true.

% conjunction/disjunction and if-then-else

:- built_in(','/2).
P, Q :- call((P, Q)).

:- built_in(';'/2).
If -> Then; _ :- If, !, Then.
_ -> _; Else :- !, Else.
P; Q :- call((P; Q)).

:- built_in('->'/2).
If -> Then :- If, !, Then.

% cut

:- built_in(!/0).
! :- !.

% logic and control

:- built_in(once/1).
once(P) :- P, !.

% not unifiable

:- built_in((\=)/2).
X \= Y :- \+(X = Y).

% term comparison

:- built_in((==)/2).
X == Y :- compare(=, X, Y).
:- built_in((\==)/2).
X \== Y :- \+(X == Y).
:- built_in((@<)/2).
X @< Y :- compare(<, X, Y).
:- built_in((@=<)/2).
X @=< Y :- compare(=, X, Y).
X @=< Y :- compare(<, X, Y).
:- built_in((@>)/2).
X @> Y :- compare(>, X, Y).
:- built_in((@>=)/2).
X @>= Y :- compare(>, X, Y).
X @>= Y :- compare(=, X, Y).

:- built_in(nonvar/1).
nonvar(X) :- \+var(X).

:- built_in(number/1).
number(X) :- float(X).
number(X) :- integer(X).

:- built_in(callable/1).
callable(X) :- atom(X).
callable(X) :- compound(X).

:- built_in(ground/1).
ground(X) :- term_variables(X, []).

:- built_in(atomic/1).
atomic(X) :- nonvar(X), \+compound(X).

:- built_in(open/3).
open(Filename, Mode, Stream) :- open(Filename, Mode, Stream, []).

:- built_in(close/1).
close(Stream) :- close(Stream, []).

:- built_in(flush_output/0).
flush_output :- current_output(S), flush_output(S).

:- built_in(write_term/2).
write_term(Term, Options) :- current_output(S), write_term(S, Term, Options).

:- built_in(write/2).
write(Stream, Term) :- write_term(Stream, Term, [numbervars(true)]).

:- built_in(write/1).
write(Term) :- current_output(S), write(S, Term).

:- built_in(write_canonical/2).
write_canonical(Stream, Term) :- write_term(Stream, Term, [quoted(true), ignore_ops(true)]).

:- built_in(write_canonical/1).
write_canonical(Term) :- current_output(S), write_canonical(S, Term).

:- built_in(writeq/2).
writeq(Stream, Term) :- write_term(Stream, Term, [quoted(true), numbervars(true)]).

:- built_in(writeq/1).
writeq(Term) :- current_output(S), writeq(S, Term).

:- built_in(nl/1).
nl(Stream) :- write_term(Stream, '\n', []).

:- built_in(nl/0).
nl :- current_output(S), nl(S).

:- built_in(put_byte/1).
put_byte(Byte) :- current_output(S), put_byte(S, Byte).

:- built_in(put_code/1).
put_code(Code) :- current_output(S), put_code(S, Code).

:- built_in(put_char/2).
put_char(Stream, Char) :- char_code(Char, Code), put_code(Stream, Code).

:- built_in(put_char/1).
put_char(Char) :- current_output(S), put_char(S, Char).

:- built_in(read_term/2).
read_term(Term, Options) :- current_input(S), read_term(S, Term, Options).

:- built_in(read/2).
read(Stream, Term) :- read_term(Stream, Term, []).

:- built_in(read/1).
read(Term) :- current_input(S), read(S, Term).

:- built_in(get_byte/1).
get_byte(Byte) :- current_input(S), get_byte(S, Byte).

:- built_in(get_char/1).
get_char(Char) :- current_input(S), get_char(S, Char).

:- built_in(get_code/2).
get_code(Stream, Code) :-
  get_char(Stream, Char),
  (Char = end_of_file -> Code = -1; char_code(Char, Code)).

:- built_in(get_code/1).
get_code(Code) :- current_input(S), get_code(S, Code).

:- built_in(peek_byte/1).
peek_byte(Byte) :- current_input(S), peek_byte(S, Byte).

:- built_in(peek_char/1).
peek_char(Char) :- current_input(S), peek_char(S, Char).

:- built_in(peek_code/2).
peek_code(Stream, Code) :-
  peek_char(Stream, Char),
  (Char = end_of_file -> Code = -1; char_code(Char, Code)).

:- built_in(peek_code/1).
peek_code(Code) :- current_input(S), peek_code(S, Code).

:- built_in(halt/0).
halt :- halt(0).

:- built_in(at_end_of_stream/1).
at_end_of_stream(Stream) :-
  stream_property(Stream, end_of_stream(E)), !,
  (E = at; E = past).

:- built_in(at_end_of_stream/0).
at_end_of_stream :- current_input(S), at_end_of_stream(S).

:- built_in(retractall/1).
retractall(Head) :-
   retract((Head :- _)),
   fail.
retractall(_).

%%%% non-ISO predicates

:- built_in(false/0).
false :- fail.

:- built_in(append/3).
append([], L, L).
append([X|L1], L2, [X|L3]) :- append(L1, L2, L3).

:- built_in(member/2).
member(X, [X|_]).
member(X, [_|Xs]) :- member(X, Xs).

:- built_in(length/2).
length([], 0).
length([_|Xs], N) :- length(Xs, L), N is L + 1.

:- built_in((.)/2).
[H|T] :- consult([H|T]).

:- built_in(phrase/2).
phrase(GRBody, S0) :- phrase(GRBody, S0, []).

:- built_in(select/3).
select(E, [E|Xs], Xs).
select(E, [X|Xs], [X|Ys]) :-
   select(E, Xs, Ys).

:- built_in(maplist/2).
maplist(_Cont_1, []).
maplist(Cont_1, [E1|E1s]) :-
   call(Cont_1, E1),
   maplist(Cont_1, E1s).

:- built_in(maplist/3).
maplist(_Cont_2, [], []).
maplist(Cont_2, [E1|E1s], [E2|E2s]) :-
   call(Cont_2, E1, E2),
   maplist(Cont_2, E1s, E2s).

:- built_in(maplist/4).
maplist(_Cont_3, [], [], []).
maplist(Cont_3, [E1|E1s], [E2|E2s], [E3|E3s]) :-
   call(Cont_3, E1, E2, E3),
   maplist(Cont_3, E1s, E2s, E3s).

:- built_in(maplist/5).
maplist(_Cont_4, [], [], [], []).
maplist(Cont_4, [E1|E1s], [E2|E2s], [E3|E3s], [E4|E4s]) :-
   call(Cont_4, E1, E2, E3, E4),
   maplist(Cont_4, E1s, E2s, E3s, E4s).

:- built_in(maplist/6).
maplist(_Cont_5, [], [], [], [], []).
maplist(Cont_5, [E1|E1s], [E2|E2s], [E3|E3s], [E4|E4s], [E5|E5s]) :-
   call(Cont_5, E1, E2, E3, E4, E5),
   maplist(Cont_5, E1s, E2s, E3s, E4s, E5s).

:- built_in(maplist/7).
maplist(_Cont_6, [], [], [], [], [], []).
maplist(Cont_6, [E1|E1s], [E2|E2s], [E3|E3s], [E4|E4s], [E5|E5s], [E6|E6s]) :-
   call(Cont_6, E1, E2, E3, E4, E5, E6),
   maplist(Cont_6, E1s, E2s, E3s, E4s, E5s, E6s).

:- built_in(maplist/8).
maplist(_Cont_7, [], [], [], [], [], [], []).
maplist(Cont_7, [E1|E1s], [E2|E2s], [E3|E3s], [E4|E4s], [E5|E5s], [E6|E6s], [E7|E7s]) :-
   call(Cont_7, E1, E2, E3, E4, E5, E6, E7),
   maplist(Cont_7, E1s, E2s, E3s, E4s, E5s, E6s, E7s).
