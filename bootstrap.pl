/*
 *  bootstrap script
 */

% operators
:-(op(1200, xfx, :-)).
:-(op(1200, xfx, -->)).
:-(op(1200, fx, :-)).
:-(op(1200, fx, ?-)).
:-(op(1100, xfy, ;)).
:-(op(1050, xfy, ->)).
:-(op(1000, xfy, ,)).
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
true.
fail :- \+true.

% if then
If -> Then :- If -> Then; fail.

% logic and control
once(P) :- call(P), !.

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

atomic(X) :- nonvar(X), \+compound(X).

findall(Term, Goal, List) :- bagof(Term, _^Goal, List), !.
findall(Term, Goal, []).

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

%%%% non-ISO predicates

false :- fail.

append([], L, L).
append([X|L1], L2, [X|L3]) :- append(L1, L2, L3).