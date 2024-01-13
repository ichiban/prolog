:-(op(1200, fx, :-)).
:- op(400, yfx, /).
:- module(prolog, [
  op(1200, xfx, [:-, -->]),
  op(1200, fx, [:-, ?-]),
  op(1105, xfy, '|'),
  op(1100, xfy, ;),
  op(1050, xfy, ->),
  op(1000, xfy, ','),
  op(900, fy, \+),
  op(700, xfx, [=, \=]),
  op(700, xfx, [==, \==, @<, @=<, @>, @>=]),
  op(700, xfx, =..),
  op(700, xfx, [is, =:=, =\=, <, =<, >, >=]),
  op(600, xfy, :),
  op(500, yfx, [+, -, /\, \/]),
  op(400, yfx, [*, /, //, div, rem, mod, <<, >>]),
  op(200, xfx, **),
  op(200, xfy, ^),
  op(200, fy, [+, -, \]),

  % Part 1: General core

  % 7.4.2 Directives
  dynamic/1,
  multifile/1,
  discontiguous/1,
  % op/3,
  % char_conversion/2,
  initialization/1,
  include/1,
  ensure_loaded/1,
  % set_prolog_flag/2,

  % 7.8 Control constructs
  true/0,
  fail/0,
  call/1,
  !/0,
  (',')/2,
  (;)/2,
  (->)/2,
  catch/3,
  throw/1,

  % 8.2 Term unification
  (=)/2,
  unify_with_occurs_check/2,
  (\=)/2,
  subsumes_term/2,

  % 8.3 Type testing
  var/1,
  atom/1,
  integer/1,
  float/1,
  atomic/1,
  compound/1,
  nonvar/1,
  number/1,
  callable/1,
  ground/1,
  acyclic_term/1,

  % 8.4 Term comparison
  (@=<)/2,
  (==)/2,
  (\==)/2,
  (@<)/2,
  (@>)/2,
  (@>=)/2,
  compare/3,
  sort/2,
  keysort/2,

  % 8.5 Term creation and decomposition
  functor/3,
  arg/3,
  (=..)/2,
  copy_term/2,
  term_variables/2,

  % 8.6 Arithmetic evaluation
  (is)/2,

  % 8.7 Arithmetic comparison
  (=:=)/2,
  (=\=)/2,
  (<)/2,
  (=<)/2,
  (>)/2,
  (>=)/2,

  % 8.8 Clause retrieval and information
  clause/2,
  current_predicate/1,

  % 8.9 Clause creation and destruction
  asserta/1,
  assertz/1,
  retract/1,
  abolish/1,
  retractall/1,

  % 8.10 All solutions
  findall/3,
  bagof/3,
  setof/3,

  % 8.11 Stream selection and control
  current_input/1,
  current_output/1,
  set_input/1,
  set_output/1,
  open/4,
  open/3,
  close/2,
  close/1,
  flush_output/1,
  flush_output/0,
  stream_property/2,
  at_end_of_stream/0,
  at_end_of_stream/1,
  set_stream_position/2,

  % 8.12 Character input/output
  get_char/2,
  get_char/1,
  get_code/1,
  get_code/2,
  peek_char/2,
  peek_char/1,
  peek_code/1,
  peek_code/2,
  put_char/2,
  put_char/1,
  put_code/1,
  put_code/2,
  nl/0,
  nl/1,

  % 8.13 Byte input/output
  get_byte/2,
  get_byte/1,
  peek_byte/2,
  peek_byte/1,
  put_byte/2,
  put_byte/1,

  % 8.14 Term input/output
  read_term/3,
  read_term/2,
  read/1,
  read/2,
  write_term/3,
  write_term/2,
  write/1,
  write/2,
  writeq/1,
  writeq/2,
  write_canonical/1,
  write_canonical/2,
  op/3,
  current_op/3,
  char_conversion/2,
  current_char_conversion/2,

  % 8.15 Logic and control
  (\+)/1,
  once/1,
  repeat/0,
  call/2,
  call/3,
  call/4,
  call/5,
  call/6,
  call/7,
  call/8,
  false/0,

  % 8.16 Atomic term processing
  atom_length/2,
  atom_concat/3,
  sub_atom/5,
  atom_chars/2,
  atom_codes/2,
  char_code/2,
  number_chars/2,
  number_codes/2,

  % 8.17 Implementation defined hooks
  set_prolog_flag/2,
  current_prolog_flag/2,
  halt/0,
  halt/1,

  % Part 2: Modules

  % 7.2 Module predicates
  current_module/1,
  predicate_property/2,

  % SICStus Prolog compatibility

  module/2,
  module/3,
  use_module/1,
  use_module/2,
  meta_predicate/1
]).

% Part 1: General core

% 7.4.2 Directives
dynamic(PI) :- '$dynamic'(PI).
multifile(PI) :- '$multifile'(PI).
discontiguous(PI) :- '$discontiguous'(PI).
initialization(T) :- '$initialization'(T).
include(F) :- '$include'(F).
ensure_loaded(P_text) :- '$ensure_loaded'(P_text).

% 7.8 Control constructs
true.
fail :- \+true.
call(G) :- '$call'(G).
! :- !.
P, Q :- call((P, Q)).
If -> Then; _ :- If, !, Then.
_ -> _; Else :- !, Else.
P; Q :- call((P; Q)).
If -> Then :- If, !, Then.
catch(Goal, Catcher, Recovery) :- '$catch'(Goal, Catcher, Recovery).
throw(B) :- '$throw'(B).

% 8.2 Term unification
X = X.
unify_with_occurs_check(X, Y) :- '$unify_with_occurs_check'(X, Y).
X \= Y :- \+(X = Y).
subsumes_term(General, Specific) :- '$subsumes_term'(General, Specific).

% 8.3 Type testing
var(X) :- '$var'(X).
atom(X) :- '$atom'(X).
integer(X) :- '$integer'(X).
float(X) :- '$float'(X).
atomic(X) :- nonvar(X), \+compound(X).
compound(X) :- '$compound'(X).
nonvar(X) :- \+var(X).
number(X) :- float(X).
number(X) :- integer(X).
callable(X) :- atom(X).
callable(X) :- compound(X).
ground(X) :- term_variables(X, []).
acyclic_term(X) :- '$acyclic_term'(X).

% 8.4 Term comparison
X @=< Y :- compare(=, X, Y).
X @=< Y :- compare(<, X, Y).
X == Y :- compare(=, X, Y).
X \== Y :- \+(X == Y).
X @< Y :- compare(<, X, Y).
X @> Y :- compare(>, X, Y).
X @>= Y :- compare(>, X, Y).
X @>= Y :- compare(=, X, Y).
compare(X, Y) :- '$compare'(X, Y).
sort(List, Sorted) :- '$sort'(List, Sorted).
keysort(Pairs, Sorted) :- '$keysort'(Pairs, Sorted).

% 8.5 Term creation and decomposition
functor(Term, Name, Arity) :- '$functor'(Term, Name, Arity).
arg(N, Term, Arg) :- '$arg'(N, Term, Arg).
Term =.. List :- '$univ'(Term, List).
copy_term(Term_1, Term_2) :- '$copy_term'(Term_1, Term_2).
term_variables(Term, Vars) :- '$term_variables'(Term, Vars).

% 8.6 Arithmetic evaluation
Result is Expression :- '$is'(Result, Expression).

% 8.7 Arithmetic comparison
E1 =:= E2 :- '$equal'(E1, E2).
E1 =\= E2 :- '$not_equal'(E1, E2).
E1 < E2 :- '$less_than'(E1, E2).
E1 =< E2 :- '$less_than_or_equal'(E1, E2).
E1 > E2 :- '$greater_than'(E1, E2).
E1 >= E2 :- '$greater_than_or_equal'(E1, E2).

% 8.8 Clause retrieval and information
clause(Head, Body) :- '$clause'(Head, Body).
current_predicate(PI) :- '$current_predicate'(PI).

% 8.9 Clause creation and destruction
asserta(Clause) :- '$asserta'(Clause).
assertz(Clause) :- '$assertz'(Clause).
retract(Clause) :- '$retract'(Clause).
abolish(Pred) :- '$abolish'(Pred).
retractall(Head) :-
  retract((Head :- _)),
  fail.
retractall(_).

% 8.10 All solutions
findall(Template, Goal, Instances) :- '$findall'(Template, Goal, Instances).
bagof(Template, Goal, Instances) :- '$bagof'(Template, Goal, Instances).
setof(Template, Goal, Instances) :- '$setof'(Template, Goal, Instances).

% 8.11 Stream selection and control
current_input(Stream) :- '$current_input'(Stream).
current_output(Stream) :- '$current_output'(Stream).
set_input(S_or_a) :- '$set_input'(S_or_a).
set_output(S_or_a) :- '$set_output'(S_or_a).
open(Source_sink, Mode, Stream, Options) :- '$open'(Source_sink, Mode, Stream, Options).
open(Source_sink, Mode, Stream) :- open(Source_sink, Mode, Stream, []).
close(S_or_a, Options) :- '$close'(S_or_a, Options).
close(S_or_a) :- close(S_or_a, []).
flush_output(S_or_a) :- '$flush_output'(S_or_a).
flush_output :- current_output(S), flush_output(S).
stream_property(Stream, Property) :- '$stream_property'(Stream, Property).
at_end_of_stream :- current_input(S), at_end_of_stream(S).
at_end_of_stream(S_or_a) :-
  (atom(S_or_a) -> stream_property(S, alias(S_or_a)); S = S_or_a),
  stream_property(S, end_of_stream(E)), !,
  (E = at; E = past).
set_stream_position(S_or_a, Position) :- '$set_stream_position'(S_or_a, Position).

% 8.12 Character input/output
get_char(S_or_a, Char) :- '$get_char'(S_or_a, Char).
get_char(Char) :- current_input(S), get_char(S, Char).
get_code(Code) :- current_input(S), get_code(S, Code).
get_code(S_or_a, Code) :- '$get_code'(S_or_a, Code).
peek_char(S_or_a, Char) :- '$peek_char'(S_or_a, Char).
peek_char(Char) :- current_input(S), peek_char(S, Char).
peek_code(Code) :- current_input(S), peek_code(S, Code).
peek_code(S_or_a, Code) :- peek_char(S_or_a, Char), (Char = end_of_file -> Code = -1; char_code(Char, Code)).
put_char(S_or_a, Char) :- '$put_char'(S_or_a, Char).
put_char(Char) :- current_output(S), put_char(S, Char).
put_code(Code) :- current_output(S), put_code(S, Code).
put_code(S_or_a, Code) :- char_code(Char, Code), put_char(S, Char).
nl :- current_output(S), nl(S).
nl(S_or_a) :- put_char(S_or_a, '\n').

% 8.13 Byte input/output
get_byte(S_or_a, Byte) :- '$get_byte'(S_or_a, Byte).
get_byte(Byte) :- current_input(S), peek_byte(S, Byte).
peek_byte(S_or_a, Byte) :- '$peek_byte'(S_or_a, Byte).
peek_byte(Byte) :- current_input(S), peek_byte(S, Byte).
put_byte(S_or_a, Byte) :- '$put_byte'(S_or_a, Byte).
put_byte(Byte) :- current_output(S), put_byte(S, Byte).

% 8.14 Term input/output
read_term(S_or_a, Term, Options) :- '$read_term'(S_or_a, Term, Options).
read_term(S_or_a, Term) :- current_input(S), read_term(S, Term, Options).
read(Term) :- current_input(S), read(S, Term).
read(S_or_a, Term) :- read_term(S_or_a, Term, []).
write_term(S_or_a, Term, Options) :- '$write_term'(S_or_a, Term, Options).
write_term(Term, Options) :- current_output(S), write_term(S, Term, Options).
write(Term) :- current_output(S), write(S, Term).
write(S_or_a, Term) :- write_term(S_or_a, Term, [numbervars(true)]).
writeq(Term) :- current_output(S), writeq(S, Term).
writeq(S_or_a, Term) :- write_term(S_or_a, Term, [quoted(true), numbervars(true)]).
write_canonical(Term) :- current_output(S), write_canonical(S, Term).
write_canonical(S_or_a, Term) :- write_term(S_or_a, Term, [quoted(true), ignore_ops(true)]).
op(Priority, Op_specifier, Operator) :- '$op'(Priority, Op_specifier, Operator).
current_op(Priority, Op_specifier, Operator) :- '$current_op'(Priority, Op_specifier, Operator).
char_conversion(In_char, Out_char) :- '$char_conversion'(In_char, Out_char).
current_char_conversion(In_char, Out_char) :- '$current_char_conversion'(In_char, Out_char).

% 8.15 Logic and control
\+Term :- '$not'(Term).
once(Term) :- Term, !.
repeat :- '$repeat'.
call(Closure, Arg1) :- '$call'(Closure, Arg1).
call(Closure, Arg1, Arg2) :- '$call'(Closure, Arg1, Arg2).
call(Closure, Arg1, Arg2, Arg3) :- '$call'(Closure, Arg1, Arg2, Arg3).
call(Closure, Arg1, Arg2, Arg3, Arg4) :- '$call'(Closure, Arg1, Arg2, Arg3, Arg4).
call(Closure, Arg1, Arg2, Arg3, Arg4, Arg5) :- '$call'(Closure, Arg1, Arg2, Arg3, Arg4, Arg5).
call(Closure, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6) :- '$call'(Closure, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6).
call(Closure, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7) :- '$call'(Closure, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7).
false :- fail.

% 8.16 Atomic term processing
atom_length(Atom, Length) :- '$atom_length'(Atom, Length).
atom_concat(Atom_1, Atom_2, Atom_3) :- '$atom_concat'(Atom_1, Atom_2, Atom_3).
sub_atom(Atom, Before, Length, After, Sub_atom) :- '$sub_atom'(Atom, Before, Length, After, Sub_atom).
atom_chars(Atom, List) :- '$atom_chars'(Atom, List).
atom_codes(Atom, List) :- '$atom_codes'(Atom, List).
char_code(Char, Code) :- '$char_code'(Char, Code).
number_chars(Number, List) :- '$number_chars'(Number, List).
number_codes(Number, List) :- '$number_codes'(Number, List).

% 8.17 Implementation defined hooks
set_prolog_flag(Flag, Value) :- '$set_prolog_flag'(Flag, Value).
current_prolog_flag(Flag, Value) :- '$current_prolog_flag'(Flag, Value).
halt :- halt(0).
halt(X) :- '$halt'(X).

% Part 2: Modules

% 7.2 Module predicates
current_module(Module) :- '$current_module'(Module).
predicate_property(Prototype, Property) :- '$predicate_property'(Prototype, Property).

% SICStus Prolog compatibility

meta_predicate(MI) :- '$meta_predicate'(MI).
