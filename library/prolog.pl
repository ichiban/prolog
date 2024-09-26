:- native:module(prolog, [
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
  meta_predicate/1,
  consult/1
]).

% 7.4.2 Directives
dynamic(PI) :- native:dynamic(PI).
multifile(PI) :- native:multifile(PI).
discontiguous(PI) :- native:discontiguous(PI).
initialization(T) :- native:initialization(T).
include(F) :- native:include(F).
ensure_loaded(P_text) :-
  load_files(P_text, [if(changed)]).

load_files([], _).
load_files([File|Files], Options) :-
  load_files(File, Options),
  load_files(Files, Options).
load_files(File, Options) :-
  load_file(File, Options).

load_file(FileSpec, Options) :-
  file_spec_path(FileSpec, Path),
  native:load_file(Path, Options).

file_spec_path(FileSpec, FileSpec) :- atom(FileSpec), !.
file_spec_path(FileSpec, Path) :- % Converts foo(bar) to 'foo/bar' for compatibility.
  FileSpec =.. [PathAlias, DirSpec],
  atom(PathAlias),
  catch(file_spec_path(DirSpec, Path1), _, false), !,
  atom_concat(PathAlias, /, X),
  atom_concat(X, Path1, Path).
file_spec_path(FileSpec1/FileSpec2, Path) :- % Converts foo/bar to 'foo/bar'
  catch(file_spec_path(FileSpec1, Path1), _, false),
  atom(FileSpec2), !,
  atom_concat(Path1, /, X),
  atom_concat(X, FileSpec2, Path).
file_spec_path(FileSpec, _) :-
  throw(error(domain_error(file_spec, FileSpec), file_spec_path/2)).

% 7.8 Control constructs
true.
fail :- \+true.
call(G) :- native:call(G).
! :- !.
P, Q :- call((P, Q)).
If -> Then; _ :- If, !, Then.
_ -> _; Else :- !, Else.
P; Q :- call((P; Q)).
If -> Then :- If, !, Then.
catch(Goal, Catcher, Recovery) :- native:catch(Goal, Catcher, Recovery).
throw(B) :- native:throw(B).

% 8.2 Term unification
X = X.
unify_with_occurs_check(X, Y) :- native:unify_with_occurs_check(X, Y).
X \= Y :- \+(X = Y).
subsumes_term(General, Specific) :- native:subsumes_term(General, Specific).

% 8.3 Type testing
var(X) :- native:var(X).
atom(X) :- native:atom(X).
integer(X) :- native:integer(X).
float(X) :- native:float(X).
atomic(X) :- nonvar(X), \+compound(X).
compound(X) :- native:compound(X).
nonvar(X) :- \+var(X).
number(X) :- float(X).
number(X) :- integer(X).
callable(X) :- atom(X).
callable(X) :- compound(X).
ground(X) :- term_variables(X, []).
acyclic_term(X) :- native:acyclic_term(X).

% 8.4 Term comparison
X @=< Y :- compare(=, X, Y).
X @=< Y :- compare(<, X, Y).
X == Y :- compare(=, X, Y).
X \== Y :- \+(X == Y).
X @< Y :- compare(<, X, Y).
X @> Y :- compare(>, X, Y).
X @>= Y :- compare(>, X, Y).
X @>= Y :- compare(=, X, Y).
compare(Order, X, Y) :- native:compare(Order, X, Y).
sort(List, Sorted) :- native:sort(List, Sorted).
keysort(Pairs, Sorted) :- native:keysort(Pairs, Sorted).

% 8.5 Term creation and decomposition
functor(Term, Name, Arity) :- native:functor(Term, Name, Arity).
arg(N, Term, Arg) :- native:arg(N, Term, Arg).
Term =.. List :- native:univ(Term, List).
copy_term(Term_1, Term_2) :- native:copy_term(Term_1, Term_2).
term_variables(Term, Vars) :- native:term_variables(Term, Vars).

% 8.6 Arithmetic evaluation
Result is Expression :- native:is(Result, Expression).

% 8.7 Arithmetic comparison
E1 =:= E2 :- native:equal(E1, E2).
E1 =\= E2 :- native:not_equal(E1, E2).
E1 < E2 :- native:less_than(E1, E2).
E1 =< E2 :- native:less_than_or_equal(E1, E2).
E1 > E2 :- native:greater_than(E1, E2).
E1 >= E2 :- native:greater_than_or_equal(E1, E2).

% 8.8 Clause retrieval and information
:- native:meta_predicate(clause(0, ?)).
clause(Head, Body) :- native:clause(Head, Body).
current_predicate(PI) :- native:current_predicate(PI).

% 8.9 Clause creation and destruction
:- native:meta_predicate(asserta(0)).
asserta(Clause) :- native:asserta(Clause).
:- native:meta_predicate(assertz(0)).
assertz(Clause) :- native:assertz(Clause).
:- native:meta_predicate(retract(0)).
retract(Clause) :- native:retract(Clause).
:- native:meta_predicate(abolish(0)).
abolish(Pred) :- native:abolish(Pred).
retractall(Head) :-
  retract((Head :- _)),
  fail.
retractall(_).

% 8.10 All solutions

:- meta_predicate(findall(+, 0, -)).
findall(Template, Goal, Instances) :-
  native:create_bag,
  findall_loop(Template, Goal),
  native:unify_bag(Instances).

findall_loop(Template, Goal) :-
  Goal,
  copy_term(Template, CT),
  native:append_bag(CT),
  fail.
findall_loop(_, _).

:- native:meta_predicate(bagof(+, 0, -)).
bagof(Template, Goal, Instances) :- native:bagof(Template, Goal, Instances).

:- native:meta_predicate(setof(+, 0, -)).
setof(Template, Goal, Instances) :- native:setof(Template, Goal, Instances).

% 8.11 Stream selection and control
current_input(Stream) :- native:current_input(Stream).
current_output(Stream) :- native:current_output(Stream).
set_input(S_or_a) :- native:set_input(S_or_a).
set_output(S_or_a) :- native:set_output(S_or_a).
open(Source_sink, Mode, Stream, Options) :- native:open(Source_sink, Mode, Stream, Options).
open(Source_sink, Mode, Stream) :- open(Source_sink, Mode, Stream, []).
close(S_or_a, Options) :- native:close(S_or_a, Options).
close(S_or_a) :- close(S_or_a, []).
flush_output(S_or_a) :- native:flush_output(S_or_a).
flush_output :- current_output(S), flush_output(S).
stream_property(Stream, Property) :- native:stream_property(Stream, Property).
at_end_of_stream :- current_input(S), at_end_of_stream(S).
at_end_of_stream(S_or_a) :-
  (atom(S_or_a) -> stream_property(S, alias(S_or_a)); S = S_or_a),
  stream_property(S, end_of_stream(E)), !,
  (E = at; E = past).
set_stream_position(S_or_a, Position) :- native:set_stream_position(S_or_a, Position).

% 8.12 Character input/output
get_char(S_or_a, Char) :- native:get_char(S_or_a, Char).
get_char(Char) :- current_input(S), get_char(S, Char).
get_code(Code) :- current_input(S), get_code(S, Code).
get_code(S_or_a, Code) :- native:get_code(S_or_a, Code).
peek_char(S_or_a, Char) :- native:peek_char(S_or_a, Char).
peek_char(Char) :- current_input(S), peek_char(S, Char).
peek_code(Code) :- current_input(S), peek_code(S, Code).
peek_code(S_or_a, Code) :- peek_char(S_or_a, Char), (Char = end_of_file -> Code = -1; char_code(Char, Code)).
put_char(S_or_a, Char) :- native:put_char(S_or_a, Char).
put_char(Char) :- current_output(S), put_char(S, Char).
put_code(Code) :- current_output(S), put_code(S, Code).
put_code(S_or_a, Code) :- char_code(Char, Code), put_char(S, Char).
nl :- current_output(S), nl(S).
nl(S_or_a) :- put_char(S_or_a, '\n').

% 8.13 Byte input/output
get_byte(S_or_a, Byte) :- native:get_byte(S_or_a, Byte).
get_byte(Byte) :- current_input(S), peek_byte(S, Byte).
peek_byte(S_or_a, Byte) :- native:peek_byte(S_or_a, Byte).
peek_byte(Byte) :- current_input(S), peek_byte(S, Byte).
put_byte(S_or_a, Byte) :- native:put_byte(S_or_a, Byte).
put_byte(Byte) :- current_output(S), put_byte(S, Byte).

% 8.14 Term input/output
read_term(S_or_a, Term, Options) :- native:read_term(S_or_a, Term, Options).
read_term(Term, Options) :- current_input(S), read_term(S, Term, Options).
read(Term) :- current_input(S), read(S, Term).
read(S_or_a, Term) :- read_term(S_or_a, Term, []).
write_term(S_or_a, Term, Options) :- native:write_term(S_or_a, Term, Options).
write_term(Term, Options) :- current_output(S), write_term(S, Term, Options).
write(Term) :- current_output(S), write(S, Term).
write(S_or_a, Term) :- write_term(S_or_a, Term, [numbervars(true)]).
writeq(Term) :- current_output(S), writeq(S, Term).
writeq(S_or_a, Term) :- write_term(S_or_a, Term, [quoted(true), numbervars(true)]).
write_canonical(Term) :- current_output(S), write_canonical(S, Term).
write_canonical(S_or_a, Term) :- write_term(S_or_a, Term, [quoted(true), ignore_ops(true)]).
op(Priority, Op_specifier, Operator) :- native:op(Priority, Op_specifier, Operator).
current_op(Priority, Op_specifier, Operator) :- native:current_op(Priority, Op_specifier, Operator).
char_conversion(In_char, Out_char) :- native:char_conversion(In_char, Out_char).
current_char_conversion(In_char, Out_char) :- native:current_char_conversion(In_char, Out_char).

% 8.15 Logic and control
:- native:meta_predicate(\+ 0).
\+Term :- native:not(Term).
:- native:meta_predicate(once(0)).
once(Term) :- Term, !.
repeat :- native:repeat.
:- native:meta_predicate(call(1, ?)).
call(Closure, Arg1) :- native:call(Closure, Arg1).
:- native:meta_predicate(call(2, ?, ?)).
call(Closure, Arg1, Arg2) :- native:call(Closure, Arg1, Arg2).
:- native:meta_predicate(call(3, ?, ?, ?)).
call(Closure, Arg1, Arg2, Arg3) :- native:call(Closure, Arg1, Arg2, Arg3).
:- native:meta_predicate(call(4, ?, ?, ?, ?)).
call(Closure, Arg1, Arg2, Arg3, Arg4) :- native:call(Closure, Arg1, Arg2, Arg3, Arg4).
:- native:meta_predicate(call(5, ?, ?, ?, ?, ?)).
call(Closure, Arg1, Arg2, Arg3, Arg4, Arg5) :- native:call(Closure, Arg1, Arg2, Arg3, Arg4, Arg5).
:- native:meta_predicate(call(6, ?, ?, ?, ?, ?, ?)).
call(Closure, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6) :- native:call(Closure, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6).
:- native:meta_predicate(call(7, ?, ?, ?, ?, ?, ?, ?)).
call(Closure, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7) :- native:call(Closure, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7).
false :- fail.

% 8.16 Atomic term processing
atom_length(Atom, Length) :- native:atom_length(Atom, Length).
atom_concat(Atom_1, Atom_2, Atom_3) :- native:atom_concat(Atom_1, Atom_2, Atom_3).
sub_atom(Atom, Before, Length, After, Sub_atom) :- native:sub_atom(Atom, Before, Length, After, Sub_atom).
atom_chars(Atom, List) :- native:atom_chars(Atom, List).
atom_codes(Atom, List) :- native:atom_codes(Atom, List).
char_code(Char, Code) :- native:char_code(Char, Code).
number_chars(Number, List) :- native:number_chars(Number, List).
number_codes(Number, List) :- native:number_codes(Number, List).

% 8.17 Implementation defined hooks
set_prolog_flag(Flag, Value) :- native:set_prolog_flag(Flag, Value).
current_prolog_flag(Flag, Value) :- native:current_prolog_flag(Flag, Value).
halt :- halt(0).
halt(X) :- native:halt(X).

% SICStus Prolog compatibility

module(ModuleName, ExportList) :-
  native:module(ModuleName, ExportList).

current_module(Module) :- native:current_module(Module).
:- native:meta_predicate(predicate_property(0, ?)).
predicate_property(Prototype, Property) :- native:predicate_property(Prototype, Property).
meta_predicate(MI) :- native:meta_predicate(MI).

use_module(File) :-
  load_files([File], [if(changed)]).
use_module(File, ImportList) :-
  load_files([File], [if(changed), imports(ImportList)]).

consult(Files) :-
  load_files(Files, []).

[].
[F|Fs] :- consult([F|Fs]).
