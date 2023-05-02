:- module(system, [
  true/0, fail/0, call/1, !/0, (',')/2, (;)/2, (->)/2, catch/3, throw/1,
  (=)/2, unify_with_occurs_check/2, (\=)/2, subsumes_term/2,
  var/1, atom/1, integer/1, float/1, atomic/1, compound/1, nonvar/1, number/1, callable/1, ground/1, acyclic_term/1,
  (@=<)/2, (==)/2, (\==)/2, (@<)/2, (@>)/2, (@>=)/2, compare/3, sort/2, keysort/2,
  functor/3, arg/3, (=..)/2, copy_term/2, term_variables/2,
  (is)/2, (=:=)/2, (=\=)/2, (<)/2, (=<)/2, (>)/2, (>=)/2,
  clause/2, current_predicate/1,
  asserta/1, assertz/1, retract/1, abolish/1, retractall/1,
  findall/3, bagof/3, setof/3,
  current_input/1, current_output/1, set_input/1, set_output/1, open/4, open/3, close/2, close/1, flush_output/1, flush_output/0, stream_property/2, at_end_of_stream/0, at_end_of_stream/1, set_stream_position/2,
  get_char/2, get_char/1, get_code/1, get_code/2, peek_char/2, peek_char/1, peek_code/1, peek_code/2, put_char/2, put_char/1, put_code/1, put_code/2, nl/0, nl/1,
  get_byte/2, get_byte/1, peek_byte/2, peek_byte/1, put_byte/2, put_byte/1,
  read_term/3, read_term/2, read/1, read/2,
  write_term/3, write_term/2, write/1, write/2, writeq/1, writeq/2, write_canonical/1, write_canonical/2,
  op/3, current_op/3, char_conversion/2, current_char_conversion/2,
  (\+)/1, once/1, repeat/0, call/2, call/3, call/4, call/5, call/6, call/7, call/8, false/0,
  atom_length/2, atom_concat/3, sub_atom/5, atom_chars/2, atom_codes/2, char_code/2, number_chars/2, number_codes/2,
  set_prolog_flag/2, current_prolog_flag/2, halt/0, halt/1,
  consult/1, (.)/1
]).

:-(use_module(user, _, all)).
