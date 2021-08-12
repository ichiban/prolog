package prolog

import (
	"bufio"
	"io"
	"strings"

	"github.com/ichiban/prolog/engine"
)

// Interpreter is a Prolog interpreter. The zero value is a valid interpreter without any predicates/operators defined.
type Interpreter struct {
	engine.VM
}

// New creates a new Prolog interpreter with predefined predicates/operators.
func New(in io.Reader, out io.Writer) *Interpreter {
	var i Interpreter
	i.SetUserInput(in)
	i.SetUserOutput(out)
	i.Register0("repeat", engine.Repeat)
	i.Register1(`\+`, i.Negation)
	i.Register1("call", i.Call)
	i.Register1("current_predicate", i.CurrentPredicate)
	i.Register1("assertz", i.Assertz)
	i.Register1("asserta", i.Asserta)
	i.Register1("retract", i.Retract)
	i.Register1("abolish", i.Abolish)
	i.Register1("var", engine.TypeVar)
	i.Register1("float", engine.TypeFloat)
	i.Register1("integer", engine.TypeInteger)
	i.Register1("atom", engine.TypeAtom)
	i.Register1("compound", engine.TypeCompound)
	i.Register1("throw", engine.Throw)
	i.Register2(",", i.Conjunction)
	i.Register2(";", i.Disjunction)
	i.Register2("=", engine.Unify)
	i.Register2("unify_with_occurs_check", engine.UnifyWithOccursCheck)
	i.Register2("=..", engine.Univ)
	i.Register2("copy_term", engine.CopyTerm)
	i.Register3("arg", engine.Arg)
	i.Register3("bagof", i.BagOf)
	i.Register3("setof", i.SetOf)
	i.Register3("catch", i.Catch)
	i.Register3("functor", engine.Functor)
	i.Register3("op", i.Op)
	i.Register3("compare", engine.Compare)
	i.Register3("current_op", i.CurrentOp)
	i.Register1("current_input", i.CurrentInput)
	i.Register1("current_output", i.CurrentOutput)
	i.Register1("set_input", i.SetInput)
	i.Register1("set_output", i.SetOutput)
	i.Register4("open", i.Open)
	i.Register2("close", i.Close)
	i.Register1("flush_output", i.FlushOutput)
	i.Register3("write_term", i.WriteTerm)
	i.Register2("char_code", engine.CharCode)
	i.Register2("put_byte", i.PutByte)
	i.Register2("put_code", i.PutCode)
	i.Register3("read_term", i.ReadTerm)
	i.Register2("get_byte", i.GetByte)
	i.Register2("get_char", i.GetChar)
	i.Register2("peek_byte", i.PeekByte)
	i.Register2("peek_char", i.PeekChar)
	i.Register1("halt", i.Halt)
	i.Register2("clause", i.Clause)
	i.Register2("atom_length", engine.AtomLength)
	i.Register3("atom_concat", engine.AtomConcat)
	i.Register5("sub_atom", engine.SubAtom)
	i.Register2("atom_chars", engine.AtomChars)
	i.Register2("atom_codes", engine.AtomCodes)
	i.Register2("number_chars", engine.NumberChars)
	i.Register2("number_codes", engine.NumberCodes)
	i.Register2("is", engine.DefaultFunctionSet.Is)
	i.Register2("=:=", engine.DefaultFunctionSet.Equal)
	i.Register2("=\\=", engine.DefaultFunctionSet.NotEqual)
	i.Register2("<", engine.DefaultFunctionSet.LessThan)
	i.Register2(">", engine.DefaultFunctionSet.GreaterThan)
	i.Register2("=<", engine.DefaultFunctionSet.LessThanOrEqual)
	i.Register2(">=", engine.DefaultFunctionSet.GreaterThanOrEqual)
	i.Register2("stream_property", i.StreamProperty)
	i.Register2("set_stream_position", i.SetStreamPosition)
	i.Register2("char_conversion", i.CharConversion)
	i.Register2("current_char_conversion", i.CurrentCharConversion)
	i.Register2("set_prolog_flag", i.SetPrologFlag)
	i.Register2("current_prolog_flag", i.CurrentPrologFlag)
	i.Register1("dynamic", i.Dynamic)
	if err := i.Exec(`
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

`); err != nil {
		panic(err)
	}
	return &i
}

// Exec executes a prolog program.
func (i *Interpreter) Exec(query string, args ...interface{}) error {
	env := engine.NewEnv(nil)
	p := engine.NewParser(&i.VM, bufio.NewReader(strings.NewReader(query)))
	if err := p.Replace("?", args...); err != nil {
		return err
	}
	for p.More() {
		t, err := p.Term()
		if err != nil {
			return err
		}

		if _, err := i.Assertz(t, engine.Success, env).Force(); err != nil {
			return err
		}
	}
	return nil
}

// Query executes a prolog query and returns *Solutions.
func (i *Interpreter) Query(query string, args ...interface{}) (*Solutions, error) {
	p := engine.NewParser(&i.VM, bufio.NewReader(strings.NewReader(query)))
	if err := p.Replace("?", args...); err != nil {
		return nil, err
	}
	t, err := p.Term()
	if err != nil {
		return nil, err
	}

	env := engine.NewEnv(nil)

	more := make(chan bool, 1)
	next := make(chan *engine.Env)
	sols := Solutions{
		vars: env.FreeVariables(t),
		more: more,
		next: next,
	}

	go func() {
		defer close(next)
		defer func() {
			if i.OnPanic == nil {
				i.OnPanic = func(r interface{}) {}
			}
			if r := recover(); r != nil {
				i.OnPanic(r)
			}
		}()
		if !<-more {
			return
		}
		if _, err := i.Call(t, func(env *engine.Env) engine.Promise {
			next <- env
			return engine.Bool(!<-more)
		}, env).Force(); err != nil {
			sols.err = err
		}
	}()

	return &sols, nil
}
