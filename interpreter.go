package prolog

import (
	"context"
	_ "embed"
	"io"
	"strings"

	"github.com/ichiban/prolog/nondet"
	"github.com/ichiban/prolog/term"

	"github.com/ichiban/prolog/engine"
)

//go:embed bootstrap.pl
var bootstrap string

// Interpreter is a Prolog interpreter. The zero value is a valid interpreter without any predicates/operators defined.
type Interpreter struct {
	engine.VM
}

// New creates a new Prolog interpreter with predefined predicates/operators.
func New(in io.Reader, out io.Writer) *Interpreter {
	var i Interpreter
	i.SetUserInput(in)
	i.SetUserOutput(out)
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
	i.Register2("=", engine.Unify)
	i.Register2("unify_with_occurs_check", engine.UnifyWithOccursCheck)
	i.Register2("=..", engine.Univ)
	i.Register2("copy_term", engine.CopyTerm)
	i.Register3("arg", engine.Arg)
	i.Register3("bagof", i.BagOf)
	i.Register3("setof", i.SetOf)
	i.Register3("findall", i.FindAll)
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
	i.Register1("halt", engine.Halt)
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
	if err := i.Exec(bootstrap); err != nil {
		panic(err)
	}
	return &i
}

// Exec executes a prolog program.
func (i *Interpreter) Exec(query string, args ...interface{}) error {
	return i.ExecContext(context.Background(), query, args...)
}

// ExecContext executes a prolog program with context.
func (i *Interpreter) ExecContext(ctx context.Context, query string, args ...interface{}) error {
	p := i.Parser(strings.NewReader(query), nil)
	if err := p.Replace("?", args...); err != nil {
		return err
	}
	for p.More() {
		t, err := p.Term()
		if err != nil {
			return err
		}

		if _, err := i.Assertz(t, engine.Success, nil).Force(ctx); err != nil {
			return err
		}
	}
	return nil
}

// Query executes a prolog query and returns *Solutions.
func (i *Interpreter) Query(query string, args ...interface{}) (*Solutions, error) {
	return i.QueryContext(context.Background(), query, args...)
}

// QueryContext executes a prolog query and returns *Solutions with context.
func (i *Interpreter) QueryContext(ctx context.Context, query string, args ...interface{}) (*Solutions, error) {
	p := i.Parser(strings.NewReader(query), nil)
	if err := p.Replace("?", args...); err != nil {
		return nil, err
	}
	t, err := p.Term()
	if err != nil {
		return nil, err
	}

	var env *term.Env

	more := make(chan bool, 1)
	next := make(chan *term.Env)
	sols := Solutions{
		vars: env.FreeVariables(t),
		more: more,
		next: next,
	}

	go func() {
		defer close(next)
		if !<-more {
			return
		}
		if _, err := i.Call(t, func(env *term.Env) *nondet.Promise {
			next <- env
			return nondet.Bool(!<-more)
		}, env).Force(ctx); err != nil {
			sols.err = err
		}
	}()

	return &sols, nil
}
