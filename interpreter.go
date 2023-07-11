package prolog

import (
	"context"
	_ "embed" // for go:embed
	"errors"
	"github.com/ichiban/prolog/engine"
	"io"
	"strings"
)

var system engine.Module

func init() {
	// Control constructs
	system.Register1(engine.NewAtom("call"), engine.Call)
	system.Register3(engine.NewAtom("catch"), engine.Catch)
	system.Register1(engine.NewAtom("throw"), engine.Throw)

	// Term unification
	system.Register2(engine.NewAtom("="), engine.Unify)
	system.Register2(engine.NewAtom("unify_with_occurs_check"), engine.UnifyWithOccursCheck)
	system.Register2(engine.NewAtom("subsumes_term"), engine.SubsumesTerm)

	// Type testing
	system.Register1(engine.NewAtom("var"), engine.TypeVar)
	system.Register1(engine.NewAtom("atom"), engine.TypeAtom)
	system.Register1(engine.NewAtom("integer"), engine.TypeInteger)
	system.Register1(engine.NewAtom("float"), engine.TypeFloat)
	system.Register1(engine.NewAtom("compound"), engine.TypeCompound)
	system.Register1(engine.NewAtom("acyclic_term"), engine.AcyclicTerm)

	// Term comparison
	system.Register3(engine.NewAtom("compare"), engine.Compare)
	system.Register2(engine.NewAtom("sort"), engine.Sort)
	system.Register2(engine.NewAtom("keysort"), engine.KeySort)

	// Term creation and decomposition
	system.Register3(engine.NewAtom("functor"), engine.Functor)
	system.Register3(engine.NewAtom("arg"), engine.Arg)
	system.Register2(engine.NewAtom("=.."), engine.Univ)
	system.Register2(engine.NewAtom("copy_term"), engine.CopyTerm)
	system.Register2(engine.NewAtom("term_variables"), engine.TermVariables)

	// Arithmetic evaluation
	system.Register2(engine.NewAtom("is"), engine.Is)

	// Arithmetic comparison
	system.Register2(engine.NewAtom("=:="), engine.Equal)
	system.Register2(engine.NewAtom("=\\="), engine.NotEqual)
	system.Register2(engine.NewAtom("<"), engine.LessThan)
	system.Register2(engine.NewAtom("=<"), engine.LessThanOrEqual)
	system.Register2(engine.NewAtom(">"), engine.GreaterThan)
	system.Register2(engine.NewAtom(">="), engine.GreaterThanOrEqual)

	// Clause retrieval and information
	system.Register2(engine.NewAtom("clause"), engine.Clause)
	system.Register1(engine.NewAtom("current_predicate"), engine.CurrentPredicate)

	// Clause creation and destruction
	system.Register1(engine.NewAtom("asserta"), engine.Asserta)
	system.Register1(engine.NewAtom("assertz"), engine.Assertz)
	system.Register1(engine.NewAtom("retract"), engine.Retract)
	system.Register1(engine.NewAtom("abolish"), engine.Abolish)

	// All solutions
	system.Register3(engine.NewAtom("findall"), engine.FindAll)
	system.Register3(engine.NewAtom("bagof"), engine.BagOf)
	system.Register3(engine.NewAtom("setof"), engine.SetOf)

	// Stream selection and control
	system.Register1(engine.NewAtom("current_input"), engine.CurrentInput)
	system.Register1(engine.NewAtom("current_output"), engine.CurrentOutput)
	system.Register1(engine.NewAtom("set_input"), engine.SetInput)
	system.Register1(engine.NewAtom("set_output"), engine.SetOutput)
	system.Register4(engine.NewAtom("open"), engine.Open)
	system.Register2(engine.NewAtom("close"), engine.Close)
	system.Register1(engine.NewAtom("flush_output"), engine.FlushOutput)
	system.Register2(engine.NewAtom("stream_property"), engine.StreamProperty)
	system.Register2(engine.NewAtom("set_stream_position"), engine.SetStreamPosition)

	// Character input/output
	system.Register2(engine.NewAtom("get_char"), engine.GetChar)
	system.Register2(engine.NewAtom("peek_char"), engine.PeekChar)
	system.Register2(engine.NewAtom("put_char"), engine.PutChar)

	// Byte input/output
	system.Register2(engine.NewAtom("get_byte"), engine.GetByte)
	system.Register2(engine.NewAtom("peek_byte"), engine.PeekByte)
	system.Register2(engine.NewAtom("put_byte"), engine.PutByte)

	// Term input/output
	system.Register3(engine.NewAtom("read_term"), engine.ReadTerm)
	system.Register3(engine.NewAtom("write_term"), engine.WriteTerm)
	system.Register3(engine.NewAtom("op"), engine.Op)
	system.Register3(engine.NewAtom("current_op"), engine.CurrentOp)
	system.Register2(engine.NewAtom("char_conversion"), engine.CharConversion)
	system.Register2(engine.NewAtom("current_char_conversion"), engine.CurrentCharConversion)

	// Logic and control
	system.Register1(engine.NewAtom(`\+`), engine.Negate)
	system.Register0(engine.NewAtom("repeat"), engine.Repeat)
	system.Register2(engine.NewAtom("call"), engine.Call1)
	system.Register3(engine.NewAtom("call"), engine.Call2)
	system.Register4(engine.NewAtom("call"), engine.Call3)
	system.Register5(engine.NewAtom("call"), engine.Call4)
	system.Register6(engine.NewAtom("call"), engine.Call5)
	system.Register7(engine.NewAtom("call"), engine.Call6)
	system.Register8(engine.NewAtom("call"), engine.Call7)

	// Atomic term processing
	system.Register2(engine.NewAtom("atom_length"), engine.AtomLength)
	system.Register3(engine.NewAtom("atom_concat"), engine.AtomConcat)
	system.Register5(engine.NewAtom("sub_atom"), engine.SubAtom)
	system.Register2(engine.NewAtom("atom_chars"), engine.AtomChars)
	system.Register2(engine.NewAtom("atom_codes"), engine.AtomCodes)
	system.Register2(engine.NewAtom("char_code"), engine.CharCode)
	system.Register2(engine.NewAtom("number_chars"), engine.NumberChars)
	system.Register2(engine.NewAtom("number_codes"), engine.NumberCodes)

	// Implementation defined hooks
	system.Register2(engine.NewAtom("set_prolog_flag"), engine.SetPrologFlag)
	system.Register2(engine.NewAtom("current_prolog_flag"), engine.CurrentPrologFlag)
	system.Register1(engine.NewAtom("halt"), engine.Halt)

	// Modules
	system.Register1(engine.NewAtom("current_module"), engine.CurrentModule)

	// Prolog prologue
	system.Register3(engine.NewAtom("append"), engine.Append)
	system.Register2(engine.NewAtom("length"), engine.Length)
	system.Register3(engine.NewAtom("between"), engine.Between)
	system.Register2(engine.NewAtom("succ"), engine.Succ)
	system.Register3(engine.NewAtom("nth0"), engine.Nth0)
	system.Register3(engine.NewAtom("nth1"), engine.Nth1)
	system.Register2(engine.NewAtom("call_nth"), engine.CallNth)

	// Compatibility
	system.Register1(engine.NewAtom("consult"), engine.Consult)
	system.Register3(engine.NewAtom("use_module"), engine.UseModule)

	// DCGs
	system.Register3(engine.NewAtom("phrase"), engine.Phrase)
	system.Register2(engine.NewAtom("expand_term"), engine.ExpandTerm)
}

//go:embed bootstrap.pl
var bootstrap string

// Interpreter is a Prolog interpreter. The zero value is a valid interpreter without any predicates/operators defined.
type Interpreter struct {
	engine.VM
	loaded map[string]struct{}
}

// New creates a new Prolog interpreter with predefined predicates/operators.
func New(in io.Reader, out io.Writer) *Interpreter {
	var i Interpreter
	i.FS = engine.OverlayFS{
		engine.MapFS{
			"/prolog/system": system,
		},
		engine.RealFS{},
	}
	i.SetUserInput(engine.NewInputTextStream(in))
	i.SetUserOutput(engine.NewOutputTextStream(out))

	if err := i.Exec(bootstrap); err != nil {
		panic(err)
	}

	// Explicitly initialize `user` module to reflect `system` module.
	if err := i.Exec(`:-(module(user, [])).`); err != nil {
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
	return i.Compile(ctx, query, args...)
}

// Query executes a prolog query and returns *Solutions.
func (i *Interpreter) Query(query string, args ...interface{}) (*Solutions, error) {
	return i.QueryContext(context.Background(), query, args...)
}

// QueryContext executes a prolog query and returns *Solutions with context.
func (i *Interpreter) QueryContext(ctx context.Context, query string, args ...interface{}) (*Solutions, error) {
	p := engine.NewParser(&i.VM, strings.NewReader(query))
	if err := p.SetPlaceholder(engine.NewAtom("?"), args...); err != nil {
		return nil, err
	}

	t, err := p.Term()
	if err != nil {
		return nil, err
	}

	var env *engine.Env

	more := make(chan bool, 1)
	next := make(chan *engine.Env)
	sols := Solutions{
		vm:   &i.VM,
		vars: p.Vars,
		more: more,
		next: next,
	}

	go func() {
		defer close(next)
		if !<-more {
			return
		}
		if _, err := engine.Call(&i.VM, t, func(env *engine.Env) *engine.Promise {
			next <- env
			return engine.Bool(!<-more)
		}, env).Force(ctx); err != nil {
			sols.err = err
		}
	}()

	return &sols, nil
}

// ErrNoSolutions indicates there's no solutions for the query.
var ErrNoSolutions = errors.New("no solutions")

// QuerySolution executes a Prolog query for the first solution.
func (i *Interpreter) QuerySolution(query string, args ...interface{}) *Solution {
	return i.QuerySolutionContext(context.Background(), query, args...)
}

// QuerySolutionContext executes a Prolog query with context.
func (i *Interpreter) QuerySolutionContext(ctx context.Context, query string, args ...interface{}) *Solution {
	sols, err := i.QueryContext(ctx, query, args...)
	if err != nil {
		return &Solution{err: err}
	}

	if !sols.Next() {
		if err := sols.Err(); err != nil {
			return &Solution{err: err}
		}
		return &Solution{err: ErrNoSolutions}
	}

	return &Solution{sols: sols, err: sols.Close()}
}
