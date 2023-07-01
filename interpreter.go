package prolog

import (
	"context"
	_ "embed" // for go:embed
	"errors"
	"github.com/ichiban/prolog/engine"
	"io"
	"strings"
)

var builtins engine.Module

func init() {
	// Control constructs
	builtins.Register1(engine.NewAtom("call"), engine.Call)
	builtins.Register3(engine.NewAtom("catch"), engine.Catch)
	builtins.Register1(engine.NewAtom("throw"), engine.Throw)

	// Term unification
	builtins.Register2(engine.NewAtom("="), engine.Unify)
	builtins.Register2(engine.NewAtom("unify_with_occurs_check"), engine.UnifyWithOccursCheck)
	builtins.Register2(engine.NewAtom("subsumes_term"), engine.SubsumesTerm)

	// Type testing
	builtins.Register1(engine.NewAtom("var"), engine.TypeVar)
	builtins.Register1(engine.NewAtom("atom"), engine.TypeAtom)
	builtins.Register1(engine.NewAtom("integer"), engine.TypeInteger)
	builtins.Register1(engine.NewAtom("float"), engine.TypeFloat)
	builtins.Register1(engine.NewAtom("compound"), engine.TypeCompound)
	builtins.Register1(engine.NewAtom("acyclic_term"), engine.AcyclicTerm)

	// Term comparison
	builtins.Register3(engine.NewAtom("compare"), engine.Compare)
	builtins.Register2(engine.NewAtom("sort"), engine.Sort)
	builtins.Register2(engine.NewAtom("keysort"), engine.KeySort)

	// Term creation and decomposition
	builtins.Register3(engine.NewAtom("functor"), engine.Functor)
	builtins.Register3(engine.NewAtom("arg"), engine.Arg)
	builtins.Register2(engine.NewAtom("=.."), engine.Univ)
	builtins.Register2(engine.NewAtom("copy_term"), engine.CopyTerm)
	builtins.Register2(engine.NewAtom("term_variables"), engine.TermVariables)

	// Arithmetic evaluation
	builtins.Register2(engine.NewAtom("is"), engine.Is)

	// Arithmetic comparison
	builtins.Register2(engine.NewAtom("=:="), engine.Equal)
	builtins.Register2(engine.NewAtom("=\\="), engine.NotEqual)
	builtins.Register2(engine.NewAtom("<"), engine.LessThan)
	builtins.Register2(engine.NewAtom("=<"), engine.LessThanOrEqual)
	builtins.Register2(engine.NewAtom(">"), engine.GreaterThan)
	builtins.Register2(engine.NewAtom(">="), engine.GreaterThanOrEqual)

	// Clause retrieval and information
	builtins.Register2(engine.NewAtom("clause"), engine.Clause)
	builtins.Register1(engine.NewAtom("current_predicate"), engine.CurrentPredicate)

	// Clause creation and destruction
	builtins.Register1(engine.NewAtom("asserta"), engine.Asserta)
	builtins.Register1(engine.NewAtom("assertz"), engine.Assertz)
	builtins.Register1(engine.NewAtom("retract"), engine.Retract)
	builtins.Register1(engine.NewAtom("abolish"), engine.Abolish)

	// All solutions
	builtins.Register3(engine.NewAtom("findall"), engine.FindAll)
	builtins.Register3(engine.NewAtom("bagof"), engine.BagOf)
	builtins.Register3(engine.NewAtom("setof"), engine.SetOf)

	// Stream selection and control
	builtins.Register1(engine.NewAtom("current_input"), engine.CurrentInput)
	builtins.Register1(engine.NewAtom("current_output"), engine.CurrentOutput)
	builtins.Register1(engine.NewAtom("set_input"), engine.SetInput)
	builtins.Register1(engine.NewAtom("set_output"), engine.SetOutput)
	builtins.Register4(engine.NewAtom("open"), engine.Open)
	builtins.Register2(engine.NewAtom("close"), engine.Close)
	builtins.Register1(engine.NewAtom("flush_output"), engine.FlushOutput)
	builtins.Register2(engine.NewAtom("stream_property"), engine.StreamProperty)
	builtins.Register2(engine.NewAtom("set_stream_position"), engine.SetStreamPosition)

	// Character input/output
	builtins.Register2(engine.NewAtom("get_char"), engine.GetChar)
	builtins.Register2(engine.NewAtom("peek_char"), engine.PeekChar)
	builtins.Register2(engine.NewAtom("put_char"), engine.PutChar)

	// Byte input/output
	builtins.Register2(engine.NewAtom("get_byte"), engine.GetByte)
	builtins.Register2(engine.NewAtom("peek_byte"), engine.PeekByte)
	builtins.Register2(engine.NewAtom("put_byte"), engine.PutByte)

	// Term input/output
	builtins.Register3(engine.NewAtom("read_term"), engine.ReadTerm)
	builtins.Register3(engine.NewAtom("write_term"), engine.WriteTerm)
	builtins.Register3(engine.NewAtom("op"), engine.Op)
	builtins.Register3(engine.NewAtom("current_op"), engine.CurrentOp)
	builtins.Register2(engine.NewAtom("char_conversion"), engine.CharConversion)
	builtins.Register2(engine.NewAtom("current_char_conversion"), engine.CurrentCharConversion)

	// Logic and control
	builtins.Register1(engine.NewAtom(`\+`), engine.Negate)
	builtins.Register0(engine.NewAtom("repeat"), engine.Repeat)
	builtins.Register2(engine.NewAtom("call"), engine.Call1)
	builtins.Register3(engine.NewAtom("call"), engine.Call2)
	builtins.Register4(engine.NewAtom("call"), engine.Call3)
	builtins.Register5(engine.NewAtom("call"), engine.Call4)
	builtins.Register6(engine.NewAtom("call"), engine.Call5)
	builtins.Register7(engine.NewAtom("call"), engine.Call6)
	builtins.Register8(engine.NewAtom("call"), engine.Call7)

	// Atomic term processing
	builtins.Register2(engine.NewAtom("atom_length"), engine.AtomLength)
	builtins.Register3(engine.NewAtom("atom_concat"), engine.AtomConcat)
	builtins.Register5(engine.NewAtom("sub_atom"), engine.SubAtom)
	builtins.Register2(engine.NewAtom("atom_chars"), engine.AtomChars)
	builtins.Register2(engine.NewAtom("atom_codes"), engine.AtomCodes)
	builtins.Register2(engine.NewAtom("char_code"), engine.CharCode)
	builtins.Register2(engine.NewAtom("number_chars"), engine.NumberChars)
	builtins.Register2(engine.NewAtom("number_codes"), engine.NumberCodes)

	// Implementation defined hooks
	builtins.Register2(engine.NewAtom("set_prolog_flag"), engine.SetPrologFlag)
	builtins.Register2(engine.NewAtom("current_prolog_flag"), engine.CurrentPrologFlag)
	builtins.Register1(engine.NewAtom("halt"), engine.Halt)

	// Modules
	builtins.Register1(engine.NewAtom("current_module"), engine.CurrentModule)

	// Prolog prologue
	builtins.Register3(engine.NewAtom("append"), engine.Append)
	builtins.Register2(engine.NewAtom("length"), engine.Length)
	builtins.Register3(engine.NewAtom("between"), engine.Between)
	builtins.Register2(engine.NewAtom("succ"), engine.Succ)
	builtins.Register3(engine.NewAtom("nth0"), engine.Nth0)
	builtins.Register3(engine.NewAtom("nth1"), engine.Nth1)
	builtins.Register2(engine.NewAtom("call_nth"), engine.CallNth)

	// Compatibility
	builtins.Register1(engine.NewAtom("consult"), engine.Consult)
	builtins.Register3(engine.NewAtom("use_module"), engine.UseModule)

	// DCGs
	builtins.Register3(engine.NewAtom("phrase"), engine.Phrase)
	builtins.Register2(engine.NewAtom("expand_term"), engine.ExpandTerm)
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
			"/prolog/builtins": builtins,
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
