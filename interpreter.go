package prolog

import (
	"context"
	_ "embed" // for go:embed
	"errors"
	"github.com/ichiban/prolog/engine"
	"io"
	"io/fs"
	"os"
	"strings"
)

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
	i.FS = defaultFS{}
	i.SetUserInput(engine.NewInputTextStream(in))
	i.SetUserOutput(engine.NewOutputTextStream(out))

	m := i.Module() // `user` module

	// Control constructs
	m.Register1(engine.NewAtom("call"), engine.Call)
	m.Register3(engine.NewAtom("catch"), engine.Catch)
	m.Register1(engine.NewAtom("throw"), engine.Throw)

	// Term unification
	m.Register2(engine.NewAtom("="), engine.Unify)
	m.Register2(engine.NewAtom("unify_with_occurs_check"), engine.UnifyWithOccursCheck)
	m.Register2(engine.NewAtom("subsumes_term"), engine.SubsumesTerm)

	// Type testing
	m.Register1(engine.NewAtom("var"), engine.TypeVar)
	m.Register1(engine.NewAtom("atom"), engine.TypeAtom)
	m.Register1(engine.NewAtom("integer"), engine.TypeInteger)
	m.Register1(engine.NewAtom("float"), engine.TypeFloat)
	m.Register1(engine.NewAtom("compound"), engine.TypeCompound)
	m.Register1(engine.NewAtom("acyclic_term"), engine.AcyclicTerm)

	// Term comparison
	m.Register3(engine.NewAtom("compare"), engine.Compare)
	m.Register2(engine.NewAtom("sort"), engine.Sort)
	m.Register2(engine.NewAtom("keysort"), engine.KeySort)

	// Term creation and decomposition
	m.Register3(engine.NewAtom("functor"), engine.Functor)
	m.Register3(engine.NewAtom("arg"), engine.Arg)
	m.Register2(engine.NewAtom("=.."), engine.Univ)
	m.Register2(engine.NewAtom("copy_term"), engine.CopyTerm)
	m.Register2(engine.NewAtom("term_variables"), engine.TermVariables)

	// Arithmetic evaluation
	m.Register2(engine.NewAtom("is"), engine.Is)

	// Arithmetic comparison
	m.Register2(engine.NewAtom("=:="), engine.Equal)
	m.Register2(engine.NewAtom("=\\="), engine.NotEqual)
	m.Register2(engine.NewAtom("<"), engine.LessThan)
	m.Register2(engine.NewAtom("=<"), engine.LessThanOrEqual)
	m.Register2(engine.NewAtom(">"), engine.GreaterThan)
	m.Register2(engine.NewAtom(">="), engine.GreaterThanOrEqual)

	// Clause retrieval and information
	m.Register2(engine.NewAtom("clause"), engine.Clause)
	m.Register1(engine.NewAtom("current_predicate"), engine.CurrentPredicate)

	// Clause creation and destruction
	m.Register1(engine.NewAtom("asserta"), engine.Asserta)
	m.Register1(engine.NewAtom("assertz"), engine.Assertz)
	m.Register1(engine.NewAtom("retract"), engine.Retract)
	m.Register1(engine.NewAtom("abolish"), engine.Abolish)

	// All solutions
	m.Register3(engine.NewAtom("findall"), engine.FindAll)
	m.Register3(engine.NewAtom("bagof"), engine.BagOf)
	m.Register3(engine.NewAtom("setof"), engine.SetOf)

	// Stream selection and control
	m.Register1(engine.NewAtom("current_input"), engine.CurrentInput)
	m.Register1(engine.NewAtom("current_output"), engine.CurrentOutput)
	m.Register1(engine.NewAtom("set_input"), engine.SetInput)
	m.Register1(engine.NewAtom("set_output"), engine.SetOutput)
	m.Register4(engine.NewAtom("open"), engine.Open)
	m.Register2(engine.NewAtom("close"), engine.Close)
	m.Register1(engine.NewAtom("flush_output"), engine.FlushOutput)
	m.Register2(engine.NewAtom("stream_property"), engine.StreamProperty)
	m.Register2(engine.NewAtom("set_stream_position"), engine.SetStreamPosition)

	// Character input/output
	m.Register2(engine.NewAtom("get_char"), engine.GetChar)
	m.Register2(engine.NewAtom("peek_char"), engine.PeekChar)
	m.Register2(engine.NewAtom("put_char"), engine.PutChar)

	// Byte input/output
	m.Register2(engine.NewAtom("get_byte"), engine.GetByte)
	m.Register2(engine.NewAtom("peek_byte"), engine.PeekByte)
	m.Register2(engine.NewAtom("put_byte"), engine.PutByte)

	// Term input/output
	m.Register3(engine.NewAtom("read_term"), engine.ReadTerm)
	m.Register3(engine.NewAtom("write_term"), engine.WriteTerm)
	m.Register3(engine.NewAtom("op"), engine.Op)
	m.Register3(engine.NewAtom("current_op"), engine.CurrentOp)
	m.Register2(engine.NewAtom("char_conversion"), engine.CharConversion)
	m.Register2(engine.NewAtom("current_char_conversion"), engine.CurrentCharConversion)

	// Logic and control
	m.Register1(engine.NewAtom(`\+`), engine.Negate)
	m.Register0(engine.NewAtom("repeat"), engine.Repeat)
	m.Register2(engine.NewAtom("call"), engine.Call1)
	m.Register3(engine.NewAtom("call"), engine.Call2)
	m.Register4(engine.NewAtom("call"), engine.Call3)
	m.Register5(engine.NewAtom("call"), engine.Call4)
	m.Register6(engine.NewAtom("call"), engine.Call5)
	m.Register7(engine.NewAtom("call"), engine.Call6)
	m.Register8(engine.NewAtom("call"), engine.Call7)

	// Atomic term processing
	m.Register2(engine.NewAtom("atom_length"), engine.AtomLength)
	m.Register3(engine.NewAtom("atom_concat"), engine.AtomConcat)
	m.Register5(engine.NewAtom("sub_atom"), engine.SubAtom)
	m.Register2(engine.NewAtom("atom_chars"), engine.AtomChars)
	m.Register2(engine.NewAtom("atom_codes"), engine.AtomCodes)
	m.Register2(engine.NewAtom("char_code"), engine.CharCode)
	m.Register2(engine.NewAtom("number_chars"), engine.NumberChars)
	m.Register2(engine.NewAtom("number_codes"), engine.NumberCodes)

	// Implementation defined hooks
	m.Register2(engine.NewAtom("set_prolog_flag"), engine.SetPrologFlag)
	m.Register2(engine.NewAtom("current_prolog_flag"), engine.CurrentPrologFlag)
	m.Register1(engine.NewAtom("halt"), engine.Halt)

	// Consult
	m.Register1(engine.NewAtom("consult"), engine.Consult)

	// Definite clause grammar
	m.Register3(engine.NewAtom("phrase"), engine.Phrase)
	m.Register2(engine.NewAtom("expand_term"), engine.ExpandTerm)

	// Prolog prologue
	m.Register3(engine.NewAtom("append"), engine.Append)
	m.Register2(engine.NewAtom("length"), engine.Length)
	m.Register3(engine.NewAtom("between"), engine.Between)
	m.Register2(engine.NewAtom("succ"), engine.Succ)
	m.Register3(engine.NewAtom("nth0"), engine.Nth0)
	m.Register3(engine.NewAtom("nth1"), engine.Nth1)
	m.Register2(engine.NewAtom("call_nth"), engine.CallNth)

	_ = i.Exec(bootstrap)

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

type defaultFS struct{}

func (d defaultFS) Open(name string) (fs.File, error) {
	return os.Open(name)
}
