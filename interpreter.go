package prolog

import (
	"context"
	_ "embed" // for go:embed
	"errors"
	"github.com/ichiban/prolog/engine"
	"io"
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
	i.FS = RealFS{}
	i.SetUserInput(engine.NewInputTextStream(in))
	i.SetUserOutput(engine.NewOutputTextStream(out))

	m := i.Module()

	// Directives
	m.Register1("dynamic", engine.Dynamic)
	m.Register1("multifile", engine.Multifile)
	m.Register1("discontiguous", engine.Discontiguous)
	m.Register1("initialization", engine.Initialization)
	m.Register1("include", engine.Include)
	m.Register1("ensure_loaded", engine.EnsureLoaded)

	// Control constructs
	m.Register1("call", engine.Call)
	m.Register3("catch", engine.Catch)
	m.Register1("throw", engine.Throw)

	// Term unification
	m.Register2("=", engine.Unify)
	m.Register2("unify_with_occurs_check", engine.UnifyWithOccursCheck)
	m.Register2("subsumes_term", engine.SubsumesTerm)

	// Type testing
	m.Register1("var", engine.TypeVar)
	m.Register1("atom", engine.TypeAtom)
	m.Register1("integer", engine.TypeInteger)
	m.Register1("float", engine.TypeFloat)
	m.Register1("compound", engine.TypeCompound)
	m.Register1("acyclic_term", engine.AcyclicTerm)

	// Term comparison
	m.Register3("compare", engine.Compare)
	m.Register2("sort", engine.Sort)
	m.Register2("keysort", engine.KeySort)

	// Term creation and decomposition
	m.Register3("functor", engine.Functor)
	m.Register3("arg", engine.Arg)
	m.Register2("=..", engine.Univ)
	m.Register2("copy_term", engine.CopyTerm)
	m.Register2("term_variables", engine.TermVariables)

	// Arithmetic evaluation
	m.Register2("is", engine.Is)

	// Arithmetic comparison
	m.Register2("=:=", engine.Equal)
	m.Register2("=\\=", engine.NotEqual)
	m.Register2("<", engine.LessThan)
	m.Register2("=<", engine.LessThanOrEqual)
	m.Register2(">", engine.GreaterThan)
	m.Register2(">=", engine.GreaterThanOrEqual)

	// Clause retrieval and information
	m.Register2("clause", engine.Clause)
	m.Register1("current_predicate", engine.CurrentPredicate)

	// Clause creation and destruction
	m.Register1("asserta", engine.Asserta)
	m.Register1("assertz", engine.Assertz)
	m.Register1("retract", engine.Retract)
	m.Register1("abolish", engine.Abolish)

	// All solutions
	m.Register3("findall", engine.FindAll)
	m.Register3("bagof", engine.BagOf)
	m.Register3("setof", engine.SetOf)

	// Stream selection and control
	m.Register1("current_input", engine.CurrentInput)
	m.Register1("current_output", engine.CurrentOutput)
	m.Register1("set_input", engine.SetInput)
	m.Register1("set_output", engine.SetOutput)
	m.Register4("open", engine.Open)
	m.Register2("close", engine.Close)
	m.Register1("flush_output", engine.FlushOutput)
	m.Register2("stream_property", engine.StreamProperty)
	m.Register2("set_stream_position", engine.SetStreamPosition)

	// Character input/output
	m.Register2("get_char", engine.GetChar)
	m.Register2("peek_char", engine.PeekChar)
	m.Register2("put_char", engine.PutChar)

	// Byte input/output
	m.Register2("get_byte", engine.GetByte)
	m.Register2("peek_byte", engine.PeekByte)
	m.Register2("put_byte", engine.PutByte)

	// Term input/output
	m.Register3("read_term", engine.ReadTerm)
	m.Register3("write_term", engine.WriteTerm)
	m.Register3("op", engine.Op)
	m.Register3("current_op", engine.CurrentOp)
	m.Register2("char_conversion", engine.CharConversion)
	m.Register2("current_char_conversion", engine.CurrentCharConversion)

	// Logic and control
	m.Register1(`\+`, engine.Negate)
	m.Register0("repeat", engine.Repeat)
	m.Register2("call", engine.Call1)
	m.Register3("call", engine.Call2)
	m.Register4("call", engine.Call3)
	m.Register5("call", engine.Call4)
	m.Register6("call", engine.Call5)
	m.Register7("call", engine.Call6)
	m.Register8("call", engine.Call7)

	// Atomic term processing
	m.Register2("atom_length", engine.AtomLength)
	m.Register3("atom_concat", engine.AtomConcat)
	m.Register5("sub_atom", engine.SubAtom)
	m.Register2("atom_chars", engine.AtomChars)
	m.Register2("atom_codes", engine.AtomCodes)
	m.Register2("char_code", engine.CharCode)
	m.Register2("number_chars", engine.NumberChars)
	m.Register2("number_codes", engine.NumberCodes)

	// Implementation defined hooks
	m.Register2("set_prolog_flag", engine.SetPrologFlag)
	m.Register2("current_prolog_flag", engine.CurrentPrologFlag)
	m.Register1("halt", engine.Halt)

	// Consult
	m.Register1("consult", engine.Consult)

	// Definite clause grammar
	m.Register3("phrase", engine.Phrase)
	m.Register2("expand_term", engine.ExpandTerm)

	// Prolog prologue
	m.Register3("append", engine.Append)
	m.Register2("length", engine.Length)
	m.Register3("between", engine.Between)
	m.Register2("succ", engine.Succ)
	m.Register3("nth0", engine.Nth0)
	m.Register3("nth1", engine.Nth1)
	m.Register2("call_nth", engine.CallNth)

	_ = i.Compile(context.Background(), bootstrap)

	return &i
}

// Query executes a prolog query and returns *Solutions.
func (i *Interpreter) Query(query string, args ...interface{}) (*Solutions, error) {
	return i.QueryContext(context.Background(), query, args...)
}

// QueryContext executes a prolog query and returns *Solutions with context.
func (i *Interpreter) QueryContext(ctx context.Context, query string, args ...interface{}) (*Solutions, error) {
	p := engine.NewParser(i.VM.Module(), strings.NewReader(query))
	if err := p.SetPlaceholder("?", args...); err != nil {
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
