package prolog

import (
	"context"
	"embed"
	_ "embed" // for go:embed
	"errors"
	"github.com/ichiban/prolog/engine"
	"io"
	"strings"
	"time"
)

//go:embed libraries
var libraries embed.FS

// Interpreter is a Prolog interpreter. The zero value is a valid interpreter without any predicates/operators defined.
type Interpreter struct {
	engine.VM
}

// New creates a new Prolog interpreter with predefined predicates/operators.
func New(in io.Reader, out io.Writer) *Interpreter {
	var i Interpreter
	i.FS = OverlayFS{
		FixedModTimeFS{FS: libraries, ModTime: time.Now()},
		RealFS{},
	}
	i.SetUserInput(engine.NewInputTextStream(in))
	i.SetUserOutput(engine.NewOutputTextStream(out))

	// Directives
	i.SetPredicate1("dynamic", engine.Dynamic)
	i.SetPredicate1("multifile", engine.Multifile)
	i.SetPredicate1("discontiguous", engine.Discontiguous)
	i.SetPredicate1("initialization", engine.Initialization)
	i.SetPredicate1("include", engine.Include)

	i.SetPredicate2("load_file", engine.LoadFile)

	// Control constructs
	i.SetPredicate1("call", engine.Call)
	i.SetPredicate3("catch", engine.Catch)
	i.SetPredicate1("throw", engine.Throw)

	// Term unification
	i.SetPredicate2("=", engine.Unify)
	i.SetPredicate2("unify_with_occurs_check", engine.UnifyWithOccursCheck)
	i.SetPredicate2("subsumes_term", engine.SubsumesTerm)

	// Type testing
	i.SetPredicate1("var", engine.TypeVar)
	i.SetPredicate1("atom", engine.TypeAtom)
	i.SetPredicate1("integer", engine.TypeInteger)
	i.SetPredicate1("float", engine.TypeFloat)
	i.SetPredicate1("compound", engine.TypeCompound)
	i.SetPredicate1("acyclic_term", engine.AcyclicTerm)

	// Term comparison
	i.SetPredicate3("compare", engine.Compare)
	i.SetPredicate2("sort", engine.Sort)
	i.SetPredicate2("keysort", engine.KeySort)

	// Term creation and decomposition
	i.SetPredicate3("functor", engine.Functor)
	i.SetPredicate3("arg", engine.Arg)
	i.SetPredicate2("univ", engine.Univ)
	i.SetPredicate2("copy_term", engine.CopyTerm)
	i.SetPredicate2("term_variables", engine.TermVariables)

	// Arithmetic evaluation
	i.SetPredicate2("is", engine.Is)

	// Arithmetic comparison
	i.SetPredicate2("equal", engine.Equal)
	i.SetPredicate2("not_equal", engine.NotEqual)
	i.SetPredicate2("less_than", engine.LessThan)
	i.SetPredicate2("less_than_or_equal", engine.LessThanOrEqual)
	i.SetPredicate2("greater_than", engine.GreaterThan)
	i.SetPredicate2("greater_than_or_equal", engine.GreaterThanOrEqual)

	// Clause retrieval and information
	i.SetPredicate2("clause", engine.Clause)
	i.SetPredicate1("current_predicate", engine.CurrentPredicate)

	// Clause creation and destruction
	i.SetPredicate1("asserta", engine.Asserta)
	i.SetPredicate1("assertz", engine.Assertz)
	i.SetPredicate1("retract", engine.Retract)
	i.SetPredicate1("abolish", engine.Abolish)

	// All solutions
	i.SetPredicate3("findall", engine.FindAll)
	i.SetPredicate3("bagof", engine.BagOf)
	i.SetPredicate3("setof", engine.SetOf)

	// Stream selection and control
	i.SetPredicate1("current_input", engine.CurrentInput)
	i.SetPredicate1("current_output", engine.CurrentOutput)
	i.SetPredicate1("set_input", engine.SetInput)
	i.SetPredicate1("set_output", engine.SetOutput)
	i.SetPredicate4("open", engine.Open)
	i.SetPredicate2("close", engine.Close)
	i.SetPredicate1("flush_output", engine.FlushOutput)
	i.SetPredicate2("stream_property", engine.StreamProperty)
	i.SetPredicate2("set_stream_position", engine.SetStreamPosition)

	// Character input/output
	i.SetPredicate2("get_char", engine.GetChar)
	i.SetPredicate2("peek_char", engine.PeekChar)
	i.SetPredicate2("put_char", engine.PutChar)

	// Byte input/output
	i.SetPredicate2("get_byte", engine.GetByte)
	i.SetPredicate2("peek_byte", engine.PeekByte)
	i.SetPredicate2("put_byte", engine.PutByte)

	// Term input/output
	i.SetPredicate3("read_term", engine.ReadTerm)
	i.SetPredicate3("write_term", engine.WriteTerm)
	i.SetPredicate3("op", engine.Op)
	i.SetPredicate3("current_op", engine.CurrentOp)
	i.SetPredicate2("char_conversion", engine.CharConversion)
	i.SetPredicate2("current_char_conversion", engine.CurrentCharConversion)

	// Logic and control
	i.SetPredicate1(`not`, engine.Not)
	i.SetPredicate0("repeat", engine.Repeat)
	i.SetPredicate2("call", engine.Call1)
	i.SetPredicate3("call", engine.Call2)
	i.SetPredicate4("call", engine.Call3)
	i.SetPredicate5("call", engine.Call4)
	i.SetPredicate6("call", engine.Call5)
	i.SetPredicate7("call", engine.Call6)
	i.SetPredicate8("call", engine.Call7)

	// Atomic term processing
	i.SetPredicate2("atom_length", engine.AtomLength)
	i.SetPredicate3("atom_concat", engine.AtomConcat)
	i.SetPredicate5("sub_atom", engine.SubAtom)
	i.SetPredicate2("atom_chars", engine.AtomChars)
	i.SetPredicate2("atom_codes", engine.AtomCodes)
	i.SetPredicate2("char_code", engine.CharCode)
	i.SetPredicate2("number_chars", engine.NumberChars)
	i.SetPredicate2("number_codes", engine.NumberCodes)

	// Implementation defined hooks
	i.SetPredicate2("set_prolog_flag", engine.SetPrologFlag)
	i.SetPredicate2("current_prolog_flag", engine.CurrentPrologFlag)
	i.SetPredicate1("halt", engine.Halt)

	// Definite clause grammar
	i.SetPredicate3("phrase", engine.Phrase)

	// A Prologue for Prolog
	// https://www.complang.tuwien.ac.at/ulrich/iso-prolog/prologue
	i.SetPredicate2("length", engine.Length)
	i.SetPredicate3("between", engine.Between)
	i.SetPredicate2("succ", engine.Succ)
	i.SetPredicate3("nth0", engine.Nth0)
	i.SetPredicate3("nth1", engine.Nth1)
	i.SetPredicate2("call_nth", engine.CallNth)

	// SICStus Prolog compatibility
	i.SetPredicate2("module", engine.DefineModule)
	i.SetPredicate1("meta_predicate", engine.MetaPredicate)

	if _, err := i.LoadFile(context.Background(), "libraries/prolog.pl"); err != nil {
		panic(err)
	}

	i.SetModule("user")
	i.SetSystemModule("prolog")

	return &i
}

// Query executes a prolog query and returns *Solutions.
func (i *Interpreter) Query(query string, args ...interface{}) (*Solutions, error) {
	return i.QueryContext(context.Background(), query, args...)
}

// QueryContext executes a prolog query and returns *Solutions with context.
func (i *Interpreter) QueryContext(ctx context.Context, query string, args ...interface{}) (*Solutions, error) {
	p := engine.NewParser(i.VM.TypeInModule, strings.NewReader(query))
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

// SetModule sets the type-in module.
func (i *Interpreter) SetModule(name string) {
	n := engine.NewAtom(name)
	i.VM.SetModule(n)
}

// SetSystemModule sets the system module.
func (i *Interpreter) SetSystemModule(name string) {
	n := engine.NewAtom(name)
	i.VM.SetSystemModule(n)
}

const moduleNameNative = "native"

// SetPredicate0 registers a native predicate of arity 0.
func (i *Interpreter) SetPredicate0(name string, p engine.Predicate0) {
	m := i.Module(moduleNameNative)
	m.Register0(name, p)
}

// SetPredicate1 registers a native predicate of arity 1.
func (i *Interpreter) SetPredicate1(name string, p engine.Predicate1) {
	m := i.Module(moduleNameNative)
	m.Register1(name, p)
}

// SetPredicate2 registers a native predicate of arity 2.
func (i *Interpreter) SetPredicate2(name string, p engine.Predicate2) {
	m := i.Module(moduleNameNative)
	m.Register2(name, p)
}

// SetPredicate3 registers a native predicate of arity 3.
func (i *Interpreter) SetPredicate3(name string, p engine.Predicate3) {
	m := i.Module(moduleNameNative)
	m.Register3(name, p)
}

// SetPredicate4 registers a native predicate of arity 4.
func (i *Interpreter) SetPredicate4(name string, p engine.Predicate4) {
	m := i.Module(moduleNameNative)
	m.Register4(name, p)
}

// SetPredicate5 registers a native predicate of arity 5.
func (i *Interpreter) SetPredicate5(name string, p engine.Predicate5) {
	m := i.Module(moduleNameNative)
	m.Register5(name, p)
}

// SetPredicate6 registers a native predicate of arity 6.
func (i *Interpreter) SetPredicate6(name string, p engine.Predicate6) {
	m := i.Module(moduleNameNative)
	m.Register6(name, p)
}

// SetPredicate7 registers a native predicate of arity 7.
func (i *Interpreter) SetPredicate7(name string, p engine.Predicate7) {
	m := i.Module(moduleNameNative)
	m.Register7(name, p)
}

// SetPredicate8 registers a native predicate of arity 8.
func (i *Interpreter) SetPredicate8(name string, p engine.Predicate8) {
	m := i.Module(moduleNameNative)
	m.Register8(name, p)
}
