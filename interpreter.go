package prolog

import (
	"context"
	"embed"
	_ "embed" // for go:embed
	"errors"
	"github.com/ichiban/prolog/builtin"
	"github.com/ichiban/prolog/internal"
	"io"
	"os"
	"strings"
)

//go:embed library
var library embed.FS

// Interpreter is a Prolog interpreter. The zero value is a valid interpreter without any predicates/operators defined.
type Interpreter struct {
	internal.VM
}

// New creates a new Prolog interpreter with predefined fs.FS/predicates.
func New(in io.Reader, out io.Writer) *Interpreter {
	var i Interpreter
	i.FS = OverlayFS{
		library,
		os.DirFS("."),
	}
	i.SetUserInput(internal.NewInputTextStream(in))
	i.SetUserOutput(internal.NewOutputTextStream(out))

	// Directives
	i.SetPredicate1("dynamic", Dynamic)
	i.SetPredicate1("multifile", Multifile)
	i.SetPredicate1("discontiguous", Discontiguous)
	i.SetPredicate1("initialization", Initialization)
	i.SetPredicate1("include", Include)

	// Control constructs
	i.SetPredicate1("call", Call)
	i.SetPredicate3("catch", Catch)
	i.SetPredicate1("throw", Throw)

	// Term unification
	i.SetPredicate2("=", Unify)
	i.SetPredicate2("unify_with_occurs_check", UnifyWithOccursCheck)
	i.SetPredicate2("subsumes_term", SubsumesTerm)

	// Type testing
	i.SetPredicate1("var", TypeVar)
	i.SetPredicate1("atom", TypeAtom)
	i.SetPredicate1("integer", TypeInteger)
	i.SetPredicate1("float", TypeFloat)
	i.SetPredicate1("compound", TypeCompound)
	i.SetPredicate1("acyclic_term", AcyclicTerm)

	// Term comparison
	i.SetPredicate3("compare", Compare)
	i.SetPredicate2("sort", Sort)
	i.SetPredicate2("keysort", KeySort)

	// Term creation and decomposition
	i.SetPredicate3("functor", internal.Functor)
	i.SetPredicate3("arg", Arg)
	i.SetPredicate2("univ", Univ)
	i.SetPredicate2("copy_term", CopyTerm)
	i.SetPredicate2("term_variables", TermVariables)

	// Arithmetic evaluation
	i.SetPredicate2("is", Is)

	// Arithmetic comparison
	i.SetPredicate2("equal", Equal)
	i.SetPredicate2("not_equal", NotEqual)
	i.SetPredicate2("less_than", LessThan)
	i.SetPredicate2("less_than_or_equal", LessThanOrEqual)
	i.SetPredicate2("greater_than", GreaterThan)
	i.SetPredicate2("greater_than_or_equal", GreaterThanOrEqual)

	// Clause retrieval and information
	i.SetPredicate2("clause", Clause)
	i.SetPredicate1("current_predicate", CurrentPredicate)

	// Clause creation and destruction
	i.SetPredicate1("asserta", Asserta)
	i.SetPredicate1("assertz", Assertz)
	i.SetPredicate1("retract", Retract)
	i.SetPredicate1("abolish", Abolish)

	// All solutions
	i.SetPredicate0("create_bag", CreateBag)
	i.SetPredicate1("append_bag", AppendBag)
	i.SetPredicate1("unify_bag", UnifyBag)

	// Stream selection and control
	i.SetPredicate1("current_input", CurrentInput)
	i.SetPredicate1("current_output", CurrentOutput)
	i.SetPredicate1("set_input", SetInput)
	i.SetPredicate1("set_output", SetOutput)
	i.SetPredicate4("open", Open)
	i.SetPredicate2("close", Close)
	i.SetPredicate1("flush_output", FlushOutput)
	i.SetPredicate2("stream_property", StreamProperty)
	i.SetPredicate2("set_stream_position", SetStreamPosition)

	// Character input/output
	i.SetPredicate2("get_char", GetChar)
	i.SetPredicate2("peek_char", PeekChar)
	i.SetPredicate2("put_char", PutChar)

	// Byte input/output
	i.SetPredicate2("get_byte", GetByte)
	i.SetPredicate2("peek_byte", PeekByte)
	i.SetPredicate2("put_byte", PutByte)

	// Term input/output
	i.SetPredicate3("read_term", ReadTerm)
	i.SetPredicate3("write_term", WriteTerm)
	i.SetPredicate3("op", Op)
	i.SetPredicate3("current_op", CurrentOp)
	i.SetPredicate2("char_conversion", CharConversion)
	i.SetPredicate2("current_char_conversion", CurrentCharConversion)

	// Logic and control
	i.SetPredicate1(`not`, Not)
	i.SetPredicate0("repeat", builtin.Repeat)
	i.SetPredicate2("call", Call1)
	i.SetPredicate3("call", Call2)
	i.SetPredicate4("call", Call3)
	i.SetPredicate5("call", Call4)
	i.SetPredicate6("call", Call5)
	i.SetPredicate7("call", Call6)
	i.SetPredicate8("call", Call7)

	// Atomic term processing
	i.SetPredicate2("atom_length", AtomLength)
	i.SetPredicate3("atom_concat", AtomConcat)
	i.SetPredicate5("sub_atom", SubAtom)
	i.SetPredicate2("atom_chars", AtomChars)
	i.SetPredicate2("atom_codes", AtomCodes)
	i.SetPredicate2("char_code", CharCode)
	i.SetPredicate2("number_chars", NumberChars)
	i.SetPredicate2("number_codes", NumberCodes)

	// Implementation defined hooks
	i.SetPredicate2("set_prolog_flag", SetPrologFlag)
	i.SetPredicate2("current_prolog_flag", CurrentPrologFlag)
	i.SetPredicate1("halt", Halt)

	// Definite clause grammar
	i.SetPredicate3("phrase", Phrase)

	// A Prologue for Prolog
	// https://www.complang.tuwien.ac.at/ulrich/iso-prolog/prologue
	i.SetPredicate2("length", Length)
	i.SetPredicate3("between", Between)
	i.SetPredicate2("succ", Succ)
	i.SetPredicate3("nth0", Nth0)
	i.SetPredicate3("nth1", Nth1)
	i.SetPredicate2("call_nth", CallNth)

	// SICStus Prolog compatibility
	i.SetPredicate2("module", DefineModule)
	i.SetPredicate1("meta_predicate", MetaPredicate)
	i.SetPredicate2("load_file", LoadFile)

	if err := i.LoadFile(context.Background(), "library/prolog.pl"); err != nil {
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
	p := internal.NewParser(i.VM.TypeInModule, strings.NewReader(query))
	if err := p.SetPlaceholder("?", args...); err != nil {
		return nil, err
	}

	t, err := p.Term()
	if err != nil {
		return nil, err
	}

	var env *internal.Env

	more := make(chan bool, 1)
	next := make(chan *internal.Env)
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
		if _, err := Call(&i.VM, t, func(env *internal.Env) *internal.Promise {
			next <- env
			return internal.Bool(!<-more)
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
	n := internal.NewAtom(name)
	i.VM.SetModule(n)
}

// SetSystemModule sets the system module.
func (i *Interpreter) SetSystemModule(name string) {
	n := internal.NewAtom(name)
	i.VM.SetSystemModule(n)
}

const moduleNameNative = "native"

// SetPredicate0 registers a native predicate of arity 0.
func (i *Interpreter) SetPredicate0(name string, p Predicate0) {
	m := i.Module(moduleNameNative)
	m.SetPredicate0(name, p)
}

// SetPredicate1 registers a native predicate of arity 1.
func (i *Interpreter) SetPredicate1(name string, p Predicate1) {
	m := i.Module(moduleNameNative)
	m.SetPredicate1(name, p)
}

// SetPredicate2 registers a native predicate of arity 2.
func (i *Interpreter) SetPredicate2(name string, p Predicate2) {
	m := i.Module(moduleNameNative)
	m.SetPredicate2(name, p)
}

// SetPredicate3 registers a native predicate of arity 3.
func (i *Interpreter) SetPredicate3(name string, p Predicate3) {
	m := i.Module(moduleNameNative)
	m.SetPredicate3(name, p)
}

// SetPredicate4 registers a native predicate of arity 4.
func (i *Interpreter) SetPredicate4(name string, p Predicate4) {
	m := i.Module(moduleNameNative)
	m.SetPredicate4(name, p)
}

// SetPredicate5 registers a native predicate of arity 5.
func (i *Interpreter) SetPredicate5(name string, p Predicate5) {
	m := i.Module(moduleNameNative)
	m.SetPredicate5(name, p)
}

// SetPredicate6 registers a native predicate of arity 6.
func (i *Interpreter) SetPredicate6(name string, p Predicate6) {
	m := i.Module(moduleNameNative)
	m.SetPredicate6(name, p)
}

// SetPredicate7 registers a native predicate of arity 7.
func (i *Interpreter) SetPredicate7(name string, p Predicate7) {
	m := i.Module(moduleNameNative)
	m.SetPredicate7(name, p)
}

// SetPredicate8 registers a native predicate of arity 8.
func (i *Interpreter) SetPredicate8(name string, p Predicate8) {
	m := i.Module(moduleNameNative)
	m.SetPredicate8(name, p)
}
