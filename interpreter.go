package prolog

import (
	"context"
	_ "embed" // for go:embed
	"errors"
	"io"
	"io/fs"
	"os"
	"strings"

	"github.com/ichiban/prolog/engine"
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
	i.Eval = engine.DefaultEval
	i.SetUserInput(in)
	i.SetUserOutput(out)
	i.Register0("repeat", engine.Repeat)
	i.Register1(`\+`, engine.Negation)
	i.Register1("call", engine.Call)
	i.Register2("call", engine.Call1)
	i.Register3("call", engine.Call2)
	i.Register4("call", engine.Call3)
	i.Register5("call", engine.Call4)
	i.Register6("call", engine.Call5)
	i.Register7("call", engine.Call6)
	i.Register8("call", engine.Call7)
	i.Register1("current_predicate", engine.CurrentPredicate)
	i.Register1("assertz", engine.Assertz)
	i.Register1("asserta", engine.Asserta)
	i.Register1("retract", engine.Retract)
	i.Register1("abolish", engine.Abolish)
	i.Register1("var", engine.TypeVar)
	i.Register1("float", engine.TypeFloat)
	i.Register1("integer", engine.TypeInteger)
	i.Register1("atom", engine.TypeAtom)
	i.Register1("compound", engine.TypeCompound)
	i.Register1("acyclic_term", engine.AcyclicTerm)
	i.Register1("throw", engine.Throw)
	i.Register2("=", engine.Unify)
	i.Register2("unify_with_occurs_check", engine.UnifyWithOccursCheck)
	i.Register2("subsumes_term", engine.SubsumesTerm)
	i.Register2("=..", engine.Univ)
	i.Register2("copy_term", engine.CopyTerm)
	i.Register2("term_variables", engine.TermVariables)
	i.Register3("arg", engine.Arg)
	i.Register3("bagof", engine.BagOf)
	i.Register3("setof", engine.SetOf)
	i.Register3("findall", engine.FindAll)
	i.Register3("catch", engine.Catch)
	i.Register3("functor", engine.Functor)
	i.Register3("op", engine.Op)
	i.Register3("compare", engine.Compare)
	i.Register3("between", engine.Between)
	i.Register2("sort", engine.Sort)
	i.Register2("keysort", engine.KeySort)
	i.Register3("current_op", engine.CurrentOp)
	i.Register1("current_input", engine.CurrentInput)
	i.Register1("current_output", engine.CurrentOutput)
	i.Register1("set_input", engine.SetInput)
	i.Register1("set_output", engine.SetOutput)
	i.Register4("open", engine.Open)
	i.Register2("close", engine.Close)
	i.Register1("flush_output", engine.FlushOutput)
	i.Register3("write_term", engine.WriteTerm)
	i.Register2("char_code", engine.CharCode)
	i.Register2("put_byte", engine.PutByte)
	i.Register2("put_code", engine.PutCode)
	i.Register3("read_term", engine.ReadTerm)
	i.Register2("get_byte", engine.GetByte)
	i.Register2("get_char", engine.GetChar)
	i.Register2("peek_byte", engine.PeekByte)
	i.Register2("peek_char", engine.PeekChar)
	i.Register1("halt", engine.Halt)
	i.Register2("clause", engine.Clause)
	i.Register2("atom_length", engine.AtomLength)
	i.Register3("atom_concat", engine.AtomConcat)
	i.Register5("sub_atom", engine.SubAtom)
	i.Register2("atom_chars", engine.AtomChars)
	i.Register2("atom_codes", engine.AtomCodes)
	i.Register2("number_chars", engine.NumberChars)
	i.Register2("number_codes", engine.NumberCodes)
	i.Register2("is", engine.Is)
	i.Register2("=:=", engine.Equal)
	i.Register2("=\\=", engine.NotEqual)
	i.Register2("<", engine.LessThan)
	i.Register2("=<", engine.LessThanOrEqual)
	i.Register2(">", engine.GreaterThan)
	i.Register2(">=", engine.GreaterThanOrEqual)
	i.Register2("stream_property", engine.StreamProperty)
	i.Register2("set_stream_position", engine.SetStreamPosition)
	i.Register2("char_conversion", engine.CharConversion)
	i.Register2("current_char_conversion", engine.CurrentCharConversion)
	i.Register2("set_prolog_flag", engine.SetPrologFlag)
	i.Register2("current_prolog_flag", engine.CurrentPrologFlag)
	i.Register2("expand_term", engine.ExpandTerm)
	i.Register1("consult", engine.Consult)
	i.Register2("environ", engine.Environ)
	i.Register3("phrase", engine.Phrase)
	i.Register3("nth0", engine.Nth0)
	i.Register3("nth1", engine.Nth1)
	i.Register2("succ", engine.Succ)
	i.Register2("length", engine.Length)
	i.Register3("append", engine.Append)
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
	p := i.Parser(strings.NewReader(query), nil)
	if err := p.Replace(engine.NewAtom("?"), args...); err != nil {
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
		vars: env.FreeVariables(t),
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
