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
	i.Register0(engine.NewAtom("repeat"), engine.Repeat)
	i.Register1(engine.NewAtom(`\+`), engine.Negation)
	i.Register1(engine.NewAtom("call"), engine.Call)
	i.Register2(engine.NewAtom("call"), engine.Call1)
	i.Register3(engine.NewAtom("call"), engine.Call2)
	i.Register4(engine.NewAtom("call"), engine.Call3)
	i.Register5(engine.NewAtom("call"), engine.Call4)
	i.Register6(engine.NewAtom("call"), engine.Call5)
	i.Register7(engine.NewAtom("call"), engine.Call6)
	i.Register8(engine.NewAtom("call"), engine.Call7)
	i.Register1(engine.NewAtom("current_predicate"), engine.CurrentPredicate)
	i.Register1(engine.NewAtom("assertz"), engine.Assertz)
	i.Register1(engine.NewAtom("asserta"), engine.Asserta)
	i.Register1(engine.NewAtom("retract"), engine.Retract)
	i.Register1(engine.NewAtom("abolish"), engine.Abolish)
	i.Register1(engine.NewAtom("var"), engine.TypeVar)
	i.Register1(engine.NewAtom("float"), engine.TypeFloat)
	i.Register1(engine.NewAtom("integer"), engine.TypeInteger)
	i.Register1(engine.NewAtom("atom"), engine.TypeAtom)
	i.Register1(engine.NewAtom("compound"), engine.TypeCompound)
	i.Register1(engine.NewAtom("acyclic_term"), engine.AcyclicTerm)
	i.Register1(engine.NewAtom("throw"), engine.Throw)
	i.Register2(engine.NewAtom("="), engine.Unify)
	i.Register2(engine.NewAtom("unify_with_occurs_check"), engine.UnifyWithOccursCheck)
	i.Register2(engine.NewAtom("subsumes_term"), engine.SubsumesTerm)
	i.Register2(engine.NewAtom("=.."), engine.Univ)
	i.Register2(engine.NewAtom("copy_term"), engine.CopyTerm)
	i.Register2(engine.NewAtom("term_variables"), engine.TermVariables)
	i.Register3(engine.NewAtom("arg"), engine.Arg)
	i.Register3(engine.NewAtom("bagof"), engine.BagOf)
	i.Register3(engine.NewAtom("setof"), engine.SetOf)
	i.Register3(engine.NewAtom("findall"), engine.FindAll)
	i.Register3(engine.NewAtom("catch"), engine.Catch)
	i.Register3(engine.NewAtom("functor"), engine.Functor)
	i.Register3(engine.NewAtom("op"), engine.Op)
	i.Register3(engine.NewAtom("compare"), engine.Compare)
	i.Register3(engine.NewAtom("between"), engine.Between)
	i.Register2(engine.NewAtom("sort"), engine.Sort)
	i.Register2(engine.NewAtom("keysort"), engine.KeySort)
	i.Register3(engine.NewAtom("current_op"), engine.CurrentOp)
	i.Register1(engine.NewAtom("current_input"), engine.CurrentInput)
	i.Register1(engine.NewAtom("current_output"), engine.CurrentOutput)
	i.Register1(engine.NewAtom("set_input"), engine.SetInput)
	i.Register1(engine.NewAtom("set_output"), engine.SetOutput)
	i.Register4(engine.NewAtom("open"), engine.Open)
	i.Register2(engine.NewAtom("close"), engine.Close)
	i.Register1(engine.NewAtom("flush_output"), engine.FlushOutput)
	i.Register3(engine.NewAtom("write_term"), engine.WriteTerm)
	i.Register2(engine.NewAtom("char_code"), engine.CharCode)
	i.Register2(engine.NewAtom("put_byte"), engine.PutByte)
	i.Register2(engine.NewAtom("put_code"), engine.PutCode)
	i.Register3(engine.NewAtom("read_term"), engine.ReadTerm)
	i.Register2(engine.NewAtom("get_byte"), engine.GetByte)
	i.Register2(engine.NewAtom("get_char"), engine.GetChar)
	i.Register2(engine.NewAtom("peek_byte"), engine.PeekByte)
	i.Register2(engine.NewAtom("peek_char"), engine.PeekChar)
	i.Register1(engine.NewAtom("halt"), engine.Halt)
	i.Register2(engine.NewAtom("clause"), engine.Clause)
	i.Register2(engine.NewAtom("atom_length"), engine.AtomLength)
	i.Register3(engine.NewAtom("atom_concat"), engine.AtomConcat)
	i.Register5(engine.NewAtom("sub_atom"), engine.SubAtom)
	i.Register2(engine.NewAtom("atom_chars"), engine.AtomChars)
	i.Register2(engine.NewAtom("atom_codes"), engine.AtomCodes)
	i.Register2(engine.NewAtom("number_chars"), engine.NumberChars)
	i.Register2(engine.NewAtom("number_codes"), engine.NumberCodes)
	i.Register2(engine.NewAtom("is"), engine.Is)
	i.Register2(engine.NewAtom("=:="), engine.Equal)
	i.Register2(engine.NewAtom("=\\="), engine.NotEqual)
	i.Register2(engine.NewAtom("<"), engine.LessThan)
	i.Register2(engine.NewAtom(">"), engine.GreaterThan)
	i.Register2(engine.NewAtom("=<"), engine.LessThanOrEqual)
	i.Register2(engine.NewAtom(">="), engine.GreaterThanOrEqual)
	i.Register2(engine.NewAtom("stream_property"), engine.StreamProperty)
	i.Register2(engine.NewAtom("set_stream_position"), engine.SetStreamPosition)
	i.Register2(engine.NewAtom("char_conversion"), engine.CharConversion)
	i.Register2(engine.NewAtom("current_char_conversion"), engine.CurrentCharConversion)
	i.Register2(engine.NewAtom("set_prolog_flag"), engine.SetPrologFlag)
	i.Register2(engine.NewAtom("current_prolog_flag"), engine.CurrentPrologFlag)
	i.Register2(engine.NewAtom("expand_term"), engine.ExpandTerm)
	i.Register1(engine.NewAtom("consult"), engine.Consult)
	i.Register2(engine.NewAtom("environ"), engine.Environ)
	i.Register3(engine.NewAtom("phrase"), engine.Phrase)
	i.Register3(engine.NewAtom("nth0"), engine.Nth0)
	i.Register3(engine.NewAtom("nth1"), engine.Nth1)
	i.Register2(engine.NewAtom("succ"), engine.Succ)
	i.Register2(engine.NewAtom("length"), engine.Length)
	i.Register3(engine.NewAtom("append"), engine.Append)
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
	p, err := i.Parse(strings.NewReader(query), nil, args...)
	if err != nil {
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
