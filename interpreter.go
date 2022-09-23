package prolog

import (
	"context"
	_ "embed" // for go:embed
	"errors"
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/ichiban/prolog/engine"
)

//go:embed bootstrap.pl
var bootstrap string

// Interpreter is a Prolog interpreter. The zero value is a valid interpreter without any predicates/operators defined.
type Interpreter struct {
	engine.State
	loaded map[string]struct{}
}

// New creates a new Prolog interpreter with predefined predicates/operators.
func New(in io.Reader, out io.Writer) *Interpreter {
	var i Interpreter
	i.SetUserInput(in)
	i.SetUserOutput(out)
	i.Register0("repeat", i.Repeat)
	i.Register1(`\+`, i.Negation)
	i.Register1("call", i.Call)
	i.Register2("call", i.Call1)
	i.Register3("call", i.Call2)
	i.Register4("call", i.Call3)
	i.Register5("call", i.Call4)
	i.Register6("call", i.Call5)
	i.Register7("call", i.Call6)
	i.Register8("call", i.Call7)
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
	i.Register1("acyclic_term", engine.AcyclicTerm)
	i.Register1("throw", engine.Throw)
	i.Register2("=", engine.Unify)
	i.Register2("unify_with_occurs_check", engine.UnifyWithOccursCheck)
	i.Register2("subsumes_term", engine.SubsumesTerm)
	i.Register2("=..", engine.Univ)
	i.Register2("copy_term", engine.CopyTerm)
	i.Register2("term_variables", engine.TermVariables)
	i.Register3("arg", engine.Arg)
	i.Register3("bagof", i.BagOf)
	i.Register3("setof", i.SetOf)
	i.Register3("findall", i.FindAll)
	i.Register3("catch", i.Catch)
	i.Register3("functor", engine.Functor)
	i.Register3("op", i.Op)
	i.Register3("compare", engine.Compare)
	i.Register3("between", engine.Between)
	i.Register2("sort", engine.Sort)
	i.Register2("keysort", engine.KeySort)
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
	i.Register2("is", engine.DefaultEvaluableFunctors.Is)
	i.Register2("=:=", engine.DefaultEvaluableFunctors.Equal)
	i.Register2("=\\=", engine.DefaultEvaluableFunctors.NotEqual)
	i.Register2("<", engine.DefaultEvaluableFunctors.LessThan)
	i.Register2(">", engine.DefaultEvaluableFunctors.GreaterThan)
	i.Register2("=<", engine.DefaultEvaluableFunctors.LessThanOrEqual)
	i.Register2(">=", engine.DefaultEvaluableFunctors.GreaterThanOrEqual)
	i.Register2("stream_property", i.StreamProperty)
	i.Register2("set_stream_position", i.SetStreamPosition)
	i.Register2("char_conversion", i.CharConversion)
	i.Register2("current_char_conversion", i.CurrentCharConversion)
	i.Register2("set_prolog_flag", i.SetPrologFlag)
	i.Register2("current_prolog_flag", i.CurrentPrologFlag)
	i.Register1("dynamic", i.Dynamic)
	i.Register1("built_in", i.BuiltIn)
	i.Register2("expand_term", i.ExpandTerm)
	i.Register1("consult", i.Consult)
	i.Register2("environ", engine.Environ)
	i.Register3("phrase", i.Phrase)
	i.Register3("nth0", engine.Nth0)
	i.Register3("nth1", engine.Nth1)
	i.Register2("succ", engine.Succ)
	i.Register2("length", engine.Length)
	i.Register3("append", engine.Append)
	i.Register1("initialization", i.Initialization)
	i.Register1("include", i.Include)
	i.Register1("ensure_loaded", i.EnsureLoaded)
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
	var t text
	ctx = context.WithValue(ctx, textCtxKey{}, &t)
	if err := i.execContext(ctx, query, args...); err != nil {
		return err
	}

	for _, g := range t.goals {
		ok, err := i.Call(g, engine.Success, nil).Force(ctx)
		if err != nil {
			return err
		}
		if !ok {
			var sb strings.Builder
			_ = i.Write(&sb, g, &engine.WriteOptions{Quoted: true}, nil)
			return fmt.Errorf("failed initialization goal: %s", sb.String())
		}
	}

	return nil
}

func (i *Interpreter) execContext(ctx context.Context, query string, args ...interface{}) error {
	query = ignoreShebangLine(query)
	p := i.Parser(strings.NewReader(query), nil)
	if err := p.Replace("?", args...); err != nil {
		return err
	}
	for p.More() {
		t, err := p.Term()
		if err != nil {
			return err
		}

		et, err := i.Expand(t, nil)
		if err != nil {
			return err
		}

		// Directive
		if c, ok := et.(engine.Compound); ok && c.Functor() == ":-" && c.Arity() == 1 {
			d := c.Arg(0)
			ok, err := i.Call(d, engine.Success, nil).Force(ctx)
			if err != nil {
				return err
			}
			if !ok {
				var sb strings.Builder
				_ = i.Write(&sb, d, &engine.WriteOptions{Quoted: true}, nil)
				return fmt.Errorf("failed directive: %s", sb.String())
			}
			continue
		}

		if err := i.Assert(et, nil); err != nil {
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
		if _, err := i.Call(t, func(env *engine.Env) *engine.Promise {
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

// Consult executes Prolog texts in files.
func (i *Interpreter) Consult(files engine.Term, k func(*engine.Env) *engine.Promise, env *engine.Env) *engine.Promise {
	var filenames []engine.Term
	iter := engine.ListIterator{List: files, Env: env}
	for iter.Next() {
		filenames = append(filenames, iter.Current())
	}
	if err := iter.Err(); err != nil {
		filenames = []engine.Term{files}
	}

	return engine.Delay(func(ctx context.Context) *engine.Promise {
		for _, filename := range filenames {
			if err := i.ensureLoaded(ctx, filename, env); err != nil {
				return engine.Error(err)
			}
		}

		return k(env)
	})
}

type text struct {
	filename string
	goals    []engine.Term
}

type textCtxKey struct{}

// Initialization registers the goal for execution right before exiting Exec or ExecContext.
func (i *Interpreter) Initialization(g engine.Term, k func(*engine.Env) *engine.Promise, env *engine.Env) *engine.Promise {
	return engine.Delay(func(ctx context.Context) *engine.Promise {
		t, ok := ctx.Value(textCtxKey{}).(*text)
		if !ok {
			return engine.Bool(false)
		}
		t.goals = append(t.goals, env.Simplify(g))
		return k(env)
	})
}

// EnsureLoaded loads the Prolog text if not loaded.
func (i *Interpreter) EnsureLoaded(file engine.Term, k func(*engine.Env) *engine.Promise, env *engine.Env) *engine.Promise {
	return engine.Delay(func(ctx context.Context) *engine.Promise {
		if err := i.ensureLoaded(ctx, file, env); err != nil {
			return engine.Error(err)
		}
		return k(env)
	})
}

// Include includes the Prolog text from file.
func (i *Interpreter) Include(file engine.Term, k func(*engine.Env) *engine.Promise, env *engine.Env) *engine.Promise {
	_, b, err := open(file, env)
	if err != nil {
		return engine.Error(err)
	}

	return engine.Delay(func(ctx context.Context) *engine.Promise {
		if err := i.execContext(ctx, string(b)); err != nil {
			return engine.Error(err)
		}

		return k(env)
	})
}

func ignoreShebangLine(query string) string {
	if !strings.HasPrefix(query, "#!") {
		return query
	}
	i := strings.Index(query, "\n")
	if i < 0 {
		i = len(query)
	}
	return query[i:]
}

func (i *Interpreter) ensureLoaded(ctx context.Context, file engine.Term, env *engine.Env) error {
	f, b, err := open(file, env)
	if err != nil {
		return err
	}

	if i.loaded == nil {
		i.loaded = map[string]struct{}{}
	}
	if _, ok := i.loaded[f]; ok {
		return nil
	}
	defer func() {
		i.loaded[f] = struct{}{}
	}()

	return i.ExecContext(ctx, string(b))
}

func open(file engine.Term, env *engine.Env) (string, []byte, error) {
	switch f := env.Resolve(file).(type) {
	case engine.Variable:
		return "", nil, engine.InstantiationError(env)
	case engine.Atom:
		for _, f := range []string{string(f), string(f) + ".pl"} {
			b, err := os.ReadFile(f)
			if err != nil {
				continue
			}

			return f, b, nil
		}
		return "", nil, engine.ExistenceError(engine.ObjectTypeSourceSink, file, env)
	default:
		return "", nil, engine.TypeError(engine.ValidTypeAtom, file, env)
	}
}
