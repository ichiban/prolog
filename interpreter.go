package prolog

import (
	"context"
	"fmt"
	"io/fs"
	"iter"

	"github.com/ichiban/prolog/v2/internal/runtime"
	"github.com/ichiban/prolog/v2/internal/syntax"
	"github.com/ichiban/prolog/v2/internal/term"
)

// Value is a Go type that can be converted into/from a Prolog type which is either:
// - Atom, as an atom,
// - int/int8/int16/int32/int64, as an integer,
// - float32/float64, as a float,
// - string, as a char list (by default, can be code list or atom, too), or
// - Raw, as an arbitrary term
type Value any

// Atom is a type to annotate the given string represents an atom, not an actual string which is a list of single-character atoms.
// Type conversion between Go and Prolog respects this annotation.
type Atom string

// Raw is a type to annotate the given string represents a term, not an atom nor string.
type Raw string

// Result is a generic result map. It contains variable names as keys and associated terms as values.
type Result map[string]Raw

// Interpreter is a Prolog processor. It loads prolog texts from files and takes queries.
type Interpreter struct {
	engine runtime.Engine
}

// New instantiates an interpreter.
func New(heapSize int) *Interpreter {
	return &Interpreter{
		engine: runtime.Engine{
			Arena: &term.Arena{
				Heap: make(term.Heap, 0, heapSize),
			},
		},
	}
}

func (i *Interpreter) SetSourceFS(fs fs.FS) {
	i.engine.SourceFS = fs
}

// Load loads a Prolog text from file via SourceFS in Config.
func (i *Interpreter) Load(ctx context.Context, filename string) error {
	e := &i.engine
	if e.Image.Code == nil {
		if err := e.LoadSystem(ctx); err != nil {
			return err
		}
	}
	return e.LoadFile(ctx, filename)
}

type ParsedVariables = []syntax.ParsedVariable

// QueryOptions is a set of options for a query.
type QueryOptions struct {
	bindings map[string]Value
}

// QueryOption is a single option for a query.
type QueryOption func(*QueryOptions)

// Bindings sets variable values for a query.
func Bindings(b map[string]Value) QueryOption {
	return func(o *QueryOptions) {
		o.bindings = b
	}
}

// Query queries an interpreter and returns results.
func Query[T any](ctx context.Context, i *Interpreter, query string, opts ...QueryOption) iter.Seq2[T, error] {
	var options QueryOptions
	for _, o := range opts {
		o(&options)
	}

	return func(yield func(T, error) bool) {
		var (
			e    = &i.engine
			zero T
			pvs  []syntax.ParsedVariable
		)

		if e.Code == nil {
			if err := e.LoadSystem(ctx); err != nil {
				_ = yield(zero, err)
				return
			}
		}

		for v, b := range options.bindings {
			v, err := syntax.ParseVariable(v,
				syntax.Variables(&pvs),
			)
			if err != nil {
				_ = yield(zero, err)
				return
			}
			param, err := i.encodeTerm(b)
			if err != nil {
				_ = yield(zero, err)
				return
			}
			if err := e.Bind(v, param); err != nil {
				_ = yield(zero, err)
				return
			}
		}

		g, err := syntax.ParseTerm(query,
			syntax.Arena(e.Arena),
			syntax.Variables(&pvs),
		)
		if err != nil {
			_ = yield(zero, err)
			return
		}

		varNames := make(map[term.Handle]term.Atom, len(pvs))
		for _, v := range pvs {
			varNames[v.Variable] = term.NewAtom(v.Name)
		}

		for err := range i.engine.Call(ctx, g) {
			if err != nil {
				_ = yield(zero, i.wrapError(err, varNames))
				return
			}

			var (
				t   T
				err = i.decodeResult(&t, varNames)
			)
			if !yield(t, err) {
				return
			}
		}
	}
}

func (i *Interpreter) wrapError(err error, varNames map[term.Handle]term.Atom) error {
	origErr := err
	errTerm, err := runtime.ErrorTerm(i.engine.Arena, err)
	if err != nil {
		return err
	}
	return fmt.Errorf("%s: %w", &syntax.Formatter{
		Arena:        i.engine.Arena,
		Term:         errTerm,
		VariableName: varNames,
	}, origErr)
}

func (i *Interpreter) decodeResult(out any, varNames map[term.Handle]term.Atom) error {
	switch out := out.(type) {
	case *Result:

		if *out == nil {
			*out = Result{}
		}
		for v, name := range varNames {
			t := v
			t = i.engine.Deref(t)
			if _, ok := i.engine.Variable(t); ok {
				if t == v {
					continue
				}
				if _, ok := varNames[t]; !ok {
					continue
				}
			}
			(*out)[name.String()] = Raw(fmt.Sprintf("%s", &syntax.Formatter{
				Arena:        i.engine.Arena,
				Term:         t,
				VariableName: varNames,
			}))
		}
		return nil
	default:
		// TODO: Support structs.
		return fmt.Errorf("unexpected result type: %T", out)
	}
}

func (i *Interpreter) decodeTerm(t term.Handle) Value {
	e := i.engine
	t = e.Deref(t)
	if _, ok := e.Variable(t); ok {
		return nil
	}
	if a, ok := e.Atom(t); ok {
		return Value(Atom(a.String()))
	}
	if n, ok := e.Integer(t); ok {
		return Value(n)
	}
	if f, ok := e.Float(t); ok {
		return Value(f)
	}
	if s, ok := e.CharList(t); ok {
		return Value(s)
	}
	return Raw(fmt.Sprintf("%s", &syntax.Formatter{Arena: i.engine.Arena, Term: t}))
}

func (i *Interpreter) encodeTerm(v Value) (term.Handle, error) {
	e := i.engine
	switch v := v.(type) {
	case Atom:
		return e.PutAtom(term.NewAtom(string(v)))
	case int:
		return e.PutInteger(int64(v))
	case int8:
		return e.PutInteger(int64(v))
	case int16:
		return e.PutInteger(int64(v))
	case int32:
		return e.PutInteger(int64(v))
	case int64:
		return e.PutInteger(v)
	case float32:
		return e.PutFloat(float64(v))
	case float64:
		return e.PutFloat(v)
	case string:
		return e.PutCharList(v)
	case Raw:
		return syntax.ParseTerm(string(v))
	default:
		return term.Handle{}, fmt.Errorf("unknown type: %T", v)
	}
}
