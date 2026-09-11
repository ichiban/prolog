package prolog

import (
	"context"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"iter"
	"strings"

	"github.com/ichiban/prolog/v2/internal/db"
	"github.com/ichiban/prolog/v2/internal/runtime"
	"github.com/ichiban/prolog/v2/internal/syntax"
	"github.com/ichiban/prolog/v2/internal/term"
)

// Value is a Go type that can be converted into/from a Prolog type which is either:
// - Atom, as an atom,
// - int64, as an integer,
// - float64, as a float,
// - string, as a char list, or
// - Raw, as an arbitrary term
type Value any

// Atom is a type to annotate the given string represents an atom, not an actual string which is a list of single-character atoms.
// Type conversion between Go and Prolog respects this annotation.
type Atom string

// Raw is a type to annotate the given string represents a term, not an atom nor string.
type Raw string

type InterpreterOptions struct {
	heapSize     int32
	tempHeapSize int32
}

type InterpreterOption func(*InterpreterOptions)

func HeapSize(heapSize int32) InterpreterOption {
	return func(o *InterpreterOptions) {
		o.heapSize = heapSize
	}
}

func TempHeapSize(tempHeapSize int32) InterpreterOption {
	return func(o *InterpreterOptions) {
		o.tempHeapSize = tempHeapSize
	}
}

// Interpreter is a Prolog processor. It loads prolog texts from files and takes queries.
type Interpreter struct {
	engine runtime.Engine
}

// New instantiates an interpreter.
func New(opts ...InterpreterOption) *Interpreter {
	opt := InterpreterOptions{
		heapSize:     8 * 1024,
		tempHeapSize: 1024,
	}
	for _, o := range opts {
		o(&opt)
	}
	return &Interpreter{
		engine: runtime.Engine{
			Arena:      term.NewArena(int(opt.heapSize)),
			TempArena:  term.NewArena(int(opt.tempHeapSize)),
			BuiltinSet: runtime.NewBuiltinSet(),
			Ops:        *syntax.NewOperatorSet(),
			DB:         &db.MemoryDB{},
		},
	}
}

func (i *Interpreter) MountFS(name string, fs fs.FS) error {
	var fsID term.Atom
	if name != "" {
		fsID = term.NewAtom(name)
	}
	return i.engine.FSs.Put(fsID, fs)
}

func (i *Interpreter) SetUserInput(r io.Reader) error {
	s, err := i.engine.PutStream(term.Stream{
		Source:     r,
		Mode:       term.Read,
		Alias:      term.NewAtom("user_input"),
		StreamType: term.Text,
	})
	if err != nil {
		return err
	}
	i.engine.Input = s
	return nil
}

func (i *Interpreter) SetUserOutput(w io.Writer) error {
	s, err := i.engine.PutStream(term.Stream{
		Sink:       w,
		Mode:       term.Write,
		Alias:      term.NewAtom("user_output"),
		StreamType: term.Text,
	})
	if err != nil {
		return err
	}
	i.engine.Output = s
	return nil
}

// SetWarn sets the handler called when the interpreter finds a problem that doesn't stop it from continuing but is worth reporting.
func (i *Interpreter) SetWarn(warn func(err error)) {
	i.engine.Warn = warn
}

// SetHalt sets the handler called when a program executes halt/0 or halt/1, with the exit code.
func (i *Interpreter) SetHalt(halt func(code int)) {
	i.engine.Halt = halt
}

// register adds fn to the builtin set as name/arity. The functor stored in the
// builtin set is binarized (arity+1) to carry the continuation, which is an
// implementation detail; errors reported here use the arity the caller passed.
func (i *Interpreter) register(name string, arity int, proc runtime.Procedure) error {
	// The builtin set is compiled into the image by LoadSystem, so a later
	// registration would never be reachable from Prolog.
	if i.engine.Code != nil {
		return fmt.Errorf("register %s/%d: predicates must be registered before the first Load or Query", name, arity)
	}

	err := i.engine.BuiltinSet.Put(runtime.Builtin{
		PI:   term.NewFunctor(term.NewAtom(name), arity+1),
		Type: runtime.InHead,
		Proc: proc,
	})
	var dup *runtime.DuplicateBuiltinError
	if errors.As(err, &dup) {
		return fmt.Errorf("duplicate predicate: %s/%d", name, arity)
	}
	return err
}

// Predicate constrains the function type [Interpreter.Register] can take.
type Predicate interface {
	func(ctx context.Context, a Activation) Outcome |
		func(ctx context.Context, a Activation, arg1 Term) Outcome |
		func(ctx context.Context, a Activation, arg1, arg2 Term) Outcome |
		func(ctx context.Context, a Activation, arg1, arg2, arg3 Term) Outcome |
		func(ctx context.Context, a Activation, arg1, arg2, arg3, arg4 Term) Outcome |
		func(ctx context.Context, a Activation, arg1, arg2, arg3, arg4, arg5 Term) Outcome |
		func(ctx context.Context, a Activation, arg1, arg2, arg3, arg4, arg5, arg6 Term) Outcome |
		func(ctx context.Context, a Activation, arg1, arg2, arg3, arg4, arg5, arg6, arg7 Term) Outcome |
		func(ctx context.Context, a Activation, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8 Term) Outcome
}

// Register registers fn as the custom predicate name/arity, where the arity is the number of [Term] arguments fn takes, up to 8.
// fn receives the goal's arguments as [Term]s, which may be bound or unbound depending on how it's called.
// Register must be called before the first [Interpreter.Load] or [Interpreter.Query] or it'll return an error.
// Also, it returns an error if name/arity is already taken.
func (i *Interpreter) Register[T Predicate](name string, fn T) error {
	switch fn := any(fn).(type) {
	case func(ctx context.Context, a Activation) Outcome:
		return i.register(name, 0, runtime.Nondeterministic0(func(ctx context.Context, a *runtime.Activation, cont runtime.Ref) runtime.Promise {
			return fn(ctx, Activation{activation: a, cont: cont}).promise
		}))
	case func(ctx context.Context, a Activation, arg1 Term) Outcome:
		return i.register(name, 1, runtime.Nondeterministic1(func(ctx context.Context, a *runtime.Activation, arg1, cont runtime.Ref) runtime.Promise {
			return fn(ctx, Activation{activation: a, cont: cont}, Term{ref: arg1}).promise
		}))
	case func(ctx context.Context, a Activation, arg1, arg2 Term) Outcome:
		return i.register(name, 2, runtime.Nondeterministic2(func(ctx context.Context, a *runtime.Activation, arg1, arg2, cont runtime.Ref) runtime.Promise {
			return fn(ctx, Activation{activation: a, cont: cont}, Term{ref: arg1}, Term{ref: arg2}).promise
		}))
	case func(ctx context.Context, a Activation, arg1, arg2, arg3 Term) Outcome:
		return i.register(name, 3, runtime.Nondeterministic3(func(ctx context.Context, a *runtime.Activation, arg1, arg2, arg3, cont runtime.Ref) runtime.Promise {
			return fn(ctx, Activation{activation: a, cont: cont}, Term{ref: arg1}, Term{ref: arg2}, Term{ref: arg3}).promise
		}))
	case func(ctx context.Context, a Activation, arg1, arg2, arg3, arg4 Term) Outcome:
		return i.register(name, 4, runtime.Nondeterministic4(func(ctx context.Context, a *runtime.Activation, arg1, arg2, arg3, arg4, cont runtime.Ref) runtime.Promise {
			return fn(ctx, Activation{activation: a, cont: cont}, Term{ref: arg1}, Term{ref: arg2}, Term{ref: arg3}, Term{ref: arg4}).promise
		}))
	case func(ctx context.Context, a Activation, arg1, arg2, arg3, arg4, arg5 Term) Outcome:
		return i.register(name, 5, runtime.Nondeterministic5(func(ctx context.Context, a *runtime.Activation, arg1, arg2, arg3, arg4, arg5, cont runtime.Ref) runtime.Promise {
			return fn(ctx, Activation{activation: a, cont: cont}, Term{ref: arg1}, Term{ref: arg2}, Term{ref: arg3}, Term{ref: arg4}, Term{ref: arg5}).promise
		}))
	case func(ctx context.Context, a Activation, arg1, arg2, arg3, arg4, arg5, arg6 Term) Outcome:
		return i.register(name, 6, runtime.Nondeterministic6(func(ctx context.Context, a *runtime.Activation, arg1, arg2, arg3, arg4, arg5, arg6, cont runtime.Ref) runtime.Promise {
			return fn(ctx, Activation{activation: a, cont: cont}, Term{ref: arg1}, Term{ref: arg2}, Term{ref: arg3}, Term{ref: arg4}, Term{ref: arg5}, Term{ref: arg6}).promise
		}))
	case func(ctx context.Context, a Activation, arg1, arg2, arg3, arg4, arg5, arg6, arg7 Term) Outcome:
		return i.register(name, 7, runtime.Nondeterministic7(func(ctx context.Context, a *runtime.Activation, arg1, arg2, arg3, arg4, arg5, arg6, arg7, cont runtime.Ref) runtime.Promise {
			return fn(ctx, Activation{activation: a, cont: cont}, Term{ref: arg1}, Term{ref: arg2}, Term{ref: arg3}, Term{ref: arg4}, Term{ref: arg5}, Term{ref: arg6}, Term{ref: arg7}).promise
		}))
	case func(ctx context.Context, a Activation, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8 Term) Outcome:
		return i.register(name, 8, runtime.Nondeterministic8(func(ctx context.Context, a *runtime.Activation, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, cont runtime.Ref) runtime.Promise {
			return fn(ctx, Activation{activation: a, cont: cont}, Term{ref: arg1}, Term{ref: arg2}, Term{ref: arg3}, Term{ref: arg4}, Term{ref: arg5}, Term{ref: arg6}, Term{ref: arg7}, Term{ref: arg8}).promise
		}))
	default:
		return errors.New("invalid function type")
	}
}

// Load loads a Prolog text from file via FS in Config.
func (i *Interpreter) Load(ctx context.Context, fsName, filename string) error {
	e := &i.engine
	if e.Image.Code == nil {
		if err := e.LoadSystem(ctx); err != nil {
			return err
		}
	}
	var fsID term.Atom
	if fsName != "" {
		fsID = term.NewAtom(fsName)
	}
	return e.LoadFile(ctx, fsID, filename)
}

type VariableName = term.VariableName

// QueryOptions is a set of options for a query.
type QueryOptions struct {
	bindings      map[string]Value
	variableNames *[]VariableName
}

// QueryOption is a single option for a query.
type QueryOption func(*QueryOptions)

// Bindings sets variable values for a query.
func Bindings(b map[string]Value) QueryOption {
	return func(o *QueryOptions) {
		o.bindings = b
	}
}

func VariableNames(varNames *[]VariableName) QueryOption {
	return func(o *QueryOptions) {
		o.variableNames = varNames
	}
}

// Query queries an interpreter and returns results.
func (i *Interpreter) Query[T any](ctx context.Context, query string, opts ...QueryOption) iter.Seq2[T, error] {
	var options QueryOptions
	for _, o := range opts {
		o(&options)
	}

	if options.variableNames == nil {
		options.variableNames = &[]VariableName{}
	}

	return func(yield func(T, error) bool) {
		var (
			e    = &i.engine
			zero T
		)

		if e.Code == nil {
			if err := e.LoadSystem(ctx); err != nil {
				_ = yield(zero, err)
				return
			}
		}

		// The caller reads each solution through these variables, so they have
		// to survive every collection the query triggers, and to be rewritten
		// when the heap moves under them. The parser appends to the slice, so
		// read it through the pointer rather than capturing it.
		defer e.AddRoots(func(yield func(*term.Cell) bool) {
			for i := range *options.variableNames {
				if !yield(&(*options.variableNames)[i].Variable) {
					return
				}
			}
		})()

		for v, b := range options.bindings {
			v, err := syntax.ParseVariable(strings.NewReader(v),
				syntax.VariableNames(options.variableNames),
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

		g, err := syntax.ParseTerm(strings.NewReader(query),
			syntax.Arena(e.Arena),
			syntax.Operators(&e.Ops),
			syntax.DoubleQuote(&e.DoubleQuotes),
			syntax.CharConv(&e.CharConversion),
			syntax.VariableNames(options.variableNames),
		)
		if err != nil {
			_ = yield(zero, err)
			return
		}

		for err := range i.engine.Call(ctx, g) {
			if err != nil {
				_ = yield(zero, i.wrapError(err, *options.variableNames))
				return
			}

			var (
				t   T
				err = i.decodeResult(&t, *options.variableNames)
			)
			if !yield(t, err) {
				return
			}
		}
	}
}

func (i *Interpreter) wrapError(err error, varNames []term.VariableName) error {
	origErr := err
	errTerm, err := runtime.ErrorTerm(i.engine.Arena, err)
	if err != nil {
		return err
	}
	return fmt.Errorf("%s: %w", &syntax.Formatter{
		Arena:         i.engine.Arena,
		Term:          errTerm,
		VariableNames: varNames,
		Quoted:        true,
	}, origErr)
}

func (i *Interpreter) encodeTerm(v Value) (term.Cell, error) {
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
		return syntax.ParseTerm(strings.NewReader(string(v) + " ."))
	default:
		return term.Cell{}, fmt.Errorf("unknown type: %T", v)
	}
}
