package prolog

import (
	"context"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"iter"
	"slices"
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

// Result is a generic result map. It contains variable names as keys and associated terms as values.
type Result map[string]Raw

type InterpreterOptions struct {
	heapSize     int32
	tempHeapSize int32
	streamSize   int32
	warn         func(error)
	halt         func(code int)
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

func StreamSize(streamSize int32) InterpreterOption {
	return func(o *InterpreterOptions) {
		o.streamSize = streamSize
	}
}

func Warn(fn func(error)) InterpreterOption {
	return func(o *InterpreterOptions) {
		o.warn = fn
	}
}

func Halt(fn func(code int)) InterpreterOption {
	return func(o *InterpreterOptions) {
		o.halt = fn
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
		streamSize:   32,
	}
	for _, o := range opts {
		o(&opt)
	}
	return &Interpreter{
		engine: runtime.Engine{
			Arena: &term.Arena{
				Heap:    make(term.Heap, 0, opt.heapSize),
				Streams: make([]term.Stream, 0, opt.streamSize),
			},
			TempArena: &term.Arena{
				Heap: make(term.Heap, 0, opt.tempHeapSize),
			},
			BuiltinSet: runtime.NewBuiltinSet(),
			Ops:        *syntax.NewOperatorSet(),
			DB:         &db.MemoryDB{},
			Warn:       opt.warn,
			Halt:       opt.halt,
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

// Register0 registers fn as the custom predicate name/0.
// See [Interpreter.Register1] for details.
func (i *Interpreter) Register0(name string, fn func(ctx context.Context, e Execution) Outcome) error {
	return i.register(name, 0, runtime.Predicate0(func(ctx context.Context, e *runtime.Execution, cont term.Handle) runtime.Promise {
		return fn(ctx, Execution{execution: e, cont: cont}).promise
	}))
}

// Register1 registers fn as the custom predicate name/1.
// fn receives the goal's argument as a [Term], which may be bound or unbound depending on how it's called.
// Register1 must be called before the first [Interpreter.Load] or [Query] or it'll return an error.
// Also, it returns an error if name/1 is already taken.
func (i *Interpreter) Register1(name string, fn func(ctx context.Context, e Execution, arg1 Term) Outcome) error {
	return i.register(name, 1, runtime.Predicate1(func(ctx context.Context, e *runtime.Execution, arg1, cont term.Handle) runtime.Promise {
		return fn(ctx, Execution{execution: e, cont: cont}, Term{handle: arg1}).promise
	}))
}

// Register2 registers fn as the custom predicate name/2.
// See [Interpreter.Register1] for details.
func (i *Interpreter) Register2(name string, fn func(ctx context.Context, e Execution, arg1, arg2 Term) Outcome) error {
	return i.register(name, 2, runtime.Predicate2(func(ctx context.Context, e *runtime.Execution, arg1, arg2, cont term.Handle) runtime.Promise {
		return fn(ctx, Execution{execution: e, cont: cont}, Term{handle: arg1}, Term{handle: arg2}).promise
	}))
}

// Register3 registers fn as the custom predicate name/3.
// See [Interpreter.Register1] for details.
func (i *Interpreter) Register3(name string, fn func(ctx context.Context, e Execution, arg1, arg2, arg3 Term) Outcome) error {
	return i.register(name, 3, runtime.Predicate3(func(ctx context.Context, e *runtime.Execution, arg1, arg2, arg3, cont term.Handle) runtime.Promise {
		return fn(ctx, Execution{execution: e, cont: cont}, Term{handle: arg1}, Term{handle: arg2}, Term{handle: arg3}).promise
	}))
}

// Register4 registers fn as the custom predicate name/4.
// See [Interpreter.Register1] for details.
func (i *Interpreter) Register4(name string, fn func(ctx context.Context, e Execution, arg1, arg2, arg3, arg4 Term) Outcome) error {
	return i.register(name, 4, runtime.Predicate4(func(ctx context.Context, e *runtime.Execution, arg1, arg2, arg3, arg4, cont term.Handle) runtime.Promise {
		return fn(ctx, Execution{execution: e, cont: cont}, Term{handle: arg1}, Term{handle: arg2}, Term{handle: arg3}, Term{handle: arg4}).promise
	}))
}

// Register5 registers fn as the custom predicate name/5.
// See [Interpreter.Register1] for details.
func (i *Interpreter) Register5(name string, fn func(ctx context.Context, e Execution, arg1, arg2, arg3, arg4, arg5 Term) Outcome) error {
	return i.register(name, 5, runtime.Predicate5(func(ctx context.Context, e *runtime.Execution, arg1, arg2, arg3, arg4, arg5, cont term.Handle) runtime.Promise {
		return fn(ctx, Execution{execution: e, cont: cont}, Term{handle: arg1}, Term{handle: arg2}, Term{handle: arg3}, Term{handle: arg4}, Term{handle: arg5}).promise
	}))
}

// Register6 registers fn as the custom predicate name/6.
// See [Interpreter.Register1] for details.
func (i *Interpreter) Register6(name string, fn func(ctx context.Context, e Execution, arg1, arg2, arg3, arg4, arg5, arg6 Term) Outcome) error {
	return i.register(name, 6, runtime.Predicate6(func(ctx context.Context, e *runtime.Execution, arg1, arg2, arg3, arg4, arg5, arg6, cont term.Handle) runtime.Promise {
		return fn(ctx, Execution{execution: e, cont: cont}, Term{handle: arg1}, Term{handle: arg2}, Term{handle: arg3}, Term{handle: arg4}, Term{handle: arg5}, Term{handle: arg6}).promise
	}))
}

// Register7 registers fn as the custom predicate name/7.
// See [Interpreter.Register1] for details.
func (i *Interpreter) Register7(name string, fn func(ctx context.Context, e Execution, arg1, arg2, arg3, arg4, arg5, arg6, arg7 Term) Outcome) error {
	return i.register(name, 7, runtime.Predicate7(func(ctx context.Context, e *runtime.Execution, arg1, arg2, arg3, arg4, arg5, arg6, arg7, cont term.Handle) runtime.Promise {
		return fn(ctx, Execution{execution: e, cont: cont}, Term{handle: arg1}, Term{handle: arg2}, Term{handle: arg3}, Term{handle: arg4}, Term{handle: arg5}, Term{handle: arg6}, Term{handle: arg7}).promise
	}))
}

// Register8 registers fn as the custom predicate name/8.
// See [Interpreter.Register1] for details.
func (i *Interpreter) Register8(name string, fn func(ctx context.Context, e Execution, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8 Term) Outcome) error {
	return i.register(name, 8, runtime.Predicate8(func(ctx context.Context, e *runtime.Execution, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, cont term.Handle) runtime.Promise {
		return fn(ctx, Execution{execution: e, cont: cont}, Term{handle: arg1}, Term{handle: arg2}, Term{handle: arg3}, Term{handle: arg4}, Term{handle: arg5}, Term{handle: arg6}, Term{handle: arg7}, Term{handle: arg8}).promise
	}))
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
func Query[T any](ctx context.Context, i *Interpreter, query string, opts ...QueryOption) iter.Seq2[T, error] {
	// FIXME: iter.Seq2[T, error] is a code smell since the error is not an element of the sequence but the error of the sequence itself.
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

func (i *Interpreter) decodeResult(out any, varNames []term.VariableName) error {
	switch out := out.(type) {
	case *Result:
		if *out == nil {
			*out = Result{}
		}
		for _, vn := range varNames {
			if vn.Name == "_" {
				continue
			}
			t := vn.Variable
			t = i.engine.Deref(t)
			if _, ok := i.engine.Variable(t); ok {
				if t == vn.Variable {
					continue
				}
				if i := slices.IndexFunc(varNames, func(vn term.VariableName) bool {
					return vn.Variable == t
				}); i < 0 {
					continue
				}
			}
			(*out)[vn.Name] = Raw(fmt.Sprintf("%s", &syntax.Formatter{
				Arena:         i.engine.Arena,
				Term:          t,
				VariableNames: varNames,
				Quoted:        true,
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
	return Raw(fmt.Sprintf("%s", &syntax.Formatter{Arena: i.engine.Arena, Term: t, Quoted: true}))
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
		return syntax.ParseTerm(strings.NewReader(string(v) + " ."))
	default:
		return term.Handle{}, fmt.Errorf("unknown type: %T", v)
	}
}
