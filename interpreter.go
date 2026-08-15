package prolog

import (
	"context"
	"fmt"
	"io"
	"io/fs"
	"iter"
	"os"
	"slices"
	"strings"

	"github.com/ichiban/prolog/v2/internal/db"
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

type InterpreterOptions struct {
	heapSize     int32
	tempHeapSize int32
	streamSize   int32
	root         *os.Root
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

func Root(root *os.Root) InterpreterOption {
	return func(o *InterpreterOptions) {
		o.root = root
	}
}

// Interpreter is a Prolog processor. It loads prolog texts from files and takes queries.
type Interpreter struct {
	engine runtime.Engine
}

// New instantiates an interpreter.
func New(opts ...InterpreterOption) *Interpreter {
	opt := InterpreterOptions{
		heapSize:     4 * 1024,
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
			DB: &db.MemoryDB{},
			FS: runtime.FS{
				Root: opt.root,
			},
		},
	}
}

func (i *Interpreter) MountFS(basePath string, fs fs.FS) {
	i.engine.FS.SourceFSs = append(i.engine.FS.SourceFSs, runtime.SourceFS{
		BasePath: basePath,
		FS:       fs,
	})
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

// Load loads a Prolog text from file via FS in Config.
func (i *Interpreter) Load(ctx context.Context, filename string) error {
	e := &i.engine
	if e.Image.Code == nil {
		if err := e.LoadSystem(ctx); err != nil {
			return err
		}
	}
	return e.LoadFile(ctx, filename)
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
