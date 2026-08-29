package prolog

import (
	"errors"
	"slices"
	"testing"

	"github.com/ichiban/prolog/v2/internal/runtime"
	"github.com/ichiban/prolog/v2/internal/term"
)

var errVM = errors.New("vm")

// fakeVM is a vm that records the calls it receives and returns canned values,
// so that Execution can be tested without an engine behind it.
type fakeVM struct {
	calls []string // method names, in the order they were called

	// Canned results. err, when set, is returned by whichever MustBe* or Put*
	// method is under test.
	err       error
	charErr   error // returned by MustBeChar, so String's element error is reachable
	atom      term.Atom
	integer   int64
	float     float64
	functor   term.Functor
	char      rune
	listElems int // number of elements MustBeList yields
	unified   bool
	variable  bool

	cont term.Handle // cont passed to Success or Throw
	argN int         // n passed to Arg
	name term.Atom   // name passed to PutCompound
	args int         // number of arguments passed to PutCompound or PutList
	str  string      // string passed to PutCharList
}

func (f *fakeVM) call(name string) { f.calls = append(f.calls, name) }

func (f *fakeVM) Success(cont term.Handle) runtime.Promise {
	f.call("Success")
	f.cont = cont
	return runtime.Promise{}
}

func (f *fakeVM) Failure() runtime.Promise {
	f.call("Failure")
	return runtime.Promise{}
}

func (f *fakeVM) Throw(err error, cont term.Handle) runtime.Promise {
	f.call("Throw")
	f.err, f.cont = err, cont
	return runtime.Promise{}
}

func (f *fakeVM) Unify(a, b term.Handle) (bool, error) {
	f.call("Unify")
	return f.unified, f.err
}

func (f *fakeVM) Deref(t term.Handle) term.Handle {
	f.call("Deref")
	return t
}

func (f *fakeVM) Variable(t term.Handle) (int, bool) {
	f.call("Variable")
	return 0, f.variable
}

func (f *fakeVM) MustBeAtom(t term.Handle) (term.Atom, error) {
	f.call("MustBeAtom")
	return f.atom, f.err
}

func (f *fakeVM) MustBeInteger(t term.Handle) (int64, error) {
	f.call("MustBeInteger")
	return f.integer, f.err
}

func (f *fakeVM) MustBeFloat(t term.Handle) (float64, error) {
	f.call("MustBeFloat")
	return f.float, f.err
}

func (f *fakeVM) MustBeCompound(t term.Handle) (term.Functor, error) {
	f.call("MustBeCompound")
	return f.functor, f.err
}

func (f *fakeVM) Arg(t term.Handle, n int) term.Handle {
	f.call("Arg")
	f.argN = n
	return term.Handle{}
}

func (f *fakeVM) MustBeList(t term.Handle, fn func(elem term.Handle) error) error {
	f.call("MustBeList")
	if f.err != nil {
		return f.err
	}
	for range f.listElems {
		if err := fn(term.Handle{}); err != nil {
			return err
		}
	}
	return nil
}

func (f *fakeVM) MustBeChar(t term.Handle) (rune, error) {
	f.call("MustBeChar")
	return f.char, f.charErr
}

func (f *fakeVM) PutVariable() (term.Handle, error) {
	f.call("PutVariable")
	return term.Handle{}, f.err
}

func (f *fakeVM) PutAtom(a term.Atom) (term.Handle, error) {
	f.call("PutAtom")
	f.atom = a
	return term.Handle{}, f.err
}

func (f *fakeVM) PutInteger(i int64) (term.Handle, error) {
	f.call("PutInteger")
	f.integer = i
	return term.Handle{}, f.err
}

func (f *fakeVM) PutFloat(fl float64) (term.Handle, error) {
	f.call("PutFloat")
	f.float = fl
	return term.Handle{}, f.err
}

func (f *fakeVM) PutCompound(name term.Atom, args ...term.Handle) (term.Handle, error) {
	f.call("PutCompound")
	f.name, f.args = name, len(args)
	return term.Handle{}, f.err
}

func (f *fakeVM) PutCharList(s string) (term.Handle, error) {
	f.call("PutCharList")
	f.str = s
	return term.Handle{}, f.err
}

func (f *fakeVM) PutList(elems ...term.Handle) (term.Handle, error) {
	f.call("PutList")
	f.args = len(elems)
	return term.Handle{}, f.err
}

// newExecution returns an Execution driven by f, with a distinguishable
// continuation so that forwarding can be checked.
func newExecution(f *fakeVM) Execution {
	return Execution{vm: f}
}

func TestExecution_Success(t *testing.T) {
	var f fakeVM
	newExecution(&f).Success()

	if got, want := f.calls, []string{"Success"}; !slices.Equal(got, want) {
		t.Errorf("got: %v, want: %v", got, want)
	}
}

func TestExecution_Failure(t *testing.T) {
	var f fakeVM
	newExecution(&f).Failure()

	if got, want := f.calls, []string{"Failure"}; !slices.Equal(got, want) {
		t.Errorf("got: %v, want: %v", got, want)
	}
}

// Error throws the given error rather than reporting it to the caller.
func TestExecution_Error(t *testing.T) {
	var f fakeVM
	newExecution(&f).Error(errVM)

	if got, want := f.calls, []string{"Throw"}; !slices.Equal(got, want) {
		t.Errorf("got: %v, want: %v", got, want)
	}
	if got, want := f.err, errVM; !errors.Is(got, want) {
		t.Errorf("got: %v, want: %v", got, want)
	}
}

func TestExecution_Unification(t *testing.T) {
	tests := []struct {
		title   string
		unified bool
		err     error
		want    []string
	}{
		{title: "unified", unified: true, want: []string{"Unify", "Success"}},
		{title: "not unified", unified: false, want: []string{"Unify", "Failure"}},
		{title: "error", err: errVM, want: []string{"Unify", "Throw"}},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			f := fakeVM{unified: test.unified, err: test.err}
			newExecution(&f).Unification(Term{}, Term{})

			if got, want := f.calls, test.want; !slices.Equal(got, want) {
				t.Errorf("got: %v, want: %v", got, want)
			}
		})
	}
}

func TestExecution_Unify(t *testing.T) {
	tests := []struct {
		title   string
		unified bool
		err     error
	}{
		{title: "unified", unified: true},
		{title: "not unified", unified: false},
		{title: "error", err: errVM},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			f := fakeVM{unified: test.unified, err: test.err}

			got, err := newExecution(&f).Unify(Term{}, Term{})
			if !errors.Is(err, test.err) {
				t.Fatalf("got: %v, want: %v", err, test.err)
			}
			if want := test.unified; got != want {
				t.Errorf("got: %v, want: %v", got, want)
			}
			if got, want := f.calls, []string{"Unify"}; !slices.Equal(got, want) {
				t.Errorf("got: %v, want: %v", got, want)
			}
		})
	}
}

// Variable dereferences before asking whether the term is a variable.
func TestExecution_Variable(t *testing.T) {
	tests := []struct {
		title string
		want  bool
	}{
		{title: "variable", want: true},
		{title: "not a variable", want: false},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			f := fakeVM{variable: test.want}

			if got, want := newExecution(&f).Variable(Term{}), test.want; got != want {
				t.Errorf("got: %v, want: %v", got, want)
			}
			if got, want := f.calls, []string{"Deref", "Variable"}; !slices.Equal(got, want) {
				t.Errorf("got: %v, want: %v", got, want)
			}
		})
	}
}

func TestExecution_Atom(t *testing.T) {
	tests := []struct {
		title string
		atom  term.Atom
		err   error
		want  Atom
	}{
		{title: "atom", atom: term.NewAtom("foo"), want: "foo"},
		{title: "empty atom", atom: term.NewAtom(""), want: ""},
		{title: "error", err: errVM},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			f := fakeVM{atom: test.atom, err: test.err}

			got, err := newExecution(&f).Atom(Term{})
			if !errors.Is(err, test.err) {
				t.Fatalf("got: %v, want: %v", err, test.err)
			}
			if want := test.want; got != want {
				t.Errorf("got: %v, want: %v", got, want)
			}
			if got, want := f.calls, []string{"MustBeAtom"}; !slices.Equal(got, want) {
				t.Errorf("got: %v, want: %v", got, want)
			}
		})
	}
}

func TestExecution_Integer(t *testing.T) {
	tests := []struct {
		title   string
		integer int64
		err     error
	}{
		{title: "integer", integer: 42},
		{title: "negative integer", integer: -1},
		{title: "error", err: errVM},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			f := fakeVM{integer: test.integer, err: test.err}

			got, err := newExecution(&f).Integer(Term{})
			if !errors.Is(err, test.err) {
				t.Fatalf("got: %v, want: %v", err, test.err)
			}
			if want := test.integer; got != want {
				t.Errorf("got: %v, want: %v", got, want)
			}
			if got, want := f.calls, []string{"MustBeInteger"}; !slices.Equal(got, want) {
				t.Errorf("got: %v, want: %v", got, want)
			}
		})
	}
}

func TestExecution_Float(t *testing.T) {
	tests := []struct {
		title string
		float float64
		err   error
	}{
		{title: "float", float: 1.5},
		{title: "error", err: errVM},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			f := fakeVM{float: test.float, err: test.err}

			got, err := newExecution(&f).Float(Term{})
			if !errors.Is(err, test.err) {
				t.Fatalf("got: %v, want: %v", err, test.err)
			}
			if want := test.float; got != want {
				t.Errorf("got: %v, want: %v", got, want)
			}
			if got, want := f.calls, []string{"MustBeFloat"}; !slices.Equal(got, want) {
				t.Errorf("got: %v, want: %v", got, want)
			}
		})
	}
}

func TestExecution_Functor(t *testing.T) {
	tests := []struct {
		title     string
		functor   term.Functor
		err       error
		wantName  Atom
		wantArity int
	}{
		// The name alone, not the predicate indicator: term.Functor formats
		// itself as name/arity, which must not leak through here.
		{title: "compound", functor: term.NewFunctor(term.NewAtom("f"), 2), wantName: "f", wantArity: 2},
		{title: "list", functor: term.NewFunctor(term.NewAtom("."), 2), wantName: ".", wantArity: 2},
		{title: "error", err: errVM},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			f := fakeVM{functor: test.functor, err: test.err}

			gotName, gotArity, err := newExecution(&f).Functor(Term{})
			if !errors.Is(err, test.err) {
				t.Fatalf("got: %v, want: %v", err, test.err)
			}
			if want := test.wantName; gotName != want {
				t.Errorf("got: %v, want: %v", gotName, want)
			}
			if want := test.wantArity; gotArity != want {
				t.Errorf("got: %v, want: %v", gotArity, want)
			}
			if got, want := f.calls, []string{"MustBeCompound"}; !slices.Equal(got, want) {
				t.Errorf("got: %v, want: %v", got, want)
			}
		})
	}
}

func TestExecution_Arg(t *testing.T) {
	tests := []struct {
		title   string
		n       int
		vmErr   error
		wantErr string
		calls   []string
	}{
		{title: "first", n: 0, calls: []string{"MustBeCompound", "Arg"}},
		{title: "last", n: 1, calls: []string{"MustBeCompound", "Arg"}},
		// The index has to be below the arity, not merely up to it.
		{title: "past the last", n: 2, wantErr: "argument out of range", calls: []string{"MustBeCompound"}},
		{title: "negative", n: -1, wantErr: "argument out of range", calls: []string{"MustBeCompound"}},
		{title: "not a compound", n: 0, vmErr: errVM, wantErr: "vm", calls: []string{"MustBeCompound"}},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			f := fakeVM{functor: term.NewFunctor(term.NewAtom("f"), 2), err: test.vmErr}

			_, err := newExecution(&f).Arg(Term{}, test.n)
			switch {
			case test.wantErr == "":
				if err != nil {
					t.Fatal(err)
				}
				if got, want := f.argN, test.n; got != want {
					t.Errorf("got: %v, want: %v", got, want)
				}
			case err == nil:
				t.Fatalf("got no error, want: %v", test.wantErr)
			default:
				if got, want := err.Error(), test.wantErr; got != want {
					t.Errorf("got: %v, want: %v", got, want)
				}
			}

			if got, want := f.calls, test.calls; !slices.Equal(got, want) {
				t.Errorf("got: %v, want: %v", got, want)
			}
		})
	}
}

func TestExecution_String(t *testing.T) {
	tests := []struct {
		title   string
		elems   int
		char    rune
		err     error
		charErr error
		want    string
		calls   []string
	}{
		{title: "empty list", elems: 0, want: "", calls: []string{"MustBeList"}},
		{title: "one character", elems: 1, char: 'h', want: "h", calls: []string{"MustBeList", "MustBeChar"}},
		{title: "several characters", elems: 3, char: 'h', want: "hhh",
			calls: []string{"MustBeList", "MustBeChar", "MustBeChar", "MustBeChar"}},
		{title: "multi-byte character", elems: 1, char: '日', want: "日", calls: []string{"MustBeList", "MustBeChar"}},
		{title: "error", err: errVM, calls: []string{"MustBeList"}},
		// An element that isn't a character stops the walk.
		{title: "element error", elems: 3, charErr: errVM, err: errVM,
			calls: []string{"MustBeList", "MustBeChar"}},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			f := fakeVM{listElems: test.elems, char: test.char, charErr: test.charErr}
			if test.charErr == nil {
				f.err = test.err
			}

			got, err := newExecution(&f).String(Term{})
			if !errors.Is(err, test.err) {
				t.Fatalf("got: %v, want: %v", err, test.err)
			}
			if want := test.want; got != want {
				t.Errorf("got: %v, want: %v", got, want)
			}
			if got, want := f.calls, test.calls; !slices.Equal(got, want) {
				t.Errorf("got: %v, want: %v", got, want)
			}
		})
	}
}

func TestExecution_List(t *testing.T) {
	tests := []struct {
		title string
		elems int
		err   error
		want  []Atom
	}{
		{title: "empty list", elems: 0, want: nil},
		{title: "one element", elems: 1, want: []Atom{"a"}},
		{title: "several elements", elems: 3, want: []Atom{"a", "a", "a"}},
		{title: "error", err: errVM},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			e := newExecution(&fakeVM{listElems: test.elems, err: test.err, atom: term.NewAtom("a")})

			got, err := e.List(Term{}, e.Atom)
			if !errors.Is(err, test.err) {
				t.Fatalf("got: %v, want: %v", err, test.err)
			}
			if want := test.want; !slices.Equal(got, want) {
				t.Errorf("got: %v, want: %v", got, want)
			}
		})
	}
}

// An error from the converter stops the walk and reaches the caller unchanged.
func TestExecution_List_converterError(t *testing.T) {
	e := newExecution(&fakeVM{listElems: 3})

	var calls int
	_, err := e.List(Term{}, func(Term) (Atom, error) {
		calls++
		return "", errVM
	})
	if !errors.Is(err, errVM) {
		t.Fatalf("got: %v, want: %v", err, errVM)
	}
	if got, want := calls, 1; got != want {
		t.Errorf("got: %v, want: %v", got, want)
	}
}

func TestExecution_NewVariable(t *testing.T) {
	var f fakeVM
	if _, err := newExecution(&f).NewVariable(); err != nil {
		t.Fatal(err)
	}

	if got, want := f.calls, []string{"PutVariable"}; !slices.Equal(got, want) {
		t.Errorf("got: %v, want: %v", got, want)
	}
}

func TestExecution_NewAtom(t *testing.T) {
	var f fakeVM
	if _, err := newExecution(&f).NewAtom("foo"); err != nil {
		t.Fatal(err)
	}

	if got, want := f.calls, []string{"PutAtom"}; !slices.Equal(got, want) {
		t.Errorf("got: %v, want: %v", got, want)
	}
	if got, want := f.atom, term.NewAtom("foo"); got != want {
		t.Errorf("got: %v, want: %v", got, want)
	}
}

// Every constructor propagates the vm's error rather than swallowing it.
func TestExecution_new_error(t *testing.T) {
	tests := []struct {
		title string
		new   func(Execution) (Term, error)
	}{
		{title: "NewVariable", new: func(e Execution) (Term, error) { return e.NewVariable() }},
		{title: "NewAtom", new: func(e Execution) (Term, error) { return e.NewAtom("foo") }},
		{title: "NewInteger", new: func(e Execution) (Term, error) { return e.NewInteger(42) }},
		{title: "NewFloat", new: func(e Execution) (Term, error) { return e.NewFloat(1.5) }},
		{title: "NewCompound", new: func(e Execution) (Term, error) { return e.NewCompound("f", Term{}) }},
		{title: "NewString", new: func(e Execution) (Term, error) { return e.NewString("hi") }},
		{title: "NewList", new: func(e Execution) (Term, error) { return e.NewList([]Atom{"a"}, e.NewAtom) }},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			f := fakeVM{err: errVM}

			if _, err := test.new(newExecution(&f)); !errors.Is(err, errVM) {
				t.Errorf("got: %v, want: %v", err, errVM)
			}
		})
	}
}

func TestExecution_NewInteger(t *testing.T) {
	var f fakeVM
	if _, err := newExecution(&f).NewInteger(42); err != nil {
		t.Fatal(err)
	}

	if got, want := f.calls, []string{"PutInteger"}; !slices.Equal(got, want) {
		t.Errorf("got: %v, want: %v", got, want)
	}
	if got, want := f.integer, int64(42); got != want {
		t.Errorf("got: %v, want: %v", got, want)
	}
}

func TestExecution_NewFloat(t *testing.T) {
	var f fakeVM
	if _, err := newExecution(&f).NewFloat(1.5); err != nil {
		t.Fatal(err)
	}

	if got, want := f.calls, []string{"PutFloat"}; !slices.Equal(got, want) {
		t.Errorf("got: %v, want: %v", got, want)
	}
	if got, want := f.float, 1.5; got != want {
		t.Errorf("got: %v, want: %v", got, want)
	}
}

func TestExecution_NewCompound(t *testing.T) {
	tests := []struct {
		title string
		args  []Term
	}{
		{title: "no arguments", args: nil},
		{title: "one argument", args: []Term{{}}},
		{title: "several arguments", args: []Term{{}, {}, {}}},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			var f fakeVM
			if _, err := newExecution(&f).NewCompound("f", test.args...); err != nil {
				t.Fatal(err)
			}

			if got, want := f.calls, []string{"PutCompound"}; !slices.Equal(got, want) {
				t.Errorf("got: %v, want: %v", got, want)
			}
			if got, want := f.name, term.NewAtom("f"); got != want {
				t.Errorf("got: %v, want: %v", got, want)
			}
			if got, want := f.args, len(test.args); got != want {
				t.Errorf("got: %v, want: %v", got, want)
			}
		})
	}
}

func TestExecution_NewString(t *testing.T) {
	var f fakeVM
	if _, err := newExecution(&f).NewString("hi"); err != nil {
		t.Fatal(err)
	}

	if got, want := f.calls, []string{"PutCharList"}; !slices.Equal(got, want) {
		t.Errorf("got: %v, want: %v", got, want)
	}
	if got, want := f.str, "hi"; got != want {
		t.Errorf("got: %v, want: %v", got, want)
	}
}

func TestExecution_NewList(t *testing.T) {
	tests := []struct {
		title string
		args  []Atom
		calls []string
	}{
		{title: "empty", args: nil, calls: []string{"PutList"}},
		{title: "one element", args: []Atom{"a"}, calls: []string{"PutAtom", "PutList"}},
		{title: "several elements", args: []Atom{"a", "b"}, calls: []string{"PutAtom", "PutAtom", "PutList"}},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			var f fakeVM
			e := newExecution(&f)

			if _, err := e.NewList(test.args, e.NewAtom); err != nil {
				t.Fatal(err)
			}
			if got, want := f.calls, test.calls; !slices.Equal(got, want) {
				t.Errorf("got: %v, want: %v", got, want)
			}
			if got, want := f.args, len(test.args); got != want {
				t.Errorf("got: %v, want: %v", got, want)
			}
		})
	}
}

// An error from the converter stops before the list is built.
func TestExecution_NewList_converterError(t *testing.T) {
	var f fakeVM

	_, err := newExecution(&f).NewList([]Atom{"a", "b"}, func(Atom) (Term, error) {
		return Term{}, errVM
	})
	if !errors.Is(err, errVM) {
		t.Fatalf("got: %v, want: %v", err, errVM)
	}
	if got, want := f.calls, []string(nil); !slices.Equal(got, want) {
		t.Errorf("got: %v, want: %v", got, want)
	}
}
