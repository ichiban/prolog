package runtime

import (
	"errors"
	"iter"
	"math"
	"reflect"
	"slices"
	"testing"

	"github.com/ichiban/prolog/v2/internal/syntax"
	"github.com/ichiban/prolog/v2/internal/term"
)

// activation returns an Activation over an execution with a heap of the given
// size.
func activation(size int) *Activation {
	return &Activation{
		exec: &Execution{
			Engine: &Engine{Arena: term.NewArena(size)},
		},
	}
}

func TestActivation_ref(t *testing.T) {
	tests := []struct {
		title string
		cells func(a *Activation) []term.Cell
	}{
		{
			title: "no cells",
			cells: func(a *Activation) []term.Cell {
				return nil
			},
		},
		{
			title: "one cell",
			cells: func(a *Activation) []term.Cell {
				return []term.Cell{must(a.exec.PutAtom(term.NewAtom("foo")))}
			},
		},
		{
			title: "a ref per cell",
			cells: func(a *Activation) []term.Cell {
				return []term.Cell{
					must(a.exec.PutVariable()),
					must(a.exec.PutAtom(term.NewAtom("foo"))),
					must(a.exec.PutInteger(1)),
				}
			},
		},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			a := activation(16)
			cells := test.cells(a)

			refs := make([]Ref, len(cells))
			for i, c := range cells {
				refs[i] = a.ref(c)
			}

			for i, r := range refs {
				if *r.cell != cells[i] {
					t.Errorf("expected: %v, got: %v", cells[i], *r.cell)
				}
			}

			// Every ref is captured so that a GC can relocate the cell behind
			// the builtin's back.
			if len(a.captured) != len(refs) {
				t.Fatalf("expected: %d, got: %d", len(refs), len(a.captured))
			}
			for i, c := range a.captured {
				if c.Value() != refs[i].cell {
					t.Errorf("expected: %p, got: %p", refs[i].cell, c.Value())
				}
			}
		})
	}
}

func TestCells(t *testing.T) {
	a := activation(16)

	tests := []struct {
		title string
		refs  []Ref
		cells []term.Cell
	}{
		{
			title: "no refs",
			refs:  nil,
			cells: []term.Cell{},
		},
		{
			title: "some refs",
			refs: []Ref{
				a.ref(must(a.exec.PutAtom(term.NewAtom("foo")))),
				a.ref(must(a.exec.PutInteger(1))),
			},
			cells: []term.Cell{
				must(a.exec.PutAtom(term.NewAtom("foo"))),
				must(a.exec.PutInteger(1)),
			},
		},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			if got := cells(test.refs); !slices.Equal(got, test.cells) {
				t.Errorf("expected: %v, got: %v", test.cells, got)
			}
		})
	}
}

func TestActivation_Success(t *testing.T) {
	tests := []struct {
		title string
		cont  func(a *Activation) Ref
	}{
		{
			title: "an atom continuation",
			cont: func(a *Activation) Ref {
				return a.ref(must(a.exec.PutAtom(term.NewAtom("true"))))
			},
		},
		{
			title: "a compound continuation",
			cont: func(a *Activation) Ref {
				return a.ref(must(a.exec.PutCompound(term.NewAtom("foo"), must(a.exec.PutAtom(term.NewAtom("bar"))))))
			},
		},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			a := activation(16)
			a.exec.programPointer = 3
			cont := test.cont(a)

			p := a.Success(cont)

			if !p.ok || p.err != nil {
				t.Errorf("expected: a successful promise, got: %+v", p)
			}
			// The continuation becomes the next goal.
			if a.exec.tempVars[1] != *cont.cell {
				t.Errorf("expected: %v, got: %v", *cont.cell, a.exec.tempVars[1])
			}
			if a.exec.programPointer != 4 {
				t.Errorf("expected: %d, got: %d", 4, a.exec.programPointer)
			}
		})
	}
}

func TestActivation_Failure(t *testing.T) {
	a := activation(16)

	p := a.Failure()

	if p.ok || p.err != nil || p.delayed != nil {
		t.Errorf("expected: a failed promise, got: %+v", p)
	}
}

func TestActivation_Throw(t *testing.T) {
	tests := []struct {
		title string
		err   error
		goal  syntax.Serialized
	}{
		{
			title: "a host error becomes a system error",
			err:   errors.New("something went wrong"),
			goal:  `throw(error(system_error,'something went wrong'),true) .`,
		},
		{
			title: "an instantiation error keeps its shape",
			err:   &InstantiationError{},
			goal:  `throw(error(instantiation_error,''/0),true) .`,
		},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			a := activation(64)
			cont := a.ref(must(a.exec.PutAtom(term.NewAtom("true"))))

			p := a.Throw(test.err, cont)

			if !p.ok || p.err != nil {
				t.Errorf("expected: a successful promise, got: %+v", p)
			}
			// The ball is thrown by continuing with throw/2.
			if got := syntax.Serialize(a.exec.Arena, a.exec.tempVars[1]); got != test.goal {
				t.Errorf("expected: %s, got: %s", test.goal, got)
			}
		})
	}
}

func TestActivation_Nondet(t *testing.T) {
	tests := []struct {
		title    string
		seq      iter.Seq[Promise]
		promises []bool // The ok of each promise in the sequence.
	}{
		{
			title:    "an empty sequence",
			seq:      slices.Values([]Promise{}),
			promises: nil,
		},
		{
			title:    "a sequence of promises",
			seq:      slices.Values([]Promise{{ok: true}, {ok: false}, {ok: true}}),
			promises: []bool{true, false, true},
		},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			a := activation(16)

			p := a.Nondet(test.seq)

			if p.ok || p.err != nil {
				t.Errorf("expected: a delayed promise, got: %+v", p)
			}
			if p.delayed == nil {
				t.Fatal("the sequence is gone")
			}
			var got []bool
			for d := range p.delayed {
				got = append(got, d.ok)
			}
			if !slices.Equal(got, test.promises) {
				t.Errorf("expected: %v, got: %v", test.promises, got)
			}
		})
	}
}

func TestActivation_Unify(t *testing.T) {
	tests := []struct {
		title string
		terms func(a *Activation) (Ref, Ref)
		ok    bool
		goal  syntax.Serialized // What x names once unified.
	}{
		{
			title: "a variable and an atom",
			terms: func(a *Activation) (Ref, Ref) {
				return a.ref(must(a.exec.PutVariable())), a.ref(must(a.exec.PutAtom(term.NewAtom("foo"))))
			},
			ok:   true,
			goal: `foo .`,
		},
		{
			title: "atoms that differ",
			terms: func(a *Activation) (Ref, Ref) {
				return a.ref(must(a.exec.PutAtom(term.NewAtom("foo")))), a.ref(must(a.exec.PutAtom(term.NewAtom("bar"))))
			},
			ok:   false,
			goal: `foo .`,
		},
		{
			title: "compounds unify argument-wise",
			terms: func(a *Activation) (Ref, Ref) {
				x := must(a.exec.PutCompound(term.NewAtom("foo"), must(a.exec.PutVariable())))
				y := must(a.exec.PutCompound(term.NewAtom("foo"), must(a.exec.PutInteger(1))))
				return a.ref(x), a.ref(y)
			},
			ok:   true,
			goal: `foo(1) .`,
		},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			a := activation(32)
			x, y := test.terms(a)

			ok, err := a.Unify(x, y)
			if err != nil {
				t.Fatal(err)
			}

			if ok != test.ok {
				t.Errorf("expected: %v, got: %v", test.ok, ok)
			}
			if got := syntax.Serialize(a.exec.Arena, *x.cell); got != test.goal {
				t.Errorf("expected: %s, got: %s", test.goal, got)
			}
		})
	}
}

func TestActivation_Deref(t *testing.T) {
	tests := []struct {
		title string
		term  func(t *testing.T, a *Activation) Ref
		cell  func(a *Activation, x Ref) term.Cell
	}{
		{
			title: "an unbound variable is itself",
			term: func(t *testing.T, a *Activation) Ref {
				return putVariable(a)
			},
			cell: func(a *Activation, x Ref) term.Cell {
				return *x.cell
			},
		},
		{
			title: "a bound variable is what it names",
			term: func(t *testing.T, a *Activation) Ref {
				v := must(a.exec.PutVariable())
				if err := a.exec.Bind(v, must(a.exec.PutAtom(term.NewAtom("foo")))); err != nil {
					t.Fatal(err)
				}
				return a.ref(v)
			},
			cell: func(a *Activation, x Ref) term.Cell {
				return must(a.exec.PutAtom(term.NewAtom("foo")))
			},
		},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			a := activation(16)
			x := test.term(t, a)

			got := a.Deref(x)

			if want := test.cell(a, x); *got.cell != want {
				t.Errorf("expected: %v, got: %v", want, *got.cell)
			}
			// The result is a ref of its own, captured like any other.
			if a.captured[len(a.captured)-1].Value() != got.cell {
				t.Error("the dereferenced term isn't captured")
			}
		})
	}
}

func TestActivation_Variable(t *testing.T) {
	a := activation(16)

	tests := []struct {
		title string
		term  Ref
		addr  int
		ok    bool
	}{
		{
			title: "variable",
			term:  a.ref(must(a.exec.PutVariable())),
			addr:  0,
			ok:    true,
		},
		{
			title: "not variable",
			term:  a.ref(must(a.exec.PutAtom(term.NewAtom("foo")))),
			ok:    false,
		},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			addr, ok := a.Variable(test.term)
			if ok != test.ok {
				t.Errorf("expected: %v, got: %v", test.ok, ok)
			}
			if addr != test.addr {
				t.Errorf("expected: %v, got: %v", test.addr, addr)
			}
		})
	}
}

func TestActivation_Atom(t *testing.T) {
	a := activation(16)

	tests := []struct {
		title string
		term  Ref
		atom  term.Atom
		ok    bool
	}{
		{
			title: "atom",
			term:  a.ref(must(a.exec.PutAtom(term.NewAtom("foo")))),
			atom:  term.NewAtom("foo"),
			ok:    true,
		},
		{
			title: "not atom",
			term:  a.ref(must(a.exec.PutInteger(1))),
			ok:    false,
		},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			atom, ok := a.Atom(test.term)
			if ok != test.ok {
				t.Errorf("expected: %v, got: %v", test.ok, ok)
			}
			if ok && atom != test.atom {
				t.Errorf("expected: %v, got: %v", test.atom, atom)
			}
		})
	}
}

func TestActivation_Functor(t *testing.T) {
	a := activation(16)

	tests := []struct {
		title   string
		term    Ref
		opts    []term.FunctorOption
		functor term.Functor
		ok      bool
	}{
		{
			title:   "compound",
			term:    a.ref(must(a.exec.PutCompound(term.NewAtom("foo"), must(a.exec.PutAtom(term.NewAtom("bar")))))),
			functor: term.NewFunctor(term.NewAtom("foo"), 1),
			ok:      true,
		},
		{
			title: "atom",
			term:  a.ref(must(a.exec.PutAtom(term.NewAtom("foo")))),
			ok:    false,
		},
		{
			title:   "atom with AllowAtom",
			term:    a.ref(must(a.exec.PutAtom(term.NewAtom("foo")))),
			opts:    []term.FunctorOption{term.AllowAtom(true)},
			functor: term.NewFunctor(term.NewAtom("foo"), 0),
			ok:      true,
		},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			f, ok := a.Functor(test.term, test.opts...)
			if ok != test.ok {
				t.Errorf("expected: %v, got: %v", test.ok, ok)
			}
			if ok && f != test.functor {
				t.Errorf("expected: %v, got: %v", test.functor, f)
			}
		})
	}
}

func TestActivation_MustBe(t *testing.T) {
	tests := []struct {
		title string
		term  func(a *Activation) Ref
		call  func(a *Activation, t Ref) (any, error)
		value any
		err   error
	}{
		{
			title: "MustBeAtom",
			term:  putAtom("foo"),
			call:  func(a *Activation, t Ref) (any, error) { return a.MustBeAtom(t) },
			value: term.NewAtom("foo"),
		},
		{
			title: "MustBeAtom with a variable",
			term:  putVariable,
			call:  func(a *Activation, t Ref) (any, error) { return a.MustBeAtom(t) },
			value: term.Atom{},
			err:   &InstantiationError{},
		},
		{
			title: "MustBeAtom with an integer",
			term:  putInteger(1),
			call:  func(a *Activation, t Ref) (any, error) { return a.MustBeAtom(t) },
			value: term.Atom{},
			err:   &TypeError{ValidType: term.NewAtom("atom"), Culprit: `1 .`},
		},
		{
			title: "MustBeInteger",
			term:  putInteger(1),
			call:  func(a *Activation, t Ref) (any, error) { return a.MustBeInteger(t) },
			value: int64(1),
		},
		{
			title: "MustBeInteger with an atom",
			term:  putAtom("foo"),
			call:  func(a *Activation, t Ref) (any, error) { return a.MustBeInteger(t) },
			value: int64(0),
			err:   &TypeError{ValidType: term.NewAtom("integer"), Culprit: `foo .`},
		},
		{
			title: "MustBeFloat",
			term: func(a *Activation) Ref {
				return a.ref(must(a.exec.PutFloat(1.5)))
			},
			call:  func(a *Activation, t Ref) (any, error) { return a.MustBeFloat(t) },
			value: 1.5,
		},
		{
			title: "MustBeFloat with an integer",
			term:  putInteger(1),
			call:  func(a *Activation, t Ref) (any, error) { return a.MustBeFloat(t) },
			value: float64(0),
			err:   &TypeError{ValidType: term.NewAtom("float"), Culprit: `1 .`},
		},
		{
			title: "MustBeChar",
			term:  putAtom("a"),
			call:  func(a *Activation, t Ref) (any, error) { return a.MustBeChar(t) },
			value: 'a',
		},
		{
			title: "MustBeChar with a variable",
			term:  putVariable,
			call:  func(a *Activation, t Ref) (any, error) { return a.MustBeChar(t) },
			value: rune(0),
			err:   &InstantiationError{},
		},
		{
			title: "MustBeCompound",
			term: func(a *Activation) Ref {
				return a.ref(must(a.exec.PutCompound(term.NewAtom("foo"), must(a.exec.PutAtom(term.NewAtom("bar"))))))
			},
			call:  func(a *Activation, t Ref) (any, error) { return a.MustBeCompound(t) },
			value: term.NewFunctor(term.NewAtom("foo"), 1),
		},
		{
			title: "MustBeCompound with an atom",
			term:  putAtom("foo"),
			call:  func(a *Activation, t Ref) (any, error) { return a.MustBeCompound(t) },
			value: term.Functor{},
			err:   &TypeError{ValidType: term.NewAtom("compound"), Culprit: `foo .`},
		},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			a := activation(16)

			value, err := test.call(a, test.term(a))

			if !reflect.DeepEqual(err, test.err) {
				t.Errorf("expected: %v, got: %v", test.err, err)
			}
			if !reflect.DeepEqual(value, test.value) {
				t.Errorf("expected: %v, got: %v", test.value, value)
			}
		})
	}
}

func TestActivation_Arg(t *testing.T) {
	a := activation(16)
	c := a.ref(must(a.exec.PutCompound(term.NewAtom("foo"), must(a.exec.PutAtom(term.NewAtom("bar"))), must(a.exec.PutInteger(1)))))

	tests := []struct {
		title string
		n     int
		cell  term.Cell
	}{
		{
			title: "first",
			n:     0,
			cell:  must(a.exec.PutAtom(term.NewAtom("bar"))),
		},
		{
			title: "second",
			n:     1,
			cell:  must(a.exec.PutInteger(1)),
		},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			arg := a.Arg(c, test.n)
			if *arg.cell != test.cell {
				t.Errorf("expected: %v, got: %v", test.cell, *arg.cell)
			}
			if a.captured[len(a.captured)-1].Value() != arg.cell {
				t.Error("the argument isn't captured")
			}
		})
	}
}

func TestActivation_Args(t *testing.T) {
	tests := []struct {
		title string
		term  func(a *Activation) Ref
		stop  int // Break out of the sequence after this many arguments, -1 to exhaust it.
		args  []syntax.Serialized
	}{
		{
			title: "a compound",
			term: func(a *Activation) Ref {
				return a.ref(must(a.exec.PutCompound(term.NewAtom("foo"), must(a.exec.PutAtom(term.NewAtom("bar"))), must(a.exec.PutInteger(1)))))
			},
			stop: -1,
			args: []syntax.Serialized{`bar .`, `1 .`},
		},
		{
			title: "an atom has no arguments",
			term:  putAtom("foo"),
			stop:  -1,
			args:  nil,
		},
		{
			title: "breaking out stops the sequence",
			term: func(a *Activation) Ref {
				return a.ref(must(a.exec.PutCompound(term.NewAtom("foo"), must(a.exec.PutAtom(term.NewAtom("bar"))), must(a.exec.PutInteger(1)))))
			},
			stop: 1,
			args: []syntax.Serialized{`bar .`},
		},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			a := activation(16)

			var args []syntax.Serialized
			for arg := range a.Args(test.term(a)) {
				args = append(args, syntax.Serialize(a.exec.Arena, *arg.cell))
				if len(args) == test.stop {
					break
				}
			}

			if !slices.Equal(args, test.args) {
				t.Errorf("expected: %v, got: %v", test.args, args)
			}
		})
	}
}

func TestActivation_MustBeList(t *testing.T) {
	errTest := errors.New("test")

	tests := []struct {
		title string
		term  func(a *Activation) Ref
		fn    func(elem Ref) error
		elems []syntax.Serialized
		err   error
	}{
		{
			title: "a proper list",
			term: func(a *Activation) Ref {
				return a.ref(must(a.exec.PutList(must(a.exec.PutInteger(1)), must(a.exec.PutInteger(2)))))
			},
			elems: []syntax.Serialized{`1 .`, `2 .`},
		},
		{
			title: "an empty list",
			term:  putAtom("[]"),
			elems: nil,
		},
		{
			title: "a partial list",
			term: func(a *Activation) Ref {
				return a.ref(must(a.exec.PutPartialList(must(a.exec.PutVariable()), must(a.exec.PutInteger(1)))))
			},
			elems: []syntax.Serialized{`1 .`},
			err:   &InstantiationError{},
		},
		{
			title: "not a list",
			term:  putAtom("foo"),
			err:   &TypeError{ValidType: term.NewAtom("list"), Culprit: `foo .`},
		},
		{
			title: "the callback's error is propagated",
			term: func(a *Activation) Ref {
				return a.ref(must(a.exec.PutList(must(a.exec.PutInteger(1)))))
			},
			fn: func(elem Ref) error {
				return errTest
			},
			err: errTest,
		},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			a := activation(32)

			var elems []syntax.Serialized
			err := a.MustBeList(test.term(a), func(elem Ref) error {
				elems = append(elems, syntax.Serialize(a.exec.Arena, *elem.cell))
				if test.fn != nil {
					return test.fn(elem)
				}
				return nil
			})

			if !reflect.DeepEqual(err, test.err) {
				t.Errorf("expected: %v, got: %v", test.err, err)
			}
			if test.fn == nil && !slices.Equal(elems, test.elems) {
				t.Errorf("expected: %v, got: %v", test.elems, elems)
			}
		})
	}
}

func TestActivation_Put(t *testing.T) {
	tests := []struct {
		title string
		size  int
		put   func(a *Activation) (Ref, error)
		check func(t *testing.T, a *Activation, r Ref)
		err   error
	}{
		{
			title: "PutVariable",
			size:  1,
			put:   func(a *Activation) (Ref, error) { return a.PutVariable() },
			check: func(t *testing.T, a *Activation, r Ref) {
				if _, ok := a.Variable(r); !ok {
					t.Errorf("expected: an unbound variable, got: %v", *r.cell)
				}
			},
		},
		{
			title: "PutVariable out of memory",
			size:  0,
			put:   func(a *Activation) (Ref, error) { return a.PutVariable() },
			err:   term.ErrOutOfMemory,
		},
		{
			title: "PutInteger",
			size:  1,
			put:   func(a *Activation) (Ref, error) { return a.PutInteger(1) },
			check: func(t *testing.T, a *Activation, r Ref) {
				if i, err := a.MustBeInteger(r); err != nil || i != 1 {
					t.Errorf("expected: %d, got: %d (%v)", 1, i, err)
				}
			},
		},
		{
			title: "PutInteger out of memory",
			size:  0,
			put:   func(a *Activation) (Ref, error) { return a.PutInteger(math.MaxInt32 + 1) },
			err:   term.ErrOutOfMemory,
		},
		{
			title: "PutFloat",
			size:  1,
			put:   func(a *Activation) (Ref, error) { return a.PutFloat(1.5) },
			check: func(t *testing.T, a *Activation, r Ref) {
				if f, err := a.MustBeFloat(r); err != nil || f != 1.5 {
					t.Errorf("expected: %f, got: %f (%v)", 1.5, f, err)
				}
			},
		},
		{
			title: "PutFloat out of memory",
			size:  0,
			put:   func(a *Activation) (Ref, error) { return a.PutFloat(1.5) },
			err:   term.ErrOutOfMemory,
		},
		{
			title: "PutCharList",
			size:  1,
			put:   func(a *Activation) (Ref, error) { return a.PutCharList("hello") },
			check: func(t *testing.T, a *Activation, r Ref) {
				if s, ok := a.exec.CharList(*r.cell); !ok || s != "hello" {
					t.Errorf("expected: %s, got: %s", "hello", s)
				}
			},
		},
		{
			title: "PutAtom",
			size:  1,
			put:   func(a *Activation) (Ref, error) { return a.PutAtom(term.NewAtom("foo")) },
			check: func(t *testing.T, a *Activation, r Ref) {
				if atom, ok := a.Atom(r); !ok || atom != term.NewAtom("foo") {
					t.Errorf("expected: %v, got: %v", term.NewAtom("foo"), atom)
				}
			},
		},
		{
			title: "PutCompound",
			size:  16,
			put: func(a *Activation) (Ref, error) {
				bar, err := a.PutAtom(term.NewAtom("bar"))
				if err != nil {
					return Ref{}, err
				}
				return a.PutCompound(term.NewAtom("foo"), bar)
			},
			check: func(t *testing.T, a *Activation, r Ref) {
				if got, want := syntax.Serialize(a.exec.Arena, *r.cell), syntax.Serialized(`foo(bar) .`); got != want {
					t.Errorf("expected: %s, got: %s", want, got)
				}
			},
		},
		{
			title: "PutCompound out of memory",
			size:  0,
			put: func(a *Activation) (Ref, error) {
				bar, err := a.PutAtom(term.NewAtom("bar"))
				if err != nil {
					return Ref{}, err
				}
				return a.PutCompound(term.NewAtom("foo"), bar)
			},
			err: term.ErrOutOfMemory,
		},
		{
			title: "PutList",
			size:  16,
			put: func(a *Activation) (Ref, error) {
				one, err := a.PutInteger(1)
				if err != nil {
					return Ref{}, err
				}
				two, err := a.PutInteger(2)
				if err != nil {
					return Ref{}, err
				}
				return a.PutList(one, two)
			},
			check: func(t *testing.T, a *Activation, r Ref) {
				if got, want := syntax.Serialize(a.exec.Arena, *r.cell), syntax.Serialized(`[1,2] .`); got != want {
					t.Errorf("expected: %s, got: %s", want, got)
				}
			},
		},
		{
			title: "PutList out of memory",
			size:  0,
			put: func(a *Activation) (Ref, error) {
				one, err := a.PutInteger(1)
				if err != nil {
					return Ref{}, err
				}
				return a.PutList(one)
			},
			err: term.ErrOutOfMemory,
		},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			a := activation(test.size)

			r, err := test.put(a)

			if !errors.Is(err, test.err) {
				t.Fatalf("expected: %v, got: %v", test.err, err)
			}
			if err != nil {
				if r != (Ref{}) {
					t.Errorf("expected: a zero Ref, got: %v", r)
				}
				return
			}
			// Everything put is captured so that a GC can relocate it.
			if a.captured[len(a.captured)-1].Value() != r.cell {
				t.Error("the term isn't captured")
			}
			test.check(t, a, r)
		})
	}
}

func putVariable(a *Activation) Ref {
	return a.ref(must(a.exec.PutVariable()))
}

func putAtom(name string) func(a *Activation) Ref {
	return func(a *Activation) Ref {
		return a.ref(must(a.exec.PutAtom(term.NewAtom(name))))
	}
}

func putInteger(i int64) func(a *Activation) Ref {
	return func(a *Activation) Ref {
		return a.ref(must(a.exec.PutInteger(i)))
	}
}
