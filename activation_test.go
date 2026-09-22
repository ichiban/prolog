package prolog

import (
	"context"
	"reflect"
	"strings"
	"testing"

	"github.com/ichiban/prolog/v2/internal/runtime"
	"github.com/ichiban/prolog/v2/internal/term"
)

// The zero Term belongs to no activation and so names no cell: dereferencing it
// would panic rather than fail if it reached the engine. The constructors reject
// it along with any other term that didn't come from this activation.
func TestActivation_NewCompound_invalidArg(t *testing.T) {
	i := New()

	var err error
	if regErr := i.Register("new_compound_invalid_arg", func(ctx context.Context, a Activation) Outcome {
		_, err = a.NewCompound("f", Term{})
		return a.Success()
	}); regErr != nil {
		t.Fatal(regErr)
	}

	for _, err := range i.Query[struct{}](context.Background(), `new_compound_invalid_arg.`) {
		if err != nil {
			t.Fatal(err)
		}
	}

	if err != errInvalidTerm {
		t.Errorf("expected: %v, got: %v", errInvalidTerm, err)
	}
}

func TestActivation_NewList_invalidElem(t *testing.T) {
	i := New()

	var err error
	if regErr := i.Register("new_list_invalid_elem", func(ctx context.Context, a Activation) Outcome {
		_, err = a.NewList([]int{1}, func(int) (Term, error) {
			return Term{}, nil
		})
		return a.Success()
	}); regErr != nil {
		t.Fatal(regErr)
	}

	for _, err := range i.Query[struct{}](context.Background(), `new_list_invalid_elem.`) {
		if err != nil {
			t.Fatal(err)
		}
	}

	if err != errInvalidTerm {
		t.Errorf("expected: %v, got: %v", errInvalidTerm, err)
	}
}

// A cut discards the choice point of a nondeterministic builtin. The builtin
// never resumes, so its iterator has to be stopped and its activation closed
// before execution moves on.
func TestActivation_Nondet_cut(t *testing.T) {
	i := New()

	var (
		activation Activation
		stopped    bool
		sawStopped bool
		sawClosed  bool
	)

	if err := i.Register("nondet_cut", func(ctx context.Context, a Activation, out Term) Outcome {
		activation = a
		return a.Nondet(func(yield func(Outcome) bool) {
			defer func() { stopped = true }()
			for n := 1; ; n++ {
				v, err := a.NewInteger(int64(n))
				if err != nil {
					yield(a.Error(err))
					return
				}
				if !yield(a.Unification(out, v)) {
					return
				}
			}
		})
	}); err != nil {
		t.Fatal(err)
	}

	if err := i.Register("nondet_cut_observe", func(ctx context.Context, a Activation) Outcome {
		sawStopped = stopped
		_, err := activation.NewAtom("probe")
		sawClosed = err == errActivationClosed
		return a.Success()
	}); err != nil {
		t.Fatal(err)
	}

	var solutions int
	for _, err := range i.Query[struct{}](context.Background(), `nondet_cut(_), !, nondet_cut_observe.`) {
		if err != nil {
			t.Fatal(err)
		}
		solutions++
	}

	if solutions != 1 {
		t.Errorf("expected: 1 solution, got: %d", solutions)
	}
	if !sawStopped {
		t.Error("the cut left the iterator running")
	}
	if !sawClosed {
		t.Error("the cut left the activation open")
	}
}

// Abandoning a query leaves the choice point of a nondeterministic builtin on
// the stack. Nothing will ever resume it, so ending the query has to stop its
// iterator and close its activation the way a cut does.
func TestActivation_Nondet_abandonedQuery(t *testing.T) {
	i := New()

	var (
		activation Activation
		stopped    bool
	)

	if err := i.Register("nondet_abandoned", func(ctx context.Context, a Activation, out Term) Outcome {
		activation = a
		return a.Nondet(func(yield func(Outcome) bool) {
			defer func() { stopped = true }()
			for n := 1; ; n++ {
				v, err := a.NewInteger(int64(n))
				if err != nil {
					yield(a.Error(err))
					return
				}
				if !yield(a.Unification(out, v)) {
					return
				}
			}
		})
	}); err != nil {
		t.Fatal(err)
	}

	var solutions int
	for _, err := range i.Query[struct{}](context.Background(), `nondet_abandoned(_).`) {
		if err != nil {
			t.Fatal(err)
		}
		solutions++
		break // Abandon the query with the choice point still open.
	}

	if solutions != 1 {
		t.Errorf("expected: 1 solution, got: %d", solutions)
	}
	if !stopped {
		t.Error("abandoning the query left the iterator running")
	}
	if _, err := activation.NewAtom("probe"); err != errActivationClosed {
		t.Errorf("expected: %v, got: %v", errActivationClosed, err)
	}
}

// activation returns an Activation backed by a bare execution, without an
// engine run behind it.
func activation() Activation {
	return Activation{activation: runtime.NewActivation(&runtime.Execution{
		Engine: &runtime.Engine{Arena: term.NewArena(1024)},
	})}
}

func TestActivation_Compound(t *testing.T) {
	tests := []struct {
		title string
		setUp func(a Activation) Term
		close bool
		name  Atom
		args  []Atom
		err   string
	}{
		{
			title: "compound",
			setUp: func(a Activation) Term {
				return must(a.NewCompound("f", must(a.NewAtom("a")), must(a.NewAtom("b"))))
			},
			name: `f`,
			args: []Atom{`a`, `b`},
		},
		{
			title: "atom",
			setUp: func(a Activation) Term { return must(a.NewAtom("a")) },
			err:   `valid type = compound`,
		},
		{
			title: "variable",
			setUp: func(a Activation) Term { return must(a.NewVariable()) },
			err:   `instantiation error`,
		},
		{
			title: "foreign term",
			setUp: func(a Activation) Term { return must(activation().NewAtom("a")) },
			err:   errInvalidTerm.Error(),
		},
		{
			title: "closed activation",
			setUp: func(a Activation) Term { return must(a.NewAtom("a")) },
			close: true,
			err:   errActivationClosed.Error(),
		},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			a := activation()
			arg := test.setUp(a)
			if test.close {
				a.activation.Close()
			}

			name, ts, err := a.Compound(arg)

			switch {
			case test.err == "" && err != nil:
				t.Errorf("expected: no error, got: %v", err)
			case test.err != "" && (err == nil || !strings.Contains(err.Error(), test.err)):
				t.Errorf("expected: %s, got: %v", test.err, err)
			}
			if name != test.name {
				t.Errorf("expected: %v, got: %v", test.name, name)
			}
			var args []Atom
			for _, t := range ts {
				args = append(args, must(a.Atom(t)))
			}
			if !reflect.DeepEqual(args, test.args) {
				t.Errorf("expected: %v, got: %v", test.args, args)
			}
		})
	}
}
