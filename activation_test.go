package prolog

import (
	"context"
	"testing"
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
