package prolog

import (
	"errors"
	"iter"
	"strings"

	"github.com/ichiban/prolog/v2/internal/runtime"
	"github.com/ichiban/prolog/v2/internal/term"
)

// Term is a reference to Prolog datum.
type Term struct {
	ref runtime.Ref
}

// Outcome is the result of a custom builtin predicate.
// Construct one with [Activation.Success], [Execution.Failure], [Activation.Error],
// [Execution.Unification], or [Activation.Nondet].
type Outcome struct {
	promise runtime.Promise
}

// Activation is an abstraction of the Prolog engine while a custom builtin predicate is active.
type Activation struct {
	activation *runtime.Activation
	cont       runtime.Ref
}

// Success creates a successful outcome.
func (a Activation) Success() Outcome {
	return Outcome{promise: a.activation.Success(a.cont)}
}

// Failure creates a failed outcome.
func (a Activation) Failure() Outcome {
	return Outcome{promise: a.activation.Failure()}
}

// Error creates an exceptional outcome.
func (a Activation) Error(err error) Outcome {
	return Outcome{promise: a.activation.Throw(err, a.cont)}
}

// Nondet creates an outcome with multiple alternatives, tried in order on backtracking.
func (a Activation) Nondet(seq iter.Seq[Outcome]) Outcome {
	return Outcome{promise: runtime.Delay(func(yield func(runtime.Promise) bool) {
		for p := range seq {
			if !yield(p.promise) {
				return
			}
		}
	})}
}

// Unification unifies two terms and returns the resulting outcome.
// Use [Activation.Unify] instead when the predicate has more work to do afterwards.
func (a Activation) Unification(x, y Term) Outcome {
	ok, err := a.activation.Unify(x.ref, y.ref)
	if err != nil {
		return a.Error(err)
	}
	if !ok {
		return a.Failure()
	}
	return a.Success()
}

// Unify unifies two terms and reports whether they unified.
// Consider using [Activation.Unification] when it's the final statement of a predicate.
func (a Activation) Unify(x, y Term) (bool, error) {
	return a.activation.Unify(x.ref, y.ref)
}

// Variable returns true if and only if the term is a variable.
func (a Activation) Variable(t Term) bool {
	h := a.activation.Deref(t.ref)
	_, ok := a.activation.Variable(h)
	return ok
}

// Atom returns the value of an atom term. It returns an error if it's not an atom term.
func (a Activation) Atom(t Term) (Atom, error) {
	atom, err := a.activation.MustBeAtom(t.ref)
	if err != nil {
		return "", err
	}
	return Atom(atom.String()), nil
}

// Integer returns the value of an integer term. It returns an error if it's not an integer term.
func (a Activation) Integer(t Term) (int64, error) {
	return a.activation.MustBeInteger(t.ref)
}

// Float returns the value of a float term. It returns an error if it's not a float term.
func (a Activation) Float(t Term) (float64, error) {
	return a.activation.MustBeFloat(t.ref)
}

// Functor returns the name and arity of a compound term. It returns an error if it's not a compound term.
func (a Activation) Functor(t Term) (Atom, int, error) {
	f, err := a.activation.MustBeCompound(t.ref)
	if err != nil {
		return "", 0, err
	}
	return Atom(f.Name().String()), f.Arity(), nil
}

// Arg returns the N-th argument of a compound term. It returns an error if it's not a compound term or the index is invalid.
func (a Activation) Arg(t Term, n int) (Term, error) {
	f, err := a.activation.MustBeCompound(t.ref)
	if err != nil {
		return Term{}, err
	}
	if n < 0 || n >= f.Arity() {
		return Term{}, errors.New("argument out of range")
	}
	c := a.activation.Arg(t.ref, n)
	return Term{ref: c}, nil
}

// String returns the string value of a character list. It returns an error if it's not a character list.
func (a Activation) String(t Term) (string, error) {
	var sb strings.Builder
	if err := a.activation.MustBeList(t.ref, func(elem runtime.Ref) error {
		r, err := a.activation.MustBeChar(elem)
		if err != nil {
			return err
		}
		_, _ = sb.WriteRune(r)
		return nil
	}); err != nil {
		return "", err
	}
	return sb.String(), nil
}

// List returns the list elements as a slice. It returns an error if it's not a list.
func (a Activation) List[T any](t Term, fn func(Term) (T, error)) ([]T, error) {
	var elems []T
	if err := a.activation.MustBeList(t.ref, func(elem runtime.Ref) error {
		a, err := fn(Term{ref: elem})
		if err != nil {
			return err
		}
		elems = append(elems, a)
		return nil
	}); err != nil {
		return nil, err
	}
	return elems, nil
}

// NewVariable creates a new variable term.
func (a Activation) NewVariable() (Term, error) {
	t, err := a.activation.PutVariable()
	if err != nil {
		return Term{}, err
	}
	return Term{ref: t}, nil
}

// NewAtom creates a new atom term.
func (a Activation) NewAtom(atom Atom) (Term, error) {
	t, err := a.activation.PutAtom(term.NewAtom(string(atom)))
	if err != nil {
		return Term{}, err
	}
	return Term{ref: t}, nil
}

// NewInteger creates a new integer term.
func (a Activation) NewInteger(i int64) (Term, error) {
	t, err := a.activation.PutInteger(i)
	if err != nil {
		return Term{}, err
	}
	return Term{ref: t}, nil
}

// NewFloat creates a new float term.
func (a Activation) NewFloat(f float64) (Term, error) {
	t, err := a.activation.PutFloat(f)
	if err != nil {
		return Term{}, err
	}
	return Term{ref: t}, nil
}

// NewCompound creates a new compound term.
func (a Activation) NewCompound(name Atom, args ...Term) (Term, error) {
	hs := make([]runtime.Ref, len(args))
	for i, arg := range args {
		hs[i] = arg.ref
	}
	c, err := a.activation.PutCompound(term.NewAtom(string(name)), hs...)
	if err != nil {
		return Term{}, err
	}
	return Term{ref: c}, nil
}

// NewString creates a new character list.
func (a Activation) NewString(s string) (Term, error) {
	t, err := a.activation.PutCharList(s)
	if err != nil {
		return Term{}, err
	}
	return Term{ref: t}, nil
}

// NewList creates a new list.
func (a Activation) NewList[T any](args []T, fn func(T) (Term, error)) (Term, error) {
	elems := make([]runtime.Ref, len(args))
	for i, arg := range args {
		a, err := fn(arg)
		if err != nil {
			return Term{}, err
		}
		elems[i] = a.ref
	}
	l, err := a.activation.PutList(elems...)
	return Term{ref: l}, err
}
