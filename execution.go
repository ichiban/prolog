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
	cell term.Cell
}

// Outcome is the result of a custom builtin predicate.
// Construct one with [Execution.Success], [Execution.Failure], [Execution.Error],
// [Execution.Unification], or [Execution.Nondet].
type Outcome struct {
	promise runtime.Promise
}

type vm interface {
	Success(cont term.Cell) runtime.Promise
	Failure() runtime.Promise
	Throw(err error, cont term.Cell) runtime.Promise
	Unify(a, b term.Cell) (bool, error)
	Deref(t term.Cell) term.Cell
	Variable(t term.Cell) (int, bool)
	MustBeAtom(t term.Cell) (term.Atom, error)
	MustBeInteger(t term.Cell) (int64, error)
	MustBeFloat(t term.Cell) (float64, error)
	MustBeCompound(t term.Cell) (term.Functor, error)
	Arg(t term.Cell, n int) term.Cell
	MustBeList(t term.Cell, f func(elem term.Cell) error) error
	MustBeChar(t term.Cell) (rune, error)
	PutVariable() (term.Cell, error)
	PutAtom(a term.Atom) (term.Cell, error)
	PutInteger(i int64) (term.Cell, error)
	PutFloat(f float64) (term.Cell, error)
	PutCompound(name term.Atom, args ...term.Cell) (term.Cell, error)
	PutCharList(s string) (term.Cell, error)
	PutList(elems ...term.Cell) (term.Cell, error)
}

// Execution is an abstraction of the Prolog engine while a custom builtin predicate is active.
// It exposes a curated set of engine functionalities to Go.
type Execution struct {
	vm   vm
	cont term.Cell
}

// Success creates a successful outcome.
func (e Execution) Success() Outcome {
	return Outcome{promise: e.vm.Success(e.cont)}
}

// Failure creates a failed outcome.
func (e Execution) Failure() Outcome {
	return Outcome{promise: e.vm.Failure()}
}

// Error creates an exceptional outcome.
func (e Execution) Error(err error) Outcome {
	return Outcome{promise: e.vm.Throw(err, e.cont)}
}

// Nondet creates an outcome with multiple alternatives, tried in order on backtracking.
func (e Execution) Nondet(seq iter.Seq[Outcome]) Outcome {
	return Outcome{promise: runtime.Delay(func(yield func(runtime.Promise) bool) {
		for p := range seq {
			if !yield(p.promise) {
				return
			}
		}
	})}
}

// Unification unifies two terms and returns the resulting outcome.
// Use [Execution.Unify] instead when the predicate has more work to do afterwards.
func (e Execution) Unification(a, b Term) Outcome {
	ok, err := e.vm.Unify(a.cell, b.cell)
	if err != nil {
		return e.Error(err)
	}
	if !ok {
		return e.Failure()
	}
	return e.Success()
}

// Unify unifies two terms and reports whether they unified.
// Consider using [Execution.Unification] when it's the final statement of a predicate.
func (e Execution) Unify(a, b Term) (bool, error) {
	return e.vm.Unify(a.cell, b.cell)
}

// Variable returns true if and only if the term is a variable.
func (e Execution) Variable(t Term) bool {
	h := e.vm.Deref(t.cell)
	_, ok := e.vm.Variable(h)
	return ok
}

// Atom returns the value of an atom term. It returns an error if it's not an atom term.
func (e Execution) Atom(t Term) (Atom, error) {
	a, err := e.vm.MustBeAtom(t.cell)
	if err != nil {
		return "", err
	}
	return Atom(a.String()), nil
}

// Integer returns the value of an integer term. It returns an error if it's not an integer term.
func (e Execution) Integer(t Term) (int64, error) {
	return e.vm.MustBeInteger(t.cell)
}

// Float returns the value of a float term. It returns an error if it's not a float term.
func (e Execution) Float(t Term) (float64, error) {
	return e.vm.MustBeFloat(t.cell)
}

// Functor returns the name and arity of a compound term. It returns an error if it's not a compound term.
func (e Execution) Functor(t Term) (Atom, int, error) {
	f, err := e.vm.MustBeCompound(t.cell)
	if err != nil {
		return "", 0, err
	}
	return Atom(f.Name().String()), f.Arity(), nil
}

// Arg returns the N-th argument of a compound term. It returns an error if it's not a compound term or the index is invalid.
func (e Execution) Arg(t Term, n int) (Term, error) {
	f, err := e.vm.MustBeCompound(t.cell)
	if err != nil {
		return Term{}, err
	}
	if n < 0 || n >= f.Arity() {
		return Term{}, errors.New("argument out of range")
	}
	c := e.vm.Arg(t.cell, n)
	return Term{cell: c}, nil
}

// String returns the string value of a character list. It returns an error if it's not a character list.
func (e Execution) String(t Term) (string, error) {
	var sb strings.Builder
	if err := e.vm.MustBeList(t.cell, func(elem term.Cell) error {
		r, err := e.vm.MustBeChar(elem)
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
func (e Execution) List[T any](t Term, fn func(Term) (T, error)) ([]T, error) {
	var elems []T
	if err := e.vm.MustBeList(t.cell, func(elem term.Cell) error {
		a, err := fn(Term{cell: elem})
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
func (e Execution) NewVariable() (Term, error) {
	t, err := e.vm.PutVariable()
	if err != nil {
		return Term{}, err
	}
	return Term{cell: t}, nil
}

// NewAtom creates a new atom term.
func (e Execution) NewAtom(a Atom) (Term, error) {
	t, err := e.vm.PutAtom(term.NewAtom(string(a)))
	if err != nil {
		return Term{}, err
	}
	return Term{cell: t}, nil
}

// NewInteger creates a new integer term.
func (e Execution) NewInteger(i int64) (Term, error) {
	t, err := e.vm.PutInteger(i)
	if err != nil {
		return Term{}, err
	}
	return Term{cell: t}, nil
}

// NewFloat creates a new float term.
func (e Execution) NewFloat(f float64) (Term, error) {
	t, err := e.vm.PutFloat(f)
	if err != nil {
		return Term{}, err
	}
	return Term{cell: t}, nil
}

// NewCompound creates a new compound term.
func (e Execution) NewCompound(name Atom, args ...Term) (Term, error) {
	hs := make([]term.Cell, len(args))
	for i, arg := range args {
		hs[i] = arg.cell
	}
	c, err := e.vm.PutCompound(term.NewAtom(string(name)), hs...)
	if err != nil {
		return Term{}, err
	}
	return Term{cell: c}, nil
}

// NewString creates a new character list.
func (e Execution) NewString(s string) (Term, error) {
	t, err := e.vm.PutCharList(s)
	if err != nil {
		return Term{}, err
	}
	return Term{cell: t}, nil
}

// NewList creates a new list.
func (e Execution) NewList[T any](args []T, fn func(T) (Term, error)) (Term, error) {
	elems := make([]term.Cell, len(args))
	for i, arg := range args {
		a, err := fn(arg)
		if err != nil {
			return Term{}, err
		}
		elems[i] = a.cell
	}
	l, err := e.vm.PutList(elems...)
	return Term{cell: l}, err
}
