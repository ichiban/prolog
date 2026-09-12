package runtime

import (
	"iter"
	"weak"

	"github.com/ichiban/prolog/v2/internal/term"
)

// Ref is a handle to a term held by a [Builtin].
type Ref struct {
	cell *term.Cell
}

// Activation is a limited view to the [Execution] passed to a [Builtin].
type Activation struct {
	exec     *Execution
	captured []weak.Pointer[term.Cell]
	closed   bool
}

func (a *Activation) ref(t term.Cell) Ref {
	c := new(t)
	a.captured = append(a.captured, weak.Make(c))
	return Ref{cell: c}
}

// cells unwraps refs for an [Execution] method taking variadic term.Cell.
func cells(refs []Ref) []term.Cell {
	cs := make([]term.Cell, len(refs))
	for i, r := range refs {
		cs[i] = *r.cell
	}
	return cs
}

// Success reports that the builtin succeeded and execution should continue
// with cont.
func (a *Activation) Success(cont Ref) Promise {
	return a.exec.Success(*cont.cell)
}

// Failure reports that the builtin failed.
func (a *Activation) Failure() Promise {
	return Failure()
}

// Throw raises err, unwinding to the nearest catch/3.
func (a *Activation) Throw(err error, cont Ref) Promise {
	return a.exec.Throw(err, *cont.cell)
}

func (a *Activation) Nondet(seq iter.Seq[Promise]) Promise {
	return Delay(seq)
}

// Unify unifies two terms.
func (a *Activation) Unify(x, y Ref) (bool, error) {
	return a.exec.Unify(*x.cell, *y.cell)
}

// Deref follows a variable's binding chain to the term it names.
func (a *Activation) Deref(t Ref) Ref {
	return a.ref(a.exec.Deref(*t.cell))
}

// Variable returns the address of an unbound variable.
func (a *Activation) Variable(t Ref) (int, bool) {
	return a.exec.Variable(*t.cell)
}

// Atom returns the atom if t is an atom term.
func (a *Activation) Atom(t Ref) (term.Atom, bool) {
	return a.exec.Atom(*t.cell)
}

// Functor returns the name and arity if t is a compound term.
func (a *Activation) Functor(t Ref, opts ...term.FunctorOption) (term.Functor, bool) {
	return a.exec.Functor(*t.cell, opts...)
}

func (a *Activation) MustBeAtom(t Ref) (term.Atom, error) {
	return a.exec.MustBeAtom(*t.cell)
}

func (a *Activation) MustBeInteger(t Ref) (int64, error) {
	return a.exec.MustBeInteger(*t.cell)
}

func (a *Activation) MustBeFloat(t Ref) (float64, error) {
	return a.exec.MustBeFloat(*t.cell)
}

func (a *Activation) MustBeChar(t Ref) (rune, error) {
	return a.exec.MustBeChar(*t.cell)
}

func (a *Activation) MustBeCompound(t Ref) (term.Functor, error) {
	return a.exec.MustBeCompound(*t.cell)
}

// Arg returns the n-th argument of a compound term.
func (a *Activation) Arg(t Ref, n int) Ref {
	return a.ref(a.exec.Arg(*t.cell, n))
}

// Args returns a sequence of arguments for a compound term.
func (a *Activation) Args(t Ref) iter.Seq[Ref] {
	return func(yield func(Ref) bool) {
		for arg := range a.exec.Args(*t.cell) {
			if !yield(a.ref(arg)) {
				return
			}
		}
	}
}

// MustBeList calls f for each element of a list.
func (a *Activation) MustBeList(t Ref, f func(elem Ref) error) error {
	return a.exec.MustBeList(*t.cell, func(elem term.Cell) error {
		return f(a.ref(elem))
	})
}

func (a *Activation) PutVariable() (Ref, error) {
	t, err := a.exec.PutVariable()
	if err != nil {
		return Ref{}, err
	}
	return a.ref(t), nil
}

func (a *Activation) PutInteger(i int64) (Ref, error) {
	t, err := a.exec.PutInteger(i)
	if err != nil {
		return Ref{}, err
	}
	return a.ref(t), nil
}

func (a *Activation) PutFloat(f float64) (Ref, error) {
	t, err := a.exec.PutFloat(f)
	if err != nil {
		return Ref{}, err
	}
	return a.ref(t), nil
}

func (a *Activation) PutCharList(str string) (Ref, error) {
	t, err := a.exec.PutCharList(str)
	if err != nil {
		return Ref{}, err
	}
	return a.ref(t), nil
}

func (a *Activation) PutAtom(x term.Atom) (Ref, error) {
	t, err := a.exec.PutAtom(x)
	if err != nil {
		return Ref{}, err
	}
	return a.ref(t), nil
}

func (a *Activation) PutCompound(name term.Atom, args ...Ref) (Ref, error) {
	t, err := a.exec.PutCompound(name, cells(args)...)
	if err != nil {
		return Ref{}, err
	}
	return a.ref(t), nil
}

func (a *Activation) PutList(elems ...Ref) (Ref, error) {
	t, err := a.exec.PutList(cells(elems)...)
	if err != nil {
		return Ref{}, err
	}
	return a.ref(t), nil
}

func (a *Activation) Close() {
	a.closed = true
}

func (a *Activation) Closed() bool {
	return a.closed
}
