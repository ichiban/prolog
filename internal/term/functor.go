package term

import "fmt"

var (
	functorCons = NewFunctor(atomDot, 2)
)

type Functor struct {
	name  Atom
	arity int
}

func NewFunctor(name Atom, arity int) Functor {
	return Functor{
		name:  name,
		arity: arity,
	}
}

func (f Functor) String() string {
	return fmt.Sprintf("%s/%d", f.Name(), f.Arity())
}

func (f Functor) Name() Atom {
	return f.name
}

func (f Functor) Arity() int {
	return f.arity
}
