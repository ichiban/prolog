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

// Procedure identifies a predicate by the module it's defined in and its
// functor. The module system is procedure based: atoms and functors are
// global, only predicates are local to a module.
type Procedure struct {
	Module Atom
	Functor
}

func NewProcedure(module Atom, f Functor) Procedure {
	return Procedure{Module: module, Functor: f}
}

func (p Procedure) String() string {
	return fmt.Sprintf("%s:%s", p.Module, p.Functor)
}
