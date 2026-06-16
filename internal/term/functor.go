package term

import "fmt"

var (
	functorCons = NewFunctor(atomDot, 2)
)

type NewFunctorOptions struct {
	module Atom
}

type NewFunctorOption func(*NewFunctorOptions)

func Qualified(module Atom) NewFunctorOption {
	return func(o *NewFunctorOptions) {
		o.module = module
	}
}

type Functor int32

func NewFunctor(name Atom, arity int, opts ...NewFunctorOption) Functor {
	var options NewFunctorOptions
	for _, o := range opts {
		o(&options)
	}

	ident := functorIdentifier{module: options.module, name: name, arity: arity}
	if id, ok := functorTable.ids[ident]; ok {
		return id
	}

	id := Functor(len(functorTable.entries))
	functorTable.entries = append(functorTable.entries, functorTableEntry{
		ident: ident,
	})
	if functorTable.ids == nil {
		functorTable.ids = map[functorIdentifier]Functor{}
	}
	functorTable.ids[ident] = id
	return id
}

func (f Functor) String() string {
	return fmt.Sprintf("%s/%d", f.Name(), f.Arity())
}

func (f Functor) Module() Atom {
	return functorTable.entries[f].ident.module
}

func (f Functor) Name() Atom {
	return functorTable.entries[f].ident.name
}

func (f Functor) Arity() int {
	return functorTable.entries[f].ident.arity
}

type functorIdentifier struct {
	module Atom
	name   Atom
	arity  int
}

var functorTable struct {
	ids     map[functorIdentifier]Functor
	entries []functorTableEntry
}

type functorTableEntry struct {
	ident functorIdentifier
	// TODO: GC
}
