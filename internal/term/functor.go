package term

var (
	functorCons = NewFunctor(atomDot, 2)
)

type Functor int32

func NewFunctor(name Atom, arity int) Functor {
	return NewFunctorQualified(Atom{}, name, arity)
}

func NewFunctorQualified(module, name Atom, arity int) Functor {
	ident := functorIdentifier{module: module, name: name, arity: arity}
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
