package prolog

import "github.com/ichiban/prolog/internal"

type (
	Term     = internal.Term
	Variable = internal.Variable
	Atom     = internal.Atom
)

func NewAtom(str string) Atom {
	return internal.NewAtom(str)
}
