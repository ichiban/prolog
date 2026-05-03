package ir

import "github.com/ichiban/prolog/v2/internal/term"

type Module struct {
	Name       term.Atom
	Predicates []Predicate
}
