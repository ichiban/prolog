package ir

import "github.com/ichiban/prolog/v2/internal/term"

type Predicate struct {
	Indicator term.Functor
	Clauses   []Clause
}
