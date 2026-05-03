package wam

import "github.com/ichiban/prolog/v2/internal/term"

type Image struct {
	Predicates map[PredicateIndicator]int
	Code       []Instruction
	Atoms      []term.Atom
	Integers   []int
	Floats     []float64
	Functors   []term.Functor
}

type PredicateIndicator struct {
	Module string
	Name   string
	Arity  int
}

type Functor struct {
	Name  string
	Arity int
}
