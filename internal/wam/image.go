package wam

import "github.com/ichiban/prolog/v2/internal/term"

type Image struct {
	Predicates map[term.Functor]int
	Code       []Instruction
	Constants  []term.Handle
}
