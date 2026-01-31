package wam

type Image struct {
	Predicates map[PredicateIndicator]int
	Code       []Instruction
	Atoms      []string
	Integers   []int
	Floats     []float64
	Functors   []Functor
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
