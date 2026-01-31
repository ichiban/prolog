package ir

type Clause struct {
	Code        []Instruction
	Atoms       []string
	Integers    []int
	Floats      []float64
	Functors    []Functor
	Occurrences []Occurrence
	Variables   []Variable
}

type Functor struct {
	Name  string
	Arity int
}

type Occurrence struct {
	VariableID int
}

type Variable struct {
	Counter       int
	MaxOccurrence int
}
