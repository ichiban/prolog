package prolog

import (
	"math"
)

// Operators is a set of defined operators.
type Operators struct {
	ops map[opKey]operator
}

// Define defines an operator.
func (o *Operators) Define(priority int, spec OperatorSpecifier, name string) {
	if o.ops == nil {
		o.ops = map[opKey]operator{}
	}
	o.ops[opKey{
		name:    name,
		opClass: operatorSpecifiers[spec].opClass,
	}] = operator{
		priority:  priority,
		specifier: spec,
		name:      name,
	}
}

func (o *Operators) definedIn(name string, opClass operatorClass) bool {
	_, ok := o.ops[opKey{name: name, opClass: opClass}]
	return ok
}

func (o *Operators) defined(name string) bool {
	return o.definedIn(name, operatorClassPrefix) ||
		o.definedIn(name, operatorClassPostfix) ||
		o.definedIn(name, operatorClassInfix)
}

type opKey struct {
	name    string
	opClass operatorClass
}

type operatorClass int8

const (
	operatorClassPrefix operatorClass = iota
	operatorClassPostfix
	operatorClassInfix
)

var operatorClasses = [...]struct {
	arity int
}{
	operatorClassPrefix: {
		arity: 1,
	},
	operatorClassPostfix: {
		arity: 1,
	},
	operatorClassInfix: {
		arity: 2,
	},
}

// OperatorSpecifier specifies a class and associativity of an operator.
type OperatorSpecifier int8

const (
	FX OperatorSpecifier = iota
	FY
	XF
	YF
	XFX
	XFY
	YFX
)

var operatorSpecifiers = [...]struct {
	name       string
	opClass    operatorClass
	priorities func(p int) (left int, right int)
}{
	FX: {
		name:    "fx",
		opClass: operatorClassPrefix,
		priorities: func(p int) (left int, right int) {
			return math.MaxInt, p - 1
		},
	},
	FY: {
		name:    "fy",
		opClass: operatorClassPrefix,
		priorities: func(p int) (left int, right int) {
			return math.MaxInt, p
		},
	},
	XF: {
		name:    "xf",
		opClass: operatorClassPostfix,
		priorities: func(p int) (left int, right int) {
			return p - 1, math.MaxInt
		},
	},
	YF: {
		name:    "yf",
		opClass: operatorClassPostfix,
		priorities: func(p int) (left int, right int) {
			return p, math.MaxInt
		},
	},
	XFX: {
		name:    "xfx",
		opClass: operatorClassInfix,
		priorities: func(p int) (left int, right int) {
			return p - 1, p - 1
		},
	},
	XFY: {
		name:    "xFy",
		opClass: operatorClassInfix,
		priorities: func(p int) (left int, right int) {
			return p - 1, p
		},
	},
	YFX: {
		name:    "yFx",
		opClass: operatorClassInfix,
		priorities: func(p int) (left int, right int) {
			return p, p - 1
		},
	},
}

func (s OperatorSpecifier) class() operatorClass {
	return operatorSpecifiers[s].opClass
}

func (s OperatorSpecifier) String() string {
	return operatorSpecifiers[s].name
}

func (s OperatorSpecifier) arity() int {
	return operatorClasses[operatorSpecifiers[s].opClass].arity
}

type operator struct {
	priority  int // 1 ~ 1200
	specifier OperatorSpecifier
	name      string
}

// Pratt parser's binding powers but in Prolog priority.
func (o *operator) bindingPriorities() (int, int) {
	return operatorSpecifiers[o.specifier].priorities(o.priority)
}
