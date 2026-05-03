package syntax

import (
	"math"

	"github.com/ichiban/prolog/v2/internal/term"
)

// OperatorSet is a set of defined operators.
type OperatorSet struct {
	ops map[opKey]operator
}

func NewOperatorSet() *OperatorSet {
	var ops OperatorSet

	// :- op(1200, xfx, [:-, -->]).
	ops.Define(1200, XFX, term.NewAtom(":-"), term.NewAtom("-->"))
	// :- op(1200, fx, [:-, ?-]).
	ops.Define(1200, FX, term.NewAtom(":-"), term.NewAtom("?-"))
	// :- op(1105, xfy, '|').
	ops.Define(1105, XFY, term.NewAtomRune('|'))
	// :- op(1100, xfy, ;).
	ops.Define(1100, XFY, term.NewAtomRune(';'))
	// :- op(1050, xfy, ->).
	ops.Define(1050, XFY, term.NewAtom("->"))
	// :- op(1000, xfy, ',').
	ops.Define(1000, XFY, term.NewAtomRune(','))
	// :- op(900, fy, \+).
	ops.Define(900, FY, term.NewAtom(`\+`))
	// :- op(700, xfx, [=, \=]).
	ops.Define(700, XFX, term.NewAtomRune('='), term.NewAtom(`\=`))
	// :- op(700, xfx, [==, \==, @<, @=<, @>, @>=]).
	ops.Define(700, XFX, term.NewAtom("=="), term.NewAtom(`\==`), term.NewAtom(`@<`), term.NewAtom(`@=<`), term.NewAtom(`@>`), term.NewAtom(`@>=`))
	// :- op(700, xfx, =..).
	ops.Define(700, XFX, term.NewAtom("=.."))
	// :- op(700, xfx, [is, =:=, =\=, <, =<, >, >=]).
	ops.Define(700, XFX, term.NewAtom("is"), term.NewAtom("=:="), term.NewAtom(`=\=`), term.NewAtomRune('<'), term.NewAtom("=<"), term.NewAtomRune('>'), term.NewAtom(">="))
	// :- op(600, xfy, :).
	ops.Define(600, XFY, term.NewAtomRune(':'))
	// :- op(500, yfx, [+, -, /\, \/]).
	ops.Define(500, YFX, term.NewAtomRune('+'), term.NewAtomRune('-'), term.NewAtom(`/\`), term.NewAtom(`\/`))
	// :- op(400, yfx, [*, /, //, div, rem, mod, <<, >>]).
	ops.Define(400, YFX, term.NewAtomRune('*'), term.NewAtomRune('/'), term.NewAtom("//"), term.NewAtom("div"), term.NewAtom("rem"), term.NewAtom("mod"), term.NewAtom("<<"), term.NewAtom(">>"))
	// :- op(200, xfx, **).
	ops.Define(200, XFX, term.NewAtom("**"))
	// :- op(200, xfy, ^).
	ops.Define(200, XFY, term.NewAtomRune('^'))
	// :- op(200, fy, [+, -, \]).
	ops.Define(200, FY, term.NewAtomRune('+'), term.NewAtomRune('-'), term.NewAtomRune('\\'))

	return &ops
}

// Define defines an operator.
func (o *OperatorSet) Define(priority int, spec OperatorSpecifier, names ...term.Atom) {
	if o.ops == nil {
		o.ops = map[opKey]operator{}
	}
	for _, name := range names {
		o.ops[opKey{
			name:    name,
			opClass: operatorSpecifiers[spec].opClass,
		}] = operator{
			priority:  priority,
			specifier: spec,
			name:      name,
		}
	}
}

func (o *OperatorSet) definedIn(name term.Atom, opClass operatorClass) bool {
	_, ok := o.ops[opKey{name: name, opClass: opClass}]
	return ok
}

func (o *OperatorSet) defined(name term.Atom) bool {
	return o.definedIn(name, operatorClassPrefix) ||
		o.definedIn(name, operatorClassPostfix) ||
		o.definedIn(name, operatorClassInfix)
}

type opKey struct {
	name    term.Atom
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
	name      term.Atom
}

// Pratt parser's binding powers but in Prolog priority.
func (o *operator) bindingPriorities() (int, int) {
	return operatorSpecifiers[o.specifier].priorities(o.priority)
}
