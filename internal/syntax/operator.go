package syntax

import (
	"math"
	"slices"

	"github.com/ichiban/prolog/v2/internal/term"
)

// OperatorSet is a set of defined operators.
type OperatorSet []Operator

var defaultOperatorSet = OperatorSet{
	// :- op(1200, xfx, [:-, -->]).
	{Priority: 1200, Specifier: XFX, Name: term.NewAtom(`:-`)},
	{Priority: 1200, Specifier: XFX, Name: term.NewAtom(`-->`)},
	// :- op(1200, fx, [:-, ?-]).
	{Priority: 1200, Specifier: FX, Name: term.NewAtom(`:-`)},
	{Priority: 1200, Specifier: FX, Name: term.NewAtom(`?-`)},
	// :- op(1105, xfy, '|').
	{Priority: 1105, Specifier: XFY, Name: term.NewAtomRune('|')},
	// :- op(1100, xfy, ;).
	{Priority: 1100, Specifier: XFY, Name: term.NewAtomRune(';')},
	// :- op(1050, xfy, ->).
	{Priority: 1050, Specifier: XFY, Name: term.NewAtom(`->`)},
	// :- op(1000, xfy, ',').
	{Priority: 1000, Specifier: XFY, Name: term.NewAtomRune(',')},
	// :- op(900, fy, \+).
	{Priority: 900, Specifier: FY, Name: term.NewAtom(`\+`)},
	// :- op(700, xfx, [=, \=]).
	{Priority: 700, Specifier: XFX, Name: term.NewAtomRune('=')},
	{Priority: 700, Specifier: XFX, Name: term.NewAtom(`\=`)},
	// :- op(700, xfx, [==, \==, @<, @=<, @>, @>=]).
	{Priority: 700, Specifier: XFX, Name: term.NewAtom(`==`)},
	{Priority: 700, Specifier: XFX, Name: term.NewAtom(`\==`)},
	{Priority: 700, Specifier: XFX, Name: term.NewAtom(`@<`)},
	{Priority: 700, Specifier: XFX, Name: term.NewAtom(`@=<`)},
	{Priority: 700, Specifier: XFX, Name: term.NewAtom(`@>`)},
	{Priority: 700, Specifier: XFX, Name: term.NewAtom(`@>=`)},
	// :- op(700, xfx, =..).
	{Priority: 700, Specifier: XFX, Name: term.NewAtom(`=..`)},
	// :- op(700, xfx, [is, =:=, =\=, <, =<, >, >=]).
	{Priority: 700, Specifier: XFX, Name: term.NewAtom(`is`)},
	{Priority: 700, Specifier: XFX, Name: term.NewAtom(`=:=`)},
	{Priority: 700, Specifier: XFX, Name: term.NewAtom(`=\=`)},
	{Priority: 700, Specifier: XFX, Name: term.NewAtomRune('<')},
	{Priority: 700, Specifier: XFX, Name: term.NewAtom(`=<`)},
	{Priority: 700, Specifier: XFX, Name: term.NewAtomRune('>')},
	{Priority: 700, Specifier: XFX, Name: term.NewAtom(`>=`)},
	// :- op(600, xfy, :).
	{Priority: 600, Specifier: XFY, Name: term.NewAtomRune(':')},
	// :- op(500, yfx, [+, -, /\, \/]).
	{Priority: 500, Specifier: YFX, Name: term.NewAtomRune('+')},
	{Priority: 500, Specifier: YFX, Name: term.NewAtomRune('-')},
	{Priority: 500, Specifier: YFX, Name: term.NewAtom(`/\`)},
	{Priority: 500, Specifier: YFX, Name: term.NewAtom(`\/`)},
	// :- op(400, yfx, [*, /, //, div, rem, mod, <<, >>]).
	{Priority: 400, Specifier: YFX, Name: term.NewAtomRune('*')},
	{Priority: 400, Specifier: YFX, Name: term.NewAtomRune('/')},
	{Priority: 400, Specifier: YFX, Name: term.NewAtom(`//`)},
	{Priority: 400, Specifier: YFX, Name: term.NewAtom(`div`)},
	{Priority: 400, Specifier: YFX, Name: term.NewAtom(`rem`)},
	{Priority: 400, Specifier: YFX, Name: term.NewAtom(`mod`)},
	{Priority: 400, Specifier: YFX, Name: term.NewAtom(`<<`)},
	{Priority: 400, Specifier: YFX, Name: term.NewAtom(`>>`)},
	// :- op(200, xfx, **).
	{Priority: 200, Specifier: XFX, Name: term.NewAtom(`**`)},
	// :- op(200, xfy, ^).
	{Priority: 200, Specifier: XFY, Name: term.NewAtomRune('^')},
	// :- op(200, fy, [+, -, \]).
	{Priority: 200, Specifier: FY, Name: term.NewAtomRune('+')},
	{Priority: 200, Specifier: FY, Name: term.NewAtomRune('-')},
	{Priority: 200, Specifier: FY, Name: term.NewAtomRune('\\')},
}

func NewOperatorSet() *OperatorSet {
	ops := slices.Clone(defaultOperatorSet)
	return &ops
}

// Define defines an operator.
func (o *OperatorSet) Define(priority int16, spec OperatorSpecifier, names ...term.Atom) {
	if priority == 0 {
		return
	}
	for _, name := range names {
		*o = append(*o, Operator{
			Priority:  priority,
			Specifier: spec,
			Name:      name,
		})
	}
}

func (o *OperatorSet) Undefine(name term.Atom, class OperatorClass) {
	_ = slices.DeleteFunc(*o, func(op Operator) bool {
		return op.Name == name && op.Specifier.Class() == class
	})
}

func (o *OperatorSet) DefinedIn(name term.Atom, opClass OperatorClass) (Operator, bool) {
	i := slices.IndexFunc(*o, func(op Operator) bool {
		return op.Name == name && op.Specifier.Class() == opClass
	})
	if i < 0 {
		return Operator{}, false
	}
	return (*o)[i], true
}

func (o *OperatorSet) defined(name term.Atom) bool {
	return slices.IndexFunc(*o, func(op Operator) bool {
		return op.Name == name
	}) >= 0
}

type OperatorClass int8

const (
	Prefix OperatorClass = iota
	Postfix
	Infix
)

var operatorClasses = [...]struct {
	arity int
}{
	Prefix: {
		arity: 1,
	},
	Postfix: {
		arity: 1,
	},
	Infix: {
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
	opClass    OperatorClass
	priorities func(p int16) (left int16, right int16)
}{
	FX: {
		name:    "fx",
		opClass: Prefix,
		priorities: func(p int16) (left int16, right int16) {
			return math.MaxInt16, p - 1
		},
	},
	FY: {
		name:    "fy",
		opClass: Prefix,
		priorities: func(p int16) (left int16, right int16) {
			return math.MaxInt16, p
		},
	},
	XF: {
		name:    "xf",
		opClass: Postfix,
		priorities: func(p int16) (left int16, right int16) {
			return p - 1, math.MaxInt16
		},
	},
	YF: {
		name:    "yf",
		opClass: Postfix,
		priorities: func(p int16) (left int16, right int16) {
			return p, math.MaxInt16
		},
	},
	XFX: {
		name:    "xfx",
		opClass: Infix,
		priorities: func(p int16) (left int16, right int16) {
			return p - 1, p - 1
		},
	},
	XFY: {
		name:    "xfy",
		opClass: Infix,
		priorities: func(p int16) (left int16, right int16) {
			return p - 1, p
		},
	},
	YFX: {
		name:    "yfx",
		opClass: Infix,
		priorities: func(p int16) (left int16, right int16) {
			return p, p - 1
		},
	},
}

func (s OperatorSpecifier) Class() OperatorClass {
	return operatorSpecifiers[s].opClass
}

func (s OperatorSpecifier) String() string {
	return operatorSpecifiers[s].name
}

func (s OperatorSpecifier) arity() int {
	return operatorClasses[operatorSpecifiers[s].opClass].arity
}

type Operator struct {
	Priority  int16 // 1 ~ 1200
	Specifier OperatorSpecifier
	Name      term.Atom
}

// Pratt parser's binding powers but in Prolog priority.
func (o *Operator) bindingPriorities() (int16, int16) {
	return operatorSpecifiers[o.Specifier].priorities(o.Priority)
}
