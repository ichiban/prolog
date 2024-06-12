package engine

import (
	"fmt"
	"slices"
)

type module struct {
	procedures map[predicateIndicator]procedureEntry
	unknown    unknownAction

	// Internal/external expression
	operators       operators
	charConversions map[rune]rune
	charConvEnabled bool
	doubleQuotes    doubleQuotes

	// Misc
	debug bool

	buf       clauses
	initGoals []Term
}

func newModule() *module {
	ops := operators{}
	//  op(1200, xfx, [:-, -->]).
	ops.define(Integer(1200), operatorSpecifierXFX, NewAtom(`:-`))
	ops.define(Integer(1200), operatorSpecifierXFX, NewAtom(`-->`))
	//  op(1200, fx, [:-, ?-]).
	ops.define(Integer(1200), operatorSpecifierFX, NewAtom(`:-`))
	ops.define(Integer(1200), operatorSpecifierFX, NewAtom(`?-`))
	//  op(1105, xfy, '|').
	ops.define(Integer(1105), operatorSpecifierXFY, NewAtom(`|`))
	//  op(1100, xfy, ;).
	ops.define(Integer(1100), operatorSpecifierXFY, NewAtom(`;`))
	//  op(1050, xfy, ->).
	ops.define(Integer(1050), operatorSpecifierXFY, NewAtom(`->`))
	//  op(1000, xfy, ',').
	ops.define(Integer(1000), operatorSpecifierXFY, NewAtom(`,`))
	//  op(900, fy, \+).
	ops.define(Integer(900), operatorSpecifierFY, NewAtom(`\+`))
	//  op(700, xfx, [=, \=]).
	ops.define(Integer(700), operatorSpecifierXFX, NewAtom(`=`))
	ops.define(Integer(700), operatorSpecifierXFX, NewAtom(`\=`))
	//  op(700, xfx, [==, \==, @<, @=<, @>, @>=]).
	ops.define(Integer(700), operatorSpecifierXFX, NewAtom(`==`))
	ops.define(Integer(700), operatorSpecifierXFX, NewAtom(`\==`))
	ops.define(Integer(700), operatorSpecifierXFX, NewAtom(`@<`))
	ops.define(Integer(700), operatorSpecifierXFX, NewAtom(`@=<`))
	ops.define(Integer(700), operatorSpecifierXFX, NewAtom(`@>`))
	ops.define(Integer(700), operatorSpecifierXFX, NewAtom(`@>=`))
	//  op(700, xfx, =..).
	ops.define(Integer(700), operatorSpecifierXFX, NewAtom(`=..`))
	//  op(700, xfx, [is, =:=, =\=, <, =<, >, >=]).
	ops.define(Integer(700), operatorSpecifierXFX, NewAtom(`is`))
	ops.define(Integer(700), operatorSpecifierXFX, NewAtom(`=:=`))
	ops.define(Integer(700), operatorSpecifierXFX, NewAtom(`=\=`))
	ops.define(Integer(700), operatorSpecifierXFX, NewAtom(`<`))
	ops.define(Integer(700), operatorSpecifierXFX, NewAtom(`=<`))
	ops.define(Integer(700), operatorSpecifierXFX, NewAtom(`>`))
	ops.define(Integer(700), operatorSpecifierXFX, NewAtom(`>=`))
	//  op(600, xfy, :).
	ops.define(Integer(600), operatorSpecifierXFY, NewAtom(`:`))
	//  op(500, yfx, [+, -, /\, \/]).
	ops.define(Integer(500), operatorSpecifierYFX, NewAtom(`+`))
	ops.define(Integer(500), operatorSpecifierYFX, NewAtom(`-`))
	ops.define(Integer(500), operatorSpecifierYFX, NewAtom(`/\`))
	ops.define(Integer(500), operatorSpecifierYFX, NewAtom(`\/`))
	//  op(400, yfx, [*, /, //, div, rem, mod, <<, >>]).
	ops.define(Integer(400), operatorSpecifierYFX, NewAtom(`*`))
	ops.define(Integer(400), operatorSpecifierYFX, NewAtom(`/`))
	ops.define(Integer(400), operatorSpecifierYFX, NewAtom(`//`))
	ops.define(Integer(400), operatorSpecifierYFX, NewAtom(`div`))
	ops.define(Integer(400), operatorSpecifierYFX, NewAtom(`rem`))
	ops.define(Integer(400), operatorSpecifierYFX, NewAtom(`mod`))
	ops.define(Integer(400), operatorSpecifierYFX, NewAtom(`<<`))
	ops.define(Integer(400), operatorSpecifierYFX, NewAtom(`>>`))
	//  op(200, xfx, **).
	ops.define(Integer(200), operatorSpecifierXFX, NewAtom(`**`))
	//  op(200, xfy, ^).
	ops.define(Integer(200), operatorSpecifierXFY, NewAtom(`^`))
	//  op(200, fy, [+, -, \]).
	ops.define(Integer(200), operatorSpecifierFY, NewAtom(`+`))
	ops.define(Integer(200), operatorSpecifierFY, NewAtom(`-`))
	ops.define(Integer(200), operatorSpecifierFY, NewAtom(`\`))

	return &module{
		operators:  ops,
		procedures: map[predicateIndicator]procedureEntry{},
	}
}

func (m *module) reset() {
	*m = *newModule()
}

// Register0 registers a predicate of arity 0.
func (m *module) Register0(name string, p Predicate0) {
	if m.procedures == nil {
		m.procedures = map[predicateIndicator]procedureEntry{}
	}
	pi := predicateIndicator{name: NewAtom(name), arity: 0}
	e := m.procedures[pi]
	e.public = false
	e.dynamic = false
	e.procedure = p
	m.procedures[pi] = e
}

// Register1 registers a predicate of arity 1.
func (m *module) Register1(name string, p Predicate1) {
	if m.procedures == nil {
		m.procedures = map[predicateIndicator]procedureEntry{}
	}
	pi := predicateIndicator{name: NewAtom(name), arity: 1}
	e := m.procedures[pi]
	e.public = false
	e.dynamic = false
	e.procedure = p
	m.procedures[pi] = e
}

// Register2 registers a predicate of arity 2.
func (m *module) Register2(name string, p Predicate2) {
	if m.procedures == nil {
		m.procedures = map[predicateIndicator]procedureEntry{}
	}
	pi := predicateIndicator{name: NewAtom(name), arity: 2}
	e := m.procedures[pi]
	e.public = false
	e.dynamic = false
	e.procedure = p
	m.procedures[pi] = e
}

// Register3 registers a predicate of arity 3.
func (m *module) Register3(name string, p Predicate3) {
	if m.procedures == nil {
		m.procedures = map[predicateIndicator]procedureEntry{}
	}
	pi := predicateIndicator{name: NewAtom(name), arity: 3}
	e := m.procedures[pi]
	e.public = false
	e.dynamic = false
	e.procedure = p
	m.procedures[pi] = e
}

// Register4 registers a predicate of arity 4.
func (m *module) Register4(name string, p Predicate4) {
	if m.procedures == nil {
		m.procedures = map[predicateIndicator]procedureEntry{}
	}
	pi := predicateIndicator{name: NewAtom(name), arity: 4}
	e := m.procedures[pi]
	e.public = false
	e.dynamic = false
	e.procedure = p
	m.procedures[pi] = e
}

// Register5 registers a predicate of arity 5.
func (m *module) Register5(name string, p Predicate5) {
	if m.procedures == nil {
		m.procedures = map[predicateIndicator]procedureEntry{}
	}
	pi := predicateIndicator{name: NewAtom(name), arity: 5}
	e := m.procedures[pi]
	e.public = false
	e.dynamic = false
	e.procedure = p
	m.procedures[pi] = e
}

// Register6 registers a predicate of arity 6.
func (m *module) Register6(name string, p Predicate6) {
	if m.procedures == nil {
		m.procedures = map[predicateIndicator]procedureEntry{}
	}
	pi := predicateIndicator{name: NewAtom(name), arity: 6}
	e := m.procedures[pi]
	e.public = false
	e.dynamic = false
	e.procedure = p
	m.procedures[pi] = e
}

// Register7 registers a predicate of arity 7.
func (m *module) Register7(name string, p Predicate7) {
	if m.procedures == nil {
		m.procedures = map[predicateIndicator]procedureEntry{}
	}
	pi := predicateIndicator{name: NewAtom(name), arity: 7}
	e := m.procedures[pi]
	e.public = false
	e.dynamic = false
	e.procedure = p
	m.procedures[pi] = e
}

// Register8 registers a predicate of arity 8.
func (m *module) Register8(name string, p Predicate8) {
	if m.procedures == nil {
		m.procedures = map[predicateIndicator]procedureEntry{}
	}
	pi := predicateIndicator{name: NewAtom(name), arity: 8}
	e := m.procedures[pi]
	e.public = false
	e.dynamic = false
	e.procedure = p
	m.procedures[pi] = e
}

func (m *module) Import(from *module, importList []predicateIndicator) {
	if m.procedures == nil {
		m.procedures = map[predicateIndicator]procedureEntry{}
	}
	for pi, e := range from.procedures {
		if !e.exported {
			continue
		}

		if importList != nil && !slices.Contains(importList, pi) {
			continue
		}

	}
}

func (m *module) flushClauseBuf() error {
	if len(m.buf) == 0 {
		return nil
	}

	pi := m.buf[0].pi
	e, ok := m.procedures[pi]
	if !ok {
		e.procedure = clauses{}
		if m.procedures == nil {
			m.procedures = map[predicateIndicator]procedureEntry{}
		}
		m.procedures[pi] = e
	}

	if e.procedure == nil {
		e.procedure = clauses{}
	}

	cs, ok := e.procedure.(clauses)
	if !ok {
		return permissionError(operationModify, permissionTypeStaticProcedure, pi, nil)
	}

	if len(cs) > 0 && !e.discontiguous {
		return &discontiguousError{pi: pi}
	}
	e.procedure = append(cs, m.buf...)
	m.procedures[pi] = e
	m.buf = m.buf[:0]
	return nil
}

type unknownAction int

const (
	unknownError unknownAction = iota
	unknownFail
	unknownWarning
)

func (u unknownAction) String() string {
	return [...]string{
		unknownError:   "error",
		unknownFail:    "fail",
		unknownWarning: "warning",
	}[u]
}

type procedureEntry struct {
	dynamic       bool
	public        bool
	builtIn       bool
	multifile     bool
	exported      bool
	metaPredicate []metaArgumentSpecifier
	importedFrom  Atom
	definedIn     Atom
	discontiguous bool
	procedure     procedure
}

type metaArgumentSpecifier struct {
	atom    Atom
	integer Integer
}

func (m *metaArgumentSpecifier) needsModuleNameExpansion() bool {
	return m.atom == atomColon || m.integer > 0
}

type procedure interface {
	call(*VM, []Term, Cont, *Env) *Promise
}

// discontiguousError is an error that the user-defined predicate is defined by clauses which are not consecutive read-terms.
type discontiguousError struct {
	pi predicateIndicator
}

func (e *discontiguousError) Error() string {
	return fmt.Sprintf("%s is discontiguous", e.pi)
}
