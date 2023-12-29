package engine

import "fmt"

type Module struct {
	name Atom

	procedures map[procedureIndicator]procedure
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

// Name returns the module name.
func (m *Module) Name() Atom {
	return m.name
}

// Register0 registers a predicate of arity 0.
func (m *Module) Register0(name string, p Predicate0) {
	if m.procedures == nil {
		m.procedures = map[procedureIndicator]procedure{}
	}
	m.procedures[procedureIndicator{name: NewAtom(name), arity: 0}] = p
}

// Register1 registers a predicate of arity 1.
func (m *Module) Register1(name string, p Predicate1) {
	if m.procedures == nil {
		m.procedures = map[procedureIndicator]procedure{}
	}
	m.procedures[procedureIndicator{name: NewAtom(name), arity: 1}] = p
}

// Register2 registers a predicate of arity 2.
func (m *Module) Register2(name string, p Predicate2) {
	if m.procedures == nil {
		m.procedures = map[procedureIndicator]procedure{}
	}
	m.procedures[procedureIndicator{name: NewAtom(name), arity: 2}] = p
}

// Register3 registers a predicate of arity 3.
func (m *Module) Register3(name string, p Predicate3) {
	if m.procedures == nil {
		m.procedures = map[procedureIndicator]procedure{}
	}
	m.procedures[procedureIndicator{name: NewAtom(name), arity: 3}] = p
}

// Register4 registers a predicate of arity 4.
func (m *Module) Register4(name string, p Predicate4) {
	if m.procedures == nil {
		m.procedures = map[procedureIndicator]procedure{}
	}
	m.procedures[procedureIndicator{name: NewAtom(name), arity: 4}] = p
}

// Register5 registers a predicate of arity 5.
func (m *Module) Register5(name string, p Predicate5) {
	if m.procedures == nil {
		m.procedures = map[procedureIndicator]procedure{}
	}
	m.procedures[procedureIndicator{name: NewAtom(name), arity: 5}] = p
}

// Register6 registers a predicate of arity 6.
func (m *Module) Register6(name string, p Predicate6) {
	if m.procedures == nil {
		m.procedures = map[procedureIndicator]procedure{}
	}
	m.procedures[procedureIndicator{name: NewAtom(name), arity: 6}] = p
}

// Register7 registers a predicate of arity 7.
func (m *Module) Register7(name string, p Predicate7) {
	if m.procedures == nil {
		m.procedures = map[procedureIndicator]procedure{}
	}
	m.procedures[procedureIndicator{name: NewAtom(name), arity: 7}] = p
}

// Register8 registers a predicate of arity 8.
func (m *Module) Register8(name string, p Predicate8) {
	if m.procedures == nil {
		m.procedures = map[procedureIndicator]procedure{}
	}
	m.procedures[procedureIndicator{name: NewAtom(name), arity: 8}] = p
}

func (m *Module) flushClauseBuf() error {
	if len(m.buf) == 0 {
		return nil
	}

	pi := m.buf[0].pi
	p, ok := m.procedures[pi]
	if !ok {
		p = &userDefined{}
		if m.procedures == nil {
			m.procedures = map[procedureIndicator]procedure{}
		}
		m.procedures[pi] = p
	}
	u, ok := p.(*userDefined)
	if !ok {
		return permissionError(operationModify, permissionTypeStaticProcedure, pi, nil)
	}
	if len(u.clauses) > 0 && !u.discontiguous {
		return &discontiguousError{pi: pi}
	}
	u.clauses = append(u.clauses, m.buf...)
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

type procedure interface {
	call(*VM, []Term, Cont, *Env) *Promise
}

// discontiguousError is an error that the user-defined predicate is defined by clauses which are not consecutive read-terms.
type discontiguousError struct {
	pi procedureIndicator
}

func (e *discontiguousError) Error() string {
	return fmt.Sprintf("%s is discontiguous", e.pi)
}
