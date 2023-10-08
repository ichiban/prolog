package engine

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
}

// Name returns the module name.
func (m *Module) Name() Atom {
	return m.name
}

// Register0 registers a predicate of arity 0.
func (m *Module) Register0(name Atom, p Predicate0) {
	if m.procedures == nil {
		m.procedures = map[procedureIndicator]procedure{}
	}
	m.procedures[procedureIndicator{name: name, arity: 0}] = p
}

// Register1 registers a predicate of arity 1.
func (m *Module) Register1(name Atom, p Predicate1) {
	if m.procedures == nil {
		m.procedures = map[procedureIndicator]procedure{}
	}
	m.procedures[procedureIndicator{name: name, arity: 1}] = p
}

// Register2 registers a predicate of arity 2.
func (m *Module) Register2(name Atom, p Predicate2) {
	if m.procedures == nil {
		m.procedures = map[procedureIndicator]procedure{}
	}
	m.procedures[procedureIndicator{name: name, arity: 2}] = p
}

// Register3 registers a predicate of arity 3.
func (m *Module) Register3(name Atom, p Predicate3) {
	if m.procedures == nil {
		m.procedures = map[procedureIndicator]procedure{}
	}
	m.procedures[procedureIndicator{name: name, arity: 3}] = p
}

// Register4 registers a predicate of arity 4.
func (m *Module) Register4(name Atom, p Predicate4) {
	if m.procedures == nil {
		m.procedures = map[procedureIndicator]procedure{}
	}
	m.procedures[procedureIndicator{name: name, arity: 4}] = p
}

// Register5 registers a predicate of arity 5.
func (m *Module) Register5(name Atom, p Predicate5) {
	if m.procedures == nil {
		m.procedures = map[procedureIndicator]procedure{}
	}
	m.procedures[procedureIndicator{name: name, arity: 5}] = p
}

// Register6 registers a predicate of arity 6.
func (m *Module) Register6(name Atom, p Predicate6) {
	if m.procedures == nil {
		m.procedures = map[procedureIndicator]procedure{}
	}
	m.procedures[procedureIndicator{name: name, arity: 6}] = p
}

// Register7 registers a predicate of arity 7.
func (m *Module) Register7(name Atom, p Predicate7) {
	if m.procedures == nil {
		m.procedures = map[procedureIndicator]procedure{}
	}
	m.procedures[procedureIndicator{name: name, arity: 7}] = p
}

// Register8 registers a predicate of arity 8.
func (m *Module) Register8(name Atom, p Predicate8) {
	if m.procedures == nil {
		m.procedures = map[procedureIndicator]procedure{}
	}
	m.procedures[procedureIndicator{name: name, arity: 8}] = p
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
