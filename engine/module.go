package engine

import (
	"io/fs"
	"os"
)

// Module is a virtual module file that stores native predicates.
type Module struct {
	fs.File
	procedures map[procedureIndicator]procedureEntry
}

// Register0 registers a predicate of arity 0.
func (m *Module) Register0(name Atom, p Predicate0) {
	if m.procedures == nil {
		m.procedures = map[procedureIndicator]procedureEntry{}
	}
	m.procedures[procedureIndicator{name: name, arity: 0}] = procedureEntry{builtIn: true, exported: true, procedure: p}
}

// Register1 registers a predicate of arity 1.
func (m *Module) Register1(name Atom, p Predicate1) {
	if m.procedures == nil {
		m.procedures = map[procedureIndicator]procedureEntry{}
	}
	m.procedures[procedureIndicator{name: name, arity: 1}] = procedureEntry{builtIn: true, exported: true, procedure: p}
}

// Register2 registers a predicate of arity 2.
func (m *Module) Register2(name Atom, p Predicate2) {
	if m.procedures == nil {
		m.procedures = map[procedureIndicator]procedureEntry{}
	}
	m.procedures[procedureIndicator{name: name, arity: 2}] = procedureEntry{builtIn: true, exported: true, procedure: p}
}

// Register3 registers a predicate of arity 3.
func (m *Module) Register3(name Atom, p Predicate3) {
	if m.procedures == nil {
		m.procedures = map[procedureIndicator]procedureEntry{}
	}
	m.procedures[procedureIndicator{name: name, arity: 3}] = procedureEntry{builtIn: true, exported: true, procedure: p}
}

// Register4 registers a predicate of arity 4.
func (m *Module) Register4(name Atom, p Predicate4) {
	if m.procedures == nil {
		m.procedures = map[procedureIndicator]procedureEntry{}
	}
	m.procedures[procedureIndicator{name: name, arity: 4}] = procedureEntry{builtIn: true, exported: true, procedure: p}
}

// Register5 registers a predicate of arity 5.
func (m *Module) Register5(name Atom, p Predicate5) {
	if m.procedures == nil {
		m.procedures = map[procedureIndicator]procedureEntry{}
	}
	m.procedures[procedureIndicator{name: name, arity: 5}] = procedureEntry{builtIn: true, exported: true, procedure: p}
}

// Register6 registers a predicate of arity 6.
func (m *Module) Register6(name Atom, p Predicate6) {
	if m.procedures == nil {
		m.procedures = map[procedureIndicator]procedureEntry{}
	}
	m.procedures[procedureIndicator{name: name, arity: 6}] = procedureEntry{builtIn: true, exported: true, procedure: p}
}

// Register7 registers a predicate of arity 7.
func (m *Module) Register7(name Atom, p Predicate7) {
	if m.procedures == nil {
		m.procedures = map[procedureIndicator]procedureEntry{}
	}
	m.procedures[procedureIndicator{name: name, arity: 7}] = procedureEntry{builtIn: true, exported: true, procedure: p}
}

// Register8 registers a predicate of arity 8.
func (m *Module) Register8(name Atom, p Predicate8) {
	if m.procedures == nil {
		m.procedures = map[procedureIndicator]procedureEntry{}
	}
	m.procedures[procedureIndicator{name: name, arity: 8}] = procedureEntry{builtIn: true, exported: true, procedure: p}
}

type RealFS struct{}

func (r RealFS) Open(name string) (fs.File, error) {
	return os.Open(name)
}

type MapFS map[string]fs.File

func (m MapFS) Open(name string) (fs.File, error) {
	f, ok := m[name]
	if !ok {
		return nil, fs.ErrNotExist
	}
	return f, nil
}

type OverlayFS []fs.FS

func (o OverlayFS) Open(name string) (fs.File, error) {
	for _, e := range o {
		if f, err := e.Open(name); err == nil {
			return f, nil
		}
	}
	return nil, fs.ErrNotExist
}
