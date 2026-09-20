package runtime

import (
	"fmt"
	"maps"
	"slices"
	"strings"

	"github.com/ichiban/prolog/v2/internal/syntax"

	"github.com/ichiban/prolog/v2/internal/term"
	"github.com/ichiban/prolog/v2/internal/wam"
)

// The module system follows "Implementing a Module System for SICStus Prolog"
// (Stefan Andersson, SICS technical report T91:08), which is Quintus
// compatible: procedure based, so atoms and functors stay global and only
// predicates are local to a module, and flat, so every module is visible to
// every other.

var (
	atomPrologModule = term.NewAtom("prolog")
	atomUserModule   = term.NewAtom("user")
	atomColon        = term.NewAtomRune(':')
)

// Module is a module record. There are two predefined ones: prolog, where all
// the built in predicates reside, and user, the default module for predicates
// with no module specification.
type Module struct {
	Name term.Atom

	// File is the file the module declaration was read from, if any.
	File string

	// Exports is the public list: the predicates other modules may import.
	Exports map[term.Functor]struct{}

	// Imports says which module each imported predicate comes from. The origin
	// may itself have imported it, i.e. re-exported it.
	Imports map[term.Functor]term.Atom

	// Meta holds the meta_predicate declarations: for each declared predicate,
	// which of its arguments are module name expanded.
	Meta map[term.Functor][]bool
}

// Module returns the record of the named module, creating it if this is the
// first time it's mentioned. A module can be defined dynamically just by
// loading or asserting predicates to it.
func (e *Engine) module(name term.Atom) *Module {
	if e.Modules == nil {
		e.Modules = map[term.Atom]*Module{}
	}
	m, ok := e.Modules[name]
	if !ok {
		m = &Module{
			Name:    name,
			Exports: map[term.Functor]struct{}{},
			Imports: map[term.Functor]term.Atom{},
			Meta:    map[term.Functor][]bool{},
		}
		e.Modules[name] = m
	}
	return m
}

// Lookup finds the predicate that a call to p.Functor from module p.Module
// refers to: the module's own definition first, then whatever it imports,
// following a re-export to the module that defines it, and finally the prolog
// module, whose built in predicates are visible in every module.
//
// ponytail: two map lookups per call to a built-in, where the report caches
// the origin's definition record in the importing module. Cache it here too if
// a profile ever says the second lookup matters.
func (e *Engine) Lookup(p term.Procedure) (wam.Predicate, bool) {
	// The bound is the longest possible re-export chain; it also stops a cycle.
	for range len(e.Modules) + 1 {
		if d, ok := e.Predicates[p]; ok {
			return d, true
		}
		m, ok := e.Modules[p.Module]
		if !ok {
			break
		}
		from, ok := m.Imports[p.Functor]
		if !ok {
			break
		}
		p.Module = from
	}
	d, ok := e.Predicates[term.NewProcedure(atomPrologModule, p.Functor)]
	return d, ok
}

// TypeIn returns the type-in module, where goals issued at the top level with
// no module specification are called.
func (e *Engine) TypeIn() term.Atom {
	if e.Module == (term.Atom{}) {
		return atomUserModule
	}
	return e.Module
}

// Unqualify strips the module prefixes of a module name expanded term,
// returning the term and the module it's to be understood in. An unprefixed
// term belongs to module def.
func (e *Engine) Unqualify(t term.Cell, def term.Atom) (term.Cell, term.Atom) {
	m := def
	for {
		t = e.Deref(t)
		f, ok := e.Functor(t)
		if !ok || f != term.NewFunctor(atomColon, 2) {
			return t, m
		}
		p := e.Deref(e.Arg(t, 0))
		a, ok := e.Atom(p)
		if !ok {
			return t, m
		}
		m, t = a, e.Arg(t, 1)
	}
}

// Qualify prefixes t with the module name, unless it already carries one.
func (e *Engine) Qualify(m term.Atom, t term.Cell) (term.Cell, error) {
	t = e.Deref(t)
	if f, ok := e.Functor(t); ok && f == term.NewFunctor(atomColon, 2) {
		return t, nil
	}
	p, err := e.PutAtom(m)
	if err != nil {
		return term.Cell{}, err
	}
	return e.PutCompound(atomColon, p, t)
}

// Import makes the predicates of module from visible in module to. Predicates
// that aren't exported, or that clash with what the receiving module already
// has, are reported and skipped.
func (e *Engine) Import(from, to term.Atom, only []term.Functor) {
	if e.Warn == nil {
		e.Warn = func(error) {}
	}
	src, dst := e.module(from), e.module(to)
	if from == to {
		return
	}
	fs := only
	if fs == nil {
		fs = slices.Collect(maps.Keys(src.Exports))
	}
	for _, f := range fs {
		if _, ok := src.Exports[f]; !ok {
			e.Warn(fmt.Errorf("%s:%s is not exported", from, unbinarize(f)))
			continue
		}
		if _, ok := e.Predicates[term.NewProcedure(to, f)]; ok {
			e.Warn(fmt.Errorf("%s:%s clashes with a local definition", to, unbinarize(f)))
			continue
		}
		if other, ok := dst.Imports[f]; ok && other != from {
			e.Warn(fmt.Errorf("%s:%s is already imported from %s", to, unbinarize(f), other))
			continue
		}
		dst.Imports[f] = from
		// A meta predicate has to be expanded the same way in the importing
		// module, so its declaration travels with it.
		if spec, ok := src.Meta[f]; ok {
			dst.Meta[f] = spec
		}
	}
}

// mustBeModule reads a module name, which is an atom.
func (e *Engine) mustBeModule(t term.Cell) (term.Atom, error) {
	t = e.Deref(t)
	if _, ok := e.Variable(t); ok {
		return term.Atom{}, &InstantiationError{Location: e.location}
	}
	a, ok := e.Atom(t)
	if !ok {
		return term.Atom{}, &TypeError{
			ValidType: term.NewAtom("atom"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}
	return a, nil
}

// MetaSpec returns which arguments of a call to f from module m are module
// name expanded, or nil if f isn't a meta predicate there.
func (e *Engine) MetaSpec(m term.Atom, f term.Functor) []bool {
	for _, n := range []term.Atom{m, atomPrologModule} {
		if mod, ok := e.Modules[n]; ok {
			if spec, ok := mod.Meta[f]; ok {
				return spec
			}
		}
	}
	return nil
}

// unbinarize turns the functor of a binarized predicate back into the
// predicate indicator a user would recognize.
func unbinarize(f term.Functor) term.Functor {
	return term.NewFunctor(f.Name(), f.Arity()-1)
}

func (m *Module) String() string {
	var sb strings.Builder
	_, _ = fmt.Fprintf(&sb, "module %s", m.Name)
	if m.File != "" {
		_, _ = fmt.Fprintf(&sb, " (%s)", m.File)
	}
	return sb.String()
}
