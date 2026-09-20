package runtime

import (
	"context"
	"fmt"
	"iter"
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

// Qualify prefixes t with the module name, unless it already carries one. A
// ^/2 marker isn't a goal but says which variables of one are existentially
// quantified, so the prefix goes inside it; that's what the report's
// meta declaration of (^)/2 as (?, :) amounts to.
func (e *Engine) Qualify(m term.Atom, t term.Cell) (term.Cell, error) {
	t = e.Deref(t)
	if f, ok := e.Functor(t); ok && f == term.NewFunctor(atomColon, 2) {
		return t, nil
	}
	if f, ok := e.Functor(t); ok && f == term.NewFunctor(term.NewAtomRune('^'), 2) {
		g, err := e.Qualify(m, e.Arg(t, 1))
		if err != nil {
			return term.Cell{}, err
		}
		return e.PutCompound(term.NewAtomRune('^'), e.Arg(t, 0), g)
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

// predicateIndicators yields the Name/Arity elements of a list, a conjunction
// or a single indicator.
func (e *Engine) predicateIndicators(t term.Cell) iter.Seq2[term.Functor, error] {
	return func(yield func(term.Functor, error) bool) {
		for elem := range e.listOrSingleton(t) {
			pi, err := e.mustBePredicateIndicator(e.Deref(elem))
			if !yield(pi, err) {
				return
			}
		}
	}
}

// listOrSingleton yields the elements of a list, or the term itself if it
// isn't one. A conjunction counts as a list too, the way a declaration is
// often written.
func (e *Engine) listOrSingleton(t term.Cell) iter.Seq[term.Cell] {
	return func(yield func(term.Cell) bool) {
		t = e.Deref(t)
		if a, ok := e.Atom(t); ok && a == term.NewAtom("[]") {
			return
		}
		if f, ok := e.Functor(t); ok && f == term.NewFunctor(term.NewAtomRune('.'), 2) {
			for elem := range e.List(t) {
				if !yield(elem) {
					return
				}
			}
			return
		}
		for elem := range e.conjunction(t) {
			if !yield(elem) {
				return
			}
		}
	}
}

// conjunction yields the conjuncts of (A, B, ...), or the term itself.
func (e *Engine) conjunction(t term.Cell) iter.Seq[term.Cell] {
	return func(yield func(term.Cell) bool) {
		for {
			t = e.Deref(t)
			if f, ok := e.Functor(t); ok && f == functorAnd {
				if !yield(e.Arg(t, 0)) {
					return
				}
				t = e.Arg(t, 1)
				continue
			}
			_ = yield(t)
			return
		}
	}
}

// Colon2 is (:)/2: it calls Goal in Module. A goal handed to a meta predicate
// comes module name expanded, and this is what executes it.
func Colon2(ctx context.Context, a *Activation, module, goal, cont Ref) Promise {
	m, err := a.exec.mustBeModule(*module.cell)
	if err != nil {
		return a.Throw(err, cont)
	}
	g, m := a.exec.Unqualify(*goal.cell, m)
	return call(ctx, a, m, a.ref(g), cont)
}

// Module1 is module/1: it sets the type-in module.
func Module1(_ context.Context, e *Execution, name, cont term.Cell) Promise {
	m, err := e.mustBeModule(name)
	if err != nil {
		return e.Throw(err, cont)
	}
	e.module(m)
	e.Module = m
	return e.Success(cont)
}

// CurrentModule1 is current_module/1: Module is a module defined in the system.
func CurrentModule1(_ context.Context, a *Activation, module, cont Ref) Promise {
	return a.modules(cont, func(m *Module) ([]Ref, error) {
		n, err := a.exec.PutAtom(m.Name)
		if err != nil {
			return nil, err
		}
		return []Ref{module, a.ref(n)}, nil
	})
}

// CurrentModule2 is current_module/2: Module is the module defined in File.
func CurrentModule2(_ context.Context, a *Activation, module, file, cont Ref) Promise {
	return a.modules(cont, func(m *Module) ([]Ref, error) {
		if m.File == "" {
			return nil, nil
		}
		n, err := a.exec.PutAtom(m.Name)
		if err != nil {
			return nil, err
		}
		f, err := a.exec.PutAtom(term.NewAtom(m.File))
		if err != nil {
			return nil, err
		}
		return []Ref{module, a.ref(n), file, a.ref(f)}, nil
	})
}

// modules backtracks through the modules presently in the system, unifying
// each against what pairs returns for it. A module pairs says nothing about is
// skipped.
func (a *Activation) modules(cont Ref, pairs func(*Module) ([]Ref, error)) Promise {
	names := slices.SortedFunc(maps.Keys(a.exec.Modules), func(x, y term.Atom) int {
		return strings.Compare(x.String(), y.String())
	})
	return a.Nondet(func(yield func(Promise) bool) {
		for _, name := range names {
			ps, err := pairs(a.exec.Modules[name])
			if err != nil {
				_ = yield(a.Throw(err, cont))
				return
			}
			if ps == nil {
				continue
			}
			ok := true
			for i := 0; i < len(ps) && ok; i += 2 {
				ok, err = a.Unify(ps[i], ps[i+1])
				if err != nil {
					_ = yield(a.Throw(err, cont))
					return
				}
			}
			if !ok {
				if !yield(Failure()) {
					return
				}
				continue
			}
			if !yield(a.Success(cont)) {
				return
			}
		}
	})
}

// UseModule1 is use_module/1: it loads the files and imports everything they
// export into the type-in module.
func UseModule1(ctx context.Context, e *Execution, files, cont term.Cell) Promise {
	if err := e.useModule(ctx, files, term.Cell{}); err != nil {
		return e.Throw(err, cont)
	}
	return e.Success(cont)
}

// UseModule2 is use_module/2: it loads the file and imports the predicates of
// the list into the type-in module.
func UseModule2(ctx context.Context, e *Execution, files, publics, cont term.Cell) Promise {
	if err := e.useModule(ctx, files, publics); err != nil {
		return e.Throw(err, cont)
	}
	return e.Success(cont)
}

// useModule loads a module file and imports from it: everything it exports, or
// just the predicates of a list.
func (e *Engine) useModule(ctx context.Context, files, publics term.Cell) error {
	var only []term.Functor
	if publics != (term.Cell{}) {
		for pi, err := range e.predicateIndicators(publics) {
			if err != nil {
				return err
			}
			only = append(only, term.NewFunctor(pi.Name(), pi.Arity()+1))
		}
		if only == nil {
			return nil
		}
	}

	for file := range e.listOrSingleton(files) {
		fsName, filename, err := e.sourceFile(e.Deref(file))
		if err != nil {
			return err
		}

		m, ok := e.moduleOf(fsName, filename)
		if !ok {
			// use_module/1-2 loads like ensure_loaded/1.
			m, err = e.LoadFile(ctx, fsName, filename)
			if err != nil {
				return err
			}
		}
		e.Import(m, e.TypeIn(), only)
	}
	return nil
}

// sourceFile reads a file specification the way the loading predicates do:
// 2.12 of the report has them look for the name as given and, failing that,
// with a '.pl' suffix.
func (e *Engine) sourceFile(t term.Cell) (term.Atom, string, error) {
	fsName, filename, err := e.mustBeSourceSink(t)
	if err != nil {
		return term.Atom{}, "", err
	}
	if fsy, ok := e.FSs.Get(fsName); ok && !strings.HasSuffix(filename, ".pl") {
		if f, err := fsy.Open(filename); err != nil {
			filename += ".pl"
		} else {
			_ = f.Close()
		}
	}
	return fsName, filename, nil
}

// unqualifyClause strips a module prefix off the head of a clause, so that
// what goes to the database is the clause as the module holds it. Both
// M:(H :- B) and (M:H :- B) name the same clause of M.
func (e *Engine) unqualifyClause(c term.Cell, module term.Atom) (term.Cell, error) {
	c = e.Deref(c)
	f, ok := e.Functor(c)
	if !ok || f != term.NewFunctor(term.NewAtom(":-"), 2) {
		t, _ := e.Unqualify(c, module)
		return t, nil
	}
	h, _ := e.Unqualify(e.Arg(c, 0), module)
	return e.PutCompound(term.NewAtom(":-"), h, e.Arg(c, 1))
}

// ExpandMeta prefixes the meta expandable arguments of a goal with the module
// it's called in. 2.6 of the report expands goals at compile time, and also
// the ones issued at the top level or as a directive, which reach the system
// unexpanded; doing it on every metacall covers both, and costs nothing for a
// goal already expanded, whose arguments carry a prefix.
func (e *Engine) ExpandMeta(m term.Atom, goal term.Cell) (term.Cell, error) {
	goal = e.Deref(goal)
	f, ok := e.Functor(goal)
	if !ok {
		return goal, nil
	}
	spec := e.MetaSpec(m, f)
	if spec == nil {
		return goal, nil
	}

	args := slices.Collect(e.Args(goal))
	var changed bool
	for i, expand := range spec {
		if !expand || i >= len(args) {
			continue
		}
		q, err := e.Qualify(m, args[i])
		if err != nil {
			return term.Cell{}, err
		}
		if q != args[i] {
			args[i], changed = q, true
		}
	}
	if !changed {
		return goal, nil
	}
	return e.PutCompound(f.Name(), args...)
}
