package internal

import (
	"context"
	"errors"
	"fmt"
	"iter"
	"slices"
)

type Module struct {
	procedures map[Functor]procedureEntry
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

func NewModule() *Module {
	ops := operators{}
	//  op(1200, xfx, [:-, -->]).
	ops.define(1200, OperatorSpecifierXFX, NewAtom(`:-`))
	ops.define(1200, OperatorSpecifierXFX, NewAtom(`-->`))
	//  op(1200, fx, [:-, ?-]).
	ops.define(1200, OperatorSpecifierFX, NewAtom(`:-`))
	ops.define(1200, OperatorSpecifierFX, NewAtom(`?-`))
	//  op(1105, xfy, '|').
	ops.define(1105, OperatorSpecifierXFY, NewAtom(`|`))
	//  op(1100, xfy, ;).
	ops.define(1100, OperatorSpecifierXFY, NewAtom(`;`))
	//  op(1050, xfy, ->).
	ops.define(1050, OperatorSpecifierXFY, NewAtom(`->`))
	//  op(1000, xfy, ',').
	ops.define(1000, OperatorSpecifierXFY, NewAtom(`,`))
	//  op(900, fy, \+).
	ops.define(900, OperatorSpecifierFY, NewAtom(`\+`))
	//  op(700, xfx, [=, \=]).
	ops.define(700, OperatorSpecifierXFX, NewAtom(`=`))
	ops.define(700, OperatorSpecifierXFX, NewAtom(`\=`))
	//  op(700, xfx, [==, \==, @<, @=<, @>, @>=]).
	ops.define(700, OperatorSpecifierXFX, NewAtom(`==`))
	ops.define(700, OperatorSpecifierXFX, NewAtom(`\==`))
	ops.define(700, OperatorSpecifierXFX, NewAtom(`@<`))
	ops.define(700, OperatorSpecifierXFX, NewAtom(`@=<`))
	ops.define(700, OperatorSpecifierXFX, NewAtom(`@>`))
	ops.define(700, OperatorSpecifierXFX, NewAtom(`@>=`))
	//  op(700, xfx, =..).
	ops.define(700, OperatorSpecifierXFX, NewAtom(`=..`))
	//  op(700, xfx, [is, =:=, =\=, <, =<, >, >=]).
	ops.define(700, OperatorSpecifierXFX, NewAtom(`is`))
	ops.define(700, OperatorSpecifierXFX, NewAtom(`=:=`))
	ops.define(700, OperatorSpecifierXFX, NewAtom(`=\=`))
	ops.define(700, OperatorSpecifierXFX, NewAtom(`<`))
	ops.define(700, OperatorSpecifierXFX, NewAtom(`=<`))
	ops.define(700, OperatorSpecifierXFX, NewAtom(`>`))
	ops.define(700, OperatorSpecifierXFX, NewAtom(`>=`))
	//  op(600, xfy, :).
	ops.define(600, OperatorSpecifierXFY, NewAtom(`:`))
	//  op(500, yfx, [+, -, /\, \/]).
	ops.define(500, OperatorSpecifierYFX, NewAtom(`+`))
	ops.define(500, OperatorSpecifierYFX, NewAtom(`-`))
	ops.define(500, OperatorSpecifierYFX, NewAtom(`/\`))
	ops.define(500, OperatorSpecifierYFX, NewAtom(`\/`))
	//  op(400, yfx, [*, /, //, div, rem, mod, <<, >>]).
	ops.define(400, OperatorSpecifierYFX, NewAtom(`*`))
	ops.define(400, OperatorSpecifierYFX, NewAtom(`/`))
	ops.define(400, OperatorSpecifierYFX, NewAtom(`//`))
	ops.define(400, OperatorSpecifierYFX, NewAtom(`div`))
	ops.define(400, OperatorSpecifierYFX, NewAtom(`rem`))
	ops.define(400, OperatorSpecifierYFX, NewAtom(`mod`))
	ops.define(400, OperatorSpecifierYFX, NewAtom(`<<`))
	ops.define(400, OperatorSpecifierYFX, NewAtom(`>>`))
	//  op(200, xfx, **).
	ops.define(200, OperatorSpecifierXFX, NewAtom(`**`))
	//  op(200, xfy, ^).
	ops.define(200, OperatorSpecifierXFY, NewAtom(`^`))
	//  op(200, fy, [+, -, \]).
	ops.define(200, OperatorSpecifierFY, NewAtom(`+`))
	ops.define(200, OperatorSpecifierFY, NewAtom(`-`))
	ops.define(200, OperatorSpecifierFY, NewAtom(`\`))

	return &Module{
		operators:  ops,
		procedures: map[Functor]procedureEntry{},
	}
}

func (m *Module) reset() {
	*m = *NewModule()
}

func (m *Module) SetOperator(priority int16, spec OperatorSpecifier, name string) error {
	n := NewAtom(name)
	if err := m.validateOp(priority, spec, n); err != nil {
		return err
	}

	if class := spec.class(); m.operators.definedInClass(n, spec.class()) {
		m.operators.remove(n, class)
	}

	m.operators.define(priority, spec, n)

	return nil
}

func (m *Module) Operators() iter.Seq[Operator] {
	return func(yield func(Operator) bool) {
		for _, ops := range m.operators {
			for _, op := range ops {
				if op == (Operator{}) {
					continue
				}
				yield(op)
			}
		}
	}
}

var (
	ErrOpCreation     = errors.New("operator creation error")
	ErrOpModification = errors.New("operator modification error")
)

func (m *Module) validateOp(p int16, spec OperatorSpecifier, name Atom) error {
	switch name {
	case Atom('.'):
		if m.operators.definedInClass(name, operatorClassInfix) {
			return ErrOpModification
		}
	case Atom('|'):
		if spec.class() != operatorClassInfix || (p > 0 && p < 1001) {
			if m.operators.definedInClass(name, operatorClassInfix) {
				return ErrOpModification
			}
			return ErrOpCreation
		}
	case atomEmptyBlock, atomEmptyList:
		return ErrOpCreation
	}

	// 6.3.4.3 There shall not be an infix and a postfix Operator with the same Name.
	switch spec.class() {
	case operatorClassInfix:
		if m.operators.definedInClass(name, operatorClassPostfix) {
			return ErrOpCreation
		}
	case operatorClassPostfix:
		if m.operators.definedInClass(name, operatorClassInfix) {
			return ErrOpCreation
		}
	default:
		break
	}

	return nil
}

// SetPredicate0 registers a predicate of Arity 0.
func (m *Module) SetPredicate0(name string, p Predicate0) {
	if m.procedures == nil {
		m.procedures = map[Functor]procedureEntry{}
	}
	pi := Functor{Name: NewAtom(name), Arity: 0}
	e := m.procedures[pi]
	e.public = false
	e.dynamic = false
	e.procedure = p
	m.procedures[pi] = e
}

// SetPredicate1 registers a predicate of Arity 1.
func (m *Module) SetPredicate1(name string, p Predicate1) {
	if m.procedures == nil {
		m.procedures = map[Functor]procedureEntry{}
	}
	pi := Functor{Name: NewAtom(name), Arity: 1}
	e := m.procedures[pi]
	e.public = false
	e.dynamic = false
	e.procedure = p
	m.procedures[pi] = e
}

// SetPredicate2 registers a predicate of Arity 2.
func (m *Module) SetPredicate2(name string, p Predicate2) {
	if m.procedures == nil {
		m.procedures = map[Functor]procedureEntry{}
	}
	pi := Functor{Name: NewAtom(name), Arity: 2}
	e := m.procedures[pi]
	e.public = false
	e.dynamic = false
	e.procedure = p
	m.procedures[pi] = e
}

// SetPredicate3 registers a predicate of Arity 3.
func (m *Module) SetPredicate3(name string, p Predicate3) {
	if m.procedures == nil {
		m.procedures = map[Functor]procedureEntry{}
	}
	pi := Functor{Name: NewAtom(name), Arity: 3}
	e := m.procedures[pi]
	e.public = false
	e.dynamic = false
	e.procedure = p
	m.procedures[pi] = e
}

// SetPredicate4 registers a predicate of Arity 4.
func (m *Module) SetPredicate4(name string, p Predicate4) {
	if m.procedures == nil {
		m.procedures = map[Functor]procedureEntry{}
	}
	pi := Functor{Name: NewAtom(name), Arity: 4}
	e := m.procedures[pi]
	e.public = false
	e.dynamic = false
	e.procedure = p
	m.procedures[pi] = e
}

// SetPredicate5 registers a predicate of Arity 5.
func (m *Module) SetPredicate5(name string, p Predicate5) {
	if m.procedures == nil {
		m.procedures = map[Functor]procedureEntry{}
	}
	pi := Functor{Name: NewAtom(name), Arity: 5}
	e := m.procedures[pi]
	e.public = false
	e.dynamic = false
	e.procedure = p
	m.procedures[pi] = e
}

// SetPredicate6 registers a predicate of Arity 6.
func (m *Module) SetPredicate6(name string, p Predicate6) {
	if m.procedures == nil {
		m.procedures = map[Functor]procedureEntry{}
	}
	pi := Functor{Name: NewAtom(name), Arity: 6}
	e := m.procedures[pi]
	e.public = false
	e.dynamic = false
	e.procedure = p
	m.procedures[pi] = e
}

// SetPredicate7 registers a predicate of Arity 7.
func (m *Module) SetPredicate7(name string, p Predicate7) {
	if m.procedures == nil {
		m.procedures = map[Functor]procedureEntry{}
	}
	pi := Functor{Name: NewAtom(name), Arity: 7}
	e := m.procedures[pi]
	e.public = false
	e.dynamic = false
	e.procedure = p
	m.procedures[pi] = e
}

// SetPredicate8 registers a predicate of Arity 8.
func (m *Module) SetPredicate8(name string, p Predicate8) {
	if m.procedures == nil {
		m.procedures = map[Functor]procedureEntry{}
	}
	pi := Functor{Name: NewAtom(name), Arity: 8}
	e := m.procedures[pi]
	e.public = false
	e.dynamic = false
	e.procedure = p
	m.procedures[pi] = e
}

func (m *Module) Import(from *Module, importList []Functor) {
	if m.procedures == nil {
		m.procedures = map[Functor]procedureEntry{}
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

func (m *Module) flushClauseBuf(p *TermPool) error {
	if len(m.buf) == 0 {
		return nil
	}

	pi := m.buf[0].pi
	e, ok := m.procedures[pi]
	if !ok {
		e.procedure = clauses{}
		if m.procedures == nil {
			m.procedures = map[Functor]procedureEntry{}
		}
		m.procedures[pi] = e
	}

	if e.procedure == nil {
		e.procedure = clauses{}
	}

	cs, ok := e.procedure.(clauses)
	if !ok {
		c, err := p.PutFunctor(pi)
		if err != nil {
			return err
		}
		return &PermissionError{Operation: NewAtom("modify"), PermissionType: NewAtom("static_procedure"), Culprit: c}
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
	integer int
}

func (m *metaArgumentSpecifier) needsModuleNameExpansion() bool {
	return m.atom == Atom(':') || m.integer > 0
}

type procedure interface {
	call(context.Context, *VM, []Term, Cont) Promise
}

// discontiguousError is an error that the user-defined predicate is defined by clauses which are not consecutive read-Terms.
type discontiguousError struct {
	pi Functor
}

func (e *discontiguousError) Error() string {
	return fmt.Sprintf("%s is discontiguous", e.pi)
}

// Predicate0 is a predicate of Arity 0.
type Predicate0 func(context.Context) Promise

func (p Predicate0) call(ctx context.Context, vm *VM, args []Term, k Cont) Promise {
	if len(args) != 0 {
		return Error(&wrongNumberOfArgumentsError{expected: 0, actual: args})
	}

	ctx = context.WithValue(ctx, CtxKeyVM{}, vm)
	ctx = WithCont(ctx, k)
	return p(ctx)
}

// Predicate1 is a predicate of Arity 1.
type Predicate1 func(context.Context, Term) Promise

func (p Predicate1) call(ctx context.Context, vm *VM, args []Term, k Cont) Promise {
	if len(args) != 1 {
		return Error(&wrongNumberOfArgumentsError{expected: 1, actual: args})
	}

	ctx = context.WithValue(ctx, CtxKeyVM{}, vm)
	ctx = WithCont(ctx, k)
	return p(ctx, args[0])
}

// Predicate2 is a predicate of Arity 2.
type Predicate2 func(context.Context, Term, Term) Promise

func (p Predicate2) call(ctx context.Context, vm *VM, args []Term, k Cont) Promise {
	if len(args) != 2 {
		return Error(&wrongNumberOfArgumentsError{expected: 2, actual: args})
	}

	ctx = context.WithValue(ctx, CtxKeyVM{}, vm)
	ctx = WithCont(ctx, k)
	return p(ctx, args[0], args[1])
}

// Predicate3 is a predicate of Arity 3.
type Predicate3 func(context.Context, Term, Term, Term) Promise

func (p Predicate3) call(ctx context.Context, vm *VM, args []Term, k Cont) Promise {
	if len(args) != 3 {
		return Error(&wrongNumberOfArgumentsError{expected: 3, actual: args})
	}

	ctx = context.WithValue(ctx, CtxKeyVM{}, vm)
	ctx = WithCont(ctx, k)
	return p(ctx, args[0], args[1], args[2])
}

// Predicate4 is a predicate of Arity 4.
type Predicate4 func(context.Context, Term, Term, Term, Term) Promise

func (p Predicate4) call(ctx context.Context, vm *VM, args []Term, k Cont) Promise {
	if len(args) != 4 {
		return Error(&wrongNumberOfArgumentsError{expected: 4, actual: args})
	}

	ctx = context.WithValue(ctx, CtxKeyVM{}, vm)
	ctx = WithCont(ctx, k)
	return p(ctx, args[0], args[1], args[2], args[3])
}

// Predicate5 is a predicate of Arity 5.
type Predicate5 func(context.Context, Term, Term, Term, Term, Term) Promise

func (p Predicate5) call(ctx context.Context, vm *VM, args []Term, k Cont) Promise {
	if len(args) != 5 {
		return Error(&wrongNumberOfArgumentsError{expected: 5, actual: args})
	}

	ctx = context.WithValue(ctx, CtxKeyVM{}, vm)
	ctx = WithCont(ctx, k)
	return p(ctx, args[0], args[1], args[2], args[3], args[4])
}

// Predicate6 is a predicate of Arity 6.
type Predicate6 func(context.Context, Term, Term, Term, Term, Term, Term) Promise

func (p Predicate6) call(ctx context.Context, vm *VM, args []Term, k Cont) Promise {
	if len(args) != 6 {
		return Error(&wrongNumberOfArgumentsError{expected: 6, actual: args})
	}

	ctx = context.WithValue(ctx, CtxKeyVM{}, vm)
	ctx = WithCont(ctx, k)
	return p(ctx, args[0], args[1], args[2], args[3], args[4], args[5])
}

// Predicate7 is a predicate of Arity 7.
type Predicate7 func(context.Context, Term, Term, Term, Term, Term, Term, Term) Promise

func (p Predicate7) call(ctx context.Context, vm *VM, args []Term, k Cont) Promise {
	if len(args) != 7 {
		return Error(&wrongNumberOfArgumentsError{expected: 7, actual: args})
	}

	ctx = context.WithValue(ctx, CtxKeyVM{}, vm)
	ctx = WithCont(ctx, k)
	return p(ctx, args[0], args[1], args[2], args[3], args[4], args[5], args[6])
}

// Predicate8 is a predicate of Arity 8.
type Predicate8 func(context.Context, Term, Term, Term, Term, Term, Term, Term, Term) Promise

func (p Predicate8) call(ctx context.Context, vm *VM, args []Term, k Cont) Promise {
	if len(args) != 8 {
		return Error(&wrongNumberOfArgumentsError{expected: 8, actual: args})
	}

	ctx = context.WithValue(ctx, CtxKeyVM{}, vm)
	ctx = WithCont(ctx, k)
	return p(ctx, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7])
}

type wrongNumberOfArgumentsError struct {
	expected int
	actual   []Term
}

func (e *wrongNumberOfArgumentsError) Error() string {
	return fmt.Sprintf("wrong number of arguments: expected=%d, actual=%d", e.expected, e.actual)
}

func Continue(ctx context.Context) Promise {
	k := ContextCont(ctx)
	if k == nil {
		return Bool(false)
	}
	return k(ctx)
}
