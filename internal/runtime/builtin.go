// Ported to Go from BinProlog (github.com/ptarau/binprolog, src/extra.pl and
// related sources), Copyright (C) Paul Tarau, licensed under Apache-2.0.
// This file has been modified: translated to Go and adapted.

package runtime

import (
	"bytes"
	"context"
	"fmt"
	"iter"
	"maps"
	"slices"
	"strings"

	"github.com/ichiban/prolog/v2/internal/syntax"
	"github.com/ichiban/prolog/v2/internal/term"
)

type BuiltinType int8

const (
	BuiltinTypeStandard BuiltinType = iota
	BuiltinTypeInline
	BuiltinTypeArithmetic0
	BuiltinTypeArithmetic1
)

type Builtin struct {
	Type BuiltinType
	Proc func(ctx context.Context, e *Execution) (bool, error)
}

type BuiltinSet struct {
	index   map[term.Functor]int
	entries []Builtin
}

func NewBuiltinSet() *BuiltinSet {
	var b BuiltinSet
	_ = b.Set(term.NewFunctor(term.NewAtom("true"), 1), Builtin{Type: BuiltinTypeStandard, Proc: true0})
	_ = b.Set(term.NewFunctor(term.NewAtom("fail"), 1), Builtin{Type: BuiltinTypeStandard, Proc: fail0})
	_ = b.Set(term.NewFunctor(term.NewAtom("call"), 2), Builtin{Type: BuiltinTypeStandard, Proc: call1})
	_ = b.Set(term.NewFunctor(term.NewAtom("throw"), 2), Builtin{Type: BuiltinTypeStandard, Proc: throw1})
	_ = b.Set(term.NewFunctor(term.NewAtom("subsumes_term"), 3), Builtin{Type: BuiltinTypeStandard, Proc: subsumesTerm2})
	_ = b.Set(term.NewFunctor(term.NewAtom("var"), 2), Builtin{Type: BuiltinTypeInline, Proc: var1})
	_ = b.Set(term.NewFunctor(term.NewAtom("atom"), 2), Builtin{Type: BuiltinTypeInline, Proc: atom1})
	_ = b.Set(term.NewFunctor(term.NewAtom("integer"), 2), Builtin{Type: BuiltinTypeInline, Proc: integer1})
	_ = b.Set(term.NewFunctor(term.NewAtom("float"), 2), Builtin{Type: BuiltinTypeInline, Proc: float1})
	_ = b.Set(term.NewFunctor(term.NewAtom("compound"), 2), Builtin{Type: BuiltinTypeInline, Proc: compound1})
	_ = b.Set(term.NewFunctor(term.NewAtom("ground"), 2), Builtin{Type: BuiltinTypeInline, Proc: ground1})
	_ = b.Set(term.NewFunctor(term.NewAtom("acyclic_term"), 2), Builtin{Type: BuiltinTypeInline, Proc: acyclicTerm1})
	_ = b.Set(term.NewFunctor(term.NewAtom("compare"), 4), Builtin{Type: BuiltinTypeStandard, Proc: compare3})
	_ = b.Set(term.NewFunctor(term.NewAtom("functor"), 4), Builtin{Type: BuiltinTypeStandard, Proc: functor3})
	_ = b.Set(term.NewFunctor(term.NewAtom("arg"), 4), Builtin{Type: BuiltinTypeStandard, Proc: arg3})
	_ = b.Set(term.NewFunctor(term.NewAtom("=.."), 3), Builtin{Type: BuiltinTypeStandard, Proc: univ2})
	_ = b.Set(term.NewFunctor(term.NewAtom("copy_term"), 3), Builtin{Type: BuiltinTypeStandard, Proc: copyTerm2})
	_ = b.Set(term.NewFunctor(term.NewAtom("$get_neck_cut"), 2), Builtin{Type: BuiltinTypeInline, Proc: getNeckCut1})
	_ = b.Set(term.NewFunctor(term.NewAtom("$get_cont"), 2), Builtin{Type: BuiltinTypeInline, Proc: getCont1})
	_ = b.Set(term.NewFunctor(term.NewAtom("$call_cont"), 2), Builtin{Type: BuiltinTypeStandard, Proc: callCont1})
	return &b
}

func (b *BuiltinSet) Lookup(pi term.Functor) (int, bool) {
	if b == nil {
		return 0, false
	}
	id, ok := b.index[pi]
	return id, ok
}

func (b *BuiltinSet) Get(id int) *Builtin {
	return &b.entries[id]
}

func (b *BuiltinSet) Set(pi term.Functor, entry Builtin) error {
	if _, ok := b.index[pi]; ok {
		return fmt.Errorf("duplicate builtin: %s", pi)
	}

	if b.index == nil {
		b.index = map[term.Functor]int{}
	}
	b.index[pi] = len(b.entries)
	b.entries = append(b.entries, entry)
	return nil
}

func (b *BuiltinSet) All() iter.Seq2[term.Functor, *Builtin] {
	keys := slices.Collect(maps.Keys(b.index))
	slices.SortFunc(keys, func(a, b term.Functor) int {
		return strings.Compare(a.String(), b.String())
	})
	return func(yield func(term.Functor, *Builtin) bool) {
		for _, key := range keys {
			id := b.index[key]
			if !yield(key, &b.entries[id]) {
				return
			}
		}
	}
}

func true0(_ context.Context, e *Execution) (bool, error) {
	cont := e.tempVars[1]
	cont = e.Deref(cont)
	pi, ok := e.Functor(cont, term.AllowAtom(true))
	if !ok {
		return false, &TypeError{
			ErrorContext: ErrorContext{
				Location: e.location,
			},
			ValidType: "callable",
			Culprit:   cont,
		}
	}
	pi = term.NewFunctor(pi.Name(), pi.Arity())
	p, ok := e.Predicates[pi]
	if !ok {
		c, err := e.PutFunctor(pi)
		if err != nil {
			return false, err
		}
		return false, &ExistenceError{
			ErrorContext: ErrorContext{
				Location: e.location,
			},
			ObjectType: "procedure",
			Culprit:    c,
		}
	}
	e.programPointer = p.Offset
	for i, arg := range indexed(e.Args(cont)) {
		e.tempVars[i+1] = arg
	}
	return true, nil
}

func fail0(_ context.Context, e *Execution) (bool, error) {
	return false, nil
}

func call1(_ context.Context, e *Execution) (bool, error) {
	goal, cont := e.tempVars[1], e.tempVars[2]
	goal = e.Deref(goal)

	// 7.8.3.1 says "When G contains ! as a subgoal, the effect of ! shall not extend outside G."
	goal, err := e.rewriteCutForCall(goal)
	if err != nil {
		return false, err
	}

	pi, ok := e.Functor(goal, term.AllowAtom(true))
	if !ok {
		if _, ok := e.Variable(goal); ok {
			return false, &InstantiationError{
				ErrorContext: ErrorContext{
					Location: e.location,
				},
			}
		}
		return false, &TypeError{
			ErrorContext: ErrorContext{
				Location: e.location,
			},
			ValidType: "callable",
			Culprit:   goal,
		}
	}

	pi = term.NewFunctor(pi.Name(), pi.Arity()+1)
	p, ok := e.Predicates[pi]
	if !ok {
		c, err := e.PutFunctor(pi)
		if err != nil {
			return false, err
		}
		return false, &ExistenceError{
			ErrorContext: ErrorContext{
				Location: e.location,
			},
			ObjectType: "procedure",
			Culprit:    c,
		}
	}
	e.programPointer = p.Offset
	for i, arg := range indexed(concat(e.Args(goal), singleton(cont))) {
		e.tempVars[i+1] = arg
	}
	return true, nil
}

func (e *Execution) rewriteCutForCall(body term.Handle) (term.Handle, error) {
	body = e.Deref(body)
	switch pi, _ := e.Functor(body, term.AllowAtom(true)); pi {
	case term.NewFunctor(term.NewAtomRune(';'), 2):
		x := e.Arg(body, 0)
		if f, _ := e.Functor(x); f == term.NewFunctor(term.NewAtom("->"), 2) {
			i, t := e.Arg(x, 0), e.Arg(x, 1)
			i, err := e.rewriteCutForCall(i)
			if err != nil {
				return term.Handle{}, err
			}
			t, err = e.rewriteCutForCall(t)
			if err != nil {
				return term.Handle{}, err
			}
			x, err = e.PutCompound(term.NewAtom("->"), i, t)
			if err != nil {
				return term.Handle{}, err
			}
		}
		fallthrough
	case term.NewFunctor(term.NewAtomRune(','), 2):
		x, y := e.Arg(body, 0), e.Arg(body, 1)
		x, err := e.rewriteCutForCall(x)
		if err != nil {
			return term.Handle{}, err
		}
		y, err = e.rewriteCutForCall(y)
		if err != nil {
			return term.Handle{}, err
		}
		return e.PutCompound(pi.Name(), x, y)
	case term.NewFunctor(term.NewAtomRune('!'), 0):
		b, err := e.PutInteger(int64(len(e.stack)))
		if err != nil {
			return term.Handle{}, err
		}
		return e.PutCompound(term.NewAtom("$cut_to"), b)
	default:
		return body, nil
	}
}

func var1(_ context.Context, e *Execution) (bool, error) {
	v := e.tempVars[0]
	v = e.Deref(v)
	if _, ok := e.Variable(v); !ok {
		return false, nil
	}
	e.Next()
	return true, nil
}

func atom1(_ context.Context, e *Execution) (bool, error) {
	t := e.tempVars[0]
	t = e.Deref(t)
	if _, ok := e.Atom(t); !ok {
		return false, nil
	}
	e.Next()
	return true, nil
}

func integer1(_ context.Context, e *Execution) (bool, error) {
	t := e.tempVars[0]
	t = e.Deref(t)
	if _, ok := e.Integer(t); !ok {
		return false, nil
	}
	e.Next()
	return true, nil
}

func float1(_ context.Context, e *Execution) (bool, error) {
	t := e.tempVars[0]
	t = e.Deref(t)
	if _, ok := e.Float(t); !ok {
		return false, nil
	}
	e.Next()
	return true, nil
}

func compound1(_ context.Context, e *Execution) (bool, error) {
	t := e.tempVars[0]
	t = e.Deref(t)
	if _, ok := e.Functor(t); !ok {
		return false, nil
	}
	e.Next()
	return true, nil
}

func ground1(_ context.Context, e *Execution) (bool, error) {
	t := e.tempVars[0]
	t = e.Deref(t)
	vs := e.VariableSet(t)
	if len(vs) > 0 {
		return false, nil
	}
	e.Next()
	return true, nil
}

func acyclicTerm1(_ context.Context, e *Execution) (bool, error) {
	t := e.tempVars[0]
	t = e.Deref(t)
	if ok := e.Acyclic(t); !ok {
		return false, nil
	}
	e.Next()
	return true, nil
}

func throw1(ctx context.Context, e *Execution) (bool, error) {
	ball, cont := e.tempVars[1], e.tempVars[2]
	ball = e.Deref(ball)
	if _, ok := e.Variable(ball); ok {
		var err error
		err = &InstantiationError{
			ErrorContext{
				Location: e.location,
			},
		}
		ball, err = ErrorTerm(e.Arena, err)
		if err != nil {
			return false, err
		}
	}

	var buf bytes.Buffer
	_, _ = fmt.Fprintf(&buf, "%s.", &syntax.Formatter{Arena: e.Arena, Term: ball})

	for cont := range contChain(e.Arena, cont) {
		if pi, ok := e.Functor(cont); !ok || pi.Name() != term.NewAtom("$to_catch") || pi.Arity() != 5 {
			continue
		}

		catcher, recovery, cutB, cont := e.Arg(cont, 0), e.Arg(cont, 1), e.Arg(cont, 2), e.Arg(cont, 3)

		b, _ := e.Integer(cutB)
		if err := e.unTrailTo(int(b)); err != nil {
			return false, err
		}

		ball, err := syntax.ParseTerm(buf.String(),
			syntax.Arena(e.Arena),
		)
		if err != nil {
			return false, fmt.Errorf("parse serialized ball(%s): %w", buf.String(), err)
		}

		ok, err := e.Unify(catcher, ball)
		if err != nil {
			return false, err
		}
		if ok {
			e.tempVars[1] = recovery
			e.tempVars[2] = cont
			return call1(ctx, e)
		}
	}
	return false, fmt.Errorf("unhandled exception: %s", &syntax.Formatter{Arena: e.Arena, Term: ball})
}

func subsumesTerm2(_ context.Context, e *Execution) (bool, error) {
	general, specific, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]
	trailTop := len(e.trail)
	vs := e.VariableSet(specific)

	// Same as unify_with_occurs_check(General, Specific).
	ok, err := e.Unify(general, specific)
	if err != nil {
		return false, err
	}
	ok = ok && e.Acyclic(general)

	// Checks if the temporary bindings keep Specific intact.
	for _, v := range vs {
		w := e.Deref(v)
		ok = ok && v == w
	}

	if err := e.unwindTrail(trailTop); err != nil {
		return false, err
	}

	if !ok {
		return false, nil
	}

	e.tempVars[1] = cont
	e.Next()
	return true, nil
}

func compare3(_ context.Context, e *Execution) (bool, error) {
	order, x, y, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4]
	order, x, y = e.Deref(order), e.Deref(x), e.Deref(y)

	if _, ok := e.Variable(order); ok {
		// Do nothing.
	} else if a, ok := e.Atom(order); ok {
		switch a {
		case term.NewAtomRune('<'), term.NewAtomRune('>'), term.NewAtomRune('='):
			break
		default:
			return false, &DomainError{
				ErrorContext: ErrorContext{
					Location: e.location,
				},
				ValidDomain: "order",
				Culprit:     order,
			}
		}
	} else {
		return false, &TypeError{
			ErrorContext: ErrorContext{
				Location: e.location,
			},
			ValidType: "atom",
			Culprit:   order,
		}
	}

	var (
		a   term.Handle
		err error
	)
	switch o := e.Compare(x, y); {
	case o < 0:
		a, err = e.PutAtom(term.NewAtomRune('<'))
	case o > 0:
		a, err = e.PutAtom(term.NewAtomRune('>'))
	default:
		a, err = e.PutAtom(term.NewAtomRune('='))
	}
	if err != nil {
		return false, err
	}

	ok, err := e.Unify(order, a)
	if err != nil {
		return false, err
	}
	if !ok {
		return false, nil
	}

	e.tempVars[1] = cont
	e.Next()
	return true, nil
}

func functor3(_ context.Context, e *Execution) (bool, error) {
	t, name, arity, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4]
	t, name, arity = e.Deref(t), e.Deref(name), e.Deref(arity)

	if _, ok := e.Variable(t); ok {
		if _, ok := e.Variable(arity); ok {
			return false, &InstantiationError{
				ErrorContext: ErrorContext{
					Location: e.location,
				},
			}
		} else if a, ok := e.Integer(arity); ok {
			if a < 0 {
				return false, &DomainError{
					ErrorContext: ErrorContext{
						Location: e.location,
					},
					ValidDomain: "not_less_than_zero",
					Culprit:     arity,
				}
			}

			if _, ok := e.Variable(name); ok {
				return false, &InstantiationError{
					ErrorContext: ErrorContext{
						Location: e.location,
					},
				}
			} else if _, ok := e.Functor(name); ok {
				return false, &TypeError{
					ErrorContext: ErrorContext{
						Location: e.location,
					},
					ValidType: "atomic",
					Culprit:   name,
				}
			}

			if a == 0 {
				ok, err := e.Unify(t, name)
				if !ok || err != nil {
					return false, err
				}
			} else if n, ok := e.Atom(name); ok {
				c, err := e.PutCompoundWithFreshVars(term.NewFunctor(n, int(a)))
				if err != nil {
					return false, err
				}

				ok, err = e.Unify(t, c)
				if !ok || err != nil {
					return false, err
				}
			} else {
				return false, &TypeError{
					ErrorContext: ErrorContext{
						Location: e.location,
					},
					ValidType: "atom",
					Culprit:   name,
				}
			}
		} else {
			return false, &TypeError{
				ErrorContext: ErrorContext{
					Location: e.location,
				},
				ValidType: "integer",
				Culprit:   arity,
			}
		}
	} else if f, ok := e.Functor(t); ok {
		n, err := e.PutAtom(f.Name())
		if err != nil {
			return false, err
		}

		ok, err := e.Unify(name, n)
		if !ok || err != nil {
			return false, err
		}

		a, err := e.PutInteger(int64(f.Arity()))
		if err != nil {
			return false, err
		}

		ok, err = e.Unify(arity, a)
		if !ok || err != nil {
			return false, err
		}
	} else { // atomic
		ok, err := e.Unify(name, t)
		if !ok || err != nil {
			return false, err
		}

		a, err := e.PutInteger(int64(0))
		if err != nil {
			return false, err
		}

		ok, err = e.Unify(arity, a)
		if !ok || err != nil {
			return false, err
		}
	}

	e.tempVars[1] = cont
	e.Next()
	return true, nil
}

func arg3(_ context.Context, e *Execution) (bool, error) {
	nth, t, arg, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4]
	nth, t, arg = e.Deref(nth), e.Deref(t), e.Deref(arg)

	if _, ok := e.Variable(t); ok {
		return false, &InstantiationError{
			ErrorContext: ErrorContext{
				Location: e.location,
			},
		}
	} else if f, ok := e.Functor(t); ok {
		if _, ok := e.Variable(nth); ok {
			return false, &InstantiationError{
				ErrorContext: ErrorContext{
					Location: e.location,
				},
			}
		} else if n, ok := e.Integer(nth); ok {
			switch {
			case n == 0, int(n) > f.Arity():
				return false, nil
			case n < 0:
				return false, &DomainError{
					ErrorContext: ErrorContext{
						Location: e.location,
					},
					ValidDomain: "not_less_than_zero",
					Culprit:     nth,
				}
			default:
				a := e.Arg(t, int(n)-1)
				ok, err := e.Unify(arg, a)
				if !ok || err != nil {
					return false, err
				}
			}

		} else {
			return false, &TypeError{
				ErrorContext: ErrorContext{
					Location: e.location,
				},
				ValidType: "integer",
				Culprit:   nth,
			}
		}
	} else {
		return false, &TypeError{
			ErrorContext: ErrorContext{
				Location: e.location,
			},
			ValidType: "compound",
			Culprit:   t,
		}
	}

	e.tempVars[1] = cont
	e.Next()
	return true, nil
}

func univ2(_ context.Context, e *Execution) (bool, error) {
	t, list, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]
	t, list = e.Deref(t), e.Deref(list)

	if _, ok := e.Variable(t); ok {
		elems, err := e.mustBeNonEmptyList(list)
		if err != nil {
			return false, err
		}

		if len(elems) == 1 {
			if err := e.mustBeAtomic(elems[0]); err != nil {
				return false, err
			}

			ok, err := e.Unify(t, elems[0])
			if !ok || err != nil {
				return false, err
			}
		}

		a, err := e.mustBeAtom(elems[0])
		if err != nil {
			return false, err
		}

		c, err := e.PutCompound(a, elems[1:]...)
		if err != nil {
			return false, err
		}

		ok, err = e.Unify(t, c)
		if !ok || err != nil {
			return false, err
		}
	} else if f, ok := e.Functor(t); ok {
		if err := e.canBeList(list); err != nil {
			return false, err
		}

		elems := make([]term.Handle, f.Arity()+1)
		a, err := e.PutAtom(f.Name())
		if err != nil {
			return false, err
		}
		elems[0] = a
		copy(elems[1:], slices.Collect(e.Args(t)))

		l, err := e.PutList(elems...)
		if err != nil {
			return false, err
		}

		ok, err := e.Unify(list, l)
		if !ok || err != nil {
			return false, err
		}
	} else {
		if err := e.canBeList(list); err != nil {
			return false, err
		}
	}

	e.tempVars[1] = cont
	e.Next()
	return true, nil
}

func copyTerm2(_ context.Context, e *Execution) (bool, error) {
	t1, t2, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]

	c, err := e.RenamedCopy(t1)
	if err != nil {
		return false, err
	}

	ok, err := e.Unify(t2, c)
	if !ok || err != nil {
		return false, err
	}

	e.tempVars[1] = cont
	e.Next()
	return true, nil
}

func getNeckCut1(_ context.Context, e *Execution) (bool, error) {
	cutB, err := e.PutInteger(int64(e.cutB))
	if err != nil {
		return false, err
	}
	e.tempVars[0] = cutB
	e.Next()
	return true, nil
}

func getCont1(_ context.Context, e *Execution) (bool, error) {
	out, cont := e.tempVars[1], e.tempVars[2]
	if ok, err := e.Unify(out, cont); !ok || err != nil {
		return false, err
	}
	e.Next()
	return true, nil
}

func callCont1(ctx context.Context, e *Execution) (bool, error) {
	// No need to move arguments.
	return true0(ctx, e)
}

func (e *Execution) unTrailTo(b int) error {
	e.stack = e.stack[:b]
	trailTop := 0
	if len(e.stack) > 0 {
		trailTop = e.stack[len(e.stack)-1].trailTop
	}
	return e.unwindTrail(trailTop)
}

func contChain(arena *term.Arena, cont term.Handle) iter.Seq[term.Handle] {
	return func(yield func(term.Handle) bool) {
		for {
			if !yield(cont) {
				return
			}

			pi, ok := arena.Functor(cont, term.AllowAtom(true))
			if !ok || pi.Arity() == 0 {
				return
			}

			cont = arena.Arg(cont, pi.Arity()-1)
		}
	}
}

func indexed[T any](s iter.Seq[T]) iter.Seq2[int, T] {
	return func(yield func(int, T) bool) {
		i := 0
		for e := range s {
			if !yield(i, e) {
				return
			}
			i++
		}
	}
}

func singleton[T any](e T) iter.Seq[T] {
	return func(yield func(T) bool) {
		_ = yield(e)
	}
}

func concat[T any](s1, s2 iter.Seq[T]) iter.Seq[T] {
	return func(yield func(T) bool) {
		for e := range s1 {
			if !yield(e) {
				return
			}
		}
		for e := range s2 {
			if !yield(e) {
				return
			}
		}
	}
}

func (e *Execution) canBeAtom(t term.Handle) (term.Atom, bool, error) {
	if _, ok := e.Variable(t); ok {
		return term.Atom{}, false, nil
	}
	a, ok := e.Atom(t)
	if !ok {
		return term.Atom{}, false, &TypeError{
			ErrorContext: ErrorContext{
				Location: e.location,
			},
			ValidType: "atom",
			Culprit:   t,
		}
	}
	return a, true, nil
}

func (e *Execution) mustBeAtom(t term.Handle) (term.Atom, error) {
	if _, ok := e.Variable(t); ok {
		return term.Atom{}, &InstantiationError{
			ErrorContext: ErrorContext{
				Location: e.location,
			},
		}
	}
	a, ok := e.Atom(t)
	if !ok {
		return term.Atom{}, &TypeError{
			ErrorContext: ErrorContext{
				Location: e.location,
			},
			ValidType: "atom",
			Culprit:   t,
		}
	}
	return a, nil
}

func (e *Execution) canBeList(list term.Handle) error {
	for _, ok := range e.List(list, term.AllowPartial(true)) {
		if !ok {
			return &TypeError{
				ErrorContext: ErrorContext{
					Location: e.location,
				},
				ValidType: "list",
				Culprit:   list,
			}
		}
	}
	return nil
}

func (e *Execution) mustBeList(list term.Handle) ([]term.Handle, error) {
	var elems []term.Handle
	for elem, ok := range e.List(list) {
		if !ok {
			if _, ok := e.Variable(elem); ok {
				return nil, &InstantiationError{
					ErrorContext: ErrorContext{
						Location: e.location,
					},
				}
			}
			return nil, &TypeError{
				ErrorContext: ErrorContext{
					Location: e.location,
				},
				ValidType: "list",
				Culprit:   list,
			}
		}

		elems = append(elems, elem)
	}
	return elems, nil
}

func (e *Execution) mustBeNonEmptyList(list term.Handle) ([]term.Handle, error) {
	elems, err := e.mustBeList(list)
	if err != nil {
		return nil, err
	}
	if len(elems) == 0 {
		return nil, &DomainError{
			ErrorContext: ErrorContext{
				Location: e.location,
			},
			ValidDomain: "non_empty_list",
			Culprit:     list,
		}
	}
	return elems, nil
}

func (e *Execution) mustBeAtomic(t term.Handle) error {
	if _, ok := e.Variable(t); ok {
		return &InstantiationError{
			ErrorContext: ErrorContext{
				Location: e.location,
			},
		}
	}
	if _, ok := e.Functor(t); ok {
		return &TypeError{
			ErrorContext: ErrorContext{
				Location: e.location,
			},
			ValidType: "atomic",
			Culprit:   t,
		}
	}
	return nil
}
