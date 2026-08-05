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
	"math"
	"slices"
	"strings"

	"github.com/ichiban/prolog/v2/internal/db"
	"github.com/ichiban/prolog/v2/internal/syntax"
	"github.com/ichiban/prolog/v2/internal/term"
	"github.com/ichiban/prolog/v2/internal/wam"
)

type CallingConvention int8

const (
	InHead CallingConvention = iota
	InBody
)

type Builtin struct {
	Type CallingConvention
	Proc func(ctx context.Context, e *Execution) (bool, error)
}

type BuiltinSet struct {
	index   map[term.Functor]int
	entries []Builtin
}

func NewBuiltinSet() *BuiltinSet {
	var b BuiltinSet
	_ = b.Set(term.NewFunctor(term.NewAtom("true"), 1), Builtin{Type: InHead, Proc: true0})
	_ = b.Set(term.NewFunctor(term.NewAtom("fail"), 1), Builtin{Type: InHead, Proc: fail0})
	_ = b.Set(term.NewFunctor(term.NewAtom("call"), 2), Builtin{Type: InHead, Proc: call1})
	_ = b.Set(term.NewFunctor(term.NewAtom("throw"), 2), Builtin{Type: InHead, Proc: throw1})
	_ = b.Set(term.NewFunctor(term.NewAtom("subsumes_term"), 3), Builtin{Type: InHead, Proc: subsumesTerm2})
	_ = b.Set(term.NewFunctor(term.NewAtom("var"), 2), Builtin{Type: InBody, Proc: var1})
	_ = b.Set(term.NewFunctor(term.NewAtom("atom"), 2), Builtin{Type: InBody, Proc: atom1})
	_ = b.Set(term.NewFunctor(term.NewAtom("integer"), 2), Builtin{Type: InBody, Proc: integer1})
	_ = b.Set(term.NewFunctor(term.NewAtom("float"), 2), Builtin{Type: InBody, Proc: float1})
	_ = b.Set(term.NewFunctor(term.NewAtom("compound"), 2), Builtin{Type: InBody, Proc: compound1})
	_ = b.Set(term.NewFunctor(term.NewAtom("ground"), 2), Builtin{Type: InBody, Proc: ground1})
	_ = b.Set(term.NewFunctor(term.NewAtom("acyclic_term"), 2), Builtin{Type: InBody, Proc: acyclicTerm1})
	_ = b.Set(term.NewFunctor(term.NewAtom("compare"), 4), Builtin{Type: InHead, Proc: compare3})
	_ = b.Set(term.NewFunctor(term.NewAtom("functor"), 4), Builtin{Type: InHead, Proc: functor3})
	_ = b.Set(term.NewFunctor(term.NewAtom("arg"), 4), Builtin{Type: InHead, Proc: arg3})
	_ = b.Set(term.NewFunctor(term.NewAtom("=.."), 3), Builtin{Type: InHead, Proc: univ2})
	_ = b.Set(term.NewFunctor(term.NewAtom("copy_term"), 3), Builtin{Type: InHead, Proc: copyTerm2})
	_ = b.Set(term.NewFunctor(term.NewAtom("term_variables"), 3), Builtin{Type: InHead, Proc: termVariables2})
	_ = b.Set(term.NewFunctor(term.NewAtom("clause"), 3), Builtin{Type: InHead, Proc: clause2})
	_ = b.Set(term.NewFunctor(term.NewAtom("assertz"), 2), Builtin{Type: InHead, Proc: assertz1})
	_ = b.Set(term.NewFunctor(term.NewAtom("$dynamic"), 2), Builtin{Type: InHead, Proc: dynamic1})
	_ = b.Set(term.NewFunctor(term.NewAtom("$get_neck_cut"), 2), Builtin{Type: InBody, Proc: getNeckCut1})
	_ = b.Set(term.NewFunctor(term.NewAtom("$get_cont"), 2), Builtin{Type: InBody, Proc: getCont1})
	_ = b.Set(term.NewFunctor(term.NewAtom("$call_cont"), 2), Builtin{Type: InHead, Proc: callCont1})
	// TODO: Implement optimized arithmetic calling convention in binprolog.
	_ = b.Set(term.NewFunctor(term.NewAtom("$+"), 4), Builtin{Type: InHead, Proc: add3})
	_ = b.Set(term.NewFunctor(term.NewAtom("$-"), 4), Builtin{Type: InHead, Proc: sub3})
	_ = b.Set(term.NewFunctor(term.NewAtom("$*"), 4), Builtin{Type: InHead, Proc: mul3})
	_ = b.Set(term.NewFunctor(term.NewAtom("$arith_eq"), 3), Builtin{Type: InHead, Proc: arithEq2})
	_ = b.Set(term.NewFunctor(term.NewAtom("$arith_dif"), 3), Builtin{Type: InHead, Proc: arithDif2})
	_ = b.Set(term.NewFunctor(term.NewAtom("$less"), 3), Builtin{Type: InHead, Proc: less2})
	_ = b.Set(term.NewFunctor(term.NewAtom("$less_eq"), 3), Builtin{Type: InHead, Proc: lessEq2})
	_ = b.Set(term.NewFunctor(term.NewAtom("$greater"), 3), Builtin{Type: InHead, Proc: greater2})
	_ = b.Set(term.NewFunctor(term.NewAtom("$greater_eq"), 3), Builtin{Type: InHead, Proc: greaterEq2})
	_ = b.Set(term.NewFunctor(term.NewAtom("$atom_concat"), 4), Builtin{Type: InHead, Proc: atomConcat3})
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

type ExceptionalValue int8

const (
	FloatOverflow ExceptionalValue = iota
	IntOverflow
	Underflow
	ZeroDivisor
	Undefined
)

func (e ExceptionalValue) Error() string {
	return exceptionalValueNames[e]
}

var exceptionalValueNames = [...]string{
	FloatOverflow: "float_overflow",
	IntOverflow:   "int_overflow",
	Underflow:     "underflow",
	ZeroDivisor:   "zero_divisor",
	Undefined:     "undefined",
}

func true0(ctx context.Context, e *Execution) (bool, error) {
	cont := e.tempVars[1]
	cont = e.Deref(cont)

	bpi, ok := e.Functor(cont, term.AllowAtom(true))
	if !ok {
		return false, &TypeError{
			ValidType: term.NewAtom("continuation"),
			Culprit:   syntax.Serialize(e.Arena, cont),
			Location:  e.location,
		}
	}

	pi := term.NewFunctor(bpi.Name(), bpi.Arity()-1)

	p, ok := e.Predicates[bpi]
	if !ok {
		c, err := e.PutFunctor(pi)
		if err != nil {
			return false, err
		}
		return false, &ExistenceError{
			ObjectType: term.NewAtom("procedure"),
			Culprit:    syntax.Serialize(e.Arena, c),
			Location:   e.location,
		}
	}

	if p.Dynamic {
		call, ok := e.Predicates[term.NewFunctor(term.NewAtom("call"), 2)]
		if !ok {
			c, err := e.PutFunctor(term.NewFunctor(term.NewAtom("call"), 1))
			if err != nil {
				return false, err
			}
			return false, &ExistenceError{
				ObjectType: term.NewAtom("procedure"),
				Culprit:    syntax.Serialize(e.Arena, c),
				Location:   e.location,
			}
		}
		args := slices.Collect(e.Args(cont))
		goal, err := e.PutCompound(pi.Name(), args[:len(args)-1]...)
		if err != nil {
			return false, err
		}
		cont = args[len(args)-1]
		return false, e.pushSeqStackFrame(func(yield func(error) bool) {
			for r := range e.DB.Select(ctx, e.Arena, pi, e.CurrentTime) {
				ok, err := e.Unify(r.Head, goal)
				if err != nil {
					_ = yield(err)
					return
				}
				if !ok {
					continue
				}

				e.tempVars[1] = r.Body
				e.tempVars[2] = cont
				e.programPointer = call.Offset

				if !yield(nil) {
					return
				}
			}
		}, 2)
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

func call1(ctx context.Context, e *Execution) (bool, error) {
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
				Location: e.location,
			}
		}
		return false, &TypeError{
			ValidType: term.NewAtom("callable"),
			Culprit:   syntax.Serialize(e.Arena, goal),
			Location:  e.location,
		}
	}

	bpi := term.NewFunctor(pi.Name(), pi.Arity()+1)
	p, ok := e.Predicates[bpi]
	if !ok {
		c, err := e.PutFunctor(pi)
		if err != nil {
			return false, err
		}
		return false, &ExistenceError{
			ObjectType: term.NewAtom("procedure"),
			Culprit:    syntax.Serialize(e.Arena, c),
			Location:   e.location,
		}
	}
	if p.Dynamic {
		call, ok := e.Predicates[term.NewFunctor(term.NewAtom("call"), 2)]
		if !ok {
			c, err := e.PutFunctor(term.NewFunctor(term.NewAtom("call"), 1))
			if err != nil {
				return false, err
			}
			return false, &ExistenceError{
				ObjectType: term.NewAtom("procedure"),
				Culprit:    syntax.Serialize(e.Arena, c),
				Location:   e.location,
			}
		}
		err = e.pushSeqStackFrame(func(yield func(error) bool) {
			for r := range e.DB.Select(ctx, e.Arena, pi, e.CurrentTime) {
				ok, err := e.Unify(r.Head, goal)
				if err != nil {
					_ = yield(err)
					return
				}
				if !ok {
					continue
				}

				e.tempVars[1] = r.Body
				e.tempVars[2] = cont
				e.programPointer = call.Offset

				if !yield(nil) {
					return
				}
			}
		}, 2)
		return false, err
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
			Location: e.location,
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
				ValidDomain: term.NewAtom("order"),
				Culprit:     syntax.Serialize(e.Arena, order),
				Location:    e.location,
			}
		}
	} else {
		return false, &TypeError{
			ValidType: term.NewAtom("atom"),
			Culprit:   syntax.Serialize(e.Arena, order),
			Location:  e.location,
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
				Location: e.location,
			}
		} else if a, ok := e.Integer(arity); ok {
			if a < 0 {
				return false, &DomainError{
					ValidDomain: term.NewAtom("not_less_than_zero"),
					Culprit:     syntax.Serialize(e.Arena, arity),
					Location:    e.location,
				}
			}

			if _, ok := e.Variable(name); ok {
				return false, &InstantiationError{
					Location: e.location,
				}
			} else if _, ok := e.Functor(name); ok {
				return false, &TypeError{
					ValidType: term.NewAtom("atomic"),
					Culprit:   syntax.Serialize(e.Arena, name),
					Location:  e.location,
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
					ValidType: term.NewAtom("atom"),
					Culprit:   syntax.Serialize(e.Arena, name),
					Location:  e.location,
				}
			}
		} else {
			return false, &TypeError{
				ValidType: term.NewAtom("integer"),
				Culprit:   syntax.Serialize(e.Arena, arity),
				Location:  e.location,
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
			Location: e.location,
		}
	} else if f, ok := e.Functor(t); ok {
		if _, ok := e.Variable(nth); ok {
			return false, &InstantiationError{
				Location: e.location,
			}
		} else if n, ok := e.Integer(nth); ok {
			switch {
			case n == 0, int(n) > f.Arity():
				return false, nil
			case n < 0:
				return false, &DomainError{
					ValidDomain: term.NewAtom("not_less_than_zero"),
					Culprit:     syntax.Serialize(e.Arena, nth),
					Location:    e.location,
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
				ValidType: term.NewAtom("integer"),
				Culprit:   syntax.Serialize(e.Arena, nth),
				Location:  e.location,
			}
		}
	} else {
		return false, &TypeError{
			ValidType: term.NewAtom("compound"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
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

		elems[0] = e.Deref(elems[0])
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

func termVariables2(_ context.Context, e *Execution) (bool, error) {
	t, vars, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]
	t, vars = e.Deref(t), e.Deref(vars)

	if err := e.canBeList(vars); err != nil {
		return false, err
	}

	ret, err := e.PutList(slices.Collect(e.WitnessVariables(t))...)
	if err != nil {
		return false, err
	}

	ok, err := e.Unify(ret, vars)
	if !ok || err != nil {
		return false, err
	}

	e.tempVars[1] = cont
	e.Next()
	return true, nil
}

func clause2(ctx context.Context, e *Execution) (bool, error) {
	head, body := e.tempVars[1], e.tempVars[2]

	pi, err := e.mustBeCallable(head)
	if err != nil {
		return false, err
	}

	if _, _, err := e.canBeCallable(body); err != nil {
		return false, err
	}

	bpi := term.NewFunctor(pi.Name(), pi.Arity()+1)
	p, ok := e.Predicates[bpi]
	if !ok {
		return false, nil
	}

	if !p.Public {
		f, err := e.PutFunctor(pi)
		if err != nil {
			return false, err
		}

		return false, &PermissionError{
			Operation:      term.NewAtom("access"),
			PermissionType: term.NewAtom("private_procedure"),
			Culprit:        syntax.Serialize(e.Arena, f),
			Location:       e.location,
		}
	}

	// Set up a special choice point.
	if err := e.pushSeqStackFrame(func(yield func(error) bool) {
		for r, err := range e.DB.Select(ctx, e.Arena, pi, e.CurrentTime) {
			if err != nil {
				_ = yield(err)
				return
			}
			head, body, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]

			ok, err := e.Unify(head, r.Head)
			if err != nil {
				_ = yield(err)
				return
			}
			if !ok {
				continue
			}

			ok, err = e.Unify(body, r.Body)
			if err != nil {
				_ = yield(err)
				return
			}
			if !ok {
				continue
			}

			e.tempVars[1] = cont
			e.Next()
			if !yield(nil) {
				return
			}
		}
	}, 3); err != nil {
		return false, err
	}

	return false, nil
}

func assertz1(ctx context.Context, e *Execution) (bool, error) {
	t, cont := e.tempVars[1], e.tempVars[2]
	t = e.Deref(t)

	if _, ok := e.Variable(t); ok {
		return true, &InstantiationError{
			Location: e.location,
		}
	}

	var (
		pi   term.Functor
		head term.Handle
		body term.Handle
		err  error
	)
	pi, ok := e.Functor(t, term.AllowAtom(true))
	if !ok {
		return false, &TypeError{
			ValidType: term.NewAtom("callable"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}
	if pi == term.NewFunctor(term.NewAtom(":-"), 2) {
		head, body = e.Arg(t, 0), e.Arg(t, 1)
		pi, ok = e.Functor(head, term.AllowAtom(true))
		if !ok {
			return false, &TypeError{
				ValidType: term.NewAtom("callable"),
				Culprit:   syntax.Serialize(e.Arena, t),
				Location:  e.location,
			}
		}
	} else {
		head = t
		body, err = e.PutAtom(term.NewAtom("true"))
		if err != nil {
			return false, err
		}
	}

	bpi := term.NewFunctor(pi.Name(), pi.Arity()+1)
	p, ok := e.Predicates[bpi]
	if !ok {
		p = wam.Predicate{
			Public:  true,
			Dynamic: true,
		}
		if e.Predicates == nil {
			e.Predicates = map[term.Functor]wam.Predicate{}
		}
		e.Predicates[bpi] = p
	}
	if !p.Dynamic {
		return false, &PermissionError{
			Operation:      term.NewAtom("modify"),
			PermissionType: term.NewAtom("static_procedure"),
			Culprit:        syntax.Serialize(e.Arena, t),
			Location:       e.location,
		}
	}

	if err := e.DB.Insert(ctx, e.Arena, db.Record{
		Head:      head,
		Body:      body,
		CreatedAt: e.CurrentTime,
	}); err != nil {
		return false, err
	}
	e.CurrentTime++

	e.tempVars[1] = cont
	e.Next()
	return true, nil
}

func dynamic1(ctx context.Context, e *Execution) (bool, error) {
	t, cont := e.tempVars[1], e.tempVars[2]
	t = e.Deref(t)

	pi, err := e.mustBePredicateIndicator(t)
	if err != nil {
		return false, err
	}

	bpi := term.NewFunctor(pi.Name(), pi.Arity()+1)
	p, _ := e.Predicates[bpi]
	p.Public = true
	p.Dynamic = true
	e.Predicates[bpi] = p

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

func add3(ctx context.Context, e *Execution) (bool, error) {
	x, y, out, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4]

	return e.mustBeNumber(x, func(e *Execution, x int64) (bool, error) {
		return e.mustBeNumber(y, func(e *Execution, y int64) (bool, error) {
			r, err := addI(x, y)
			if err != nil {
				return false, err
			}
			t, err := e.PutInteger(r)
			if err != nil {
				return false, err
			}
			ok, err := e.Unify(out, t)
			if !ok || err != nil {
				return false, err
			}
			e.tempVars[1] = cont
			e.Next()
			return true, nil
		}, func(e *Execution, y float64) (bool, error) {
			r, err := addIF(x, y)
			if err != nil {
				return false, &EvaluationError{
					Cause:    err,
					Location: e.location,
				}
			}
			t, err := e.PutFloat(r)
			if err != nil {
				return false, err
			}
			ok, err := e.Unify(out, t)
			if !ok || err != nil {
				return false, err
			}
			e.tempVars[1] = cont
			e.Next()
			return true, nil
		})
	}, func(e *Execution, x float64) (bool, error) {
		return e.mustBeNumber(y, func(e *Execution, y int64) (bool, error) {
			r, err := addFI(x, y)
			if err != nil {
				return false, &EvaluationError{
					Cause:    err,
					Location: e.location,
				}
			}
			t, err := e.PutFloat(r)
			if err != nil {
				return false, err
			}
			ok, err := e.Unify(out, t)
			if !ok || err != nil {
				return false, err
			}
			e.tempVars[1] = cont
			e.Next()
			return true, nil
		}, func(e *Execution, y float64) (bool, error) {
			r, err := addF(x, y)
			if err != nil {
				return false, &EvaluationError{
					Cause:    err,
					Location: e.location,
				}
			}
			t, err := e.PutFloat(r)
			if err != nil {
				return false, err
			}
			ok, err := e.Unify(out, t)
			if !ok || err != nil {
				return false, err
			}
			e.tempVars[1] = cont
			e.Next()
			return true, nil
		})
	})
}

func sub3(ctx context.Context, e *Execution) (bool, error) {
	x, y, out, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4]

	return e.mustBeNumber(x, func(e *Execution, x int64) (bool, error) {
		return e.mustBeNumber(y, func(e *Execution, y int64) (bool, error) {
			r, err := subI(x, y)
			if err != nil {
				return false, &EvaluationError{
					Cause:    err,
					Location: e.location,
				}
			}
			t, err := e.PutInteger(r)
			if err != nil {
				return false, err
			}
			ok, err := e.Unify(out, t)
			if !ok || err != nil {
				return false, err
			}
			e.tempVars[1] = cont
			e.Next()
			return true, nil
		}, func(e *Execution, y float64) (bool, error) {
			r, err := subIF(x, y)
			if err != nil {
				return false, &EvaluationError{
					Cause:    err,
					Location: e.location,
				}
			}
			t, err := e.PutFloat(r)
			if err != nil {
				return false, err
			}
			ok, err := e.Unify(out, t)
			if !ok || err != nil {
				return false, err
			}
			e.tempVars[1] = cont
			e.Next()
			return true, nil
		})
	}, func(e *Execution, x float64) (bool, error) {
		return e.mustBeNumber(y, func(e *Execution, y int64) (bool, error) {
			r, err := subFI(x, y)
			if err != nil {
				return false, &EvaluationError{
					Cause:    err,
					Location: e.location,
				}
			}
			t, err := e.PutFloat(r)
			if err != nil {
				return false, err
			}
			ok, err := e.Unify(out, t)
			if !ok || err != nil {
				return false, err
			}
			e.tempVars[1] = cont
			e.Next()
			return true, nil
		}, func(e *Execution, y float64) (bool, error) {
			r, err := subF(x, y)
			if err != nil {
				return false, &EvaluationError{
					Cause:    err,
					Location: e.location,
				}
			}
			t, err := e.PutFloat(r)
			if err != nil {
				return false, err
			}
			ok, err := e.Unify(out, t)
			if !ok || err != nil {
				return false, err
			}
			e.tempVars[1] = cont
			e.Next()
			return true, nil
		})
	})
}

func mul3(ctx context.Context, e *Execution) (bool, error) {
	x, y, out, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4]

	return e.mustBeNumber(x, func(e *Execution, x int64) (bool, error) {
		return e.mustBeNumber(y, func(e *Execution, y int64) (bool, error) {
			r, err := mulI(x, y)
			if err != nil {
				return false, &EvaluationError{
					Cause:    err,
					Location: e.location,
				}
			}
			t, err := e.PutInteger(r)
			if err != nil {
				return false, err
			}
			ok, err := e.Unify(out, t)
			if !ok || err != nil {
				return false, err
			}
			e.tempVars[1] = cont
			e.Next()
			return true, nil
		}, func(e *Execution, y float64) (bool, error) {
			r, err := mulIF(x, y)
			if err != nil {
				return false, &EvaluationError{
					Cause:    err,
					Location: e.location,
				}
			}
			t, err := e.PutFloat(r)
			if err != nil {
				return false, err
			}
			e.tempVars[0] = t
			e.Next()
			return true, nil
		})
	}, func(e *Execution, x float64) (bool, error) {
		return e.mustBeNumber(y, func(e *Execution, y int64) (bool, error) {
			r, err := mulFI(x, y)
			if err != nil {
				return false, &EvaluationError{
					Cause:    err,
					Location: e.location,
				}
			}
			t, err := e.PutFloat(r)
			if err != nil {
				return false, err
			}
			e.tempVars[0] = t
			e.Next()
			return true, nil
		}, func(e *Execution, y float64) (bool, error) {
			r, err := mulF(x, y)
			if err != nil {
				return false, &EvaluationError{
					Cause:    err,
					Location: e.location,
				}
			}
			t, err := e.PutFloat(r)
			if err != nil {
				return false, err
			}
			e.tempVars[0] = t
			e.Next()
			return true, nil
		})
	})
}

func arithEq2(ctx context.Context, e *Execution) (bool, error) {
	x, y, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]
	x, y = e.Deref(x), e.Deref(y)

	return e.mustBeNumber(x, func(e *Execution, x int64) (bool, error) {
		return e.mustBeNumber(y, func(e *Execution, y int64) (bool, error) {
			if !eqI(x, y) {
				return false, nil
			}
			e.tempVars[1] = cont
			e.Next()
			return true, nil
		}, func(e *Execution, y float64) (bool, error) {
			if !eqIF(x, y) {
				return false, nil
			}
			e.tempVars[1] = cont
			e.Next()
			return true, nil
		})
	}, func(e *Execution, x float64) (bool, error) {
		return e.mustBeNumber(y, func(e *Execution, y int64) (bool, error) {
			if !eqFI(x, y) {
				return false, nil
			}
			e.tempVars[1] = cont
			e.Next()
			return true, nil
		}, func(e *Execution, y float64) (bool, error) {
			if !eqF(x, y) {
				return false, nil
			}
			e.tempVars[1] = cont
			e.Next()
			return true, nil
		})
	})
}

func arithDif2(ctx context.Context, e *Execution) (bool, error) {
	x, y, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]
	x, y = e.Deref(x), e.Deref(y)

	return e.mustBeNumber(x, func(e *Execution, x int64) (bool, error) {
		return e.mustBeNumber(y, func(e *Execution, y int64) (bool, error) {
			if !neqI(x, y) {
				return false, nil
			}
			e.tempVars[1] = cont
			e.Next()
			return true, nil
		}, func(e *Execution, y float64) (bool, error) {
			if !neqIF(x, y) {
				return false, nil
			}
			e.tempVars[1] = cont
			e.Next()
			return true, nil
		})
	}, func(e *Execution, x float64) (bool, error) {
		return e.mustBeNumber(y, func(e *Execution, y int64) (bool, error) {
			if !neqFI(x, y) {
				return false, nil
			}
			e.tempVars[1] = cont
			e.Next()
			return true, nil
		}, func(e *Execution, y float64) (bool, error) {
			if !neqF(x, y) {
				return false, nil
			}
			e.tempVars[1] = cont
			e.Next()
			return true, nil
		})
	})
}

func less2(ctx context.Context, e *Execution) (bool, error) {
	x, y, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]
	x, y = e.Deref(x), e.Deref(y)

	return e.mustBeNumber(x, func(e *Execution, x int64) (bool, error) {
		return e.mustBeNumber(y, func(e *Execution, y int64) (bool, error) {
			if !lssI(x, y) {
				return false, nil
			}
			e.tempVars[1] = cont
			e.Next()
			return true, nil
		}, func(e *Execution, y float64) (bool, error) {
			if !lssIF(x, y) {
				return false, nil
			}
			e.tempVars[1] = cont
			e.Next()
			return true, nil
		})
	}, func(e *Execution, x float64) (bool, error) {
		return e.mustBeNumber(y, func(e *Execution, y int64) (bool, error) {
			if !lssFI(x, y) {
				return false, nil
			}
			e.tempVars[1] = cont
			e.Next()
			return true, nil
		}, func(e *Execution, y float64) (bool, error) {
			if !lssF(x, y) {
				return false, nil
			}
			e.tempVars[1] = cont
			e.Next()
			return true, nil
		})
	})
}

func lessEq2(ctx context.Context, e *Execution) (bool, error) {
	x, y, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]
	x, y = e.Deref(x), e.Deref(y)

	return e.mustBeNumber(x, func(e *Execution, x int64) (bool, error) {
		return e.mustBeNumber(y, func(e *Execution, y int64) (bool, error) {
			if !leqI(x, y) {
				return false, nil
			}
			e.tempVars[1] = cont
			e.Next()
			return true, nil
		}, func(e *Execution, y float64) (bool, error) {
			if !leqIF(x, y) {
				return false, nil
			}
			e.tempVars[1] = cont
			e.Next()
			return true, nil
		})
	}, func(e *Execution, x float64) (bool, error) {
		return e.mustBeNumber(y, func(e *Execution, y int64) (bool, error) {
			if !leqFI(x, y) {
				return false, nil
			}
			e.tempVars[1] = cont
			e.Next()
			return true, nil
		}, func(e *Execution, y float64) (bool, error) {
			if !leqF(x, y) {
				return false, nil
			}
			e.tempVars[1] = cont
			e.Next()
			return true, nil
		})
	})
}

func greater2(ctx context.Context, e *Execution) (bool, error) {
	x, y, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]
	x, y = e.Deref(x), e.Deref(y)

	return e.mustBeNumber(x, func(e *Execution, x int64) (bool, error) {
		return e.mustBeNumber(y, func(e *Execution, y int64) (bool, error) {
			if !gtrI(x, y) {
				return false, nil
			}
			e.tempVars[1] = cont
			e.Next()
			return true, nil
		}, func(e *Execution, y float64) (bool, error) {
			if !gtrIF(x, y) {
				return false, nil
			}
			e.tempVars[1] = cont
			e.Next()
			return true, nil
		})
	}, func(e *Execution, x float64) (bool, error) {
		return e.mustBeNumber(y, func(e *Execution, y int64) (bool, error) {
			if !gtrFI(x, y) {
				return false, nil
			}
			e.tempVars[1] = cont
			e.Next()
			return true, nil
		}, func(e *Execution, y float64) (bool, error) {
			if !gtrF(x, y) {
				return false, nil
			}
			e.tempVars[1] = cont
			e.Next()
			return true, nil
		})
	})
}

func greaterEq2(ctx context.Context, e *Execution) (bool, error) {
	x, y, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]
	x, y = e.Deref(x), e.Deref(y)

	return e.mustBeNumber(x, func(e *Execution, x int64) (bool, error) {
		return e.mustBeNumber(y, func(e *Execution, y int64) (bool, error) {
			if !geqI(x, y) {
				return false, nil
			}
			e.tempVars[1] = cont
			e.Next()
			return true, nil
		}, func(e *Execution, y float64) (bool, error) {
			if !geqIF(x, y) {
				return false, nil
			}
			e.tempVars[1] = cont
			e.Next()
			return true, nil
		})
	}, func(e *Execution, x float64) (bool, error) {
		return e.mustBeNumber(y, func(e *Execution, y int64) (bool, error) {
			if !geqFI(x, y) {
				return false, nil
			}
			e.tempVars[1] = cont
			e.Next()
			return true, nil
		}, func(e *Execution, y float64) (bool, error) {
			if !geqF(x, y) {
				return false, nil
			}
			e.tempVars[1] = cont
			e.Next()
			return true, nil
		})
	})
}

func atomConcat3(ctx context.Context, e *Execution) (bool, error) {
	// Simpler one-directional variant of atom_concat/3.
	atom1, atom2, atom12, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4]
	atom1, atom2 = e.Deref(atom1), e.Deref(atom2)

	a1, err := e.mustBeAtom(atom1)
	if err != nil {
		return false, err
	}

	a2, err := e.mustBeAtom(atom2)
	if err != nil {
		return false, err
	}

	r, err := e.PutAtom(term.NewAtom(a1.String() + a2.String()))
	if err != nil {
		return false, err
	}

	ok, err := e.Unify(atom12, r)
	if !ok || err != nil {
		return false, err
	}

	e.tempVars[1] = cont
	e.Next()
	return true, nil
}

func (e *Execution) unTrailTo(b int) error {
	//
	for i := len(e.stack) - 1; i > b; i-- {
		f := e.stack[i]
		if f.stop == nil {
			continue
		}
		f.stop()
	}

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
			ValidType: term.NewAtom("atom"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}
	return a, true, nil
}

func (e *Execution) mustBeAtom(t term.Handle) (term.Atom, error) {
	if _, ok := e.Variable(t); ok {
		return term.Atom{}, &InstantiationError{
			Location: e.location,
		}
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

func (e *Execution) canBeList(list term.Handle) error {
	for _, ok := range e.List(list, term.AllowPartial(true)) {
		if !ok {
			return &TypeError{
				ValidType: term.NewAtom("list"),
				Culprit:   syntax.Serialize(e.Arena, list),
				Location:  e.location,
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
					Location: e.location,
				}
			}
			return nil, &TypeError{
				ValidType: term.NewAtom("list"),
				Culprit:   syntax.Serialize(e.Arena, list),
				Location:  e.location,
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
			ValidDomain: term.NewAtom("non_empty_list"),
			Culprit:     syntax.Serialize(e.Arena, list),
			Location:    e.location,
		}
	}
	return elems, nil
}

func (e *Execution) mustBeAtomic(t term.Handle) error {
	if _, ok := e.Variable(t); ok {
		return &InstantiationError{
			Location: e.location,
		}
	}
	if _, ok := e.Functor(t); ok {
		return &TypeError{
			ValidType: term.NewAtom("atomic"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}
	return nil
}

func (e *Execution) mustBeNumber(t term.Handle, intFn func(e *Execution, i int64) (bool, error), floatFn func(e *Execution, f float64) (bool, error)) (bool, error) {
	t = e.Deref(t)

	if _, ok := e.Variable(t); ok {
		return false, &InstantiationError{
			Location: e.location,
		}
	}

	if i, ok := e.Integer(t); ok {
		return intFn(e, i)
	}

	if f, ok := e.Float(t); ok {
		return floatFn(e, f)
	}

	return false, &TypeError{
		ValidType: term.NewAtom("number"),
		Culprit:   syntax.Serialize(e.Arena, t),
		Location:  e.location,
	}
}

func (e *Execution) canBeCallable(t term.Handle) (term.Functor, bool, error) {
	if _, ok := e.Variable(t); ok {
		return 0, false, nil
	}

	f, ok := e.Functor(t, term.AllowAtom(true))
	if !ok {
		return 0, false, &TypeError{
			ValidType: term.NewAtom("callable"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}

	return f, true, nil
}

func (e *Execution) mustBeCallable(t term.Handle) (term.Functor, error) {
	f, ok, err := e.canBeCallable(t)
	if err != nil {
		return 0, err
	}
	if !ok {
		return 0, &InstantiationError{
			Location: e.location,
		}
	}
	return f, nil
}

func (e *Execution) canBePredicateIndicator(t term.Handle) (term.Functor, bool, error) {
	if _, ok := e.Variable(t); ok {
		return 0, false, nil
	}

	if f, ok := e.Functor(t, term.AllowAtom(true)); !ok || f != term.NewFunctor(term.NewAtomRune('/'), 2) {
		return 0, false, &TypeError{
			ValidType: term.NewAtom("predicate_indicator"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}

	n, ok := e.Atom(e.Deref(e.Arg(t, 0)))
	if !ok {
		return 0, false, &TypeError{
			ValidType: term.NewAtom("predicate_indicator"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}

	a, ok := e.Integer(e.Deref(e.Arg(t, 1)))
	if !ok {
		return 0, false, &TypeError{
			ValidType: term.NewAtom("predicate_indicator"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}

	pi := term.NewFunctor(n, int(a))
	return pi, true, nil
}

func (e *Execution) mustBePredicateIndicator(t term.Handle) (term.Functor, error) {
	pi, ok, err := e.canBePredicateIndicator(t)
	if err != nil {
		return 0, err
	}
	if !ok {
		return 0, &InstantiationError{
			Location: e.location,
		}
	}
	return pi, nil
}

func addI(x, y int64) (int64, error) {
	switch {
	case y > 0 && x > math.MaxInt64-y:
		return 0, IntOverflow
	case y < 0 && x < math.MinInt64-y:
		return 0, IntOverflow
	default:
		return x + y, nil
	}
}

func addF(x, y float64) (float64, error) {
	switch {
	case y > 0 && x > math.MaxFloat64-y:
		return 0, FloatOverflow
	case y < 0 && x < -math.MaxFloat64-y:
		return 0, FloatOverflow
	default:
		return x + y, nil
	}
}

func addIF(x int64, y float64) (float64, error) {
	return addF(float64(x), y)
}

func addFI(x float64, y int64) (float64, error) {
	return addF(x, float64(y))
}

func subI(x, y int64) (int64, error) {
	switch {
	case y < 0 && x > math.MaxInt64+y:
		return 0, IntOverflow
	case y > 0 && x < math.MinInt64+y:
		return 0, IntOverflow
	default:
		return x - y, nil
	}
}

func subF(x, y float64) (float64, error) {
	return addF(x, -y)
}

func subFI(x float64, n int64) (float64, error) {
	return subF(x, float64(n))
}

func subIF(n int64, x float64) (float64, error) {
	return subF(float64(n), x)
}

func mulI(x, y int64) (int64, error) {
	switch {
	case x == -1 && y == math.MinInt64:
		return 0, IntOverflow
	case x == math.MinInt64 && y == -1:
		return 0, IntOverflow
	case y == 0:
		return 0, nil
	default:
		r := x * y
		if r/y != x {
			return 0, IntOverflow
		}
		return r, nil
	}
}

func mulF(x, y float64) (float64, error) {
	switch {
	case y != 0 && x > math.MaxFloat64/y:
		return 0, FloatOverflow
	case y != 0 && x < -math.MaxFloat64/y:
		return 0, FloatOverflow
	}

	r := x * y

	// Underflow: x*y = 0 iff x = 0 or y = 0.
	if r == 0 && x != 0 && y != 0 {
		return 0, Underflow
	}

	return r, nil
}

func mulIF(n int64, x float64) (float64, error) {
	return mulF(float64(n), x)
}

func mulFI(x float64, n int64) (float64, error) {
	return mulF(x, float64(n))
}

func intDivI(x, y int64) (int64, error) {
	switch {
	case y == 0:
		return 0, ZeroDivisor
	case x == math.MinInt64 && y == -1:
		// Two's complement special case
		return 0, IntOverflow
	default:
		return x / y, nil
	}
}

func divI(n, m int64) (float64, error) {
	return divF(float64(n), float64(m))
}

func divF(x, y float64) (float64, error) {
	switch {
	case y == 0:
		return 0, ZeroDivisor
	case x > math.MaxFloat64*y:
		return 0, FloatOverflow
	case x < -math.MaxFloat64*y:
		return 0, FloatOverflow
	}

	r := x / y

	// Underflow: x/y = 0 iff x = 0 and y != 0.
	if r == 0 && x != 0 {
		return 0, Underflow
	}

	return r, nil
}

func divIF(n int64, x float64) (float64, error) {
	return divF(float64(n), x)
}

func divFI(x float64, n int64) (float64, error) {
	return divF(x, float64(n))
}

func remI(x, y int64) (int64, error) {
	if y == 0 {
		return 0, ZeroDivisor
	}
	return x - ((x / y) * y), nil
}

func modI(x, y int64) (int64, error) {
	if y == 0 {
		return 0, ZeroDivisor
	}
	return x - (int64(math.Floor(float64(x)/float64(y))) * y), nil
}

func negI(x int64) (int64, error) {
	// Two's complement special case
	if x == math.MinInt64 {
		return 0, IntOverflow
	}
	return -x, nil
}

func negF(x float64) float64 {
	return -x
}

func absI(x int64) (int64, error) {
	switch {
	case x == math.MinInt64:
		return 0, IntOverflow
	case x < 0:
		return -x, nil
	default:
		return x, nil
	}
}

func absF(x float64) float64 {
	return math.Abs(float64(x))
}

func signI(x int64) int64 {
	switch {
	case x > 0:
		return 1
	case x < 0:
		return -1
	default:
		return 0
	}
}

func signF(x float64) float64 {
	switch {
	case x > 0:
		return 1
	case x < 0:
		return -1
	default:
		return 0
	}
}

func posI(x int64) (int64, error) {
	return x, nil
}

func posF(x float64) (float64, error) {
	return x, nil
}

func intFloorDivI(x, y int64) (int64, error) {
	switch {
	case x == math.MinInt64 && y == -1:
		return 0, IntOverflow
	case y == 0:
		return 0, ZeroDivisor
	default:
		return int64(math.Floor(float64(x) / float64(y))), nil
	}
}

func intPartF(x float64) float64 {
	s := signF(x)
	return s * math.Floor(math.Abs(x))
}

func fractPartF(x float64) float64 {
	i := intPartF(x)
	return x - i
}

func eqI(m, n int64) bool {
	return m == n
}

func eqF(x, y float64) bool {
	return x == y
}

func eqFI(x float64, n int64) bool {
	y := floatItoF(n)
	return eqF(x, y)
}

func eqIF(n int64, y float64) bool {
	return eqFI(y, n)
}

func neqF(x, y float64) bool {
	return x != y
}

func neqI(m, n int64) bool {
	return m != n
}

func neqFI(x float64, n int64) bool {
	y := floatItoF(n)
	return neqF(x, y)
}

func neqIF(n int64, y float64) bool {
	return neqFI(y, n)
}

func lssF(x, y float64) bool {
	return x < y
}

func lssI(m, n int64) bool {
	return m < n
}

func lssFI(x float64, n int64) bool {
	y := floatItoF(n)
	return lssF(x, y)
}

func lssIF(n int64, y float64) bool {
	return gtrFI(y, n)
}

func leqF(x, y float64) bool {
	return x <= y
}

func leqI(m, n int64) bool {
	return m <= n
}

func leqFI(x float64, n int64) bool {
	y := floatItoF(n)
	return leqF(x, y)
}

func leqIF(n int64, y float64) bool {
	return geqFI(y, n)
}

func gtrF(x, y float64) bool {
	return x > y
}

func gtrI(m, n int64) bool {
	return m > n
}

func gtrFI(x float64, n int64) bool {
	y := floatItoF(n)
	return gtrF(x, y)
}

func gtrIF(n int64, y float64) bool {
	return lssFI(y, n)
}

func geqF(x, y float64) bool {
	return x >= y
}

func geqI(m, n int64) bool {
	return m >= n
}

func geqFI(x float64, n int64) bool {
	y := floatItoF(n)
	return geqF(x, y)
}

func geqIF(n int64, y float64) bool {
	return leqFI(y, n)
}

// Type conversion operations

func floatItoF(n int64) float64 {
	return float64(n)
}

func floatFtoF(x float64) float64 {
	return x
}

func floorFtoI(x float64) (int64, error) {
	f := math.Floor(x)
	if f > float64(math.MaxInt64) || f < float64(math.MinInt64) {
		return 0, IntOverflow
	}
	return int64(f), nil
}

func truncateFtoI(x float64) (int64, error) {
	t := math.Trunc(x)
	if t > float64(math.MaxInt64) || t < float64(math.MinInt64) {
		return 0, IntOverflow
	}
	return int64(t), nil
}

func roundFtoI(x float64) (int64, error) {
	r := math.Round(x)
	if r > float64(math.MaxInt64) || r < float64(math.MinInt64) {
		return 0, IntOverflow
	}
	return int64(r), nil
}

func ceilingFtoI(x float64) (int64, error) {
	c := math.Ceil(x)
	if c > float64(math.MaxInt64) || c < float64(math.MinInt64) {
		return 0, IntOverflow
	}
	return int64(c), nil
}
