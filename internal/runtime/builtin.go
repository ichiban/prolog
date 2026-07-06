package runtime

import (
	"context"
	"fmt"
	"iter"
	"maps"
	"slices"
	"strings"

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
	_ = b.Set(term.NewFunctor(term.NewAtom("var"), 2), Builtin{Type: BuiltinTypeInline, Proc: var1})
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

func true0(ctx context.Context, e *Execution) (bool, error) {
	if err := ctx.Err(); err != nil {
		return false, err
	}
	cont := e.tempVars[0]
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
		e.tempVars[i] = arg
	}
	return true, nil
}

func fail0(ctx context.Context, e *Execution) (bool, error) {
	err := ctx.Err()
	return e.Backtrack(), err
}

func call1(ctx context.Context, e *Execution) (bool, error) {
	if err := ctx.Err(); err != nil {
		return false, err
	}
	goal, cont := e.tempVars[0], e.tempVars[1]
	goal = e.Deref(goal)
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
		e.tempVars[i] = arg
	}
	return true, nil
}

func var1(ctx context.Context, e *Execution) (bool, error) {
	if err := ctx.Err(); err != nil {
		return false, err
	}
	v := e.tempVars[0]
	v = e.Deref(v)
	if _, ok := e.Variable(v); !ok {
		return e.Backtrack(), nil
	}
	e.Next()
	return true, nil
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
