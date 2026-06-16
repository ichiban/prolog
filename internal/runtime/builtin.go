package runtime

import (
	"context"
	"fmt"
	"iter"

	"github.com/ichiban/prolog/v2/internal/term"
)

var (
	True0 = Builtin{Type: BuiltinTypeInHead, Proc: true0}
	Call1 = Builtin{Type: BuiltinTypeInHead, Proc: call1}
)

type BuiltinType int8

const (
	BuiltinTypeInHead BuiltinType = iota
	BuiltinTypeInline
	BuiltinTypeArithmetic0
	BuiltinTypeArithmetic1
)

type Builtin struct {
	Type BuiltinType
	Proc func(context.Context, *Execution) error
}

type BuiltinSet struct {
	index   map[term.Functor]int
	entries []Builtin
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

func true0(ctx context.Context, e *Execution) error {
	cont := e.tempVars[0]
	pi, ok := e.Functor(cont, term.AllowAtom(true))
	if !ok {
		return &TypeError{
			Arena:     e.Arena,
			ValidType: "callable",
			Culprit:   cont,
		}
	}
	pi = term.NewFunctor(pi.Name(), pi.Arity())
	p, ok := e.Predicates[pi]
	if !ok {
		c, err := e.PutFunctor(pi)
		if err != nil {
			return err
		}
		return &ExistenceError{
			Arena:      e.Arena,
			ObjectType: "procedure",
			Culprit:    c,
		}
	}
	e.programPointer = p.Offset
	for i, arg := range indexed(e.Args(cont)) {
		e.tempVars[i] = arg
	}
	return nil
}

func call1(ctx context.Context, e *Execution) error {
	goal, cont := e.tempVars[0], e.tempVars[1]
	pi, ok := e.Functor(goal, term.AllowAtom(true))
	if !ok {
		return &TypeError{
			Arena:     e.Arena,
			ValidType: "callable",
			Culprit:   goal,
		}
	}
	pi = term.NewFunctor(pi.Name(), pi.Arity()+1)
	p, ok := e.Predicates[pi]
	if !ok {
		c, err := e.PutFunctor(pi)
		if err != nil {
			return err
		}
		return &ExistenceError{
			Arena:      e.Arena,
			ObjectType: "procedure",
			Culprit:    c,
		}
	}
	e.programPointer = p.Offset
	for i, arg := range indexed(concat(e.Args(goal), singleton(cont))) {
		e.tempVars[i] = arg
	}
	return nil
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
