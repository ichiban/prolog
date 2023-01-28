package engine

import (
	"context"
	"fmt"
	"sync/atomic"
)

var varCounter int64

func lastVariable() Variable {
	return Variable(varCounter)
}

// Variable is a prolog variable.
type Variable int64

// NewVariable creates a new anonymous variable.
func NewVariable() Variable {
	n := atomic.AddInt64(&varCounter, 1)
	return Variable(n)
}

func (v Variable) String() string {
	return fmt.Sprintf("_%d", v)
}

// variableSet is a set of variables. The key is the variable and the value is the number of occurrences.
// So if you look at the value it's a multi set of variable occurrences and if you ignore the value it's a set of occurrences (witness).
type variableSet map[Variable]int

func newVariableSet(ctx context.Context, t Term) variableSet {
	s := variableSet{}
	for terms := []Term{t}; len(terms) > 0; terms, t = terms[:len(terms)-1], terms[len(terms)-1] {
		switch t := Resolve(ctx, t).(type) {
		case Variable:
			s[t] += 1
		case Compound:
			for i := 0; i < t.Arity(); i++ {
				terms = append(terms, t.Arg(i))
			}
		}
	}
	return s
}

func newExistentialVariablesSet(ctx context.Context, t Term) variableSet {
	ev := variableSet{}
	for terms := []Term{t}; len(terms) > 0; terms, t = terms[:len(terms)-1], terms[len(terms)-1] {
		if c, ok := Resolve(ctx, t).(Compound); ok && c.Functor() == atomCaret && c.Arity() == 2 {
			for v, o := range newVariableSet(ctx, c.Arg(0)) {
				ev[v] = o
			}
			terms = append(terms, c.Arg(1))
		}
	}
	return ev
}

func newFreeVariablesSet(ctx context.Context, t, v Term) variableSet {
	fv := variableSet{}
	s := newVariableSet(ctx, t)

	bv := newVariableSet(ctx, v)
	for v := range newExistentialVariablesSet(ctx, t) {
		bv[v] += 1
	}

	for v, n := range s {
		if m, ok := bv[v]; !ok {
			fv[v] = n + m
		}
	}

	return fv
}
