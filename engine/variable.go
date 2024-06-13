package engine

import (
	"fmt"
	"io"
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

func (v Variable) WriteTerm(w io.Writer, opts *WriteOptions, env *Env) error {
	x := env.Resolve(v)
	v, ok := x.(Variable)
	if !ok {
		return x.WriteTerm(w, opts, env)
	}

	ew := errWriter{w: w}

	if letterDigit(opts.left.name) {
		_, _ = ew.Write([]byte(" "))
	}
	if a, ok := opts.variableNames[v]; ok {
		_ = a.WriteTerm(&ew, opts.withQuoted(false).withLeft(operator{}).withRight(operator{}), env)
	} else {
		_, _ = ew.Write([]byte(fmt.Sprintf("_%d", v)))
	}
	if letterDigit(opts.right.name) {
		_, _ = ew.Write([]byte(" "))
	}

	return ew.err
}

func (v Variable) Compare(t Term, env *Env) int {
	w := env.Resolve(v)
	v, ok := w.(Variable)
	if !ok {
		return w.Compare(t, env)
	}

	switch t := env.Resolve(t).(type) {
	case Variable:
		switch {
		case v > t:
			return 1
		case v < t:
			return -1
		default:
			return 0
		}
	default:
		return -1
	}
}

// variableSet is a set of variables. The key is the variable and the value is the number of occurrences.
// So if you look at the value it's a multi set of variable occurrences and if you ignore the value it's a set of occurrences (witness).
type variableSet map[Variable]int

func newVariableSet(t Term, env *Env) variableSet {
	s := variableSet{}
	for terms := []Term{t}; len(terms) > 0; terms, t = terms[:len(terms)-1], terms[len(terms)-1] {
		switch t := env.Resolve(t).(type) {
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

func newExistentialVariablesSet(t Term, env *Env) variableSet {
	ev := variableSet{}
	for terms := []Term{t}; len(terms) > 0; terms, t = terms[:len(terms)-1], terms[len(terms)-1] {
		if c, ok := env.Resolve(t).(Compound); ok && c.Functor() == atomCaret && c.Arity() == 2 {
			for v, o := range newVariableSet(c.Arg(0), env) {
				ev[v] = o
			}
			terms = append(terms, c.Arg(1))
		}
	}
	return ev
}

func newFreeVariablesSet(t, v Term, env *Env) variableSet {
	fv := variableSet{}
	s := newVariableSet(t, env)

	bv := newVariableSet(v, env)
	for v := range newExistentialVariablesSet(t, env) {
		bv[v] += 1
	}

	for v, n := range s {
		if m, ok := bv[v]; !ok {
			fv[v] = n + m
		}
	}

	return fv
}
