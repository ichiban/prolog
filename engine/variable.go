package engine

import (
	"fmt"
	"regexp"
	"sync/atomic"
)

// Variable is a prolog variable.
type Variable string

var varCounter uint64

// NewVariable creates a new generated variable.
func NewVariable() Variable {
	n := atomic.AddUint64(&varCounter, 1)
	return Variable(fmt.Sprintf("_%d", n))
}

var generatedPattern = regexp.MustCompile(`\A_\d+\z`)

// Generated checks if the variable is generated.
func (v Variable) Generated() bool {
	return generatedPattern.MatchString(string(v))
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
		case *Compound:
			terms = append(terms, t.Args...)
		}
	}
	return s
}

func newExistentialVariablesSet(t Term, env *Env) variableSet {
	ev := variableSet{}
	for terms := []Term{t}; len(terms) > 0; terms, t = terms[:len(terms)-1], terms[len(terms)-1] {
		if c, ok := env.Resolve(t).(*Compound); ok && c.Functor == "^" && len(c.Args) == 2 {
			for v, o := range newVariableSet(c.Args[0], env) {
				ev[v] = o
			}
			terms = append(terms, c.Args[1])
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
