package engine

import (
	"fmt"
	"sync"
	"sync/atomic"
)

// GoStringEnv is an Env which resolves a Variable when GoString() is called.
var GoStringEnv *Env

var (
	varCounter int64
	varTable   = struct {
		sync.RWMutex
		names map[Variable]string
		vars  map[string]Variable
	}{
		names: map[Variable]string{},
		vars:  map[string]Variable{},
	}
)

// Variable is a prolog variable.
type Variable int64

// NewVariable creates a new anonymous variable.
func NewVariable() Variable {
	n := atomic.AddInt64(&varCounter, 1)
	return Variable(n)
}

func NewNamedVariable(name string) Variable {
	varTable.Lock()
	defer varTable.Unlock()

	v, ok := varTable.vars[name]
	if ok {
		return v
	}

	v = NewVariable()
	varTable.vars[name] = v
	varTable.names[v] = name
	return v
}

func (v Variable) String() string {
	varTable.RLock()
	defer varTable.RUnlock()

	n, ok := varTable.names[v]
	if ok {
		return n
	}

	return fmt.Sprintf("_%d", v)
}

func (v Variable) GoString() string {
	switch t := GoStringEnv.Resolve(v).(type) {
	case Variable:
		return t.String()
	default:
		return fmt.Sprintf("%#v", t)
	}
}

// Anonymous checks if the variable has no name.
func (v Variable) Anonymous() bool {
	varTable.RLock()
	defer varTable.RUnlock()
	_, ok := varTable.names[v]
	return !ok
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
		if c, ok := env.Resolve(t).(Compound); ok && c.Functor() == "^" && c.Arity() == 2 {
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
