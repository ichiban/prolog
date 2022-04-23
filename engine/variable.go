package engine

import (
	"fmt"
	"regexp"
	"strings"
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

// Unify unifies the variable with t.
func (v Variable) Unify(t Term, occursCheck bool, env *Env) (*Env, bool) {
	r, t := env.Resolve(v), env.Resolve(t)
	v, ok := r.(Variable)
	if !ok {
		return r.Unify(t, occursCheck, env)
	}
	switch {
	case v == t:
		return env, true
	case occursCheck && Contains(t, v, env):
		return env, false
	default:
		return env.Bind(v, t), true
	}
}

// Unparse emits tokens that represent the variable.
func (v Variable) Unparse(emit func(token Token), env *Env, opts ...WriteOption) {
	wo := defaultWriteOptions
	for _, o := range opts {
		o(&wo)
	}

	switch v := env.Resolve(v).(type) {
	case Variable:
		if a, ok := wo.variableNames[v]; ok {
			a.Unparse(emit, env, append(opts, WithQuoted(false))...)
			return
		}
		emit(Token{Kind: TokenVariable, Val: string(v)})
	default:
		v.Unparse(emit, env, opts...)
	}
}

// Compare compares the variable to another term.
func (v Variable) Compare(t Term, env *Env) int64 {
	switch v := env.Resolve(v).(type) {
	case Variable:
		switch t := env.Resolve(t).(type) {
		case Variable:
			return int64(strings.Compare(string(v), string(t)))
		default:
			return -1
		}
	default:
		return v.Compare(t, env)
	}
}
