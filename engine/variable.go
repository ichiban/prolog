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

func NewVariable() Variable {
	atomic.AddUint64(&varCounter, 1)
	return Variable(fmt.Sprintf("_%d", varCounter))
}

var generatedPattern = regexp.MustCompile(`\A_\d+\z`)

func (v Variable) Generated() bool {
	return generatedPattern.MatchString(string(v))
}

func (v Variable) String() string {
	var sb strings.Builder
	_ = Write(&sb, v, defaultWriteTermOptions, nil)
	return sb.String()
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
func (v Variable) Unparse(emit func(token Token), opts WriteTermOptions, env *Env) {
	switch v := env.Resolve(v).(type) {
	case Variable:
		emit(Token{Kind: TokenVariable, Val: string(v)})
	default:
		v.Unparse(emit, opts, env)
	}
}