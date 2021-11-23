package engine

import (
	"strconv"
	"strings"
)

// Float is a prolog floating-point number.
type Float float64

func (f Float) String() string {
	var sb strings.Builder
	_ = Write(&sb, f, defaultWriteTermOptions, nil)
	return sb.String()
}

// Unify unifies the float with t.
func (f Float) Unify(t Term, occursCheck bool, env *Env) (*Env, bool) {
	switch t := env.Resolve(t).(type) {
	case Float:
		return env, f == t
	case Variable:
		return t.Unify(f, occursCheck, env)
	default:
		return env, false
	}
}

// Unparse emits tokens that represent the float.
func (f Float) Unparse(emit func(Token), _ WriteTermOptions, _ *Env) {
	if f < 0 {
		emit(Token{Kind: TokenSign, Val: "-"})
		f *= -1
	}
	s := strconv.FormatFloat(float64(f), 'f', -1, 64)
	if !strings.ContainsRune(s, '.') {
		s += ".0"
	}
	emit(Token{Kind: TokenFloat, Val: s})
}
