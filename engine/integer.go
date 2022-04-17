package engine

import (
	"strconv"
)

// Integer is a prolog integer.
type Integer int64

func (i Integer) number() {}

// Unify unifies the integer with t.
func (i Integer) Unify(t Term, occursCheck bool, env *Env) (*Env, bool) {
	switch t := env.Resolve(t).(type) {
	case Integer:
		return env, i == t
	case Variable:
		return t.Unify(i, occursCheck, env)
	default:
		return env, false
	}
}

// Unparse emits tokens that represent the integer.
func (i Integer) Unparse(emit func(token Token), _ *Env, _ ...WriteOption) {
	if i < 0 {
		emit(Token{Kind: TokenGraphic, Val: "-"})
		i *= -1
	}
	s := strconv.FormatInt(int64(i), 10)
	emit(Token{Kind: TokenInteger, Val: s})
}

// Compare compares the integer to another term.
func (i Integer) Compare(t Term, env *Env) int64 {
	switch t := env.Resolve(t).(type) {
	case Variable, Float:
		return 1
	case Integer:
		return int64(i - t)
	default:
		return -1
	}
}
