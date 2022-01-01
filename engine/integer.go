package engine

import (
	"fmt"
	"io"
	"strconv"
	"strings"
)

// Integer is a prolog integer.
type Integer int64

func (i Integer) String() string {
	var sb strings.Builder
	_ = Write(&sb, i, nil)
	return sb.String()
}

// WriteTerm writes the integer into w.
func (i Integer) WriteTerm(w io.Writer, _ writeTermOptions, _ *Env) error {
	_, err := fmt.Fprint(w, strconv.FormatInt(int64(i), 10))
	return err
}

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
		emit(Token{Kind: TokenSign, Val: "-"})
		i *= -1
	}
	s := strconv.FormatInt(int64(i), 10)
	emit(Token{Kind: TokenInteger, Val: s})
}

// Compare compares the integer to another term.
func (i Integer) Compare(t Term, env *Env) int64 {
	switch t := env.Resolve(t).(type) {
	case Variable:
		return 1
	case Float:
		d := int64(Float(i) - t)
		if d == 0 {
			return 1
		}
		return d
	case Integer:
		return int64(i - t)
	default:
		return -1
	}
}
