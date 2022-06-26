package engine

import (
	"fmt"
	"io"
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

func (i Integer) WriteTerm(w io.Writer, _ *WriteOptions, _ *Env) error {
	s := strconv.FormatInt(int64(i), 10)
	_, err := fmt.Fprint(w, s)
	return err
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
