package term

import (
	"bytes"
	"fmt"
	"io"
	"strconv"
)

// Integer is a prolog integer.
type Integer int64

func (i Integer) String() string {
	var buf bytes.Buffer
	_ = i.WriteTerm(&buf, DefaultWriteTermOptions, nil)
	return buf.String()
}

// WriteTerm writes the integer into w.
func (i Integer) WriteTerm(w io.Writer, _ WriteTermOptions, _ *Env) error {
	_, err := fmt.Fprint(w, strconv.FormatInt(int64(i), 10))
	return err
}

// Unify unifies the integer with t.
func (i Integer) Unify(t Interface, occursCheck bool, env *Env) (*Env, bool) {
	switch t := env.Resolve(t).(type) {
	case Integer:
		return env, i == t
	case Variable:
		return t.Unify(i, occursCheck, env)
	default:
		return env, false
	}
}
