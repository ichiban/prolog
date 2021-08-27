package term

import (
	"bytes"
	"fmt"
	"io"
	"strconv"
	"strings"
)

// Float is a prolog floating-point number.
type Float float64

func (f Float) String() string {
	var buf bytes.Buffer
	_ = f.WriteTerm(&buf, DefaultWriteTermOptions, nil)
	return buf.String()
}

// WriteTerm writes the float into w.
func (f Float) WriteTerm(w io.Writer, _ WriteTermOptions, _ Env) error {
	s := strconv.FormatFloat(float64(f), 'f', -1, 64)
	if !strings.ContainsRune(s, '.') {
		s += ".0"
	}
	_, err := fmt.Fprint(w, s)
	return err
}

// Unify unifies the float with t.
func (f Float) Unify(t Interface, occursCheck bool, env *Env) bool {
	switch t := env.Resolve(t).(type) {
	case Float:
		return f == t
	case Variable:
		return t.Unify(f, occursCheck, env)
	default:
		return false
	}
}
