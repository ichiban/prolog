package engine

import (
	"fmt"
	"io"
	"strconv"
	"strings"
)

// Float is a prolog floating-point number.
type Float float64

func (f Float) number() {}

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

// WriteTerm writes the Float to the io.Writer.
func (f Float) WriteTerm(w io.Writer, opts *WriteOptions, _ *Env) error {
	ew := errWriter{w: w}
	openClose := opts.before.name == "-" && (opts.before.specifier == operatorSpecifierFX || opts.before.specifier == operatorSpecifierFY) && f > 0

	if openClose || (f < 0 && opts.before != operator{}) {
		_, _ = fmt.Fprint(&ew, " ")
	}

	if openClose {
		_, _ = fmt.Fprint(&ew, "(")
	}

	s := strconv.FormatFloat(float64(f), 'g', -1, 64)
	if !strings.ContainsRune(s, '.') {
		if strings.ContainsRune(s, 'e') {
			s = strings.Replace(s, "e", ".0e", 1)
		} else {
			s += ".0"
		}
	}
	_, _ = fmt.Fprint(&ew, s)

	if openClose {
		_, _ = fmt.Fprint(&ew, ")")
	}

	if !openClose && opts.after != (operator{}) && (opts.after.name == "e" || opts.after.name == "E") {
		_, _ = fmt.Fprint(&ew, " ")
	}

	return ew.err
}

// Compare compares the float to another term.
func (f Float) Compare(t Term, env *Env) int64 {
	switch t := env.Resolve(t).(type) {
	case Variable:
		return 1
	case Float:
		switch d := f - t; {
		case d < 0:
			return -1
		case d > 0:
			return 1
		default:
			return 0
		}
	default:
		return -1
	}
}
