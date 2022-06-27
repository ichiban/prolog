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

func (f Float) WriteTerm(w io.Writer, opts *WriteOptions, _ *Env) error {
	openClose := opts.before.name == "-" && (opts.before.specifier == operatorSpecifierFX || opts.before.specifier == operatorSpecifierFY) && f > 0

	if openClose || (f < 0 && opts.before != operator{}) {
		if _, err := fmt.Fprint(w, " "); err != nil {
			return err
		}
	}

	if openClose {
		if _, err := fmt.Fprint(w, "("); err != nil {
			return err
		}
	}

	s := strconv.FormatFloat(float64(f), 'f', -1, 64)
	if !strings.ContainsRune(s, '.') {
		s += ".0"
	}
	if _, err := fmt.Fprint(w, s); err != nil {
		return err
	}

	if openClose {
		if _, err := fmt.Fprint(w, ")"); err != nil {
			return err
		}
	}

	return nil
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
