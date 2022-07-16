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

// WriteTerm writes the Integer to the io.Writer.
func (i Integer) WriteTerm(w io.Writer, opts *WriteOptions, _ *Env) error {
	ew := errWriter{w: w}
	openClose := opts.left.name == "-" && opts.left.specifier.class() == operatorClassPrefix && i > 0

	if openClose {
		_, _ = fmt.Fprint(&ew, " (")
		opts = opts.withLeft(operator{}).withRight(operator{})
	} else {
		if opts.left != (operator{}) && (letterDigit(opts.left.name) || (i < 0 && graphic(opts.left.name))) {
			_, _ = fmt.Fprint(&ew, " ")
		}
	}

	s := strconv.FormatInt(int64(i), 10)
	_, _ = fmt.Fprint(&ew, s)

	if openClose {
		_, _ = fmt.Fprint(&ew, ")")
	}

	// Avoid ambiguous 0b, 0o, 0x or 0'.
	if !openClose && opts.right != (operator{}) && (letterDigit(opts.right.name) || (needQuoted(opts.right.name) && opts.right.name != "," && opts.right.name != "|")) {
		_, _ = fmt.Fprint(&ew, " ")
	}

	return ew.err
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
