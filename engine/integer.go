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
	openClose := opts.before.name == "-" && opts.before.specifier&operatorSpecifierClass == operatorSpecifierPrefix && i > 0

	if openClose || (i < 0 && opts.before != operator{}) {
		_, _ = fmt.Fprint(&ew, " ")
	}

	if openClose {
		_, _ = fmt.Fprint(&ew, "(")
	}

	s := strconv.FormatInt(int64(i), 10)
	_, _ = fmt.Fprint(&ew, s)

	if openClose {
		_, _ = fmt.Fprint(&ew, ")")
	}

	if !openClose && opts.after != (operator{}) && (opts.after.name == "e" || opts.after.name == "E") {
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
