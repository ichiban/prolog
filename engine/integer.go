package engine

import (
	"io"
	"strconv"
)

// Integer is a prolog integer.
type Integer int64

func (i Integer) number() {}

func (i Integer) WriteTerm(w io.Writer, opts *WriteOptions, _ *Env) error {
	ew := errWriter{w: w}
	openClose := opts.left.name == atomMinus && opts.left.specifier.class() == operatorClassPrefix && i > 0

	if openClose {
		_, _ = ew.Write([]byte(" ("))
		opts = opts.withLeft(operator{}).withRight(operator{})
	} else {
		if opts.left != (operator{}) && (letterDigit(opts.left.name) || (i < 0 && graphic(opts.left.name))) {
			_, _ = ew.Write([]byte(" "))
		}
	}

	s := strconv.FormatInt(int64(i), 10)
	_, _ = ew.Write([]byte(s))

	if openClose {
		_, _ = ew.Write([]byte(")"))
	}

	// Avoid ambiguous 0b, 0o, 0x or 0'.
	if !openClose && opts.right != (operator{}) && (letterDigit(opts.right.name) || (needQuoted(opts.right.name) && opts.right.name != atomComma && opts.right.name != atomBar)) {
		_, _ = ew.Write([]byte(" "))
	}

	return ew.err
}

func (i Integer) Compare(t Term, env *Env) int {
	switch t := env.Resolve(t).(type) {
	case Variable, Float:
		return 1
	case Integer:
		switch {
		case i > t:
			return 1
		case i < t:
			return -1
		default:
			return 0
		}
	default:
		return -1
	}
}
