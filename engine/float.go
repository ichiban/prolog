package engine

import (
	"io"
	"strconv"
	"strings"
)

// Float is a prolog floating-point number.
type Float float64

func (f Float) number() {}

// WriteTerm outputs the Float to an io.Writer.
func (f Float) WriteTerm(w io.Writer, opts *WriteOptions, _ *Env) error {
	ew := errWriter{w: w}
	openClose := opts.left.name == atomMinus && opts.left.specifier.class() == operatorClassPrefix && f > 0

	if openClose || (f < 0 && opts.left != operator{}) {
		_, _ = ew.Write([]byte(" "))
	}

	if openClose {
		_, _ = ew.Write([]byte("("))
	}

	s := strconv.FormatFloat(float64(f), 'g', -1, 64)
	if !strings.ContainsRune(s, '.') {
		if strings.ContainsRune(s, 'e') {
			s = strings.Replace(s, "e", ".0e", 1)
		} else {
			s += ".0"
		}
	}
	_, _ = ew.Write([]byte(s))

	if openClose {
		_, _ = ew.Write([]byte(")"))
	}

	if !openClose && opts.right != (operator{}) && (opts.right.name == atomSmallE || opts.right.name == atomE) {
		_, _ = ew.Write([]byte(" "))
	}

	return ew.err
}

// Compare compares the Float with a Term.
func (f Float) Compare(t Term, env *Env) int {
	switch t := env.Resolve(t).(type) {
	case Variable:
		return 1
	case Float:
		switch {
		case f > t:
			return 1
		case f < t:
			return -1
		default:
			return 0
		}
	default: // Integer, Atom, custom atomic terms, Compound.
		return -1
	}
}
