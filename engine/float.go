package engine

import (
	"fmt"
	"github.com/cockroachdb/apd"
	"io"
	"strings"
)

// Float is a prolog floating-point number.
// The underlying implementation is not based on floating-point, it's a [GDA](https://speleotrove.com/decimal/)
// compatible implementation to avoid approximation and determinism issues.
// It allows a 34 digits precision.
type Float struct {
	dec *apd.Decimal
}

var decimal128Ctx = apd.Context{
	Precision:   34,
	MaxExponent: 6144,
	MinExponent: -6143,
	Traps:       apd.DefaultTraps,
}

func NewFloatFromString(s string) (Float, error) {
	dec, _, err := decimal128Ctx.NewFromString(s)
	f := Float{dec: dec}

	if err != nil {
		return f, representationError(flagInvalidDec, nil)
	}

	if dec.Form == apd.Infinite {
		return f, representationError(flagInfiniteDec, nil)
	}

	return f, nil
}

func NewFloatFromInt64(i int64) Float {
	var dec apd.Decimal
	dec.SetInt64(i)

	return Float{dec: &dec}
}

func mapDecimalConditionErr(flags apd.Condition) error {
	e := flags & decimal128Ctx.Traps
	if e == 0 {
		return exceptionalValueUndefined
	}

	for m := apd.Condition(1); m > 0; m <<= 1 {
		err := e & m
		if err == 0 {
			continue
		}

		switch err {
		case apd.Overflow:
			return exceptionalValueFloatOverflow
		case apd.Underflow:
			return exceptionalValueUnderflow
		case apd.Subnormal:
			return exceptionalValueUnderflow
		case apd.DivisionByZero:
			return exceptionalValueZeroDivisor
		default:
			return exceptionalValueUndefined
		}
	}

	return exceptionalValueUndefined
}

func (f Float) number() {}

// WriteTerm outputs the Float to an io.Writer.
func (f Float) WriteTerm(w io.Writer, opts *WriteOptions, _ *Env) error {
	ew := errWriter{w: w}
	openClose := opts.left.name == atomMinus && opts.left.specifier.class() == operatorClassPrefix && !f.Negative()

	if openClose || (f.Negative() && opts.left != operator{}) {
		_, _ = ew.Write([]byte(" "))
	}

	if openClose {
		_, _ = ew.Write([]byte("("))
	}

	s := fmt.Sprintf("%g", f.dec)
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
		return f.dec.Cmp(t.dec)
	default: // Integer, Atom, custom atomic terms, Compound.
		return -1
	}
}

func (f Float) String() string {
	return fmt.Sprintf("%g", f.dec)
}

func (f Float) Negative() bool {
	return f.dec.Sign() < 0
}

func (f Float) Positive() bool {
	return f.dec.Sign() > 0
}

func (f Float) Zero() bool {
	return f.dec.Sign() == 0
}

func (f Float) Eq(other Float) bool {
	return f.dec.Cmp(other.dec) == 0
}

func (f Float) Gt(other Float) bool {
	return f.dec.Cmp(other.dec) == 1
}

func (f Float) Gte(other Float) bool {
	return f.dec.Cmp(other.dec) >= 0
}

func (f Float) Lt(other Float) bool {
	return f.dec.Cmp(other.dec) == -1
}

func (f Float) Lte(other Float) bool {
	return f.dec.Cmp(other.dec) <= 0
}
