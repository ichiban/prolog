package engine

import (
	"bytes"
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestIntegerNumber(t *testing.T) {
	assert.Implements(t, (*Number)(nil), Integer(0))
}

func TestInteger_WriteTerm(t *testing.T) {
	tests := []struct {
		title  string
		i      Integer
		opts   WriteOptions
		output string
	}{
		{title: "positive", i: 33, output: `33`},
		{title: "positive following unary minus", i: 33, opts: WriteOptions{left: operator{name: atomMinus, specifier: operatorSpecifierFX}}, output: ` (33)`},
		{title: "negative", i: -33, output: `-33`},
		{title: "ambiguous 0b", i: 0, opts: WriteOptions{right: operator{name: NewAtom(`b0`)}}, output: `0 `},  // So that it won't be 0b0.
		{title: "ambiguous 0o", i: 0, opts: WriteOptions{right: operator{name: NewAtom(`o0`)}}, output: `0 `},  // So that it won't be 0o0.
		{title: "ambiguous 0x", i: 0, opts: WriteOptions{right: operator{name: NewAtom(`x0`)}}, output: `0 `},  // So that it won't be 0x0.
		{title: "ambiguous 0'", i: 0, opts: WriteOptions{right: operator{name: NewAtom(`Foo`)}}, output: `0 `}, // So that it won't be 0'Foo'.
	}

	var buf bytes.Buffer
	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			buf.Reset()
			assert.NoError(t, tt.i.WriteTerm(&buf, &tt.opts, nil))
			assert.Equal(t, tt.output, buf.String())
		})
	}
}

func TestInteger_Compare(t *testing.T) {
	x := NewVariable()

	tests := []struct {
		title string
		i     Integer
		t     Term
		o     int
	}{
		{title: `1 > X`, i: 1, t: x, o: 1},
		{title: `1 > 1.0`, i: 1, t: NewFloatFromInt64(1), o: 1},
		{title: `1 > 0`, i: 1, t: Integer(0), o: 1},
		{title: `1 = 1`, i: 1, t: Integer(1), o: 0},
		{title: `1 < 2`, i: 1, t: Integer(2), o: -1},
		{title: `1 < a`, i: 1, t: NewAtom("a"), o: -1},
		{title: `1 < f(a)`, i: 1, t: NewAtom("f").Apply(NewAtom("a")), o: -1},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			assert.Equal(t, tt.o, tt.i.Compare(tt.t, nil))
		})
	}
}
