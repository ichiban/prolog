package engine

import (
	"bytes"
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestFloatNumber(t *testing.T) {
	assert.Implements(t, (*Number)(nil), Float(0))
}

func TestFloat_WriteTerm(t *testing.T) {
	tests := []struct {
		title  string
		f      Float
		opts   WriteOptions
		output string
	}{
		{title: "positive", f: 33.0, output: `33.0`},
		{title: "with e", f: 3.0e+100, output: `3.0e+100`},
		{title: "positive following unary minus", f: 33.0, opts: WriteOptions{left: operator{specifier: operatorSpecifierFX, name: atomMinus}}, output: ` (33.0)`},
		{title: "negative", f: -33.0, output: `-33.0`},
		{title: "ambiguous e", f: 33.0, opts: WriteOptions{right: operator{name: NewAtom(`e`)}}, output: `33.0 `}, // So that it won't be 33.0e.
	}

	var buf bytes.Buffer
	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			buf.Reset()
			assert.NoError(t, tt.f.WriteTerm(&buf, &tt.opts, nil))
			assert.Equal(t, tt.output, buf.String())
		})
	}
}

func TestFloat_Compare(t *testing.T) {
	x := NewVariable()

	tests := []struct {
		title string
		f     Float
		t     Term
		o     int
	}{
		{title: `1.0 > X`, f: Float(1), t: x, o: 1},
		{title: `1.0 > 0.0`, f: Float(1), t: Float(0), o: 1},
		{title: `1.0 = 1.0`, f: Float(1), t: Float(1), o: 0},
		{title: `1.0 < 2.0`, f: Float(1), t: Float(2), o: -1},
		{title: `1.0 < 1`, f: Float(1), t: Integer(1), o: -1},
		{title: `1.0 < a`, f: Float(1), t: NewAtom("a"), o: -1},
		{title: `1.0 < f(a)`, f: Float(1), t: NewAtom("f").Apply(NewAtom("a")), o: -1},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			assert.Equal(t, tt.o, tt.f.Compare(tt.t, nil))
		})
	}
}
