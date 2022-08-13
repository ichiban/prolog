package engine

import (
	"bytes"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestInteger_WriteTerm(t *testing.T) {
	tests := []struct {
		title   string
		integer Integer
		opts    WriteOptions
		output  string
	}{
		{title: "positive", integer: 33, output: `33`},
		{title: "positive following unary minus", integer: 33, opts: WriteOptions{left: operator{name: "-", specifier: operatorSpecifierFX}}, output: ` (33)`},
		{title: "negative", integer: -33, output: `-33`},

		{title: "ambiguous 0b", integer: 0, opts: WriteOptions{right: operator{name: `b0`}}, output: `0 `},  // So that it won't be 0b0.
		{title: "ambiguous 0o", integer: 0, opts: WriteOptions{right: operator{name: `o0`}}, output: `0 `},  // So that it won't be 0o0.
		{title: "ambiguous 0x", integer: 0, opts: WriteOptions{right: operator{name: `x0`}}, output: `0 `},  // So that it won't be 0x0.
		{title: "ambiguous 0'", integer: 0, opts: WriteOptions{right: operator{name: `Foo`}}, output: `0 `}, // So that it won't be 0'Foo'.
	}

	var buf bytes.Buffer
	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			buf.Reset()
			assert.NoError(t, tt.integer.WriteTerm(&buf, &tt.opts, nil))
			assert.Equal(t, tt.output, buf.String())
		})
	}
}

func TestInteger_Compare(t *testing.T) {
	var m mockTerm
	defer m.AssertExpectations(t)

	assert.Equal(t, int64(-1), Integer(0).Compare(&m, nil))
	assert.Equal(t, int64(-1), Integer(0).Compare(Integer(1), nil))
	assert.Equal(t, int64(0), Integer(0).Compare(Integer(0), nil))
	assert.Equal(t, int64(1), Integer(1).Compare(Integer(0), nil))
	assert.Equal(t, int64(1), Integer(0).Compare(Float(0), nil))
	assert.Equal(t, int64(1), Integer(0).Compare(Variable("X"), nil))
}
