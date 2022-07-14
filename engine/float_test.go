package engine

import (
	"bytes"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestFloat_WriteTerm(t *testing.T) {
	tests := []struct {
		title  string
		float  Float
		opts   WriteOptions
		output string
	}{
		{title: "positive", float: 33.0, output: `33.0`},
		{title: "with e", float: 3.0e+100, output: `3.0e+100`},
		{title: "positive following unary minus", float: 33.0, opts: WriteOptions{left: operator{specifier: operatorSpecifierFX, name: "-"}}, output: ` (33.0)`},
		{title: "negative", float: -33.0, output: `-33.0`},

		{title: "ambiguous e", float: 33.0, opts: WriteOptions{right: operator{name: `e`}}, output: `33.0 `}, // So that it won't be 33.0e.
	}

	var buf bytes.Buffer
	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			buf.Reset()
			assert.NoError(t, tt.float.WriteTerm(&buf, &tt.opts, nil))
			assert.Equal(t, tt.output, buf.String())
		})
	}
}

func TestFloat_Compare(t *testing.T) {
	var m mockTerm
	defer m.AssertExpectations(t)

	assert.Equal(t, int64(-1), Float(0).Compare(&m, nil))
	assert.Equal(t, int64(-1), Float(0).Compare(Integer(0), nil))
	assert.Equal(t, int64(-1), Float(0).Compare(Float(1), nil))
	assert.Equal(t, int64(0), Float(0).Compare(Float(0), nil))
	assert.Equal(t, int64(1), Float(1).Compare(Float(0), nil))
	assert.Equal(t, int64(1), Float(1).Compare(Variable("X"), nil))
}
