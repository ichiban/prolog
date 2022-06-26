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
		output string
	}{
		{title: "positive", float: 33.0, output: `33.0`},
		{title: "negative", float: -33.0, output: `-33.0`},
	}

	var buf bytes.Buffer
	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			buf.Reset()
			assert.NoError(t, tt.float.WriteTerm(&buf, &WriteOptions{}, nil))
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
