package engine

import (
	"errors"
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestErrWriter_Write(t *testing.T) {
	var failed = errors.New("failed")

	var m mockWriter
	m.On("Write", []byte("foo")).Return(0, failed).Once()
	defer m.AssertExpectations(t)

	ew := errWriter{w: &m}
	_, err := ew.Write([]byte("foo"))
	assert.NoError(t, err)
	_, err = ew.Write([]byte("bar"))
	assert.NoError(t, err)
	_, err = ew.Write([]byte("baz"))
	assert.NoError(t, err)
	assert.Equal(t, failed, ew.err)
}

func TestCompareAtomic(t *testing.T) {
	type x struct {
		mockTerm
	}
	type y struct {
		mockTerm
		val int
	}
	type z struct {
		mockTerm
	}

	cmp := func(y1 *y, y2 *y) int {
		return y1.val - y2.val
	}

	tests := []struct {
		a   *y
		t   Term
		cmp func(*y, *y) int
		o   int
	}{
		{a: &y{}, t: NewVariable(), o: 1},
		{a: &y{}, t: NewFloatFromInt64(0), o: 1},
		{a: &y{}, t: Integer(0), o: 1},
		{a: &y{}, t: Atom(0), o: 1},
		{a: &y{}, t: &x{}, o: 1},
		{a: &y{val: 1}, t: &y{val: 0}, cmp: cmp, o: 1},
		{a: &y{val: 0}, t: &y{val: 0}, cmp: cmp, o: 0},
		{a: &y{val: 0}, t: &y{val: 1}, cmp: cmp, o: -1},
		{a: &y{}, t: &z{}, o: -1},
		{a: &y{}, t: Atom(0).Apply(Integer(0)), o: -1},
	}

	for _, tt := range tests {
		assert.Equal(t, tt.o, CompareAtomic[*y](tt.a, tt.t, tt.cmp, nil))
	}
}
