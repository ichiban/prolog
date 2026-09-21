package bit

import (
	"reflect"
	"testing"
)

func TestSet_Add(t *testing.T) {
	tests := []struct {
		name string
		set  *Set
		n    int
		bits []uint64
	}{
		{name: "0", set: NewSet(127), n: 0, bits: []uint64{1 << 0, 0}},
		{name: "1", set: NewSet(127), n: 1, bits: []uint64{1 << 1, 0}},
		{name: "63", set: NewSet(127), n: 63, bits: []uint64{1 << 63, 0}},
		{name: "64", set: NewSet(127), n: 64, bits: []uint64{0, 1 << 0}},
		{name: "127", set: NewSet(127), n: 127, bits: []uint64{0, 1 << 63}},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			test.set.Add(test.n)
			bits := test.set.bits
			if !reflect.DeepEqual(test.bits, bits) {
				t.Errorf("got %v, want %v", bits, test.bits)
			}
		})
	}
}

func TestSet_Exists(t *testing.T) {
	s := NewSet(127)
	s.Add(0)
	s.Add(1)
	s.Add(63)
	s.Add(64)
	s.Add(127)

	tests := []struct {
		name string
		n    int
		ok   bool
	}{
		{name: "0", n: 0, ok: true},
		{name: "1", n: 1, ok: true},
		{name: "63", n: 63, ok: true},
		{name: "64", n: 64, ok: true},
		{name: "127", n: 127, ok: true},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			ok := s.Exists(test.n)
			if ok != test.ok {
				t.Errorf("got %v, want %v", ok, test.ok)
			}
		})
	}
}

func TestSet_Rank(t *testing.T) {
	s := NewSet(127)
	s.Add(0)
	s.Add(1)
	s.Add(63)
	s.Add(64)
	s.Add(127)

	tests := []struct {
		name   string
		n      int
		result int
	}{
		{name: "0", n: 0, result: 1},
		{name: "1", n: 1, result: 2},
		{name: "2", n: 2, result: 2},
		{name: "63", n: 63, result: 3},
		{name: "64", n: 64, result: 4},
		{name: "65", n: 65, result: 4},
		{name: "127", n: 127, result: 5},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			result := s.Rank(test.n)
			if result != test.result {
				t.Errorf("got %v, want %v", result, test.result)
			}
		})
	}
}

func TestSet_Clear(t *testing.T) {
	s := NewSet(127)
	s.Add(0)
	s.Add(1)
	s.Add(63)
	s.Add(64)
	s.Add(127)
	s.Clear()

	want := []uint64{0, 0}
	if !reflect.DeepEqual(s.bits, want) {
		t.Errorf("got %v, want %v", s.bits, want)
	}
}

func TestSet_Rank_reflectsElementsAddedAfterAnEarlierRank(t *testing.T) {
	s := NewSet(127)
	s.Add(0)
	s.Add(64)

	// Rank before the set is complete, so any cached summary is populated
	// while it is still stale.
	if got, want := s.Rank(127), 2; got != want {
		t.Fatalf("got %v, want %v", got, want)
	}

	s.Add(1)
	s.Add(127)

	tests := []struct {
		name   string
		n      int
		result int
	}{
		{name: "0", n: 0, result: 1},
		{name: "1", n: 1, result: 2},
		{name: "63", n: 63, result: 2},
		{name: "64", n: 64, result: 3},
		{name: "126", n: 126, result: 3},
		{name: "127", n: 127, result: 4},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			if got := s.Rank(test.n); got != test.result {
				t.Errorf("got %v, want %v", got, test.result)
			}
		})
	}
}
