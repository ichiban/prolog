package term

import (
	"errors"
	"math"
	"testing"
)

func TestHeap_PutVariable(t *testing.T) {
	h := make(Heap, 0, 1)
	zero := make(Heap, 0)

	tests := []struct {
		title string
		heap  *Heap
		term  Handle
		err   error
	}{
		{
			title: "ok",
			heap:  &h,
			term: Handle{
				heap: &h,
				cell: cell{tag: cellTagReference, value: 0},
			},
		},
		{
			title: "ng",
			heap:  &zero,
			err:   ErrOutOfMemory,
		},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			h = h[:0]
			v, err := tt.heap.PutVariable()
			if !errors.Is(err, tt.err) {
				t.Errorf("got %v, want %v", err, tt.err)
			}
			if v != tt.term {
				t.Errorf("expected: %v, got: %v", tt.term, v)
			}
		})
	}
}

func TestHeap_PutAtom(t *testing.T) {
	h := make(Heap, 0, 1)

	tests := []struct {
		title string
		heap  *Heap
		atom  Atom
		term  Handle
		err   error
	}{
		{
			title: "single char",
			atom:  NewAtomRune('a'),
			term: Handle{
				cell: cell{tag: cellTagCharacter, value: 'a'},
			},
		},
		{
			title: "multiple chars",
			atom:  NewAtom("foo"),
			term: Handle{
				cell: cell{tag: cellTagAtom, value: NewAtom("foo").value},
			},
		},
		{
			title: "ng",
			atom:  Atom{},
			term:  Handle{},
		},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			h = h[:0]
			a, err := h.PutAtom(tt.atom)
			if !errors.Is(err, tt.err) {
				t.Errorf("expected: %v, got: %v", tt.err, err)
			}

			if a != tt.term {
				t.Errorf("expected: %v, got: %v", tt.term, a)
			}
		})
	}
}

func TestHeap_PutInteger(t *testing.T) {
	h := make(Heap, 0, 1)
	zero := make(Heap, 0)

	tests := []struct {
		title   string
		heap    *Heap
		integer int64
		term    Handle
		err     error
	}{
		{
			title:   "int64",
			heap:    &h,
			integer: math.MaxInt32 + 1,
			term: Handle{
				heap: &h,
				cell: cell{tag: cellTagInt64, value: 0},
			},
		},
		{
			title:   "int32",
			heap:    &h,
			integer: math.MaxInt32 - 1,
			term: Handle{
				cell: cell{tag: cellTagInt32, value: math.MaxInt32 - 1},
			},
		},
		{
			title:   "out of memory",
			heap:    &zero,
			integer: math.MaxInt32 + 1,
			err:     ErrOutOfMemory,
		},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			h = h[:0]
			i, err := tt.heap.PutInteger(tt.integer)
			if !errors.Is(err, tt.err) {
				t.Errorf("expected: %v, got: %v", tt.err, err)
			}

			if i != tt.term {
				t.Errorf("expected: %v, got: %v", tt.term, i)
			}
		})
	}
}

func TestHeap_PutFloat(t *testing.T) {
	h := make(Heap, 0, 1)
	zero := make(Heap, 0)

	tests := []struct {
		title string
		heap  *Heap
		float float64
		term  Handle
		err   error
	}{
		{
			title: "ok",
			heap:  &h,
			float: 1,
			term: Handle{
				heap: &h,
				cell: cell{tag: cellTagFloat, value: 0},
			},
		},
		{
			title: "ng",
			heap:  &zero,
			float: 1,
			err:   ErrOutOfMemory,
		},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			h = h[:0]
			f, err := tt.heap.PutFloat(tt.float)
			if !errors.Is(err, tt.err) {
				t.Errorf("expected: %v, got: %v", tt.err, err)
			}

			if f != tt.term {
				t.Errorf("expected: %v, got: %v", tt.term, f)
			}
		})
	}
}
