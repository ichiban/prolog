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

func TestHeap_PutCompound(t *testing.T) {
	h := make(Heap, 0, 10)
	zero := make(Heap, 0)
	one := make(Heap, 0, 1)
	another := make(Heap, 0, 10)

	bar, err := another.PutCompound(NewAtom("bar"),
		Handle{cell: cell{tag: cellTagAtom, value: NewAtomRune('c').value}},
		Handle{cell: cell{tag: cellTagAtom, value: NewAtomRune('d').value}},
	)
	if err != nil {
		t.Fatal(err)
	}

	tests := []struct {
		title string
		heap  *Heap
		name  Atom
		args  []Handle
		term  Handle
		err   error
	}{
		{
			title: "atom",
			heap:  &h,
			name:  NewAtom("foo"),
			term: Handle{
				cell: cell{tag: cellTagAtom, value: NewAtom("foo").value},
			},
		},
		{
			title: "compound",
			heap:  &h,
			name:  NewAtom("foo"),
			args: []Handle{
				{
					cell: cell{tag: cellTagAtom, value: NewAtomRune('a').value},
				},
				{
					cell: cell{tag: cellTagAtom, value: NewAtomRune('b').value},
				},
			},
			term: Handle{
				heap: &h,
				cell: cell{tag: cellTagStructure, value: 0},
			},
		},
		{
			title: "not enough heap for the functor cell",
			heap:  &zero,
			name:  NewAtom("foo"),
			args: []Handle{
				{
					cell: cell{tag: cellTagAtom, value: NewAtomRune('a').value},
				},
				{
					cell: cell{tag: cellTagAtom, value: NewAtomRune('b').value},
				},
			},
			err: ErrOutOfMemory,
		},
		{
			title: "not enough heap for the args",
			heap:  &one,
			name:  NewAtom("foo"),
			args: []Handle{
				{
					cell: cell{tag: cellTagAtom, value: NewAtomRune('a').value},
				},
				{
					cell: cell{tag: cellTagAtom, value: NewAtomRune('b').value},
				},
			},
			err: ErrOutOfMemory,
		},
		{
			title: "from another heap",
			heap:  &h,
			name:  NewAtom("foo"),
			args: []Handle{
				bar,
			},
			err: ErrIncompatibleHandle,
		},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			h = h[:0]
			c, err := test.heap.PutCompound(test.name, test.args...)
			if !errors.Is(err, test.err) {
				t.Errorf("expected: %v, got: %v", test.err, err)
			}
			if c != test.term {
				t.Errorf("expected: %v, got: %v", test.term, c)
			}
		})
	}
}

func TestHeap_PutList(t *testing.T) {
	h := make(Heap, 0, 10)

	tests := []struct {
		title string
		heap  *Heap
		args  []Handle
		term  Handle
		err   error
	}{
		{
			title: "ok",
			heap:  &h,
			args: []Handle{
				{
					cell: cell{tag: cellTagAtom, value: NewAtomRune('a').value},
				},
				{
					cell: cell{tag: cellTagAtom, value: NewAtomRune('b').value},
				},
				{
					cell: cell{tag: cellTagAtom, value: NewAtomRune('c').value},
				},
			},
			term: Handle{
				heap: &h,
				cell: cell{tag: cellTagStructure, value: 0},
			},
		},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			h = h[:0]
			l, err := test.heap.PutList(test.args...)
			if !errors.Is(err, test.err) {
				t.Errorf("expected: %v, got: %v", test.err, err)
			}
			if l != test.term {
				t.Errorf("expected: %v, got: %v", test.term, l)
			}
		})
	}
}

func TestHeap_PutPartialList(t *testing.T) {
	h := make(Heap, 0, 10)
	zero := make(Heap, 0)
	four := make(Heap, 0, 4)

	tests := []struct {
		title string
		heap  *Heap
		tail  Handle
		elems []Handle
		term  Handle
		err   error
	}{
		{
			title: "ok",
			heap:  &h,
			tail:  Handle{cell: cell{tag: cellTagAtom, value: NewAtomRune('c').value}},
			elems: []Handle{
				{cell: cell{tag: cellTagAtom, value: NewAtomRune('a').value}},
				{cell: cell{tag: cellTagAtom, value: NewAtomRune('b').value}},
			},
			term: Handle{
				heap: &h,
				cell: cell{tag: cellTagStructure, value: 0},
			},
		},
		{
			title: "only tail",
			heap:  &h,
			tail:  Handle{cell: cell{tag: cellTagAtom, value: NewAtomRune('a').value}},
			elems: []Handle{},
			term:  Handle{cell: cell{tag: cellTagAtom, value: NewAtomRune('a').value}},
		},
		{
			title: "not enough heap for elements",
			heap:  &zero,
			tail:  Handle{cell: cell{tag: cellTagAtom, value: NewAtomRune('c').value}},
			elems: []Handle{
				{cell: cell{tag: cellTagAtom, value: NewAtomRune('a').value}},
				{cell: cell{tag: cellTagAtom, value: NewAtomRune('b').value}},
			},
			err: ErrOutOfMemory,
		},
		{
			title: "not enough heap for elements",
			heap:  &zero,
			tail:  Handle{cell: cell{tag: cellTagAtom, value: NewAtomRune('c').value}},
			elems: []Handle{
				{cell: cell{tag: cellTagAtom, value: NewAtomRune('a').value}},
				{cell: cell{tag: cellTagAtom, value: NewAtomRune('b').value}},
			},
			err: ErrOutOfMemory,
		},
		{
			title: "not enough heap for tail",
			heap:  &four,
			tail:  Handle{cell: cell{tag: cellTagAtom, value: NewAtomRune('c').value}},
			elems: []Handle{
				{cell: cell{tag: cellTagAtom, value: NewAtomRune('a').value}},
				{cell: cell{tag: cellTagAtom, value: NewAtomRune('b').value}},
			},
			err: ErrOutOfMemory,
		},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			h = h[:0]
			p, err := test.heap.PutPartialList(test.tail, test.elems...)
			if !errors.Is(err, test.err) {
				t.Errorf("expected: %v, got: %v", test.err, err)
			}
			if p != test.term {
				t.Errorf("expected: %v, got: %v", test.term, p)
			}
		})
	}
}

func TestHeap_PutCharList(t *testing.T) {
	h := make(Heap, 0, 10)

	tests := []struct {
		title string
		heap  *Heap
		str   string
		term  Handle
		err   error
	}{
		{
			title: "ok",
			heap:  &h,
			str:   "abc",
			term: Handle{
				heap: &h,
				cell: cell{tag: cellTagString0, value: 0},
			},
		},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			h = h[:0]
			l, err := test.heap.PutCharList(test.str)
			if !errors.Is(err, test.err) {
				t.Errorf("expected: %v, got: %v", test.err, err)
			}
			if l != test.term {
				t.Errorf("expected: %v, got: %v", test.term, l)
			}
		})
	}
}

func TestHeap_PutPartialCharList(t *testing.T) {
	h := make(Heap, 0, 10)
	zero := make(Heap, 0)
	two := make(Heap, 0, 2)

	tests := []struct {
		title string
		heap  *Heap
		str   string
		tail  Handle
		term  Handle
		err   error
	}{
		{
			title: "ok",
			heap:  &h,
			str:   "abc",
			tail:  Handle{cell: cell{tag: cellTagAtom, value: NewAtom("[]").value}},
			term: Handle{
				heap: &h,
				cell: cell{tag: cellTagString0, value: 0},
			},
		},
		{
			title: "multiple of 8",
			heap:  &h,
			str:   "abcdefghabcdefgh",
			tail:  Handle{cell: cell{tag: cellTagAtom, value: NewAtom("[]").value}},
			term: Handle{
				heap: &h,
				cell: cell{tag: cellTagString0, value: 0},
			},
		},
		{
			title: "not enough heap for chunks",
			heap:  &zero,
			str:   "abc",
			tail:  Handle{cell: cell{tag: cellTagAtom, value: NewAtom("[]").value}},
			err:   ErrOutOfMemory,
		},
		{
			title: "not enough heap for null word",
			heap:  &two,
			str:   "abcdefghabcdefgh",
			tail:  Handle{cell: cell{tag: cellTagAtom, value: NewAtom("[]").value}},
			err:   ErrOutOfMemory,
		},
		{
			title: "not enough heap for tail",
			heap:  &two,
			str:   "abcdefgh",
			tail:  Handle{cell: cell{tag: cellTagAtom, value: NewAtom("[]").value}},
			err:   ErrOutOfMemory,
		},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			h = h[:0]
			two = two[:0]
			l, err := test.heap.PutPartialCharList(test.str, test.tail)
			if !errors.Is(err, test.err) {
				t.Errorf("expected: %v, got: %v", test.err, err)
			}
			if l != test.term {
				t.Errorf("expected: %v, got: %v", test.term, l)
			}
		})
	}
}

func TestHeap_PutCodeList(t *testing.T) {
	h := make(Heap, 0, 10)

	tests := []struct {
		title string
		heap  *Heap
		str   string
		term  Handle
		err   error
	}{
		{
			title: "ok",
			heap:  &h,
			str:   "abc",
			term: Handle{
				heap: &h,
				cell: cell{tag: cellTagStructure, value: 0},
			},
		},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			h = h[:0]
			l, err := test.heap.PutCodeList(test.str)
			if !errors.Is(err, test.err) {
				t.Errorf("expected: %v, got: %v", test.err, err)
			}
			if l != test.term {
				t.Errorf("expected: %v, got: %v", test.term, l)
			}
		})
	}
}

func TestHeap_PutPartialCodeList(t *testing.T) {
	h := make(Heap, 0, 10)
	tests := []struct {
		title string
		heap  *Heap
		str   string
		tail  Handle
		term  Handle
		err   error
	}{
		{
			title: "ok",
			heap:  &h,
			str:   "abc",
			tail:  Handle{cell: cell{tag: cellTagAtom, value: NewAtom("[]").value}},
			term: Handle{
				heap: &h,
				cell: cell{tag: cellTagStructure, value: 0},
			},
		},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			h = h[:0]
			l, err := test.heap.PutPartialCodeList(test.str, test.tail)
			if !errors.Is(err, test.err) {
				t.Errorf("expected: %v, got: %v", test.err, err)
			}
			if l != test.term {
				t.Errorf("expected: %v, got: %v", test.term, l)
			}
		})
	}
}
