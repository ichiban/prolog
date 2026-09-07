package term

import (
	"errors"
	"iter"
	"math"
	"reflect"
	"slices"
	"testing"
)

func TestArena_PutVariable(t *testing.T) {
	tests := []struct {
		title string
		arena *Arena
		term  Cell
		err   error
	}{
		{
			title: "ok",
			arena: NewArena(1),
			term:  Cell{tag: cellTagReference, value: 0},
		},
		{
			title: "ng",
			arena: NewArena(0),
			err:   ErrOutOfMemory,
		},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			v, err := tt.arena.PutVariable()
			if !errors.Is(err, tt.err) {
				t.Errorf("got %v, want %v", err, tt.err)
			}
			if v != tt.term {
				t.Errorf("expected: %v, got: %v", tt.term, v)
			}
		})
	}
}

func TestArena_Variable(t *testing.T) {
	arena := NewArena(1)

	tests := []struct {
		title string
		term  Cell
		addr  int
		ok    bool
	}{
		{
			title: "variable",
			term:  must(arena.PutVariable()),
			addr:  0,
			ok:    true,
		},
		{
			title: "not variable",
			term:  must(arena.PutInteger(1)),
			ok:    false,
		},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			addr, ok := arena.Variable(test.term)
			if ok != test.ok {
				t.Errorf("expected: %v, got: %v", test.ok, ok)
			}
			if addr != test.addr {
				t.Errorf("expected: %v, got: %v", test.addr, addr)
			}
		})
	}
}

func TestArena_Deref(t *testing.T) {
	arena := Arena{
		Heap: Heap{
			Cell{tag: cellTagReference, value: 0},
			Cell{tag: cellTagCharacter, value: 'a'},
			Cell{tag: cellTagReference, value: 1},
		},
	}

	tests := []struct {
		title  string
		term   Cell
		result Cell
	}{
		{
			title:  "free variable",
			term:   Cell{tag: cellTagReference, value: 0},
			result: Cell{tag: cellTagReference, value: 0},
		},
		{
			title:  "bound variable",
			term:   Cell{tag: cellTagReference, value: 2},
			result: Cell{tag: cellTagCharacter, value: 'a'},
		},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			ret := arena.Deref(test.term)
			if ret != test.result {
				t.Errorf("expected: %v, got: %v", test.result, ret)
			}
		})
	}
}

func TestArena_Bind(t *testing.T) {
	arena := NewArena(2)

	x := must(arena.PutVariable())
	i := must(arena.PutInteger(1))

	tests := []struct {
		title string
		x, y  Cell
		err   error
	}{
		{
			title: "variable",
			x:     x,
			y:     i,
		},
		{
			title: "not variable",
			x:     i,
			y:     x,
			err:   ErrUnsupportedOperation,
		},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			err := arena.Bind(test.x, test.y)
			if !errors.Is(err, test.err) {
				t.Fatalf("expected: %v, got: %v", test.err, err)
			}
		})
	}
}

func TestArena_PutAtom(t *testing.T) {
	arena := NewArena(2)

	tests := []struct {
		title string
		heap  *Heap
		atom  Atom
		term  Cell
		err   error
	}{
		{
			title: "single char",
			atom:  NewAtomRune('a'),
			term:  Cell{tag: cellTagCharacter, value: 'a'},
		},
		{
			title: "multiple chars",
			atom:  NewAtom("foo"),
			term:  Cell{tag: cellTagAtom, value: NewAtom("foo").value},
		},
		{
			title: "ng",
			atom:  Atom{},
			term:  Cell{},
		},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			a, err := arena.PutAtom(tt.atom)
			if !errors.Is(err, tt.err) {
				t.Errorf("expected: %v, got: %v", tt.err, err)
			}

			if a != tt.term {
				t.Errorf("expected: %v, got: %v", tt.term, a)
			}
		})
	}
}

func TestArena_Atom(t *testing.T) {
	arena := NewArena(1)

	tests := []struct {
		title string
		term  Cell
		atom  Atom
		ok    bool
	}{
		{
			title: "character",
			term:  must(arena.PutAtom(NewAtomRune('a'))),
			atom:  NewAtomRune('a'),
			ok:    true,
		},
		{
			title: "atom",
			term:  must(arena.PutAtom(NewAtom("foo"))),
			atom:  NewAtom("foo"),
			ok:    true,
		},
		{
			title: "not atom nor character",
			term:  must(arena.PutInteger(1)),
			atom:  Atom{kind: atomKindInvalid},
			ok:    false,
		},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			a, ok := arena.Atom(test.term)
			if ok != test.ok {
				t.Errorf("expected: %v, got: %v", test.ok, ok)
			}
			if a != test.atom {
				t.Errorf("expected: %v, got: %v", test.atom, a)
			}
		})
	}
}

func TestArena_PutInteger(t *testing.T) {
	tests := []struct {
		title   string
		arena   *Arena
		integer int64
		term    Cell
		err     error
	}{
		{
			title:   "int64",
			arena:   NewArena(1),
			integer: math.MaxInt32 + 1,
			term:    Cell{tag: cellTagInt64, value: 0},
		},
		{
			title:   "int32",
			arena:   NewArena(1),
			integer: math.MaxInt32 - 1,
			term:    Cell{tag: cellTagInt32, value: math.MaxInt32 - 1},
		},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			i, err := tt.arena.PutInteger(tt.integer)
			if !errors.Is(err, tt.err) {
				t.Errorf("expected: %v, got: %v", tt.err, err)
			}

			if i != tt.term {
				t.Errorf("expected: %v, got: %v", tt.term, i)
			}
		})
	}
}

func TestArena_Integer(t *testing.T) {
	arena := NewArena(1)

	tests := []struct {
		title string
		term  Cell
		n     int64
		ok    bool
	}{
		{
			title: "int64",
			term:  must(arena.PutInteger(math.MaxInt32 + 1)),
			n:     math.MaxInt32 + 1,
			ok:    true,
		},
		{
			title: "int32",
			term:  must(arena.PutInteger(math.MaxInt32)),
			n:     math.MaxInt32,
			ok:    true,
		},
		{
			title: "not integer",
			term:  must(arena.PutAtom(NewAtomRune('a'))),
			ok:    false,
		},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			i, ok := arena.Integer(test.term)
			if ok != test.ok {
				t.Errorf("expected: %v, got: %v", test.ok, ok)
			}
			if i != test.n {
				t.Errorf("expected: %v, got: %v", test.n, i)
			}
		})
	}
}

func TestArena_PutFloat(t *testing.T) {
	tests := []struct {
		title string
		arena *Arena
		float float64
		term  Cell
		err   error
	}{
		{
			title: "ok",
			arena: NewArena(1),
			float: 1,
			term:  Cell{tag: cellTagFloat, value: 0},
		},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			f, err := tt.arena.PutFloat(tt.float)
			if !errors.Is(err, tt.err) {
				t.Errorf("expected: %v, got: %v", tt.err, err)
			}

			if f != tt.term {
				t.Errorf("expected: %v, got: %v", tt.term, f)
			}
		})
	}
}

// Integers and floats live in a raw heap slot rather than a cell, so the bits
// have to survive the round trip exactly.
func TestArena_numberRoundTrip(t *testing.T) {
	t.Run("integer", func(t *testing.T) {
		for _, n := range []int64{
			math.MinInt64, math.MinInt32 - 1, math.MinInt32, -1, 0, 1,
			math.MaxInt32, math.MaxInt32 + 1, math.MaxInt64,
		} {
			arena := NewArena(1)

			h, err := arena.PutInteger(n)
			if err != nil {
				t.Fatal(err)
			}
			got, ok := arena.Integer(h)
			if !ok {
				t.Errorf("%d is not an integer", n)
			}
			if got != n {
				t.Errorf("expected: %d, got: %d", n, got)
			}
		}
	})

	t.Run("float", func(t *testing.T) {
		for _, f := range []float64{
			0, math.Copysign(0, -1), 1, -1, 1.5,
			math.MaxFloat64, -math.MaxFloat64, math.SmallestNonzeroFloat64,
			math.Inf(1), math.Inf(-1), math.NaN(),
		} {
			arena := NewArena(1)

			h, err := arena.PutFloat(f)
			if err != nil {
				t.Fatal(err)
			}
			got, ok := arena.Float(h)
			if !ok {
				t.Errorf("%v is not a float", f)
			}
			// Compare bits so that NaN and -0.0 are held to the same standard.
			if math.Float64bits(got) != math.Float64bits(f) {
				t.Errorf("expected: %x, got: %x", math.Float64bits(f), math.Float64bits(got))
			}
		}
	})

	t.Run("out of memory", func(t *testing.T) {
		arena := NewArena(0)

		if _, err := arena.PutFloat(1); !errors.Is(err, ErrOutOfMemory) {
			t.Errorf("expected: %v, got: %v", ErrOutOfMemory, err)
		}
		if _, err := arena.PutInteger(math.MaxInt64); !errors.Is(err, ErrOutOfMemory) {
			t.Errorf("expected: %v, got: %v", ErrOutOfMemory, err)
		}
		// A small integer is immediate, so it needs no heap.
		if _, err := arena.PutInteger(1); err != nil {
			t.Errorf("expected: %v, got: %v", nil, err)
		}
	})
}

func TestArena_Float(t *testing.T) {
	arena := NewArena(1)

	tests := []struct {
		title string
		term  Cell
		f     float64
		ok    bool
	}{
		{
			title: "float64",
			term:  must(arena.PutFloat(1)),
			f:     1,
			ok:    true,
		},
		{
			title: "not float64",
			term:  must(arena.PutAtom(NewAtomRune('a'))),
			ok:    false,
		},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			f, ok := arena.Float(test.term)
			if ok != test.ok {
				t.Errorf("expected: %v, got: %v", test.ok, ok)
			}
			if f != test.f {
				t.Errorf("expected: %v, got: %v", test.f, f)
			}
		})
	}
}

func TestArena_PutFunctor(t *testing.T) {
	tests := []struct {
		title   string
		functor Functor
		name    Atom
		arity   int64
	}{
		{
			title:   "ordinary",
			functor: NewFunctor(NewAtom("foo"), 2),
			name:    NewAtom("foo"),
			arity:   2,
		},
		{
			// The zero Functor has no name. It still has to come out as a
			// well-formed term since it ends up in error terms.
			title:   "zero",
			functor: Functor{},
			name:    NewAtom(""),
			arity:   0,
		},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			arena := NewArena(3)

			f, err := arena.PutFunctor(tt.functor)
			if err != nil {
				t.Fatal(err)
			}

			if pi, ok := arena.Functor(f); !ok || pi != NewFunctor(NewAtomRune('/'), 2) {
				t.Fatalf("expected: /2, got: %v (ok=%v)", pi, ok)
			}

			name, ok := arena.Atom(arena.Arg(f, 0))
			if !ok {
				t.Errorf("name is not an atom")
			}
			if name != tt.name {
				t.Errorf("expected: %v, got: %v", tt.name, name)
			}

			arity, ok := arena.Integer(arena.Arg(f, 1))
			if !ok {
				t.Errorf("arity is not an integer")
			}
			if arity != tt.arity {
				t.Errorf("expected: %v, got: %v", tt.arity, arity)
			}
		})
	}
}

func TestArena_PutCompound(t *testing.T) {
	tests := []struct {
		title string
		arena *Arena
		name  Atom
		args  []Cell
		term  Cell
		err   error
	}{
		{
			title: "atom",
			arena: NewArena(0),
			name:  NewAtom("foo"),
			term:  Cell{tag: cellTagAtom, value: NewAtom("foo").value},
		},
		{
			title: "compound",
			arena: NewArena(3),
			name:  NewAtom("foo"),
			args: []Cell{
				{tag: cellTagAtom, value: NewAtomRune('a').value},
				{tag: cellTagAtom, value: NewAtomRune('b').value},
			},
			term: Cell{tag: cellTagStructure, value: 0},
		},
		{
			title: "not enough heap for the functor cell",
			arena: NewArena(0),
			name:  NewAtom("foo"),
			args: []Cell{
				{tag: cellTagAtom, value: NewAtomRune('a').value},
				{tag: cellTagAtom, value: NewAtomRune('b').value},
			},
			err: ErrOutOfMemory,
		},
		{
			title: "not enough heap for the args",
			arena: NewArena(1),
			name:  NewAtom("foo"),
			args: []Cell{
				{tag: cellTagAtom, value: NewAtomRune('a').value},
				{tag: cellTagAtom, value: NewAtomRune('b').value},
			},
			err: ErrOutOfMemory,
		},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			c, err := test.arena.PutCompound(test.name, test.args...)
			if !errors.Is(err, test.err) {
				t.Errorf("expected: %v, got: %v", test.err, err)
			}
			if c != test.term {
				t.Errorf("expected: %v, got: %v", test.term, c)
			}
		})
	}
}

func TestArena_PutList(t *testing.T) {
	tests := []struct {
		title string
		arena *Arena
		args  []Cell
		term  Cell
		err   error
	}{
		{
			title: "ok",
			arena: NewArena(10),
			args: []Cell{
				{tag: cellTagAtom, value: NewAtomRune('a').value},
				{tag: cellTagAtom, value: NewAtomRune('b').value},
				{tag: cellTagAtom, value: NewAtomRune('c').value},
			},
			term: Cell{tag: cellTagStructure, value: 0},
		},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			l, err := test.arena.PutList(test.args...)
			if !errors.Is(err, test.err) {
				t.Errorf("expected: %v, got: %v", test.err, err)
			}
			if l != test.term {
				t.Errorf("expected: %v, got: %v", test.term, l)
			}
		})
	}
}

func TestArena_PutPartialList(t *testing.T) {
	tests := []struct {
		title string
		arena *Arena
		tail  Cell
		elems []Cell
		term  Cell
		err   error
	}{
		{
			title: "ok",
			arena: NewArena(10),
			tail:  Cell{tag: cellTagAtom, value: NewAtomRune('c').value},
			elems: []Cell{
				{tag: cellTagAtom, value: NewAtomRune('a').value},
				{tag: cellTagAtom, value: NewAtomRune('b').value},
			},
			term: Cell{tag: cellTagStructure, value: 0},
		},
		{
			title: "only tail",
			arena: NewArena(10),
			tail:  Cell{tag: cellTagAtom, value: NewAtomRune('a').value},
			elems: []Cell{},
			term:  Cell{tag: cellTagAtom, value: NewAtomRune('a').value},
		},
		{
			title: "not enough heap for elements",
			arena: NewArena(0),
			tail:  Cell{tag: cellTagAtom, value: NewAtomRune('c').value},
			elems: []Cell{
				{tag: cellTagAtom, value: NewAtomRune('a').value},
				{tag: cellTagAtom, value: NewAtomRune('b').value},
			},
			err: ErrOutOfMemory,
		},
		{
			title: "not enough heap for tail",
			arena: NewArena(4),
			tail:  Cell{tag: cellTagAtom, value: NewAtomRune('c').value},
			elems: []Cell{
				{tag: cellTagAtom, value: NewAtomRune('a').value},
				{tag: cellTagAtom, value: NewAtomRune('b').value},
			},
			err: ErrOutOfMemory,
		},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			p, err := test.arena.PutPartialList(test.tail, test.elems...)
			if !errors.Is(err, test.err) {
				t.Errorf("expected: %v, got: %v", test.err, err)
			}
			if p != test.term {
				t.Errorf("expected: %v, got: %v", test.term, p)
			}
		})
	}
}

func TestArena_PutCharList(t *testing.T) {
	tests := []struct {
		title string
		arena *Arena
		str   string
		term  Cell
		err   error
	}{
		{
			title: "ok",
			arena: NewArena(10),
			str:   "abc",
			term:  Cell{tag: cellTagString, value: 0, aux: 0},
		},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			l, err := test.arena.PutCharList(test.str)
			if !errors.Is(err, test.err) {
				t.Errorf("expected: %v, got: %v", test.err, err)
			}
			if l != test.term {
				t.Errorf("expected: %v, got: %v", test.term, l)
			}
		})
	}
}

func TestArena_PutPartialCharList(t *testing.T) {
	tests := []struct {
		title string
		arena *Arena
		str   string
		tail  Cell
		term  Cell
		err   error
	}{
		{
			title: "ok",
			arena: NewArena(10),
			str:   "abc",
			tail:  Cell{tag: cellTagAtom, value: NewAtom("[]").value},
			term:  Cell{tag: cellTagString, value: 0},
		},
		{
			title: "multiple of 8",
			arena: NewArena(10),
			str:   "abcdefghabcdefgh",
			tail:  Cell{tag: cellTagAtom, value: NewAtom("[]").value},
			term:  Cell{tag: cellTagString, value: 0},
		},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			l, err := test.arena.PutPartialCharList(test.str, test.tail)
			if !errors.Is(err, test.err) {
				t.Errorf("expected: %v, got: %v", test.err, err)
			}
			if l != test.term {
				t.Errorf("expected: %v, got: %v", test.term, l)
			}
		})
	}
}

func TestArena_PutCodeList(t *testing.T) {
	tests := []struct {
		title string
		arena *Arena
		str   string
		term  Cell
		err   error
	}{
		{
			title: "ok",
			arena: NewArena(10),
			str:   "abc",
			term:  Cell{tag: cellTagStructure, value: 0},
		},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			l, err := test.arena.PutCodeList(test.str)
			if !errors.Is(err, test.err) {
				t.Errorf("expected: %v, got: %v", test.err, err)
			}
			if l != test.term {
				t.Errorf("expected: %v, got: %v", test.term, l)
			}
		})
	}
}

func TestArena_PutPartialCodeList(t *testing.T) {
	tests := []struct {
		title string
		arena *Arena
		str   string
		tail  Cell
		term  Cell
		err   error
	}{
		{
			title: "ok",
			arena: NewArena(10),
			str:   "abc",
			tail:  Cell{tag: cellTagAtom, value: NewAtom("[]").value},
			term:  Cell{tag: cellTagStructure, value: 0},
		},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			l, err := test.arena.PutPartialCodeList(test.str, test.tail)
			if !errors.Is(err, test.err) {
				t.Errorf("expected: %v, got: %v", test.err, err)
			}
			if l != test.term {
				t.Errorf("expected: %v, got: %v", test.term, l)
			}
		})
	}
}

func TestArena_Functor(t *testing.T) {
	arena := NewArena(4)

	tests := []struct {
		title string
		term  Cell
		f     Functor
		ok    bool
	}{
		{
			title: "compound",
			term:  must(arena.PutCompound(NewAtom("foo"), must(arena.PutAtom(NewAtomRune('a'))))),
			f:     NewFunctor(NewAtom("foo"), 1),
			ok:    true,
		},
		{
			title: "string",
			term:  must(arena.PutCharList("foo")),
			f:     functorCons,
			ok:    true,
		},
		{
			title: "atomic",
			term:  must(arena.PutAtom(NewAtom("foo"))),
			ok:    false,
		},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			f, ok := arena.Functor(test.term)
			if ok != test.ok {
				t.Errorf("expected: %v, got: %v", test.ok, ok)
			}
			if f != test.f {
				t.Errorf("expected: %v, got: %v", test.f, f)
			}
		})
	}
}

func TestArena_Arg(t *testing.T) {
	arena := NewArena(20)
	tests := []struct {
		title string
		term  Cell
		n     int
		arg   Cell
	}{
		{
			title: "compound",
			term:  must(arena.PutCompound(NewAtom("foo"), must(arena.PutCompound(NewAtom("bar"), must(arena.PutAtom(NewAtomRune('a'))))))),
			n:     0,
			arg:   must(arena.PutCompound(NewAtom("bar"), must(arena.PutAtom(NewAtomRune('a'))))),
		},
		{
			title: "list",
			term:  must(arena.PutList(must(arena.PutAtom(NewAtomRune('a'))), must(arena.PutAtom(NewAtomRune('b'))))),
			n:     1,
			arg:   must(arena.PutList(must(arena.PutAtom(NewAtomRune('b'))))),
		},
		{
			title: "string car",
			term:  must(arena.PutCharList("foo")),
			n:     0,
			arg:   must(arena.PutAtom(NewAtomRune('f'))),
		},
		{
			title: "string cdr",
			term:  must(arena.PutCharList("foo")),
			n:     1,
			arg:   must(arena.PutCharList("oo")),
		},
		{
			title: "atomic",
			term:  must(arena.PutAtom(NewAtom("foo"))),
			n:     1,
			arg:   Cell{},
		},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			arg := arena.Arg(test.term, test.n)
			if arena.Compare(arg, test.arg) != 0 {
				t.Errorf("expected: %v, got: %v", test.arg, arg)
			}
		})
	}
}

func TestArena_Args(t *testing.T) {
	arena := NewArena(4)
	tests := []struct {
		title string
		term  Cell
		args  []Cell
		n     int
	}{
		{
			title: "compound",
			term:  must(arena.PutCompound(NewAtom("foo"), must(arena.PutAtom(NewAtomRune('a'))), must(arena.PutAtom(NewAtomRune('b'))))),
			args: []Cell{
				must(arena.PutAtom(NewAtomRune('a'))),
			},
			n: 1,
		},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			args := slices.Collect(take(arena.Args(test.term), test.n))
			if !reflect.DeepEqual(args, test.args) {
				t.Errorf("expected: %v, got: %v", test.args, args)
			}
		})
	}
}

func TestArena_List(t *testing.T) {
	arena := NewArena(30)
	x := must(arena.PutVariable())
	v := must(arena.PutVariable())
	cycle := must(arena.PutPartialList(v, must(arena.PutAtom(NewAtomRune('a')))))
	if err := arena.Bind(v, cycle); err != nil {
		t.Fatal(err)
	}
	type result struct {
		elem Cell
		ok   bool
	}
	tests := []struct {
		title   string
		term    Cell
		opts    []ListOption
		results []result
	}{
		{
			title: "list",
			term:  must(arena.PutList(must(arena.PutAtom(NewAtomRune('a'))), must(arena.PutAtom(NewAtomRune('b'))))),
			results: []result{
				{elem: must(arena.PutAtom(NewAtomRune('a'))), ok: true},
				{elem: must(arena.PutAtom(NewAtomRune('b'))), ok: true},
			},
		},
		{
			title: "list-ish (atom)",
			term:  must(arena.PutPartialList(must(arena.PutAtom(NewAtom("rest"))), must(arena.PutAtom(NewAtomRune('a'))), must(arena.PutAtom(NewAtomRune('b'))))),
			results: []result{
				{elem: must(arena.PutAtom(NewAtomRune('a'))), ok: true},
				{elem: must(arena.PutAtom(NewAtomRune('b'))), ok: true},
				{elem: must(arena.PutAtom(NewAtom("rest"))), ok: false},
			},
		},
		{
			title: "list-ish (non-atom)",
			term:  must(arena.PutPartialList(must(arena.PutInteger(0)), must(arena.PutAtom(NewAtomRune('a'))), must(arena.PutAtom(NewAtomRune('b'))))),
			results: []result{
				{elem: must(arena.PutAtom(NewAtomRune('a'))), ok: true},
				{elem: must(arena.PutAtom(NewAtomRune('b'))), ok: true},
				{elem: must(arena.PutInteger(0)), ok: false},
			},
		},
		{
			title: "partial list",
			term:  must(arena.PutPartialList(x, must(arena.PutAtom(NewAtomRune('a'))), must(arena.PutAtom(NewAtomRune('b'))))),
			results: []result{
				{elem: must(arena.PutAtom(NewAtomRune('a'))), ok: true},
				{elem: must(arena.PutAtom(NewAtomRune('b'))), ok: true},
				{elem: x, ok: false},
			},
		},
		{
			title: "partial list (allow partial)",
			term:  must(arena.PutPartialList(x, must(arena.PutAtom(NewAtomRune('a'))), must(arena.PutAtom(NewAtomRune('b'))))),
			opts: []ListOption{
				AllowPartial(true),
			},
			results: []result{
				{elem: must(arena.PutAtom(NewAtomRune('a'))), ok: true},
				{elem: must(arena.PutAtom(NewAtomRune('b'))), ok: true},
			},
		},
		{
			title: "cyclic list",
			term:  cycle,
			results: []result{
				{elem: must(arena.PutAtom(NewAtomRune('a'))), ok: true},
				{elem: cycle, ok: false},
			},
		},
		{
			title: "cyclic list (allow cycle)",
			term:  cycle,
			opts: []ListOption{
				AllowCycle(true),
			},
			results: []result{
				{elem: must(arena.PutAtom(NewAtomRune('a'))), ok: true},
				{elem: must(arena.PutAtom(NewAtomRune('a'))), ok: true},
				{elem: must(arena.PutAtom(NewAtomRune('a'))), ok: true},
				{elem: must(arena.PutAtom(NewAtomRune('a'))), ok: true},
				{elem: must(arena.PutAtom(NewAtomRune('a'))), ok: true},
				// ...
			},
		},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			var (
				i       int
				results []result
			)
			for elem, ok := range arena.List(test.term, test.opts...) {
				if i > 4 {
					break
				}
				results = append(results, result{
					elem: elem,
					ok:   ok,
				})
				i++
			}
			if !reflect.DeepEqual(results, test.results) {
				t.Errorf("expected: %v, got: %v", test.results, results)
			}
		})
	}
}

func TestArena_CharList(t *testing.T) {
	arena := NewArena(40)

	tests := []struct {
		title string
		term  Cell
		str   string
		ok    bool
	}{
		{
			title: "empty",
			term:  must(arena.PutList()),
			str:   "",
			ok:    true,
		},
		{
			title: "single-char list",
			term:  must(arena.PutList(must(arena.PutAtom(NewAtomRune('a'))), must(arena.PutAtom(NewAtomRune('b'))), must(arena.PutAtom(NewAtomRune('c'))))),
			str:   "abc",
			ok:    true,
		},
		{
			title: "atom list",
			term:  must(arena.PutList(must(arena.PutAtom(NewAtom("a!"))), must(arena.PutAtom(NewAtom("b!"))), must(arena.PutAtom(NewAtom("c!"))))),
			ok:    false,
		},
		{
			title: "non-atom list",
			term:  must(arena.PutList(must(arena.PutInteger(1)), must(arena.PutInteger(2)), must(arena.PutInteger(3)))),
			ok:    false,
		},
		{
			title: "partisl list",
			term:  must(arena.PutPartialList(must(arena.PutVariable()), must(arena.PutAtom(NewAtomRune('a'))), must(arena.PutAtom(NewAtomRune('b'))), must(arena.PutAtom(NewAtomRune('c'))))),
			ok:    false,
		},
		{
			title: "string",
			term:  must(arena.PutCharList("abc")),
			str:   "abc",
			ok:    true,
		},
		{
			title: "string-ish",
			term:  must(arena.PutPartialCharList("abc", must(arena.PutAtom(NewAtom("rest"))))),
			ok:    false,
		},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			str, ok := arena.CharList(test.term)
			if ok != test.ok {
				t.Errorf("expected ok=%v, got %v", test.ok, ok)
			}
			if str != test.str {
				t.Errorf("expected %v, got %v", test.str, str)
			}
		})
	}
}

func TestArena_Compare(t *testing.T) {
	arena := NewArena(40)

	w := must(arena.PutVariable())
	x := must(arena.PutVariable())
	y := must(arena.PutVariable())
	a := must(arena.PutAtom(NewAtom("a")))
	b := must(arena.PutAtom(NewAtom("b")))
	z := must(arena.PutAtom(NewAtom("Z")))
	i0 := must(arena.PutInteger(0))
	i1 := must(arena.PutInteger(1))
	i2 := must(arena.PutInteger(2))
	f0 := must(arena.PutFloat(0))
	f1 := must(arena.PutFloat(1))
	f2 := must(arena.PutFloat(2))
	fa := must(arena.PutCompound(NewAtom("f"), a))
	fb := must(arena.PutCompound(NewAtom("f"), b))
	fz := must(arena.PutCompound(NewAtom("f"), z))
	ea := must(arena.PutCompound(NewAtom("e"), a))
	ga := must(arena.PutCompound(NewAtom("g"), a))
	fab := must(arena.PutCompound(NewAtom("f"), a, b))

	tests := []struct {
		title    string
		lhs, rhs Cell
		o        int
		err      error
	}{
		{title: `X > W`, lhs: x, rhs: w, o: 1},
		{title: `X = X`, lhs: x, rhs: x, o: 0},
		{title: `X < Y`, lhs: x, rhs: y, o: -1},
		{title: `X < 0.0`, lhs: x, rhs: f0, o: -1},
		{title: `X < 0`, lhs: x, rhs: i0, o: -1},
		{title: `X < a`, lhs: x, rhs: a, o: -1},
		{title: `X < f(a)`, lhs: x, rhs: fa, o: -1},

		{title: `a > X`, lhs: a, rhs: x, o: 1},
		{title: `a > 0.0`, lhs: a, rhs: f0, o: 1},
		{title: `a > 0`, lhs: a, rhs: i0, o: 1},
		{title: `a > 'Z'`, lhs: a, rhs: z, o: 1},
		{title: `a = a`, lhs: a, rhs: a, o: 0},
		{title: `a < b`, lhs: a, rhs: b, o: -1},
		{title: `a < f(a)`, lhs: a, rhs: fa, o: -1},

		{title: `1.0 > X`, lhs: f1, rhs: x, o: 1},
		{title: `1.0 > 0.0`, lhs: f1, rhs: f0, o: 1},
		{title: `1.0 = 1.0`, lhs: f1, rhs: f1, o: 0},
		{title: `1.0 < 2.0`, lhs: f1, rhs: f2, o: -1},
		{title: `1.0 < 1`, lhs: f1, rhs: i1, o: -1},
		{title: `1.0 < a`, lhs: f1, rhs: a, o: -1},
		{title: `1.0 < f(a)`, lhs: f1, rhs: fa, o: -1},

		{title: `1 > X`, lhs: i1, rhs: x, o: 1},
		{title: `1 > 1.0`, lhs: i1, rhs: f1, o: 1},
		{title: `1 > 0`, lhs: i1, rhs: i0, o: 1},
		{title: `1 = 1`, lhs: i1, rhs: i1, o: 0},
		{title: `1 < 2`, lhs: i1, rhs: i2, o: -1},
		{title: `1 < a`, lhs: i1, rhs: a, o: -1},
		{title: `1 < f(a)`, lhs: i1, rhs: fa, o: -1},

		{title: `f(a) > X`, lhs: fa, rhs: x, o: 1},
		{title: `f(a) > 0.0`, lhs: fa, rhs: f0, o: 1},
		{title: `f(a) > 0`, lhs: fa, rhs: i0, o: 1},
		{title: `f(a) > a`, lhs: fa, rhs: a, o: 1},
		{title: `f(a) > f('Z')`, lhs: fa, rhs: fz, o: 1},
		{title: `f(a) > e(a)`, lhs: fa, rhs: ea, o: 1},
		{title: `f(a, b) > f(a)`, lhs: fab, rhs: fa, o: 1},
		{title: `f(a) = f(a)`, lhs: fa, rhs: fa, o: 0},
		{title: `f(a) < g(a)`, lhs: fa, rhs: ga, o: -1},
		{title: `f(a) < f(a,b)`, lhs: fa, rhs: fab, o: -1},
		{title: `f(a) < f(b)`, lhs: fa, rhs: fb, o: -1},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			o := arena.Compare(test.lhs, test.rhs)
			if o != test.o {
				t.Errorf("expected %d, got %d", test.o, o)
			}
		})
	}
}

func TestArena_Acyclic(t *testing.T) {
	arena := NewArena(40)

	a, _ := arena.PutAtom(NewAtomRune('a'))
	fa, _ := arena.PutCompound(NewAtomRune('f'), a)
	x, _ := arena.PutVariable()
	fx, _ := arena.PutCompound(NewAtomRune('f'), x)
	_ = arena.Bind(x, fx)

	tests := []struct {
		title  string
		term   Cell
		result bool
	}{
		{title: "atom", term: a, result: true},
		{title: "compound", term: fa, result: true},
		{title: "cyclic", term: fx, result: false},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			if ok := arena.Acyclic(test.term); ok != test.result {
				t.Errorf("expected %v, got %v", test.result, ok)
			}
		})
	}
}

func TestArena_RenamedCopy(t *testing.T) {
	arena := NewArena(40)

	a := must(arena.PutAtom(NewAtomRune('a')))
	b := must(arena.PutAtom(NewAtomRune('b')))
	c := must(arena.PutAtom(NewAtomRune('c')))

	three := must(arena.PutInteger(3))
	threePointThree := must(arena.PutFloat(3.3))

	fa := must(arena.PutCompound(NewAtomRune('f'), a))

	abc := must(arena.PutList(a, b, c))

	tests := []struct {
		title  string
		term   Cell
		result Cell
		err    error
	}{
		{title: "atom", term: a, result: a},
		{title: "integer", term: three, result: three},
		{title: "float", term: threePointThree, result: threePointThree},
		{title: "compound", term: fa, result: fa},
		{title: "list", term: abc, result: abc},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			result, err := RenamedCopy(arena, arena, test.term)
			if !errors.Is(err, test.err) {
				t.Errorf("expected %v, got %v", test.err, err)
			}
			if arena.Compare(result, test.result) != 0 {
				t.Errorf("expected %v, got %v", test.result, result)
			}
		})
	}
}

func must[T any](val T, err error) T {
	if err != nil {
		panic(err)
	}
	return val
}

func take[T any](s iter.Seq[T], n int) iter.Seq[T] {
	return func(yield func(T) bool) {
		i := 0
		for e := range s {
			if i >= n {
				return
			}
			if !yield(e) {
				return
			}
			i++
		}
	}
}

func TestArena_GC(t *testing.T) {
	t.Run("unreachable cells are reclaimed", func(t *testing.T) {
		a := NewArena(8)
		_ = must(a.PutVariable()) // Garbage.
		v := must(a.PutVariable())

		a.GC([]*Cell{&v})

		if len(a.Heap) != 1 {
			t.Errorf("expected: %d, got: %d", 1, len(a.Heap))
		}
		if _, ok := a.Variable(a.Deref(v)); !ok {
			t.Errorf("the survivor is no longer an unbound variable: %v", a.Deref(v))
		}
	})

	t.Run("a compound survives with its arguments", func(t *testing.T) {
		a := NewArena(16)
		_ = must(a.PutVariable()) // Garbage.
		c := must(a.PutCompound(NewAtom("foo"), must(a.PutInteger(1)), must(a.PutAtom(NewAtom("bar")))))

		a.GC([]*Cell{&c})

		if len(a.Heap) != 3 {
			t.Errorf("expected: %d, got: %d", 3, len(a.Heap))
		}
		if f, ok := a.Functor(c); !ok || f != NewFunctor(NewAtom("foo"), 2) {
			t.Errorf("expected: %v, got: %v", NewFunctor(NewAtom("foo"), 2), f)
		}
		if i, ok := a.Integer(a.Arg(c, 0)); !ok || i != 1 {
			t.Errorf("expected: %d, got: %d", 1, i)
		}
		if atom, ok := a.Atom(a.Arg(c, 1)); !ok || atom != NewAtom("bar") {
			t.Errorf("expected: %v, got: %v", NewAtom("bar"), atom)
		}
	})

	t.Run("a CDR-coded list survives", func(t *testing.T) {
		a := NewArena(32)
		_ = must(a.PutVariable()) // Garbage.
		l := must(a.PutList(must(a.PutInteger(1)), must(a.PutInteger(2)), must(a.PutInteger(3))))

		a.GC([]*Cell{&l})

		var elems []int64
		for elem, ok := range a.List(l) {
			if !ok {
				t.Fatalf("not a proper list: %v", elem)
			}
			i, _ := a.Integer(elem)
			elems = append(elems, i)
		}
		if want := []int64{1, 2, 3}; !slices.Equal(elems, want) {
			t.Errorf("expected: %v, got: %v", want, elems)
		}
		if len(a.Heap) != 7 {
			t.Errorf("expected: %d, got: %d", 7, len(a.Heap))
		}
	})

	t.Run("an int64 survives", func(t *testing.T) {
		a := NewArena(16)
		_ = must(a.PutVariable()) // Garbage.
		i := must(a.PutInteger(math.MaxInt32 + 1))

		a.GC([]*Cell{&i})

		if n, ok := a.Integer(i); !ok || n != math.MaxInt32+1 {
			t.Errorf("expected: %d, got: %d", int64(math.MaxInt32)+1, n)
		}
	})

	t.Run("an int64 whose bits look like a cell isn't relocated", func(t *testing.T) {
		a := NewArena(16)
		n := cast[Cell, int64](Cell{tag: cellTagReference, value: 5})
		i := must(a.PutInteger(n))

		a.GC([]*Cell{&i})

		if got, ok := a.Integer(i); !ok || got != n {
			t.Errorf("expected: %d, got: %d", n, got)
		}
	})

	t.Run("a float survives", func(t *testing.T) {
		a := NewArena(16)
		_ = must(a.PutVariable()) // Garbage.
		f := must(a.PutFloat(1.5))

		a.GC([]*Cell{&f})

		if got, ok := a.Float(f); !ok || got != 1.5 {
			t.Errorf("expected: %f, got: %f", 1.5, got)
		}
	})

	t.Run("an unreachable string is collected", func(t *testing.T) {
		a := NewArena(16)
		garbage := must(a.PutCharList("garbage"))
		s := must(a.PutCharList("hello"))

		a.GC([]*Cell{&s})

		if got, ok := a.CharList(s); !ok || got != "hello" {
			t.Errorf("expected: %s, got: %s", "hello", got)
		}
		if got := a.Strings.Get(int(garbage.value)); got.Body != "" {
			t.Errorf("the unreachable string survived: %v", got)
		}
	})

	t.Run("the tail of a string is relocated", func(t *testing.T) {
		a := NewArena(16)
		_ = must(a.PutVariable()) // Garbage.
		v := must(a.PutVariable())
		s := must(a.PutPartialCharList("hello", v))

		a.GC([]*Cell{&s})

		if len(a.Heap) != 1 {
			t.Errorf("expected: %d, got: %d", 1, len(a.Heap))
		}
		tail := a.Strings.Get(int(s.value)).Tail
		if want := (Cell{tag: cellTagReference, value: 0}); tail != want {
			t.Errorf("expected: %v, got: %v", want, tail)
		}
	})

	t.Run("an unreachable stream is collected", func(t *testing.T) {
		a := NewArena(16)
		garbage := must(a.PutStream(Stream{Alias: NewAtom("garbage")}))
		s := must(a.PutStream(Stream{Alias: NewAtom("hello")}))

		a.GC([]*Cell{&s})

		if got, ok := a.Stream(s); !ok || got.Alias != NewAtom("hello") {
			t.Errorf("expected: %v, got: %v", NewAtom("hello"), got.Alias)
		}
		if got := a.Streams.Get(int(garbage.value)); got != nil {
			t.Errorf("the unreachable stream survived: %v", got)
		}
	})

	t.Run("a bound variable keeps its binding", func(t *testing.T) {
		a := NewArena(16)
		_ = must(a.PutVariable()) // Garbage.
		v := must(a.PutVariable())
		c := must(a.PutCompound(NewAtom("foo"), must(a.PutAtom(NewAtom("bar")))))
		if err := a.Bind(v, c); err != nil {
			t.Fatal(err)
		}

		a.GC([]*Cell{&v})

		d := a.Deref(v)
		if f, ok := a.Functor(d); !ok || f != NewFunctor(NewAtom("foo"), 1) {
			t.Errorf("expected: %v, got: %v", NewFunctor(NewAtom("foo"), 1), f)
		}
		if atom, ok := a.Atom(a.Arg(d, 0)); !ok || atom != NewAtom("bar") {
			t.Errorf("expected: %v, got: %v", NewAtom("bar"), atom)
		}
	})

	t.Run("a cyclic term survives", func(t *testing.T) {
		a := NewArena(16)
		v := must(a.PutVariable())
		c := must(a.PutCompound(NewAtom("f"), v))
		if err := a.Bind(v, c); err != nil {
			t.Fatal(err)
		}

		a.GC([]*Cell{&v})

		if a.Acyclic(v) {
			t.Error("the cycle is gone")
		}
		if a.Deref(a.Arg(a.Deref(v), 0)) != a.Deref(v) {
			t.Errorf("expected: %v, got: %v", a.Deref(v), a.Deref(a.Arg(a.Deref(v), 0)))
		}
	})

	t.Run("no roots empties the heap", func(t *testing.T) {
		a := NewArena(16)
		_ = must(a.PutCompound(NewAtom("foo"), must(a.PutVariable())))

		a.GC(nil)

		if len(a.Heap) != 0 {
			t.Errorf("expected: %d, got: %d", 0, len(a.Heap))
		}
	})

	t.Run("collecting an already compact heap changes nothing", func(t *testing.T) {
		a := NewArena(16)
		c := must(a.PutCompound(NewAtom("foo"), must(a.PutInteger(math.MaxInt32+1))))

		a.GC([]*Cell{&c})
		before, heap := c, slices.Clone(a.Heap)
		a.GC([]*Cell{&c})

		if c != before {
			t.Errorf("expected: %v, got: %v", before, c)
		}
		if !slices.Equal(heap, a.Heap) {
			t.Errorf("expected: %v, got: %v", heap, a.Heap)
		}
	})
}
