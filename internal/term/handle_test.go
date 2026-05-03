package term

import (
	"errors"
	"iter"
	"math"
	"reflect"
	"slices"
	"testing"
	"unsafe"
)

func TestCell_size(t *testing.T) {
	if unsafe.Sizeof(cell{}) != 8 {
		t.Fatal("cell must be 8 bytes")
	}
}

func TestHandle_Variable(t *testing.T) {
	heap := make(Heap, 0, 1)

	must := func(term Handle, err error) Handle {
		if err != nil {
			t.Fatal(err)
		}
		return term
	}

	x := must(heap.PutVariable())
	i := must(heap.PutInteger(1))

	tests := []struct {
		title string
		term  Handle
		addr  int
		ok    bool
	}{
		{
			title: "variable",
			term:  x,
			addr:  0,
			ok:    true,
		},
		{
			title: "not variable",
			term:  i,
			ok:    false,
		},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			addr, ok := test.term.Variable()
			if ok != test.ok {
				t.Errorf("expected: %v, got: %v", test.ok, ok)
			}
			if addr != test.addr {
				t.Errorf("expected: %v, got: %v", test.addr, addr)
			}
		})
	}
}

func TestHandle_Deref(t *testing.T) {
	heap := Heap{
		pack(cell{tag: cellTagReference, value: 0}),
		pack(cell{tag: cellTagCharacter, value: 'a'}),
		pack(cell{tag: cellTagReference, value: 1}),
	}

	tests := []struct {
		title  string
		term   Handle
		result Handle
	}{
		{
			title: "free variable",
			term: Handle{
				heap: &heap,
				cell: cell{tag: cellTagReference, value: 0},
			},
			result: Handle{
				heap: &heap,
				cell: cell{tag: cellTagReference, value: 0},
			},
		},
		{
			title: "bound variable",
			term: Handle{
				heap: &heap,
				cell: cell{tag: cellTagReference, value: 2},
			},
			result: Handle{
				cell: cell{tag: cellTagCharacter, value: 'a'},
			},
		},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			ret := test.term.Deref()
			if ret != test.result {
				t.Errorf("expected: %v, got: %v", test.result, ret)
			}
		})
	}
}

func TestHandle_Bind(t *testing.T) {
	heap := make(Heap, 0, 2)
	another := make(Heap, 0, 2)

	must := func(term Handle, err error) Handle {
		if err != nil {
			t.Fatal(err)
		}
		return term
	}

	x := must(heap.PutVariable())
	y := must(heap.PutVariable())
	i := must(heap.PutInteger(1))
	foo := must(another.PutCompound(NewAtom("foo"), must(another.PutAtom(NewAtomRune('a')))))

	tests := []struct {
		title string
		x, y  Handle
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
		{
			title: "different heap",
			x:     y,
			y:     foo,
			err:   ErrIncompatibleHandle,
		},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			err := test.x.Bind(test.y)
			if !errors.Is(err, test.err) {
				t.Fatalf("expected: %v, got: %v", test.err, err)
			}
		})
	}
}

func TestHandle_Atom(t *testing.T) {
	heap := make(Heap, 0, 1)

	must := func(term Handle, err error) Handle {
		if err != nil {
			t.Fatal(err)
		}
		return term
	}

	tests := []struct {
		title string
		term  Handle
		atom  Atom
		ok    bool
	}{
		{
			title: "character",
			term:  must(heap.PutAtom(NewAtomRune('a'))),
			atom:  NewAtomRune('a'),
			ok:    true,
		},
		{
			title: "atom",
			term:  must(heap.PutAtom(NewAtom("foo"))),
			atom:  NewAtom("foo"),
			ok:    true,
		},
		{
			title: "not atom nor character",
			term:  must(heap.PutInteger(1)),
			atom:  Atom{kind: atomKindInvalid},
			ok:    false,
		},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			a, ok := test.term.Atom()
			if ok != test.ok {
				t.Errorf("expected: %v, got: %v", test.ok, ok)
			}
			if a != test.atom {
				t.Errorf("expected: %v, got: %v", test.atom, a)
			}
		})
	}
}

func TestHandle_Integer(t *testing.T) {
	heap := make(Heap, 0, 1)
	must := func(term Handle, err error) Handle {
		if err != nil {
			t.Fatal(err)
		}
		return term
	}

	tests := []struct {
		title string
		term  Handle
		n     int64
		ok    bool
	}{
		{
			title: "int64",
			term:  must(heap.PutInteger(math.MaxInt32 + 1)),
			n:     math.MaxInt32 + 1,
			ok:    true,
		},
		{
			title: "int32",
			term:  must(heap.PutInteger(math.MaxInt32)),
			n:     math.MaxInt32,
			ok:    true,
		},
		{
			title: "not integer",
			term:  must(heap.PutAtom(NewAtomRune('a'))),
			ok:    false,
		},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			i, ok := test.term.Integer()
			if ok != test.ok {
				t.Errorf("expected: %v, got: %v", test.ok, ok)
			}
			if i != test.n {
				t.Errorf("expected: %v, got: %v", test.n, i)
			}
		})
	}
}

func TestHandle_Float(t *testing.T) {
	heap := make(Heap, 0, 1)
	must := func(term Handle, err error) Handle {
		if err != nil {
			t.Fatal(err)
		}
		return term
	}

	tests := []struct {
		title string
		term  Handle
		f     float64
		ok    bool
	}{
		{
			title: "float64",
			term:  must(heap.PutFloat(1)),
			f:     1,
			ok:    true,
		},
		{
			title: "not float64",
			term:  must(heap.PutAtom(NewAtomRune('a'))),
			ok:    false,
		},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			f, ok := test.term.Float()
			if ok != test.ok {
				t.Errorf("expected: %v, got: %v", test.ok, ok)
			}
			if f != test.f {
				t.Errorf("expected: %v, got: %v", test.f, f)
			}
		})
	}
}

func TestHandle_Functor(t *testing.T) {
	heap := make(Heap, 0, 4)
	must := func(term Handle, err error) Handle {
		if err != nil {
			t.Fatal(err)
		}
		return term
	}
	tests := []struct {
		title string
		term  Handle
		f     Functor
		ok    bool
	}{
		{
			title: "compound",
			term:  must(heap.PutCompound(NewAtom("foo"), must(heap.PutAtom(NewAtomRune('a'))))),
			f:     NewFunctor(NewAtom("foo"), 1),
			ok:    true,
		},
		{
			title: "string",
			term:  must(heap.PutCharList("foo")),
			f:     functorCons,
			ok:    true,
		},
		{
			title: "atomic",
			term:  must(heap.PutAtom(NewAtom("foo"))),
			ok:    false,
		},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			f, ok := test.term.Functor()
			if ok != test.ok {
				t.Errorf("expected: %v, got: %v", test.ok, ok)
			}
			if f != test.f {
				t.Errorf("expected: %v, got: %v", test.f, f)
			}
		})
	}
}

func TestHandle_Arg(t *testing.T) {
	heap := make(Heap, 0, 20)
	must := func(term Handle, err error) Handle {
		if err != nil {
			t.Fatal(err)
		}
		return term
	}
	tests := []struct {
		title string
		term  Handle
		n     int
		arg   Handle
	}{
		{
			title: "compound",
			term:  must(heap.PutCompound(NewAtom("foo"), must(heap.PutCompound(NewAtom("bar"), must(heap.PutAtom(NewAtomRune('a'))))))),
			n:     0,
			arg:   must(heap.PutCompound(NewAtom("bar"), must(heap.PutAtom(NewAtomRune('a'))))),
		},
		{
			title: "list",
			term:  must(heap.PutList(must(heap.PutAtom(NewAtomRune('a'))), must(heap.PutAtom(NewAtomRune('b'))))),
			n:     1,
			arg:   must(heap.PutList(must(heap.PutAtom(NewAtomRune('b'))))),
		},
		{
			title: "string car",
			term:  must(heap.PutCharList("foo")),
			n:     0,
			arg:   must(heap.PutAtom(NewAtomRune('f'))),
		},
		{
			title: "string cdr",
			term:  must(heap.PutCharList("foo")),
			n:     1,
			arg:   must(heap.PutCharList("oo")),
		},
		{
			title: "atomic",
			term:  must(heap.PutAtom(NewAtom("foo"))),
			n:     1,
			arg:   Handle{},
		},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			arg := test.term.Arg(test.n)
			if Compare(arg, test.arg) != 0 {
				t.Errorf("expected: %v, got: %v", test.arg, arg)
			}
		})
	}
}

func TestHandle_Args(t *testing.T) {
	heap := make(Heap, 0, 4)
	must := func(term Handle, err error) Handle {
		if err != nil {
			t.Fatal(err)
		}
		return term
	}
	tests := []struct {
		title string
		term  Handle
		args  []Handle
		n     int
	}{
		{
			title: "compound",
			term:  must(heap.PutCompound(NewAtom("foo"), must(heap.PutAtom(NewAtomRune('a'))), must(heap.PutAtom(NewAtomRune('b'))))),
			args: []Handle{
				must(heap.PutAtom(NewAtomRune('a'))),
			},
			n: 1,
		},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			args := slices.Collect(take(test.term.Args(), test.n))
			if !reflect.DeepEqual(args, test.args) {
				t.Errorf("expected: %v, got: %v", test.args, args)
			}
		})
	}
}

func TestHandle_List(t *testing.T) {
	heap := make(Heap, 0, 30)
	must := func(term Handle, err error) Handle {
		if err != nil {
			t.Fatal(err)
		}
		return term
	}
	x := must(heap.PutVariable())
	v := must(heap.PutVariable())
	cycle := must(heap.PutPartialList(v, must(heap.PutAtom(NewAtomRune('a')))))
	if err := v.Bind(cycle); err != nil {
		t.Fatal(err)
	}
	type result struct {
		elem Handle
		ok   bool
	}
	tests := []struct {
		title   string
		term    Handle
		opts    []ListOption
		results []result
	}{
		{
			title: "list",
			term:  must(heap.PutList(must(heap.PutAtom(NewAtomRune('a'))), must(heap.PutAtom(NewAtomRune('b'))))),
			results: []result{
				{elem: must(heap.PutAtom(NewAtomRune('a'))), ok: true},
				{elem: must(heap.PutAtom(NewAtomRune('b'))), ok: true},
			},
		},
		{
			title: "list-ish (atom)",
			term:  must(heap.PutPartialList(must(heap.PutAtom(NewAtom("rest"))), must(heap.PutAtom(NewAtomRune('a'))), must(heap.PutAtom(NewAtomRune('b'))))),
			results: []result{
				{elem: must(heap.PutAtom(NewAtomRune('a'))), ok: true},
				{elem: must(heap.PutAtom(NewAtomRune('b'))), ok: true},
				{elem: must(heap.PutAtom(NewAtom("rest"))), ok: false},
			},
		},
		{
			title: "list-ish (non-atom)",
			term:  must(heap.PutPartialList(must(heap.PutInteger(0)), must(heap.PutAtom(NewAtomRune('a'))), must(heap.PutAtom(NewAtomRune('b'))))),
			results: []result{
				{elem: must(heap.PutAtom(NewAtomRune('a'))), ok: true},
				{elem: must(heap.PutAtom(NewAtomRune('b'))), ok: true},
				{elem: must(heap.PutInteger(0)), ok: false},
			},
		},
		{
			title: "partial list",
			term:  must(heap.PutPartialList(x, must(heap.PutAtom(NewAtomRune('a'))), must(heap.PutAtom(NewAtomRune('b'))))),
			results: []result{
				{elem: must(heap.PutAtom(NewAtomRune('a'))), ok: true},
				{elem: must(heap.PutAtom(NewAtomRune('b'))), ok: true},
				{elem: x, ok: false},
			},
		},
		{
			title: "partial list (allow partial)",
			term:  must(heap.PutPartialList(x, must(heap.PutAtom(NewAtomRune('a'))), must(heap.PutAtom(NewAtomRune('b'))))),
			opts: []ListOption{
				AllowPartial(true),
			},
			results: []result{
				{elem: must(heap.PutAtom(NewAtomRune('a'))), ok: true},
				{elem: must(heap.PutAtom(NewAtomRune('b'))), ok: true},
			},
		},
		{
			title: "cyclic list",
			term:  cycle,
			results: []result{
				{elem: must(heap.PutAtom(NewAtomRune('a'))), ok: true},
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
				{elem: must(heap.PutAtom(NewAtomRune('a'))), ok: true},
				{elem: must(heap.PutAtom(NewAtomRune('a'))), ok: true},
				{elem: must(heap.PutAtom(NewAtomRune('a'))), ok: true},
				{elem: must(heap.PutAtom(NewAtomRune('a'))), ok: true},
				{elem: must(heap.PutAtom(NewAtomRune('a'))), ok: true},
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
			for elem, ok := range test.term.List(test.opts...) {
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

func TestHandle_CharList(t *testing.T) {
	heap := make(Heap, 0, 40)
	must := func(term Handle, err error) Handle {
		if err != nil {
			t.Fatal(err)
		}
		return term
	}
	tests := []struct {
		title string
		term  Handle
		str   string
		ok    bool
	}{
		{
			title: "empty",
			term:  must(heap.PutList()),
			str:   "",
			ok:    true,
		},
		{
			title: "single-char list",
			term:  must(heap.PutList(must(heap.PutAtom(NewAtomRune('a'))), must(heap.PutAtom(NewAtomRune('b'))), must(heap.PutAtom(NewAtomRune('c'))))),
			str:   "abc",
			ok:    true,
		},
		{
			title: "atom list",
			term:  must(heap.PutList(must(heap.PutAtom(NewAtom("a!"))), must(heap.PutAtom(NewAtom("b!"))), must(heap.PutAtom(NewAtom("c!"))))),
			ok:    false,
		},
		{
			title: "non-atom list",
			term:  must(heap.PutList(must(heap.PutInteger(1)), must(heap.PutInteger(2)), must(heap.PutInteger(3)))),
			ok:    false,
		},
		{
			title: "partisl list",
			term:  must(heap.PutPartialList(must(heap.PutVariable()), must(heap.PutAtom(NewAtomRune('a'))), must(heap.PutAtom(NewAtomRune('b'))), must(heap.PutAtom(NewAtomRune('c'))))),
			ok:    false,
		},
		{
			title: "string",
			term:  must(heap.PutCharList("abc")),
			str:   "abc",
			ok:    true,
		},
		{
			title: "string-ish",
			term:  must(heap.PutPartialCharList("abc", must(heap.PutAtom(NewAtom("rest"))))),
			ok:    false,
		},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			str, ok := test.term.CharList()
			if ok != test.ok {
				t.Errorf("expected ok=%v, got %v", test.ok, ok)
			}
			if str != test.str {
				t.Errorf("expected %v, got %v", test.str, str)
			}
		})
	}
}

func TestCompare(t *testing.T) {
	h := make(Heap, 0, 40)
	must := func(term Handle, err error) Handle {
		if err != nil {
			t.Fatal(err)
		}
		return term
	}

	w := must(h.PutVariable())
	x := must(h.PutVariable())
	y := must(h.PutVariable())
	a := must(h.PutAtom(NewAtom("a")))
	b := must(h.PutAtom(NewAtom("b")))
	z := must(h.PutAtom(NewAtom("Z")))
	i0 := must(h.PutInteger(0))
	i1 := must(h.PutInteger(1))
	i2 := must(h.PutInteger(2))
	f0 := must(h.PutFloat(0))
	f1 := must(h.PutFloat(1))
	f2 := must(h.PutFloat(2))
	fa := must(h.PutCompound(NewAtom("f"), a))
	fb := must(h.PutCompound(NewAtom("f"), b))
	fz := must(h.PutCompound(NewAtom("f"), z))
	ea := must(h.PutCompound(NewAtom("e"), a))
	ga := must(h.PutCompound(NewAtom("g"), a))
	fab := must(h.PutCompound(NewAtom("f"), a, b))

	tests := []struct {
		title    string
		lhs, rhs Handle
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
			o := Compare(test.lhs, test.rhs)
			if o != test.o {
				t.Errorf("expected %d, got %d", test.o, o)
			}
		})
	}
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
