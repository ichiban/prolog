package prolog

import (
	"iter"
	"maps"
	"math"
	"reflect"
	"slices"
	"testing"

	"github.com/ichiban/prolog/v2/internal/rbtree"
)

func TestTermTag_String(t *testing.T) {
	tests := []struct {
		tag termTag
		str string
	}{
		{str: "unknown"},
		{tag: termTagVariable, str: "variable"},
		{tag: termTagAtom, str: "atom"},
		{tag: termTagCharacter, str: "character"},
		{tag: termTagInteger, str: "integer"},
		{tag: termTagFloat, str: "float"},
		{tag: termTagReference, str: "reference"},
		{tag: termTagCompound, str: "compound"},
		{tag: termTagString, str: "string"},
	}

	for _, tt := range tests {
		t.Run(tt.str, func(t *testing.T) {
			if got := tt.tag.String(); got != tt.str {
				t.Errorf("got: %v, want: %v", got, tt.str)
			}
		})
	}
}

func TestFunctor_String(t *testing.T) {
	tests := []struct {
		functor Functor
		string  string
	}{
		{functor: Functor{Name: "foo", Arity: 2}, string: "foo/2"},
	}

	for _, tt := range tests {
		t.Run(tt.string, func(t *testing.T) {
			if got := tt.functor.String(); got != tt.string {
				t.Errorf("expected: %s, got: %s", tt.string, got)
			}
		})
	}
}

func TestNewHeap(t *testing.T) {
	h := NewHeap(1024)
	if h == nil {
		t.Errorf("NewHeap() returned nil")
	}
}

func TestNewVariable(t *testing.T) {
	tests := []struct {
		title string
		heap  *Heap
		term  Term
		err   error
	}{
		{
			title: "ok",
			heap:  &Heap{},
			term:  Term{tag: termTagVariable, payload: 1},
		},
		{
			title: "ng",
			heap:  &Heap{env: env{lastVariable: math.MaxInt32}},
			err:   &ResourceError{Resource: "variables"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			v, err := NewVariable(tt.heap)
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected: %v, got: %v", tt.err, err)
			}

			if v != tt.term {
				t.Errorf("expected: %v, got: %v", tt.term, v)
			}
		})
	}
}

func TestNewAtom(t *testing.T) {
	tests := []struct {
		title string
		heap  *Heap
		name  string
		term  Term
		err   error
	}{
		{
			title: "single char",
			name:  "a",
			term:  Term{tag: termTagCharacter, payload: 'a'},
		},
		{
			title: "multiple chars",
			heap: &Heap{
				atoms: atomTable{
					IDs: rbtree.Map[string, atomID]{
						Nodes: make([]rbtree.Node[string, atomID], 0, 4),
					},
					Names: make([]string, 0, 1),
				},
			},
			name: "foo",
			term: Term{tag: termTagAtom, payload: 0},
		},
		{
			title: "ng",
			heap:  &Heap{},
			err:   &ResourceError{Resource: "atoms"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			a, err := NewAtom(tt.heap, tt.name)
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected: %v, got: %v", tt.err, err)
			}

			if a != tt.term {
				t.Errorf("expected: %v, got: %v", tt.term, a)
			}
		})
	}
}

func TestNewInteger(t *testing.T) {
	tests := []struct {
		title   string
		heap    *Heap
		integer int64
		term    Term
		err     error
	}{
		{
			title:   "ok",
			heap:    &Heap{integers: make([]int64, 0, 1)},
			integer: 1,
			term:    Term{tag: termTagInteger, payload: 0},
		},
		{
			title: "ng",
			heap:  &Heap{},
			err:   &ResourceError{Resource: "integers"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			i, err := NewInteger(tt.heap, tt.integer)
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected: %v, got: %v", tt.err, err)
			}

			if i != tt.term {
				t.Errorf("expected: %v, got: %v", tt.term, i)
			}
		})
	}
}

func TestNewFloat(t *testing.T) {
	tests := []struct {
		title string
		heap  *Heap
		float float64
		term  Term
		err   error
	}{
		{
			title: "ok",
			heap:  &Heap{floats: make([]float64, 0, 1)},
			float: 1,
			term:  Term{tag: termTagFloat, payload: 0},
		},
		{
			title: "ng",
			heap:  &Heap{},
			err:   &ResourceError{Resource: "floats"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			f, err := NewFloat(tt.heap, tt.float)
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected: %v, got: %v", tt.err, err)
			}

			if f != tt.term {
				t.Errorf("expected: %v, got: %v", tt.term, f)
			}
		})
	}
}

func TestNewCompound(t *testing.T) {
	tests := []struct {
		title string
		heap  *Heap
		name  string
		args  []Term
		term  Term
		err   error
	}{
		{
			title: "atom",
			name:  "f",
			term:  Term{tag: termTagCharacter, payload: 'f'},
		},
		{
			title: "ok",
			heap: &Heap{
				terms:    make([]Term, 0, 5),
				integers: make([]int64, 0, 1),
			},
			name: "f",
			args: []Term{
				{tag: termTagCharacter, payload: 'a'},
				{tag: termTagCharacter, payload: 'b'},
			},
			term: Term{tag: termTagReference, payload: 0},
		},
		{
			title: "insufficient atoms",
			heap: &Heap{
				terms:    make([]Term, 0, 5),
				integers: make([]int64, 0, 1),
			},
			name: "foo",
			args: []Term{
				{tag: termTagCharacter, payload: 'a'},
				{tag: termTagCharacter, payload: 'b'},
			},
			err: &ResourceError{Resource: "atoms"},
		},
		{
			title: "insufficient integers",
			heap: &Heap{
				terms:    make([]Term, 0, 5),
				integers: make([]int64, 0),
			},
			name: "f",
			args: []Term{
				{tag: termTagCharacter, payload: 'a'},
				{tag: termTagCharacter, payload: 'b'},
			},
			err: &ResourceError{Resource: "integers"},
		},
		{
			title: "insufficient terms",
			heap: &Heap{
				terms:    make([]Term, 0, 4),
				integers: make([]int64, 0, 1),
			},
			name: "f",
			args: []Term{
				{tag: termTagCharacter, payload: 'a'},
				{tag: termTagCharacter, payload: 'b'},
			},
			err: &ResourceError{Resource: "terms"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			c, err := NewCompound(tt.heap, tt.name, tt.args...)
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected: %v, got: %v", tt.err, err)
			}

			if c != tt.term {
				t.Errorf("expected: %v, got: %v", tt.term, c)
			}
		})
	}
}

func TestNewList(t *testing.T) {
	tests := []struct {
		title string
		heap  *Heap
		elems iter.Seq[Term]
		term  Term
		err   error
	}{
		{
			title: "empty",
			heap: &Heap{
				atoms: atomTable{
					IDs: rbtree.Map[string, atomID]{
						Nodes: make([]rbtree.Node[string, atomID], 0, 4),
					},
					Names: make([]string, 0, 1),
				},
			},
			elems: slices.Values([]Term{}),
			term:  Term{tag: termTagAtom, payload: 0},
		},
		{
			title: "ok",
			heap: &Heap{
				terms: make([]Term, 0, 9),
				atoms: atomTable{
					IDs: rbtree.Map[string, atomID]{
						Nodes: make([]rbtree.Node[string, atomID], 0, 4),
					},
					Names: make([]string, 0, 1),
				},
				integers: make([]int64, 0, 2),
			},
			elems: slices.Values([]Term{
				{tag: termTagCharacter, payload: 'a'},
				{tag: termTagCharacter, payload: 'b'},
			}),
			term: Term{tag: termTagReference, payload: 0},
		},
		{
			title: "insufficient atoms",
			heap: &Heap{
				terms: make([]Term, 0, 9),
				atoms: atomTable{
					Names: make([]string, 0),
				},
				integers: make([]int64, 0, 2),
			},
			elems: slices.Values([]Term{
				{tag: termTagCharacter, payload: 'a'},
				{tag: termTagCharacter, payload: 'b'},
			}),
			err: &ResourceError{Resource: "atoms"},
		},
		{
			title: "insufficient terms: tail",
			heap: &Heap{
				terms: make([]Term, 0, 8),
				atoms: atomTable{
					IDs: rbtree.Map[string, atomID]{
						Nodes: make([]rbtree.Node[string, atomID], 0, 4),
					},
					Names: make([]string, 0, 1),
				},
				integers: make([]int64, 0, 2),
			},
			elems: slices.Values([]Term{
				{tag: termTagCharacter, payload: 'a'},
				{tag: termTagCharacter, payload: 'b'},
			}),
			err: &ResourceError{Resource: "terms"},
		},
		{
			title: "insufficient terms: element",
			heap: &Heap{
				terms: make([]Term, 0, 7),
				atoms: atomTable{
					IDs: rbtree.Map[string, atomID]{
						Nodes: make([]rbtree.Node[string, atomID], 0, 4),
					},
					Names: make([]string, 0, 1),
				},
				integers: make([]int64, 0, 2),
			},
			elems: slices.Values([]Term{
				{tag: termTagCharacter, payload: 'a'},
				{tag: termTagCharacter, payload: 'b'},
			}),
			err: &ResourceError{Resource: "terms"},
		},
		{
			title: "insufficient terms: list constructor",
			heap: &Heap{
				terms: make([]Term, 0, 6),
				atoms: atomTable{
					IDs: rbtree.Map[string, atomID]{
						Nodes: make([]rbtree.Node[string, atomID], 0, 4),
					},
					Names: make([]string, 0, 1),
				},
				integers: make([]int64, 0, 2),
			},
			elems: slices.Values([]Term{
				{tag: termTagCharacter, payload: 'a'},
				{tag: termTagCharacter, payload: 'b'},
			}),
			err: &ResourceError{Resource: "terms"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			l, err := NewList(tt.heap, tt.elems)
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected: %v, got: %v", tt.err, err)
			}

			if l != tt.term {
				t.Errorf("expected: %v, got: %v", tt.term, l)
			}
		})
	}
}

func TestNewCharList(t *testing.T) {
	tests := []struct {
		title string
		heap  *Heap
		str   string
		term  Term
		err   error
	}{
		{
			title: "ok",
			heap: &Heap{
				atoms: atomTable{
					IDs: rbtree.Map[string, atomID]{
						Nodes: make([]rbtree.Node[string, atomID], 0, 4),
					},
					Names: make([]string, 0, 1),
				},
				strings: make(stringPool, 0, 1),
			},
			str:  "foo",
			term: Term{tag: termTagString, payload: 0},
		},
		{
			title: "insufficient atoms",
			heap:  &Heap{},
			str:   "foo",
			err:   &ResourceError{Resource: "atoms"},
		},
		{
			title: "insufficient strings",
			heap: &Heap{
				atoms: atomTable{
					IDs: rbtree.Map[string, atomID]{
						Nodes: make([]rbtree.Node[string, atomID], 0, 4),
					},
					Names: make([]string, 0, 1),
				},
			},
			str: "foo",
			err: &ResourceError{Resource: "strings"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			term, err := NewCharList(tt.heap, tt.str)
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected: %v, got: %v", tt.err, err)
			}
			if term != tt.term {
				t.Errorf("expected: %v, got: %v", tt.term, term)
			}
		})
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {

		})
	}
}

func TestTerm_Variable(t *testing.T) {
	h := NewHeap(1024)

	v, err := NewVariable(h)
	if err != nil {
		t.Fatal(err)
	}

	a, err := NewAtom(h, "a")
	if err != nil {
		t.Fatal(err)
	}

	tests := []struct {
		title string
		term  Term
		err   error
	}{
		{title: "variable", term: v, err: nil},
		{title: "atom", term: a, err: &UninstantiationError{Culprit: a}},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			err := tt.term.Variable(h)
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected: %v, got: %v", tt.err, err)
			}
		})
	}
}

func TestTerm_Atom(t *testing.T) {
	h := NewHeap(1024)

	v, err := NewVariable(h)
	if err != nil {
		t.Fatal(err)
	}

	a, err := NewAtom(h, "a")
	if err != nil {
		t.Fatal(err)
	}

	foo, err := NewAtom(h, "foo")
	if err != nil {
		t.Fatal(err)
	}

	one, err := NewInteger(h, 1)
	if err != nil {
		t.Fatal(err)
	}

	tests := []struct {
		title string
		term  Term
		name  string
		err   error
	}{
		{title: "atom", term: foo, name: "foo"},
		{title: "single-character atom", term: a, name: "a"},
		{title: "variable", term: v, err: ErrInstantiation},
		{title: "integer", term: one, err: &TypeError{ValidType: "atom", Culprit: one}},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			n, err := tt.term.Atom(h)
			if n != tt.name {
				t.Errorf("expected: %v, got: %v", tt.name, n)
			}
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected: %v, got: %v", tt.err, err)
			}
		})
	}
}

func TestTerm_Integer(t *testing.T) {
	h := NewHeap(1024)

	v, err := NewVariable(h)
	if err != nil {
		t.Fatal(err)
	}

	a, err := NewAtom(h, "a")
	if err != nil {
		t.Fatal(err)
	}

	one, err := NewInteger(h, 1)
	if err != nil {
		t.Fatal(err)
	}

	tests := []struct {
		title   string
		term    Term
		integer int64
		err     error
	}{
		{title: "integer", term: one, integer: 1},
		{title: "variable", term: v, err: ErrInstantiation},
		{title: "atom", term: a, err: &TypeError{ValidType: "integer", Culprit: a}},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			n, err := tt.term.Integer(h)
			if n != tt.integer {
				t.Errorf("expected: %v, got: %v", tt.integer, n)
			}
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected: %v, got: %v", tt.err, err)
			}
		})
	}
}

func TestTerm_Float(t *testing.T) {
	h := NewHeap(1024)

	v, err := NewVariable(h)
	if err != nil {
		t.Fatal(err)
	}

	a, err := NewAtom(h, "a")
	if err != nil {
		t.Fatal(err)
	}

	one, err := NewFloat(h, 1)
	if err != nil {
		t.Fatal(err)
	}

	tests := []struct {
		title string
		term  Term
		float float64
		err   error
	}{
		{title: "float", term: one, float: 1},
		{title: "variable", term: v, err: ErrInstantiation},
		{title: "atom", term: a, err: &TypeError{ValidType: "float", Culprit: a}},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			f, err := tt.term.Float(h)
			if f != tt.float {
				t.Errorf("expected: %v, got: %v", tt.float, f)
			}
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected: %v, got: %v", tt.err, err)
			}
		})
	}
}

func TestTerm_Compound(t *testing.T) {
	h := NewHeap(1024)

	v, err := NewVariable(h)
	if err != nil {
		t.Fatal(err)
	}

	a, err := NewAtom(h, "a")
	if err != nil {
		t.Fatal(err)
	}

	b, err := NewAtom(h, "b")
	if err != nil {
		t.Fatal(err)
	}

	f, err := NewCompound(h, "f", a, b)
	if err != nil {
		t.Fatal(err)
	}

	s, err := NewCharList(h, "foo")
	if err != nil {
		t.Fatal(err)
	}

	tests := []struct {
		title    string
		term     Term
		compound Compound
		err      error
	}{
		{title: "compound", term: f, compound: Compound{Functor: Functor{Name: "f", Arity: 2}, ref: f}},
		{title: "string", term: s, compound: Compound{Functor: Functor{Name: ".", Arity: 2}, ref: s}},
		{title: "variable", term: v, err: ErrInstantiation},
		{title: "atom", term: a, err: &TypeError{ValidType: "compound", Culprit: a}},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			c, err := tt.term.Compound(h)
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected: %v, got: %v", tt.err, err)
			}

			if c == nil {
				return
			}
			if !reflect.DeepEqual(c, &tt.compound) {
				t.Errorf("expected: %v, got: %v", &tt.compound, c)
			}
		})
	}
}

func TestTerm_List(t *testing.T) {
	h := NewHeap(2 * 1024)

	v, err := NewVariable(h)
	if err != nil {
		t.Fatal(err)
	}

	a, err := NewAtom(h, "a")
	if err != nil {
		t.Fatal(err)
	}

	b, err := NewAtom(h, "b")
	if err != nil {
		t.Fatal(err)
	}

	one, err := NewInteger(h, 1)
	if err != nil {
		t.Fatal(err)
	}

	l, err := NewList(h, slices.Values([]Term{a, b}))
	if err != nil {
		t.Fatal(err)
	}

	nl, err := NewPartialList(h, slices.Values([]Term{a, b}), a)
	if err != nil {
		t.Fatal(err)
	}

	nl2, err := NewPartialList(h, slices.Values([]Term{a, b}), one)
	if err != nil {
		t.Fatal(err)
	}

	pl, err := NewPartialList(h, slices.Values([]Term{a, b}), v)
	if err != nil {
		t.Fatal(err)
	}

	tail, err := NewVariable(h)
	if err != nil {
		t.Fatal(err)
	}
	cl, err := NewPartialList(h, slices.Values([]Term{a, b}), tail)
	if err != nil {
		t.Fatal(err)
	}
	ok, err := tail.Unify(h, cl)
	if err != nil {
		t.Fatal(err)
	}
	if !ok {
		t.Fatal("tail unification failed")
	}

	type result struct {
		term Term
		err  error
	}

	tests := []struct {
		title   string
		term    Term
		options []ListOption
		results []result
		count   int
	}{
		{title: "[a, b]", term: l, results: []result{
			{term: a},
			{term: b},
		}},
		{title: "[a, b] but just one", term: l, count: 1, results: []result{
			{term: a},
		}},
		{title: "[a, b|a]", term: nl, results: []result{
			{term: a},
			{term: b},
			{err: &TypeError{ValidType: "list", Culprit: nl}},
		}},
		{title: "[a, b|1]", term: nl2, results: []result{
			{term: a},
			{term: b},
			{err: &TypeError{ValidType: "list", Culprit: nl2}},
		}},
		{title: "[a, b|_]", term: pl, results: []result{
			{term: a},
			{term: b},
			{err: ErrInstantiation},
		}},
		{title: "[a, b|_] with AllowPartial", term: pl, options: []ListOption{AllowPartial}, results: []result{
			{term: a},
			{term: b},
		}},
		{title: "[a, b, a, b|...]", term: cl, results: []result{
			{term: a},
			{term: b},
			{err: &TypeError{ValidType: "list", Culprit: cl}},
		}},
		{title: "[a, b, a, b|...] with AllowCyclic", term: cl, options: []ListOption{AllowCycle}, count: 8, results: []result{
			{term: a},
			{term: b},
			{term: a},
			{term: b},
			{term: a},
			{term: b},
			{term: a},
			{term: b},
		}},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			count := 0
			var results []result
			for t, err := range tt.term.List(h, tt.options...) {
				if tt.count != 0 && count == tt.count {
					break
				}
				results = append(results, result{term: t, err: err})
				count++
			}
			if !reflect.DeepEqual(results, tt.results) {
				t.Errorf("expected: %+v, got: %+v", tt.results, results)
			}
		})
	}
}

func TestTerm_CharList(t *testing.T) {
	h := NewHeap(2 * 1024)

	a, err := NewAtom(h, "a")
	if err != nil {
		t.Fatal(err)
	}

	b, err := NewAtom(h, "b")
	if err != nil {
		t.Fatal(err)
	}

	c, err := NewAtom(h, "c")
	if err != nil {
		t.Fatal(err)
	}

	list, err := NewList(h, slices.Values([]Term{a, b, c}))
	if err != nil {
		t.Fatal(err)
	}

	str, err := NewCharList(h, "abc")
	if err != nil {
		t.Fatal(err)
	}

	tests := []struct {
		title string
		term  Term
		heap  *Heap
		str   string
		err   error
	}{
		{title: "[a, b, c]", term: list, heap: h, str: "abc"},
		{title: `"abc"`, term: str, heap: h, str: "abc"},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			str, err := tt.term.CharList(tt.heap)
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected: %v, got: %v", tt.err, err)
			}
			if str != tt.str {
				t.Errorf("expected: %v, got: %v", tt.str, str)
			}
		})
	}
}

func TestTerm_Callable(t *testing.T) {
	h := NewHeap(1024)

	a, err := NewAtom(h, "a")
	if err != nil {
		t.Fatal(err)
	}

	b, err := NewAtom(h, "b")
	if err != nil {
		t.Fatal(err)
	}

	fab, err := NewCompound(h, "f", a, b)
	if err != nil {
		t.Fatal(err)
	}

	tests := []struct {
		title    string
		term     Term
		heap     *Heap
		compound *Compound
		err      error
	}{
		{title: "a", term: a, heap: h, compound: &Compound{Functor: Functor{Name: "a", Arity: 0}}},
		{title: "f(a, b)", term: fab, heap: h, compound: &Compound{Functor: Functor{Name: "f", Arity: 2}, ref: fab}},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			c, err := tt.term.Callable(tt.heap)
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected: %v, got: %v", tt.err, err)
			}
			if *c != *tt.compound {
				t.Errorf("expected: %v, got: %v", tt.compound, c)
			}
		})
	}
}

func TestTerm_Unify(t *testing.T) {
	h := NewHeap(2 * 1024)

	a, err := NewAtom(h, "a")
	if err != nil {
		t.Fatal(err)
	}

	b, err := NewAtom(h, "b")
	if err != nil {
		t.Fatal(err)
	}

	fa, err := NewCompound(h, "f", a)
	if err != nil {
		t.Fatal(err)
	}

	ga, err := NewCompound(h, "g", a)
	if err != nil {
		t.Fatal(err)
	}

	fb, err := NewCompound(h, "f", b)
	if err != nil {
		t.Fatal(err)
	}

	v, err := NewVariable(h)
	if err != nil {
		t.Fatal(err)
	}

	w, err := NewVariable(h)
	if err != nil {
		t.Fatal(err)
	}

	x, err := NewVariable(h)
	if err != nil {
		t.Fatal(err)
	}

	fx, err := NewCompound(h, "f", x)
	if err != nil {
		t.Fatal(err)
	}

	tests := []struct {
		title string
		heap  *Heap
		x, y  Term
		ok    bool
		err   error
		env   map[variable]Term
	}{
		{title: "a = a", heap: h, x: a, y: a, ok: true},
		{title: "V = V", heap: h, x: v, y: v, ok: true},
		{title: "V = W", heap: h, x: v, y: w, ok: true, env: map[variable]Term{
			variable(v.payload): w,
		}},
		{title: "f(a) = g(a)", heap: h, x: fa, y: ga, ok: false},
		{title: "f(a) = f(b)", heap: h, x: fa, y: fb, ok: false},
		{title: "a = V", heap: h, x: a, y: v, ok: true, env: map[variable]Term{
			variable(v.payload): a,
		}},
		{title: "a = b", heap: h, x: a, y: b, ok: false},
		{title: "X = f(X)", heap: h, x: x, y: fx, ok: true, env: map[variable]Term{
			variable(x.payload): fx,
		}},
		{title: "insufficient variables", heap: &Heap{}, x: v, y: a, err: &ResourceError{Resource: "variables"}},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			snapshot := *tt.heap
			defer func() {
				*tt.heap = snapshot
			}()

			ok, err := tt.x.Unify(tt.heap, tt.y)
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected: %v, got: %v", tt.err, err)
				return
			}
			if ok != tt.ok {
				t.Errorf("expected: %v, got: %v", tt.ok, ok)
			}

			env := map[variable]Term{}
			for k, v := range h.env.Values.All() {
				env[k] = v
			}

			if !maps.Equal(env, tt.env) {
				t.Errorf("expected: %+v, got: %+v", tt.env, env)
			}
		})
	}
}

func TestTerm_UnifyWithOccursCheck(t *testing.T) {
	h := NewHeap(2 * 1024)

	a, err := NewAtom(h, "a")
	if err != nil {
		t.Fatal(err)
	}

	b, err := NewAtom(h, "b")
	if err != nil {
		t.Fatal(err)
	}

	fa, err := NewCompound(h, "f", a)
	if err != nil {
		t.Fatal(err)
	}

	ga, err := NewCompound(h, "g", a)
	if err != nil {
		t.Fatal(err)
	}

	fb, err := NewCompound(h, "f", b)
	if err != nil {
		t.Fatal(err)
	}

	v, err := NewVariable(h)
	if err != nil {
		t.Fatal(err)
	}

	w, err := NewVariable(h)
	if err != nil {
		t.Fatal(err)
	}

	x, err := NewVariable(h)
	if err != nil {
		t.Fatal(err)
	}

	fx, err := NewCompound(h, "f", x)
	if err != nil {
		t.Fatal(err)
	}

	tests := []struct {
		title string
		heap  *Heap
		x, y  Term
		ok    bool
		err   error
		env   map[variable]Term
	}{
		{title: "a = a", heap: h, x: a, y: a, ok: true},
		{title: "V = V", heap: h, x: v, y: v, ok: true},
		{title: "V = W", heap: h, x: v, y: w, ok: true, env: map[variable]Term{
			variable(v.payload): w,
		}},
		{title: "f(a) = g(a)", heap: h, x: fa, y: ga, ok: false},
		{title: "f(a) = f(b)", heap: h, x: fa, y: fb, ok: false},
		{title: "a = V", heap: h, x: a, y: v, ok: true, env: map[variable]Term{
			variable(v.payload): a,
		}},
		{title: "a = b", heap: h, x: a, y: b, ok: false},
		{title: "X = f(X)", heap: h, x: x, y: fx, ok: false},
		{title: "insufficient variables", heap: &Heap{}, x: v, y: a, err: &ResourceError{Resource: "variables"}},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			snapshot := *tt.heap
			defer func() {
				*tt.heap = snapshot
			}()

			ok, err := tt.x.UnifyWithOccursCheck(tt.heap, tt.y)
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected: %v, got: %v", tt.err, err)
				return
			}
			if ok != tt.ok {
				t.Errorf("expected: %v, got: %v", tt.ok, ok)
			}

			env := map[variable]Term{}
			for k, v := range h.env.Values.All() {
				env[k] = v
			}

			if !maps.Equal(env, tt.env) {
				t.Errorf("expected: %+v, got: %+v", tt.env, env)
			}
		})
	}
}

func TestTerm_Contains(t *testing.T) {
	h := NewHeap(1024)

	a, err := NewAtom(h, "a")
	if err != nil {
		t.Fatal(err)
	}

	b, err := NewAtom(h, "b")
	if err != nil {
		t.Fatal(err)
	}

	fa, err := NewCompound(h, "f", a)
	if err != nil {
		t.Fatal(err)
	}

	tests := []struct {
		title string
		x, y  Term
		ok    bool
	}{
		{title: "a contains a", x: a, y: a, ok: true},
		{title: "f(a) contains a", x: fa, y: a, ok: true},
		{title: "a doesn't contain b", x: a, y: b, ok: false},
		{title: "f(a) doesn't contain b", x: fa, y: b, ok: false},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ok := tt.x.Contains(h, tt.y)
			if ok != tt.ok {
				t.Errorf("expected: %v, got: %v", tt.ok, ok)
			}
		})
	}
}

func TestTerm_RenamedCopy(t *testing.T) {
	h := NewHeap(1024)

	a, err := NewAtom(h, "a")
	if err != nil {
		t.Fatal(err)
	}

	fa, err := NewCompound(h, "f", a)
	if err != nil {
		t.Fatal(err)
	}

	gfafa, err := NewCompound(h, "g", fa, fa)
	if err != nil {
		t.Fatal(err)
	}

	x, err := NewVariable(h)
	if err != nil {
		t.Fatal(err)
	}

	// Peek the next variable Y.
	snapshot := *h
	y, err := NewVariable(h)
	if err != nil {
		t.Fatal(err)
	}
	*h = snapshot

	smallHeap := Heap{
		terms:    make([]Term, 0, 8),
		integers: make([]int64, 0, 3),
	}
	sfa, err := NewCompound(&smallHeap, "f", a)
	if err != nil {
		t.Fatal(err)
	}
	sffa, err := NewCompound(&smallHeap, "f", sfa)
	if err != nil {
		t.Fatal(err)
	}

	tests := []struct {
		title  string
		term   Term
		heap   *Heap
		result Term
		err    error
	}{
		{title: "a", term: a, heap: h, result: a},
		{title: "g(f(a), f(a))", term: gfafa, heap: h, result: gfafa},
		{title: "X", term: x, heap: h, result: y},
		{title: "X with insufficient variables", term: x, heap: &Heap{env: env{lastVariable: math.MaxInt32}}, err: &ResourceError{Resource: "variables"}},
		{title: "f(f(a)) with insufficient terms", term: sffa, heap: &smallHeap, err: &ResourceError{Resource: "terms"}},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			result, err := tt.term.RenamedCopy(tt.heap)
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected: %v, got: %v", tt.err, err)
			}
			if o := result.Compare(tt.heap, tt.result); o != 0 {
				t.Errorf("expected: %v, got: %v", tt.result, result)
			}
		})
	}
}

func TestTerm_Cyclic(t *testing.T) {
	h := NewHeap(1024)

	a, err := NewAtom(h, "a")
	if err != nil {
		t.Fatal(err)
	}

	fa, err := NewCompound(h, "f", a)
	if err != nil {
		t.Fatal(err)
	}

	x, err := NewVariable(h)
	if err != nil {
		t.Fatal(err)
	}

	fx, err := NewCompound(h, "f", x)
	if err != nil {
		t.Fatal(err)
	}

	if _, err := x.Unify(h, fx); err != nil {
		t.Fatal(err)
	}

	tests := []struct {
		title string
		term  Term
		heap  *Heap
		ok    bool
	}{
		{title: "a", term: a, heap: h, ok: false},
		{title: "f(a)", term: fa, heap: h, ok: false},
		{title: "f(X) where X = f(X)", term: fx, heap: h, ok: true},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ok := tt.term.Cyclic(tt.heap)
			if ok != tt.ok {
				t.Errorf("expected: %v, got: %v", tt.ok, ok)
			}
		})
	}
}

func TestTerm_Unqualify(t *testing.T) {
	h := NewHeap(1024)

	foo, err := NewAtom(h, "foo")
	if err != nil {
		t.Fatal(err)
	}

	bar, err := NewAtom(h, "bar")
	if err != nil {
		t.Fatal(err)
	}

	fooBar, err := NewCompound(h, "foo", bar)
	if err != nil {
		t.Fatal(err)
	}

	fooColonBar, err := NewCompound(h, ":", foo, bar)
	if err != nil {
		t.Fatal(err)
	}

	one, err := NewInteger(h, 1)
	if err != nil {
		t.Fatal(err)
	}

	oneColonFoo, err := NewCompound(h, ":", one, foo)
	if err != nil {
		t.Fatal(err)
	}

	tests := []struct {
		title            string
		term             Term
		heap             *Heap
		module           string
		qualifyingModule string
		unqualifiedTerm  Term
	}{
		{title: "foo", term: foo, heap: h, module: "user", qualifyingModule: "user", unqualifiedTerm: foo},
		{title: "foo(bar)", term: fooBar, heap: h, module: "user", qualifyingModule: "user", unqualifiedTerm: fooBar},
		{title: "foo:bar", term: fooColonBar, heap: h, module: "user", qualifyingModule: "foo", unqualifiedTerm: bar},
		{title: "1:foo", term: oneColonFoo, heap: h, module: "user", qualifyingModule: "user", unqualifiedTerm: oneColonFoo},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			module, term := tt.term.Unqualify(tt.heap, tt.module)
			if module != tt.qualifyingModule {
				t.Errorf("expected: %v, got: %v", tt.qualifyingModule, module)
			}
			if term != tt.unqualifiedTerm {
				t.Errorf("expected: %v, got: %v", tt.unqualifiedTerm, term)
			}
		})
	}
}

func TestTerm_Compare(t *testing.T) {
	h := NewHeap(2 * 1024)

	w, err := NewVariable(h)
	if err != nil {
		t.Fatal(err)
	}

	x, err := NewVariable(h)
	if err != nil {
		t.Fatal(err)
	}

	y, err := NewVariable(h)
	if err != nil {
		t.Fatal(err)
	}

	a, err := NewAtom(h, "a")
	if err != nil {
		t.Fatal(err)
	}

	b, err := NewAtom(h, "b")
	if err != nil {
		t.Fatal(err)
	}

	z, err := NewAtom(h, "Z")
	if err != nil {
		t.Fatal(err)
	}

	i0, err := NewInteger(h, 0)
	if err != nil {
		t.Fatal(err)
	}

	i1, err := NewInteger(h, 1)
	if err != nil {
		t.Fatal(err)
	}

	i2, err := NewInteger(h, 2)
	if err != nil {
		t.Fatal(err)
	}

	f0, err := NewFloat(h, 0)
	if err != nil {
		t.Fatal(err)
	}

	f1, err := NewFloat(h, 1)
	if err != nil {
		t.Fatal(err)
	}

	f2, err := NewFloat(h, 2)
	if err != nil {
		t.Fatal(err)
	}

	fa, err := NewCompound(h, "f", a)
	if err != nil {
		t.Fatal(err)
	}

	fb, err := NewCompound(h, "f", b)
	if err != nil {
		t.Fatal(err)
	}

	fz, err := NewCompound(h, "f", z)
	if err != nil {
		t.Fatal(err)
	}

	ea, err := NewCompound(h, "e", a)
	if err != nil {
		t.Fatal(err)
	}

	ga, err := NewCompound(h, "g", a)
	if err != nil {
		t.Fatal(err)
	}

	fab, err := NewCompound(h, "f", a, b)
	if err != nil {
		t.Fatal(err)
	}

	tests := []struct {
		title    string
		lhs, rhs Term
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
			o := test.lhs.Compare(h, test.rhs)
			if o != test.o {
				t.Errorf("expected %d, got %d", test.o, o)
			}
		})
	}
}

func TestCompound_Arg(t *testing.T) {
	h := NewHeap(1024)

	a, err := NewAtom(h, "a")
	if err != nil {
		t.Fatal(err)
	}

	b, err := NewAtom(h, "b")
	if err != nil {
		t.Fatal(err)
	}

	fab, err := NewCompound(h, "f", a, b)
	if err != nil {
		t.Fatal(err)
	}

	cfab, err := fab.Compound(h)
	if err != nil {
		t.Fatal(err)
	}

	listAB, err := NewList(h, slices.Values([]Term{a, b}))
	if err != nil {
		t.Fatal(err)
	}

	listB, err := NewList(h, slices.Values([]Term{b}))
	if err != nil {
		t.Fatal(err)
	}

	cListAB, err := listAB.Compound(h)
	if err != nil {
		t.Fatal(err)
	}

	stringAB, err := NewCharList(h, "ab")
	if err != nil {
		t.Fatal(err)
	}

	cStringAB, err := stringAB.Compound(h)
	if err != nil {
		t.Fatal(err)
	}

	tests := []struct {
		title    string
		compound *Compound
		heap     *Heap
		n        int
		term     Term
		err      error
	}{
		{title: "f(a, b), 0", compound: cfab, heap: h, n: 0, term: a},
		{title: "f(a, b), 1", compound: cfab, heap: h, n: 1, term: b},
		{title: "[a, b], 0", compound: cListAB, heap: h, n: 0, term: a},
		{title: "[a, b], 1", compound: cListAB, heap: h, n: 1, term: listB},
		{title: `"ab", 0`, compound: cStringAB, heap: h, n: 0, term: Term{tag: termTagCharacter, payload: 'a'}},
		{title: `"ab", 1`, compound: cStringAB, heap: h, n: 1, term: listB},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			term := tt.compound.Arg(tt.heap, tt.n)
			if term.Compare(tt.heap, tt.term) != 0 {
				t.Errorf("expected %v, got %v", tt.term, term)
			}
		})
	}
}

func indirect[T any](t T) *T {
	return &t
}
