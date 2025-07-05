package prolog

import (
	"reflect"
	"testing"
)

func TestTermTag_String(t *testing.T) {
	tests := []struct {
		tag termTag
		str string
	}{
		{str: "invalid"},
		{tag: termTagReference, str: "reference"},
		{tag: termTagAtom, str: "atom"},
		{tag: termTagCharacter, str: "character"},
		{tag: termTagInteger, str: "integer"},
		{tag: termTagFloat, str: "float"},
		{tag: termTagStructure, str: "structure"},
		{tag: termTagFunctor, str: "functor"},
		{tag: termTagString0, str: "string(0)"},
		{tag: termTagString1, str: "string(1)"},
		{tag: termTagString2, str: "string(2)"},
		{tag: termTagString3, str: "string(3)"},
		{tag: termTagString4, str: "string(4)"},
		{tag: termTagString5, str: "string(5)"},
		{tag: termTagString6, str: "string(6)"},
		{tag: termTagString7, str: "string(7)"},
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
	h := NewHeap(&HeapConfig{
		MaxTerms: 1024,
		MaxAtoms: 1024,
	})
	if h == nil {
		t.Errorf("NewHeap() returned nil")
	}
}

func TestHeap_PutVariable(t *testing.T) {
	tests := []struct {
		title string
		heap  *Heap
		term  Term
		err   error
	}{
		{
			title: "ok",
			heap:  NewHeap(nil),
			term:  Term{tag: termTagReference, value: 0},
		},
		{
			title: "ng",
			heap:  &Heap{},
			err:   &ResourceError{Resource: "heap"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			v, err := tt.heap.PutVariable()
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected: %v, got: %v", tt.err, err)
			}

			if v != tt.term {
				t.Errorf("expected: %v, got: %v", tt.term, v)
			}
		})
	}
}

func TestHeap_Variable(t *testing.T) {
	h := NewHeap(nil)

	v, err := h.PutVariable()
	if err != nil {
		t.Fatal(err)
	}

	a, err := h.PutAtom("a")
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
			_, err := h.Variable(tt.term)
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected: %v, got: %v", tt.err, err)
			}
		})
	}
}

func TestHeap_PutAtom(t *testing.T) {
	tests := []struct {
		title string
		heap  *Heap
		name  Atom
		term  Term
		err   error
	}{
		{
			title: "single char",
			name:  "a",
			term:  Term{tag: termTagCharacter, value: 'a'},
		},
		{
			title: "multiple chars",
			heap:  NewHeap(nil),
			name:  "foo",
			term:  Term{tag: termTagAtom, value: 0},
		},
		{
			title: "ng",
			heap:  &Heap{},
			err:   &ResourceError{Resource: "atom"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			a, err := tt.heap.PutAtom(tt.name)
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected: %v, got: %v", tt.err, err)
			}

			if a != tt.term {
				t.Errorf("expected: %v, got: %v", tt.term, a)
			}
		})
	}
}

func TestHeap_Atom(t *testing.T) {
	h := NewHeap(nil)

	v, err := h.PutVariable()
	if err != nil {
		t.Fatal(err)
	}

	a, err := h.PutAtom("a")
	if err != nil {
		t.Fatal(err)
	}

	foo, err := h.PutAtom("foo")
	if err != nil {
		t.Fatal(err)
	}

	one, err := h.PutInteger(1)
	if err != nil {
		t.Fatal(err)
	}

	tests := []struct {
		title string
		term  Term
		name  Atom
		err   error
	}{
		{title: "atom", term: foo, name: "foo"},
		{title: "single-character atom", term: a, name: "a"},
		{title: "variable", term: v, err: ErrInstantiation},
		{title: "integer", term: one, err: &TypeError{ValidType: "atom", Culprit: one}},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			n, err := h.Atom(tt.term)
			if n != tt.name {
				t.Errorf("expected: %v, got: %v", tt.name, n)
			}
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected: %v, got: %v", tt.err, err)
			}
		})
	}
}

func TestHeap_Character(t *testing.T) {
	h := NewHeap(nil)

	v, err := h.PutVariable()
	if err != nil {
		t.Fatal(err)
	}

	a, err := h.PutAtom("a")
	if err != nil {
		t.Fatal(err)
	}

	foo, err := h.PutAtom("foo")
	if err != nil {
		t.Fatal(err)
	}

	one, err := h.PutInteger(1)
	if err != nil {
		t.Fatal(err)
	}

	tests := []struct {
		title     string
		term      Term
		character rune
		err       error
	}{
		{title: "atom", term: foo, err: &TypeError{ValidType: "character", Culprit: foo}},
		{title: "single-character atom", term: a, character: 'a'},
		{title: "variable", term: v, err: ErrInstantiation},
		{title: "integer", term: one, err: &TypeError{ValidType: "character", Culprit: one}},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			r, err := h.Character(tt.term)
			if r != tt.character {
				t.Errorf("expected: %v, got: %v", tt.character, r)
			}
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected: %v, got: %v", tt.err, err)
			}
		})
	}
}

func TestHeap_PutInteger(t *testing.T) {
	tests := []struct {
		title   string
		heap    *Heap
		integer int64
		term    Term
		err     error
	}{
		{
			title:   "ok",
			heap:    NewHeap(nil),
			integer: 1,
			term:    Term{tag: termTagInteger, value: 0},
		},
		{
			title: "ng",
			heap:  &Heap{},
			err:   &ResourceError{Resource: "heap"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			i, err := tt.heap.PutInteger(tt.integer)
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected: %v, got: %v", tt.err, err)
			}

			if i != tt.term {
				t.Errorf("expected: %v, got: %v", tt.term, i)
			}
		})
	}
}

func TestHeap_Integer(t *testing.T) {
	h := NewHeap(nil)

	v, err := h.PutVariable()
	if err != nil {
		t.Fatal(err)
	}

	a, err := h.PutAtom("a")
	if err != nil {
		t.Fatal(err)
	}

	one, err := h.PutInteger(1)
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
			n, err := h.Integer(tt.term)
			if n != tt.integer {
				t.Errorf("expected: %v, got: %v", tt.integer, n)
			}
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected: %v, got: %v", tt.err, err)
			}
		})
	}
}

func TestHeap_PutFloat(t *testing.T) {
	tests := []struct {
		title string
		heap  *Heap
		float float64
		term  Term
		err   error
	}{
		{
			title: "ok",
			heap:  NewHeap(nil),
			float: 1,
			term:  Term{tag: termTagFloat, value: 0},
		},
		{
			title: "ng",
			heap:  &Heap{},
			err:   &ResourceError{Resource: "heap"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			f, err := tt.heap.PutFloat(tt.float)
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected: %v, got: %v", tt.err, err)
			}

			if f != tt.term {
				t.Errorf("expected: %v, got: %v", tt.term, f)
			}
		})
	}
}

func TestHeap_Float(t *testing.T) {
	h := NewHeap(nil)

	v, err := h.PutVariable()
	if err != nil {
		t.Fatal(err)
	}

	a, err := h.PutAtom("a")
	if err != nil {
		t.Fatal(err)
	}

	one, err := h.PutFloat(1)
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
			f, err := h.Float(tt.term)
			if f != tt.float {
				t.Errorf("expected: %v, got: %v", tt.float, f)
			}
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected: %v, got: %v", tt.err, err)
			}
		})
	}
}

func TestHeap_PutCompound(t *testing.T) {
	tests := []struct {
		title string
		heap  *Heap
		name  Atom
		args  []Term
		term  Term
		err   error
	}{
		{
			title: "atom",
			name:  "f",
			term:  Term{tag: termTagCharacter, value: 'f'},
		},
		{
			title: "ok",
			heap:  NewHeap(nil),
			name:  "f",
			args: []Term{
				{tag: termTagCharacter, value: 'a'},
				{tag: termTagCharacter, value: 'b'},
			},
			term: Term{tag: termTagStructure, value: 3},
		},
		{
			title: "insufficient heap",
			heap:  &Heap{},
			name:  "foo",
			args: []Term{
				{tag: termTagCharacter, value: 'a'},
				{tag: termTagCharacter, value: 'b'},
			},
			err: &ResourceError{Resource: "atom"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			c, err := tt.heap.PutCompound(tt.name, tt.args...)
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected: %v, got: %v", tt.err, err)
			}

			if c != tt.term {
				t.Errorf("expected: %v, got: %v", tt.term, c)
			}
		})
	}
}

func TestHeap_PutList(t *testing.T) {
	tests := []struct {
		title string
		heap  *Heap
		elems []Term
		term  Term
		err   error
	}{
		{
			title: "empty",
			heap:  NewHeap(nil),
			elems: []Term{},
			term:  Term{tag: termTagAtom, value: 0},
		},
		{
			title: "ok",
			heap:  NewHeap(nil),
			elems: []Term{
				{tag: termTagCharacter, value: 'a'},
				{tag: termTagCharacter, value: 'b'},
			},
			term: Term{tag: termTagStructure, value: 3},
		},
		{
			title: "insufficient heap",
			heap:  &Heap{},
			elems: []Term{
				{tag: termTagCharacter, value: 'a'},
				{tag: termTagCharacter, value: 'b'},
			},
			err: &ResourceError{Resource: "atom"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			l, err := tt.heap.PutList(tt.elems...)
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected: %v, got: %v", tt.err, err)
			}

			if l != tt.term {
				t.Errorf("expected: %v, got: %v", tt.term, l)
			}
		})
	}
}

func TestHeap_PutCharList(t *testing.T) {
	tests := []struct {
		title string
		heap  *Heap
		str   string
		term  Term
		err   error
	}{
		{
			title: "ok",
			heap:  NewHeap(nil),
			str:   "foo",
			term:  Term{tag: termTagString0, value: 0},
		},
		{
			title: "insufficient atoms",
			heap:  &Heap{},
			str:   "foo",
			err:   &ResourceError{Resource: "atom"},
		},
		{
			title: "insufficient heap",
			heap:  NewHeap(&HeapConfig{MaxAtoms: 1}),
			str:   "foo",
			err:   &ResourceError{Resource: "heap"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			term, err := tt.heap.PutCharList(tt.str)
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected: %v, got: %v", tt.err, err)
			}
			if term != tt.term {
				t.Errorf("expected: %v, got: %v", tt.term, term)
			}
		})
	}
}

func TestHeap_Functor(t *testing.T) {
	h := NewHeap(nil)

	v, err := h.PutVariable()
	if err != nil {
		t.Fatal(err)
	}

	a, err := h.PutAtom("a")
	if err != nil {
		t.Fatal(err)
	}

	b, err := h.PutAtom("b")
	if err != nil {
		t.Fatal(err)
	}

	f, err := h.PutCompound("f", a, b)
	if err != nil {
		t.Fatal(err)
	}

	s, err := h.PutCharList("foo")
	if err != nil {
		t.Fatal(err)
	}

	tests := []struct {
		title   string
		term    Term
		functor Functor
		err     error
	}{
		{title: "compound", term: f, functor: Functor{Name: "f", Arity: 2}},
		{title: "string", term: s, functor: Functor{Name: ".", Arity: 2}},
		{title: "variable", term: v, err: ErrInstantiation},
		{title: "atom", term: a, err: &TypeError{ValidType: "compound", Culprit: a}},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			f, err := h.Functor(tt.term)
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected: %v, got: %v", tt.err, err)
			}

			if !reflect.DeepEqual(f, tt.functor) {
				t.Errorf("expected: %v, got: %v", tt.functor, f)
			}
		})
	}
}

func TestHeap_Arg(t *testing.T) {
	h := NewHeap(nil)

	a, err := h.PutAtom("a")
	if err != nil {
		t.Fatal(err)
	}

	b, err := h.PutAtom("b")
	if err != nil {
		t.Fatal(err)
	}

	fab, err := h.PutCompound("f", a, b)
	if err != nil {
		t.Fatal(err)
	}

	listAB, err := h.PutList(a, b)
	if err != nil {
		t.Fatal(err)
	}

	listB, err := h.PutList(b)
	if err != nil {
		t.Fatal(err)
	}

	stringAB, err := h.PutCharList("ab")
	if err != nil {
		t.Fatal(err)
	}

	tests := []struct {
		title string
		term  Term
		heap  *Heap
		n     int
		arg   Term
		err   error
	}{
		{title: "f(a, b), 0", term: fab, heap: h, n: 0, arg: a},
		{title: "f(a, b), 1", term: fab, heap: h, n: 1, arg: b},
		{title: "[a, b], 0", term: listAB, heap: h, n: 0, arg: a},
		{title: "[a, b], 1", term: listAB, heap: h, n: 1, arg: listB},
		{title: `"ab", 0`, term: stringAB, heap: h, n: 0, arg: Term{tag: termTagCharacter, value: 'a'}},
		{title: `"ab", 1`, term: stringAB, heap: h, n: 1, arg: listB},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			term := tt.heap.Arg(tt.term, tt.n)
			if tt.heap.Compare(term, tt.arg) != 0 {
				t.Errorf("expected %v, got %v", tt.arg, term)
			}
		})
	}
}

func TestHeap_List(t *testing.T) {
	h := NewHeap(nil)

	v, err := h.PutVariable()
	if err != nil {
		t.Fatal(err)
	}

	a, err := h.PutAtom("a")
	if err != nil {
		t.Fatal(err)
	}

	b, err := h.PutAtom("b")
	if err != nil {
		t.Fatal(err)
	}

	one, err := h.PutInteger(1)
	if err != nil {
		t.Fatal(err)
	}

	l, err := h.PutList(a, b)
	if err != nil {
		t.Fatal(err)
	}

	nl, err := h.PutPartialList(a, a, b)
	if err != nil {
		t.Fatal(err)
	}

	nl2, err := h.PutPartialList(one, a, b)
	if err != nil {
		t.Fatal(err)
	}

	pl, err := h.PutPartialList(v, a, b)
	if err != nil {
		t.Fatal(err)
	}

	tail, err := h.PutVariable()
	if err != nil {
		t.Fatal(err)
	}
	cl, err := h.PutPartialList(tail, a, b)
	if err != nil {
		t.Fatal(err)
	}
	var trail []Variable
	if !h.unify(&trail, tail, cl, false) {
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
			{term: a, err: &TypeError{ValidType: "list", Culprit: nl}},
		}},
		{title: "[a, b|1]", term: nl2, results: []result{
			{term: a},
			{term: b},
			{term: one, err: &TypeError{ValidType: "list", Culprit: nl2}},
		}},
		{title: "[a, b|_]", term: pl, results: []result{
			{term: a},
			{term: b},
			{term: Term{tag: termTagReference, value: 0}, err: ErrInstantiation},
		}},
		{title: "[a, b|_] with AllowPartial", term: pl, options: []ListOption{AllowPartial(true)}, results: []result{
			{term: a},
			{term: b},
		}},
		{title: "[a, b, a, b|...]", term: cl, results: []result{
			{term: a},
			{term: b},
			{term: cl, err: &TypeError{ValidType: "list", Culprit: cl}},
		}},
		{title: "[a, b, a, b|...] with AllowCyclic", term: cl, options: []ListOption{AllowCycle(true)}, count: 8, results: []result{
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
			for t, err := range h.List(tt.term, tt.options...) {
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
	h := NewHeap(nil)

	a, err := h.PutAtom("a")
	if err != nil {
		t.Fatal(err)
	}

	b, err := h.PutAtom("b")
	if err != nil {
		t.Fatal(err)
	}

	c, err := h.PutAtom("c")
	if err != nil {
		t.Fatal(err)
	}

	list, err := h.PutList(a, b, c)
	if err != nil {
		t.Fatal(err)
	}

	str, err := h.PutCharList("abc")
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
			str, err := tt.heap.CharList(tt.term)
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected: %v, got: %v", tt.err, err)
			}
			if str != tt.str {
				t.Errorf("expected: %v, got: %v", tt.str, str)
			}
		})
	}
}

func TestHeap_Contains(t *testing.T) {
	h := NewHeap(nil)

	a, err := h.PutAtom("a")
	if err != nil {
		t.Fatal(err)
	}

	b, err := h.PutAtom("b")
	if err != nil {
		t.Fatal(err)
	}

	fa, err := h.PutCompound("f", a)
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
			ok := h.Contains(tt.x, tt.y)
			if ok != tt.ok {
				t.Errorf("expected: %v, got: %v", tt.ok, ok)
			}
		})
	}
}

func TestHeap_RenamedCopy(t *testing.T) {
	h := NewHeap(nil)

	a, err := h.PutAtom("a")
	if err != nil {
		t.Fatal(err)
	}

	fa, err := h.PutCompound("f", a)
	if err != nil {
		t.Fatal(err)
	}

	gfafa, err := h.PutCompound("g", fa, fa)
	if err != nil {
		t.Fatal(err)
	}

	x, err := h.PutVariable()
	if err != nil {
		t.Fatal(err)
	}

	smallHeap := NewHeap(&HeapConfig{MaxTerms: 10})
	sfa, err := smallHeap.PutCompound("f", a)
	if err != nil {
		t.Fatal(err)
	}
	sffa, err := smallHeap.PutCompound("f", sfa)
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
		{title: "X", term: x, heap: h, result: Term{tag: termTagReference, value: 23}},
		{title: "f(f(a)) with insufficient terms", term: sffa, heap: smallHeap, err: &ResourceError{Resource: "heap"}},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			result, err := tt.heap.RenamedCopy(tt.term)
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected: %v, got: %v", tt.err, err)
			}
			if o := tt.heap.Compare(result, tt.result); o != 0 {
				t.Errorf("expected: %v, got: %v", tt.result, result)
			}
		})
	}
}

func TestHeap_Cyclic(t *testing.T) {
	h := NewHeap(nil)

	a, err := h.PutAtom("a")
	if err != nil {
		t.Fatal(err)
	}

	fa, err := h.PutCompound("f", a)
	if err != nil {
		t.Fatal(err)
	}

	x, err := h.PutVariable()
	if err != nil {
		t.Fatal(err)
	}

	fx, err := h.PutCompound("f", x)
	if err != nil {
		t.Fatal(err)
	}

	var trail []Variable
	if !h.unify(&trail, x, fx, false) {
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
			ok := tt.heap.Cyclic(tt.term)
			if ok != tt.ok {
				t.Errorf("expected: %v, got: %v", tt.ok, ok)
			}
		})
	}
}

func TestHeap_Unqualify(t *testing.T) {
	h := NewHeap(nil)

	foo, err := h.PutAtom("foo")
	if err != nil {
		t.Fatal(err)
	}

	bar, err := h.PutAtom("bar")
	if err != nil {
		t.Fatal(err)
	}

	fooBar, err := h.PutCompound("foo", bar)
	if err != nil {
		t.Fatal(err)
	}

	fooColonBar, err := h.PutCompound(":", foo, bar)
	if err != nil {
		t.Fatal(err)
	}

	one, err := h.PutInteger(1)
	if err != nil {
		t.Fatal(err)
	}

	oneColonFoo, err := h.PutCompound(":", one, foo)
	if err != nil {
		t.Fatal(err)
	}

	tests := []struct {
		title            string
		term             Term
		heap             *Heap
		module           Atom
		qualifyingModule Atom
		unqualifiedTerm  Term
	}{
		{title: "foo", term: foo, heap: h, module: "user", qualifyingModule: "user", unqualifiedTerm: foo},
		{title: "foo(bar)", term: fooBar, heap: h, module: "user", qualifyingModule: "user", unqualifiedTerm: fooBar},
		{title: "foo:bar", term: fooColonBar, heap: h, module: "user", qualifyingModule: "foo", unqualifiedTerm: bar},
		{title: "1:foo", term: oneColonFoo, heap: h, module: "user", qualifyingModule: "user", unqualifiedTerm: oneColonFoo},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			module, term := tt.heap.Unqualify(tt.term, tt.module)
			if module != tt.qualifyingModule {
				t.Errorf("expected: %v, got: %v", tt.qualifyingModule, module)
			}
			if term != tt.unqualifiedTerm {
				t.Errorf("expected: %v, got: %v", tt.unqualifiedTerm, term)
			}
		})
	}
}

func TestHeap_Compare(t *testing.T) {
	h := NewHeap(nil)

	w, err := h.PutVariable()
	if err != nil {
		t.Fatal(err)
	}

	x, err := h.PutVariable()
	if err != nil {
		t.Fatal(err)
	}

	y, err := h.PutVariable()
	if err != nil {
		t.Fatal(err)
	}

	a, err := h.PutAtom("a")
	if err != nil {
		t.Fatal(err)
	}

	b, err := h.PutAtom("b")
	if err != nil {
		t.Fatal(err)
	}

	z, err := h.PutAtom("Z")
	if err != nil {
		t.Fatal(err)
	}

	i0, err := h.PutInteger(0)
	if err != nil {
		t.Fatal(err)
	}

	i1, err := h.PutInteger(1)
	if err != nil {
		t.Fatal(err)
	}

	i2, err := h.PutInteger(2)
	if err != nil {
		t.Fatal(err)
	}

	f0, err := h.PutFloat(0)
	if err != nil {
		t.Fatal(err)
	}

	f1, err := h.PutFloat(1)
	if err != nil {
		t.Fatal(err)
	}

	f2, err := h.PutFloat(2)
	if err != nil {
		t.Fatal(err)
	}

	fa, err := h.PutCompound("f", a)
	if err != nil {
		t.Fatal(err)
	}

	fb, err := h.PutCompound("f", b)
	if err != nil {
		t.Fatal(err)
	}

	fz, err := h.PutCompound("f", z)
	if err != nil {
		t.Fatal(err)
	}

	ea, err := h.PutCompound("e", a)
	if err != nil {
		t.Fatal(err)
	}

	ga, err := h.PutCompound("g", a)
	if err != nil {
		t.Fatal(err)
	}

	fab, err := h.PutCompound("f", a, b)
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
			o := h.Compare(test.lhs, test.rhs)
			if o != test.o {
				t.Errorf("expected %d, got %d", test.o, o)
			}
		})
	}
}

/*

func TestTerm_Unify(t *testing.T) {
	h := NewHeap(2 * 1024)

	a, err := h.PutAtom("a")
	if err != nil {
		t.Fatal(err)
	}

	b, err := h.PutAtom("b")
	if err != nil {
		t.Fatal(err)
	}

	fa, err := h.PutCompound("f", a)
	if err != nil {
		t.Fatal(err)
	}

	ga, err := h.PutCompound("g", a)
	if err != nil {
		t.Fatal(err)
	}

	fb, err := h.PutCompound("f", b)
	if err != nil {
		t.Fatal(err)
	}

	v, err := h.PutVariable()
	if err != nil {
		t.Fatal(err)
	}

	w, err := h.PutVariable()
	if err != nil {
		t.Fatal(err)
	}

	x, err := h.PutVariable()
	if err != nil {
		t.Fatal(err)
	}

	fx, err := h.PutCompound("f", x)
	if err != nil {
		t.Fatal(err)
	}

	tests := []struct {
		title string
		heap  *Heap
		x, y  Term
		ok    bool
		err   error
		env   map[Variable]Term
	}{
		{title: "a = a", heap: h, x: a, y: a, ok: true},
		{title: "V = V", heap: h, x: v, y: v, ok: true},
		{title: "V = W", heap: h, x: v, y: w, ok: true, env: map[Variable]Term{
			Variable(v.value): w,
		}},
		{title: "f(a) = g(a)", heap: h, x: fa, y: ga, ok: false},
		{title: "f(a) = f(b)", heap: h, x: fa, y: fb, ok: false},
		{title: "a = V", heap: h, x: a, y: v, ok: true, env: map[Variable]Term{
			Variable(v.value): a,
		}},
		{title: "a = b", heap: h, x: a, y: b, ok: false},
		{title: "X = f(X)", heap: h, x: x, y: fx, ok: true, env: map[Variable]Term{
			Variable(x.value): fx,
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

			env := map[Variable]Term{}
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

	a, err := h.PutAtom("a")
	if err != nil {
		t.Fatal(err)
	}

	b, err := h.PutAtom("b")
	if err != nil {
		t.Fatal(err)
	}

	fa, err := h.PutCompound("f", a)
	if err != nil {
		t.Fatal(err)
	}

	ga, err := h.PutCompound("g", a)
	if err != nil {
		t.Fatal(err)
	}

	fb, err := h.PutCompound("f", b)
	if err != nil {
		t.Fatal(err)
	}

	v, err := h.PutVariable()
	if err != nil {
		t.Fatal(err)
	}

	w, err := h.PutVariable()
	if err != nil {
		t.Fatal(err)
	}

	x, err := h.PutVariable()
	if err != nil {
		t.Fatal(err)
	}

	fx, err := h.PutCompound("f", x)
	if err != nil {
		t.Fatal(err)
	}

	tests := []struct {
		title string
		heap  *Heap
		x, y  Term
		ok    bool
		err   error
		env   map[Variable]Term
	}{
		{title: "a = a", heap: h, x: a, y: a, ok: true},
		{title: "V = V", heap: h, x: v, y: v, ok: true},
		{title: "V = W", heap: h, x: v, y: w, ok: true, env: map[Variable]Term{
			Variable(v.value): w,
		}},
		{title: "f(a) = g(a)", heap: h, x: fa, y: ga, ok: false},
		{title: "f(a) = f(b)", heap: h, x: fa, y: fb, ok: false},
		{title: "a = V", heap: h, x: a, y: v, ok: true, env: map[Variable]Term{
			Variable(v.value): a,
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

			env := map[Variable]Term{}
			for k, v := range h.env.Values.All() {
				env[k] = v
			}

			if !maps.Equal(env, tt.env) {
				t.Errorf("expected: %+v, got: %+v", tt.env, env)
			}
		})
	}
}
*/
