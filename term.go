package prolog

import (
	"fmt"
	"iter"
	"math"
	"slices"
	"strings"
	"unicode/utf8"
	"unsafe"

	"github.com/ichiban/prolog/v2/internal/rbtree"
)

type termTag int8

const (
	termTagVariable = iota + 1
	termTagAtom
	termTagCharacter
	termTagInteger
	termTagFloat
	termTagReference
	termTagCompound
	termTagString
)

func (t termTag) String() string {
	switch t {
	case termTagVariable:
		return "variable"
	case termTagCharacter:
		return "character"
	case termTagAtom:
		return "atom"
	case termTagInteger:
		return "integer"
	case termTagFloat:
		return "float"
	case termTagReference:
		return "reference"
	case termTagCompound:
		return "compound"
	case termTagString:
		return "string"
	default:
		return "unknown"
	}
}

// Heap is a memory region to store Terms.
type Heap struct {
	terms []Term

	env      env
	atoms    atomTable
	integers []int64
	floats   []float64
	strings  stringPool
}

// NewHeap creates a heap with given bytes.
// Those bytes are distributed among several arrays of respective data types.
func NewHeap(bytes int) *Heap {
	var (
		termBytes    = int(unsafe.Sizeof(Term{}))
		atomBytes    = int(unsafe.Sizeof(rbtree.Map[string, atomID]{})) // TODO: ?
		integerBytes = int(unsafe.Sizeof(int64(0)))
		floatBytes   = int(unsafe.Sizeof(float64(0)))
		bindingBytes = int(unsafe.Sizeof(rbtree.Node[Variable, Term]{}))
		stringBytes  = int(unsafe.Sizeof(stringEntry{}))
	)

	const (
		termRatio    = 5
		atomRatio    = 2
		integerRatio = 1
		floatRatio   = 1
		bindingRatio = 5
		stringRatio  = 1
	)

	unit := bytes / (termRatio + atomRatio + integerRatio + floatRatio + bindingRatio + stringRatio)

	var (
		MaxTerms    = (termRatio * unit) / termBytes
		MaxAtoms    = (atomRatio * unit) / atomBytes
		MaxIntegers = (integerRatio * unit) / integerBytes
		MaxFloats   = (floatRatio * unit) / floatBytes
		MaxBindings = (bindingRatio * unit) / bindingBytes
		MaxStrings  = (stringRatio * unit) / stringBytes
	)

	h := Heap{
		terms: make([]Term, 0, MaxTerms),

		env: env{
			Values: rbtree.Map[Variable, Term]{
				Nodes: make([]rbtree.Node[Variable, Term], 0, MaxBindings),
			},
		},
		atoms: atomTable{
			IDs: rbtree.Map[string, atomID]{
				Nodes: make([]rbtree.Node[string, atomID], 0, MaxAtoms),
			},
			Names: make([]string, 0, MaxAtoms),
		},
		integers: make([]int64, 0, MaxIntegers),
		floats:   make([]float64, 0, MaxFloats),
		strings:  make(stringPool, 0, MaxStrings),
	}

	return &h
}

func (h *Heap) put(ts ...Term) (Term, error) {
	id := len(h.terms)

	var ok bool
	h.terms, ok = cappend(h.terms, ts...)
	if !ok {
		return Term{}, &ResourceError{Resource: "terms"}
	}

	return Term{tag: termTagReference, payload: int32(id)}, nil
}

func (h *Heap) putFunctor(f Functor) (Term, error) {
	n, err := NewAtom(h, f.Name)
	if err != nil {
		return Term{}, err
	}

	a, err := NewInteger(h, int64(f.Arity))
	if err != nil {
		return Term{}, err
	}

	// TODO: Compact it!
	return h.put(Term{tag: termTagCompound}, n, a)
}

// Term is a Prolog datum.
// It may refer to another Term in Heap.
type Term struct {
	tag     termTag
	payload int32
}

func (t Term) String() string {
	return fmt.Sprintf("<%s %d>", t.tag, t.payload)
}

// NewVariable creates a variable term.
func NewVariable(h *Heap) (Term, error) {
	v, err := h.env.Generate()
	if err != nil {
		return Term{}, err
	}
	return Term{tag: termTagVariable, payload: int32(v)}, nil
}

// Variable returns an error if it's not a variable term.
func (t Term) Variable(h *Heap) (Variable, error) {
	t = t.resolve(h)
	if t.tag != termTagVariable {
		return 0, &UninstantiationError{Culprit: t}
	}
	return Variable(t.payload), nil
}

func (t Term) resolve(h *Heap) Term {
	for {
		if t.tag != termTagVariable {
			return t
		}

		val, ok := h.env.Values.Get(Variable(t.payload))
		if !ok {
			return t
		}
		t = val
	}
}

// NewAtom creates an atom term.
func NewAtom(h *Heap, name string) (Term, error) {
	// A one-char atom is just a rune.
	if r, n := utf8.DecodeLastRuneInString(name); r != utf8.RuneError && n == len(name) {
		return Term{tag: termTagCharacter, payload: r}, nil
	}

	atom, err := h.atoms.Put(name)
	if err != nil {
		return Term{}, &ResourceError{Resource: "atoms"}
	}
	return Term{tag: termTagAtom, payload: int32(atom)}, nil
}

// Atom returns the name if it's an atom term.
// Otherwise, it returns an error.
func (t Term) Atom(h *Heap) (string, error) {
	t = t.resolve(h)

	switch t.tag {
	case termTagVariable:
		return "", ErrInstantiation
	case termTagCharacter:
		return string(t.payload), nil
	case termTagAtom:
		break
	default:
		return "", &TypeError{ValidType: "atom", Culprit: t}
	}

	a := h.atoms.Names[int(t.payload)]
	return a, nil
}

func (t Term) Character(h *Heap) (rune, error) {
	t = t.resolve(h)

	switch t.tag {
	case termTagVariable:
		return 0, ErrInstantiation
	case termTagCharacter:
		return t.payload, nil
	default:
		return 0, &TypeError{ValidType: "character", Culprit: t}
	}
}

// NewInteger creates an integer term.
func NewInteger(h *Heap, n int64) (Term, error) {
	// TODO: optimize for smaller/bigger integers.
	var ok bool
	h.integers, ok = cappend(h.integers, n)
	if !ok {
		return Term{}, &ResourceError{Resource: "integers"}
	}

	return Term{tag: termTagInteger, payload: int32(len(h.integers) - 1)}, nil
}

// Integer returns the integer if it's an integer term.
// Otherwise, it returns an error.
func (t Term) Integer(h *Heap) (int64, error) {
	t = t.resolve(h)

	switch t.tag {
	case termTagVariable:
		return 0, ErrInstantiation
	case termTagInteger:
		break
	default:
		return 0, &TypeError{ValidType: "integer", Culprit: t}
	}

	return h.integers[t.payload], nil
}

// NewFloat creates a float term.
func NewFloat(h *Heap, f float64) (Term, error) {
	var ok bool
	h.floats, ok = cappend(h.floats, f)
	if !ok {
		return Term{}, &ResourceError{Resource: "floats"}
	}

	return Term{tag: termTagFloat, payload: int32(len(h.floats) - 1)}, nil
}

// Float returns a float value if it's a float term.
// Otherwise, it returns an error.
func (t Term) Float(h *Heap) (float64, error) {
	t = t.resolve(h)

	switch t.tag {
	case termTagVariable:
		return 0, ErrInstantiation
	case termTagFloat:
		break
	default:
		return 0, &TypeError{ValidType: "float", Culprit: t}
	}

	return h.floats[t.payload], nil
}

// NewCompound creates a compound term.
func NewCompound(h *Heap, name string, args ...Term) (Term, error) {
	if len(args) == 0 {
		return NewAtom(h, name)
	}

	ref, err := h.putFunctor(Functor{Name: name, Arity: len(args)})
	if err != nil {
		return Term{}, err
	}

	if _, err := h.put(args...); err != nil {
		return Term{}, err
	}

	return ref, nil
}

// NewList creates a series of compound terms for a list.
func NewList(h *Heap, elems ...Term) (Term, error) {
	tail, err := NewAtom(h, "[]")
	if err != nil {
		return Term{}, err
	}
	return NewPartialList(h, tail, elems...)
}

// NewPartialList creates a series of compound terms for a partial list with the specified tail term.
func NewPartialList(h *Heap, tail Term, elems ...Term) (Term, error) {
	id := int32(len(h.terms))

	// CDR coding
	empty := true
	for _, t := range elems {
		empty = false
		if _, err := h.putFunctor(Functor{Name: ".", Arity: 2}); err != nil {
			return Term{}, err
		}
		if _, err := h.put(t); err != nil {
			return Term{}, err
		}
	}
	if empty {
		return tail, nil
	}
	if _, err := h.put(tail); err != nil {
		return Term{}, err
	}

	return Term{tag: termTagReference, payload: id}, nil
}

// NewCharList creates a list of single-character atoms.
func NewCharList(h *Heap, str string) (Term, error) {
	tail, err := NewAtom(h, "[]")
	if err != nil {
		return Term{}, err
	}
	return NewPartialCharList(h, str, tail)
}

func NewPartialCharList(h *Heap, str string, tail Term) (Term, error) {
	id, err := h.strings.Put(str, tail)
	if err != nil {
		return Term{}, &ResourceError{Resource: "strings"}
	}
	return Term{tag: termTagString, payload: int32(id)}, nil
}

// NewCodeList creates a list of single-character atoms.
func NewCodeList(h *Heap, str string) (Term, error) {
	tail, err := NewAtom(h, "[]")
	if err != nil {
		return Term{}, err
	}
	return NewPartialCodeList(h, str, tail)
}

func NewPartialCodeList(h *Heap, str string, tail Term) (Term, error) {
	var elems []Term
	for _, r := range str {
		i, err := NewInteger(h, int64(r))
		if err != nil {
			return Term{}, err
		}
		elems = append(elems, i)
	}
	return NewPartialList(h, tail, elems...)
}

// Functor is a Name with Arity.
type Functor struct {
	Name  string
	Arity int
}

func (f Functor) String() string {
	return fmt.Sprintf("%s/%d", f.Name, f.Arity)
}

// Compound represents a compound term.
type Compound struct {
	Functor
	ref Term
}

// Arg returns an argument of the compound term.
func (c *Compound) Arg(h *Heap, n int) Term {
	if c.ref.tag == termTagString {
		if n == 0 {
			return h.strings.First(stringID(c.ref.payload))
		}
		return h.strings.Rest(stringID(c.ref.payload))
	}

	// Assuming h.terms[c.ref.payload].tag is termTagCompound
	id := int(c.ref.payload) + 3 + n
	t := h.terms[id]
	if t.tag == termTagCompound { // i.e. CDR coding
		t = Term{tag: termTagReference, payload: int32(id)}
	}
	return t
}

// Args returns an iterator of the arguments of the compound term.
func (c *Compound) Args(h *Heap) iter.Seq2[int, Term] {
	return func(yield func(int, Term) bool) {
		for i := range c.Arity {
			a := c.Arg(h, i)
			if !yield(i, a) {
				break
			}
		}
	}
}

// Compound returns a Compound value if it's a compound term.
// Otherwise, it returns an error.
func (t Term) Compound(h *Heap) (*Compound, error) {
	t = t.resolve(h)

	switch t.tag {
	case termTagVariable:
		return nil, ErrInstantiation
	case termTagString:
		return &Compound{Functor: Functor{Name: ".", Arity: 2}, ref: t}, nil
	case termTagReference:
		break
	default:
		return nil, &TypeError{ValidType: "compound", Culprit: t}
	}

	id := t.payload

	// Assuming h.terms[id].tag == termTagCompound
	name, _ := h.terms[id+1].Atom(h)
	arity, _ := h.terms[id+2].Integer(h)
	return &Compound{
		Functor: Functor{Name: name, Arity: int(arity)},
		ref:     t,
	}, nil
}

// ListOptions is a set of options that configures how a list iterator behaves.
type ListOptions struct {
	allowCycle   bool
	allowPartial bool
}

// ListOption is an option for list iterators.
type ListOption func(*ListOptions)

// AllowCycle configures the list iterator to allow cyclic lists.
func AllowCycle(ok bool) func(*ListOptions) {
	return func(opts *ListOptions) {
		opts.allowCycle = ok
	}
}

// AllowPartial configures the list iterator to allow partial lists.
func AllowPartial(ok bool) func(*ListOptions) {
	return func(opts *ListOptions) {
		opts.allowPartial = ok
	}
}

// List returns an iterator iterates over the elements of a list.
func (t Term) List(h *Heap, opts ...ListOption) iter.Seq2[Term, error] {
	var o ListOptions
	for _, opt := range opts {
		opt(&o)
	}

	// Brent's cycle detection algorithm
	var (
		tortoise Term
		hare     = t.resolve(h)
		power    = 1
		lam      = 1
	)
	return func(yield func(Term, error) bool) {
		for {
			if tortoise == hare && !o.allowCycle { // Detected a cycle.
				_ = yield(hare, &TypeError{ValidType: "list", Culprit: t})
				return
			}

			if power == lam {
				tortoise = hare
				power *= 2
				lam = 0
			}

			if _, err := hare.Variable(h); err == nil {
				if !o.allowPartial {
					_ = yield(hare, ErrInstantiation)
				}
				return
			}

			if a, err := hare.Atom(h); err == nil {
				if a != "[]" {
					_ = yield(hare, &TypeError{ValidType: "list", Culprit: t})
				}
				return
			}

			c, err := hare.Compound(h)
			if err != nil || c.Functor != (Functor{Name: ".", Arity: 2}) {
				_ = yield(hare, &TypeError{ValidType: "list", Culprit: t})
				return
			}

			if !yield(c.Arg(h, 0), nil) {
				return
			}

			hare = c.Arg(h, 1).resolve(h)
			lam++
		}
	}
}

// CharList returns a string if the term is a list of single-character atoms.
func (t Term) CharList(h *Heap) (string, error) {
	t = t.resolve(h)

	if t.tag == termTagString {
		e := h.strings[t.payload]
		n, err := e.tail.Atom(h)
		if err != nil || n != "[]" {
			return "", &TypeError{ValidType: "list", Culprit: t}
		}
		return e.string, nil
	}

	var sb strings.Builder
	for elem, err := range t.List(h) {
		if err != nil {
			return "", err
		}

		c, err := elem.Character(h)
		if err != nil {
			return "", err
		}

		_, _ = sb.WriteRune(c)
	}
	return sb.String(), nil
}

// Callable returns a compound value if the term is either an atom or a compound term.
func (t Term) Callable(h *Heap) (*Compound, error) {
	if a, err := t.Atom(h); err == nil {
		return &Compound{Functor: Functor{Name: a}}, nil
	}

	return t.Compound(h)
}

// Unify unifies two terms.
func (t Term) Unify(h *Heap, u Term) (bool, error) {
	return unify(h, t, u, false)
}

// UnifyWithOccursCheck unifies two terms with occurs-check.
func (t Term) UnifyWithOccursCheck(h *Heap, u Term) (bool, error) {
	return unify(h, t, u, true)
}

func unify(h *Heap, x, y Term, occursCheck bool) (bool, error) {
	x, y = x.resolve(h), y.resolve(h)

	if x == y {
		return true, nil
	}

	if _, err := x.Variable(h); err == nil {
		if _, err := y.Compound(h); err == nil && occursCheck {
			if y.Contains(h, x) {
				return false, nil
			}
		}
		if err := h.env.Values.Set(Variable(x.payload), y); err != nil {
			return false, &ResourceError{Resource: "variables"}
		}
		return true, nil
	}

	if cx, err := x.Compound(h); err == nil {
		if cy, err := y.Compound(h); err == nil {
			if cx.Functor != cy.Functor {
				return false, nil
			}

			for i := range cx.Arity {
				x, y := cx.Arg(h, i), cy.Arg(h, i)

				ok, err := unify(h, x, y, occursCheck)
				if err != nil || !ok {
					return ok, err
				}
			}
		}
	}

	if _, err := y.Variable(h); err == nil {
		return unify(h, y, x, occursCheck)
	}

	return false, nil
}

// Contains returns true if one term contains another.
func (t Term) Contains(h *Heap, u Term) bool {
	return contains(h, t, u)
}

func contains(h *Heap, t, u Term) bool {
	t, u = t.resolve(h), u.resolve(h)

	if t == u {
		return true
	}

	c, err := t.Compound(h)
	if err != nil {
		return false
	}

	for _, a := range c.Args(h) {
		if a.Contains(h, u) {
			return true
		}
	}

	return false
}

// RenamedCopy creates a copy of a term with fresh variables.
func (t Term) RenamedCopy(h *Heap) (Term, error) {
	return renamedCopy(h, t, map[Term]Term{})
}

func renamedCopy(h *Heap, t Term, copied map[Term]Term) (Term, error) {
	t = t.resolve(h)

	if c, ok := copied[t]; ok {
		return c, nil
	}

	if _, err := t.Variable(h); err == nil {
		c, err := NewVariable(h)
		if err != nil {
			return Term{}, err
		}
		copied[t] = c
		return c, nil
	}

	if c, err := t.Compound(h); err == nil {
		cs := make([]Term, c.Arity)
		for i, a := range c.Args(h) {
			c, err := renamedCopy(h, a, copied)
			if err != nil {
				return Term{}, err
			}
			cs[i] = c
		}
		c, err := NewCompound(h, c.Name, cs...)
		if err != nil {
			return Term{}, err
		}
		copied[t] = c
		return c, nil
	}

	return t, nil
}

// Cyclic returns true if the term is cyclic.
func (t Term) Cyclic(h *Heap) bool {
	return cyclic(h, t, map[Term]struct{}{})
}

func cyclic(h *Heap, t Term, visited map[Term]struct{}) bool {
	t = t.resolve(h)

	if _, ok := visited[t]; ok {
		return true
	}
	visited[t] = struct{}{}

	c, err := t.Compound(h)
	if err != nil {
		return false
	}

	for _, a := range c.Args(h) {
		if cyclic(h, a, visited) {
			return true
		}
	}

	return false
}

// Unqualify returns qualifying module and unqualified term.
func (t Term) Unqualify(h *Heap, module string) (qualifyingModule string, unqualifiedTerm Term) {
	c, err := t.Compound(h)
	if err != nil {
		return module, t
	}

	if c.Functor != (Functor{Name: ":", Arity: 2}) {
		return module, t
	}

	mm, tt := c.Arg(h, 0), c.Arg(h, 1)

	m, err := mm.Atom(h)
	if err != nil {
		return module, t
	}

	return tt.Unqualify(h, m)
}

// Compare compares two Terms.
func (t Term) Compare(h *Heap, u Term) int {
	x, y := t.resolve(h), u.resolve(h)

	if x == y {
		return 0
	}

	if vx, err := x.Variable(h); err == nil {
		if vy, err := y.Variable(h); err == nil {
			return int(vx) - int(vy)
		}

		return -1
	}

	if x, err := x.Float(h); err == nil {
		if _, err := y.Variable(h); err == nil {
			return 1
		}

		if y, err := y.Float(h); err == nil {
			switch {
			case x > y:
				return 1
			case x < y:
				return -1
			default:
				return 0
			}
		}

		return -1
	}

	if x, err := x.Integer(h); err == nil {
		if _, err := y.Variable(h); err == nil {
			return 1
		}

		if _, err := y.Float(h); err == nil {
			return 1
		}

		if y, err := y.Integer(h); err == nil {
			switch {
			case x > y:
				return 1
			case x < y:
				return -1
			default:
				return 0
			}
		}

		return -1
	}

	if x, err := x.Atom(h); err == nil {
		if _, err := y.Variable(h); err == nil {
			return 1
		}

		if _, err := y.Float(h); err == nil {
			return 1
		}

		if _, err := y.Integer(h); err == nil {
			return 1
		}

		if y, err := y.Atom(h); err == nil {
			return strings.Compare(x, y)
		}

		return -1
	}

	cx, _ := x.Compound(h)
	cy, err := y.Compound(h)
	if err != nil {
		return 1
	}

	if o := cx.Arity - cy.Arity; o != 0 {
		return o
	}

	if o := strings.Compare(cx.Name, cy.Name); o != 0 {
		return o
	}

	for i := range cx.Arity {
		ax, ay := cx.Arg(h, i), cy.Arg(h, i)

		if o := ax.Compare(h, ay); o != 0 {
			return o
		}
	}

	return 0
}

type Variable int32

type env struct {
	lastVariable Variable
	Values       rbtree.Map[Variable, Term]
}

func (e *env) Generate() (Variable, error) {
	if e.lastVariable == math.MaxInt32 {
		return 0, &ResourceError{Resource: "variables"}
	}
	e.lastVariable++
	return e.lastVariable, nil
}

type atomID int32

type atomTable struct {
	IDs   rbtree.Map[string, atomID]
	Names []string
}

func (a *atomTable) Put(name string) (atomID, error) {
	if id, ok := a.IDs.Get(name); ok {
		return id, nil
	}

	id := atomID(len(a.Names))
	if err := a.IDs.Set(name, id); err != nil {
		return 0, err
	}

	var ok bool
	a.Names, ok = cappend(a.Names, name)
	if !ok {
		return 0, &ResourceError{Resource: "atoms"}
	}

	return id, nil
}

type stringID int32

type stringPool []stringEntry

type stringEntry struct {
	offset stringID
	string string
	tail   Term
}

func (s *stringPool) Put(str string, tail Term) (stringID, error) {
	e := stringEntry{
		string: str,
		tail:   tail,
	}
	if len(*s) > 0 {
		last := (*s)[len(*s)-1]
		e.offset = last.offset + stringID(len(last.string))
	}

	var ok bool
	*s, ok = cappend(*s, e)
	if !ok {
		return 0, &ResourceError{Resource: "strings"}
	}
	return e.offset, nil
}

func (s *stringPool) First(id stringID) Term {
	i := slices.IndexFunc(*s, func(e stringEntry) bool {
		return e.offset+stringID(len(e.string)) > id
	})
	if i == -1 {
		return Term{}
	}
	e := (*s)[i]
	str := e.string[id-e.offset:]
	r, _ := utf8.DecodeRuneInString(str)
	return Term{tag: termTagCharacter, payload: r}
}

func (s *stringPool) Rest(id stringID) Term {
	i := slices.IndexFunc(*s, func(e stringEntry) bool {
		return e.offset+stringID(len(e.string)) > id
	})
	if i == -1 {
		return Term{}
	}
	e := (*s)[i]
	str := e.string[id-e.offset:]
	_, n := utf8.DecodeRuneInString(str)
	if str[n:] == "" {
		return e.tail
	}
	return Term{tag: termTagString, payload: int32(int(id) + n)}
}
