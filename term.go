package prolog

import (
	"fmt"
	"iter"
	"slices"
	"strings"
	"unicode/utf8"
	"unsafe"
)

type Variable int32

type termTag uint8

const (
	termTagInvalid termTag = iota
	termTagReference
	termTagAtom
	termTagCharacter
	termTagInteger
	termTagFloat
	termTagStructure
	termTagFunctor
	termTagString0
	termTagString1
	termTagString2
	termTagString3
	termTagString4
	termTagString5
	termTagString6
	termTagString7
)

var termTagNames = map[termTag]string{
	termTagInvalid:   "invalid",
	termTagReference: "reference",
	termTagAtom:      "atom",
	termTagCharacter: "character",
	termTagInteger:   "integer",
	termTagFloat:     "float",
	termTagStructure: "structure",
	termTagFunctor:   "functor",
	termTagString0:   "string(0)",
	termTagString1:   "string(1)",
	termTagString2:   "string(2)",
	termTagString3:   "string(3)",
	termTagString4:   "string(4)",
	termTagString5:   "string(5)",
	termTagString6:   "string(6)",
	termTagString7:   "string(7)",
}

func (t termTag) String() string {
	return termTagNames[t]
}

type HeapConfig struct {
	MaxTerms int
	MaxAtoms int
}

var DefaultHeapConfig = HeapConfig{
	MaxTerms: 8 * 1024,
	MaxAtoms: 1024,
}

type word uint64

// Heap is a memory region to store Terms.
type Heap struct {
	terms []word
	atoms AtomTable
}

// NewHeap creates a heap with given bytes.
// Those bytes are distributed among several arrays of respective data types.
func NewHeap(config *HeapConfig) *Heap {
	if config == nil {
		config = &DefaultHeapConfig
	}

	h := Heap{
		terms: make([]word, 0, config.MaxTerms),
		atoms: AtomTable{
			ids:     make(map[Atom]int32, config.MaxAtoms),
			entries: make([]atomTableEntry, 0, config.MaxAtoms),
		},
	}

	return &h
}

func (h *Heap) Deref(t Term) Term {
	var prev Term
	for {
		if t.tag != termTagReference {
			return t
		}
		prev, t = t, cast[word, Term](h.terms[t.value])
		if t == prev {
			return prev
		}
	}
}

func (h *Heap) Unify(trail *[]Variable, x, y Term) bool {
	return h.unify(trail, x, y, false)
}

func (h *Heap) UnifyWithOccursCheck(trail *[]Variable, x, y Term) bool {
	return h.unify(trail, x, y, true)
}

func (h *Heap) unify(trail *[]Variable, x, y Term, occursCheck bool) bool {
	pdl := [][2]Term{{x, y}}
	for len(pdl) > 0 {
		var d [2]Term
		d, pdl = pdl[len(pdl)-1], pdl[:len(pdl)-1]
		d[0], d[1] = h.Deref(d[0]), h.Deref(d[1])
		if d[0] == d[1] {
			continue
		}
		if d[0].tag == termTagReference || d[1].tag == termTagReference {
			if !h.bind(trail, d[0], d[1], occursCheck) {
				return false
			}
			continue
		}
		f0, err := h.Functor(d[0])
		if err != nil {
			return false
		}
		f1, err := h.Functor(d[1])
		if err != nil {
			return false
		}
		if f0 != f1 {
			return false
		}
		for i := 0; i < f0.Arity; i++ {
			pdl = append(pdl, [2]Term{h.Arg(d[0], i), h.Arg(d[1], i)})
		}
	}
	return true
}

func (h *Heap) bind(trail *[]Variable, x, y Term, occursCheck bool) bool {
	if x.tag != termTagReference {
		if y.tag != termTagReference || x.value > y.value {
			return false
		}
		return h.bind(trail, y, x, occursCheck)
	}
	// TODO: occurs check
	h.terms[x.value] = cast[Term, word](y)
	*trail = append(*trail, Variable(x.value)) // TODO: Check for HB, H, and B.
	return true
}

func (h *Heap) UnwindTrail(trail []Variable) {
	for _, i := range trail {
		h.terms[i] = cast[Term, word](Term{tag: termTagReference, value: int32(i)})
	}
}

// Term is a Prolog datum.
// It may refer to another Term in Heap.
type Term struct {
	tag   termTag
	value int32
}

func init() {
	if unsafe.Sizeof(Term{}) != 8 {
		panic("term must be 8 bytes")
	}
}

func (t Term) String() string {
	return fmt.Sprintf("<%s %d>", t.tag, t.value)
}

func (h *Heap) put(words ...word) (int32, error) {
	if cap(h.terms)-len(h.terms) < len(words) {
		return 0, &ResourceError{Resource: "heap"}
	}
	id := int32(len(h.terms))
	h.terms = append(h.terms, words...)
	return id, nil
}

// PutVariable creates a variable term.
func (h *Heap) PutVariable() (Term, error) {
	id := int32(len(h.terms))
	t := Term{tag: termTagReference, value: id}
	if _, err := h.put(cast[Term, word](t)); err != nil {
		return Term{}, err
	}
	return t, nil
}

// Variable returns an error if it's not a variable term.
func (h *Heap) Variable(t Term) (Variable, error) {
	t = h.Deref(t)
	if t.tag != termTagReference {
		return 0, &UninstantiationError{Culprit: t}
	}
	if next := cast[word, Term](h.terms[t.value]); next != t {
		return 0, &UninstantiationError{Culprit: t}
	}
	return Variable(t.value), nil
}

// PutAtom creates an atom term.
func (h *Heap) PutAtom(name Atom) (Term, error) {
	// A one-char atom is just a rune.
	if r, n := utf8.DecodeLastRuneInString(string(name)); r != utf8.RuneError && n == len(name) {
		return Term{tag: termTagCharacter, value: r}, nil
	}

	id, err := h.atoms.Put(name)
	if err != nil {
		return Term{}, &ResourceError{Resource: "atom"}
	}
	return Term{tag: termTagAtom, value: id}, nil
}

// Atom returns the name if it's an atom term.
// Otherwise, it returns an error.
func (h *Heap) Atom(t Term) (Atom, error) {
	switch t := h.Deref(t); t.tag {
	case termTagReference:
		return "", ErrInstantiation
	case termTagCharacter:
		return Atom(t.value), nil
	case termTagAtom:
		return h.atoms.Get(t.value), nil
	default:
		return "", &TypeError{ValidType: "atom", Culprit: t}
	}
}

// Character returns the rune if it's a single-character atom term.
// Otherwise, it returns an error.
func (h *Heap) Character(t Term) (rune, error) {
	switch t := h.Deref(t); t.tag {
	case termTagReference:
		return 0, ErrInstantiation
	case termTagCharacter:
		return t.value, nil
	default:
		return 0, &TypeError{ValidType: "character", Culprit: t}
	}
}

// PutInteger creates an integer term.
func (h *Heap) PutInteger(n int64) (Term, error) {
	// TODO: optimize for smaller/bigger integers.
	id, err := h.put(cast[int64, word](n))
	if err != nil {
		return Term{}, err
	}
	return Term{tag: termTagInteger, value: id}, nil
}

// Integer returns the integer if it's an integer term.
// Otherwise, it returns an error.
func (h *Heap) Integer(t Term) (int64, error) {
	switch t := h.Deref(t); t.tag {
	case termTagReference:
		return 0, ErrInstantiation
	case termTagInteger:
		return cast[word, int64](h.terms[t.value]), nil
	default:
		return 0, &TypeError{ValidType: "integer", Culprit: t}
	}
}

// PutFloat creates a float term.
func (h *Heap) PutFloat(f float64) (Term, error) {
	id, err := h.put(cast[float64, word](f))
	if err != nil {
		return Term{}, err
	}
	return Term{tag: termTagFloat, value: id}, nil
}

// Float returns a float value if it's a float term.
// Otherwise, it returns an error.
func (h *Heap) Float(t Term) (float64, error) {
	switch t := h.Deref(t); t.tag {
	case termTagReference:
		return 0, ErrInstantiation
	case termTagFloat:
		return cast[word, float64](h.terms[t.value]), nil
	default:
		return 0, &TypeError{ValidType: "float", Culprit: t}
	}
}

func (h *Heap) putFunctor(name Atom, arity int) (Term, error) {
	n, err := h.PutAtom(name)
	if err != nil {
		return Term{}, err
	}
	a, err := h.PutInteger(int64(arity))
	if err != nil {
		return Term{}, err
	}
	id, err := h.put(cast[Term, word](n), cast[Term, word](a))
	if err != nil {
		return Term{}, err
	}
	return Term{tag: termTagFunctor, value: id}, nil
}

// PutCompound creates a compound term.
func (h *Heap) PutCompound(name Atom, args ...Term) (Term, error) {
	if len(args) == 0 {
		return h.PutAtom(name)
	}

	f, err := h.putFunctor(name, len(args))
	if err != nil {
		return Term{}, err
	}
	id, err := h.put(append([]word{cast[Term, word](f)}, cast[[]Term, []word](args)...)...)
	return Term{tag: termTagStructure, value: id}, err
}

// PutList creates a series of compound terms for a list.
func (h *Heap) PutList(elems ...Term) (Term, error) {
	tail, err := h.PutAtom("[]")
	if err != nil {
		return Term{}, err
	}
	return h.PutPartialList(tail, elems...)
}

// PutPartialList creates a series of compound terms for a partial list with the specified tail term.
func (h *Heap) PutPartialList(tail Term, elems ...Term) (Term, error) {
	if len(elems) == 0 {
		return tail, nil
	}

	cons, err := h.putFunctor(".", 2)
	if err != nil {
		return Term{}, err
	}

	// CDR coding
	id := int32(len(h.terms))
	for _, elem := range elems {
		if _, err := h.put(cast[Term, word](cons), cast[Term, word](elem)); err != nil {
			return Term{}, err
		}
	}
	if _, err := h.put(cast[Term, word](tail)); err != nil {
		return Term{}, err
	}
	return Term{tag: termTagStructure, value: id}, nil
}

// PutCharList creates a list of single-character atoms.
func (h *Heap) PutCharList(str string) (Term, error) {
	tail, err := h.PutAtom("[]")
	if err != nil {
		return Term{}, err
	}
	return h.PutPartialCharList(str, tail)
}

func (h *Heap) PutPartialCharList(str string, tail Term) (Term, error) {
	id := int32(len(h.terms))

	b := unsafe.Slice(unsafe.StringData(str), len(str))
	for chunk := range slices.Chunk(b, 8) {
		chunk = append(chunk, make([]byte, 8-len(chunk))...) // Fills with null chars.
		var val [8]uint8
		copy(val[:], chunk)
		if _, err := h.put(cast[[8]uint8, word](val)); err != nil {
			return Term{}, err
		}
	}

	// Ensures null termination.
	if len(b)%8 == 0 {
		if _, err := h.put(cast[uint64, word](0)); err != nil {
			return Term{}, err
		}
	}

	if _, err := h.put(cast[Term, word](tail)); err != nil {
		return Term{}, err
	}
	return Term{tag: termTagString0, value: id}, nil
}

// PutCodeList creates a list of single-character atoms.
func (h *Heap) PutCodeList(str string) (Term, error) {
	tail, err := h.PutAtom("[]")
	if err != nil {
		return Term{}, err
	}
	return h.PutPartialCodeList(str, tail)
}

func (h *Heap) PutPartialCodeList(str string, tail Term) (Term, error) {
	// It's okay not to optimise this since CharList is the preferred representation of strings.
	var elems []Term
	for _, r := range str {
		i, err := h.PutInteger(int64(r))
		if err != nil {
			return Term{}, err
		}
		elems = append(elems, i)
	}
	return h.PutPartialList(tail, elems...)
}

func (h *Heap) Functor(t Term) (Functor, error) {
	switch t := h.Deref(t); t.tag {
	case termTagReference:
		return Functor{}, ErrInstantiation
	case termTagStructure:
		f := cast[word, Term](h.terms[t.value])
		n, err := h.Atom(cast[word, Term](h.terms[f.value]))
		if err != nil {
			return Functor{}, err
		}
		a, err := h.Integer(cast[word, Term](h.terms[f.value+1]))
		if err != nil {
			return Functor{}, err
		}
		return Functor{Name: n, Arity: int(a)}, nil
	case termTagString0, termTagString1, termTagString2, termTagString3,
		termTagString4, termTagString5, termTagString6, termTagString7:
		return Functor{Name: ".", Arity: 2}, nil
	default:
		return Functor{}, &TypeError{ValidType: "compound", Culprit: t}
	}
}

func (h *Heap) Arg(t Term, n int) Term {
	switch t := h.Deref(t); t.tag {
	case termTagStructure:
		arg := cast[word, Term](h.terms[int(t.value)+1+n])
		if arg.tag == termTagFunctor { // Possibly CDR coding.
			return Term{tag: termTagStructure, value: t.value + 1 + int32(n)}
		}
		return arg
	case termTagString0, termTagString1, termTagString2, termTagString3, termTagString4, termTagString5, termTagString6, termTagString7:
		offset := int32(t.tag - termTagString0)
		b := castSlice[word, byte](h.terms[t.value:])[offset:]
		r, s := utf8.DecodeRune(b)
		switch n {
		case 0:
			return Term{tag: termTagCharacter, value: r}
		case 1:
			if r, _ := utf8.DecodeRune(b[s:]); r == 0 { // tail
				return cast[word, Term](h.terms[t.value+1])
			}
			offset += int32(s)
			return Term{tag: termTagString0 + termTag(offset%8), value: t.value + offset/8}
		default:
			return Term{tag: termTagInvalid}
		}
	default:
		return Term{tag: termTagInvalid}
	}
}

func (h *Heap) Args(t Term) iter.Seq[Term] {
	return func(yield func(Term) bool) {
		f, _ := h.Functor(t)
		for i := range f.Arity {
			if !yield(h.Arg(t, i)) {
				return
			}
		}
	}
}

// Functor is a Name with Arity.
type Functor struct {
	Name  Atom
	Arity int
}

func (f Functor) String() string {
	return fmt.Sprintf("%s/%d", f.Name, f.Arity)
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
func (h *Heap) List(t Term, opts ...ListOption) iter.Seq2[Term, error] {
	var o ListOptions
	for _, opt := range opts {
		opt(&o)
	}

	// Brent's cycle detection algorithm
	var (
		tortoise Term
		hare     = h.Deref(t)
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

			if _, err := h.Variable(hare); err == nil {
				if !o.allowPartial {
					_ = yield(hare, ErrInstantiation)
				}
				return
			}

			if a, err := h.Atom(hare); err == nil {
				if a != "[]" {
					_ = yield(hare, &TypeError{ValidType: "list", Culprit: t})
				}
				return
			}

			f, err := h.Functor(hare)
			if err != nil || f != (Functor{Name: ".", Arity: 2}) {
				_ = yield(hare, &TypeError{ValidType: "list", Culprit: t})
				return
			}

			if !yield(h.Arg(hare, 0), nil) {
				return
			}

			hare = h.Deref(h.Arg(hare, 1))
			lam++
		}
	}
}

// CharList returns a string if the term is a list of single-character atoms.
func (h *Heap) CharList(t Term) (string, error) {
	t = h.Deref(t)

	if t.tag >= termTagString0 && t.tag <= termTagString7 {
		offset := int32(t.tag - termTagString0)
		b := castSlice[word, byte](h.terms[t.value:])[offset:]
		l := slices.Index(b, 0)
		tail := cast[word, Term](h.terms[t.value+(offset+int32(l))/8+1])
		if a, err := h.Atom(tail); err != nil || a != "[]" {
			return "", &TypeError{ValidType: "list", Culprit: t}
		}
		return string(b[:l]), nil
	}

	var sb strings.Builder
	for elem, err := range h.List(t) {
		if err != nil {
			return "", err
		}

		c, err := h.Character(elem)
		if err != nil {
			return "", err
		}

		_, _ = sb.WriteRune(c)
	}
	return sb.String(), nil
}

// Contains returns true if one term contains another.
func (h *Heap) Contains(t1 Term, t2 Term) bool {
	t1, t2 = h.Deref(t1), h.Deref(t2)

	if t1 == t2 {
		return true
	}

	for a := range h.Args(t1) {
		if h.Contains(a, t2) {
			return true
		}
	}

	return false
}

// RenamedCopy creates a copy of a term with fresh variables.
func (h *Heap) RenamedCopy(t Term) (Term, error) {
	return renamedCopy(h, t, map[Term]Term{})
}

func renamedCopy(h *Heap, t Term, copied map[Term]Term) (Term, error) {
	t = h.Deref(t)

	if c, ok := copied[t]; ok {
		return c, nil
	}

	if _, err := h.Variable(t); err == nil {
		c, err := h.PutVariable()
		if err != nil {
			return Term{}, err
		}
		copied[t] = c
		return c, nil
	}

	if f, err := h.Functor(t); err == nil {
		cs := make([]Term, 0, f.Arity)
		for a := range h.Args(t) {
			c, err := renamedCopy(h, a, copied)
			if err != nil {
				return Term{}, err
			}
			cs = append(cs, c)
		}
		c, err := h.PutCompound(f.Name, cs...)
		if err != nil {
			return Term{}, err
		}
		copied[t] = c
		return c, nil
	}

	return t, nil
}

// Cyclic returns true if the term is cyclic.
func (h *Heap) Cyclic(t Term) bool {
	return cyclic(h, t, map[Term]struct{}{})
}

func cyclic(h *Heap, t Term, visited map[Term]struct{}) bool {
	t = h.Deref(t)

	if _, ok := visited[t]; ok {
		return true
	}
	visited[t] = struct{}{}

	for a := range h.Args(t) {
		if cyclic(h, a, visited) {
			return true
		}
	}

	return false
}

// Unqualify returns qualifying module and unqualified term.
func (h *Heap) Unqualify(t Term, module Atom) (qualifyingModule Atom, unqualifiedTerm Term) {
	f, err := h.Functor(t)
	if err != nil {
		return module, t
	}

	if f != (Functor{Name: ":", Arity: 2}) {
		return module, t
	}

	mm, tt := h.Arg(t, 0), h.Arg(t, 1)

	m, err := h.Atom(mm)
	if err != nil {
		return module, t
	}

	return h.Unqualify(tt, m)
}

// Compare compares two Terms.
func (h *Heap) Compare(t1 Term, t2 Term) int {
	x, y := h.Deref(t1), h.Deref(t2)

	if x == y {
		return 0
	}

	if x, err := h.Variable(x); err == nil {
		if y, err := h.Variable(y); err == nil {
			return int(x) - int(y)
		}

		return -1
	}

	if x, err := h.Float(x); err == nil {
		if _, err := h.Variable(y); err == nil {
			return 1
		}

		if y, err := h.Float(y); err == nil {
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

	if x, err := h.Integer(x); err == nil {
		if _, err := h.Variable(y); err == nil {
			return 1
		}

		if _, err := h.Float(y); err == nil {
			return 1
		}

		if y, err := h.Integer(y); err == nil {
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

	if x, err := h.Atom(x); err == nil {
		if _, err := h.Variable(y); err == nil {
			return 1
		}

		if _, err := h.Float(y); err == nil {
			return 1
		}

		if _, err := h.Integer(y); err == nil {
			return 1
		}

		if y, err := h.Atom(y); err == nil {
			return strings.Compare(string(x), string(y))
		}

		return -1
	}

	fx, _ := h.Functor(x)
	fy, err := h.Functor(y)
	if err != nil {
		return 1
	}

	if o := fx.Arity - fy.Arity; o != 0 {
		return o
	}

	if o := strings.Compare(string(fx.Name), string(fy.Name)); o != 0 {
		return o
	}

	for i := range fx.Arity {
		x, y := h.Arg(x, i), h.Arg(y, i)
		if o := h.Compare(x, y); o != 0 {
			return o
		}
	}

	return 0
}

func cast[F, T any](from F) T {
	return *(*T)(unsafe.Pointer(&from))
}

func castSlice[F, T any](from []F) []T {
	var (
		zeroF F
		zeroT T
		sizeF = unsafe.Sizeof(zeroF)
		sizeT = unsafe.Sizeof(zeroT)
		n     = sizeF / sizeT
		ptr   = unsafe.SliceData(from)
	)
	return unsafe.Slice((*T)(unsafe.Pointer(ptr)), n)
}
