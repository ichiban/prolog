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

type word uint64

func (e *Engine) Deref(t Term) Term {
	var prev Term
	for {
		if t.tag != termTagReference {
			return t
		}
		prev, t = t, cast[word, Term](e.heap[t.value])
		if t == prev {
			return prev
		}
	}
}

type UnifyOptions struct {
	occursCheck bool
}

type UnifyOption func(*UnifyOptions)

func WithOccursCheck(check bool) UnifyOption {
	return func(o *UnifyOptions) {
		o.occursCheck = check
	}
}

func (e *Engine) Unify(trail *[]Variable, x, y Term, opts ...UnifyOption) bool {
	var o UnifyOptions
	for _, opt := range opts {
		opt(&o)
	}
	return e.unify(trail, x, y, &o)
}

func (e *Engine) unify(trail *[]Variable, x, y Term, o *UnifyOptions) bool {
	pdl := [][2]Term{{x, y}}
	for len(pdl) > 0 {
		var d [2]Term
		d, pdl = pdl[len(pdl)-1], pdl[:len(pdl)-1]
		d[0], d[1] = e.Deref(d[0]), e.Deref(d[1])
		if d[0] == d[1] {
			continue
		}
		if d[0].tag == termTagReference || d[1].tag == termTagReference {
			if !e.bind(trail, d[0], d[1], o.occursCheck) {
				return false
			}
			continue
		}
		f0, err := e.Functor(d[0])
		if err != nil {
			return false
		}
		f1, err := e.Functor(d[1])
		if err != nil {
			return false
		}
		if f0 != f1 {
			return false
		}
		for i := 0; i < f0.Arity; i++ {
			pdl = append(pdl, [2]Term{e.Arg(d[0], i), e.Arg(d[1], i)})
		}
	}
	return true
}

func (e *Engine) bind(trail *[]Variable, x, y Term, occursCheck bool) bool {
	if x.tag != termTagReference {
		if y.tag != termTagReference || x.value > y.value {
			return false
		}
		return e.bind(trail, y, x, occursCheck)
	}
	// TODO: occurs check
	e.heap[x.value] = cast[Term, word](y)
	*trail = append(*trail, Variable(x.value)) // TODO: Check for HB, H, and B.
	return true
}

func (e *Engine) UnwindTrail(trail []Variable) {
	for _, i := range trail {
		e.heap[i] = cast[Term, word](Term{tag: termTagReference, value: int32(i)})
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

func (e *Engine) put(words ...word) (int32, error) {
	if cap(e.heap)-len(e.heap) < len(words) {
		return 0, &ResourceError{Resource: "heap"}
	}
	id := int32(len(e.heap))
	e.heap = append(e.heap, words...)
	return id, nil
}

// PutVariable creates a variable term.
func (e *Engine) PutVariable() (Term, error) {
	id := int32(len(e.heap))
	t := Term{tag: termTagReference, value: id}
	if _, err := e.put(cast[Term, word](t)); err != nil {
		return Term{}, err
	}
	return t, nil
}

// Variable returns an error if it's not a variable term.
func (e *Engine) Variable(t Term) (Variable, error) {
	t = e.Deref(t)
	if t.tag != termTagReference {
		return 0, &UninstantiationError{Culprit: t}
	}
	if next := cast[word, Term](e.heap[t.value]); next != t {
		return 0, &UninstantiationError{Culprit: t}
	}
	return Variable(t.value), nil
}

// PutAtom creates an atom term.
func (e *Engine) PutAtom(name Atom) (Term, error) {
	// A one-char atom is just a rune.
	if r, n := utf8.DecodeLastRuneInString(string(name)); r != utf8.RuneError && n == len(name) {
		return Term{tag: termTagCharacter, value: r}, nil
	}

	id, err := e.atoms.Put(name)
	if err != nil {
		return Term{}, &ResourceError{Resource: "atom"}
	}
	return Term{tag: termTagAtom, value: id}, nil
}

// Atom returns the name if it's an atom term.
// Otherwise, it returns an error.
func (e *Engine) Atom(t Term) (Atom, error) {
	switch t := e.Deref(t); t.tag {
	case termTagReference:
		return "", ErrInstantiation
	case termTagCharacter:
		return Atom(t.value), nil
	case termTagAtom:
		return e.atoms.Get(t.value), nil
	default:
		return "", &TypeError{ValidType: "atom", Culprit: t}
	}
}

// Character returns the rune if it's a single-character atom term.
// Otherwise, it returns an error.
func (e *Engine) Character(t Term) (rune, error) {
	switch t := e.Deref(t); t.tag {
	case termTagReference:
		return 0, ErrInstantiation
	case termTagCharacter:
		return t.value, nil
	default:
		return 0, &TypeError{ValidType: "character", Culprit: t}
	}
}

// PutInteger creates an integer term.
func (e *Engine) PutInteger(n int64) (Term, error) {
	// TODO: optimize for smaller/bigger integers.
	id, err := e.put(cast[int64, word](n))
	if err != nil {
		return Term{}, err
	}
	return Term{tag: termTagInteger, value: id}, nil
}

// Integer returns the integer if it's an integer term.
// Otherwise, it returns an error.
func (e *Engine) Integer(t Term) (int64, error) {
	switch t := e.Deref(t); t.tag {
	case termTagReference:
		return 0, ErrInstantiation
	case termTagInteger:
		return cast[word, int64](e.heap[t.value]), nil
	default:
		return 0, &TypeError{ValidType: "integer", Culprit: t}
	}
}

// PutFloat creates a float term.
func (e *Engine) PutFloat(f float64) (Term, error) {
	id, err := e.put(cast[float64, word](f))
	if err != nil {
		return Term{}, err
	}
	return Term{tag: termTagFloat, value: id}, nil
}

// Float returns a float value if it's a float term.
// Otherwise, it returns an error.
func (e *Engine) Float(t Term) (float64, error) {
	switch t := e.Deref(t); t.tag {
	case termTagReference:
		return 0, ErrInstantiation
	case termTagFloat:
		return cast[word, float64](e.heap[t.value]), nil
	default:
		return 0, &TypeError{ValidType: "float", Culprit: t}
	}
}

func (e *Engine) putFunctor(name Atom, arity int) (Term, error) {
	n, err := e.PutAtom(name)
	if err != nil {
		return Term{}, err
	}
	a, err := e.PutInteger(int64(arity))
	if err != nil {
		return Term{}, err
	}
	id, err := e.put(cast[Term, word](n), cast[Term, word](a))
	if err != nil {
		return Term{}, err
	}
	return Term{tag: termTagFunctor, value: id}, nil
}

// PutCompound creates a compound term.
func (e *Engine) PutCompound(name Atom, args ...Term) (Term, error) {
	if len(args) == 0 {
		return e.PutAtom(name)
	}

	f, err := e.putFunctor(name, len(args))
	if err != nil {
		return Term{}, err
	}
	id, err := e.put(append([]word{cast[Term, word](f)}, cast[[]Term, []word](args)...)...)
	return Term{tag: termTagStructure, value: id}, err
}

// PutList creates a series of compound terms for a list.
func (e *Engine) PutList(elems ...Term) (Term, error) {
	tail, err := e.PutAtom("[]")
	if err != nil {
		return Term{}, err
	}
	return e.PutPartialList(tail, elems...)
}

// PutPartialList creates a series of compound terms for a partial list with the specified tail term.
func (e *Engine) PutPartialList(tail Term, elems ...Term) (Term, error) {
	if len(elems) == 0 {
		return tail, nil
	}

	cons, err := e.putFunctor(".", 2)
	if err != nil {
		return Term{}, err
	}

	// CDR coding
	id := int32(len(e.heap))
	for _, elem := range elems {
		if _, err := e.put(cast[Term, word](cons), cast[Term, word](elem)); err != nil {
			return Term{}, err
		}
	}
	if _, err := e.put(cast[Term, word](tail)); err != nil {
		return Term{}, err
	}
	return Term{tag: termTagStructure, value: id}, nil
}

// PutCharList creates a list of single-character atoms.
func (e *Engine) PutCharList(str string) (Term, error) {
	tail, err := e.PutAtom("[]")
	if err != nil {
		return Term{}, err
	}
	return e.PutPartialCharList(str, tail)
}

func (e *Engine) PutPartialCharList(str string, tail Term) (Term, error) {
	id := int32(len(e.heap))

	b := unsafe.Slice(unsafe.StringData(str), len(str))
	for chunk := range slices.Chunk(b, 8) {
		chunk = append(chunk, make([]byte, 8-len(chunk))...) // Fills with null chars.
		var val [8]uint8
		copy(val[:], chunk)
		if _, err := e.put(cast[[8]uint8, word](val)); err != nil {
			return Term{}, err
		}
	}

	// Ensures null termination.
	if len(b)%8 == 0 {
		if _, err := e.put(cast[uint64, word](0)); err != nil {
			return Term{}, err
		}
	}

	if _, err := e.put(cast[Term, word](tail)); err != nil {
		return Term{}, err
	}
	return Term{tag: termTagString0, value: id}, nil
}

// PutCodeList creates a list of single-character atoms.
func (e *Engine) PutCodeList(str string) (Term, error) {
	tail, err := e.PutAtom("[]")
	if err != nil {
		return Term{}, err
	}
	return e.PutPartialCodeList(str, tail)
}

func (e *Engine) PutPartialCodeList(str string, tail Term) (Term, error) {
	// It's okay not to optimise this since CharList is the preferred representation of strings.
	var elems []Term
	for _, r := range str {
		i, err := e.PutInteger(int64(r))
		if err != nil {
			return Term{}, err
		}
		elems = append(elems, i)
	}
	return e.PutPartialList(tail, elems...)
}

func (e *Engine) Functor(t Term) (Functor, error) {
	switch t := e.Deref(t); t.tag {
	case termTagReference:
		return Functor{}, ErrInstantiation
	case termTagStructure:
		f := cast[word, Term](e.heap[t.value])
		n, err := e.Atom(cast[word, Term](e.heap[f.value]))
		if err != nil {
			return Functor{}, err
		}
		a, err := e.Integer(cast[word, Term](e.heap[f.value+1]))
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

func (e *Engine) FunctorCallable(t Term) (Functor, error) {
	f, err := e.Functor(t)
	if err != nil {
		a, err := e.Atom(t)
		if err != nil {
			return Functor{}, &TypeError{ValidType: "callable", Culprit: t}
		}
		return Functor{Name: a, Arity: 0}, nil
	}
	return f, nil
}

func (e *Engine) Arg(t Term, n int) Term {
	switch t := e.Deref(t); t.tag {
	case termTagStructure:
		arg := cast[word, Term](e.heap[int(t.value)+1+n])
		if arg.tag == termTagFunctor { // Possibly CDR coding.
			return Term{tag: termTagStructure, value: t.value + 1 + int32(n)}
		}
		return arg
	case termTagString0, termTagString1, termTagString2, termTagString3, termTagString4, termTagString5, termTagString6, termTagString7:
		offset := int32(t.tag - termTagString0)
		b := castSlice[word, byte](e.heap[t.value:])[offset:]
		r, s := utf8.DecodeRune(b)
		switch n {
		case 0:
			return Term{tag: termTagCharacter, value: r}
		case 1:
			if r, _ := utf8.DecodeRune(b[s:]); r == 0 { // tail
				return cast[word, Term](e.heap[t.value+1])
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

func (e *Engine) Args(t Term) iter.Seq[Term] {
	return func(yield func(Term) bool) {
		f, _ := e.Functor(t)
		for i := range f.Arity {
			if !yield(e.Arg(t, i)) {
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
func AllowCycle(ok bool) ListOption {
	return func(opts *ListOptions) {
		opts.allowCycle = ok
	}
}

// AllowPartial configures the list iterator to allow partial lists.
func AllowPartial(ok bool) ListOption {
	return func(opts *ListOptions) {
		opts.allowPartial = ok
	}
}

// List returns an iterator iterates over the elements of a list.
func (e *Engine) List(t Term, opts ...ListOption) iter.Seq2[Term, error] {
	var o ListOptions
	for _, opt := range opts {
		opt(&o)
	}

	// Brent's cycle detection algorithm
	var (
		tortoise Term
		hare     = e.Deref(t)
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

			if _, err := e.Variable(hare); err == nil {
				if !o.allowPartial {
					_ = yield(hare, ErrInstantiation)
				}
				return
			}

			if a, err := e.Atom(hare); err == nil {
				if a != "[]" {
					_ = yield(hare, &TypeError{ValidType: "list", Culprit: t})
				}
				return
			}

			f, err := e.Functor(hare)
			if err != nil || f != (Functor{Name: ".", Arity: 2}) {
				_ = yield(hare, &TypeError{ValidType: "list", Culprit: t})
				return
			}

			if !yield(e.Arg(hare, 0), nil) {
				return
			}

			hare = e.Deref(e.Arg(hare, 1))
			lam++
		}
	}
}

// CharList returns a string if the term is a list of single-character atoms.
func (e *Engine) CharList(t Term) (string, error) {
	t = e.Deref(t)

	if t.tag >= termTagString0 && t.tag <= termTagString7 {
		offset := int32(t.tag - termTagString0)
		b := castSlice[word, byte](e.heap[t.value:])[offset:]
		l := slices.Index(b, 0)
		tail := cast[word, Term](e.heap[t.value+(offset+int32(l))/8+1])
		if a, err := e.Atom(tail); err != nil || a != "[]" {
			return "", &TypeError{ValidType: "list", Culprit: t}
		}
		return string(b[:l]), nil
	}

	var sb strings.Builder
	for elem, err := range e.List(t) {
		if err != nil {
			return "", err
		}

		c, err := e.Character(elem)
		if err != nil {
			return "", err
		}

		_, _ = sb.WriteRune(c)
	}
	return sb.String(), nil
}

// Contains returns true if one term contains another.
func (e *Engine) Contains(t1 Term, t2 Term) bool {
	t1, t2 = e.Deref(t1), e.Deref(t2)

	if t1 == t2 {
		return true
	}

	for a := range e.Args(t1) {
		if e.Contains(a, t2) {
			return true
		}
	}

	return false
}

// RenamedCopy creates a copy of a term with fresh variables.
func (e *Engine) RenamedCopy(t Term) (Term, error) {
	return renamedCopy(e, t, map[Term]Term{})
}

func renamedCopy(e *Engine, t Term, copied map[Term]Term) (Term, error) {
	t = e.Deref(t)

	if c, ok := copied[t]; ok {
		return c, nil
	}

	if _, err := e.Variable(t); err == nil {
		c, err := e.PutVariable()
		if err != nil {
			return Term{}, err
		}
		copied[t] = c
		return c, nil
	}

	if f, err := e.Functor(t); err == nil {
		cs := make([]Term, 0, f.Arity)
		for a := range e.Args(t) {
			c, err := renamedCopy(e, a, copied)
			if err != nil {
				return Term{}, err
			}
			cs = append(cs, c)
		}
		c, err := e.PutCompound(f.Name, cs...)
		if err != nil {
			return Term{}, err
		}
		copied[t] = c
		return c, nil
	}

	return t, nil
}

// Cyclic returns true if the term is cyclic.
func (e *Engine) Cyclic(t Term) bool {
	return cyclic(e, t, map[Term]struct{}{})
}

func cyclic(e *Engine, t Term, visited map[Term]struct{}) bool {
	t = e.Deref(t)

	if _, ok := visited[t]; ok {
		return true
	}
	visited[t] = struct{}{}

	for a := range e.Args(t) {
		if cyclic(e, a, visited) {
			return true
		}
	}

	return false
}

// Unqualify returns qualifying module and unqualified term.
func (e *Engine) Unqualify(t Term, module Atom) (qualifyingModule Atom, unqualifiedTerm Term) {
	f, err := e.Functor(t)
	if err != nil {
		return module, t
	}

	if f != (Functor{Name: ":", Arity: 2}) {
		return module, t
	}

	mm, tt := e.Arg(t, 0), e.Arg(t, 1)

	m, err := e.Atom(mm)
	if err != nil {
		return module, t
	}

	return e.Unqualify(tt, m)
}

// Compare compares two Terms.
func (e *Engine) Compare(t1 Term, t2 Term) int {
	x, y := e.Deref(t1), e.Deref(t2)

	if x == y {
		return 0
	}

	if x, err := e.Variable(x); err == nil {
		if y, err := e.Variable(y); err == nil {
			return int(x) - int(y)
		}

		return -1
	}

	if x, err := e.Float(x); err == nil {
		if _, err := e.Variable(y); err == nil {
			return 1
		}

		if y, err := e.Float(y); err == nil {
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

	if x, err := e.Integer(x); err == nil {
		if _, err := e.Variable(y); err == nil {
			return 1
		}

		if _, err := e.Float(y); err == nil {
			return 1
		}

		if y, err := e.Integer(y); err == nil {
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

	if x, err := e.Atom(x); err == nil {
		if _, err := e.Variable(y); err == nil {
			return 1
		}

		if _, err := e.Float(y); err == nil {
			return 1
		}

		if _, err := e.Integer(y); err == nil {
			return 1
		}

		if y, err := e.Atom(y); err == nil {
			return strings.Compare(string(x), string(y))
		}

		return -1
	}

	fx, _ := e.Functor(x)
	fy, err := e.Functor(y)
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
		x, y := e.Arg(x, i), e.Arg(y, i)
		if o := e.Compare(x, y); o != 0 {
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
