package term

import (
	"iter"
	"math"
	"slices"
	"strings"
	"unicode/utf8"

	"github.com/ichiban/prolog/v2/internal/side"
)

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

// Arena is a memory region allocated for terms.
// Terms live inside the arena. You're not allowed to operate them directly.
type Arena struct {
	Heap
	// TODO: Add a side-car table for big integers.
	Strings side.Table[String]
	Streams side.Table[*Stream]
}

// PutVariable creates a variable term and returns it.
func (a *Arena) PutVariable() (Cell, error) {
	addr := int32(len(a.Heap))
	c := Cell{tag: cellTagReference, value: addr}
	if _, err := a.put(c); err != nil {
		return Cell{}, err
	}
	return c, nil
}

// Variable returns the address it points to if it's a variable term.
func (a *Arena) Variable(t Cell) (int, bool) {
	if t.tag != cellTagReference {
		return 0, false
	}
	return int(t.value), true
}

func (a *Arena) Deref(x Cell) Cell {
	var (
		current = x
		prev    Cell
	)
	for current.tag == cellTagReference && current != prev {
		prev, current = current, a.Heap[current.value]
	}
	return current
}

// Bind binds a variable term to another term.
func (a *Arena) Bind(x, t Cell) error {
	if x == t {
		return nil
	}

	if x.tag != cellTagReference || a.Heap[x.value] != x {
		return ErrUnsupportedOperation
	}

	a.Heap[x.value] = t
	return nil
}

func (a *Arena) Unbind(x Cell) error {
	if x.tag != cellTagReference {
		return ErrUnsupportedOperation
	}
	a.Heap[x.value] = Cell{tag: cellTagReference, value: x.value}
	return nil
}

// PutAtom creates an atom term and returns it.
func (a *Arena) PutAtom(atom Atom) (Cell, error) {
	c := Cell{value: atom.value}
	switch atom.kind {
	case atomKindRune:
		c.tag = cellTagCharacter
	case atomKindID:
		c.tag = cellTagAtom
	default:
		c.tag = cellTagInvalid
	}
	return c, nil
}

// Atom returns the atom if it's an atom term.
func (a *Arena) Atom(t Cell) (Atom, bool) {
	switch t.tag {
	case cellTagCharacter:
		return Atom{kind: atomKindRune, value: t.value}, true
	case cellTagAtom:
		return Atom{kind: atomKindID, value: t.value}, true
	default:
		return Atom{kind: atomKindInvalid}, false
	}
}

// PutInteger creates an integer term and returns it.
func (a *Arena) PutInteger(n int64) (Cell, error) {
	// TODO: support bigger integers.
	if n >= math.MinInt32 && n <= math.MaxInt32 {
		return Cell{tag: cellTagInt32, value: int32(n)}, nil
	}
	addr, err := a.put(cast[int64, Cell](n))
	if err != nil {
		return Cell{}, err
	}
	return Cell{tag: cellTagInt64, value: int32(addr)}, nil
}

// Integer returns the integer if it's an integer term.
func (a *Arena) Integer(t Cell) (int64, bool) {
	switch t.tag {
	case cellTagInt64:
		return cast[Cell, int64](a.Heap[t.value]), true
	case cellTagInt32:
		return int64(t.value), true
	default:
		return 0, false
	}
}

// PutFloat creates a float term and returns it.
func (a *Arena) PutFloat(f float64) (Cell, error) {
	addr, err := a.put(cast[float64, Cell](f))
	if err != nil {
		return Cell{}, err
	}
	return Cell{tag: cellTagFloat, value: int32(addr)}, nil
}

// Float returns a float value if it's a float term.
func (a *Arena) Float(t Cell) (float64, bool) {
	switch t.tag {
	case cellTagFloat:
		return cast[Cell, float64](a.Heap[t.value]), true
	default:
		return 0, false
	}
}

// PutCompound creates a compound term and returns it.
func (a *Arena) PutCompound(name Atom, args ...Cell) (Cell, error) {
	if len(args) == 0 {
		return a.PutAtom(name)
	}

	f := NewFunctor(name, len(args))
	ret, err := a.PutStructure(f)
	if err != nil {
		return Cell{}, err
	}
	if _, err := a.Put(args...); err != nil {
		return Cell{}, err
	}
	return ret, nil
}

func (a *Arena) PutCompoundWithFreshVars(f Functor) (Cell, error) {
	if f.Arity() == 0 {
		return a.PutAtom(f.Name())
	}
	ret, err := a.PutStructure(f)
	if err != nil {
		return Cell{}, err
	}
	for range f.Arity() {
		if _, err := a.PutVariable(); err != nil {
			return Cell{}, err
		}
	}
	return ret, nil
}

func (a *Arena) PutFunctor(f Functor) (Cell, error) {
	n := f.Name()
	if n == (Atom{}) {
		// The zero Functor has no name. Render it as ''/0 so that it stays a
		// well-formed term; an invalid atom writes nothing at all.
		n = NewAtom("")
	}
	name, err := a.PutAtom(n)
	if err != nil {
		return Cell{}, err
	}
	arity, err := a.PutInteger(int64(f.Arity()))
	if err != nil {
		return Cell{}, err
	}
	return a.PutCompound(NewAtomRune('/'), name, arity)
}

// PutList creates a series of compound terms for a list.
func (a *Arena) PutList(elems ...Cell) (Cell, error) {
	tail, _ := a.PutAtom(atomEmptyList) // Always succeeds.
	return a.PutPartialList(tail, elems...)
}

// PutPartialList creates a series of compound terms for a partial list with the specified tail term.
func (a *Arena) PutPartialList(tail Cell, elems ...Cell) (Cell, error) {
	if len(elems) == 0 {
		return tail, nil
	}

	// CDR coding
	addr := int32(len(a.Heap))
	for _, elem := range elems {
		if _, err := a.PutStructure(functorCons); err != nil {
			return Cell{}, err
		}
		if _, err := a.Put(elem); err != nil {
			return Cell{}, err
		}
	}
	if _, err := a.Put(tail); err != nil {
		return Cell{}, err
	}
	return Cell{tag: cellTagStructure, value: addr}, nil
}

func (a *Arena) PutSpine(r Atom, elems ...Cell) (Cell, error) {
	switch len(elems) {
	case 0:
		return Cell{}, ErrUnsupportedOperation
	case 1:
		return elems[0], nil
	}

	// CDR coding
	cons := NewFunctor(r, 2)
	addr := int32(len(a.Heap))
	for _, elem := range elems[:len(elems)-1] {
		if _, err := a.PutStructure(cons); err != nil {
			return Cell{}, err
		}
		if _, err := a.Put(elem); err != nil {
			return Cell{}, err
		}
	}
	if _, err := a.Put(elems[len(elems)-1]); err != nil {
		return Cell{}, err
	}
	return Cell{tag: cellTagStructure, value: addr}, nil
}

// PutCharList creates a list of single-character atoms.
func (a *Arena) PutCharList(str string) (Cell, error) {
	tail, _ := a.PutAtom(atomEmptyList) // Always succeeds.
	return a.PutPartialCharList(str, tail)
}

func (a *Arena) PutPartialCharList(str string, tail Cell) (Cell, error) {
	if str == "" {
		return tail, nil
	}

	// Chunks str when the body length exceeds uint16.
	if l, r := splitByRuneCount(str, math.MaxUint16); r != "" {
		str = l
		var err error
		tail, err = a.PutPartialCharList(r, tail)
		if err != nil {
			return Cell{}, err
		}
	}

	strID := a.Strings.Add(String{Body: str, Tail: tail})

	return Cell{tag: cellTagString, value: int32(strID), aux: 0}, nil
}

func splitByRuneCount(str string, n int) (string, string) {
	c := 0
	for i := range str {
		c++
		if c == n {
			return str[:i], str[i:]
		}
	}
	return str, ""
}

// PutCodeList creates a list of single-character atoms.
func (a *Arena) PutCodeList(str string) (Cell, error) {
	tail, _ := a.PutAtom(atomEmptyList) // Always succeeds.
	return a.PutPartialCodeList(str, tail)
}

func (a *Arena) PutPartialCodeList(str string, tail Cell) (Cell, error) {
	// It's okay not to optimize this since CharList is the preferred representation of strings.
	elems := make([]Cell, 0, len(str))
	for _, r := range str {
		i, _ := a.PutInteger(int64(r)) // Since a rune is int32, this always succeeds.
		elems = append(elems, i)
	}
	return a.PutPartialList(tail, elems...)
}

type FunctorOptions struct {
	allowAtom bool
}

type FunctorOption func(*FunctorOptions)

func AllowAtom(ok bool) FunctorOption {
	return func(o *FunctorOptions) {
		o.allowAtom = ok
	}
}

// Functor returns a functor value if it's a compound term.
func (a *Arena) Functor(t Cell, opts ...FunctorOption) (Functor, bool) {
	var opt FunctorOptions
	for _, o := range opts {
		o(&opt)
	}
	switch t.tag {
	case cellTagStructure:
		f := a.Heap[t.value]
		kind := atomKindID
		if f.tag == cellTagFunctorChar {
			kind = atomKindRune
		}
		return Functor{
			name:  Atom{kind: kind, value: f.value},
			arity: int(f.aux),
		}, true
	case cellTagString:
		return functorCons, true
	default:
		if atom, ok := a.Atom(t); ok && opt.allowAtom {
			return NewFunctor(atom, 0), true
		}
		return Functor{}, false
	}
}

// Arg returns the n-th argument of the term.
func (a *Arena) Arg(t Cell, n int) Cell {
	switch t.tag {
	case cellTagStructure:
		arg := a.Heap[int(t.value)+1+n]
		if arg.tag == cellTagFunctor || arg.tag == cellTagFunctorChar { // Possibly CDR coding.
			return Cell{tag: cellTagStructure, value: t.value + 1 + int32(n)}
		}
		return arg
	case cellTagString:
		offset := t.aux
		str := a.Strings.Get(int(t.value))
		r, s := utf8.DecodeRuneInString(str.Body[offset:])
		switch n {
		case 0:
			return Cell{tag: cellTagCharacter, value: r}
		case 1:
			if r, s := utf8.DecodeRuneInString(str.Body[int(offset)+s:]); r == utf8.RuneError && s == 0 { // tail
				return str.Tail
			}
			offset += uint16(s)
			return Cell{tag: cellTagString, value: t.value, aux: offset}
		default:
			return Cell{}
		}
	default:
		return Cell{}
	}
}

func (a *Arena) Args(t Cell) iter.Seq[Cell] {
	return func(yield func(Cell) bool) {
		f, ok := a.Functor(t)
		if !ok {
			return
		}
		for i := range f.Arity() {
			if !yield(a.Arg(t, i)) {
				return
			}
		}
	}
}

func (a *Arena) WithArgs(t Cell, args ...Cell) (Cell, error) {
	f, ok := a.Functor(t)
	if !ok || f.Arity() != len(args) {
		return Cell{}, ErrUnsupportedOperation
	}
	existing := slices.Collect(a.Args(t))
	if slices.Equal(existing, args) {
		return t, nil
	}
	return a.PutCompound(f.Name(), args...)
}

// List returns an iterator iterates over the elements of a list.
func (a *Arena) List(t Cell, opts ...ListOption) iter.Seq2[Cell, bool] {
	var o ListOptions
	for _, opt := range opts {
		opt(&o)
	}

	// Brent's cycle detection algorithm
	var (
		tortoise Cell
		hare     = t
		power    = 1
		lam      = 1
	)
	return func(yield func(Cell, bool) bool) {
		for {
			if tortoise == hare && !o.allowCycle { // Detected a cycle.
				_ = yield(hare, false)
				return
			}

			if power == lam {
				tortoise = hare
				power *= 2
				lam = 0
			}

			if _, ok := a.Variable(hare); ok {
				if !o.allowPartial {
					_ = yield(hare, false)
				}
				return
			}

			if a, ok := a.Atom(hare); ok {
				if a != atomEmptyList {
					_ = yield(hare, false)
				}
				return
			}

			f, ok := a.Functor(hare)
			if !ok || f != functorCons {
				_ = yield(hare, false)
				return
			}

			if !yield(a.Arg(hare, 0), true) {
				return
			}

			hare = a.Arg(hare, 1)
			hare = a.Deref(hare)
			lam++
		}
	}
}

// CharList returns a string if the term is a list of single-character atoms.
func (a *Arena) CharList(t Cell) (string, bool) {
	if a, _ := a.Atom(t); a == atomEmptyList {
		return "", true
	}

	if t.tag == cellTagString {
		offset := t.aux
		str := a.Strings.Get(int(t.value))
		tail, ok := a.CharList(a.Deref(str.Tail))
		if !ok {
			return "", false
		}
		return str.Body[offset:] + tail, true
	}

	var sb strings.Builder
	for elem, ok := range a.List(t) {
		if !ok {
			return "", false
		}

		atom, ok := a.Atom(elem)
		if !ok {
			return "", false
		}
		c := atom.Rune()
		if c == utf8.RuneError {
			return "", false
		}
		_, _ = sb.WriteRune(c)
	}
	return sb.String(), true
}

func (a *Arena) PutStream(s Stream) (Cell, error) {
	id := a.Streams.Add(&s)
	return Cell{tag: cellTagStream, value: int32(id)}, nil
}

func (a *Arena) Stream(t Cell) (*Stream, bool) {
	if t.tag != cellTagStream {
		return nil, false
	}
	return a.Streams.Get(int(t.value)), true
}

func (a *Arena) OpenStreams() iter.Seq[Cell] {
	return func(yield func(Cell) bool) {
		for i, s := range a.Streams.All() {
			if s.Closed {
				continue
			}
			if !yield(Cell{tag: cellTagStream, value: int32(i)}) {
				return
			}
		}
	}
}

func (a *Arena) Compare(x, y Cell) int {
	x, y = a.Deref(x), a.Deref(y)
	if x == y {
		return 0
	}

	if x, ok := a.Variable(x); ok {
		if y, ok := a.Variable(y); ok {
			return x - y
		}
		return -1
	}

	if x, ok := a.Float(x); ok {
		if _, ok := a.Variable(y); ok {
			return 1
		}

		if y, ok := a.Float(y); ok {
			o := x - y
			if o > 0 {
				return int(math.Ceil(o))
			}
			return int(math.Floor(o))
		}

		return -1
	}

	if x, ok := a.Integer(x); ok {
		if _, ok := a.Variable(y); ok {
			return 1
		}
		if _, ok := a.Float(y); ok {
			return 1
		}
		if y, ok := a.Integer(y); ok {
			return int(x - y)
		}
		return -1
	}

	if x, ok := a.Atom(x); ok {
		if _, ok := a.Variable(y); ok {
			return 1
		}
		if _, ok := a.Float(y); ok {
			return 1
		}
		if _, ok := a.Integer(y); ok {
			return 1
		}
		if y, ok := a.Atom(y); ok {
			return strings.Compare(x.String(), y.String())
		}
		return -1
	}

	if _, ok := a.Stream(x); ok {
		if _, ok := a.Variable(y); ok {
			return 1
		}
		if _, ok := a.Float(y); ok {
			return 1
		}
		if _, ok := a.Integer(y); ok {
			return 1
		}
		if _, ok := a.Atom(y); ok {
			return 1
		}
		if _, ok := a.Stream(y); ok {
			return int(x.value - y.value)
		}
		return -1
	}

	fx, _ := a.Functor(x)
	fy, ok := a.Functor(y)
	if !ok {
		return 1
	}

	if o := fx.Arity() - fy.Arity(); o != 0 {
		return o
	}

	if o := strings.Compare(fx.Name().String(), fy.Name().String()); o != 0 {
		return o
	}

	for i := range fx.Arity() {
		x, y := a.Arg(x, i), a.Arg(y, i)
		if o := a.Compare(x, y); o != 0 {
			return o
		}
	}

	return 0
}

func (a *Arena) Acyclic(t Cell) bool {
	return !a.cyclic(t, map[Cell]struct{}{})
}

func (a *Arena) cyclic(t Cell, visited map[Cell]struct{}) bool {
	t = a.Deref(t)
	if _, ok := a.Functor(t); !ok {
		return false
	}
	if _, ok := visited[t]; ok {
		return true
	}
	visited[t] = struct{}{}
	for t := range a.Args(t) {
		if a.cyclic(t, visited) {
			return true
		}
	}
	return false
}

func RenamedCopy(from, to *Arena, t Cell) (Cell, error) {
	return renamedCopy(from, to, t, map[Cell]Cell{})
}

func renamedCopy(from, to *Arena, t Cell, copied map[Cell]Cell) (Cell, error) {
	t = from.Deref(t)
	if t, ok := copied[t]; ok {
		return t, nil
	}

	if _, ok := from.Stream(t); ok {
		return Cell{}, ErrUnsupportedOperation
	}

	if _, ok := from.Variable(t); ok {
		v, err := to.PutVariable()
		if err != nil {
			return Cell{}, err
		}
		copied[t] = v
		return v, nil
	}

	if from != to {
		if a, ok := from.Atom(t); ok {
			return to.PutAtom(a)
		}

		if i, ok := from.Integer(t); ok {
			return to.PutInteger(i)
		}

		if f, ok := from.Float(t); ok {
			return to.PutFloat(f)
		}
	}

	if s, ok := from.CharList(t); ok {
		return to.PutCharList(s)
	}

	// TODO: Specialize on list, partial list, and string.
	if f, ok := from.Functor(t); ok {
		args := make([]Cell, 0, f.Arity())
		for arg := range from.Args(t) {
			arg, err := renamedCopy(from, to, arg, copied)
			if err != nil {
				return Cell{}, err
			}
			args = append(args, arg)
		}
		c, err := to.PutCompound(f.Name(), args...)
		if err != nil {
			return Cell{}, err
		}

		copied[t] = c
		return c, nil
	}

	copied[t] = t
	return t, nil
}
