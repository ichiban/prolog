package term

import (
	"iter"
	"math"
	"slices"
	"strings"
	"unicode/utf8"
	"unsafe"
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
}

// PutVariable creates a variable term and returns its reference.
func (a *Arena) PutVariable() (Handle, error) {
	addr := int32(len(a.Heap))
	c := cell{tag: cellTagReference, value: addr}
	if _, err := a.put(pack(c)); err != nil {
		return Handle{}, err
	}
	return Handle{
		cell: c,
	}, nil
}

// Variable returns the address it points to if it's a variable term.
func (a *Arena) Variable(t Handle) (int, bool) {
	if t.cell.tag != cellTagReference {
		return 0, false
	}
	return int(t.cell.value), true
}

func (a *Arena) Deref(x Handle) Handle {
	var (
		current = x.cell
		prev    cell
	)
	for current.tag == cellTagReference && current != prev {
		prev, current = current, unpack(a.Heap[current.value])
	}
	return Handle{
		cell: current,
	}
}

// Bind binds a variable term to another term.
func (a *Arena) Bind(x, t Handle) error {
	if x == t {
		return nil
	}

	if x.cell.tag != cellTagReference || unpack(a.Heap[x.cell.value]) != x.cell {
		return ErrUnsupportedOperation
	}

	a.Heap[x.cell.value] = pack(t.cell)
	return nil
}

// PutAtom creates an atom term and returns its address.
func (a *Arena) PutAtom(atom Atom) (Handle, error) {
	c := cell{value: atom.value}
	switch atom.kind {
	case atomKindRune:
		c.tag = cellTagCharacter
	case atomKindID:
		c.tag = cellTagAtom
	default:
		c.tag = cellTagInvalid
	}
	return Handle{cell: c}, nil
}

// Atom returns the atom if it's an atom term.
func (a *Arena) Atom(t Handle) (Atom, bool) {
	switch t.cell.tag {
	case cellTagCharacter:
		return Atom{kind: atomKindRune, value: t.cell.value}, true
	case cellTagAtom:
		return Atom{kind: atomKindID, value: t.cell.value}, true
	default:
		return Atom{kind: atomKindInvalid}, false
	}
}

// PutInteger creates an integer term and returns its address.
func (a *Arena) PutInteger(n int64) (Handle, error) {
	// TODO: support bigger integers.
	if n >= math.MinInt32 && n <= math.MaxInt32 {
		return Handle{cell: cell{tag: cellTagInt32, value: int32(n)}}, nil
	}
	addr, err := a.put(cast[int64, word](n))
	if err != nil {
		return Handle{}, err
	}
	return Handle{
		cell: cell{tag: cellTagInt64, value: int32(addr)},
	}, nil
}

// Integer returns the integer if it's an integer term.
func (a *Arena) Integer(t Handle) (int64, bool) {
	switch t.cell.tag {
	case cellTagInt64:
		return cast[word, int64](a.Heap[t.cell.value]), true
	case cellTagInt32:
		return int64(t.cell.value), true
	default:
		return 0, false
	}
}

// PutFloat creates a float term and returns its address.
func (a *Arena) PutFloat(f float64) (Handle, error) {
	addr, err := a.put(cast[float64, word](f))
	if err != nil {
		return Handle{}, err
	}
	return Handle{
		cell: cell{tag: cellTagFloat, value: int32(addr)},
	}, nil
}

// Float returns a float value if it's a float term.
func (a *Arena) Float(t Handle) (float64, bool) {
	switch t.cell.tag {
	case cellTagFloat:
		return cast[word, float64](a.Heap[t.cell.value]), true
	default:
		return 0, false
	}
}

// PutCompound creates a compound term and returns its address.
func (a *Arena) PutCompound(name Atom, args ...Handle) (Handle, error) {
	if len(args) == 0 {
		return a.PutAtom(name)
	}

	f := NewFunctor(name, len(args))
	ret, err := a.putFunctor(f)
	if err != nil {
		return Handle{}, err
	}
	if _, err := a.putTerms(args...); err != nil {
		return Handle{}, err
	}
	return ret, nil
}

func (a *Arena) PutCompoundWithFreshVars(f Functor) (Handle, error) {
	ret, err := a.putFunctor(f)
	if err != nil {
		return Handle{}, err
	}
	for range f.Arity() {
		if _, err := a.PutVariable(); err != nil {
			return Handle{}, err
		}
	}
	return ret, nil
}

// PutList creates a series of compound terms for a list.
func (a *Arena) PutList(elems ...Handle) (Handle, error) {
	tail, _ := a.PutAtom(atomEmptyList) // Always succeeds.
	return a.PutPartialList(tail, elems...)
}

// PutPartialList creates a series of compound terms for a partial list with the specified tail term.
func (a *Arena) PutPartialList(tail Handle, elems ...Handle) (Handle, error) {
	if len(elems) == 0 {
		return tail, nil
	}

	// CDR coding
	addr := int32(len(a.Heap))
	for _, elem := range elems {
		if _, err := a.putFunctor(functorCons); err != nil {
			return Handle{}, err
		}
		if _, err := a.putTerms(elem); err != nil {
			return Handle{}, err
		}
	}
	if _, err := a.putTerms(tail); err != nil {
		return Handle{}, err
	}
	return Handle{
		cell: cell{tag: cellTagStructure, value: addr},
	}, nil
}

func (a *Arena) PutSpine(r Atom, elems ...Handle) (Handle, error) {
	switch len(elems) {
	case 0:
		return Handle{}, ErrUnsupportedOperation
	case 1:
		return elems[0], nil
	}

	// CDR coding
	cons := NewFunctor(r, 2)
	addr := int32(len(a.Heap))
	for _, elem := range elems[:len(elems)-1] {
		if _, err := a.putFunctor(cons); err != nil {
			return Handle{}, err
		}
		if _, err := a.putTerms(elem); err != nil {
			return Handle{}, err
		}
	}
	if _, err := a.putTerms(elems[len(elems)-1]); err != nil {
		return Handle{}, err
	}
	return Handle{
		cell: cell{tag: cellTagStructure, value: addr},
	}, nil
}

// PutCharList creates a list of single-character atoms.
func (a *Arena) PutCharList(str string) (Handle, error) {
	tail, _ := a.PutAtom(atomEmptyList) // Always succeeds.
	return a.PutPartialCharList(str, tail)
}

func (a *Arena) PutPartialCharList(str string, tail Handle) (Handle, error) {
	addr := int32(len(a.Heap))

	b := unsafe.Slice(unsafe.StringData(str), len(str))
	for chunk := range slices.Chunk(b, 8) {
		chunk = append(chunk, make([]byte, 8-len(chunk))...) // Fills with null chars.
		var val [8]uint8
		copy(val[:], chunk)
		if _, err := a.put(cast[[8]uint8, word](val)); err != nil {
			return Handle{}, err
		}
	}

	// Ensures null termination.
	// If the last cell is packed with characters, append a word of null characters.
	if len(b)%8 == 0 {
		if _, err := a.put(0); err != nil {
			return Handle{}, err
		}
	}

	if _, err := a.putTerms(tail); err != nil {
		return Handle{}, err
	}
	return Handle{
		cell: cell{tag: cellTagString0, value: addr},
	}, nil
}

// PutCodeList creates a list of single-character atoms.
func (a *Arena) PutCodeList(str string) (Handle, error) {
	tail, _ := a.PutAtom(atomEmptyList) // Always succeeds.
	return a.PutPartialCodeList(str, tail)
}

func (a *Arena) PutPartialCodeList(str string, tail Handle) (Handle, error) {
	// It's okay not to optimize this since CharList is the preferred representation of strings.
	elems := make([]Handle, 0, len(str))
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
func (a *Arena) Functor(t Handle, opts ...FunctorOption) (Functor, bool) {
	var opt FunctorOptions
	for _, o := range opts {
		o(&opt)
	}
	switch t.cell.tag {
	case cellTagStructure:
		f := unpack(a.Heap[t.cell.value])
		return Functor(f.value), true
	case cellTagString0, cellTagString1, cellTagString2, cellTagString3,
		cellTagString4, cellTagString5, cellTagString6, cellTagString7:
		return functorCons, true
	default:
		if atom, ok := a.Atom(t); ok && opt.allowAtom {
			return NewFunctor(atom, 0), true
		}
		return 0, false
	}
}

// Arg returns the n-th argument of the term.
func (a *Arena) Arg(t Handle, n int) Handle {
	switch t.cell.tag {
	case cellTagStructure:
		arg := unpack(a.Heap[int(t.cell.value)+1+n])
		if arg.tag == cellTagFunctor { // Possibly CDR coding.
			return Handle{
				cell: cell{tag: cellTagStructure, value: t.cell.value + 1 + int32(n)},
			}
		}
		return Handle{
			cell: arg,
		}
	case cellTagString0, cellTagString1, cellTagString2, cellTagString3, cellTagString4, cellTagString5, cellTagString6, cellTagString7:
		offset := int32(t.cell.tag - cellTagString0)
		b := castSlice[word, byte](a.Heap[t.cell.value:])[offset:]
		r, s := utf8.DecodeRune(b)
		switch n {
		case 0:
			return Handle{
				cell: cell{tag: cellTagCharacter, value: r},
			}
		case 1:
			if r, _ := utf8.DecodeRune(b[s:]); r == 0 { // tail
				return Handle{
					cell: unpack(a.Heap[t.cell.value+1]),
				}
			}
			offset += int32(s)
			return Handle{
				cell: cell{tag: cellTagString0 + cellTag(offset%8), value: t.cell.value + offset/8},
			}
		default:
			return Handle{}
		}
	default:
		return Handle{}
	}
}

func (a *Arena) Args(t Handle) iter.Seq[Handle] {
	return func(yield func(Handle) bool) {
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

func (a *Arena) WithArgs(t Handle, args ...Handle) (Handle, error) {
	f, ok := a.Functor(t)
	if !ok || f.Arity() != len(args) {
		return Handle{}, ErrUnsupportedOperation
	}
	existing := slices.Collect(a.Args(t))
	if slices.Equal(existing, args) {
		return t, nil
	}
	return a.PutCompound(f.Name(), args...)
}

// List returns an iterator iterates over the elements of a list.
func (a *Arena) List(t Handle, opts ...ListOption) iter.Seq2[Handle, bool] {
	var o ListOptions
	for _, opt := range opts {
		opt(&o)
	}

	// Brent's cycle detection algorithm
	var (
		tortoise Handle
		hare     = t
		power    = 1
		lam      = 1
	)
	return func(yield func(Handle, bool) bool) {
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
func (a *Arena) CharList(t Handle) (string, bool) {
	if t.cell.tag >= cellTagString0 && t.cell.tag <= cellTagString7 {
		offset := int32(t.cell.tag - cellTagString0)
		b := castSlice[word, byte](a.Heap[t.cell.value:])[offset:]
		l := slices.Index(b, 0)
		tail := Handle{
			cell: unpack(a.Heap[t.cell.value+(offset+int32(l))/8+1]),
		}
		if atom, ok := a.Atom(tail); !ok || atom != atomEmptyList {
			return "", false
		}
		return string(b[:l]), true
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

func (a *Arena) Compare(x, y Handle) int {
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
