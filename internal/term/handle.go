package term

import (
	"errors"
	"fmt"
	"iter"
	"math"
	"slices"
	"strings"
	"unicode/utf8"
)

var (
	ErrUnsupportedOperation = errors.New("unsupported operation")
)

// Handle is a reference to a term.
type Handle struct {
	heap *Heap // Can be nil for an immediate cell.
	cell cell  // TODO: Might need to be an address?
}

// Variable returns the address it points to if it's a variable term.
func (h Handle) Variable() (int, bool) {
	if h.cell.tag != cellTagReference {
		return 0, false
	}
	return int(h.cell.value), true
}

func (h Handle) Deref() Handle {
	var (
		current = h.cell
		prev    cell
	)
	for current.tag == cellTagReference && current != prev {
		prev, current = current, unpack((*h.heap)[current.value])
	}
	ret := Handle{
		cell: current,
	}
	if !current.tag.Immediate() {
		ret.heap = h.heap
	}
	return ret
}

// Bind binds a variable term to another term.
func (h Handle) Bind(t Handle) error {
	if h == t {
		return nil
	}

	if h.cell.tag != cellTagReference || unpack((*h.heap)[h.cell.value]) != h.cell {
		return ErrUnsupportedOperation
	}

	if t.heap != nil && t.heap != h.heap {
		return ErrIncompatibleHandle
	}

	(*h.heap)[h.cell.value] = pack(t.cell)
	return nil
}

// Atom returns the atom if it's an atom term.
func (h Handle) Atom() (Atom, bool) {
	switch h.cell.tag {
	case cellTagCharacter:
		return Atom{kind: atomKindRune, value: h.cell.value}, true
	case cellTagAtom:
		return Atom{kind: atomKindID, value: h.cell.value}, true
	default:
		return Atom{kind: atomKindInvalid}, false
	}
}

// Integer returns the integer if it's an integer term.
func (h Handle) Integer() (int64, bool) {
	switch h.cell.tag {
	case cellTagInt64:
		return cast[word, int64]((*h.heap)[h.cell.value]), true
	case cellTagInt32:
		return int64(h.cell.value), true
	default:
		return 0, false
	}
}

// Float returns a float value if it's a float term.
func (h Handle) Float() (float64, bool) {
	switch h.cell.tag {
	case cellTagFloat:
		return cast[word, float64]((*h.heap)[h.cell.value]), true
	default:
		return 0, false
	}
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
func (h Handle) Functor(opts ...FunctorOption) (Functor, bool) {
	var opt FunctorOptions
	for _, o := range opts {
		o(&opt)
	}
	switch h.cell.tag {
	case cellTagStructure:
		f := unpack((*h.heap)[h.cell.value])
		return Functor(f.value), true
	case cellTagString0, cellTagString1, cellTagString2, cellTagString3,
		cellTagString4, cellTagString5, cellTagString6, cellTagString7:
		return functorCons, true
	default:
		if a, ok := h.Atom(); ok && opt.allowAtom {
			return NewFunctor(a, 0), true
		}
		return 0, false
	}
}

// Arg returns the n-th argument of the term.
func (h Handle) Arg(n int) Handle {
	switch h.cell.tag {
	case cellTagStructure:
		arg := unpack((*h.heap)[int(h.cell.value)+1+n])
		if arg.tag == cellTagFunctor { // Possibly CDR coding.
			return Handle{
				heap: h.heap,
				cell: cell{tag: cellTagStructure, value: h.cell.value + 1 + int32(n)},
			}
		}
		ret := Handle{
			cell: arg,
		}
		if !arg.tag.Immediate() {
			ret.heap = h.heap
		}
		return ret
	case cellTagString0, cellTagString1, cellTagString2, cellTagString3, cellTagString4, cellTagString5, cellTagString6, cellTagString7:
		offset := int32(h.cell.tag - cellTagString0)
		b := castSlice[word, byte]((*h.heap)[h.cell.value:])[offset:]
		r, s := utf8.DecodeRune(b)
		switch n {
		case 0:
			return Handle{
				cell: cell{tag: cellTagCharacter, value: r},
			}
		case 1:
			if r, _ := utf8.DecodeRune(b[s:]); r == 0 { // tail
				return Handle{
					heap: h.heap,
					cell: unpack((*h.heap)[h.cell.value+1]),
				}
			}
			offset += int32(s)
			return Handle{
				heap: h.heap,
				cell: cell{tag: cellTagString0 + cellTag(offset%8), value: h.cell.value + offset/8},
			}
		default:
			return Handle{}
		}
	default:
		return Handle{}
	}
}

func (h Handle) Args() iter.Seq[Handle] {
	return func(yield func(Handle) bool) {
		f, ok := h.Functor()
		if !ok {
			return
		}
		for i := range f.Arity() {
			if !yield(h.Arg(i)) {
				return
			}
		}
	}
}

func (h Handle) WithArgs(args ...Handle) (Handle, error) {
	f, ok := h.Functor()
	if !ok || f.Arity() != len(args) {
		return Handle{}, ErrUnsupportedOperation
	}
	existing := slices.Collect(h.Args())
	if slices.Equal(existing, args) {
		return h, nil
	}
	return h.heap.PutCompound(f.Name(), args...)
}

// List returns an iterator iterates over the elements of a list.
func (h Handle) List(opts ...ListOption) iter.Seq2[Handle, bool] {
	var o ListOptions
	for _, opt := range opts {
		opt(&o)
	}

	// Brent's cycle detection algorithm
	var (
		tortoise Handle
		hare     = h
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

			if _, ok := hare.Variable(); ok {
				if !o.allowPartial {
					_ = yield(hare, false)
				}
				return
			}

			if a, ok := hare.Atom(); ok {
				if a != atomEmptyList {
					_ = yield(hare, false)
				}
				return
			}

			f, ok := hare.Functor()
			if !ok || f != functorCons {
				_ = yield(hare, false)
				return
			}

			if !yield(hare.Arg(0), true) {
				return
			}

			hare = hare.Arg(1).Deref()
			lam++
		}
	}
}

// CharList returns a string if the term is a list of single-character atoms.
func (h Handle) CharList() (string, bool) {
	if h.cell.tag >= cellTagString0 && h.cell.tag <= cellTagString7 {
		offset := int32(h.cell.tag - cellTagString0)
		b := castSlice[word, byte]((*h.heap)[h.cell.value:])[offset:]
		l := slices.Index(b, 0)
		tail := Handle{
			cell: unpack((*h.heap)[h.cell.value+(offset+int32(l))/8+1]),
		}
		if a, ok := tail.Atom(); !ok || a != atomEmptyList {
			return "", false
		}
		return string(b[:l]), true
	}

	var sb strings.Builder
	for elem, ok := range h.List() {
		if !ok {
			return "", false
		}

		a, ok := elem.Atom()
		if !ok {
			return "", false
		}
		c := a.Rune()
		if c == utf8.RuneError {
			return "", false
		}
		_, _ = sb.WriteRune(c)
	}
	return sb.String(), true
}

type cell struct {
	tag   cellTag // TODO: NaN Boxing?
	value int32
}

func (c cell) String() string {
	return fmt.Sprintf("<%s %d>", c.tag, c.value)
}

type cellTag uint8

const (
	cellTagInvalid cellTag = iota
	cellTagReference
	cellTagAtom
	cellTagCharacter
	cellTagInt64
	cellTagInt32
	cellTagFloat
	cellTagStructure
	cellTagFunctor
	cellTagString0
	cellTagString1
	cellTagString2
	cellTagString3
	cellTagString4
	cellTagString5
	cellTagString6
	cellTagString7
	_cellTagLen
)

var cellTagNames = [...]string{
	cellTagInvalid:   "invalid",
	cellTagReference: "reference",
	cellTagAtom:      "atom",
	cellTagCharacter: "character",
	cellTagInt64:     "int64",
	cellTagInt32:     "int32",
	cellTagFloat:     "float",
	cellTagStructure: "structure",
	cellTagFunctor:   "functor",
	cellTagString0:   "string(0)",
	cellTagString1:   "string(1)",
	cellTagString2:   "string(2)",
	cellTagString3:   "string(3)",
	cellTagString4:   "string(4)",
	cellTagString5:   "string(5)",
	cellTagString6:   "string(6)",
	cellTagString7:   "string(7)",
}

var cellTagImmediate = [...]bool{
	cellTagInvalid:   true,
	cellTagReference: false,
	cellTagAtom:      true,
	cellTagCharacter: true,
	cellTagInt64:     false,
	cellTagInt32:     true,
	cellTagFloat:     false,
	cellTagStructure: false,
	cellTagFunctor:   true,
	cellTagString0:   false,
	cellTagString1:   false,
	cellTagString2:   false,
	cellTagString3:   false,
	cellTagString4:   false,
	cellTagString5:   false,
	cellTagString6:   false,
	cellTagString7:   false,
}

func (t cellTag) String() string {
	if 0 > t || t >= _cellTagLen {
		return "invalid"
	}
	return cellTagNames[t]
}

func (t cellTag) Immediate() bool {
	return int(t) < len(cellTagImmediate) && cellTagImmediate[t]
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

func Compare(x, y Handle) int {
	x, y = x.Deref(), y.Deref()
	if x == y {
		return 0
	}

	if x, ok := x.Variable(); ok {
		if y, ok := y.Variable(); ok {
			return x - y
		}
		return -1
	}

	if x, ok := x.Float(); ok {
		if _, ok := y.Variable(); ok {
			return 1
		}

		if y, ok := y.Float(); ok {
			o := x - y
			if o > 0 {
				return int(math.Ceil(o))
			}
			return int(math.Floor(o))
		}

		return -1
	}

	if x, ok := x.Integer(); ok {
		if _, ok := y.Variable(); ok {
			return 1
		}
		if _, ok := y.Float(); ok {
			return 1
		}
		if y, ok := y.Integer(); ok {
			return int(x - y)
		}
		return -1
	}

	if x, ok := x.Atom(); ok {
		if _, ok := y.Variable(); ok {
			return 1
		}
		if _, ok := y.Float(); ok {
			return 1
		}
		if _, ok := y.Integer(); ok {
			return 1
		}
		if y, ok := y.Atom(); ok {
			return strings.Compare(x.String(), y.String())
		}
		return -1
	}

	fx, _ := x.Functor()
	fy, ok := y.Functor()
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
		x, y := x.Arg(i), y.Arg(i)
		if o := Compare(x, y); o != 0 {
			return o
		}
	}

	return 0
}
