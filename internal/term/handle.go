package term

import (
	"errors"
	"fmt"
	"iter"
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
	return int(h.cell.value), h.cell.tag == cellTagReference
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
	if h.cell.tag != cellTagReference {
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

// Functor returns a functor value if it's a compound term.
func (h Handle) Functor() (Functor, bool) {
	switch h.cell.tag {
	case cellTagStructure:
		f := unpack((*h.heap)[h.cell.value])
		return Functor(f.value), true
	case cellTagString0, cellTagString1, cellTagString2, cellTagString3,
		cellTagString4, cellTagString5, cellTagString6, cellTagString7:
		return functorCons, true
	default:
		return 0, false
	}
}

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
				c := unpack((*h.heap)[h.cell.value+1])
				return Handle{
					heap: h.heap,
					cell: c,
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
		f, _ := h.Functor()
		for i := range f.Arity() {
			if !yield(h.Arg(i)) {
				return
			}
		}
	}
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
		hare     = h.Deref()
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
	h = h.Deref()

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
	return cellTagNames[t]
}

func (t cellTag) Immediate() bool {
	return cellTagImmediate[t]
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
