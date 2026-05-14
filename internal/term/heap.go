package term

import (
	"errors"
	"fmt"
	"math"
	"slices"
	"strings"
	"unsafe"
)

var (
	ErrOutOfMemory        = errors.New("out of memory")
	ErrIncompatibleHandle = errors.New("incompatible handle")
)

// Heap is a memory arena where Prolog terms reside.
type Heap []word

func (h Heap) String() string {
	var sb strings.Builder
	for i, w := range h {
		c := unpack(w)
		_, _ = fmt.Fprintf(&sb, "%4d: %s\n", i, c)
	}
	return sb.String()
}

// PutVariable creates a variable term and returns its reference.
func (h *Heap) PutVariable() (Handle, error) {
	addr := int32(len(*h))
	c := cell{tag: cellTagReference, value: addr}
	if _, err := h.put(pack(c)); err != nil {
		return Handle{}, err
	}
	return Handle{
		heap: h,
		cell: c,
	}, nil
}

// PutAtom creates an atom term and returns its address.
func (h *Heap) PutAtom(a Atom) (Handle, error) {
	c := cell{value: a.value}
	switch a.kind {
	case atomKindRune:
		c.tag = cellTagCharacter
	case atomKindID:
		c.tag = cellTagAtom
	default:
		c.tag = cellTagInvalid
	}
	return Handle{cell: c}, nil
}

// PutInteger creates an integer term and returns its address.
func (h *Heap) PutInteger(n int64) (Handle, error) {
	// TODO: support bigger integers.
	if n >= math.MinInt32 && n <= math.MaxInt32 {
		return Handle{cell: cell{tag: cellTagInt32, value: int32(n)}}, nil
	}
	addr, err := h.put(cast[int64, word](n))
	if err != nil {
		return Handle{}, err
	}
	return Handle{
		heap: h,
		cell: cell{tag: cellTagInt64, value: int32(addr)},
	}, nil
}

// PutFloat creates a float term and returns its address.
func (h *Heap) PutFloat(f float64) (Handle, error) {
	addr, err := h.put(cast[float64, word](f))
	if err != nil {
		return Handle{}, err
	}
	return Handle{
		heap: h,
		cell: cell{tag: cellTagFloat, value: int32(addr)},
	}, nil
}

// PutCompound creates a compound term and returns its address.
func (h *Heap) PutCompound(name Atom, args ...Handle) (Handle, error) {
	if len(args) == 0 {
		return h.PutAtom(name)
	}

	f := NewFunctor(name, len(args))
	ret, err := h.putFunctor(f)
	if err != nil {
		return Handle{}, err
	}
	if _, err := h.putTerms(args...); err != nil {
		return Handle{}, err
	}
	return ret, nil
}

func (h *Heap) PutCompoundWithFreshVars(f Functor) (Handle, error) {
	ret, err := h.putFunctor(f)
	if err != nil {
		return Handle{}, err
	}
	for range f.Arity() {
		if _, err := h.PutVariable(); err != nil {
			return Handle{}, err
		}
	}
	return ret, nil
}

func (h *Heap) PutFunctor(f Functor) (Handle, error) {
	n, err := h.PutAtom(f.Name())
	if err != nil {
		return Handle{}, err
	}
	a, err := h.PutInteger(int64(f.Arity()))
	if err != nil {
		return Handle{}, err
	}
	return h.PutCompound(NewAtomRune('/'), n, a)
}

func (h *Heap) putFunctor(f Functor) (Handle, error) {
	addr, err := h.put(pack(cell{tag: cellTagFunctor, value: int32(f)}))
	if err != nil {
		return Handle{}, err
	}
	return Handle{
		heap: h,
		cell: cell{tag: cellTagStructure, value: int32(addr)},
	}, nil
}

// PutList creates a series of compound terms for a list.
func (h *Heap) PutList(elems ...Handle) (Handle, error) {
	tail, _ := h.PutAtom(atomEmptyList) // Always succeeds.
	return h.PutPartialList(tail, elems...)
}

// PutPartialList creates a series of compound terms for a partial list with the specified tail term.
func (h *Heap) PutPartialList(tail Handle, elems ...Handle) (Handle, error) {
	if len(elems) == 0 {
		return tail, nil
	}

	// CDR coding
	addr := int32(len(*h))
	for _, elem := range elems {
		if _, err := h.putFunctor(functorCons); err != nil {
			return Handle{}, err
		}
		if _, err := h.putTerms(elem); err != nil {
			return Handle{}, err
		}
	}
	if _, err := h.putTerms(tail); err != nil {
		return Handle{}, err
	}
	return Handle{
		heap: h,
		cell: cell{tag: cellTagStructure, value: addr},
	}, nil
}

func (h *Heap) PutSpine(r Atom, elems ...Handle) (Handle, error) {
	switch len(elems) {
	case 0:
		return Handle{}, ErrUnsupportedOperation
	case 1:
		return elems[0], nil
	}

	// CDR coding
	cons := NewFunctor(r, 2)
	addr := int32(len(*h))
	for _, elem := range elems[:len(elems)-1] {
		if _, err := h.putFunctor(cons); err != nil {
			return Handle{}, err
		}
		if _, err := h.putTerms(elem); err != nil {
			return Handle{}, err
		}
	}
	if _, err := h.putTerms(elems[len(elems)-1]); err != nil {
		return Handle{}, err
	}
	return Handle{
		heap: h,
		cell: cell{tag: cellTagStructure, value: addr},
	}, nil
}

// PutCharList creates a list of single-character atoms.
func (h *Heap) PutCharList(str string) (Handle, error) {
	tail, _ := h.PutAtom(atomEmptyList) // Always succeeds.
	return h.PutPartialCharList(str, tail)
}

func (h *Heap) PutPartialCharList(str string, tail Handle) (Handle, error) {
	addr := int32(len(*h))

	b := unsafe.Slice(unsafe.StringData(str), len(str))
	for chunk := range slices.Chunk(b, 8) {
		chunk = append(chunk, make([]byte, 8-len(chunk))...) // Fills with null chars.
		var val [8]uint8
		copy(val[:], chunk)
		if _, err := h.put(cast[[8]uint8, word](val)); err != nil {
			return Handle{}, err
		}
	}

	// Ensures null termination.
	// If the last cell is packed with characters, append a word of null characters.
	if len(b)%8 == 0 {
		if _, err := h.put(0); err != nil {
			return Handle{}, err
		}
	}

	if _, err := h.putTerms(tail); err != nil {
		return Handle{}, err
	}
	return Handle{
		heap: h,
		cell: cell{tag: cellTagString0, value: addr},
	}, nil
}

// PutCodeList creates a list of single-character atoms.
func (h *Heap) PutCodeList(str string) (Handle, error) {
	tail, _ := h.PutAtom(atomEmptyList) // Always succeeds.
	return h.PutPartialCodeList(str, tail)
}

func (h *Heap) PutPartialCodeList(str string, tail Handle) (Handle, error) {
	// It's okay not to optimize this since CharList is the preferred representation of strings.
	elems := make([]Handle, 0, len(str))
	for _, r := range str {
		i, _ := h.PutInteger(int64(r)) // Since a rune is int32, this always succeeds.
		elems = append(elems, i)
	}
	return h.PutPartialList(tail, elems...)
}

func (h *Heap) putTerms(terms ...Handle) (int, error) {
	words := make([]word, len(terms))
	for i, t := range terms {
		if t.heap != nil && t.heap != h {
			return 0, ErrIncompatibleHandle
		}
		words[i] = pack(t.cell)
	}
	return h.put(words...)
}

func (h *Heap) put(words ...word) (int, error) {
	if cap(*h)-len(*h) < len(words) {
		return 0, ErrOutOfMemory
	}
	addr := len(*h)
	*h = append(*h, words...)
	return addr, nil
}

type word uint64

func pack(c cell) word {
	return cast[cell, word](c)
}

func unpack(w word) cell {
	return cast[word, cell](w)
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
