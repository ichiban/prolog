package term

import (
	"errors"
	"math"
	"slices"
	"unsafe"
)

var (
	ErrOutOfMemory        = errors.New("out of memory")
	ErrIncompatibleHandle = errors.New("incompatible handle")
)

// Heap is a memory arena where Prolog terms reside.
type Heap []word

// PutVariable creates a variable term and returns its reference.
func (h *Heap) PutVariable() (Handle, error) {
	addr := int32(len(*h))
	c := cell{tag: cellTagReference, value: addr}
	if _, err := h.put(pack(c)); err != nil {
		return Handle{}, err
	}
	return Handle{
		heap: h,
		cell: cell{tag: cellTagReference, value: addr},
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
	addr, err := h.put(pack(cell{tag: cellTagFunctor, value: int32(f)}))
	if err != nil {
		return Handle{}, err
	}
	for _, a := range args {
		if a.heap != nil && a.heap != h {
			return Handle{}, ErrIncompatibleHandle
		}
		if _, err := h.put(pack(a.cell)); err != nil {
			return Handle{}, err
		}
	}
	return Handle{
		heap: h,
		cell: cell{tag: cellTagStructure, value: int32(addr)},
	}, nil
}

// PutList creates a series of compound terms for a list.
func (h *Heap) PutList(elems ...Handle) (Handle, error) {
	tail, err := h.PutAtom(atomEmptyList)
	if err != nil {
		return Handle{}, err
	}
	return h.PutPartialList(tail, elems...)
}

// PutPartialList creates a series of compound terms for a partial list with the specified tail term.
func (h *Heap) PutPartialList(tail Handle, elems ...Handle) (Handle, error) {
	if len(elems) == 0 {
		return tail, nil
	}

	cons := functorCons

	// CDR coding
	addr := int32(len(*h))
	for _, elem := range elems {
		if elem.heap != nil && elem.heap != h {
			return Handle{}, ErrIncompatibleHandle
		}
		if _, err := h.put(pack(cell{tag: cellTagFunctor, value: int32(cons)}), pack(elem.cell)); err != nil {
			return Handle{}, err
		}
	}
	if tail.heap != nil && tail.heap != h {
		return Handle{}, ErrIncompatibleHandle
	}
	if _, err := h.put(pack(tail.cell)); err != nil {
		return Handle{}, err
	}
	return Handle{
		heap: h,
		cell: cell{tag: cellTagStructure, value: addr},
	}, nil
}

// PutCharList creates a list of single-character atoms.
func (h *Heap) PutCharList(str string) (Handle, error) {
	tail, err := h.PutAtom(atomEmptyList)
	if err != nil {
		return Handle{}, err
	}
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
	if len(b)%8 == 0 {
		if _, err := h.put(cast[uint64, word](0)); err != nil {
			return Handle{}, err
		}
	}

	if tail.heap != nil && tail.heap != h {
		return Handle{}, ErrIncompatibleHandle
	}
	if _, err := h.put(pack(tail.cell)); err != nil {
		return Handle{}, err
	}
	return Handle{
		heap: h,
		cell: cell{tag: cellTagString0, value: addr},
	}, nil
}

// PutCodeList creates a list of single-character atoms.
func (h *Heap) PutCodeList(str string) (Handle, error) {
	tail, err := h.PutAtom(atomEmptyList)
	if err != nil {
		return Handle{}, err
	}
	return h.PutPartialCodeList(str, tail)
}

func (h *Heap) PutPartialCodeList(str string, tail Handle) (Handle, error) {
	// It's okay not to optimize this since CharList is the preferred representation of strings.
	var elems []Handle
	for _, r := range str {
		i, err := h.PutInteger(int64(r))
		if err != nil {
			return Handle{}, err
		}
		elems = append(elems, i)
	}
	return h.PutPartialList(tail, elems...)
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
