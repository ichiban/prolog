package term

import (
	"errors"
	"fmt"
	"strings"
	"unsafe"
)

var (
	ErrOutOfMemory = errors.New("out of memory")
)

// Heap is a memory arena where Prolog terms reside.
type Heap []cell

func (h *Heap) String() string {
	var sb strings.Builder
	for i, c := range *h {
		_, _ = fmt.Fprintf(&sb, "%4d: %s\n", i, c)
	}
	return sb.String()
}

func (h *Heap) PutStructure(f Functor) (Handle, error) {
	tag := cellTagFunctor
	if f.name.kind == atomKindRune {
		tag = cellTagFunctorChar
	}
	addr, err := h.put(cell{tag: tag, value: f.name.value, aux: uint16(f.Arity())})
	if err != nil {
		return Handle{}, err
	}
	return Handle{
		cell: cell{tag: cellTagStructure, value: int32(addr)},
	}, nil
}

func (h *Heap) Put(terms ...Handle) (Handle, error) {
	cells := make([]cell, len(terms))
	for i, t := range terms {
		cells[i] = t.cell
	}
	addr, err := h.put(cells...)
	return Handle{cell: cell{tag: cellTagReference, value: int32(addr)}}, err
}

func (h *Heap) put(cells ...cell) (int, error) {
	if cap(*h)-len(*h) < len(cells) {
		return 0, ErrOutOfMemory
	}
	addr := len(*h)
	*h = append(*h, cells...)
	return addr, nil
}

// cast reinterprets the bits of from as a T. A cell is exactly as wide as an
// int64 or a float64 (see TestCell_size), so one heap slot holds one of them
// as a raw payload instead of a cell.
func cast[F, T any](from F) T {
	return *(*T)(unsafe.Pointer(&from))
}
