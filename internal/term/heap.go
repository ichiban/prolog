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
type Heap []word

func (h *Heap) String() string {
	var sb strings.Builder
	for i, w := range *h {
		c := unpack(w)
		_, _ = fmt.Fprintf(&sb, "%4d: %s\n", i, c)
	}
	return sb.String()
}

func (h *Heap) putFunctor(f Functor) (Handle, error) {
	addr, err := h.put(pack(cell{tag: cellTagFunctor, value: int32(f)}))
	if err != nil {
		return Handle{}, err
	}
	return Handle{
		cell: cell{tag: cellTagStructure, value: int32(addr)},
	}, nil
}

func (h *Heap) putTerms(terms ...Handle) (int, error) {
	words := make([]word, len(terms))
	for i, t := range terms {
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
