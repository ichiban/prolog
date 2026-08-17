package term

import (
	"errors"
	"fmt"
)

var (
	ErrUnsupportedOperation = errors.New("unsupported operation")
)

// Handle is a reference to a term.
type Handle struct {
	cell
}

func (h *Handle) String() string {
	return h.cell.String()
}

type cell struct {
	tag   cellTag // TODO: NaN Boxing?
	flags uint8   // For GC.
	aux   uint16  // TODO: Use this for functor arity and string offset.
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
	cellTagStream
	cellTagString
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
	cellTagStream:    "stream",
	cellTagString:    "string",
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
	cellTagStream:    true,
	cellTagString:    true,
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
