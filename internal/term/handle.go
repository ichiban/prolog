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
