package prolog

import (
	"fmt"
	"strings"
)

const (
	Nop byte = iota
	Enter
	Call
	Exit
	Const
	Var
	Functor
	Pop
)

type Bytecode []byte

func (b Bytecode) String() string {
	ret := make([]string, 0, len(b))
	for i := 0; i < len(b); i++ {
		switch b[i] {
		case Nop:
			ret = append(ret, "nop")
		case Const:
			i++
			ret = append(ret, fmt.Sprintf("const %d", b[i]))
		case Var:
			i++
			ret = append(ret, fmt.Sprintf("var %d", b[i]))
		case Functor:
			i++
			ret = append(ret, fmt.Sprintf("functor %d", b[i]))
		case Pop:
			ret = append(ret, "pop")
		case Enter:
			ret = append(ret, "enter")
		case Call:
			i++
			ret = append(ret, fmt.Sprintf("call %d", b[i]))
		case Exit:
			ret = append(ret, "exit")
		default:
			ret = append(ret, fmt.Sprintf("unknown(%d)", b[i]))
		}
	}
	return strings.Join(ret, "; ")
}
