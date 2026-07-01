package wam

import (
	"fmt"
	"slices"
	"strings"

	"github.com/ichiban/prolog/v2/internal/syntax"
	"github.com/ichiban/prolog/v2/internal/term"
)

type Predicate struct {
	Offset int
}

type FirstArgKey struct {
	PI    term.Functor
	Term  term.Handle
	Arity int
}

// Image is a compiled image of Prolog texts/modules.
type Image struct {
	Predicates    map[term.Functor]Predicate // TODO: module?
	FirstArgIndex map[FirstArgKey]int        // TODO: module?

	// Code is a sequence of BinWAM instructions.
	// Its operand may refer to sidecar tables Constants or Functors.
	// This design choice, instead of holding the value inline, is because Go doesn't support union types.
	Code      []Instruction
	Constants []term.Handle
	Functors  []term.Functor
}

func (i *Image) EmbedConstants(t term.Handle) int {
	if j := slices.Index(i.Constants, t); j >= 0 {
		return j
	}
	i.Constants = append(i.Constants, t)
	return len(i.Constants) - 1
}

func (i *Image) EmbedFunctor(f term.Functor) int {
	if j := slices.Index(i.Functors, f); j >= 0 {
		return j
	}
	i.Functors = append(i.Functors, f)
	return len(i.Functors) - 1
}

func (i *Image) String() string {
	labels := map[int]string{}
	for pi, p := range i.Predicates {
		labels[p.Offset] = pi.String() + ":"
	}
	for k, i := range i.FirstArgIndex {
		if k.Arity == 0 {
			labels[i] = fmt.Sprintf("(%s):", &syntax.Formatter{Term: k.Term})
		} else {
			labels[i] = fmt.Sprintf("(%s/%d):", &syntax.Formatter{Term: k.Term}, k.Arity)
		}
	}

	var sb strings.Builder
	for j, inst := range i.Code {
		l, _ := labels[j]
		_, _ = fmt.Fprintf(&sb, "%4d %16s %s", j, l, inst.Op)
		switch inst.Op {
		case OpWriteValue, OpWriteVariable:
			_, _ = fmt.Fprintf(&sb, " X%d\n", inst.I)
		case OpWriteConstant:
			_, _ = fmt.Fprintf(&sb, " %s\n", &syntax.Formatter{Term: i.Constants[inst.N]})
		case OpPutVariable, OpPutValue, OpGetVariable, OpGetValue, OpUnifyValue, OpUnifyVariable, OpMove:
			_, _ = fmt.Fprintf(&sb, " X%d, A%d\n", inst.N, inst.I)
		case OpPutStructure, OpGetStructure, OpPushStructure:
			_, _ = fmt.Fprintf(&sb, " %s, A%d\n", i.Functors[inst.N], inst.I)
		case OpPutConstant, OpGetConstant, OpUnifyConstant:
			_, _ = fmt.Fprintf(&sb, " %s, A%d\n", &syntax.Formatter{Term: i.Constants[inst.N]}, inst.I)
		case OpExecute:
			_, _ = fmt.Fprintf(&sb, " %s\n", i.Functors[inst.N])
		case OpTryMeElse, OpRetryMeElse:
			_, _ = fmt.Fprintf(&sb, " %d\n", int(inst.N))
		case OpSwitch:
			_, _ = fmt.Fprintf(&sb, " %s\n", i.Functors[inst.N])
		default:
			_, _ = fmt.Fprintf(&sb, "\n")
		}
	}

	return sb.String()
}
