package wam

import (
	"fmt"
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

type Image struct {
	Predicates    map[term.Functor]Predicate
	FirstArgIndex map[FirstArgKey]int
	Code          []Instruction
	Constants     []term.Handle
	Functors      []term.Functor
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
		_, _ = fmt.Fprintf(&sb, "%16s %s", l, inst.Op)
		switch inst.Op {
		case OpPutVariable:
			_, _ = fmt.Fprintf(&sb, " X%d A%d\n", inst.N, inst.I)
		case OpPutStructure:
			_, _ = fmt.Fprintf(&sb, " %s A%d\n", i.Functors[inst.N], inst.I)
		case OpPutConstant:
			_, _ = fmt.Fprintf(&sb, "%s A%d\n", &syntax.Formatter{Term: i.Constants[inst.N]}, inst.I)
		case OpExecute:
			_, _ = fmt.Fprintf(&sb, " %s\n", i.Functors[inst.N])
		default:
			_, _ = fmt.Fprintf(&sb, "\n")
		}
	}

	return sb.String()
}
