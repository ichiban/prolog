package ir

import (
	"errors"
	"testing"

	"github.com/ichiban/prolog/v2/internal/syntax"
	"github.com/ichiban/prolog/v2/internal/term"
)

func TestClause_Compile(t *testing.T) {
	heap := make(term.Heap, 0, 1024)
	tests := []struct {
		title  string
		head   string
		body   string
		clause Clause
		err    error
	}{
		{
			title: "simplest",
			head:  `p(Cont).`,
			body:  `q(Cont).`,
			clause: Clause{
				PI:       term.NewFunctor(term.NewAtomRune('p'), 1),
				FirstArg: Index{Term: must(heap.PutAtom(term.NewAtomRune('_')))},
				MaxRegs:  1,
				Code:     []Instruction{},
				Execute:  term.NewFunctor(term.NewAtomRune('q'), 1),
			},
		},
		{
			title: "atomic in head",
			head:  `p(a, Cont).`,
			body:  `q(Cont).`,
			clause: Clause{
				PI:       term.NewFunctor(term.NewAtomRune('p'), 2),
				FirstArg: Index{Term: must(heap.PutAtom(term.NewAtomRune('a')))},
				MaxRegs:  2,
				Code: []Instruction{
					{OpCode: OpGet, Type: TypeConstant, A: Operand{Kind: OperandKindArgument, Index: 0}, B: Operand{Kind: OperandKindTerm, Term: must(heap.PutAtom(term.NewAtomRune('a')))}},
					{OpCode: OpGet, Type: TypeVariable, A: Operand{Kind: OperandKindArgument, Index: 1}, B: Operand{Kind: OperandKindRegister, Index: 0}},
				},
				Execute: term.NewFunctor(term.NewAtomRune('q'), 1),
			},
		},
		{
			title: "repeated head variable",
			head:  "p(X, X, Cont).",
			body:  "q(Cont).",
			clause: Clause{
				PI:       term.NewFunctor(term.NewAtomRune('p'), 3),
				FirstArg: Index{Term: must(heap.PutAtom(term.NewAtomRune('_')))},
				MaxRegs:  3,
				Code: []Instruction{
					{OpCode: OpGet, Type: TypeValue, A: Operand{Kind: OperandKindArgument, Index: 1}, B: Operand{Kind: OperandKindRegister, Index: 0}},
					{OpCode: OpGet, Type: TypeVariable, A: Operand{Kind: OperandKindArgument, Index: 2}, B: Operand{Kind: OperandKindRegister, Index: 0}},
				},
				Execute: term.NewFunctor(term.NewAtomRune('q'), 1),
			},
		},
		{
			title: "atomic in body",
			head:  "p(Cont).",
			body:  "q(a, Cont).",
			clause: Clause{
				PI:       term.NewFunctor(term.NewAtomRune('p'), 1),
				FirstArg: Index{Term: must(heap.PutAtom(term.NewAtomRune('_')))},
				MaxRegs:  2,
				Code: []Instruction{
					{OpCode: OpGet, Type: TypeVariable, A: Operand{Kind: OperandKindArgument, Index: 0}, B: Operand{Kind: OperandKindRegister, Index: 1}},
					{OpCode: OpPut, Type: TypeConstant, A: Operand{Kind: OperandKindArgument, Index: 0}, B: Operand{Kind: OperandKindTerm, Term: must(heap.PutAtom(term.NewAtomRune('a')))}},
				},
				Execute: term.NewFunctor(term.NewAtomRune('q'), 2),
			},
		},
		{
			title: "structure in head",
			head:  "p(f(X), Cont).",
			body:  "q(X, Cont).",
			clause: Clause{
				PI:       term.NewFunctor(term.NewAtomRune('p'), 2),
				FirstArg: Index{Term: must(heap.PutAtom(term.NewAtomRune('f'))), Arity: 1},
				MaxRegs:  2,
				Code: []Instruction{
					{OpCode: OpGet, Type: TypeStructure, A: Operand{Kind: OperandKindFunctor, Functor: term.NewFunctor(term.NewAtomRune('f'), 1)}, B: Operand{Kind: OperandKindRegister, Index: 0}},
					{OpCode: OpUnify, Type: TypeVariable, A: Operand{Kind: OperandKindGet}, B: Operand{Kind: OperandKindRegister, Index: 0}},
				},
				Execute: term.NewFunctor(term.NewAtomRune('q'), 2),
			},
		},
		{
			title: "structures in body",
			head:  "p(X, Cont).",
			body:  "q(f(X), Cont).",
			clause: Clause{
				PI:       term.NewFunctor(term.NewAtomRune('p'), 2),
				FirstArg: Index{Term: must(heap.PutAtom(term.NewAtomRune('_')))},
				MaxRegs:  3,
				Code: []Instruction{
					{OpCode: OpPut, Type: TypeStructure, A: Operand{Kind: OperandKindFunctor, Functor: term.NewFunctor(term.NewAtomRune('f'), 1)}, B: Operand{Kind: OperandKindRegister, Index: 2}},
					{OpCode: OpWrite, Type: TypeValue, A: Operand{Kind: OperandKindGet}, B: Operand{Kind: OperandKindRegister, Index: 0}},
					{OpCode: OpPut, Type: TypeValue, A: Operand{Kind: OperandKindArgument, Index: 0}, B: Operand{Kind: OperandKindRegister, Index: 2}},
				},
				Execute: term.NewFunctor(term.NewAtomRune('q'), 2),
			},
		},
		{
			title: "argument shuffling",
			head:  "p(X, Y, Cont).",
			body:  "q(Y, X, Cont).",
			clause: Clause{
				PI:       term.NewFunctor(term.NewAtomRune('p'), 3),
				FirstArg: Index{Term: must(heap.PutAtom(term.NewAtomRune('_')))},
				MaxRegs:  4,
				Code: []Instruction{
					{OpCode: OpGet, Type: TypeVariable, A: Operand{Kind: OperandKindArgument, Index: 0}, B: Operand{Kind: OperandKindRegister, Index: 3}},
					{OpCode: OpGet, Type: TypeVariable, A: Operand{Kind: OperandKindArgument, Index: 1}, B: Operand{Kind: OperandKindRegister, Index: 0}},
					{OpCode: OpPut, Type: TypeValue, A: Operand{Kind: OperandKindArgument, Index: 1}, B: Operand{Kind: OperandKindRegister, Index: 3}},
				},
				Execute: term.NewFunctor(term.NewAtomRune('q'), 3),
			},
		},
		{
			title: "nested structure in head",
			head:  "p(f(g(X)), Cont).",
			body:  "q(X, Cont).",
			clause: Clause{
				PI:       term.NewFunctor(term.NewAtomRune('p'), 2),
				FirstArg: Index{Term: must(heap.PutAtom(term.NewAtomRune('f'))), Arity: 1},
				MaxRegs:  3,
				Code: []Instruction{
					{OpCode: OpGet, Type: TypeStructure, A: Operand{Kind: OperandKindFunctor, Functor: term.NewFunctor(term.NewAtomRune('f'), 1)}, B: Operand{Kind: OperandKindRegister, Index: 0}},
					{OpCode: OpUnify, Type: TypeVariable, A: Operand{Kind: OperandKindGet}, B: Operand{Kind: OperandKindRegister, Index: 2}},
					{OpCode: OpGet, Type: TypeStructure, A: Operand{Kind: OperandKindFunctor, Functor: term.NewFunctor(term.NewAtomRune('g'), 1)}, B: Operand{Kind: OperandKindRegister, Index: 2}},
					{OpCode: OpUnify, Type: TypeVariable, A: Operand{Kind: OperandKindGet}, B: Operand{Kind: OperandKindRegister, Index: 0}},
				},
				Execute: term.NewFunctor(term.NewAtomRune('q'), 2),
			},
		},
		{
			title: "nested structure in body",
			head:  "p(X, Cont).",
			body:  "q(f(g(X)), Cont).",
			clause: Clause{
				PI:       term.NewFunctor(term.NewAtomRune('p'), 2),
				FirstArg: Index{Term: must(heap.PutAtom(term.NewAtomRune('_')))},
				MaxRegs:  4,
				Code: []Instruction{
					{OpCode: OpPut, Type: TypeStructure, A: Operand{Kind: OperandKindFunctor, Functor: term.NewFunctor(term.NewAtomRune('f'), 1)}, B: Operand{Kind: OperandKindRegister, Index: 2}},
					{OpCode: OpPush, Type: TypeVariable, A: Operand{Kind: OperandKindGet}, B: Operand{Kind: OperandKindRegister, Index: 3}},
					{OpCode: OpPush, Type: TypeStructure, A: Operand{Kind: OperandKindFunctor, Functor: term.NewFunctor(term.NewAtomRune('g'), 1)}, B: Operand{Kind: OperandKindRegister, Index: 3}},
					{OpCode: OpWrite, Type: TypeValue, A: Operand{Kind: OperandKindGet}, B: Operand{Kind: OperandKindRegister, Index: 0}},
					{OpCode: OpPut, Type: TypeValue, A: Operand{Kind: OperandKindArgument, Index: 0}, B: Operand{Kind: OperandKindRegister, Index: 2}},
				},
				Execute: term.NewFunctor(term.NewAtomRune('q'), 2),
			},
		},
		{
			title: "cut",
			head:  "p(Cont).",
			body:  "'$cut_to'('$cut', q(Cont)).",
			clause: Clause{
				PI:       term.NewFunctor(term.NewAtomRune('p'), 1),
				FirstArg: Index{Term: must(heap.PutAtom(term.NewAtomRune('_')))},
				MaxRegs:  2,
				Code: []Instruction{
					{OpCode: OpPut, Type: TypeConstant, A: Operand{Kind: OperandKindCutArg, Index: 1}, B: Operand{Kind: OperandKindTerm, Term: must(heap.PutAtom(term.NewAtom("$cut")))}},
				},
				Execute: term.NewFunctor(term.NewAtomRune('q'), 1),
			},
		},
		{
			title: "equal",
			head:  "p(X, Cont).",
			body:  "'='(X, a, q(Cont)).",
			clause: Clause{
				PI:       term.NewFunctor(term.NewAtomRune('p'), 2),
				FirstArg: Index{Term: must(heap.PutAtom(term.NewAtomRune('_')))},
				MaxRegs:  3,
				Code: []Instruction{
					{OpCode: OpPut, Type: TypeValue, A: Operand{Kind: OperandKindTemp, Index: 0}, B: Operand{Kind: OperandKindRegister, Index: 0}},
					{OpCode: OpGet, Type: TypeConstant, A: Operand{Kind: OperandKindTemp, Index: 0}, B: Operand{Kind: OperandKindTerm, Term: must(heap.PutAtom(term.NewAtomRune('a')))}},
					{OpCode: OpPut, Type: TypeValue, A: Operand{Kind: OperandKindArgument, Index: 0}, B: Operand{Kind: OperandKindRegister, Index: 1}},
				},
				Execute: term.NewFunctor(term.NewAtomRune('q'), 1),
			},
		},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			heap = heap[:0]
			var vars []syntax.ParsedVariable
			h, err := syntax.ParseTerm(test.head, syntax.Heap(&heap), syntax.Variables(&vars))
			if err != nil {
				t.Fatalf("ParseTerm(%q): %v", test.head, err)
			}
			b, err := syntax.ParseTerm(test.body, syntax.Heap(&heap), syntax.Variables(&vars))
			if err != nil {
				t.Fatalf("ParseTerm(%q): %v", test.body, err)
			}
			var c Clause
			err = c.Compile(&heap, h, b)
			if !errors.Is(err, test.err) {
				t.Errorf("Compile(%q): got %v, want %v", test.head, err, test.err)
			}

			if got, want := c.String(), test.clause.String(); got != want {
				t.Errorf("Compile(%q): got %v, want %v", test.head, got, want)
			}
		})
	}
}

func must[T any](v T, err error) T {
	if err != nil {
		panic(err)
	}
	return v
}
