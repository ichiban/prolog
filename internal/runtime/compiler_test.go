package runtime

import (
	"errors"
	"fmt"
	"testing"

	"github.com/ichiban/prolog/v2/internal/ir"
	"github.com/ichiban/prolog/v2/internal/syntax"
	"github.com/ichiban/prolog/v2/internal/term"
)

func TestCompile(t *testing.T) {
	arena := term.Arena{
		Heap: make(term.Heap, 0, 1024),
	}
	tests := []struct {
		title  string
		text   string
		result *ir.Module
		err    error
	}{
		{
			title: "empty",
			text:  ``,
			result: &ir.Module{
				Name: term.NewAtom("user"),
			},
		},
		{
			title: "simple",
			text:  `p :- q.`,
			result: &ir.Module{
				Name: term.NewAtom("user"),
				Clauses: []ir.Clause{
					{
						PI:      term.NewFunctor(term.NewAtomRune('p'), 1),
						MaxRegs: 1,
						Code:    []ir.Instruction{},
						Execute: term.NewFunctor(term.NewAtomRune('q'), 1),
					},
				},
			},
		},
		{
			title: "rule",
			text:  `r(X, Y) :- p(X), q(Y).`,
			result: &ir.Module{
				Name: term.NewAtom("user"),
				Clauses: []ir.Clause{
					{
						PI:      term.NewFunctor(term.NewAtomRune('r'), 3),
						MaxRegs: 4,
						Code: []ir.Instruction{
							{
								OpCode: ir.OpPut,
								Type:   ir.TypeStructure,
								A:      ir.Operand{Kind: ir.OperandKindFunctor, Functor: term.NewFunctor(term.NewAtomRune('q'), 2)},
								B:      ir.Operand{Kind: ir.OperandKindRegister, Index: 4},
							},
							{
								OpCode: ir.OpWrite,
								Type:   ir.TypeValue,
								A:      ir.Operand{Kind: ir.OperandKindPut},
								B:      ir.Operand{Kind: ir.OperandKindRegister, Index: 2},
							},
							{
								OpCode: ir.OpWrite,
								Type:   ir.TypeValue,
								A:      ir.Operand{Kind: ir.OperandKindPut},
								B:      ir.Operand{Kind: ir.OperandKindRegister, Index: 3},
							},
							{
								OpCode: ir.OpPut,
								Type:   ir.TypeValue,
								A:      ir.Operand{Kind: ir.OperandKindArgument, Index: 2},
								B:      ir.Operand{Kind: ir.OperandKindRegister, Index: 4},
							},
						},
						Execute: term.NewFunctor(term.NewAtomRune('p'), 2),
					},
				},
			},
		},
		{
			title: "deep cut",
			text:  `deep_cut(X) :- p(X), !, q(X).`,
			result: &ir.Module{
				Name: term.NewAtom("user"),
				Clauses: []ir.Clause{
					{
						PI:      term.NewFunctor(term.NewAtom("deep_cut"), 2),
						MaxRegs: 4,
						Code: []ir.Instruction{
							{
								OpCode: ir.OpPut,
								Type:   ir.TypeStructure,
								A:      ir.Operand{Kind: ir.OperandKindFunctor, Functor: term.NewFunctor(term.NewAtom("$cut_to"), 2)},
								B:      ir.Operand{Kind: ir.OperandKindRegister, Index: 3},
							},
							{
								OpCode: ir.OpPush,
								Type:   ir.TypeCut,
							},
							{
								OpCode: ir.OpPush,
								Type:   ir.TypeVariable,
								A:      ir.Operand{Kind: ir.OperandKindPut},
								B:      ir.Operand{Kind: ir.OperandKindRegister, Index: 4},
							},
							{
								OpCode: ir.OpPush,
								Type:   ir.TypeStructure,
								A:      ir.Operand{Kind: ir.OperandKindFunctor, Functor: term.NewFunctor(term.NewAtomRune('q'), 2)},
								B:      ir.Operand{Kind: ir.OperandKindRegister, Index: 4},
							},
							{
								OpCode: ir.OpWrite,
								Type:   ir.TypeValue,
								A:      ir.Operand{Kind: ir.OperandKindPut},
								B:      ir.Operand{Kind: ir.OperandKindRegister, Index: 1},
							},
							{
								OpCode: ir.OpWrite,
								Type:   ir.TypeValue,
								A:      ir.Operand{Kind: ir.OperandKindPut},
								B:      ir.Operand{Kind: ir.OperandKindRegister, Index: 2},
							},
							{
								OpCode: ir.OpPut,
								Type:   ir.TypeValue,
								A:      ir.Operand{Kind: ir.OperandKindArgument, Index: 2},
								B:      ir.Operand{Kind: ir.OperandKindRegister, Index: 3},
							},
						},
						Execute: term.NewFunctor(term.NewAtomRune('p'), 2),
					},
				},
			},
		},
		{
			title: "equal",
			text:  `X = Y :- X = Y.`,
			result: &ir.Module{
				Name: term.NewAtom("user"),
				Clauses: []ir.Clause{
					{
						PI:      term.NewFunctor(term.NewAtomRune('='), 3),
						MaxRegs: 3,
						Code: []ir.Instruction{
							{OpCode: ir.OpPut, Type: ir.TypeValue, A: ir.Operand{Kind: ir.OperandKindTemp}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 1}},
							{OpCode: ir.OpGet, Type: ir.TypeValue, A: ir.Operand{Kind: ir.OperandKindTemp}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 2}},
							{OpCode: ir.OpPut, Type: ir.TypeValue, A: ir.Operand{Kind: ir.OperandKindArgument, Index: 1}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 3}},
						},
						Execute: term.NewFunctor(term.NewAtom("true"), 1),
					},
				},
			},
		},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			arena.Heap = arena.Heap[:0]
			c := Compiler{
				Engine: &Engine{
					BuiltinSet: &BuiltinSet{},
					Module:     term.NewAtom("user"),
					Arena:      &arena,
					Ops:        syntax.NewOperatorSet(),
				},
			}
			var m ir.Module
			err := c.CompileText(t.Context(), &m, test.text)
			if !errors.Is(err, test.err) {
				t.Errorf("got error %v, want %v", err, test.err)
			}
			got, want := (ir.ModuleStringer{Arena: &arena, Module: &m}).String(), (ir.ModuleStringer{Arena: &arena, Module: test.result}).String()
			if got != want {
				t.Errorf("got %v, want %v", got, want)
			}
		})
	}
}

func TestReplaceBody(t *testing.T) {
	tests := []struct {
		goal   string
		result string
		todo   []string
		err    error
	}{
		{goal: `true.`, result: `true`},
		{goal: `X.`, result: `call(X)`},
		{goal: `!.`, result: `$cut_to($cut)`},
		{goal: `call(!).`, result: `$cut_to($cut)`},
		{goal: `var(foo).`, result: `fail`},
		{goal: `var(X).`, result: `var(X)`},
		{goal: `nonvar(foo).`, result: `true`},
		{goal: `nonvar(X).`, result: `nonvar(X)`},
		{goal: `atomic(foo).`, result: `true`},
		{goal: `atomic(foo(bar)).`, result: `fail`},
		{goal: `atomic(X).`, result: `atomic(X)`},
		{goal: `X is A.`, result: `$expr(A,X)`},
		{goal: `X < Y.`, result: `$expr(X,A),$expr(Y,B),$less(A,B)`},
		{goal: `X > Y.`, result: `$expr(X,A),$expr(Y,B),$greater(A,B)`},
		{goal: `X =< Y.`, result: `$expr(X,A),$expr(Y,B),$less_eq(A,B)`},
		{goal: `X >= Y.`, result: `$expr(X,A),$expr(Y,B),$greater_eq(A,B)`},
		{goal: `X =:= Y.`, result: `$expr(X,A),$expr(Y,B),$arith_eq(A,B)`},
		{goal: `X =\= Y.`, result: `$expr(X,A),$expr(Y,B),$arith_dif(A,B)`},
		{goal: `X is 1.`, result: `$+(1,0,X)`},
		{goal: `X is pi.`, result: `$pi(X)`},
		{goal: `X is abs(Y).`, result: `$expr(Y,A),$abs(A,X)`},
		{goal: `X is rem(Y, Z).`, result: `$expr(Y,A),$expr(Z,B),$rem(A,B,X)`},
		{goal: `X, Y.`, result: `call(X),call(Y)`},
		{goal: `!; X.`, result: `$or($cut_to($cut),call(X))`},
		{goal: `X; !.`, result: `$or(call(X),$cut_to($cut))`},
		{goal: `X -> Y.`, result: `call(X)->call(Y)`},
		{goal: `X == Y.`, result: `compare(=,X,Y)`},
		{goal: `X @< Y.`, result: `compare(<,X,Y)`},
		{goal: `X @> Y.`, result: `compare(>,X,Y)`},
	}
	for _, test := range tests {
		t.Run(test.goal, func(t *testing.T) {
			var (
				arena    = term.Arena{Heap: make(term.Heap, 0, 1024)}
				a        = must(arena.PutVariable())
				b        = must(arena.PutVariable())
				c        = must(arena.PutVariable())
				vars     = []term.Handle{a, b, c}
				pvs      []syntax.ParsedVariable
				compiler = Compiler{
					Engine: &Engine{Arena: &arena},
					makeVariable: func() (term.Handle, error) {
						var v term.Handle
						v, vars = vars[0], vars[1:]
						return v, nil
					},
				}
			)

			tr, err := arena.PutAtom(term.NewAtom("true"))
			if err != nil {
				t.Fatal(err)
			}

			goal, err := syntax.ParseTerm(test.goal,
				syntax.Arena(&arena),
				syntax.Variables(&pvs),
			)
			if err != nil {
				t.Fatal(err)
			}
			goal, err = compiler.ReplaceBody(goal, tr)
			if !errors.Is(err, test.err) {
				t.Errorf("got error %v, want %v", err, test.err)
			}

			varNames := map[term.Handle]term.Atom{
				a: term.NewAtomRune('A'),
				b: term.NewAtomRune('B'),
				c: term.NewAtomRune('C'),
			}
			for _, pv := range pvs {
				varNames[pv.Variable] = term.NewAtom(pv.Name)
			}

			got := fmt.Sprintf("%s", &syntax.Formatter{
				Arena:        &arena,
				Term:         goal,
				Quoted:       true,
				VariableName: varNames,
			})

			if got != test.result {
				t.Errorf("got %v, want %v", got, test.result)
			}
		})
	}
}

func TestBinarize(t *testing.T) {
	tests := []struct {
		head, body       string
		newHead, newBody string
		err              error
	}{
		{
			head:    `p(X).`,
			body:    `q(X), r(X).`,
			newHead: `p(X, Cont).`,
			newBody: `q(X, r(X, Cont)).`,
		},
		{
			head:    `p.`,
			body:    `true, q.`,
			newHead: `p(Cont).`,
			newBody: `q(Cont).`,
		},
		{
			head:    `p.`,
			body:    `fail, q.`,
			newHead: `p(Cont).`,
			newBody: `fail(Cont).`,
		},
		{
			head: `1.`,
			body: `q.`,
			err:  errUnhandled,
		},
		{
			head: `p.`,
			body: `1.`,
			err:  errUnhandled,
		},
	}
	for _, test := range tests {
		t.Run(test.head, func(t *testing.T) {
			var (
				arena    = term.Arena{Heap: make(term.Heap, 0, 1024)}
				pvs      []syntax.ParsedVariable
				compiler = Compiler{
					Engine: &Engine{Arena: &arena},
				}
			)
			cont, err := arena.PutVariable()
			if err != nil {
				t.Fatal(err)
			}
			pvs = append(pvs, syntax.ParsedVariable{
				Name:     "Cont",
				Variable: cont,
			})
			head, err := syntax.ParseTerm(test.head,
				syntax.Arena(&arena),
				syntax.Variables(&pvs),
			)
			if err != nil {
				t.Fatal(err)
			}
			body, err := syntax.ParseTerm(test.body,
				syntax.Arena(&arena),
				syntax.Variables(&pvs),
			)
			if err != nil {
				t.Fatal(err)
			}
			newHead, newBody, err := compiler.Binarize(head, body, cont)
			if !errors.Is(err, test.err) {
				t.Errorf("got error %v, want %v", err, test.err)
			}
			if err != nil {
				return
			}
			expectedHead, err := syntax.ParseTerm(test.newHead,
				syntax.Arena(&arena),
				syntax.Variables(&pvs),
			)
			if err != nil {
				t.Fatal(err)
			}
			expectedBody, err := syntax.ParseTerm(test.newBody,
				syntax.Arena(&arena),
				syntax.Variables(&pvs),
			)
			if err != nil {
				t.Fatal(err)
			}
			varNames := map[term.Handle]term.Atom{
				cont: term.NewAtom("Cont"),
			}
			if arena.Compare(newHead, expectedHead) != 0 {
				t.Errorf("got %s, want %s", &syntax.Formatter{
					Arena:        &arena,
					Term:         newHead,
					VariableName: varNames,
				}, &syntax.Formatter{
					Arena:        &arena,
					Term:         expectedHead,
					VariableName: varNames,
				})
			}
			if arena.Compare(newBody, expectedBody) != 0 {
				t.Errorf("got %s, want %s", &syntax.Formatter{
					Arena:        &arena,
					Term:         newBody,
					VariableName: varNames,
				}, &syntax.Formatter{
					Arena:        &arena,
					Term:         expectedBody,
					VariableName: varNames,
				})
			}
		})
	}
}

func TestCompiler_CompileClause(t *testing.T) {
	arena := term.Arena{
		Heap: make(term.Heap, 0, 1024),
	}
	engine := Engine{
		Arena: &arena,
		BuiltinSet: &BuiltinSet{
			index: map[term.Functor]int{
				term.NewFunctor(term.NewAtom("functor"), 4): 1,
				term.NewFunctor(term.NewAtom("fail"), 1):    3,
				term.NewFunctor(term.NewAtom("var"), 2):     4,
				term.NewFunctor(term.NewAtom("$less"), 3):   5,
				term.NewFunctor(term.NewAtom("$+"), 4):      6,
			},
			entries: []Builtin{
				{},
				{Type: BuiltinTypeStandard},
				{},
				{Type: BuiltinTypeInline},
				{Type: BuiltinTypeInline},
				{Type: BuiltinTypeArithmetic0},
				{Type: BuiltinTypeArithmetic1},
			},
		},
	}
	compiler := Compiler{Engine: &engine}
	tests := []struct {
		title  string
		head   string
		body   string
		clause ir.Clause
		err    error
	}{
		{
			title: "simplest",
			head:  `p(Cont).`,
			body:  `q(Cont).`,
			clause: ir.Clause{
				PI:      term.NewFunctor(term.NewAtomRune('p'), 1),
				MaxRegs: 1,
				Code:    []ir.Instruction{},
				Execute: term.NewFunctor(term.NewAtomRune('q'), 1),
			},
		},
		{
			title: "atomic in head",
			head:  `p(a, Cont).`,
			body:  `q(Cont).`,
			clause: ir.Clause{
				PI:       term.NewFunctor(term.NewAtomRune('p'), 2),
				FirstArg: ir.Index{Term: must(arena.PutAtom(term.NewAtomRune('a')))},
				MaxRegs:  2,
				Code: []ir.Instruction{
					{OpCode: ir.OpGet, Type: ir.TypeConstant, A: ir.Operand{Kind: ir.OperandKindArgument, Index: 1}, B: ir.Operand{Kind: ir.OperandKindTerm, Term: must(arena.PutAtom(term.NewAtomRune('a')))}},
					{OpCode: ir.OpGet, Type: ir.TypeVariable, A: ir.Operand{Kind: ir.OperandKindArgument, Index: 2}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 1}},
				},
				Execute: term.NewFunctor(term.NewAtomRune('q'), 1),
			},
		},
		{
			title: "repeated head variable",
			head:  "p(X, X, Cont).",
			body:  "q(Cont).",
			clause: ir.Clause{
				PI:      term.NewFunctor(term.NewAtomRune('p'), 3),
				MaxRegs: 3,
				Code: []ir.Instruction{
					{OpCode: ir.OpGet, Type: ir.TypeValue, A: ir.Operand{Kind: ir.OperandKindArgument, Index: 2}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 1}},
					{OpCode: ir.OpGet, Type: ir.TypeVariable, A: ir.Operand{Kind: ir.OperandKindArgument, Index: 3}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 1}},
				},
				Execute: term.NewFunctor(term.NewAtomRune('q'), 1),
			},
		},
		{
			title: "atomic in body",
			head:  "p(Cont).",
			body:  "q(a, Cont).",
			clause: ir.Clause{
				PI:      term.NewFunctor(term.NewAtomRune('p'), 1),
				MaxRegs: 2,
				Code: []ir.Instruction{
					{OpCode: ir.OpGet, Type: ir.TypeVariable, A: ir.Operand{Kind: ir.OperandKindArgument, Index: 1}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 2}},
					{OpCode: ir.OpPut, Type: ir.TypeConstant, A: ir.Operand{Kind: ir.OperandKindArgument, Index: 1}, B: ir.Operand{Kind: ir.OperandKindTerm, Term: must(arena.PutAtom(term.NewAtomRune('a')))}},
				},
				Execute: term.NewFunctor(term.NewAtomRune('q'), 2),
			},
		},
		{
			title: "structure in head",
			head:  "p(f(X), Cont).",
			body:  "q(X, Cont).",
			clause: ir.Clause{
				PI:       term.NewFunctor(term.NewAtomRune('p'), 2),
				FirstArg: ir.Index{Term: must(arena.PutAtom(term.NewAtomRune('f'))), Arity: 1},
				MaxRegs:  2,
				Code: []ir.Instruction{
					{OpCode: ir.OpGet, Type: ir.TypeStructure, A: ir.Operand{Kind: ir.OperandKindFunctor, Functor: term.NewFunctor(term.NewAtomRune('f'), 1)}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 1}},
					{OpCode: ir.OpUnify, Type: ir.TypeVariable, A: ir.Operand{Kind: ir.OperandKindGet}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 1}},
				},
				Execute: term.NewFunctor(term.NewAtomRune('q'), 2),
			},
		},
		{
			title: "structures in body",
			head:  "p(X, Cont).",
			body:  "q(f(X), Cont).",
			clause: ir.Clause{
				PI:      term.NewFunctor(term.NewAtomRune('p'), 2),
				MaxRegs: 3,
				Code: []ir.Instruction{
					{OpCode: ir.OpPut, Type: ir.TypeStructure, A: ir.Operand{Kind: ir.OperandKindFunctor, Functor: term.NewFunctor(term.NewAtomRune('f'), 1)}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 3}},
					{OpCode: ir.OpWrite, Type: ir.TypeValue, A: ir.Operand{Kind: ir.OperandKindPut}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 1}},
					{OpCode: ir.OpPut, Type: ir.TypeValue, A: ir.Operand{Kind: ir.OperandKindArgument, Index: 1}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 3}},
				},
				Execute: term.NewFunctor(term.NewAtomRune('q'), 2),
			},
		},
		{
			title: "argument shuffling",
			head:  "p(X, Y, Cont).",
			body:  "q(Y, X, Cont).",
			clause: ir.Clause{
				PI:      term.NewFunctor(term.NewAtomRune('p'), 3),
				MaxRegs: 4,
				Code: []ir.Instruction{
					{OpCode: ir.OpGet, Type: ir.TypeVariable, A: ir.Operand{Kind: ir.OperandKindArgument, Index: 1}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 4}},
					{OpCode: ir.OpGet, Type: ir.TypeVariable, A: ir.Operand{Kind: ir.OperandKindArgument, Index: 2}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 1}},
					{OpCode: ir.OpPut, Type: ir.TypeValue, A: ir.Operand{Kind: ir.OperandKindArgument, Index: 2}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 4}},
				},
				Execute: term.NewFunctor(term.NewAtomRune('q'), 3),
			},
		},
		{
			title: "nested structure in head",
			head:  "p(f(g(X)), Cont).",
			body:  "q(X, Cont).",
			clause: ir.Clause{
				PI:       term.NewFunctor(term.NewAtomRune('p'), 2),
				FirstArg: ir.Index{Term: must(arena.PutAtom(term.NewAtomRune('f'))), Arity: 1},
				MaxRegs:  3,
				Code: []ir.Instruction{
					{OpCode: ir.OpGet, Type: ir.TypeStructure, A: ir.Operand{Kind: ir.OperandKindFunctor, Functor: term.NewFunctor(term.NewAtomRune('f'), 1)}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 1}},
					{OpCode: ir.OpUnify, Type: ir.TypeVariable, A: ir.Operand{Kind: ir.OperandKindGet}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 3}},
					{OpCode: ir.OpGet, Type: ir.TypeStructure, A: ir.Operand{Kind: ir.OperandKindFunctor, Functor: term.NewFunctor(term.NewAtomRune('g'), 1)}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 3}},
					{OpCode: ir.OpUnify, Type: ir.TypeVariable, A: ir.Operand{Kind: ir.OperandKindGet}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 1}},
				},
				Execute: term.NewFunctor(term.NewAtomRune('q'), 2),
			},
		},
		{
			title: "nested structure in body",
			head:  "p(X, Cont).",
			body:  "q(f(g(X)), Cont).",
			clause: ir.Clause{
				PI:      term.NewFunctor(term.NewAtomRune('p'), 2),
				MaxRegs: 4,
				Code: []ir.Instruction{
					{OpCode: ir.OpPut, Type: ir.TypeStructure, A: ir.Operand{Kind: ir.OperandKindFunctor, Functor: term.NewFunctor(term.NewAtomRune('f'), 1)}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 3}},
					{OpCode: ir.OpPush, Type: ir.TypeVariable, A: ir.Operand{Kind: ir.OperandKindPut}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 4}},
					{OpCode: ir.OpPush, Type: ir.TypeStructure, A: ir.Operand{Kind: ir.OperandKindFunctor, Functor: term.NewFunctor(term.NewAtomRune('g'), 1)}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 4}},
					{OpCode: ir.OpWrite, Type: ir.TypeValue, A: ir.Operand{Kind: ir.OperandKindPut}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 1}},
					{OpCode: ir.OpPut, Type: ir.TypeValue, A: ir.Operand{Kind: ir.OperandKindArgument, Index: 1}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 3}},
				},
				Execute: term.NewFunctor(term.NewAtomRune('q'), 2),
			},
		},
		{
			title: "cut",
			head:  "p(Cont).",
			body:  "'$cut_to'('$cut', q(Cont)).",
			clause: ir.Clause{
				PI:      term.NewFunctor(term.NewAtomRune('p'), 1),
				MaxRegs: 1,
				Code: []ir.Instruction{
					{OpCode: ir.OpPut, Type: ir.TypeCut},
				},
				Execute: term.NewFunctor(term.NewAtomRune('q'), 1),
			},
		},
		{
			title: "equal",
			head:  "p(X, Cont).",
			body:  "'='(X, a, q(Cont)).",
			clause: ir.Clause{
				PI:      term.NewFunctor(term.NewAtomRune('p'), 2),
				MaxRegs: 2,
				Code: []ir.Instruction{
					{OpCode: ir.OpPut, Type: ir.TypeValue, A: ir.Operand{Kind: ir.OperandKindTemp}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 1}},
					{OpCode: ir.OpGet, Type: ir.TypeConstant, A: ir.Operand{Kind: ir.OperandKindTemp}, B: ir.Operand{Kind: ir.OperandKindTerm, Term: must(arena.PutAtom(term.NewAtomRune('a')))}},
					{OpCode: ir.OpPut, Type: ir.TypeValue, A: ir.Operand{Kind: ir.OperandKindArgument, Index: 1}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 2}},
				},
				Execute: term.NewFunctor(term.NewAtomRune('q'), 1),
			},
		},
		{
			title: "builtin",
			head:  "functor(T, F, N, Cont).",
			body:  "true(Cont).",
			clause: ir.Clause{
				PI:      term.NewFunctor(term.NewAtom("functor"), 4),
				MaxRegs: 4,
				Code: []ir.Instruction{
					{OpCode: ir.OpBuiltin, Type: ir.TypeNotApplicable, A: ir.Operand{Kind: ir.OperandKindBuiltin, Index: 1}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 1}},
				},
				Execute: term.NewFunctor(term.NewAtom("true"), 1),
			},
		},
		{
			title: "inline with arity 0",
			head:  "p(Cont).",
			body:  "fail(q(Cont)).",
			clause: ir.Clause{
				PI:      term.NewFunctor(term.NewAtomRune('p'), 1),
				MaxRegs: 2,
				Code: []ir.Instruction{
					{OpCode: ir.OpInline, Type: ir.TypeVariable, A: ir.Operand{Kind: ir.OperandKindBuiltin, Index: 3}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 2}},
				},
				Execute: term.NewFunctor(term.NewAtomRune('q'), 1),
			},
		},
		{
			title: "inline with arity 1",
			head:  "p(X, Cont).",
			body:  "var(X, q(Cont)).",
			clause: ir.Clause{
				PI:      term.NewFunctor(term.NewAtomRune('p'), 2),
				MaxRegs: 3,
				Code: []ir.Instruction{
					{OpCode: ir.OpPut, Type: ir.TypeValue, A: ir.Operand{Kind: ir.OperandKindTemp}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 1}},
					{OpCode: ir.OpInline, Type: ir.TypeVariable, A: ir.Operand{Kind: ir.OperandKindBuiltin, Index: 4}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 3}},
					{OpCode: ir.OpPut, Type: ir.TypeValue, A: ir.Operand{Kind: ir.OperandKindArgument, Index: 1}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 2}},
				},
				Execute: term.NewFunctor(term.NewAtomRune('q'), 1),
			},
		},
		{
			title: "arithmetic",
			head:  "p(X, Y, Cont).",
			body:  "'$less'(X, Y, q(Cont)).",
			clause: ir.Clause{
				PI:      term.NewFunctor(term.NewAtomRune('p'), 3),
				MaxRegs: 3,
				Code: []ir.Instruction{
					{OpCode: ir.OpLoad, Type: ir.TypeValue, A: ir.Operand{Kind: ir.OperandKindArgument, Index: 1}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 1}},
					{OpCode: ir.OpLoad, Type: ir.TypeValue, A: ir.Operand{Kind: ir.OperandKindArgument, Index: 2}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 2}},
					{OpCode: ir.OpArithmetic, Type: ir.TypeConstant, A: ir.Operand{Kind: ir.OperandKindBuiltin, Index: 5}, B: ir.Operand{Kind: ir.OperandKindTerm, Term: must(engine.PutInteger(0))}},
					{OpCode: ir.OpPut, Type: ir.TypeValue, A: ir.Operand{Kind: ir.OperandKindArgument, Index: 1}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 3}},
				},
				Execute: term.NewFunctor(term.NewAtomRune('q'), 1),
			},
		},
		{
			title: "arithmetic with output",
			head:  "p(X, Y, Z, Cont).",
			body:  "$+(X, Y, Z, q(Z, Cont)).",
			clause: ir.Clause{
				PI:      term.NewFunctor(term.NewAtomRune('p'), 4),
				MaxRegs: 4,
				Code: []ir.Instruction{
					{OpCode: ir.OpLoad, Type: ir.TypeValue, A: ir.Operand{Kind: ir.OperandKindArgument, Index: 1}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 1}},
					{OpCode: ir.OpLoad, Type: ir.TypeValue, A: ir.Operand{Kind: ir.OperandKindArgument, Index: 2}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 2}},
					{OpCode: ir.OpArithmetic, Type: ir.TypeValue, A: ir.Operand{Kind: ir.OperandKindBuiltin, Index: 6}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 3}},
					{OpCode: ir.OpPut, Type: ir.TypeValue, A: ir.Operand{Kind: ir.OperandKindArgument, Index: 1}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 3}},
					{OpCode: ir.OpPut, Type: ir.TypeValue, A: ir.Operand{Kind: ir.OperandKindArgument, Index: 2}, B: ir.Operand{Kind: ir.OperandKindRegister, Index: 4}},
				},
				Execute: term.NewFunctor(term.NewAtomRune('q'), 2),
			},
		},
		{
			title: "$cut_to",
			head:  "'$cut_to'('$cut', Cont).",
			body:  "true(Cont).",
			clause: ir.Clause{
				PI:       term.NewFunctor(term.NewAtom("$cut_to"), 2),
				FirstArg: ir.Index{Term: must(engine.PutAtom(term.NewAtom("$cut"))), Arity: 0},
				MaxRegs:  2,
				Code: []ir.Instruction{
					{
						OpCode: ir.OpGet,
						Type:   ir.TypeCut,
					},
					{
						OpCode: ir.OpGet,
						Type:   ir.TypeVariable,
						A:      ir.Operand{Kind: ir.OperandKindArgument, Index: 2},
						B:      ir.Operand{Kind: ir.OperandKindRegister, Index: 1},
					},
				},
				Execute: term.NewFunctor(term.NewAtom("true"), 1),
			},
		},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			arena.Heap = arena.Heap[:0]
			var vars []syntax.ParsedVariable
			h, err := syntax.ParseTerm(test.head, syntax.Arena(&arena), syntax.Variables(&vars))
			if err != nil {
				t.Fatalf("ParseTerm(%q): %v", test.head, err)
			}
			b, err := syntax.ParseTerm(test.body, syntax.Arena(&arena), syntax.Variables(&vars))
			if err != nil {
				t.Fatalf("ParseTerm(%q): %v", test.body, err)
			}
			var c ir.Clause
			err = compiler.CompileBinaryClause(&c, h, b)
			if !errors.Is(err, test.err) {
				t.Errorf("CompileText(%q): got %v, want %v", test.head, err, test.err)
			}

			if got, want := (ir.ClauseStringer{Arena: &arena, Clause: &c}).String(), (ir.ClauseStringer{Arena: &arena, Clause: &test.clause}).String(); got != want {
				t.Errorf("CompileText(%q): got %v, want %v", test.head, got, want)
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
