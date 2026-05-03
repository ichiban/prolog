package ir

import (
	"errors"
	"reflect"
	"testing"

	"github.com/ichiban/prolog/v2/internal/syntax"
	"github.com/ichiban/prolog/v2/internal/term"
)

func TestCompile(t *testing.T) {
	tests := []struct {
		c      Compiler
		text   string
		result *Module
		err    error
	}{
		// TODO
	}

	for _, test := range tests {
		t.Run(test.text, func(t *testing.T) {
			result, err := test.c.Compile(t.Context(), test.text)
			if !errors.Is(err, test.err) {
				t.Errorf("got error %v, want %v", err, test.err)
			}
			if !reflect.DeepEqual(result, test.result) {
				t.Errorf("got %v, want %v", result, test.result)
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
		{goal: `X.`, result: `call(X).`},
		{goal: `!.`, result: `'$cut_to'('$cut').`},
		{goal: `var(foo).`, result: `fail.`},
		{goal: `var(X).`, result: `var(X).`},
		{goal: `nonvar(foo).`, result: `true.`},
		{goal: `nonvar(X).`, result: `nonvar(X).`},
		{goal: `atomic(foo).`, result: `true.`},
		{goal: `atomic(foo(bar)).`, result: `fail.`},
		{goal: `atomic(X).`, result: `atomic(X).`},
		// TODO: split_op.
		{goal: `X, Y.`, result: `call(X), call(Y).`},
		{goal: `!; X.`, result: `'$or'('$cut_to'('$cut'), call(X)).`},
		{goal: `X; !.`, result: `'$or'(call(X), '$cut_to'('$cut')).`},
		{goal: `X -> Y.`, result: `call(X) -> call(Y).`},
		{goal: `X == Y.`, result: `compare(=, X, Y).`},
		{goal: `X @< Y.`, result: `compare(<, X, Y).`},
		{goal: `X @> Y.`, result: `compare(>, X, Y).`},
	}
	for _, test := range tests {
		t.Run(test.goal, func(t *testing.T) {
			var (
				h   = make(term.Heap, 0, 1024)
				pvs []syntax.ParsedVariable
			)
			goal, err := syntax.ParseTerm(test.goal,
				syntax.Heap(&h),
				syntax.Variables(&pvs),
			)
			if err != nil {
				t.Fatal(err)
			}
			var (
				counter int
				todo    []term.Handle
			)
			got, err := ReplaceBody(&h, &counter, goal, &todo)
			if !errors.Is(err, test.err) {
				t.Errorf("got error %v, want %v", err, test.err)
			}
			expected, err := syntax.ParseTerm(test.result,
				syntax.Heap(&h),
				syntax.Variables(&pvs),
			)
			if err != nil {
				t.Fatal(err)
			}
			if term.Compare(got, expected) != 0 {
				t.Errorf("got %s, want %s", &syntax.Formatter{Term: got}, &syntax.Formatter{Term: expected})
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
				h   = make(term.Heap, 0, 1024)
				pvs []syntax.ParsedVariable
			)
			cont, err := h.PutVariable()
			if err != nil {
				t.Fatal(err)
			}
			pvs = append(pvs, syntax.ParsedVariable{
				Name:     "Cont",
				Variable: cont,
			})
			head, err := syntax.ParseTerm(test.head,
				syntax.Heap(&h),
				syntax.Variables(&pvs),
			)
			if err != nil {
				t.Fatal(err)
			}
			body, err := syntax.ParseTerm(test.body,
				syntax.Heap(&h),
				syntax.Variables(&pvs),
			)
			if err != nil {
				t.Fatal(err)
			}
			newHead, newBody, err := Binarize(&h, head, body, cont)
			if !errors.Is(err, test.err) {
				t.Errorf("got error %v, want %v", err, test.err)
			}
			if err != nil {
				return
			}
			expectedHead, err := syntax.ParseTerm(test.newHead,
				syntax.Heap(&h),
				syntax.Variables(&pvs),
			)
			if err != nil {
				t.Fatal(err)
			}
			expectedBody, err := syntax.ParseTerm(test.newBody,
				syntax.Heap(&h),
				syntax.Variables(&pvs),
			)
			if err != nil {
				t.Fatal(err)
			}
			varNames := map[term.Handle]term.Atom{
				cont: term.NewAtom("Cont"),
			}
			if term.Compare(newHead, expectedHead) != 0 {
				t.Errorf("got %s, want %s", &syntax.Formatter{
					Term:         newHead,
					VariableName: varNames,
				}, &syntax.Formatter{
					Term:         expectedHead,
					VariableName: varNames,
				})
			}
			if term.Compare(newBody, expectedBody) != 0 {
				t.Errorf("got %s, want %s", &syntax.Formatter{
					Term:         newBody,
					VariableName: varNames,
				}, &syntax.Formatter{
					Term:         expectedBody,
					VariableName: varNames,
				})
			}
		})
	}
}

func TestCompileClause(t *testing.T) {

}
