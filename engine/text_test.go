package engine

import (
	"context"
	"embed"
	"errors"
	"testing"

	"github.com/stretchr/testify/assert"
)

//go:embed testdata
var testdata embed.FS

func TestState_Compile(t *testing.T) {
	tests := []struct {
		title  string
		text   string
		args   []interface{}
		err    error
		result map[ProcedureIndicator]procedure
	}{
		{title: "facts", text: `
foo(a).
foo(b).
`, result: map[ProcedureIndicator]procedure{
			{Name: "foo", Arity: 1}: &userDefined{
				clauses: clauses{
					{pi: ProcedureIndicator{Name: "foo", Arity: 1}, raw: &compound{functor: "foo", args: []Term{Atom("a")}}, xrTable: []Term{Atom("a")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opExit},
					}},
					{pi: ProcedureIndicator{Name: "foo", Arity: 1}, raw: &compound{functor: "foo", args: []Term{Atom("b")}}, xrTable: []Term{Atom("b")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opExit},
					}},
				},
			},
		}},
		{title: "rules", text: `
bar(X) :- foo(X).
baz(X) :- bar(X).
`, result: map[ProcedureIndicator]procedure{
			{Name: "foo", Arity: 1}: &userDefined{
				multifile: true,
				clauses: clauses{
					{pi: ProcedureIndicator{Name: "foo", Arity: 1}, raw: &compound{functor: "foo", args: []Term{Atom("c")}}, xrTable: []Term{Atom("c")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opExit},
					}},
				},
			},
			{Name: "bar", Arity: 1}: &userDefined{
				clauses: clauses{
					{pi: ProcedureIndicator{Name: "bar", Arity: 1}, raw: Atom(":-").Apply(Atom("bar").Apply(Variable("X")), Atom("foo").Apply(Variable("X"))), xrTable: []Term{ProcedureIndicator{Name: "foo", Arity: 1}}, vars: []Variable{"X"}, bytecode: bytecode{
						{opcode: opVar, operand: 0},
						{opcode: opEnter},
						{opcode: opVar, operand: 0},
						{opcode: opCall, operand: 0},
						{opcode: opExit},
					}},
				},
			},
			{Name: "baz", Arity: 1}: &userDefined{
				clauses: clauses{
					{pi: ProcedureIndicator{Name: "baz", Arity: 1}, raw: Atom(":-").Apply(Atom("baz").Apply(Variable("X")), Atom("bar").Apply(Variable("X"))), xrTable: []Term{ProcedureIndicator{Name: "bar", Arity: 1}}, vars: []Variable{"X"}, bytecode: bytecode{
						{opcode: opVar, operand: 0},
						{opcode: opEnter},
						{opcode: opVar, operand: 0},
						{opcode: opCall, operand: 0},
						{opcode: opExit},
					}},
				},
			},
		}},
		{title: "dynamic", text: `
:- dynamic(foo/1).
foo(a).
foo(b).
`, result: map[ProcedureIndicator]procedure{
			{Name: "foo", Arity: 1}: &userDefined{
				public:  true,
				dynamic: true,
				clauses: clauses{
					{pi: ProcedureIndicator{Name: "foo", Arity: 1}, raw: &compound{functor: "foo", args: []Term{Atom("a")}}, xrTable: []Term{Atom("a")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opExit},
					}},
					{pi: ProcedureIndicator{Name: "foo", Arity: 1}, raw: &compound{functor: "foo", args: []Term{Atom("b")}}, xrTable: []Term{Atom("b")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opExit},
					}},
				},
			},
		}},
		{title: "multifile", text: `
:- multifile(foo/1).
foo(a).
foo(b).
`, result: map[ProcedureIndicator]procedure{
			{Name: "foo", Arity: 1}: &userDefined{
				multifile: true,
				clauses: clauses{
					{pi: ProcedureIndicator{Name: "foo", Arity: 1}, raw: &compound{functor: "foo", args: []Term{Atom("c")}}, xrTable: []Term{Atom("c")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opExit},
					}},
					{pi: ProcedureIndicator{Name: "foo", Arity: 1}, raw: &compound{functor: "foo", args: []Term{Atom("a")}}, xrTable: []Term{Atom("a")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opExit},
					}},
					{pi: ProcedureIndicator{Name: "foo", Arity: 1}, raw: &compound{functor: "foo", args: []Term{Atom("b")}}, xrTable: []Term{Atom("b")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opExit},
					}},
				},
			},
		}},
		{title: "discontiguous", text: `
:- discontiguous(foo/1).
foo(a).
bar(a).
foo(b).
`, result: map[ProcedureIndicator]procedure{
			{Name: "foo", Arity: 1}: &userDefined{
				discontiguous: true,
				clauses: clauses{
					{pi: ProcedureIndicator{Name: "foo", Arity: 1}, raw: &compound{functor: "foo", args: []Term{Atom("a")}}, xrTable: []Term{Atom("a")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opExit},
					}},
					{pi: ProcedureIndicator{Name: "foo", Arity: 1}, raw: &compound{functor: "foo", args: []Term{Atom("b")}}, xrTable: []Term{Atom("b")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opExit},
					}},
				},
			},
			{Name: "bar", Arity: 1}: &userDefined{
				clauses: clauses{
					{pi: ProcedureIndicator{Name: "bar", Arity: 1}, raw: &compound{functor: "bar", args: []Term{Atom("a")}}, xrTable: []Term{Atom("a")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opExit},
					}},
				},
			},
		}},
		{title: "include", text: `
:- include('testdata/foo').
`, result: map[ProcedureIndicator]procedure{
			{Name: "foo", Arity: 0}: &userDefined{
				clauses: clauses{
					{pi: ProcedureIndicator{Name: "foo", Arity: 0}, raw: Atom("foo"), bytecode: bytecode{
						{opcode: opExit},
					}},
				},
			},
			{Name: "foo", Arity: 1}: &userDefined{
				multifile: true,
				clauses: clauses{
					{pi: ProcedureIndicator{Name: "foo", Arity: 1}, raw: &compound{functor: "foo", args: []Term{Atom("c")}}, xrTable: []Term{Atom("c")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opExit},
					}},
				},
			},
		}},
		{title: "ensure_loaded", text: `
:- ensure_loaded('testdata/foo').
`, result: map[ProcedureIndicator]procedure{
			{Name: "foo", Arity: 0}: &userDefined{
				clauses: clauses{
					{pi: ProcedureIndicator{Name: "foo", Arity: 0}, raw: Atom("foo"), bytecode: bytecode{
						{opcode: opExit},
					}},
				},
			},
			{Name: "foo", Arity: 1}: &userDefined{
				multifile: true,
				clauses: clauses{
					{pi: ProcedureIndicator{Name: "foo", Arity: 1}, raw: &compound{functor: "foo", args: []Term{Atom("c")}}, xrTable: []Term{Atom("c")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opExit},
					}},
				},
			},
		}},
		{title: "initialization", text: `
:- initialization(foo(c)).
`, result: map[ProcedureIndicator]procedure{
			{Name: "foo", Arity: 1}: &userDefined{
				multifile: true,
				clauses: clauses{
					{pi: ProcedureIndicator{Name: "foo", Arity: 1}, raw: &compound{functor: "foo", args: []Term{Atom("c")}}, xrTable: []Term{Atom("c")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opExit},
					}},
				},
			},
		}},
		{title: "predicate-backed directive", text: `
:- foo(c).
`, result: map[ProcedureIndicator]procedure{
			{Name: "foo", Arity: 1}: &userDefined{
				multifile: true,
				clauses: clauses{
					{pi: ProcedureIndicator{Name: "foo", Arity: 1}, raw: &compound{functor: "foo", args: []Term{Atom("c")}}, xrTable: []Term{Atom("c")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opExit},
					}},
				},
			},
		}},

		{title: "error: invalid argument", text: `
foo(?).
`, args: []interface{}{nil}, err: errors.New("can't convert to term: <invalid reflect.Value>")},
		{title: "error: syntax error", text: `
foo().
`, err: unexpectedTokenError{actual: Token{Kind: TokenClose, Val: ")"}}},
		{title: "error: variable fact", text: `
X.
`, err: InstantiationError(nil)},
		{title: "error: variable rule", text: `
X :- X.
`, err: InstantiationError(nil)},
		{title: "error: non-callable rule body", text: `
foo :- 1.
`, err: TypeError(ValidTypeCallable, Integer(1), nil)},
		{title: "error: included variable", text: `
:- include(X).
`, err: InstantiationError(nil)},
		{title: "error: included file not found", text: `
:- include('testdata/not_found').
`, err: ExistenceError(ObjectTypeSourceSink, Atom("testdata/not_found"), nil)},
		{title: "error: included non-atom", text: `
:- include(1).
`, err: TypeError(ValidTypeAtom, Integer(1), nil)},
		{title: "error: predicate-backed directive exception", text: `
:- bar.
`, err: ExistenceError(ObjectTypeProcedure, Atom("/").Apply(Atom("bar"), Integer(0)), nil)},
		{title: "error: predicate-backed directive failure", text: `
:- foo(d).
`, err: errors.New("failed directive: foo(d)")},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			var state State
			state.operators.define(1200, operatorSpecifierXFX, ":-")
			state.operators.define(1200, operatorSpecifierFX, ":-")
			state.operators.define(400, operatorSpecifierYFX, "/")
			state.procedures = map[ProcedureIndicator]procedure{
				{Name: "foo", Arity: 1}: &userDefined{
					multifile: true,
					clauses: clauses{
						{pi: ProcedureIndicator{Name: "foo", Arity: 1}, raw: &compound{functor: "foo", args: []Term{Atom("c")}}, xrTable: []Term{Atom("c")}, bytecode: bytecode{
							{opcode: opConst, operand: 0},
							{opcode: opExit},
						}},
					},
				},
			}
			state.FS = testdata
			assert.Equal(t, tt.err, state.Compile(context.Background(), tt.text, tt.args...))
			if tt.err == nil {
				assert.Equal(t, tt.result, state.procedures)
			}
		})
	}
}

func TestState_Consult(t *testing.T) {
	tests := []struct {
		title string
		files Term
		ok    bool
		err   error
	}{
		{title: `:- consult('testdata/empty.txt').`, files: Atom("testdata/empty.txt"), ok: true},
		{title: `:- consult([]).`, files: List(), ok: true},
		{title: `:- consult(['testdata/empty.txt']).`, files: List(Atom("testdata/empty.txt")), ok: true},
		{title: `:- consult(['testdata/empty.txt', 'testdata/empty.txt']).`, files: List(Atom("testdata/empty.txt"), Atom("testdata/empty.txt")), ok: true},

		{title: `:- consult('testdata/abc.txt').`, files: Atom("testdata/abc.txt"), err: ErrInsufficient},
		{title: `:- consult(['testdata/abc.txt']).`, files: List(Atom("testdata/abc.txt")), err: ErrInsufficient},

		{title: `:- consult(X).`, files: Variable("X"), err: InstantiationError(nil)},
		{title: `:- consult(foo(bar)).`, files: Atom("foo").Apply(Atom("bar")), err: TypeError(ValidTypeAtom, Atom("foo").Apply(Atom("bar")), nil)},
		{title: `:- consult(1).`, files: Integer(1), err: TypeError(ValidTypeAtom, Integer(1), nil)},
		{title: `:- consult(['testdata/empty.txt'|_]).`, files: ListRest(NewVariable(), Atom("testdata/empty.txt")), err: TypeError(ValidTypeAtom, ListRest(NewVariable(), Atom("testdata/empty.txt")), nil)},
		{title: `:- consult([X]).`, files: List(Variable("X")), err: InstantiationError(nil)},
		{title: `:- consult([1]).`, files: List(Integer(1)), err: TypeError(ValidTypeAtom, Integer(1), nil)},

		{title: `:- consult('testdata/not_found.txt').`, files: Atom("testdata/not_found.txt"), err: ExistenceError(ObjectTypeSourceSink, Atom("testdata/not_found.txt"), nil)},
		{title: `:- consult(['testdata/not_found.txt']).`, files: List(Atom("testdata/not_found.txt")), err: ExistenceError(ObjectTypeSourceSink, Atom("testdata/not_found.txt"), nil)},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			state := State{
				FS: testdata,
			}
			ok, err := state.Consult(tt.files, Success, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			if e, ok := tt.err.(Exception); ok {
				_, ok := NewEnv().Unify(e.Term(), err.(Exception).Term(), false)
				assert.True(t, ok)
			} else {
				assert.Equal(t, tt.err, err)
			}
		})
	}
}
