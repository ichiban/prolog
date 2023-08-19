package engine

import (
	"context"
	"embed"
	"errors"
	"io"
	"io/fs"
	"testing"

	"github.com/stretchr/testify/assert"
)

//go:embed testdata
var testdata embed.FS

func mustOpen(fs fs.FS, name string) fs.File {
	f, err := fs.Open(name)
	if err != nil {
		panic(err)
	}
	return f
}

func TestVM_Compile(t *testing.T) {
	tests := []struct {
		title  string
		text   string
		args   []interface{}
		err    error
		result map[procedureIndicator]procedureEntry
	}{
		{title: "shebang", text: `#!/foo/bar
foo(a).
`, result: map[procedureIndicator]procedureEntry{
			{module: atomUser, name: NewAtom("foo"), arity: 1}: {
				definedIn: atomUser,
				procedure: clauses{
					{
						pi:  procedureIndicator{name: NewAtom("foo"), arity: 1},
						raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("a")}},
						bytecode: bytecode{
							{opcode: opGetConst, operand: NewAtom("a")},
							{opcode: opExit},
						},
					},
				},
			},
		}},
		{title: "shebang: no following lines", text: `#!/foo/bar`, result: map[procedureIndicator]procedureEntry{
			{module: atomUser, name: NewAtom("foo"), arity: 1}: {
				multifile: true,
				procedure: clauses{
					{
						pi:  procedureIndicator{name: NewAtom("foo"), arity: 1},
						raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("c")}},
						bytecode: bytecode{
							{opcode: opGetConst, operand: NewAtom("c")},
							{opcode: opExit},
						},
					},
				},
			},
		}},
		{title: "facts", text: `
foo(a).
foo(b).
`, result: map[procedureIndicator]procedureEntry{
			{module: atomUser, name: NewAtom("foo"), arity: 1}: {
				definedIn: atomUser,
				procedure: clauses{
					{
						pi:  procedureIndicator{name: NewAtom("foo"), arity: 1},
						raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("a")}},
						bytecode: bytecode{
							{opcode: opGetConst, operand: NewAtom("a")},
							{opcode: opExit},
						},
					},
					{
						pi:  procedureIndicator{name: NewAtom("foo"), arity: 1},
						raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("b")}},
						bytecode: bytecode{
							{opcode: opGetConst, operand: NewAtom("b")},
							{opcode: opExit},
						},
					},
				},
			},
		}},
		{title: "rules", text: `
bar :- true.
bar(X, "abc", [a, b], [a, b|Y], f(a)) :- X, !, foo(X, "abc", [a, b], [a, b|Y], f(a)).
`, result: map[procedureIndicator]procedureEntry{
			{module: atomUser, name: NewAtom("foo"), arity: 1}: {
				multifile: true,
				procedure: clauses{
					{
						pi:  procedureIndicator{name: NewAtom("foo"), arity: 1},
						raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("c")}},
						bytecode: bytecode{
							{opcode: opGetConst, operand: NewAtom("c")},
							{opcode: opExit},
						},
					},
				},
			},
			{module: atomUser, name: NewAtom("bar"), arity: 0}: {
				definedIn: atomUser,
				procedure: clauses{
					{
						pi:  procedureIndicator{name: NewAtom("bar"), arity: 0},
						raw: atomIf.Apply(NewAtom("bar"), atomTrue),
						bytecode: bytecode{
							{opcode: opEnter},
							{opcode: opCall, operand: procedureIndicator{module: atomUser, name: atomTrue, arity: 0}},
							{opcode: opExit},
						},
					},
				},
			},
			{module: atomUser, name: NewAtom("bar"), arity: 5}: {
				definedIn: atomUser,
				procedure: clauses{
					{
						pi: procedureIndicator{name: NewAtom("bar"), arity: 5},
						raw: atomIf.Apply(
							NewAtom("bar").Apply(lastVariable()+1, charList("abc"), List(NewAtom("a"), NewAtom("b")), PartialList(lastVariable()+2, NewAtom("a"), NewAtom("b")), NewAtom("f").Apply(NewAtom("a"))),
							seq(atomComma,
								lastVariable()+1,
								atomCut,
								NewAtom("foo").Apply(lastVariable()+1, charList("abc"), List(NewAtom("a"), NewAtom("b")), PartialList(lastVariable()+2, NewAtom("a"), NewAtom("b")), NewAtom("f").Apply(NewAtom("a"))),
							),
						),
						vars: []Variable{lastVariable() + 1, lastVariable() + 2},
						bytecode: bytecode{
							{opcode: opGetVar, operand: Integer(0)},
							{opcode: opGetConst, operand: charList("abc")},
							{opcode: opGetList, operand: Integer(2)},
							{opcode: opGetConst, operand: NewAtom("a")},
							{opcode: opGetConst, operand: NewAtom("b")},
							{opcode: opPop},
							{opcode: opGetPartial, operand: Integer(2)},
							{opcode: opGetVar, operand: Integer(1)},
							{opcode: opGetConst, operand: NewAtom("a")},
							{opcode: opGetConst, operand: NewAtom("b")},
							{opcode: opPop},
							{opcode: opGetFunctor, operand: procedureIndicator{name: NewAtom("f"), arity: 1}},
							{opcode: opGetConst, operand: NewAtom("a")},
							{opcode: opPop},
							{opcode: opEnter},
							{opcode: opPutVar, operand: Integer(0)},
							{opcode: opCall, operand: procedureIndicator{module: atomUser, name: atomCall, arity: 1}},
							{opcode: opCut},
							{opcode: opPutVar, operand: Integer(0)},
							{opcode: opPutConst, operand: charList("abc")},
							{opcode: opPutList, operand: Integer(2)},
							{opcode: opPutConst, operand: NewAtom("a")},
							{opcode: opPutConst, operand: NewAtom("b")},
							{opcode: opPop},
							{opcode: opPutPartial, operand: Integer(2)},
							{opcode: opPutVar, operand: Integer(1)},
							{opcode: opPutConst, operand: NewAtom("a")},
							{opcode: opPutConst, operand: NewAtom("b")},
							{opcode: opPop},
							{opcode: opPutFunctor, operand: procedureIndicator{name: NewAtom("f"), arity: 1}},
							{opcode: opPutConst, operand: NewAtom("a")},
							{opcode: opPop},
							{opcode: opCall, operand: procedureIndicator{module: atomUser, name: NewAtom("foo"), arity: 5}},
							{opcode: opExit},
						},
					},
				},
			},
		}},
		{title: "dynamic", text: `
:- dynamic(foo/1).
foo(a).
foo(b).
`, result: map[procedureIndicator]procedureEntry{
			{module: atomUser, name: NewAtom("foo"), arity: 1}: {
				public:    true,
				dynamic:   true,
				definedIn: atomUser,
				procedure: clauses{
					{
						pi:  procedureIndicator{name: NewAtom("foo"), arity: 1},
						raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("a")}},
						bytecode: bytecode{
							{opcode: opGetConst, operand: NewAtom("a")},
							{opcode: opExit},
						},
					},
					{
						pi:  procedureIndicator{name: NewAtom("foo"), arity: 1},
						raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("b")}},
						bytecode: bytecode{
							{opcode: opGetConst, operand: NewAtom("b")},
							{opcode: opExit},
						},
					},
				},
			},
		}},
		{title: "multifile", text: `
:- multifile(foo/1).
foo(a).
foo(b).
`, result: map[procedureIndicator]procedureEntry{
			{module: atomUser, name: NewAtom("foo"), arity: 1}: {
				multifile: true,
				definedIn: atomUser,
				procedure: clauses{
					{
						pi:  procedureIndicator{name: NewAtom("foo"), arity: 1},
						raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("c")}},
						bytecode: bytecode{
							{opcode: opGetConst, operand: NewAtom("c")},
							{opcode: opExit},
						},
					},
					{
						pi:  procedureIndicator{name: NewAtom("foo"), arity: 1},
						raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("a")}},
						bytecode: bytecode{
							{opcode: opGetConst, operand: NewAtom("a")},
							{opcode: opExit},
						},
					},
					{
						pi:  procedureIndicator{name: NewAtom("foo"), arity: 1},
						raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("b")}},
						bytecode: bytecode{
							{opcode: opGetConst, operand: NewAtom("b")},
							{opcode: opExit},
						},
					},
				},
			},
		}},
		{title: "discontiguous", text: `
:- discontiguous(foo/1).
foo(a).
bar(a).
foo(b).
`, result: map[procedureIndicator]procedureEntry{
			{module: atomUser, name: NewAtom("foo"), arity: 1}: {
				discontiguous: true,
				definedIn:     atomUser,
				procedure: clauses{
					{
						pi:  procedureIndicator{name: NewAtom("foo"), arity: 1},
						raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("a")}},
						bytecode: bytecode{
							{opcode: opGetConst, operand: NewAtom("a")},
							{opcode: opExit},
						},
					},
					{
						pi:  procedureIndicator{name: NewAtom("foo"), arity: 1},
						raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("b")}},
						bytecode: bytecode{
							{opcode: opGetConst, operand: NewAtom("b")},
							{opcode: opExit},
						},
					},
				},
			},
			{module: atomUser, name: NewAtom("bar"), arity: 1}: {
				definedIn: atomUser,
				procedure: clauses{
					{
						pi:  procedureIndicator{name: NewAtom("bar"), arity: 1},
						raw: &compound{functor: NewAtom("bar"), args: []Term{NewAtom("a")}},
						bytecode: bytecode{
							{opcode: opGetConst, operand: NewAtom("a")},
							{opcode: opExit},
						},
					},
				},
			},
		}},
		{title: "include", text: `
:- include('testdata/foo.pl').
`, result: map[procedureIndicator]procedureEntry{
			{module: atomUser, name: NewAtom("foo"), arity: 0}: {
				definedIn: atomUser,
				procedure: clauses{
					{
						pi:  procedureIndicator{name: NewAtom("foo"), arity: 0},
						raw: NewAtom("foo"),
						bytecode: bytecode{
							{opcode: opExit},
						},
					},
				},
			},
			{module: atomUser, name: NewAtom("foo"), arity: 1}: {
				multifile: true,
				procedure: clauses{
					{
						pi:  procedureIndicator{name: NewAtom("foo"), arity: 1},
						raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("c")}},
						bytecode: bytecode{
							{opcode: opGetConst, operand: NewAtom("c")},
							{opcode: opExit},
						},
					},
				},
			},
		}},
		{title: "ensure_loaded", text: `
:- ensure_loaded('testdata/foo').
`, result: map[procedureIndicator]procedureEntry{
			{module: atomUser, name: NewAtom("foo"), arity: 0}: {
				definedIn: atomUser,
				procedure: clauses{
					{
						pi:  procedureIndicator{name: NewAtom("foo"), arity: 0},
						raw: NewAtom("foo"),
						bytecode: bytecode{
							{opcode: opExit},
						},
					},
				},
			},
			{module: atomUser, name: NewAtom("foo"), arity: 1}: {
				multifile: true,
				procedure: clauses{
					{
						pi:  procedureIndicator{name: NewAtom("foo"), arity: 1},
						raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("c")}},
						bytecode: bytecode{
							{opcode: opGetConst, operand: NewAtom("c")},
							{opcode: opExit},
						},
					},
				},
			},
		}},
		{title: "initialization", text: `
:- initialization(foo(c)).
`, result: map[procedureIndicator]procedureEntry{
			{module: atomUser, name: NewAtom("foo"), arity: 1}: {
				multifile: true,
				procedure: clauses{
					{
						pi:  procedureIndicator{name: NewAtom("foo"), arity: 1},
						raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("c")}},
						bytecode: bytecode{
							{opcode: opGetConst, operand: NewAtom("c")},
							{opcode: opExit},
						},
					},
				},
			},
		}},
		{title: "predicate-backed directive", text: `
:- foo(c).
`, result: map[procedureIndicator]procedureEntry{
			{module: atomUser, name: NewAtom("foo"), arity: 1}: {
				multifile: true,
				procedure: clauses{
					{
						pi:  procedureIndicator{name: NewAtom("foo"), arity: 1},
						raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("c")}},
						bytecode: bytecode{
							{opcode: opGetConst, operand: NewAtom("c")},
							{opcode: opExit},
						},
					},
				},
			},
		}},

		{title: "error: invalid argument", text: `
foo(?).
`, args: []interface{}{nil}, err: errors.New("can't convert to term: <invalid reflect.Value>")},
		{title: "error: syntax error", text: `
foo().
`, err: unexpectedTokenError{actual: Token{kind: tokenClose, val: ")"}}},
		{title: "error: expansion error", text: `
:- ensure_loaded('testdata/break_term_expansion').
foo(a).
`, err: Exception{term: NewAtom("ball")}},
		{title: "error: variable fact", text: `
X.
`, err: InstantiationError(nil)},
		{title: "error: variable rule", text: `
X :- X.
`, err: InstantiationError(nil)},
		{title: "error: non-callable rule body", text: `
foo :- 1.
`, err: typeError(validTypeCallable, Integer(1), nil)},
		{title: "error: non-PI argument, variable", text: `:- dynamic(PI).`, err: InstantiationError(nil)},
		{title: "error: non-PI argument, not compound", text: `:- dynamic(foo).`, err: typeError(validTypePredicateIndicator, NewAtom("foo"), nil)},
		{title: "error: non-PI argument, compound", text: `:- dynamic(foo(a, b)).`, err: typeError(validTypePredicateIndicator, NewAtom("foo").Apply(NewAtom("a"), NewAtom("b")), nil)},
		{title: "error: non-PI argument, name is variable", text: `:- dynamic(Name/2).`, err: InstantiationError(nil)},
		{title: "error: non-PI argument, arity is variable", text: `:- dynamic(foo/Arity).`, err: InstantiationError(nil)},
		{title: "error: non-PI argument, arity is not integer", text: `:- dynamic(foo/bar).`, err: typeError(validTypePredicateIndicator, atomSlash.Apply(NewAtom("foo"), NewAtom("bar")), nil)},
		{title: "error: non-PI argument, name is not atom", text: `:- dynamic(0/2).`, err: typeError(validTypePredicateIndicator, atomSlash.Apply(Integer(0), Integer(2)), nil)},
		{title: "error: included variable", text: `
:- include(X).
`, err: InstantiationError(nil)},
		{title: "error: included file not found", text: `
:- include('testdata/not_found').
`, err: existenceError(objectTypeSourceSink, NewAtom("testdata/not_found"), nil)},
		{title: "error: included non-atom", text: `
:- include(1).
`, err: typeError(validTypeAtom, Integer(1), nil)},
		{title: "error: initialization exception", text: `
:- initialization(bar).
`, err: existenceError(objectTypeProcedure, atomSlash.Apply(NewAtom("bar"), Integer(0)), NewEnv().bind(varContext, procedureIndicator{module: atomUser, name: atomInitialization, arity: 1}))},
		{title: "error: initialization failure", text: `
:- initialization(foo(d)).
`, err: errors.New("failed initialization goal: foo(d)")},
		{title: "error: predicate-backed directive exception", text: `
:- bar.
`, err: existenceError(objectTypeProcedure, atomSlash.Apply(NewAtom("bar"), Integer(0)), NewEnv().bind(varContext, procedureIndicator{module: atomUser, name: atomIf, arity: 1}))},
		{title: "error: predicate-backed directive failure", text: `
:- foo(d).
`, err: errors.New("failed directive: foo(d)")},
		{title: "error: discontiguous, end of text", text: `
foo(a).
bar(a).
foo(b).
`, err: &discontiguousError{pi: procedureIndicator{name: NewAtom("foo"), arity: 1}}},
		{title: "error: discontiguous, before directive", text: `
foo(a).
bar(a).
foo(b).
:- foo(c).
`, err: &discontiguousError{pi: procedureIndicator{name: NewAtom("foo"), arity: 1}}},
		{title: "error: discontiguous, before other facts", text: `
foo(a).
bar(a).
foo(b).
bar(b).
`, err: &discontiguousError{pi: procedureIndicator{name: NewAtom("foo"), arity: 1}}},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ops := operators{}
			ops.define(1200, operatorSpecifierXFX, atomIf)
			ops.define(1200, operatorSpecifierXFX, atomArrow)
			ops.define(1200, operatorSpecifierFX, atomIf)
			ops.define(1000, operatorSpecifierXFY, atomComma)
			ops.define(400, operatorSpecifierYFX, atomSlash)
			vm := VM{moduleLocals: map[Atom]moduleLocal{atomUser: {operators: ops}}}
			vm.procedures = map[procedureIndicator]procedureEntry{
				{module: atomUser, name: NewAtom("throw"), arity: 1}: {procedure: Predicate1(Throw)},
				{module: atomUser, name: NewAtom("foo"), arity: 1}: {
					multifile: true,
					procedure: clauses{
						{
							pi:  procedureIndicator{name: NewAtom("foo"), arity: 1},
							raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("c")}},
							bytecode: bytecode{
								{opcode: opGetConst, operand: NewAtom("c")},
								{opcode: opExit},
							},
						},
					},
				},
			}
			vm.FS = testdata
			err := vm.Compile(context.Background(), tt.text, tt.args...)
			assert.Equal(t, tt.err, err)
			if tt.err == nil {
				delete(vm.procedures, procedureIndicator{module: atomUser, name: NewAtom("throw"), arity: 1})
				assert.Equal(t, tt.result, vm.procedures)
			}
		})
	}
}

func TestVM_Consult(t *testing.T) {
	x := NewVariable()

	tests := []struct {
		title string
		files Term
		ok    bool
		err   error
	}{
		{title: `:- consult('testdata/empty.txt').`, files: NewAtom("testdata/empty.txt"), ok: true},
		{title: `:- consult([]).`, files: List(), ok: true},
		{title: `:- consult(['testdata/empty.txt']).`, files: List(NewAtom("testdata/empty.txt")), ok: true},
		{title: `:- consult(['testdata/empty.txt', 'testdata/empty.txt']).`, files: List(NewAtom("testdata/empty.txt"), NewAtom("testdata/empty.txt")), ok: true},

		{title: `:- consult('testdata/abc.txt').`, files: NewAtom("testdata/abc.txt"), err: io.EOF},
		{title: `:- consult(['testdata/abc.txt']).`, files: List(NewAtom("testdata/abc.txt")), err: io.EOF},

		{title: `:- consult(X).`, files: x, err: InstantiationError(nil)},
		{title: `:- consult(foo(bar)).`, files: NewAtom("foo").Apply(NewAtom("bar")), err: typeError(validTypeAtom, NewAtom("foo").Apply(NewAtom("bar")), nil)},
		{title: `:- consult(1).`, files: Integer(1), err: typeError(validTypeAtom, Integer(1), nil)},
		{title: `:- consult(['testdata/empty.txt'|_]).`, files: PartialList(NewVariable(), NewAtom("testdata/empty.txt")), err: typeError(validTypeAtom, PartialList(NewVariable(), NewAtom("testdata/empty.txt")), nil)},
		{title: `:- consult([X]).`, files: List(x), err: InstantiationError(nil)},
		{title: `:- consult([1]).`, files: List(Integer(1)), err: typeError(validTypeAtom, Integer(1), nil)},

		{title: `:- consult('testdata/not_found.txt').`, files: NewAtom("testdata/not_found.txt"), err: existenceError(objectTypeSourceSink, NewAtom("testdata/not_found.txt"), nil)},
		{title: `:- consult(['testdata/not_found.txt']).`, files: List(NewAtom("testdata/not_found.txt")), err: existenceError(objectTypeSourceSink, NewAtom("testdata/not_found.txt"), nil)},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			vm := VM{
				FS: testdata,
			}
			ok, err := Consult(&vm, tt.files, Success, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			if e, ok := tt.err.(Exception); ok {
				_, ok := NewEnv().Unify(e.Term(), err.(Exception).Term())
				assert.True(t, ok)
			} else {
				assert.Equal(t, tt.err, err)
			}
		})
	}
}

func TestDiscontiguousError_Error(t *testing.T) {
	e := discontiguousError{pi: procedureIndicator{name: NewAtom("foo"), arity: 1}}
	assert.Equal(t, "foo/1 is discontiguous", e.Error())
}
