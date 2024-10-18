package internal

import (
	"bytes"
	"errors"
	"fmt"
	"testing"
)

func TestTermPool_Compound(t *testing.T) {
	pool := NewHeap(2 * 1024)

	must := func(id Term, err error) Term {
		if err != nil {
			t.Fatal(err)
		}
		return id
	}

	a := must(pool.PutAtom(Atom('a')))
	b := must(pool.PutAtom(Atom('b')))
	c := must(pool.PutAtom(Atom('c')))
	empty := must(pool.PutAtom(atomEmptyList))

	fabc := must(pool.PutCompound(Atom('f'), a, b, c))
	labc := must(pool.PutList(a, b, c))
	lbc := must(pool.PutList(b, c))
	lc := must(pool.PutList(c))
	sabc := must(pool.PutString("abc"))
	sbc := must(pool.PutString("bc"))
	sc := must(pool.PutString("c"))

	tests := []struct {
		title   string
		term    Term
		functor Functor
		args    []Term
		ok      bool
	}{
		{title: "f(a, b, c)", term: fabc, functor: Functor{Name: Atom('f'), Arity: 3}, args: []Term{a, b, c}, ok: true},
		{title: "[a, b, c]", term: labc, functor: Functor{Name: Atom('.'), Arity: 2}, args: []Term{a, lbc}, ok: true},
		{title: "[b, c]", term: lbc, functor: Functor{Name: Atom('.'), Arity: 2}, args: []Term{b, lc}, ok: true},
		{title: "[c]", term: lc, functor: Functor{Name: Atom('.'), Arity: 2}, args: []Term{c, empty}, ok: true},
		{title: `"abc"`, term: sabc, functor: Functor{Name: Atom('.'), Arity: 2}, args: []Term{a, lbc}, ok: true},
		{title: `"bc"`, term: sbc, functor: Functor{Name: Atom('.'), Arity: 2}, args: []Term{b, lc}, ok: true},
		{title: `"c"`, term: sc, functor: Functor{Name: Atom('.'), Arity: 2}, args: []Term{c, empty}, ok: true},
		{title: "a", term: a, ok: false},
	}
	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			f, arg, ok := pool.Compound(tt.term)
			if ok != tt.ok {
				t.Errorf("ok = %v; want %v", ok, tt.ok)
			}
			if f != tt.functor {
				t.Errorf("f = %v; want %v", f, tt.functor)
			}

			var args []Term
			for i := 0; i < f.Arity; i++ {
				a, err := arg(i)
				if err != nil {
					t.Fatal(err)
				}
				args = append(args, a)
			}
			if len(args) != len(tt.args) {
				t.Errorf("len(%s) = %d; want %d", tt.title, len(args), len(args))
			}
			for i, a := range args {
				o, err := pool.Compare(a, tt.args[i])
				if err != nil {
					t.Fatal(err)
				}
				if o != 0 {
					t.Errorf("args[%d] = %d; want 0", i, o)
				}
			}
		})
	}
}

func TestTermPool_Write(t *testing.T) {
	pool := NewHeap(1024 * 1024)
	must := func(id Term, err error) Term {
		if err != nil {
			t.Fatal(err)
		}
		return id
	}

	varX := NewVariable(&pool)

	x, err := pool.PutVariable(varX)
	if err != nil {
		t.Fatal(err)
	}

	empty, err := pool.PutAtom(NewAtom(""))
	if err != nil {
		t.Fatal(err)
	}

	a, err := pool.PutAtom(NewAtom(`a`))
	if err != nil {
		t.Fatal(err)
	}

	b, err := pool.PutAtom(NewAtom(`b`))
	if err != nil {
		t.Fatal(err)
	}

	c, err := pool.PutAtom(NewAtom(`c`))
	if err != nil {
		t.Fatal(err)
	}

	rest, err := pool.PutAtom(NewAtom("rest"))
	if err != nil {
		t.Fatal(err)
	}

	foo, err := pool.PutAtom(NewAtom(`foo`))
	if err != nil {
		t.Fatal(err)
	}

	bar, err := pool.PutAtom(NewAtom(`bar`))
	if err != nil {
		t.Fatal(err)
	}

	baz, err := pool.PutAtom(NewAtom(`baz`))
	if err != nil {
		t.Fatal(err)
	}

	gibberish, err := pool.PutAtom(NewAtom("\a\b\f\n\r\t\v\u0000\\'\"`"))
	if err != nil {
		t.Fatal(err)
	}

	i0, err := pool.PutInteger(0)
	if err != nil {
		t.Fatal(err)
	}

	i1, err := pool.PutInteger(1)
	if err != nil {
		t.Fatal(err)
	}

	i2, err := pool.PutInteger(2)
	if err != nil {
		t.Fatal(err)
	}

	i25, err := pool.PutInteger(25)
	if err != nil {
		t.Fatal(err)
	}

	i26, err := pool.PutInteger(26)
	if err != nil {
		t.Fatal(err)
	}

	i27, err := pool.PutInteger(27)
	if err != nil {
		t.Fatal(err)
	}

	i33, err := pool.PutInteger(33)
	if err != nil {
		t.Fatal(err)
	}

	mi2, err := pool.PutInteger(-2)
	if err != nil {
		t.Fatal(err)
	}

	mi33, err := pool.PutInteger(-33)
	if err != nil {
		t.Fatal(err)
	}

	f33, err := pool.PutFloat(33)
	if err != nil {
		t.Fatal(err)
	}

	f3e100, err := pool.PutFloat(3.0e+100)
	if err != nil {
		t.Fatal(err)
	}

	mf33, err := pool.PutFloat(-33)
	if err != nil {
		t.Fatal(err)
	}

	listABC, err := pool.PutList(a, b, c)
	if err != nil {
		t.Fatal(err)
	}

	partialListRestAB, err := pool.PutPartialList(rest, a, b)
	if err != nil {
		t.Fatal(err)
	}

	circularRest, err := pool.PutVariable(NewVariable(&pool))
	if err != nil {
		t.Fatal(err)
	}

	circularList, err := pool.PutPartialList(circularRest, a, b)
	if err != nil {
		t.Fatal(err)
	}

	if _, err := pool.Unify(circularRest, circularList); err != nil {
		t.Fatal(err)
	}

	curlyFoo, err := pool.PutCompound(atomEmptyBlock, foo)
	if err != nil {
		t.Fatal(err)
	}

	colonMinusFoo, err := pool.PutCompound(NewAtom(":-"), foo)
	if err != nil {
		t.Fatal(err)
	}

	colonMinusColonMinusFoo, err := pool.PutCompound(NewAtom(":-"), colonMinusFoo)
	if err != nil {
		t.Fatal(err)
	}

	notFoo, err := pool.PutCompound(NewAtom(`\+`), foo)
	if err != nil {
		t.Fatal(err)
	}

	minusNotFoo, err := pool.PutCompound(Atom('-'), notFoo)
	if err != nil {
		t.Fatal(err)
	}

	notMinusNotFoo, err := pool.PutCompound(NewAtom(`\+`), minusNotFoo)
	if err != nil {
		t.Fatal(err)
	}

	minusColonFoo, err := pool.PutCompound(NewAtom(`-:`), foo)
	if err != nil {
		t.Fatal(err)
	}

	minusColonMinusColonFoo, err := pool.PutCompound(NewAtom(`-:`), minusColonFoo)
	if err != nil {
		t.Fatal(err)
	}

	tonFoo, err := pool.PutCompound(NewAtom(`+/`), foo)
	if err != nil {
		t.Fatal(err)
	}

	minusMinusTonFoo, err := pool.PutCompound(NewAtom(`--`), tonFoo)
	if err != nil {
		t.Fatal(err)
	}

	tonMinusMinusTonFoo, err := pool.PutCompound(NewAtom(`+/`), minusMinusTonFoo)
	if err != nil {
		t.Fatal(err)
	}

	colonMinusBarBaz, err := pool.PutCompound(NewAtom(`:-`), bar, baz)
	if err != nil {
		t.Fatal(err)
	}

	colonMinusFooColonMinusBarBaz, err := pool.PutCompound(NewAtom(`:-`), foo, colonMinusBarBaz)
	if err != nil {
		t.Fatal(err)
	}

	plusI2I2, err := pool.PutCompound(Atom('+'), i2, i2)
	if err != nil {
		t.Fatal(err)
	}

	asteriskI2PlusI2I2, err := pool.PutCompound(Atom('*'), i2, plusI2I2)
	if err != nil {
		t.Fatal(err)
	}

	barI2I2, err := pool.PutCompound(Atom('|'), i2, i2)
	if err != nil {
		t.Fatal(err)
	}

	commaI2BarI2I2, err := pool.PutCompound(Atom(','), i2, barI2I2)
	if err != nil {
		t.Fatal(err)
	}

	plusI2MI2, err := pool.PutCompound(Atom('+'), i2, mi2)
	if err != nil {
		t.Fatal(err)
	}

	varI0, err := pool.PutCompound(NewAtom("$VAR"), i0)
	if err != nil {
		t.Fatal(err)
	}

	varI1, err := pool.PutCompound(NewAtom("$VAR"), i1)
	if err != nil {
		t.Fatal(err)
	}

	varI25, err := pool.PutCompound(NewAtom("$VAR"), i25)
	if err != nil {
		t.Fatal(err)
	}

	varI26, err := pool.PutCompound(NewAtom("$VAR"), i26)
	if err != nil {
		t.Fatal(err)
	}

	varI27, err := pool.PutCompound(NewAtom("$VAR"), i27)
	if err != nil {
		t.Fatal(err)
	}

	fVarI0VarI1VarI25VarI26VarI27, err := pool.PutCompound(Atom('f'), varI0, varI1, varI25, varI26, varI27)
	if err != nil {
		t.Fatal(err)
	}

	minusB, err := pool.PutCompound(Atom('-'), b)
	if err != nil {
		t.Fatal(err)
	}

	asteriskAMinusB, err := pool.PutCompound(Atom('*'), a, minusB)
	if err != nil {
		t.Fatal(err)
	}

	tonA, err := pool.PutCompound(NewAtom(`+/`), a)
	if err != nil {
		t.Fatal(err)
	}

	minusTonA, err := pool.PutCompound(Atom('-'), tonA)
	if err != nil {
		t.Fatal(err)
	}

	asteriskAB, err := pool.PutCompound(Atom('*'), a, b)
	if err != nil {
		t.Fatal(err)
	}

	minusAsteriskAB, err := pool.PutCompound(Atom('-'), asteriskAB)
	if err != nil {
		t.Fatal(err)
	}

	varRecursive, err := pool.PutVariable(NewVariable(&pool))
	if err != nil {
		t.Fatal(err)
	}
	r, err := pool.PutCompound(Atom('f'), varRecursive)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := pool.Unify(varRecursive, r); err != nil {
		t.Fatal(err)
	}

	ops := operators{}
	ops.define(1200, OperatorSpecifierXFX, NewAtom(`:-`))
	ops.define(1200, OperatorSpecifierFX, NewAtom(`:-`))
	ops.define(1200, OperatorSpecifierXF, NewAtom(`-:`))
	ops.define(1105, OperatorSpecifierXFY, NewAtom(`|`))
	ops.define(1000, OperatorSpecifierXFY, NewAtom(`,`))
	ops.define(900, OperatorSpecifierFY, NewAtom(`\+`))
	ops.define(900, OperatorSpecifierYF, NewAtom(`+/`))
	ops.define(500, OperatorSpecifierYFX, NewAtom(`+`))
	ops.define(400, OperatorSpecifierYFX, NewAtom(`*`))
	ops.define(200, OperatorSpecifierFY, NewAtom(`-`))
	ops.define(200, OperatorSpecifierYF, NewAtom(`--`))

	tests := []struct {
		title  string
		termID Term
		opts   WriteOptions
		output string
		err    error
	}{
		{title: "variable: unnamed", termID: x, output: fmt.Sprintf("_%d", varX)},
		{title: "variable: variable_names", termID: x, opts: WriteOptions{variableNames: map[Variable]Atom{varX: NewAtom("Foo")}}, output: `Foo`},

		{title: "atom: empty", termID: empty, opts: WriteOptions{}, output: ""},
		{title: `atom: a`, termID: a, opts: WriteOptions{quoted: false}, output: `a`},
		{title: `atom: a`, termID: a, opts: WriteOptions{quoted: true}, output: `a`},
		{title: "atom: \a\b\f\n\r\t\v\x00\\'\"`", termID: gibberish, opts: WriteOptions{quoted: false}, output: "\a\b\f\n\r\t\v\x00\\'\"`"},
		{title: "atom: \a\b\f\n\r\t\v\x00\\'\"`", termID: gibberish, opts: WriteOptions{quoted: true}, output: "'\\a\\b\\f\\n\\r\\t\\v\\x0\\\\\\\\'\"`'"},
		{title: `atom: ,`, termID: must(pool.PutAtom(Atom(','))), opts: WriteOptions{quoted: false}, output: `,`},
		{title: `atom: ,`, termID: must(pool.PutAtom(Atom(','))), opts: WriteOptions{quoted: true}, output: `','`},
		{title: `atom: []`, termID: must(pool.PutAtom(NewAtom(`[]`))), opts: WriteOptions{quoted: false}, output: `[]`},
		{title: `atom: []`, termID: must(pool.PutAtom(NewAtom(`[]`))), opts: WriteOptions{quoted: true}, output: `[]`},
		{title: `atom: {}`, termID: must(pool.PutAtom(NewAtom(`{}`))), opts: WriteOptions{quoted: false}, output: `{}`},
		{title: `atom: {}`, termID: must(pool.PutAtom(NewAtom(`{}`))), opts: WriteOptions{quoted: true}, output: `{}`},
		{title: `atom: -`, termID: must(pool.PutAtom(Atom('-'))), output: `-`},
		{title: `atom: -`, termID: must(pool.PutAtom(Atom('-'))), opts: WriteOptions{ops: operators{Atom('+'): {}, Atom('-'): {}}, left: Operator{Specifier: OperatorSpecifierFY, Name: Atom('+')}}, output: ` (-)`},
		{title: `atom: -`, termID: must(pool.PutAtom(Atom('-'))), opts: WriteOptions{ops: operators{Atom('+'): {}, Atom('-'): {}}, right: Operator{Name: Atom('+')}}, output: `(-)`},
		{title: `atom: X`, termID: must(pool.PutAtom(Atom('X'))), opts: WriteOptions{quoted: true, left: Operator{Name: NewAtom(`F`)}}, output: ` 'X'`},  // So that it won't be 'F''X'.
		{title: `atom: X`, termID: must(pool.PutAtom(Atom('X'))), opts: WriteOptions{quoted: true, right: Operator{Name: NewAtom(`F`)}}, output: `'X' `}, // So that it won't be 'X''F'.
		{title: `atom: foo`, termID: must(pool.PutAtom(NewAtom("foo"))), opts: WriteOptions{left: Operator{Name: NewAtom(`bar`)}}, output: ` foo`},       // So that it won't be barfoo.
		{title: `atom: foo`, termID: must(pool.PutAtom(NewAtom("foo"))), opts: WriteOptions{right: Operator{Name: NewAtom(`bar`)}}, output: `foo `},      // So that it won't be foobar.

		{title: "integer: positive", termID: i33, output: `33`},
		{title: "integer: positive following unary minus", termID: i33, opts: WriteOptions{left: Operator{Name: Atom('-'), Specifier: OperatorSpecifierFX}}, output: ` (33)`},
		{title: "integer: negative", termID: mi33, output: `-33`},
		{title: "integer: ambiguous 0b", termID: i0, opts: WriteOptions{right: Operator{Name: NewAtom(`b0`)}}, output: `0 `},  // So that it won't be 0b0.
		{title: "integer: ambiguous 0o", termID: i0, opts: WriteOptions{right: Operator{Name: NewAtom(`o0`)}}, output: `0 `},  // So that it won't be 0o0.
		{title: "integer: ambiguous 0x", termID: i0, opts: WriteOptions{right: Operator{Name: NewAtom(`x0`)}}, output: `0 `},  // So that it won't be 0x0.
		{title: "integer: ambiguous 0'", termID: i0, opts: WriteOptions{right: Operator{Name: NewAtom(`Foo`)}}, output: `0 `}, // So that it won't be 0'Foo.

		{title: "float: positive", termID: f33, output: `33.0`},
		{title: "float: with e", termID: f3e100, output: `3.0e+100`},
		{title: "float: positive following unary minus", termID: f33, opts: WriteOptions{left: Operator{Specifier: OperatorSpecifierFX, Name: Atom('-')}}, output: ` (33.0)`},
		{title: "float: negative", termID: mf33, output: `-33.0`},
		{title: "float: ambiguous e", termID: f33, opts: WriteOptions{right: Operator{Name: NewAtom(`e`)}}, output: `33.0 `}, // So that it won't be 33.0e.

		{title: "list", termID: listABC, output: `[a,b,c]`},
		{title: "list-ish", termID: partialListRestAB, output: `[a,b|rest]`},
		{title: "circular list", termID: circularList, output: `[a,b,a|...]`},
		{title: "curly brackets", termID: curlyFoo, output: `{foo}`},
		{title: "fx", termID: colonMinusColonMinusFoo, opts: WriteOptions{ops: ops, priority: 1201}, output: `:- (:-foo)`},
		{title: "fy", termID: notMinusNotFoo, opts: WriteOptions{ops: ops, priority: 1201}, output: `\+ - (\+foo)`},
		{title: "xf", termID: minusColonMinusColonFoo, opts: WriteOptions{ops: ops, priority: 1201}, output: `(foo-:)-:`},
		{title: "yf", termID: tonMinusMinusTonFoo, opts: WriteOptions{ops: ops, priority: 1201}, output: `(foo+/)-- +/`},
		{title: "xfx", termID: colonMinusFooColonMinusBarBaz, opts: WriteOptions{ops: ops, priority: 1201}, output: `foo:-(bar:-baz)`},
		{title: "yfx", termID: asteriskI2PlusI2I2, opts: WriteOptions{ops: ops, priority: 1201}, output: `2*(2+2)`},
		{title: "xfy", termID: commaI2BarI2I2, opts: WriteOptions{ops: ops, priority: 1201}, output: `2,(2|2)`},
		{title: "ignore_ops(false)", termID: plusI2MI2, opts: WriteOptions{ignoreOps: false, ops: ops, priority: 1201}, output: `2+ -2`},
		{title: "ignore_ops(true)", termID: plusI2MI2, opts: WriteOptions{ignoreOps: true, ops: ops, priority: 1201}, output: `+(2,-2)`},
		{title: "number_vars(false)", termID: fVarI0VarI1VarI25VarI26VarI27, opts: WriteOptions{quoted: true, numberVars: false, ops: ops, priority: 1201}, output: `f('$VAR'(0),'$VAR'(1),'$VAR'(25),'$VAR'(26),'$VAR'(27))`},
		{title: "number_vars(true)", termID: fVarI0VarI1VarI25VarI26VarI27, opts: WriteOptions{quoted: true, numberVars: true, ops: ops, priority: 1201}, output: `f(A,B,Z,A1,B1)`},
		{title: "prefix: spacing between operators", termID: asteriskAMinusB, opts: WriteOptions{ops: ops, priority: 1201}, output: `a* -b`},
		{title: "postfix: spacing between unary minus and open/close", termID: minusTonA, opts: WriteOptions{ops: ops, priority: 1201}, output: `- (a+/)`},
		{title: "infix: spacing between unary minus and open/close", termID: minusAsteriskAB, opts: WriteOptions{ops: ops, priority: 1201}, output: `- (a*b)`},
		{title: "recursive", termID: r, output: `f(...)`},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			var b bytes.Buffer
			if err := pool.Write(&b, test.termID, &test.opts); !errors.Is(err, test.err) {
				t.Errorf("expected error %v, got %v", test.err, err)
			}

			if b.String() != test.output {
				t.Errorf("expected %q, got %q", test.output, b.String())
			}
		})
	}
}

func TestTermPool_Compare(t *testing.T) {
	pool := NewHeap(1024 * 1024)

	w, err := pool.PutVariable(NewVariable(&pool))
	if err != nil {
		t.Fatal(err)
	}

	x, err := pool.PutVariable(NewVariable(&pool))
	if err != nil {
		t.Fatal(err)
	}

	y, err := pool.PutVariable(NewVariable(&pool))
	if err != nil {
		t.Fatal(err)
	}

	a, err := pool.PutAtom(Atom('a'))
	if err != nil {
		t.Fatal(err)
	}

	b, err := pool.PutAtom(Atom('b'))
	if err != nil {
		t.Fatal(err)
	}

	z, err := pool.PutAtom(Atom('Z'))
	if err != nil {
		t.Fatal(err)
	}

	i0, err := pool.PutInteger(0)
	if err != nil {
		t.Fatal(err)
	}

	i1, err := pool.PutInteger(1)
	if err != nil {
		t.Fatal(err)
	}

	i2, err := pool.PutInteger(2)
	if err != nil {
		t.Fatal(err)
	}

	f0, err := pool.PutFloat(0)
	if err != nil {
		t.Fatal(err)
	}

	f1, err := pool.PutFloat(1)
	if err != nil {
		t.Fatal(err)
	}

	f2, err := pool.PutFloat(2)
	if err != nil {
		t.Fatal(err)
	}

	fa, err := pool.PutCompound(Atom('f'), a)
	if err != nil {
		t.Fatal(err)
	}

	fb, err := pool.PutCompound(Atom('f'), b)
	if err != nil {
		t.Fatal(err)
	}

	fz, err := pool.PutCompound(Atom('f'), z)
	if err != nil {
		t.Fatal(err)
	}

	ea, err := pool.PutCompound(Atom('e'), a)
	if err != nil {
		t.Fatal(err)
	}

	ga, err := pool.PutCompound(Atom('g'), a)
	if err != nil {
		t.Fatal(err)
	}

	fab, err := pool.PutCompound(Atom('f'), a, b)
	if err != nil {
		t.Fatal(err)
	}

	tests := []struct {
		title    string
		lhs, rhs Term
		o        int
		err      error
	}{
		{title: `X > W`, lhs: x, rhs: w, o: 1},
		{title: `X = X`, lhs: x, rhs: x, o: 0},
		{title: `X < Y`, lhs: x, rhs: y, o: -1},
		{title: `X < 0.0`, lhs: x, rhs: f0, o: -1},
		{title: `X < 0`, lhs: x, rhs: i0, o: -1},
		{title: `X < a`, lhs: x, rhs: a, o: -1},
		{title: `X < f(a)`, lhs: x, rhs: fa, o: -1},

		{title: `a > X`, lhs: a, rhs: x, o: 1},
		{title: `a > 0.0`, lhs: a, rhs: f0, o: 1},
		{title: `a > 0`, lhs: a, rhs: i0, o: 1},
		{title: `a > 'Z'`, lhs: a, rhs: z, o: 1},
		{title: `a = a`, lhs: a, rhs: a, o: 0},
		{title: `a < b`, lhs: a, rhs: b, o: -1},
		{title: `a < f(a)`, lhs: a, rhs: fa, o: -1},

		{title: `1.0 > X`, lhs: f1, rhs: x, o: 1},
		{title: `1.0 > 0.0`, lhs: f1, rhs: f0, o: 1},
		{title: `1.0 = 1.0`, lhs: f1, rhs: f1, o: 0},
		{title: `1.0 < 2.0`, lhs: f1, rhs: f2, o: -1},
		{title: `1.0 < 1`, lhs: f1, rhs: i1, o: -1},
		{title: `1.0 < a`, lhs: f1, rhs: a, o: -1},
		{title: `1.0 < f(a)`, lhs: f1, rhs: fa, o: -1},

		{title: `1 > X`, lhs: i1, rhs: x, o: 1},
		{title: `1 > 1.0`, lhs: i1, rhs: f1, o: 1},
		{title: `1 > 0`, lhs: i1, rhs: i0, o: 1},
		{title: `1 = 1`, lhs: i1, rhs: i1, o: 0},
		{title: `1 < 2`, lhs: i1, rhs: i2, o: -1},
		{title: `1 < a`, lhs: i1, rhs: a, o: -1},
		{title: `1 < f(a)`, lhs: i1, rhs: fa, o: -1},

		{title: `f(a) > X`, lhs: fa, rhs: x, o: 1},
		{title: `f(a) > 0.0`, lhs: fa, rhs: f0, o: 1},
		{title: `f(a) > 0`, lhs: fa, rhs: i0, o: 1},
		{title: `f(a) > a`, lhs: fa, rhs: a, o: 1},
		{title: `f(a) > f('Z')`, lhs: fa, rhs: fz, o: 1},
		{title: `f(a) > e(a)`, lhs: fa, rhs: ea, o: 1},
		{title: `f(a, b) > f(a)`, lhs: fab, rhs: fa, o: 1},
		{title: `f(a) = f(a)`, lhs: fa, rhs: fa, o: 0},
		{title: `f(a) < g(a)`, lhs: fa, rhs: ga, o: -1},
		{title: `f(a) < f(a,b)`, lhs: fa, rhs: fab, o: -1},
		{title: `f(a) < f(b)`, lhs: fa, rhs: fb, o: -1},
	}
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			o, err := pool.Compare(test.lhs, test.rhs)
			if !errors.Is(err, test.err) {
				t.Errorf("expected error %v, got %v", test.err, err)
			}
			if o != test.o {
				t.Errorf("expected %d, got %d", test.o, o)
			}
		})
	}
}
