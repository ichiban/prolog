package engine

import (
	"bytes"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestCompound_Unify(t *testing.T) {
	unit := Compound{
		Functor: "foo",
		Args:    []Term{Atom("bar")},
	}

	t.Run("atom", func(t *testing.T) {
		env, ok := unit.Unify(Atom("foo"), false, nil)
		assert.False(t, ok)
		assert.Nil(t, env)
	})

	t.Run("integer", func(t *testing.T) {
		env, ok := unit.Unify(Integer(1), false, nil)
		assert.False(t, ok)
		assert.Nil(t, env)
	})

	t.Run("variable", func(t *testing.T) {
		t.Run("free", func(t *testing.T) {
			v := Variable("X")
			env, ok := unit.Unify(v, false, nil)
			assert.True(t, ok)
			assert.Equal(t, &unit, env.Resolve(v))
		})
		t.Run("bound to the same value", func(t *testing.T) {
			v := Variable("X")
			env := NewEnv().
				Bind(v, &unit)
			_, ok := unit.Unify(v, false, env)
			assert.True(t, ok)
		})
		t.Run("bound to a different value", func(t *testing.T) {
			v := Variable("X")
			env := NewEnv().
				Bind(v, &Compound{
					Functor: "foo",
					Args:    []Term{Atom("baz")},
				})

			_, ok := unit.Unify(&v, false, env)
			assert.False(t, ok)
		})
	})

	t.Run("compound", func(t *testing.T) {
		_, ok := unit.Unify(&Compound{
			Functor: "foo",
			Args:    []Term{Atom("bar")},
		}, false, nil)
		assert.True(t, ok)
		_, ok = unit.Unify(&Compound{
			Functor: "foo",
			Args:    []Term{Atom("bar"), Atom("baz")},
		}, false, nil)
		assert.False(t, ok)
		_, ok = unit.Unify(&Compound{
			Functor: "baz",
			Args:    []Term{Atom("bar")},
		}, false, nil)
		assert.False(t, ok)
		_, ok = unit.Unify(&Compound{
			Functor: "foo",
			Args:    []Term{Atom("baz")},
		}, false, nil)
		assert.False(t, ok)
		v := Variable("X")
		env, ok := unit.Unify(&Compound{
			Functor: "foo",
			Args:    []Term{v},
		}, false, nil)
		assert.True(t, ok)
		assert.Equal(t, Atom("bar"), env.Resolve(v))
	})
}

func TestCompound_WriteTerm(t *testing.T) {
	v := Variable("L")
	l := ListRest(v, Atom("a"), Atom("b"))
	env := NewEnv().Bind(v, l)

	ops := operators{
		{priority: 1200, specifier: operatorSpecifierXFX, name: `:-`},
		{priority: 1200, specifier: operatorSpecifierFX, name: `:-`},
		{priority: 1200, specifier: operatorSpecifierXF, name: `-:`},
		{priority: 900, specifier: operatorSpecifierFY, name: `\+`},
		{priority: 900, specifier: operatorSpecifierYF, name: `+/`},
		{priority: 500, specifier: operatorSpecifierYFX, name: `+`},
		{priority: 400, specifier: operatorSpecifierYFX, name: `*`},
		{priority: 200, specifier: operatorSpecifierFY, name: `-`},
		{priority: 200, specifier: operatorSpecifierYF, name: `--`},
	}

	tests := []struct {
		title      string
		ignoreOps  bool
		numberVars bool
		compound   Term
		output     string
	}{
		{title: "list", compound: List(Atom(`a`), Atom(`b`), Atom(`c`)), output: `[a,b,c]`},
		{title: "list-ish", compound: ListRest(Atom(`rest`), Atom(`a`), Atom(`b`)), output: `[a,b|rest]`},
		{title: "circular list", compound: l, output: `[a,b,a|...]`},
		{title: "curly brackets", compound: &Compound{Functor: `{}`, Args: []Term{Atom(`foo`)}}, output: `{foo}`},
		{title: "fx", compound: &Compound{Functor: `:-`, Args: []Term{&Compound{Functor: `:-`, Args: []Term{Atom(`foo`)}}}}, output: `:- (:-foo)`},
		{title: "fy", compound: &Compound{Functor: `\+`, Args: []Term{&Compound{Functor: `-`, Args: []Term{&Compound{Functor: `\+`, Args: []Term{Atom(`foo`)}}}}}}, output: `\+ -(\+foo)`},
		{title: "xf", compound: &Compound{Functor: `-:`, Args: []Term{&Compound{Functor: `-:`, Args: []Term{Atom(`foo`)}}}}, output: `(foo-:)-:`},
		{title: "yf", compound: &Compound{Functor: `+/`, Args: []Term{&Compound{Functor: `--`, Args: []Term{&Compound{Functor: `+/`, Args: []Term{Atom(`foo`)}}}}}}, output: `(foo+/)-- +/`},
		{title: "xfx", compound: &Compound{Functor: ":-", Args: []Term{Atom("foo"), &Compound{Functor: ":-", Args: []Term{Atom("bar"), Atom("baz")}}}}, output: `foo:-(bar:-baz)`},
		{title: "yfx", compound: &Compound{Functor: "*", Args: []Term{Integer(2), &Compound{Functor: "+", Args: []Term{Integer(2), Integer(2)}}}}, output: `2*(2+2)`},
		{title: "ignore_ops(false)", ignoreOps: false, compound: &Compound{Functor: "+", Args: []Term{Integer(2), Integer(-2)}}, output: `2 + -2`},
		{title: "ignore_ops(true)", ignoreOps: true, compound: &Compound{Functor: "+", Args: []Term{Integer(2), Integer(-2)}}, output: `+(2,-2)`},
		{title: "number_vars(false)", numberVars: false, compound: &Compound{Functor: "f", Args: []Term{&Compound{Functor: "$VAR", Args: []Term{Integer(0)}}, &Compound{Functor: "$VAR", Args: []Term{Integer(1)}}, &Compound{Functor: "$VAR", Args: []Term{Integer(25)}}, &Compound{Functor: "$VAR", Args: []Term{Integer(26)}}, &Compound{Functor: "$VAR", Args: []Term{Integer(27)}}}}, output: `f('$VAR'(0),'$VAR'(1),'$VAR'(25),'$VAR'(26),'$VAR'(27))`},
		{title: "number_vars(true)", numberVars: true, compound: &Compound{Functor: "f", Args: []Term{&Compound{Functor: "$VAR", Args: []Term{Integer(0)}}, &Compound{Functor: "$VAR", Args: []Term{Integer(1)}}, &Compound{Functor: "$VAR", Args: []Term{Integer(25)}}, &Compound{Functor: "$VAR", Args: []Term{Integer(26)}}, &Compound{Functor: "$VAR", Args: []Term{Integer(27)}}}}, output: `f(A,B,Z,A1,B1)`},
	}

	var buf bytes.Buffer
	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			buf.Reset()
			assert.NoError(t, tt.compound.WriteTerm(&buf, &WriteOptions{
				IgnoreOps:  tt.ignoreOps,
				Quoted:     true,
				NumberVars: tt.numberVars,
				ops:        ops,
				priority:   1201,
			}, env))
			assert.Equal(t, tt.output, buf.String())
		})
	}
}

func TestEnv_Set(t *testing.T) {
	env := NewEnv()
	assert.Equal(t, List(), env.Set())
	assert.Equal(t, List(Atom("a")), env.Set(Atom("a")))
	assert.Equal(t, List(Atom("a")), env.Set(Atom("a"), Atom("a"), Atom("a")))
	assert.Equal(t, List(Atom("a"), Atom("b"), Atom("c")), env.Set(Atom("c"), Atom("b"), Atom("a")))
}

func TestSeq(t *testing.T) {
	assert.Equal(t, Atom("a"), Seq(",", Atom("a")))
	assert.Equal(t, &Compound{
		Functor: ",",
		Args: []Term{
			Atom("a"),
			Atom("b"),
		},
	}, Seq(",", Atom("a"), Atom("b")))
	assert.Equal(t, &Compound{
		Functor: ",",
		Args: []Term{
			Atom("a"),
			&Compound{
				Functor: ",",
				Args: []Term{
					Atom("b"),
					Atom("c"),
				},
			},
		},
	}, Seq(",", Atom("a"), Atom("b"), Atom("c")))
}

func TestCompound_Compare(t *testing.T) {
	var m mockTerm
	defer m.AssertExpectations(t)

	assert.Equal(t, int64(-1), (&Compound{Functor: "f"}).Compare(&Compound{Functor: "g"}, nil))
	assert.Equal(t, int64(-1), (&Compound{Functor: "f", Args: make([]Term, 1)}).Compare(&Compound{Functor: "f", Args: make([]Term, 2)}, nil))
	assert.Equal(t, int64(-1), (&Compound{Functor: "f", Args: []Term{Atom("a"), Atom("a")}}).Compare(&Compound{Functor: "f", Args: []Term{Atom("a"), Atom("b")}}, nil))
	assert.Equal(t, int64(0), (&Compound{Functor: "f", Args: []Term{Atom("a"), Atom("b")}}).Compare(&Compound{Functor: "f", Args: []Term{Atom("a"), Atom("b")}}, nil))
	assert.Equal(t, int64(1), (&Compound{Functor: "f", Args: []Term{Atom("a"), Atom("b")}}).Compare(&Compound{Functor: "f", Args: []Term{Atom("a"), Atom("a")}}, nil))
	assert.Equal(t, int64(1), (&Compound{Functor: "f", Args: make([]Term, 2)}).Compare(&Compound{Functor: "f", Args: make([]Term, 1)}, nil))
	assert.Equal(t, int64(1), (&Compound{Functor: "g"}).Compare(&Compound{Functor: "f"}, nil))
	assert.Equal(t, int64(1), (&Compound{Functor: "f"}).Compare(&m, nil))
}
