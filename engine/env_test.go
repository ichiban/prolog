package engine

import (
	"fmt"
	"math/rand"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestEnv_Bind(t *testing.T) {
	var env *Env
	assert.Equal(t, &Env{
		color: black,
		left: &Env{
			binding: binding{
				key:   newEnvKey(NewNamedVariable("A")),
				value: NewAtom("a"),
			},
		},
		binding: binding{
			key:   newEnvKey(varContext),
			value: NewAtom("root"),
		},
	}, env.Bind(NewNamedVariable("A"), NewAtom("a")))
}

func TestEnv_Lookup(t *testing.T) {
	vars := make([]Variable, 1000)
	for i := range vars {
		vars[i] = NewNamedVariable(fmt.Sprintf("V%d", i))
	}

	rand.Shuffle(len(vars), func(i, j int) {
		vars[i], vars[j] = vars[j], vars[i]
	})

	var env *Env
	for _, v := range vars {
		env = env.Bind(v, v)
	}

	rand.Shuffle(len(vars), func(i, j int) {
		vars[i], vars[j] = vars[j], vars[i]
	})

	for _, v := range vars {
		t.Run(v.String(), func(t *testing.T) {
			w, ok := env.Lookup(v)
			assert.True(t, ok)
			assert.Equal(t, v, w)
		})
	}
}

func TestEnv_Simplify(t *testing.T) {
	// L = [a, b|L] ==> [a, b, a, b, ...]
	l := NewNamedVariable("L")
	env := NewEnv().Bind(l, PartialList(l, NewAtom("a"), NewAtom("b")))
	c := env.Simplify(l)
	iter := ListIterator{List: c, Env: env}
	assert.True(t, iter.Next())
	assert.Equal(t, NewAtom("a"), iter.Current())
	assert.True(t, iter.Next())
	assert.Equal(t, NewAtom("b"), iter.Current())
	assert.False(t, iter.Next())
	suffix, ok := iter.Suffix().(*compound)
	assert.True(t, ok)
	assert.Equal(t, atomDot, suffix.functor)
	assert.Len(t, suffix.args, 2)
}

func TestEnv_Compare(t *testing.T) {
	type A struct{}
	type B struct{ b interface{} }
	type C struct{}

	n, m := 1, 1

	tests := []struct {
		title string
		x, y  Term
		o     Order
	}{
		{title: `X > W`, x: NewNamedVariable("X"), y: NewNamedVariable("W"), o: OrderGreater},
		{title: `X = X`, x: NewNamedVariable("X"), y: NewNamedVariable("X"), o: OrderEqual},
		{title: `X < Y`, x: NewNamedVariable("X"), y: NewNamedVariable("Y"), o: OrderLess},
		{title: `X < 0.0`, x: NewNamedVariable("X"), y: Float(0), o: OrderLess},
		{title: `X < 0`, x: NewNamedVariable("X"), y: Integer(0), o: OrderLess},
		{title: `X < a`, x: NewNamedVariable("X"), y: NewAtom("a"), o: OrderLess},
		{title: `X < f(a)`, x: NewNamedVariable("X"), y: NewAtom("f").Apply(NewAtom("a")), o: OrderLess},
		{title: `X < any`, x: NewNamedVariable("X"), y: A{}, o: OrderLess},

		{title: `1.0 > X`, x: Float(1), y: NewNamedVariable("X"), o: OrderGreater},
		{title: `1.0 > 0.0`, x: Float(1), y: Float(0), o: OrderGreater},
		{title: `1.0 = 1.0`, x: Float(1), y: Float(1), o: OrderEqual},
		{title: `1.0 < 2.0`, x: Float(1), y: Float(2), o: OrderLess},
		{title: `1.0 < 1`, x: Float(1), y: Integer(1), o: OrderLess},
		{title: `1.0 < a`, x: Float(1), y: NewAtom("a"), o: OrderLess},
		{title: `1.0 < f(a)`, x: Float(1), y: NewAtom("f").Apply(NewAtom("a")), o: OrderLess},
		{title: `1.0 < any`, x: Float(1), y: A{}, o: OrderLess},

		{title: `1 > X`, x: Integer(1), y: NewNamedVariable("X"), o: OrderGreater},
		{title: `1 > 1.0`, x: Integer(1), y: Float(1), o: OrderGreater},
		{title: `1 > 0`, x: Integer(1), y: Integer(0), o: OrderGreater},
		{title: `1 = 1`, x: Integer(1), y: Integer(1), o: OrderEqual},
		{title: `1 < 2`, x: Integer(1), y: Integer(2), o: OrderLess},
		{title: `1 < a`, x: Integer(1), y: NewAtom("a"), o: OrderLess},
		{title: `1 < f(a)`, x: Integer(1), y: NewAtom("f").Apply(NewAtom("a")), o: OrderLess},
		{title: `1 < any`, x: Integer(1), y: A{}, o: OrderLess},

		{title: `a > X`, x: NewAtom("a"), y: NewNamedVariable("X"), o: OrderGreater},
		{title: `a > 1.0`, x: NewAtom("a"), y: Float(1), o: OrderGreater},
		{title: `a > 1`, x: NewAtom("a"), y: Integer(1), o: OrderGreater},
		{title: `a > 'Z'`, x: NewAtom("a"), y: NewAtom("Z"), o: OrderGreater},
		{title: `a = a`, x: NewAtom("a"), y: NewAtom("a"), o: OrderEqual},
		{title: `a < b`, x: NewAtom("a"), y: NewAtom("b"), o: OrderLess},
		{title: `a < f(a)`, x: NewAtom("a"), y: NewAtom("f").Apply(NewAtom("a")), o: OrderLess},
		{title: `a < any`, x: NewAtom("a"), y: A{}, o: OrderLess},

		{title: `f(a) > X`, x: NewAtom("f").Apply(NewAtom("a")), y: NewNamedVariable("X"), o: OrderGreater},
		{title: `f(a) > 1.0`, x: NewAtom("f").Apply(NewAtom("a")), y: Float(1), o: OrderGreater},
		{title: `f(a) > 1`, x: NewAtom("f").Apply(NewAtom("a")), y: Integer(1), o: OrderGreater},
		{title: `f(a) > a`, x: NewAtom("f").Apply(NewAtom("a")), y: NewAtom("a"), o: OrderGreater}, {title: `f(a) > f('Z')`, x: NewAtom("f").Apply(NewAtom("a")), y: NewAtom("f").Apply(NewAtom("Z")), o: OrderGreater},
		{title: `f(a) > e(a)`, x: NewAtom("f").Apply(NewAtom("a")), y: NewAtom("e").Apply(NewAtom("a")), o: OrderGreater},
		{title: `f(a, b) > f(a)`, x: NewAtom("f").Apply(NewAtom("a"), NewAtom("b")), y: NewAtom("f").Apply(NewAtom("a")), o: OrderGreater},
		{title: `f(a) = f(a)`, x: NewAtom("f").Apply(NewAtom("a")), y: NewAtom("f").Apply(NewAtom("a")), o: OrderEqual},
		{title: `f(a) < g(a)`, x: NewAtom("f").Apply(NewAtom("a")), y: NewAtom("g").Apply(NewAtom("a")), o: OrderLess},
		{title: `f(a) < f(a,b)`, x: NewAtom("f").Apply(NewAtom("a")), y: NewAtom("f").Apply(NewAtom("a"), NewAtom("b")), o: OrderLess},
		{title: `f(a) < f(b)`, x: NewAtom("f").Apply(NewAtom("a")), y: NewAtom("f").Apply(NewAtom("b")), o: OrderLess},
		{title: `f(a) < any`, x: NewAtom("f").Apply(NewAtom("a")), y: A{}, o: OrderLess},

		{title: `nil = nil`, x: nil, y: nil, o: OrderEqual},

		{title: `A{} > X`, x: A{}, y: NewNamedVariable("X"), o: OrderGreater},
		{title: `A{} > 1.0`, x: A{}, y: Float(1), o: OrderGreater},
		{title: `A{} > 1`, x: A{}, y: Integer(1), o: OrderGreater},
		{title: `A{} > a`, x: A{}, y: NewAtom("a"), o: OrderGreater},
		{title: `A{} > f(a)`, x: A{}, y: NewAtom("f").Apply(NewAtom("a")), o: OrderGreater},
		{title: `B{} > A{}`, x: B{}, y: A{}, o: OrderGreater},
		{title: `B{2} = B{1}`, x: B{b: 2}, y: B{b: 1}, o: OrderGreater},
		{title: `B{} = B{}`, x: B{}, y: B{}, o: OrderEqual},
		{title: `B{1} = B{2}`, x: B{b: 1}, y: B{b: 2}, o: OrderLess},
		{title: `B{} < C{}`, x: B{}, y: C{}, o: OrderLess},

		{title: `true > false`, x: true, y: false, o: OrderGreater},
		{title: `false = false`, x: false, y: false, o: OrderEqual},
		{title: `true = true`, x: true, y: true, o: OrderEqual},
		{title: `false < true`, x: false, y: true, o: OrderLess},

		{title: `2 > 1`, x: 2, y: 1, o: OrderGreater},
		{title: `1 = 1`, x: 1, y: 1, o: OrderEqual},
		{title: `1 < 2`, x: 1, y: 2, o: OrderLess},

		{title: `uint(2) > uint(1)`, x: uint(2), y: uint(1), o: OrderGreater},
		{title: `uint(1) = uint(1)`, x: uint(1), y: uint(1), o: OrderEqual},
		{title: `uint(1) < uint(2)`, x: uint(1), y: uint(2), o: OrderLess},

		{title: `2.0 > 1.0`, x: 2.0, y: 1.0, o: OrderGreater},
		{title: `1.0 = 1.0`, x: 1.0, y: 1.0, o: OrderEqual},
		{title: `1.0 < 2.0`, x: 1.0, y: 2.0, o: OrderLess},

		{title: `2 + 1i > 1 + 1i`, x: 2 + 1i, y: 1 + 1i, o: OrderGreater},
		{title: `1 + 2i > 1 + 1i`, x: 1 + 2i, y: 1 + 1i, o: OrderGreater},
		{title: `1 + 1i = 1 + 1i`, x: 1 + 1i, y: 1 + 1i, o: OrderEqual},
		{title: `1 + 1i < 2 + 1i`, x: 1 + 1i, y: 2 + 1i, o: OrderLess},
		{title: `1 + 1i < 1 + 2i`, x: 1 + 1i, y: 1 + 2i, o: OrderLess},

		{title: `{1,2,3} > {1,2}`, x: []int{1, 2, 3}, y: []int{1, 2}, o: OrderGreater},
		{title: `{1,2} > {1,1}`, x: []int{1, 2}, y: []int{1, 1}, o: OrderGreater},
		{title: `{1,2} = {1,2}`, x: []int{1, 2}, y: []int{1, 2}, o: OrderEqual},
		{title: `{1,1} < {1,2}`, x: []int{1, 1}, y: []int{1, 2}, o: OrderLess},
		{title: `{1,2} < {1,2,3}`, x: []int{1, 2}, y: []int{1, 2, 3}, o: OrderLess},

		{title: `&m > &n`, x: &m, y: &n, o: OrderGreater},
		{title: `&n = &n`, x: &n, y: &n, o: OrderEqual},
		{title: `&n < &m`, x: &n, y: &m, o: OrderLess},

		{title: `{a:1} > {}`, x: map[string]int{"a": 1}, y: map[string]int{}, o: OrderGreater},
		{title: `{a:1} = {a:1}`, x: map[string]int{"a": 1}, y: map[string]int{"a": 1}, o: OrderEqual},
		{title: `{} < {a:1}`, x: map[string]int{}, y: map[string]int{"a": 1}, o: OrderLess},

		{title: `def > abc`, x: "def", y: "abc", o: OrderGreater},
		{title: `abc = abc`, x: "abc", y: "abc", o: OrderEqual},
		{title: `abc < def`, x: "abc", y: "def", o: OrderLess},
	}

	env := NewEnv()
	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			assert.Equal(t, tt.o, env.Compare(tt.x, tt.y))
		})
	}
}

func TestContains(t *testing.T) {
	var env *Env
	assert.True(t, contains(NewAtom("a"), NewAtom("a"), env))
	assert.False(t, contains(NewVariable(), NewAtom("a"), env))
	v := NewNamedVariable("V")
	env = env.Bind(v, NewAtom("a"))
	assert.True(t, contains(v, NewAtom("a"), env))
	assert.True(t, contains(&compound{functor: NewAtom("a")}, NewAtom("a"), env))
	assert.True(t, contains(&compound{functor: NewAtom("f"), args: []Term{NewAtom("a")}}, NewAtom("a"), env))
	assert.False(t, contains(&compound{functor: NewAtom("f")}, NewAtom("a"), env))
}
