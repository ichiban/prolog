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
		right: &Env{
			binding: binding{
				variable: "A",
				value:    Atom("a"),
			},
		},
		binding: binding{
			variable: varContext,
			value:    Atom("root"),
		},
	}, env.Bind("A", Atom("a")))
}

func TestEnv_Lookup(t *testing.T) {
	vars := make([]Variable, 1000)
	for i := range vars {
		vars[i] = Variable(fmt.Sprintf("V%d", i))
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
		t.Run(string(v), func(t *testing.T) {
			w, ok := env.Lookup(v)
			assert.True(t, ok)
			assert.Equal(t, v, w)
		})
	}
}

func TestEnv_Simplify(t *testing.T) {
	// L = [a, b|L] ==> [a, b, a, b, ...]
	l := Variable("L")
	env := NewEnv().Bind(l, ListRest(l, Atom("a"), Atom("b")))
	c := env.Simplify(l)
	iter := ListIterator{List: c, Env: env}
	assert.True(t, iter.Next())
	assert.Equal(t, Atom("a"), iter.Current())
	assert.True(t, iter.Next())
	assert.Equal(t, Atom("b"), iter.Current())
	assert.False(t, iter.Next())
	suffix, ok := iter.Suffix().(*compound)
	assert.True(t, ok)
	assert.Equal(t, Atom("."), suffix.functor)
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
		{title: `X > W`, x: Variable("X"), y: Variable("W"), o: OrderGreater},
		{title: `X = X`, x: Variable("X"), y: Variable("X"), o: OrderEqual},
		{title: `X < Y`, x: Variable("X"), y: Variable("Y"), o: OrderLess},
		{title: `X < 0.0`, x: Variable("X"), y: Float(0), o: OrderLess},
		{title: `X < 0`, x: Variable("X"), y: Integer(0), o: OrderLess},
		{title: `X < a`, x: Variable("X"), y: Atom("a"), o: OrderLess},
		{title: `X < f(a)`, x: Variable("X"), y: Atom("f").Apply(Atom("a")), o: OrderLess},
		{title: `X < any`, x: Variable("X"), y: A{}, o: OrderLess},

		{title: `1.0 > X`, x: Float(1), y: Variable("X"), o: OrderGreater},
		{title: `1.0 > 0.0`, x: Float(1), y: Float(0), o: OrderGreater},
		{title: `1.0 = 1.0`, x: Float(1), y: Float(1), o: OrderEqual},
		{title: `1.0 < 2.0`, x: Float(1), y: Float(2), o: OrderLess},
		{title: `1.0 < 1`, x: Float(1), y: Integer(1), o: OrderLess},
		{title: `1.0 < a`, x: Float(1), y: Atom("a"), o: OrderLess},
		{title: `1.0 < f(a)`, x: Float(1), y: Atom("f").Apply(Atom("a")), o: OrderLess},
		{title: `1.0 < any`, x: Float(1), y: A{}, o: OrderLess},

		{title: `1 > X`, x: Integer(1), y: Variable("X"), o: OrderGreater},
		{title: `1 > 1.0`, x: Integer(1), y: Float(1), o: OrderGreater},
		{title: `1 > 0`, x: Integer(1), y: Integer(0), o: OrderGreater},
		{title: `1 = 1`, x: Integer(1), y: Integer(1), o: OrderEqual},
		{title: `1 < 2`, x: Integer(1), y: Integer(2), o: OrderLess},
		{title: `1 < a`, x: Integer(1), y: Atom("a"), o: OrderLess},
		{title: `1 < f(a)`, x: Integer(1), y: Atom("f").Apply(Atom("a")), o: OrderLess},
		{title: `1 < any`, x: Integer(1), y: A{}, o: OrderLess},

		{title: `a > X`, x: Atom("a"), y: Variable("X"), o: OrderGreater},
		{title: `a > 1.0`, x: Atom("a"), y: Float(1), o: OrderGreater},
		{title: `a > 1`, x: Atom("a"), y: Integer(1), o: OrderGreater},
		{title: `a > 'Z'`, x: Atom("a"), y: Atom("Z"), o: OrderGreater},
		{title: `a = a`, x: Atom("a"), y: Atom("a"), o: OrderEqual},
		{title: `a < b`, x: Atom("a"), y: Atom("b"), o: OrderLess},
		{title: `a < f(a)`, x: Atom("a"), y: Atom("f").Apply(Atom("a")), o: OrderLess},
		{title: `a < any`, x: Atom("a"), y: A{}, o: OrderLess},

		{title: `f(a) > X`, x: Atom("f").Apply(Atom("a")), y: Variable("X"), o: OrderGreater},
		{title: `f(a) > 1.0`, x: Atom("f").Apply(Atom("a")), y: Float(1), o: OrderGreater},
		{title: `f(a) > 1`, x: Atom("f").Apply(Atom("a")), y: Integer(1), o: OrderGreater},
		{title: `f(a) > a`, x: Atom("f").Apply(Atom("a")), y: Atom("a"), o: OrderGreater}, {title: `f(a) > f('Z')`, x: Atom("f").Apply(Atom("a")), y: Atom("f").Apply(Atom("Z")), o: OrderGreater},
		{title: `f(a) > e(a)`, x: Atom("f").Apply(Atom("a")), y: Atom("e").Apply(Atom("a")), o: OrderGreater},
		{title: `f(a, b) > f(a)`, x: Atom("f").Apply(Atom("a"), Atom("b")), y: Atom("f").Apply(Atom("a")), o: OrderGreater},
		{title: `f(a) = f(a)`, x: Atom("f").Apply(Atom("a")), y: Atom("f").Apply(Atom("a")), o: OrderEqual},
		{title: `f(a) < g(a)`, x: Atom("f").Apply(Atom("a")), y: Atom("g").Apply(Atom("a")), o: OrderLess},
		{title: `f(a) < f(a,b)`, x: Atom("f").Apply(Atom("a")), y: Atom("f").Apply(Atom("a"), Atom("b")), o: OrderLess},
		{title: `f(a) < f(b)`, x: Atom("f").Apply(Atom("a")), y: Atom("f").Apply(Atom("b")), o: OrderLess},
		{title: `f(a) < any`, x: Atom("f").Apply(Atom("a")), y: A{}, o: OrderLess},

		{title: `nil = nil`, x: nil, y: nil, o: OrderEqual},

		{title: `A{} > X`, x: A{}, y: Variable("X"), o: OrderGreater},
		{title: `A{} > 1.0`, x: A{}, y: Float(1), o: OrderGreater},
		{title: `A{} > 1`, x: A{}, y: Integer(1), o: OrderGreater},
		{title: `A{} > a`, x: A{}, y: Atom("a"), o: OrderGreater},
		{title: `A{} > f(a)`, x: A{}, y: Atom("f").Apply(Atom("a")), o: OrderGreater},
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
