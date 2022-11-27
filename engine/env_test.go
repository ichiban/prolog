package engine

import (
	"math/rand"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestEnv_Bind(t *testing.T) {
	a := NewVariable()

	var env *Env
	assert.Equal(t, &Env{
		color: black,
		left: &Env{
			binding: binding{
				key:   newEnvKey(a),
				value: NewAtom("a"),
			},
		},
		binding: binding{
			key:   newEnvKey(varContext),
			value: NewAtom("root"),
		},
	}, env.Bind(a, NewAtom("a")))
}

func TestEnv_Lookup(t *testing.T) {
	vars := make([]Variable, 1000)
	for i := range vars {
		vars[i] = NewVariable()
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
	l := NewVariable()
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
	w, x, y := NewVariable(), NewVariable(), NewVariable()
	type A struct{}
	type B struct{ b interface{} }
	type C struct{}

	n, m := 1, 1

	tests := []struct {
		title string
		x, y  Term
		o     int
	}{
		{title: `X > W`, x: x, y: w, o: 1},
		{title: `X = X`, x: x, y: x, o: 0},
		{title: `X < Y`, x: x, y: y, o: -1},
		{title: `X < 0.0`, x: x, y: Float(0), o: -1},
		{title: `X < 0`, x: x, y: Integer(0), o: -1},
		{title: `X < a`, x: x, y: NewAtom("a"), o: -1},
		{title: `X < f(a)`, x: x, y: NewAtom("f").Apply(NewAtom("a")), o: -1},
		{title: `X < any`, x: x, y: A{}, o: -1},

		{title: `1.0 > X`, x: Float(1), y: x, o: 1},
		{title: `1.0 > 0.0`, x: Float(1), y: Float(0), o: 1},
		{title: `1.0 = 1.0`, x: Float(1), y: Float(1), o: 0},
		{title: `1.0 < 2.0`, x: Float(1), y: Float(2), o: -1},
		{title: `1.0 < 1`, x: Float(1), y: Integer(1), o: -1},
		{title: `1.0 < a`, x: Float(1), y: NewAtom("a"), o: -1},
		{title: `1.0 < f(a)`, x: Float(1), y: NewAtom("f").Apply(NewAtom("a")), o: -1},
		{title: `1.0 < any`, x: Float(1), y: A{}, o: -1},

		{title: `1 > X`, x: Integer(1), y: x, o: 1},
		{title: `1 > 1.0`, x: Integer(1), y: Float(1), o: 1},
		{title: `1 > 0`, x: Integer(1), y: Integer(0), o: 1},
		{title: `1 = 1`, x: Integer(1), y: Integer(1), o: 0},
		{title: `1 < 2`, x: Integer(1), y: Integer(2), o: -1},
		{title: `1 < a`, x: Integer(1), y: NewAtom("a"), o: -1},
		{title: `1 < f(a)`, x: Integer(1), y: NewAtom("f").Apply(NewAtom("a")), o: -1},
		{title: `1 < any`, x: Integer(1), y: A{}, o: -1},

		{title: `a > X`, x: NewAtom("a"), y: x, o: 1},
		{title: `a > 1.0`, x: NewAtom("a"), y: Float(1), o: 1},
		{title: `a > 1`, x: NewAtom("a"), y: Integer(1), o: 1},
		{title: `a > 'Z'`, x: NewAtom("a"), y: NewAtom("Z"), o: 1},
		{title: `a = a`, x: NewAtom("a"), y: NewAtom("a"), o: 0},
		{title: `a < b`, x: NewAtom("a"), y: NewAtom("b"), o: -1},
		{title: `a < f(a)`, x: NewAtom("a"), y: NewAtom("f").Apply(NewAtom("a")), o: -1},
		{title: `a < any`, x: NewAtom("a"), y: A{}, o: -1},

		{title: `f(a) > X`, x: NewAtom("f").Apply(NewAtom("a")), y: x, o: 1},
		{title: `f(a) > 1.0`, x: NewAtom("f").Apply(NewAtom("a")), y: Float(1), o: 1},
		{title: `f(a) > 1`, x: NewAtom("f").Apply(NewAtom("a")), y: Integer(1), o: 1},
		{title: `f(a) > a`, x: NewAtom("f").Apply(NewAtom("a")), y: NewAtom("a"), o: 1}, {title: `f(a) > f('Z')`, x: NewAtom("f").Apply(NewAtom("a")), y: NewAtom("f").Apply(NewAtom("Z")), o: 1},
		{title: `f(a) > e(a)`, x: NewAtom("f").Apply(NewAtom("a")), y: NewAtom("e").Apply(NewAtom("a")), o: 1},
		{title: `f(a, b) > f(a)`, x: NewAtom("f").Apply(NewAtom("a"), NewAtom("b")), y: NewAtom("f").Apply(NewAtom("a")), o: 1},
		{title: `f(a) = f(a)`, x: NewAtom("f").Apply(NewAtom("a")), y: NewAtom("f").Apply(NewAtom("a")), o: 0},
		{title: `f(a) < g(a)`, x: NewAtom("f").Apply(NewAtom("a")), y: NewAtom("g").Apply(NewAtom("a")), o: -1},
		{title: `f(a) < f(a,b)`, x: NewAtom("f").Apply(NewAtom("a")), y: NewAtom("f").Apply(NewAtom("a"), NewAtom("b")), o: -1},
		{title: `f(a) < f(b)`, x: NewAtom("f").Apply(NewAtom("a")), y: NewAtom("f").Apply(NewAtom("b")), o: -1},
		{title: `f(a) < any`, x: NewAtom("f").Apply(NewAtom("a")), y: A{}, o: -1},

		{title: `nil = nil`, x: nil, y: nil, o: 0},

		{title: `A{} > X`, x: A{}, y: x, o: 1},
		{title: `A{} > 1.0`, x: A{}, y: Float(1), o: 1},
		{title: `A{} > 1`, x: A{}, y: Integer(1), o: 1},
		{title: `A{} > a`, x: A{}, y: NewAtom("a"), o: 1},
		{title: `A{} > f(a)`, x: A{}, y: NewAtom("f").Apply(NewAtom("a")), o: 1},
		{title: `B{} > A{}`, x: B{}, y: A{}, o: 1},
		{title: `B{2} = B{1}`, x: B{b: 2}, y: B{b: 1}, o: 1},
		{title: `B{} = B{}`, x: B{}, y: B{}, o: 0},
		{title: `B{1} = B{2}`, x: B{b: 1}, y: B{b: 2}, o: -1},
		{title: `B{} < C{}`, x: B{}, y: C{}, o: -1},

		{title: `true > false`, x: true, y: false, o: 1},
		{title: `false = false`, x: false, y: false, o: 0},
		{title: `true = true`, x: true, y: true, o: 0},
		{title: `false < true`, x: false, y: true, o: -1},

		{title: `2 > 1`, x: 2, y: 1, o: 1},
		{title: `1 = 1`, x: 1, y: 1, o: 0},
		{title: `1 < 2`, x: 1, y: 2, o: -1},

		{title: `uint(2) > uint(1)`, x: uint(2), y: uint(1), o: 1},
		{title: `uint(1) = uint(1)`, x: uint(1), y: uint(1), o: 0},
		{title: `uint(1) < uint(2)`, x: uint(1), y: uint(2), o: -1},

		{title: `2.0 > 1.0`, x: 2.0, y: 1.0, o: 1},
		{title: `1.0 = 1.0`, x: 1.0, y: 1.0, o: 0},
		{title: `1.0 < 2.0`, x: 1.0, y: 2.0, o: -1},

		{title: `2 + 1i > 1 + 1i`, x: 2 + 1i, y: 1 + 1i, o: 1},
		{title: `1 + 2i > 1 + 1i`, x: 1 + 2i, y: 1 + 1i, o: 1},
		{title: `1 + 1i = 1 + 1i`, x: 1 + 1i, y: 1 + 1i, o: 0},
		{title: `1 + 1i < 2 + 1i`, x: 1 + 1i, y: 2 + 1i, o: -1},
		{title: `1 + 1i < 1 + 2i`, x: 1 + 1i, y: 1 + 2i, o: -1},

		{title: `{1,2,3} > {1,2}`, x: []int{1, 2, 3}, y: []int{1, 2}, o: 1},
		{title: `{1,2} > {1,1}`, x: []int{1, 2}, y: []int{1, 1}, o: 1},
		{title: `{1,2} = {1,2}`, x: []int{1, 2}, y: []int{1, 2}, o: 0},
		{title: `{1,1} < {1,2}`, x: []int{1, 1}, y: []int{1, 2}, o: -1},
		{title: `{1,2} < {1,2,3}`, x: []int{1, 2}, y: []int{1, 2, 3}, o: -1},

		{title: `&m > &n`, x: &m, y: &n, o: 1},
		{title: `&n = &n`, x: &n, y: &n, o: 0},
		{title: `&n < &m`, x: &n, y: &m, o: -1},

		{title: `{a:1} > {}`, x: map[string]int{"a": 1}, y: map[string]int{}, o: 1},
		{title: `{a:1} = {a:1}`, x: map[string]int{"a": 1}, y: map[string]int{"a": 1}, o: 0},
		{title: `{} < {a:1}`, x: map[string]int{}, y: map[string]int{"a": 1}, o: -1},

		{title: `def > abc`, x: "def", y: "abc", o: 1},
		{title: `abc = abc`, x: "abc", y: "abc", o: 0},
		{title: `abc < def`, x: "abc", y: "def", o: -1},
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
	v := NewVariable()
	env = env.Bind(v, NewAtom("a"))
	assert.True(t, contains(v, NewAtom("a"), env))
	assert.True(t, contains(&compound{functor: NewAtom("a")}, NewAtom("a"), env))
	assert.True(t, contains(&compound{functor: NewAtom("f"), args: []Term{NewAtom("a")}}, NewAtom("a"), env))
	assert.False(t, contains(&compound{functor: NewAtom("f")}, NewAtom("a"), env))
}
