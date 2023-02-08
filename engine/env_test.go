package engine

import (
	"fmt"
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
	}, env.bind(a, NewAtom("a")))
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
		env = env.bind(v, v)
	}

	rand.Shuffle(len(vars), func(i, j int) {
		vars[i], vars[j] = vars[j], vars[i]
	})

	for _, v := range vars {
		t.Run(fmt.Sprintf("_%d", v), func(t *testing.T) {
			w, ok := env.lookup(v)
			assert.True(t, ok)
			assert.Equal(t, v, w)
		})
	}
}

func TestEnv_Simplify(t *testing.T) {
	// L = [a, b|L] ==> [a, b, a, b, ...]
	l := NewVariable()
	p := PartialList(l, NewAtom("a"), NewAtom("b"))
	env := NewEnv().bind(l, p)
	c := env.simplify(l)
	iter := ListIterator{List: c, Env: env}
	assert.True(t, iter.Next())
	assert.Equal(t, NewAtom("a"), iter.Current())
	assert.True(t, iter.Next())
	assert.Equal(t, NewAtom("b"), iter.Current())
	assert.False(t, iter.Next())
	suffix, ok := iter.Suffix().(*partial)
	assert.True(t, ok)
	assert.Equal(t, atomDot, suffix.Functor())
	assert.Equal(t, 2, suffix.Arity())
}

func TestContains(t *testing.T) {
	var env *Env
	assert.True(t, contains(NewAtom("a"), NewAtom("a"), env))
	assert.False(t, contains(NewVariable(), NewAtom("a"), env))
	v := NewVariable()
	env = env.bind(v, NewAtom("a"))
	assert.True(t, contains(v, NewAtom("a"), env))
	assert.True(t, contains(&compound{functor: NewAtom("a")}, NewAtom("a"), env))
	assert.True(t, contains(&compound{functor: NewAtom("f"), args: []Term{NewAtom("a")}}, NewAtom("a"), env))
	assert.False(t, contains(&compound{functor: NewAtom("f")}, NewAtom("a"), env))
}
