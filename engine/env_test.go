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
	suffix, ok := iter.Suffix().(*Compound)
	assert.True(t, ok)
	assert.Equal(t, Atom("."), suffix.Functor)
	assert.Len(t, suffix.Args, 2)
}
