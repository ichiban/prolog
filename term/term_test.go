package term

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestContains(t *testing.T) {
	env := Env{}
	assert.True(t, Contains(Atom("a"), Atom("a"), &env))
	assert.False(t, Contains(NewVariable(), Atom("a"), &env))
	v := Variable("V")
	env = append(env, Binding{
		Variable: v,
		Value:    Atom("a"),
	})
	assert.True(t, Contains(v, Atom("a"), &env))
	assert.True(t, Contains(&Compound{Functor: "a"}, Atom("a"), &env))
	assert.True(t, Contains(&Compound{Functor: "f", Args: []Interface{Atom("a")}}, Atom("a"), &env))
	assert.False(t, Contains(&Compound{Functor: "f"}, Atom("a"), &env))
}

func TestRulify(t *testing.T) {
	assert.Equal(t, &Compound{
		Functor: ":-",
		Args:    []Interface{Atom("a"), Atom("true")},
	}, Rulify(Atom("a"), nil))
	v := Variable("V")
	env := Env{
		{
			Variable: v,
			Value:    Atom("a"),
		},
	}
	assert.Equal(t, &Compound{
		Functor: ":-",
		Args:    []Interface{Atom("a"), Atom("true")},
	}, Rulify(v, env))
	assert.Equal(t, &Compound{
		Functor: ":-",
		Args:    []Interface{Atom("a"), Atom("b")},
	}, Rulify(&Compound{
		Functor: ":-",
		Args:    []Interface{Atom("a"), Atom("b")},
	}, nil))
}
