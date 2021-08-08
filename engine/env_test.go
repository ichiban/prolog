package engine

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestEnv_Bind(t *testing.T) {
	t.Run("insert", func(t *testing.T) {
		env := Env{
			bindings: []binding{
				{
					variable: "X",
					value:    Atom("a"),
				},
				{
					variable: "Y",
					value:    Atom("b"),
				},
			},
		}
		env.Bind("Z", Atom("c"))
		assert.Equal(t, Env{
			bindings: []binding{
				{
					variable: "X",
					value:    Atom("a"),
				},
				{
					variable: "Y",
					value:    Atom("b"),
				},
				{
					variable: "Z",
					value:    Atom("c"),
				},
			},
		}, env)
	})

	t.Run("update", func(t *testing.T) {
		env := Env{
			bindings: []binding{
				{
					variable: "X",
					value:    Atom("a"),
				},
				{
					variable: "Y",
					value:    Atom("b"),
				},
				{
					variable: "Z",
					value:    Atom("c"),
				},
			},
		}
		env.Bind("Z", Atom("d"))
		assert.Equal(t, Env{
			bindings: []binding{
				{
					variable: "X",
					value:    Atom("a"),
				},
				{
					variable: "Y",
					value:    Atom("b"),
				},
				{
					variable: "Z",
					value:    Atom("d"),
				},
			},
		}, env)
	})
}
