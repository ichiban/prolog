package term

import (
	"bytes"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestVariable_Unify(t *testing.T) {
	env := Env{}
	v1, v2 := Variable("V1"), Variable("V2")
	assert.True(t, v1.Unify(v2, false, &env))
	assert.True(t, v1.Unify(Atom("foo"), false, &env))
	assert.Equal(t, Atom("foo"), env.Resolve(v1))
	assert.Equal(t, Atom("foo"), env.Resolve(v2))

	v3, v4 := Variable("V3"), Variable("V4")
	assert.True(t, v3.Unify(v4, false, &env))
	assert.True(t, v4.Unify(Atom("bar"), false, &env))
	assert.Equal(t, Atom("bar"), env.Resolve(v3))
	assert.Equal(t, Atom("bar"), env.Resolve(v4))
}

func TestVariable_WriteTerm(t *testing.T) {
	t.Run("named", func(t *testing.T) {
		v := Variable("X")
		env := Env{
			{
				Variable: v,
				Value:    Integer(1),
			},
		}
		var buf bytes.Buffer
		assert.NoError(t, v.WriteTerm(&buf, WriteTermOptions{}, env))
		assert.Equal(t, "X", buf.String())
	})

	t.Run("unnamed", func(t *testing.T) {
		v := NewVariable()
		env := Env{
			{
				Variable: v,
				Value:    Integer(1),
			},
		}
		var buf bytes.Buffer
		assert.NoError(t, v.WriteTerm(&buf, WriteTermOptions{}, env))
		assert.Regexp(t, `\A_\d+\z`, buf.String())
	})
}
