package prolog

import (
	"errors"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestCopyTerm(t *testing.T) {
	in := &Variable{Ref: Atom("a")}
	out := &Variable{}
	k := func() (bool, error) {
		return true, nil
	}
	ok, err := CopyTerm(in, out, k)
	assert.NoError(t, err)
	assert.True(t, ok)
	assert.Equal(t, Atom("a"), out.Ref)
}

func TestRepeat(t *testing.T) {
	c := 3
	ok, err := Repeat(func() (bool, error) {
		c--
		return c == 0, nil
	})
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Repeat(func() (bool, error) {
		return false, errors.New("")
	})
	assert.Error(t, err)
	assert.False(t, ok)

	ok, err = Repeat(func() (bool, error) {
		return true, errCut
	})
	assert.True(t, errors.Is(err, errCut))
	assert.True(t, ok)
}

func TestBagOf(t *testing.T) {
	e, err := NewEngine()
	assert.NoError(t, err)
	assert.NoError(t, e.Load(`
foo(a, b, c).
foo(a, b, d).
foo(b, c, e).
foo(b, c, f).
foo(c, c, g).
`))

	t.Run("without qualifier", func(t *testing.T) {
		var vars [][]Variable
		ok, err := e.Query(`bagof(C, foo(A, B, C), Cs).`, func(vs []Variable) bool {
			vars = append(vars, vs)
			return false
		})
		assert.NoError(t, err)
		assert.False(t, ok)

		assert.Len(t, vars, 3)
		assert.Equal(t, []Variable{
			{Name: "C", Ref: &Variable{}},
			{Name: "A", Ref: Atom("a")},
			{Name: "B", Ref: Atom("b")},
			{Name: "Cs", Ref: List(Atom("c"), Atom("d"))},
		}, vars[0])
		assert.Equal(t, []Variable{
			{Name: "C", Ref: &Variable{}},
			{Name: "A", Ref: Atom("b")},
			{Name: "B", Ref: Atom("c")},
			{Name: "Cs", Ref: List(Atom("e"), Atom("f"))},
		}, vars[1])
		assert.Equal(t, []Variable{
			{Name: "C", Ref: &Variable{}},
			{Name: "A", Ref: Atom("c")},
			{Name: "B", Ref: Atom("c")},
			{Name: "Cs", Ref: List(Atom("g"))},
		}, vars[2])
	})

	t.Run("with qualifier", func(t *testing.T) {
		var vars [][]Variable
		ok, err := e.Query(`bagof(C, A^foo(A, B, C), Cs).`, func(vs []Variable) bool {
			vars = append(vars, vs)
			return false
		})
		assert.NoError(t, err)
		assert.False(t, ok)

		assert.Len(t, vars, 2)
		assert.Equal(t, []Variable{
			{Name: "C", Ref: &Variable{}},
			{Name: "A", Ref: &Variable{}},
			{Name: "B", Ref: Atom("b")},
			{Name: "Cs", Ref: List(Atom("c"), Atom("d"))},
		}, vars[0])
		assert.Equal(t, []Variable{
			{Name: "C", Ref: &Variable{}},
			{Name: "A", Ref: &Variable{}},
			{Name: "B", Ref: Atom("c")},
			{Name: "Cs", Ref: List(Atom("e"), Atom("f"), Atom("g"))},
		}, vars[1])
	})

	t.Run("with multiple qualifiers", func(t *testing.T) {
		var vars [][]Variable
		ok, err := e.Query(`bagof(C, (A, B)^foo(A, B, C), Cs).`, func(vs []Variable) bool {
			vars = append(vars, vs)
			return false
		})
		assert.NoError(t, err)
		assert.False(t, ok)

		assert.Len(t, vars, 1)
		assert.Equal(t, []Variable{
			{Name: "C", Ref: &Variable{}},
			{Name: "Cs", Ref: List(Atom("c"), Atom("d"), Atom("e"), Atom("f"), Atom("g"))},
		}, vars[0])
	})
}
