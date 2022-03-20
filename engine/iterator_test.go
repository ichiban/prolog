package engine

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestListIterator_Next(t *testing.T) {
	t.Run("proper list", func(t *testing.T) {
		iter := ListIterator{List: List(Atom("a"), Atom("b"), Atom("c"))}
		assert.True(t, iter.Next())
		assert.Equal(t, Atom("a"), iter.Current())
		assert.True(t, iter.Next())
		assert.Equal(t, Atom("b"), iter.Current())
		assert.True(t, iter.Next())
		assert.Equal(t, Atom("c"), iter.Current())
		assert.False(t, iter.Next())
		assert.NoError(t, iter.Err())
	})

	t.Run("improper list", func(t *testing.T) {
		t.Run("variable", func(t *testing.T) {
			iter := ListIterator{List: ListRest(Variable("X"), Atom("a"), Atom("b"))}
			assert.True(t, iter.Next())
			assert.Equal(t, Atom("a"), iter.Current())
			assert.True(t, iter.Next())
			assert.Equal(t, Atom("b"), iter.Current())
			assert.False(t, iter.Next())
			assert.Equal(t, ErrInstantiation, iter.Err())
		})

		t.Run("atom", func(t *testing.T) {
			iter := ListIterator{List: ListRest(Atom("foo"), Atom("a"), Atom("b"))}
			assert.True(t, iter.Next())
			assert.Equal(t, Atom("a"), iter.Current())
			assert.True(t, iter.Next())
			assert.Equal(t, Atom("b"), iter.Current())
			assert.False(t, iter.Next())
			assert.Equal(t, TypeErrorList(Atom("foo")), iter.Err())
		})

		t.Run("compound", func(t *testing.T) {
			iter := ListIterator{List: ListRest(Atom("f").Apply(Integer(0)), Atom("a"), Atom("b"))}
			assert.True(t, iter.Next())
			assert.Equal(t, Atom("a"), iter.Current())
			assert.True(t, iter.Next())
			assert.Equal(t, Atom("b"), iter.Current())
			assert.False(t, iter.Next())
			assert.Equal(t, TypeErrorList(Atom("f").Apply(Integer(0))), iter.Err())
		})

		t.Run("other", func(t *testing.T) {
			iter := ListIterator{List: ListRest(&mockTerm{}, Atom("a"), Atom("b"))}
			assert.True(t, iter.Next())
			assert.Equal(t, Atom("a"), iter.Current())
			assert.True(t, iter.Next())
			assert.Equal(t, Atom("b"), iter.Current())
			assert.False(t, iter.Next())
			assert.Equal(t, TypeErrorList(&mockTerm{}), iter.Err())
		})
	})
}
