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
			iter := ListIterator{List: ListRest(NewNamedVariable("X"), Atom("a"), Atom("b"))}
			assert.True(t, iter.Next())
			assert.Equal(t, Atom("a"), iter.Current())
			assert.True(t, iter.Next())
			assert.Equal(t, Atom("b"), iter.Current())
			assert.False(t, iter.Next())
			assert.Equal(t, InstantiationError(nil), iter.Err())
		})

		t.Run("atom", func(t *testing.T) {
			iter := ListIterator{List: ListRest(Atom("foo"), Atom("a"), Atom("b"))}
			assert.True(t, iter.Next())
			assert.Equal(t, Atom("a"), iter.Current())
			assert.True(t, iter.Next())
			assert.Equal(t, Atom("b"), iter.Current())
			assert.False(t, iter.Next())
			assert.Equal(t, TypeError(ValidTypeList, ListRest(Atom("foo"), Atom("a"), Atom("b")), nil), iter.Err())
		})

		t.Run("compound", func(t *testing.T) {
			iter := ListIterator{List: ListRest(Atom("f").Apply(Integer(0)), Atom("a"), Atom("b"))}
			assert.True(t, iter.Next())
			assert.Equal(t, Atom("a"), iter.Current())
			assert.True(t, iter.Next())
			assert.Equal(t, Atom("b"), iter.Current())
			assert.False(t, iter.Next())
			assert.Equal(t, TypeError(ValidTypeList, ListRest(Atom("f").Apply(Integer(0)), Atom("a"), Atom("b")), nil), iter.Err())
		})

		t.Run("other", func(t *testing.T) {
			iter := ListIterator{List: ListRest(&mockTerm{}, Atom("a"), Atom("b"))}
			assert.True(t, iter.Next())
			assert.Equal(t, Atom("a"), iter.Current())
			assert.True(t, iter.Next())
			assert.Equal(t, Atom("b"), iter.Current())
			assert.False(t, iter.Next())
			assert.Equal(t, TypeError(ValidTypeList, ListRest(&mockTerm{}, Atom("a"), Atom("b")), nil), iter.Err())
		})

		t.Run("circular list", func(t *testing.T) {
			l := NewNamedVariable("L")
			const max = 500
			elems := make([]Term, 0, max)
			for i := 0; i < max; i++ {
				elems = append(elems, Atom("a"))
				env := NewEnv().Bind(l, ListRest(l, elems...))
				iter := ListIterator{List: l, Env: env}
				for iter.Next() {
					assert.Equal(t, Atom("a"), iter.Current())
				}
				assert.Equal(t, TypeError(ValidTypeList, l, env), iter.Err())
			}
		})
	})
}

func TestListIterator_Suffix(t *testing.T) {
	iter := ListIterator{List: List(Atom("a"), Atom("b"), Atom("c"))}
	assert.Equal(t, List(Atom("a"), Atom("b"), Atom("c")), iter.Suffix())
	assert.True(t, iter.Next())
	assert.Equal(t, List(Atom("b"), Atom("c")), iter.Suffix())
	assert.True(t, iter.Next())
	assert.Equal(t, List(Atom("c")), iter.Suffix())
	assert.True(t, iter.Next())
	assert.Equal(t, List(), iter.Suffix())
	assert.False(t, iter.Next())
}

func TestSeqIterator_Next(t *testing.T) {
	t.Run("sequence", func(t *testing.T) {
		iter := SeqIterator{Seq: Seq(",", Atom("a"), Atom("b"), Atom("c"))}
		assert.True(t, iter.Next())
		assert.Equal(t, Atom("a"), iter.Current())
		assert.True(t, iter.Next())
		assert.Equal(t, Atom("b"), iter.Current())
		assert.True(t, iter.Next())
		assert.Equal(t, Atom("c"), iter.Current())
		assert.False(t, iter.Next())
	})

	t.Run("sequence with a trailing compound", func(t *testing.T) {
		iter := SeqIterator{Seq: Seq(",", Atom("a"), Atom("b"), Atom("f").Apply(Atom("c")))}
		assert.True(t, iter.Next())
		assert.Equal(t, Atom("a"), iter.Current())
		assert.True(t, iter.Next())
		assert.Equal(t, Atom("b"), iter.Current())
		assert.True(t, iter.Next())
		assert.Equal(t, Atom("f").Apply(Atom("c")), iter.Current())
		assert.False(t, iter.Next())
	})
}

func TestAltIterator_Next(t *testing.T) {
	t.Run("alternatives", func(t *testing.T) {
		iter := AltIterator{Alt: Seq(";", Atom("a"), Atom("b"), Atom("c"))}
		assert.True(t, iter.Next())
		assert.Equal(t, Atom("a"), iter.Current())
		assert.True(t, iter.Next())
		assert.Equal(t, Atom("b"), iter.Current())
		assert.True(t, iter.Next())
		assert.Equal(t, Atom("c"), iter.Current())
		assert.False(t, iter.Next())
	})

	t.Run("alternatives with a trailing compound", func(t *testing.T) {
		iter := AltIterator{Alt: Seq(";", Atom("a"), Atom("b"), Atom("f").Apply(Atom("c")))}
		assert.True(t, iter.Next())
		assert.Equal(t, Atom("a"), iter.Current())
		assert.True(t, iter.Next())
		assert.Equal(t, Atom("b"), iter.Current())
		assert.True(t, iter.Next())
		assert.Equal(t, Atom("f").Apply(Atom("c")), iter.Current())
		assert.False(t, iter.Next())
	})

	t.Run("if then else", func(t *testing.T) {
		iter := AltIterator{Alt: Seq(";", Atom("->").Apply(Atom("a"), Atom("b")), Atom("c"))}
		assert.True(t, iter.Next())
		assert.Equal(t, Seq(";", Atom("->").Apply(Atom("a"), Atom("b")), Atom("c")), iter.Current())
		assert.False(t, iter.Next())
	})
}

func TestAnyIterator_Next(t *testing.T) {
	t.Run("proper list", func(t *testing.T) {
		iter := AnyIterator{Any: List(Atom("a"), Atom("b"), Atom("c"))}
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
			iter := AnyIterator{Any: ListRest(NewNamedVariable("X"), Atom("a"), Atom("b"))}
			assert.True(t, iter.Next())
			assert.Equal(t, Atom("a"), iter.Current())
			assert.True(t, iter.Next())
			assert.Equal(t, Atom("b"), iter.Current())
			assert.False(t, iter.Next())
			assert.Equal(t, InstantiationError(nil), iter.Err())
		})

		t.Run("atom", func(t *testing.T) {
			iter := AnyIterator{Any: ListRest(Atom("foo"), Atom("a"), Atom("b"))}
			assert.True(t, iter.Next())
			assert.Equal(t, Atom("a"), iter.Current())
			assert.True(t, iter.Next())
			assert.Equal(t, Atom("b"), iter.Current())
			assert.False(t, iter.Next())
			assert.Equal(t, TypeError(ValidTypeList, ListRest(Atom("foo"), Atom("a"), Atom("b")), nil), iter.Err())
		})
	})

	t.Run("sequence", func(t *testing.T) {
		iter := AnyIterator{Any: Seq(",", Atom("a"), Atom("b"), Atom("c"))}
		assert.True(t, iter.Next())
		assert.Equal(t, Atom("a"), iter.Current())
		assert.True(t, iter.Next())
		assert.Equal(t, Atom("b"), iter.Current())
		assert.True(t, iter.Next())
		assert.Equal(t, Atom("c"), iter.Current())
		assert.False(t, iter.Next())
		assert.NoError(t, iter.Err())
	})

	t.Run("single", func(t *testing.T) {
		iter := AnyIterator{Any: Atom("a")}
		assert.True(t, iter.Next())
		assert.Equal(t, Atom("a"), iter.Current())
		assert.False(t, iter.Next())
		assert.NoError(t, iter.Err())
	})
}
