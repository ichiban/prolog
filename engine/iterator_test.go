package engine

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestListIterator_Next(t *testing.T) {
	t.Run("proper list", func(t *testing.T) {
		iter := ListIterator{List: List(NewAtom("a"), NewAtom("b"), NewAtom("c"))}
		assert.True(t, iter.Next())
		assert.Equal(t, NewAtom("a"), iter.Current())
		assert.True(t, iter.Next())
		assert.Equal(t, NewAtom("b"), iter.Current())
		assert.True(t, iter.Next())
		assert.Equal(t, NewAtom("c"), iter.Current())
		assert.False(t, iter.Next())
		assert.NoError(t, iter.Err())
	})

	t.Run("improper list", func(t *testing.T) {
		t.Run("variable", func(t *testing.T) {
			iter := ListIterator{List: ListRest(NewNamedVariable("X"), NewAtom("a"), NewAtom("b"))}
			assert.True(t, iter.Next())
			assert.Equal(t, NewAtom("a"), iter.Current())
			assert.True(t, iter.Next())
			assert.Equal(t, NewAtom("b"), iter.Current())
			assert.False(t, iter.Next())
			assert.Equal(t, InstantiationError(nil), iter.Err())
		})

		t.Run("atom", func(t *testing.T) {
			iter := ListIterator{List: ListRest(NewAtom("foo"), NewAtom("a"), NewAtom("b"))}
			assert.True(t, iter.Next())
			assert.Equal(t, NewAtom("a"), iter.Current())
			assert.True(t, iter.Next())
			assert.Equal(t, NewAtom("b"), iter.Current())
			assert.False(t, iter.Next())
			assert.Equal(t, TypeError(ValidTypeList, ListRest(NewAtom("foo"), NewAtom("a"), NewAtom("b")), nil), iter.Err())
		})

		t.Run("compound", func(t *testing.T) {
			iter := ListIterator{List: ListRest(NewAtom("f").Apply(Integer(0)), NewAtom("a"), NewAtom("b"))}
			assert.True(t, iter.Next())
			assert.Equal(t, NewAtom("a"), iter.Current())
			assert.True(t, iter.Next())
			assert.Equal(t, NewAtom("b"), iter.Current())
			assert.False(t, iter.Next())
			assert.Equal(t, TypeError(ValidTypeList, ListRest(NewAtom("f").Apply(Integer(0)), NewAtom("a"), NewAtom("b")), nil), iter.Err())
		})

		t.Run("other", func(t *testing.T) {
			iter := ListIterator{List: ListRest(&mockTerm{}, NewAtom("a"), NewAtom("b"))}
			assert.True(t, iter.Next())
			assert.Equal(t, NewAtom("a"), iter.Current())
			assert.True(t, iter.Next())
			assert.Equal(t, NewAtom("b"), iter.Current())
			assert.False(t, iter.Next())
			assert.Equal(t, TypeError(ValidTypeList, ListRest(&mockTerm{}, NewAtom("a"), NewAtom("b")), nil), iter.Err())
		})

		t.Run("circular list", func(t *testing.T) {
			l := NewNamedVariable("L")
			const max = 500
			elems := make([]Term, 0, max)
			for i := 0; i < max; i++ {
				elems = append(elems, NewAtom("a"))
				env := NewEnv().Bind(l, ListRest(l, elems...))
				iter := ListIterator{List: l, Env: env}
				for iter.Next() {
					assert.Equal(t, NewAtom("a"), iter.Current())
				}
				assert.Equal(t, TypeError(ValidTypeList, l, env), iter.Err())
			}
		})
	})
}

func TestListIterator_Suffix(t *testing.T) {
	iter := ListIterator{List: List(NewAtom("a"), NewAtom("b"), NewAtom("c"))}
	assert.Equal(t, List(NewAtom("a"), NewAtom("b"), NewAtom("c")), iter.Suffix())
	assert.True(t, iter.Next())
	assert.Equal(t, List(NewAtom("b"), NewAtom("c")), iter.Suffix())
	assert.True(t, iter.Next())
	assert.Equal(t, List(NewAtom("c")), iter.Suffix())
	assert.True(t, iter.Next())
	assert.Equal(t, List(), iter.Suffix())
	assert.False(t, iter.Next())
}

func TestSeqIterator_Next(t *testing.T) {
	t.Run("sequence", func(t *testing.T) {
		iter := SeqIterator{Seq: Seq(atomComma, NewAtom("a"), NewAtom("b"), NewAtom("c"))}
		assert.True(t, iter.Next())
		assert.Equal(t, NewAtom("a"), iter.Current())
		assert.True(t, iter.Next())
		assert.Equal(t, NewAtom("b"), iter.Current())
		assert.True(t, iter.Next())
		assert.Equal(t, NewAtom("c"), iter.Current())
		assert.False(t, iter.Next())
	})

	t.Run("sequence with a trailing compound", func(t *testing.T) {
		iter := SeqIterator{Seq: Seq(atomComma, NewAtom("a"), NewAtom("b"), NewAtom("f").Apply(NewAtom("c")))}
		assert.True(t, iter.Next())
		assert.Equal(t, NewAtom("a"), iter.Current())
		assert.True(t, iter.Next())
		assert.Equal(t, NewAtom("b"), iter.Current())
		assert.True(t, iter.Next())
		assert.Equal(t, NewAtom("f").Apply(NewAtom("c")), iter.Current())
		assert.False(t, iter.Next())
	})
}

func TestAltIterator_Next(t *testing.T) {
	t.Run("alternatives", func(t *testing.T) {
		iter := AltIterator{Alt: Seq(atomSemiColon, NewAtom("a"), NewAtom("b"), NewAtom("c"))}
		assert.True(t, iter.Next())
		assert.Equal(t, NewAtom("a"), iter.Current())
		assert.True(t, iter.Next())
		assert.Equal(t, NewAtom("b"), iter.Current())
		assert.True(t, iter.Next())
		assert.Equal(t, NewAtom("c"), iter.Current())
		assert.False(t, iter.Next())
	})

	t.Run("alternatives with a trailing compound", func(t *testing.T) {
		iter := AltIterator{Alt: Seq(atomSemiColon, NewAtom("a"), NewAtom("b"), NewAtom("f").Apply(NewAtom("c")))}
		assert.True(t, iter.Next())
		assert.Equal(t, NewAtom("a"), iter.Current())
		assert.True(t, iter.Next())
		assert.Equal(t, NewAtom("b"), iter.Current())
		assert.True(t, iter.Next())
		assert.Equal(t, NewAtom("f").Apply(NewAtom("c")), iter.Current())
		assert.False(t, iter.Next())
	})

	t.Run("if then else", func(t *testing.T) {
		iter := AltIterator{Alt: Seq(atomSemiColon, atomThen.Apply(NewAtom("a"), NewAtom("b")), NewAtom("c"))}
		assert.True(t, iter.Next())
		assert.Equal(t, Seq(atomSemiColon, atomThen.Apply(NewAtom("a"), NewAtom("b")), NewAtom("c")), iter.Current())
		assert.False(t, iter.Next())
	})
}

func TestAnyIterator_Next(t *testing.T) {
	t.Run("proper list", func(t *testing.T) {
		iter := AnyIterator{Any: List(NewAtom("a"), NewAtom("b"), NewAtom("c"))}
		assert.True(t, iter.Next())
		assert.Equal(t, NewAtom("a"), iter.Current())
		assert.True(t, iter.Next())
		assert.Equal(t, NewAtom("b"), iter.Current())
		assert.True(t, iter.Next())
		assert.Equal(t, NewAtom("c"), iter.Current())
		assert.False(t, iter.Next())
		assert.NoError(t, iter.Err())
	})

	t.Run("improper list", func(t *testing.T) {
		t.Run("variable", func(t *testing.T) {
			iter := AnyIterator{Any: ListRest(NewNamedVariable("X"), NewAtom("a"), NewAtom("b"))}
			assert.True(t, iter.Next())
			assert.Equal(t, NewAtom("a"), iter.Current())
			assert.True(t, iter.Next())
			assert.Equal(t, NewAtom("b"), iter.Current())
			assert.False(t, iter.Next())
			assert.Equal(t, InstantiationError(nil), iter.Err())
		})

		t.Run("atom", func(t *testing.T) {
			iter := AnyIterator{Any: ListRest(NewAtom("foo"), NewAtom("a"), NewAtom("b"))}
			assert.True(t, iter.Next())
			assert.Equal(t, NewAtom("a"), iter.Current())
			assert.True(t, iter.Next())
			assert.Equal(t, NewAtom("b"), iter.Current())
			assert.False(t, iter.Next())
			assert.Equal(t, TypeError(ValidTypeList, ListRest(NewAtom("foo"), NewAtom("a"), NewAtom("b")), nil), iter.Err())
		})
	})

	t.Run("sequence", func(t *testing.T) {
		iter := AnyIterator{Any: Seq(atomComma, NewAtom("a"), NewAtom("b"), NewAtom("c"))}
		assert.True(t, iter.Next())
		assert.Equal(t, NewAtom("a"), iter.Current())
		assert.True(t, iter.Next())
		assert.Equal(t, NewAtom("b"), iter.Current())
		assert.True(t, iter.Next())
		assert.Equal(t, NewAtom("c"), iter.Current())
		assert.False(t, iter.Next())
		assert.NoError(t, iter.Err())
	})

	t.Run("single", func(t *testing.T) {
		iter := AnyIterator{Any: NewAtom("a")}
		assert.True(t, iter.Next())
		assert.Equal(t, NewAtom("a"), iter.Current())
		assert.False(t, iter.Next())
		assert.NoError(t, iter.Err())
	})
}
