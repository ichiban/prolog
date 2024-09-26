package internal

import (
	"errors"
	"testing"
)

func TestListIterator_Next(t *testing.T) {
	t.Run("proper list", func(t *testing.T) {
		pool := NewTermPool(1024)

		a, err := pool.PutAtom(NewAtom("a"))
		if err != nil {
			t.Fatal(err)
		}

		b, err := pool.PutAtom(NewAtom("b"))
		if err != nil {
			t.Fatal(err)
		}

		c, err := pool.PutAtom(NewAtom("c"))
		if err != nil {
			t.Fatal(err)
		}

		l, err := pool.PutList(a, b, c)
		if err != nil {
			t.Fatal(err)
		}

		iter := ListIterator{TermPool: &pool, List: l}
		if !iter.Next() {
			t.Fatal("expected true, got false")
		}
		if iter.Current() != a {
			t.Errorf("expected %d, got %d", a, iter.Current())
		}

		if !iter.Next() {
			t.Fatal("expected true, got false")
		}
		if iter.Current() != b {
			t.Errorf("expected %d, got %d", b, iter.Current())
		}

		if !iter.Next() {
			t.Fatal("expected true, got false")
		}
		if iter.Current() != c {
			t.Errorf("expected %d, got %d", c, iter.Current())
		}

		if iter.Next() {
			t.Error("expected false, got true")
		}
		if err := iter.Err(); err != nil {
			t.Fatal(err)
		}
	})

	t.Run("improper list", func(t *testing.T) {
		t.Run("variable", func(t *testing.T) {
			pool := NewTermPool(1024)

			rest, err := pool.PutVariable(NewVariable(&pool))
			if err != nil {
				t.Fatal(err)
			}

			a, err := pool.PutAtom(NewAtom("a"))
			if err != nil {
				t.Fatal(err)
			}

			b, err := pool.PutAtom(NewAtom("b"))
			if err != nil {
				t.Fatal(err)
			}

			pl, err := pool.PutPartialList(rest, a, b)
			if err != nil {
				t.Fatal(err)
			}

			iter := ListIterator{TermPool: &pool, List: pl}
			if !iter.Next() {
				t.Fatal("expected true, got false")
			}
			if iter.Current() != a {
				t.Errorf("expected %d, got %d", a, iter.Current())
			}

			if !iter.Next() {
				t.Fatal("expected true, got false")
			}
			if iter.Current() != b {
				t.Errorf("expected %d, got %d", b, iter.Current())
			}

			if iter.Next() {
				t.Error("expected false, got true")
			}

			if err := iter.Err(); !errors.Is(err, ErrInstantiation) {
				t.Errorf("expected %v, got %v", ErrInstantiation, err)
			}
		})

		t.Run("atom", func(t *testing.T) {
			pool := NewTermPool(1024)

			foo, err := pool.PutAtom(NewAtom("foo"))
			if err != nil {
				t.Fatal(err)
			}

			a, err := pool.PutAtom(Atom('a'))
			if err != nil {
				t.Fatal(err)
			}

			b, err := pool.PutAtom(Atom('b'))
			if err != nil {
				t.Fatal(err)
			}

			pl, err := pool.PutPartialList(foo, a, b)
			if err != nil {
				t.Fatal(err)
			}

			iter := ListIterator{TermPool: &pool, List: pl}
			if !iter.Next() {
				t.Fatal("expected true, got false")
			}
			if iter.Current() != a {
				t.Errorf("expected %d, got %d", a, iter.Current())
			}

			if !iter.Next() {
				t.Fatal("expected true, got false")
			}
			if iter.Current() != b {
				t.Errorf("expected %d, got %d", b, iter.Current())
			}

			if iter.Next() {
				t.Error("expected false, got true")
			}
			if err := iter.Err(); !errors.Is(err, errInvalidList) {
				t.Errorf("expected %v, got %v", errInvalidList, err)
			}
		})

		t.Run("compound", func(t *testing.T) {
			pool := NewTermPool(1024)

			i0, err := pool.PutInteger(0)
			if err != nil {
				t.Fatal(err)
			}

			a, err := pool.PutAtom(Atom('a'))
			if err != nil {
				t.Fatal(err)
			}

			b, err := pool.PutAtom(Atom('b'))
			if err != nil {
				t.Fatal(err)
			}

			f, err := pool.PutCompound(Atom('f'), i0)
			if err != nil {
				t.Fatal(err)
			}

			pl, err := pool.PutPartialList(f, a, b)
			if err != nil {
				t.Fatal(err)
			}

			iter := ListIterator{TermPool: &pool, List: pl}
			if !iter.Next() {
				t.Fatal("expected true, got false")
			}
			if iter.Current() != a {
				t.Errorf("expected %d, got %d", a, iter.Current())
			}

			if !iter.Next() {
				t.Fatal("expected true, got false")
			}
			if iter.Current() != b {
				t.Errorf("expected %d, got %d", b, iter.Current())
			}

			if iter.Next() {
				t.Error("expected false, got true")
			}
			if err := iter.Err(); !errors.Is(err, errInvalidList) {
				t.Errorf("expected %v, got %v", errInvalidList, err)
			}
		})

		t.Run("circular list", func(t *testing.T) {
			pool := NewTermPool(1024)

			l, err := pool.PutVariable(NewVariable(&pool))
			if err != nil {
				t.Fatal(err)
			}

			a, err := pool.PutAtom(Atom('a'))
			if err != nil {
				t.Fatal(err)
			}

			pl, err := pool.PutPartialList(l, a, a, a, a)
			if err != nil {
				t.Fatal(err)
			}

			ok, err := pool.Unify(l, pl)
			if err != nil {
				t.Fatal(err)
			}
			if !ok {
				t.Fatal("expected true, got false")
			}

			iter := ListIterator{TermPool: &pool, List: pl}
			for iter.Next() {
				if a, _ := pool.Atom(iter.Current()); a != Atom('a') {
					t.Errorf("expected %d, got %d", Atom('a'), a)
				}
			}
			if err := iter.Err(); !errors.Is(err, errInvalidList) {
				t.Errorf("expected %v, got %v", errInvalidList, err)
			}
		})
	})
}

func TestListIterator_Suffix(t *testing.T) {
	pool := NewTermPool(1024)

	a, err := pool.PutAtom(Atom('a'))
	if err != nil {
		t.Fatal(err)
	}

	b, err := pool.PutAtom(Atom('b'))
	if err != nil {
		t.Fatal(err)
	}

	c, err := pool.PutAtom(Atom('c'))
	if err != nil {
		t.Fatal(err)
	}

	l, err := pool.PutList(a, b, c)
	if err != nil {
		t.Fatal(err)
	}

	iter := ListIterator{TermPool: &pool, List: l}
	if iter.Suffix() != l {
		t.Errorf("expected %v, got %v", l, iter.Suffix())
	}
	if !iter.Next() {
		t.Fatal("expected true, got false")
	}
	if iter.Suffix() != l+1 {
		t.Errorf("expected %v, got %v", l+1, iter.Suffix())
	}
	if !iter.Next() {
		t.Fatal("expected true, got false")
	}
	if iter.Suffix() != l+2 {
		t.Errorf("expected %d, got %d", l+2, iter.Suffix())
	}
	if !iter.Next() {
		t.Fatal("expected true, got false")
	}
	if iter.Suffix() != l+3 {
		t.Errorf("expected %d, got %d", l+3, iter.Suffix())
	}
	if iter.Next() {
		t.Error("expected false, got true")
	}
}

func TestSeqIterator_Next(t *testing.T) {
	t.Run("sequence", func(t *testing.T) {
		pool := NewTermPool(1024)

		a, err := pool.PutAtom(Atom('a'))
		if err != nil {
			t.Fatal(err)
		}

		b, err := pool.PutAtom(Atom('b'))
		if err != nil {
			t.Fatal(err)
		}

		c, err := pool.PutAtom(Atom('c'))
		if err != nil {
			t.Fatal(err)
		}

		s, err := pool.PutSequence(Atom(','), a, b, c)
		if err != nil {
			t.Fatal(err)
		}

		iter := seqIterator{TermPool: &pool, Seq: s}
		if !iter.Next() {
			t.Fatal("expected true, got false")
		}
		if iter.Current() != a {
			t.Errorf("expected %d, got %d", a, iter.Current())
		}
		if !iter.Next() {
			t.Fatal("expected true, got false")
		}
		if iter.Current() != b {
			t.Errorf("expected %d, got %d", b, iter.Current())
		}
		if !iter.Next() {
			t.Fatal("expected true, got false")
		}
		if iter.Current() != c {
			t.Errorf("expected %d, got %d", c, iter.Current())
		}
		if iter.Next() {
			t.Error("expected false, got true")
		}
	})

	t.Run("sequence with a trailing compound", func(t *testing.T) {
		pool := NewTermPool(1024)

		a, err := pool.PutAtom(Atom('a'))
		if err != nil {
			t.Fatal(err)
		}

		b, err := pool.PutAtom(Atom('b'))
		if err != nil {
			t.Fatal(err)
		}

		c, err := pool.PutAtom(Atom('c'))
		if err != nil {
			t.Fatal(err)
		}

		f, err := pool.PutCompound(Atom('f'), c)
		if err != nil {
			t.Fatal(err)
		}

		s, err := pool.PutSequence(Atom(','), a, b, f)
		if err != nil {
			t.Fatal(err)
		}

		iter := seqIterator{TermPool: &pool, Seq: s}
		if !iter.Next() {
			t.Fatal("expected true, got false")
		}
		if iter.Current() != a {
			t.Errorf("expected %d, got %d", a, iter.Current())
		}
		if !iter.Next() {
			t.Fatal("expected true, got false")
		}
		if iter.Current() != b {
			t.Errorf("expected %d, got %d", b, iter.Current())
		}
		if !iter.Next() {
			t.Fatal("expected true, got false")
		}
		if iter.Current() != f {
			t.Errorf("expected %d, got %d", f, iter.Current())
		}
		if iter.Next() {
			t.Error("expected false, got true")
		}
	})
}

func TestAltIterator_Next(t *testing.T) {
	t.Run("alternatives", func(t *testing.T) {
		pool := NewTermPool(1024)

		a, err := pool.PutAtom(Atom('a'))
		if err != nil {
			t.Fatal(err)
		}

		b, err := pool.PutAtom(Atom('b'))
		if err != nil {
			t.Fatal(err)
		}

		c, err := pool.PutAtom(Atom('c'))
		if err != nil {
			t.Fatal(err)
		}

		s, err := pool.PutSequence(Atom(';'), a, b, c)
		if err != nil {
			t.Fatal(err)
		}

		iter := altIterator{TermPool: &pool, Alt: s}
		if !iter.Next() {
			t.Fatal("expected true, got false")
		}
		if iter.Current() != a {
			t.Errorf("expected %d, got %d", a, iter.Current())
		}
		if !iter.Next() {
			t.Fatal("expected true, got false")
		}
		if iter.Current() != b {
			t.Errorf("expected %d, got %d", b, iter.Current())
		}
		if !iter.Next() {
			t.Fatal("expected true, got false")
		}
		if iter.Current() != c {
			t.Errorf("expected %d, got %d", c, iter.Current())
		}
		if iter.Next() {
			t.Error("expected false, got true")
		}
	})

	t.Run("alternatives with a trailing compound", func(t *testing.T) {
		pool := NewTermPool(1024)

		a, err := pool.PutAtom(Atom('a'))
		if err != nil {
			t.Fatal(err)
		}

		b, err := pool.PutAtom(Atom('b'))
		if err != nil {
			t.Fatal(err)
		}

		c, err := pool.PutAtom(Atom('c'))
		if err != nil {
			t.Fatal(err)
		}

		f, err := pool.PutCompound(Atom('f'), c)
		if err != nil {
			t.Fatal(err)
		}

		s, err := pool.PutSequence(Atom(';'), a, b, f)
		if err != nil {
			t.Fatal(err)
		}

		iter := altIterator{TermPool: &pool, Alt: s}
		if !iter.Next() {
			t.Fatal("expected true, got false")
		}
		if iter.Current() != a {
			t.Errorf("expected %d, got %d", a, iter.Current())
		}
		if !iter.Next() {
			t.Fatal("expected true, got false")
		}
		if iter.Current() != b {
			t.Errorf("expected %d, got %d", b, iter.Current())
		}
		if !iter.Next() {
			t.Fatal("expected true, got false")
		}
		if iter.Current() != f {
			t.Errorf("expected %d, got %d", f, iter.Current())
		}
		if iter.Next() {
			t.Error("expected false, got true")
		}
	})

	t.Run("if then else", func(t *testing.T) {
		pool := NewTermPool(1024)

		a, err := pool.PutAtom(Atom('a'))
		if err != nil {
			t.Fatal(err)
		}

		b, err := pool.PutAtom(Atom('b'))
		if err != nil {
			t.Fatal(err)
		}

		then, err := pool.PutCompound(NewAtom("->"), a, b)
		if err != nil {
			t.Fatal(err)
		}

		c, err := pool.PutAtom(Atom('c'))
		if err != nil {
			t.Fatal(err)
		}

		s, err := pool.PutSequence(Atom(';'), then, c)
		if err != nil {
			t.Fatal(err)
		}

		iter := altIterator{TermPool: &pool, Alt: s}
		if !iter.Next() {
			t.Fatal("expected true, got false")
		}
		if iter.Current() != s {
			t.Errorf("expected %d, got %d", s, iter.Current())
		}
		if iter.Next() {
			t.Error("expected false, got true")
		}
	})
}

func TestAnyIterator_Next(t *testing.T) {
	t.Run("proper list", func(t *testing.T) {
		pool := NewTermPool(1024)

		a, err := pool.PutAtom(Atom('a'))
		if err != nil {
			t.Fatal(err)
		}

		b, err := pool.PutAtom(Atom('b'))
		if err != nil {
			t.Fatal(err)
		}

		c, err := pool.PutAtom(Atom('c'))
		if err != nil {
			t.Fatal(err)
		}

		l, err := pool.PutList(a, b, c)
		if err != nil {
			t.Fatal(err)
		}

		iter := anyIterator{TermPool: &pool, Any: l}
		if !iter.Next() {
			t.Fatal("expected true, got false")
		}
		if iter.Current() != a {
			t.Errorf("expected %d, got %d", a, iter.Current())
		}
		if !iter.Next() {
			t.Fatal("expected true, got false")
		}
		if iter.Current() != b {
			t.Errorf("expected %d, got %d", b, iter.Current())
		}
		if !iter.Next() {
			t.Fatal("expected true, got false")
		}
		if iter.Current() != c {
			t.Errorf("expected %d, got %d", c, iter.Current())
		}
		if iter.Next() {
			t.Error("expected false, got true")
		}
		if err := iter.Err(); err != nil {
			t.Fatal(err)
		}
	})

	t.Run("improper list", func(t *testing.T) {
		t.Run("variable", func(t *testing.T) {
			pool := NewTermPool(1024)

			x, err := pool.PutVariable(NewVariable(&pool))
			if err != nil {
				t.Fatal(err)
			}

			a, err := pool.PutAtom(Atom('a'))
			if err != nil {
				t.Fatal(err)
			}

			b, err := pool.PutAtom(Atom('b'))
			if err != nil {
				t.Fatal(err)
			}

			pl, err := pool.PutPartialList(x, a, b)
			if err != nil {
				t.Fatal(err)
			}

			iter := anyIterator{TermPool: &pool, Any: pl}
			if !iter.Next() {
				t.Fatal("expected true, got false")
			}
			if iter.Current() != a {
				t.Errorf("expected %d, got %d", a, iter.Current())
			}
			if !iter.Next() {
				t.Fatal("expected true, got false")
			}
			if iter.Current() != b {
				t.Errorf("expected %d, got %d", b, iter.Current())
			}
			if iter.Next() {
				t.Error("expected false, got true")
			}
			if err := iter.Err(); !errors.Is(err, ErrInstantiation) {
				t.Fatal(err)
			}
		})

		t.Run("atom", func(t *testing.T) {
			pool := NewTermPool(1024)

			foo, err := pool.PutAtom(NewAtom("foo"))
			if err != nil {
				t.Fatal(err)
			}

			a, err := pool.PutAtom(Atom('a'))
			if err != nil {
				t.Fatal(err)
			}

			b, err := pool.PutAtom(Atom('b'))
			if err != nil {
				t.Fatal(err)
			}

			pl, err := pool.PutPartialList(foo, a, b)
			if err != nil {
				t.Fatal(err)
			}

			iter := anyIterator{TermPool: &pool, Any: pl}
			if !iter.Next() {
				t.Fatal("expected true, got false")
			}
			if iter.Current() != a {
				t.Errorf("expected %d, got %d", a, iter.Current())
			}
			if !iter.Next() {
				t.Fatal("expected true, got false")
			}
			if iter.Current() != b {
				t.Errorf("expected %d, got %d", b, iter.Current())
			}
			if iter.Next() {
				t.Error("expected false, got true")
			}
			if err := iter.Err(); !errors.Is(err, errInvalidList) {
				t.Errorf("expected: %v, got: %v", errInvalidList, err)
			}
		})
	})

	t.Run("sequence", func(t *testing.T) {
		pool := NewTermPool(1024)

		a, err := pool.PutAtom(Atom('a'))
		if err != nil {
			t.Fatal(err)
		}

		b, err := pool.PutAtom(Atom('b'))
		if err != nil {
			t.Fatal(err)
		}

		c, err := pool.PutAtom(Atom('c'))
		if err != nil {
			t.Fatal(err)
		}

		s, err := pool.PutSequence(Atom(','), a, b, c)
		if err != nil {
			t.Fatal(err)
		}

		iter := anyIterator{TermPool: &pool, Any: s}
		if !iter.Next() {
			t.Fatal("expected true, got false")
		}
		if iter.Current() != a {
			t.Errorf("expected %d, got %d", a, iter.Current())
		}
		if !iter.Next() {
			t.Fatal("expected true, got false")
		}
		if iter.Current() != b {
			t.Errorf("expected %d, got %d", b, iter.Current())
		}
		if !iter.Next() {
			t.Fatal("expected true, got false")
		}
		if iter.Current() != c {
			t.Errorf("expected %d, got %d", c, iter.Current())
		}
		if iter.Next() {
			t.Error("expected false, got true")
		}
		if err := iter.Err(); err != nil {
			t.Fatal(err)
		}
	})

	t.Run("single", func(t *testing.T) {
		pool := NewTermPool(1024)

		a, err := pool.PutAtom(Atom('a'))
		if err != nil {
			t.Fatal(err)
		}

		iter := anyIterator{TermPool: &pool, Any: a}
		if !iter.Next() {
			t.Fatal("expected true, got false")
		}
		if iter.Current() != a {
			t.Errorf("expected %d, got %d", a, iter.Current())
		}
		if iter.Next() {
			t.Error("expected false, got true")
		}
		if err := iter.Err(); err != nil {
			t.Fatal(err)
		}
	})
}
