package side

import (
	"math"
	"testing"
)

func TestTable_Add(t *testing.T) {
	var tb Table[string]

	a := tb.Add("a")
	b := tb.Add("b")

	if a == b {
		t.Errorf("distinct entries share an id: %d", a)
	}
	if got := tb.Get(a); got != "a" {
		t.Errorf("expected: %s, got: %s", "a", got)
	}
	if got := tb.Get(b); got != "b" {
		t.Errorf("expected: %s, got: %s", "b", got)
	}

	// Unlike a Pool, a Table doesn't deduplicate.
	if got := tb.Add("a"); got == a {
		t.Errorf("equal values share an id: %d", got)
	}
}

func TestTable_Sweep(t *testing.T) {
	t.Run("unmarked entries are collected", func(t *testing.T) {
		var tb Table[string]
		a := tb.Add("a")
		b := tb.Add("b")

		tb.Mark(a)
		tb.Sweep()

		if got := tb.Get(a); got != "a" {
			t.Errorf("expected: %s, got: %s", "a", got)
		}
		if got := tb.Get(b); got != "" {
			t.Errorf("expected: %s, got: %s", "", got)
		}
	})

	t.Run("a collected id is recycled", func(t *testing.T) {
		var tb Table[string]
		a := tb.Add("a")
		b := tb.Add("b")

		tb.Mark(a)
		tb.Sweep()

		if got := tb.Add("c"); got != b {
			t.Errorf("expected: %d, got: %d", b, got)
		}
		if got := tb.Get(b); got != "c" {
			t.Errorf("expected: %s, got: %s", "c", got)
		}
		if got := tb.Get(a); got != "a" {
			t.Errorf("survivor was corrupted: expected: %s, got: %s", "a", got)
		}
	})

	t.Run("marks don't carry over to the next sweep", func(t *testing.T) {
		var tb Table[string]
		a := tb.Add("a")

		tb.Mark(a)
		tb.Sweep()
		tb.Sweep() // Not marked this time around.

		if got := tb.Get(a); got != "" {
			t.Errorf("entry survived a sweep it wasn't marked for: %s", got)
		}
	})

	t.Run("the free list holds more than one entry", func(t *testing.T) {
		var tb Table[string]
		tb.Add("a")
		tb.Add("b")
		tb.Add("c")

		tb.Sweep()

		for _, v := range []string{"x", "y", "z"} {
			if got := tb.Get(tb.Add(v)); got != v {
				t.Errorf("expected: %s, got: %s", v, got)
			}
		}
		if got := len(tb.entries); got != 3 {
			t.Errorf("expected: %d, got: %d", 3, got)
		}
	})

	t.Run("an entry isn't freed twice", func(t *testing.T) {
		var tb Table[string]
		a := tb.Add("a")
		tb.Add("b")

		tb.Mark(a)
		tb.Sweep()
		tb.Mark(a)
		tb.Sweep() // "b"'s entry is already free.

		x, y := tb.Add("x"), tb.Add("y")
		if x == y {
			t.Errorf("distinct entries share an id: %d", x)
		}
		if got := tb.Get(x); got != "x" {
			t.Errorf("expected: %s, got: %s", "x", got)
		}
		if got := tb.Get(y); got != "y" {
			t.Errorf("expected: %s, got: %s", "y", got)
		}
	})
}

// A Table takes any type, including the ones a Pool can't key on:
// an uncomparable Stream, or a float where ±0.0 collide and NaN never matches.
func TestTable_uncomparable(t *testing.T) {
	var tb Table[func()]
	if id := tb.Add(func() {}); tb.Get(id) == nil {
		t.Error("expected a func, got nil")
	}

	var fs Table[float64]
	pos := fs.Add(0)
	neg := fs.Add(math.Copysign(0, -1))
	if pos == neg {
		t.Errorf("+0.0 and -0.0 share an id: %d", pos)
	}
}
