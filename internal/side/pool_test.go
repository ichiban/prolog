package side

import (
	"testing"
)

func TestPool_Add(t *testing.T) {
	var p Pool[string]

	a := p.Add("a")
	b := p.Add("b")

	if a == b {
		t.Errorf("distinct values share an id: %d", a)
	}
	if got := p.Add("a"); got != a {
		t.Errorf("expected: %d, got: %d", a, got)
	}
	if got := p.Get(a); got != "a" {
		t.Errorf("expected: %s, got: %s", "a", got)
	}
	if got := p.Get(b); got != "b" {
		t.Errorf("expected: %s, got: %s", "b", got)
	}
}

func TestPool_Sweep(t *testing.T) {
	t.Run("unmarked entries are collected", func(t *testing.T) {
		var p Pool[string]
		a := p.Add("a")
		p.Add("b")

		p.Mark(a)
		p.Sweep()

		if got := p.Get(a); got != "a" {
			t.Errorf("marked entry was collected: expected: %s, got: %s", "a", got)
		}
		if _, ok := p.ids["b"]; ok {
			t.Error("unmarked entry survived")
		}
	})

	t.Run("a collected id is recycled", func(t *testing.T) {
		var p Pool[string]
		a := p.Add("a")
		b := p.Add("b")

		p.Mark(a)
		p.Sweep()

		if got := p.Add("c"); got != b {
			t.Errorf("expected: %d, got: %d", b, got)
		}
		if got := p.Get(b); got != "c" {
			t.Errorf("expected: %s, got: %s", "c", got)
		}
		if got := p.Get(a); got != "a" {
			t.Errorf("survivor was corrupted: expected: %s, got: %s", "a", got)
		}
	})

	t.Run("marks don't carry over to the next sweep", func(t *testing.T) {
		var p Pool[string]
		a := p.Add("a")

		p.Mark(a)
		p.Sweep()
		p.Sweep() // Not marked this time around.

		if _, ok := p.ids["a"]; ok {
			t.Error("entry survived a sweep it wasn't marked for")
		}
	})

	t.Run("the free list holds more than one entry", func(t *testing.T) {
		var p Pool[string]
		p.Add("a")
		p.Add("b")
		p.Add("c")

		p.Sweep()

		for _, v := range []string{"x", "y", "z"} {
			p.Add(v)
		}
		if len(p.entries) != 3 {
			t.Errorf("expected: %d, got: %d", 3, len(p.entries))
		}
		for _, v := range []string{"x", "y", "z"} {
			if got := p.Get(p.Add(v)); got != v {
				t.Errorf("expected: %s, got: %s", v, got)
			}
		}
	})
}
