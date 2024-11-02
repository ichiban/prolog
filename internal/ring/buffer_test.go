package ring

import "testing"

func TestGet(t *testing.T) {
	b := NewBuffer[int](3)
	b.Put(1)
	b.Put(2)
	b.Put(3)

	if e := b.Get(); 1 != e {
		t.Errorf("expected 1 but got %d", e)
	}
	if e := b.Get(); 2 != e {
		t.Errorf("expected 2 but got %d", e)
	}
	if e := b.Get(); 3 != e {
		t.Errorf("expected 3 but got %d", e)
	}
}

func TestCurrent(t *testing.T) {
	b := NewBuffer[int](3)
	b.Put(1)
	b.Put(2)
	b.Put(3)

	if e := b.Current(); 1 != e {
		t.Errorf("expected 1 but got %d", e)
	}
	if e := b.Current(); 1 != e {
		t.Errorf("expected 1 but got %d", e)
	}
}

func TestEmpty(t *testing.T) {
	b := NewBuffer[int](3)

	if !b.Empty() {
		t.Error("expected true, got false")
	}

	b.Put(1)

	if b.Empty() {
		t.Error("expected false, got true")
	}
}

func TestBackup(t *testing.T) {
	b := NewBuffer[int](3)
	b.Put(0)
	if e := b.Get(); e != 0 {
		t.Errorf("expected 0 but got %d", e)
	}

	b.Put(1)
	b.Put(2)
	b.Put(3)

	if e := b.Get(); e != 1 {
		t.Errorf("expected 1 but got %d", e)
	}
	if e := b.Get(); e != 2 {
		t.Errorf("expected 2 but got %d", e)
	}

	b.Backup()
	b.Backup()

	if e := b.Get(); 1 != e {
		t.Errorf("expected 1 but got %d", e)
	}
	if e := b.Get(); 2 != e {
		t.Errorf("expected 2 but got %d", e)
	}
	if e := b.Get(); 3 != e {
		t.Errorf("expected 3 but got %d", e)
	}
}
