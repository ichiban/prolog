package internal

import (
	"context"
	"testing"
)

func TestModule_SetPredicate0(t *testing.T) {
	var m Module
	m.SetPredicate0("foo", func(ctx context.Context) Promise {
		return Continue(ctx)
	})
	p := m.procedures[Functor{Name: NewAtom("foo"), Arity: 0}].procedure

	t.Run("ok", func(t *testing.T) {
		ok, err := p.call(context.Background(), nil, []Term{}, Success).Force(Stack{})
		if err != nil {
			t.Error(err)
		}
		if !ok {
			t.Error("expected true")
		}
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		ok, err := p.call(context.Background(), nil, []Term{1}, Success).Force(Stack{})
		if err == nil {
			t.Error("expected error")
		}
		if ok {
			t.Error("expected false")
		}
	})
}

func TestModule_SetPredicate1(t *testing.T) {
	var m Module
	m.SetPredicate1("foo", func(ctx context.Context, a Term) Promise {
		return Continue(ctx)
	})
	p := m.procedures[Functor{Name: NewAtom("foo"), Arity: 1}].procedure

	t.Run("ok", func(t *testing.T) {
		ok, err := p.call(context.Background(), nil, []Term{1}, Success).Force(Stack{})
		if err != nil {
			t.Error(err)
		}
		if !ok {
			t.Error("expected true")
		}
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		ok, err := p.call(context.Background(), nil, []Term{1, 2}, Success).Force(Stack{})
		if err == nil {
			t.Error("expected error")
		}
		if ok {
			t.Error("expected false")
		}
	})
}

func TestModule_SetPredicate2(t *testing.T) {
	var m Module
	m.SetPredicate2("foo", func(ctx context.Context, a, b Term) Promise {
		return Continue(ctx)
	})
	p := m.procedures[Functor{Name: NewAtom("foo"), Arity: 2}].procedure

	t.Run("ok", func(t *testing.T) {
		ok, err := p.call(context.Background(), nil, []Term{1, 2}, Success).Force(Stack{})
		if err != nil {
			t.Error(err)
		}
		if !ok {
			t.Error("expected true")
		}
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		ok, err := p.call(context.Background(), nil, []Term{1, 2, 3}, Success).Force(Stack{})
		if err == nil {
			t.Error("expected error")
		}
		if ok {
			t.Error("expected false")
		}
	})
}

func TestModule_SetPredicate3(t *testing.T) {
	var m Module
	m.SetPredicate3("foo", func(ctx context.Context, a, b, c Term) Promise {
		return Continue(ctx)
	})
	p := m.procedures[Functor{Name: NewAtom("foo"), Arity: 3}].procedure

	t.Run("ok", func(t *testing.T) {
		ok, err := p.call(context.Background(), nil, []Term{1, 2, 3}, Success).Force(Stack{})
		if err != nil {
			t.Error(err)
		}
		if !ok {
			t.Error("expected true")
		}
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		ok, err := p.call(context.Background(), nil, []Term{1, 2, 3, 4}, Success).Force(Stack{})
		if err == nil {
			t.Error("expected error")
		}
		if ok {
			t.Error("expected false")
		}
	})
}

func TestModule_SetPredicate4(t *testing.T) {
	var m Module
	m.SetPredicate4("foo", func(ctx context.Context, a, b, c, d Term) Promise {
		return Continue(ctx)
	})
	p := m.procedures[Functor{Name: NewAtom("foo"), Arity: 4}].procedure

	t.Run("ok", func(t *testing.T) {
		ok, err := p.call(context.Background(), nil, []Term{1, 2, 3, 4}, Success).Force(Stack{})
		if err != nil {
			t.Error(err)
		}
		if !ok {
			t.Error("expected true")
		}
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		ok, err := p.call(context.Background(), nil, []Term{1, 2, 3, 4, 5}, Success).Force(Stack{})
		if err == nil {
			t.Error("expected error")
		}
		if ok {
			t.Error("expected false")
		}
	})
}

func TestModule_SetPredicate5(t *testing.T) {
	var m Module
	m.SetPredicate5("foo", func(ctx context.Context, a, b, c, d, e Term) Promise {
		return Continue(ctx)
	})
	p := m.procedures[Functor{Name: NewAtom("foo"), Arity: 5}].procedure

	t.Run("ok", func(t *testing.T) {
		ok, err := p.call(context.Background(), nil, []Term{1, 2, 3, 4, 5}, Success).Force(Stack{})
		if err != nil {
			t.Error(err)
		}
		if !ok {
			t.Error("expected true")
		}
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		ok, err := p.call(context.Background(), nil, []Term{1, 2, 3, 4, 5, 6}, Success).Force(Stack{})
		if err == nil {
			t.Error("expected error")
		}
		if ok {
			t.Error("expected false")
		}
	})
}

func TestModule_SetPredicate6(t *testing.T) {
	var m Module
	m.SetPredicate6("foo", func(ctx context.Context, a, b, c, d, e, f Term) Promise {
		return Continue(ctx)
	})
	p := m.procedures[Functor{Name: NewAtom("foo"), Arity: 6}].procedure

	t.Run("ok", func(t *testing.T) {
		ok, err := p.call(context.Background(), nil, []Term{1, 2, 3, 4, 5, 6}, Success).Force(Stack{})
		if err != nil {
			t.Error(err)
		}
		if !ok {
			t.Error("expected true")
		}
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		ok, err := p.call(context.Background(), nil, []Term{1, 2, 3, 4, 5, 6, 7}, Success).Force(Stack{})
		if err == nil {
			t.Error("expected error")
		}
		if ok {
			t.Error("expected false")
		}
	})
}

func TestModule_SetPredicate7(t *testing.T) {
	var m Module
	m.SetPredicate7("foo", func(ctx context.Context, a, b, c, d, e, f, g Term) Promise {
		return Continue(ctx)
	})
	p := m.procedures[Functor{Name: NewAtom("foo"), Arity: 7}].procedure

	t.Run("ok", func(t *testing.T) {
		ok, err := p.call(context.Background(), nil, []Term{1, 2, 3, 4, 5, 6, 7}, Success).Force(Stack{})
		if err != nil {
			t.Error(err)
		}
		if !ok {
			t.Error("expected true")
		}
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		ok, err := p.call(context.Background(), nil, []Term{1, 2, 3, 4, 5, 6, 7, 8}, Success).Force(Stack{})
		if err == nil {
			t.Error("expected error")
		}
		if ok {
			t.Error("expected false")
		}
	})
}

func TestModule_SetPredicate8(t *testing.T) {
	var m Module
	m.SetPredicate8("foo", func(ctx context.Context, a, b, c, d, e, f, g, h Term) Promise {
		return Continue(ctx)
	})
	p := m.procedures[Functor{Name: NewAtom("foo"), Arity: 8}].procedure

	t.Run("ok", func(t *testing.T) {
		ok, err := p.call(context.Background(), nil, []Term{1, 2, 3, 4, 5, 6, 7, 8}, Success).Force(Stack{})
		if err != nil {
			t.Error(err)
		}
		if !ok {
			t.Error("expected true")
		}
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		ok, err := p.call(context.Background(), nil, []Term{1, 2, 3, 4, 5, 6, 7, 8, 9}, Success).Force(Stack{})
		if err == nil {
			t.Error("expected error")
		}
		if ok {
			t.Error("expected false")
		}
	})
}
