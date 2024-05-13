package engine

import (
	"context"
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestModule_Register0(t *testing.T) {
	var m module
	m.Register0("foo", func(_ *VM, k Cont, env *Env) *Promise {
		return k(env)
	})
	p := m.procedures[predicateIndicator{name: NewAtom("foo"), arity: 0}].procedure

	t.Run("ok", func(t *testing.T) {
		ok, err := p.call(nil, []Term{}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		ok, err := p.call(nil, []Term{NewAtom("a")}, Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)

		assert.Equal(t, "wrong number of arguments: expected=0, actual=[a]", err.Error())
	})
}

func TestModule_Register1(t *testing.T) {
	var m module
	m.Register1("foo", func(_ *VM, a Term, k Cont, env *Env) *Promise {
		return k(env)
	})
	p := m.procedures[predicateIndicator{name: NewAtom("foo"), arity: 1}].procedure

	t.Run("ok", func(t *testing.T) {
		ok, err := p.call(nil, []Term{NewAtom("a")}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		ok, err := p.call(nil, []Term{NewAtom("a"), NewAtom("b")}, Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})
}

func TestModule_Register2(t *testing.T) {
	var m module
	m.Register2("foo", func(_ *VM, a, b Term, k Cont, env *Env) *Promise {
		return k(env)
	})
	p := m.procedures[predicateIndicator{name: NewAtom("foo"), arity: 2}].procedure

	t.Run("ok", func(t *testing.T) {
		ok, err := p.call(nil, []Term{NewAtom("a"), NewAtom("b")}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		ok, err := p.call(nil, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c")}, Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})
}

func TestModule_Register3(t *testing.T) {
	var m module
	m.Register3("foo", func(_ *VM, a, b, c Term, k Cont, env *Env) *Promise {
		return k(env)
	})
	p := m.procedures[predicateIndicator{name: NewAtom("foo"), arity: 3}].procedure

	t.Run("ok", func(t *testing.T) {
		ok, err := p.call(nil, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c")}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		ok, err := p.call(nil, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c"), NewAtom("d")}, Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})
}

func TestModule_Register4(t *testing.T) {
	var m module
	m.Register4("foo", func(_ *VM, a, b, c, d Term, k Cont, env *Env) *Promise {
		return k(env)
	})
	p := m.procedures[predicateIndicator{name: NewAtom("foo"), arity: 4}].procedure

	t.Run("ok", func(t *testing.T) {
		ok, err := p.call(nil, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c"), NewAtom("d")}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		ok, err := p.call(nil, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e")}, Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})
}

func TestModule_Register5(t *testing.T) {
	var m module
	m.Register5("foo", func(_ *VM, a, b, c, d, e Term, k Cont, env *Env) *Promise {
		return k(env)
	})
	p := m.procedures[predicateIndicator{name: NewAtom("foo"), arity: 5}].procedure

	t.Run("ok", func(t *testing.T) {
		ok, err := p.call(nil, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e")}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		ok, err := p.call(nil, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f")}, Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})
}

func TestModule_Register6(t *testing.T) {
	var m module
	m.Register6("foo", func(_ *VM, a, b, c, d, e, f Term, k Cont, env *Env) *Promise {
		return k(env)
	})
	p := m.procedures[predicateIndicator{name: NewAtom("foo"), arity: 6}].procedure

	t.Run("ok", func(t *testing.T) {
		ok, err := p.call(nil, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f")}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		ok, err := p.call(nil, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f"), NewAtom("g")}, Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})
}

func TestModule_Register7(t *testing.T) {
	var m module
	m.Register7("foo", func(_ *VM, a, b, c, d, e, f, g Term, k Cont, env *Env) *Promise {
		return k(env)
	})
	p := m.procedures[predicateIndicator{name: NewAtom("foo"), arity: 7}].procedure

	t.Run("ok", func(t *testing.T) {
		ok, err := p.call(nil, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f"), NewAtom("g")}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		ok, err := p.call(nil, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f"), NewAtom("g"), NewAtom("h")}, Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})
}

func TestModule_Register8(t *testing.T) {
	var m module
	m.Register8("foo", func(_ *VM, a, b, c, d, e, f, g, h Term, k Cont, env *Env) *Promise {
		return k(env)
	})
	p := m.procedures[predicateIndicator{name: NewAtom("foo"), arity: 8}].procedure

	t.Run("ok", func(t *testing.T) {
		ok, err := p.call(nil, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f"), NewAtom("g"), NewAtom("h")}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		ok, err := p.call(nil, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f"), NewAtom("g"), NewAtom("h"), NewAtom("i")}, Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})
}
