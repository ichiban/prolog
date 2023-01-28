package engine

import (
	"context"
	"errors"
)

var errNoVM = errors.New("no VM")

type envCtxKey struct{}

func withEnv(parent context.Context, env *Env) context.Context {
	return context.WithValue(parent, envCtxKey{}, env)
}

func withUnification(parent context.Context, x, y Term) (context.Context, bool) {
	env := env(parent)
	env, ok := env.Unify(x, y)
	if !ok {
		return nil, false
	}
	return withEnv(parent, env), true
}

func env(ctx context.Context) *Env {
	v := ctx.Value(envCtxKey{})
	if v == nil {
		return nil
	}
	return v.(*Env)
}

type vmCtxKey struct{}

func withVM(parent context.Context, vm *VM) context.Context {
	return context.WithValue(parent, vmCtxKey{}, vm)
}

func vm(ctx context.Context) (*VM, error) {
	vm, ok := ctx.Value(vmCtxKey{}).(*VM)
	if !ok {
		return nil, errNoVM
	}
	return vm, nil
}

type prologCtxKey struct{}

func withPrologContext(parent context.Context, pc Term) context.Context {
	return context.WithValue(parent, prologCtxKey{}, pc)
}

var rootContext = NewAtom("toplevel")

func prologContext(ctx context.Context) Term {
	pc, ok := ctx.Value(prologCtxKey{}).(Term)
	if !ok {
		pc = rootContext
	}
	return pc
}

type contCtxKey struct{}

func WithCont(parent context.Context, k Cont) context.Context {
	return context.WithValue(parent, contCtxKey{}, k)
}

func cont(ctx context.Context) Cont {
	k, ok := ctx.Value(contCtxKey{}).(Cont)
	if !ok {
		k = func(context.Context) *Promise {
			return Bool(true)
		}
	}
	return k
}

func Continue(ctx context.Context) *Promise {
	k := cont(ctx)
	return k(ctx)
}
