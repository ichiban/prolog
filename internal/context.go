package internal

import "context"

type (
	CtxKeyVM   struct{}
	CtxKeyCont struct{}
)

func WithVM(ctx context.Context, vm *VM) context.Context {
	return context.WithValue(ctx, CtxKeyVM{}, vm)
}

func ContextVM(ctx context.Context) *VM {
	return ctx.Value(CtxKeyVM{}).(*VM)
}

func WithCont(ctx context.Context, cont Cont) context.Context {
	return context.WithValue(ctx, CtxKeyCont{}, cont)
}

func ContextCont(ctx context.Context) Cont {
	return ctx.Value(CtxKeyCont{}).(Cont)
}
