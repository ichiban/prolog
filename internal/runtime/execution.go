// Ported to Go from BinProlog (github.com/ptarau/binprolog, src/engine.c and
// related sources), Copyright (C) Paul Tarau, licensed under Apache-2.0.
// This file has been modified: translated to Go and adapted.

package runtime

import (
	"context"
	"errors"
	"fmt"
	"iter"
	"math"

	"github.com/ichiban/prolog/v2/internal/term"
	"github.com/ichiban/prolog/v2/internal/wam"
)

const (
	maxRegisters = 1024
)

type stackFrame struct {
	programPointer int                       // P, next clause address
	heapTop        int                       // H, saved top of the heap
	trailTop       int                       // TR, saved top of the trail
	tempVars       [maxRegisters]term.Handle // The backing array to save An TODO: Maybe store them in a sidecar array, or put $tempVars(...) to the heap?
	cutB           int

	next func() (error, bool) // for built-in predicates
	stop func()               // for built-in predicates
}

type structurePointer struct {
	term  term.Handle
	argNo int
}

type Execution struct {
	*Engine

	location term.Functor

	programPointer int // P

	stack []stackFrame // A

	trail []term.Handle // TR

	heapBacktrackPoint int              // HB
	structurePointer   structurePointer // S

	tempVars [maxRegisters]term.Handle // Xn
	cutB     int

	mode wam.Mode
}

func (e *Execution) run(ctx context.Context) iter.Seq[error] {
	return func(yield func(error) bool) {
		var (
			image = e.Engine.Image
			code  = image.Code
		)
		for e.programPointer < len(code) {
			var (
				inst = code[e.programPointer]
				op   = inst.Op
				i    = inst.I
				n    = inst.N
			)
			switch op {
			case wam.OpNop, wam.OpNondet: // nop
				e.Next()
			case wam.OpPutVariable: // put_variable Xn, Ai
				v, err := e.PutVariable()
				if err != nil {
					_ = yield(err)
					return
				}
				e.tempVars[n] = v
				e.tempVars[i] = v
				e.Next()
			case wam.OpPutConstant: // put_constant c, Xi
				k := e.Constants[n]
				e.tempVars[i] = k
				e.Next()
			case wam.OpPutStructure: // put_structure f/n, Xi
				f := image.Functors[n]
				s, err := e.PutStructure(f)
				if err != nil {
					_ = yield(err)
					return
				}
				e.tempVars[i] = s
				e.Next()
			case wam.OpPushStructure: // push_structure f/n, Xi
				f := image.Functors[n]
				s, err := e.PutStructure(f)
				if err != nil {
					_ = yield(err)
					return
				}
				v := e.tempVars[i]
				if err := e.Bind(v, s); err != nil {
					_ = yield(err)
					return
				}
				e.trail = append(e.trail, v)
				e.Next()
			case wam.OpWriteConstant: // write_constant c
				k := e.Constants[n]
				if _, err := e.Put(k); err != nil {
					_ = yield(err)
					return
				}
				e.Next()
			case wam.OpGetValue: // get_value Xn, Ai
				t := e.tempVars[n]
				s := e.tempVars[i]
				ok, err := e.Unify(t, s, term.OnBind(func(v term.Handle) {
					e.trail = append(e.trail, v)
				}))
				if err != nil {
					_ = yield(err)
					return
				}
				if !ok {
					if !e.Backtrack() {
						return
					}
					continue
				}
				e.Next()
			case wam.OpGetConstant: // get_constant c, Xi
				k := e.Constants[n]
				t := e.Deref(e.tempVars[i])
				if _, ok := e.Variable(t); ok {
					if err := e.Bind(t, k); err != nil {
						_ = yield(err)
						return
					}
					e.trail = append(e.trail, t)
				} else if o := e.Compare(t, k); o != 0 {
					if !e.Backtrack() {
						return
					}
					continue
				}
				e.Next()
			case wam.OpGetStructure: // get_structure f/n, Xi
				f := image.Functors[n]
				t := e.Deref(e.tempVars[i])
				if _, ok := e.Variable(t); ok {
					s, err := e.PutStructure(f)
					if err != nil {
						_ = yield(err)
						return
					}
					if err := e.Bind(t, s); err != nil {
						_ = yield(err)
						return
					}
					e.trail = append(e.trail, t)
					e.mode = wam.ModeWrite
					e.Next()
				} else if g, ok := e.Functor(t); ok && f == g {
					e.structurePointer = structurePointer{
						term:  t,
						argNo: 0,
					}
					e.mode = wam.ModeRead
					e.Next()
				} else {
					if !e.Backtrack() {
						return
					}
				}
			case wam.OpUnifyVariable: // unify_variable Xi
				if e.mode == wam.ModeRead {
					s := e.structurePointer
					e.tempVars[i] = e.Arg(s.term, s.argNo)
					e.structurePointer.argNo++
					e.Next()
					break
				}
				fallthrough
			case wam.OpWriteVariable: // write_variable Xi
				t, err := e.PutVariable()
				if err != nil {
					_ = yield(err)
					return
				}
				e.tempVars[i] = t
				e.Next()
			case wam.OpUnifyValue: // unify_value Xi
				if e.mode == wam.ModeRead {
					s := e.structurePointer
					ok, err := e.Unify(e.tempVars[i], e.Arg(s.term, s.argNo), term.OnBind(func(v term.Handle) {
						e.trail = append(e.trail, v)
					}))
					if err != nil {
						_ = yield(err)
						return
					}
					if !ok {
						if !e.Backtrack() {
							return
						}
						continue
					}
					e.structurePointer.argNo++
					e.Next()
					break
				}
				fallthrough
			case wam.OpWriteValue: // write_value Xi
				t := e.tempVars[i]
				t = e.Deref(t)
				if _, err := e.Put(t); err != nil {
					_ = yield(err)
					return
				}
				e.Next()
			case wam.OpUnifyVoid:
				if e.mode == wam.ModeRead {
					e.structurePointer.argNo++
					e.Next()
					break
				}
				fallthrough
			case wam.OpWriteVoid:
				if _, err := e.PutVariable(); err != nil {
					_ = yield(err)
					return
				}
				e.Next()
			case wam.OpUnifyConstant:
				c := e.Constants[n]
				if e.mode == wam.ModeRead {
					s := e.structurePointer
					a := e.Arg(s.term, s.argNo)
					a = e.Deref(a)
					if _, ok := e.Variable(a); ok {
						if err := e.Bind(a, c); err != nil {
							_ = yield(err)
							return
						}
						e.trail = append(e.trail, a)
					}
					_, ok := e.Functor(a)
					if ok || e.Compare(a, c) != 0 {
						if !e.Backtrack() {
							return
						}
						continue
					}
					e.structurePointer.argNo++
					e.Next()
					break
				}
				if _, err := e.Put(c); err != nil {
					_ = yield(err)
					return
				}
				e.Next()
			case wam.OpExecute: // execute P
				if err := ctx.Err(); err != nil {
					_ = yield(err)
					return
				}
				pi := e.Functors[n]
				p, ok := e.Predicates[pi]
				if !ok {
					culprit, err := e.PutFunctor(pi)
					if err != nil {
						_ = yield(err)
						return
					}
					_ = yield(&ExistenceError{
						ErrorContext: ErrorContext{
							Location: e.location,
						},
						ObjectType: "procedure",
						Culprit:    culprit,
					})
					return
				}
				e.location = term.NewFunctor(pi.Name(), pi.Arity()-1)
				e.programPointer = p.Offset
			case wam.OpProceed: // proceed
				if !yield(nil) {
					return
				}
				if !e.Backtrack() {
					return
				}
			case wam.OpTryMeElse: // try_me_else L
				arity := int(inst.I)
				f := stackFrame{
					programPointer: int(n),
					heapTop:        len(e.Heap),
					trailTop:       len(e.trail),
				}
				copy(f.tempVars[:arity+1], e.tempVars[:arity+1])
				e.stack = append(e.stack, f)
				e.heapBacktrackPoint = len(e.Heap)
				e.Next()
			case wam.OpRetryMeElse: // retry_me_else L
				arity := int(inst.I)
				e.stack[len(e.stack)-1].programPointer = int(n)
				if err := e.restoreState(arity); err != nil {
					_ = yield(err)
					return
				}
				e.stack = e.stack[:len(e.stack)+1]
				e.Next()
			case wam.OpTrustMe: // trust_me
				arity := int(inst.I)
				if err := e.restoreState(arity); err != nil {
					_ = yield(err)
					return
				}
				e.Next()
			case wam.OpMove: // move Xn<-Xi
				e.tempVars[n] = e.tempVars[i]
				e.Next()
			case wam.OpSwitch: // switch
				pi := e.Functors[n]
				var (
					t     = e.tempVars[1]
					arity int
				)
				t = e.Deref(t)
				if f, ok := e.Functor(t); ok {
					var err error
					t, err = e.PutAtom(f.Name())
					if err != nil {
						_ = yield(err)
						return
					}
					arity = f.Arity()
				}
				if i, ok := e.FirstArgIndex[wam.FirstArgKey{
					PI:    pi,
					Term:  t,
					Arity: arity,
				}]; ok {
					e.jumpTo(i)
					continue
				}
				e.Next()
			case wam.OpPutCut: // put_cut
				e.stack = e.stack[:e.cutB]
				e.Next()
			case wam.OpGetCut: // get_cut TODO: Do we really need this?
				t := e.tempVars[1]
				t = e.Deref(t)
				n, _ := e.Integer(t)
				e.stack = e.stack[:n]
				e.Next()
			case wam.OpPushCut: // push_cut
				if e.cutB > math.MaxInt32 {
					_ = yield(fmt.Errorf("cut b is too large"))
					return
				}
				cb, err := e.PutInteger(int64(e.cutB))
				if err != nil {
					_ = yield(err)
					return
				}
				if _, err := e.Put(cb); err != nil {
					_ = yield(err)
					return
				}
				e.Next()
			default: // Builtins
				if inst.Op < wam.OpBuiltin0 {
					_ = yield(fmt.Errorf("unknown op %v", inst.Op))
					return
				}
				bid := int(inst.Op - wam.OpBuiltin0)
				b := e.BuiltinSet.Get(bid)
				ok, err := b.Proc(ctx, e)
				if err != nil {
					_ = yield(err)
					return
				}
				if !ok {
					return
				}
			}
		}
		_ = yield(errors.New("invalid end of code"))
		return
	}
}

func (e *Execution) Next() {
	e.programPointer++
}

func (e *Execution) jumpTo(addr int) {
	e.programPointer = addr
}

func (e *Execution) Backtrack() bool {
	if len(e.stack) == 0 {
		return false
	}
	f := e.stack[len(e.stack)-1]
	e.cutB = f.cutB
	e.programPointer = f.programPointer
	return true
}

func (e *Execution) restoreState(arity int) error {
	var f stackFrame
	f, e.stack = e.stack[len(e.stack)-1], e.stack[:len(e.stack)-1]
	if err := e.unwindTrail(f.trailTop); err != nil {
		return err
	}
	e.Heap = e.Heap[:f.heapTop]
	copy(e.tempVars[:arity+1], f.tempVars[:arity+1])
	e.cutB = f.cutB
	return nil
}

func (e *Execution) unwindTrail(trailTop int) error {
	for i := len(e.trail) - 1; i >= trailTop; i-- {
		v := e.trail[i]
		if err := e.Unbind(v); err != nil {
			return err
		}
	}
	e.trail = e.trail[:trailTop]
	return nil
}
