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
	"slices"

	"github.com/ichiban/prolog/v2/internal/term"
	"github.com/ichiban/prolog/v2/internal/wam"
)

const (
	maxRegisters = 1024
)

// gcEveryExecute makes the engine collect before every execute instead of
// waiting for the heap to fill. Tests set it: a root the set is missing only
// misleads once a collection lands between the write and the read, which on the
// normal schedule almost never happens.
var gcEveryExecute bool

type stackFrame struct {
	programPointer int       // P, next clause address
	heapTop        int       // H, saved top of the heap
	trailTop       int       // TR, saved top of the trail
	tempVars       term.Cell // The backing array in the form of '$temp_vars'(A1, ..., An) to save An
	cutB           int       // B0, cut pointer

	// for built-in predicates
	next       func() (Promise, bool)
	stop       func()
	activation *Activation
}

func (s *stackFrame) Close() {
	if s.stop != nil {
		s.stop()
	}
	if s.activation != nil {
		s.activation.Close()
	}
}

type structurePointer struct {
	term  term.Cell
	argNo int
}

type Execution struct {
	*Engine

	programPointer int // P

	stack []stackFrame // B = len(stack)

	trail []term.Cell // TR

	heapBacktrackPoint int              // HB
	structurePointer   structurePointer // S

	tempVars [maxRegisters]term.Cell // Xn
	cutB     int                     // B0

	// liveRegs is how many argument registers hold a term right now, so that GC
	// roots X1..X_liveRegs and nothing else. Registers above it belong to an
	// abandoned branch, and the heap they point into has already been cut back.
	liveRegs int

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
				ok, err := e.Unify(t, s)
				if err != nil {
					_ = yield(err)
					return
				}
				if !ok {
					ok, err := e.Backtrack()
					if err != nil {
						_ = yield(err)
						return
					}
					if !ok {
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
					ok, err := e.Backtrack()
					if err != nil {
						_ = yield(err)
						return
					}
					if !ok {
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
					ok, err := e.Backtrack()
					if err != nil {
						_ = yield(err)
						return
					}
					if !ok {
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
					ok, err := e.Unify(e.tempVars[i], e.Arg(s.term, s.argNo))
					if err != nil {
						_ = yield(err)
						return
					}
					if !ok {
						ok, err := e.Backtrack()
						if err != nil {
							_ = yield(err)
							return
						}
						if !ok {
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
						ok, err := e.Backtrack()
						if err != nil {
							_ = yield(err)
							return
						}
						if !ok {
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
				bpi := e.Functors[n]
				p, ok, err := e.Predicate(bpi)
				if err != nil {
					_ = yield(err)
					return
				}
				if !ok {
					ok, err := e.Backtrack()
					if err != nil {
						_ = yield(err)
						return
					}
					if !ok {
						return
					}
					continue
				}
				if p.Dynamic {
					g, err := e.PutCompound(bpi.Name(), e.tempVars[1:bpi.Arity()]...)
					if err != nil {
						_ = yield(err)
						return
					}
					cont := e.tempVars[bpi.Arity()]
					call, ok, err := e.Predicate(term.NewFunctor(term.NewAtom("call"), 2))
					if err != nil {
						_ = yield(err)
						return
					}
					if !ok {
						_ = yield(errors.New("call/2 not defined"))
						return
					}
					e.enter(call.Offset, concat(singleton(g), singleton(cont)))
					continue
				}

				e.liveRegs = bpi.Arity()

				if e.gcThreshold == 0 {
					e.setNextGCThreshold()
				}
				if gcEveryExecute || e.gcThreshold <= len(e.Heap) {
					e.Engine.GC(e.Engine.roots(), e.Engine.heapTops())
					e.setNextGCThreshold()
				}

				e.location = term.NewFunctor(bpi.Name(), bpi.Arity()-1)
				e.programPointer = p.Offset
				e.cutB = len(e.stack)
			case wam.OpProceed: // proceed
				if !yield(nil) {
					return
				}
				ok, err := e.Backtrack()
				if err != nil {
					_ = yield(err)
					return
				}
				if !ok {
					return
				}
			case wam.OpTryMeElse: // try_me_else L
				arity := int(inst.I)
				tvs, err := e.PutCompound(term.NewAtom("$temp_vars"), e.tempVars[1:arity+1]...)
				if err != nil {
					_ = yield(err)
					return
				}
				f := stackFrame{
					programPointer: int(n),
					heapTop:        len(e.Heap),
					trailTop:       len(e.trail),
					tempVars:       tvs,
					cutB:           e.cutB,
				}
				e.stack = append(e.stack, f)
				e.heapBacktrackPoint = len(e.Heap)
				e.Next()
			case wam.OpRetryMeElse: // retry_me_else L
				e.stack[len(e.stack)-1].programPointer = int(n)
				if err := e.restoreState(); err != nil {
					_ = yield(err)
					return
				}
				e.stack = e.stack[:len(e.stack)+1]
				e.Next()
			case wam.OpTrustMe: // trust_me
				if err := e.restoreState(); err != nil {
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
				p, _ := e.Predicates[pi]
				key := wam.FirstArgKey{
					Term:  t,
					Arity: arity,
				}
				if i := slices.IndexFunc(p.FirstArgIndex, func(arg wam.FirstArg) bool {
					return arg.FirstArgKey == key
				}); i >= 0 {
					e.jumpTo(p.FirstArgIndex[i].Offset)
					continue
				}
				e.Next()
			case wam.OpPutCut: // put_cut
				e.closeStackTo(e.cutB)
				e.Next()
			case wam.OpGetCut: // get_cut
				t := e.tempVars[1]
				t = e.Deref(t)
				n, _ := e.Integer(t)
				e.closeStackTo(int(n))
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
				a := Activation{
					exec: e,
				}
				switch p := b.Proc.Call(ctx, &a); {
				case p.err != nil:
					a.Close()
					_ = yield(p.err)
					return
				case p.delayed != nil:
					if err := e.pushSeqStackFrame(p.delayed, b.PI.Arity(), &a); err != nil {
						_ = yield(err)
						return
					}

					ok, err := e.Backtrack() // Triggers the iterator.
					if err != nil {
						_ = yield(err)
						return
					}
					if !ok {
						return
					}
				case !p.ok:
					a.Close()
					ok, err := e.Backtrack()
					if err != nil {
						_ = yield(err)
						return
					}
					if !ok {
						return
					}
				case p.ok:
					a.Close()
				}
			}
		}
		_ = yield(errors.New("invalid end of code"))
		return
	}
}

// enter jumps to a predicate and loads its arguments into X1..Xn, which is what
// a continuation term amounts to once a built-in has decided to run it. Filling
// the registers and setting liveRegs is one operation because GC roots exactly
// X1..X_liveRegs: leave liveRegs behind and a register the caller just wrote
// goes uncollected and unrelocated, leave it ahead and GC follows a register
// belonging to an abandoned branch.
func (e *Execution) enter(offset int, args iter.Seq[term.Cell]) {
	e.programPointer = offset
	var n int
	for arg := range args {
		n++
		e.tempVars[n] = arg
	}
	e.liveRegs = n
}

func (e *Execution) Next() {
	e.programPointer++
}

func (e *Execution) jumpTo(addr int) {
	e.programPointer = addr
}

func (e *Execution) Backtrack() (bool, error) {
	if len(e.stack) == 0 {
		return false, nil
	}
	f := e.stack[len(e.stack)-1]
	e.cutB = f.cutB
	e.programPointer = f.programPointer
	if f.next != nil {
		for {
			if err := e.restoreState(); err != nil {
				return false, err
			}
			e.stack = e.stack[:len(e.stack)+1]
			switch p, ok := f.next(); {
			case !ok:
				f.Close()
				if len(e.stack) == 0 {
					return false, nil
				}
				e.stack = e.stack[:len(e.stack)-1]
				return e.Backtrack()
			case p.err != nil:
				return false, p.err
			case p.delayed != nil:
				if err := e.pushSeqStackFrame(p.delayed, 0, f.activation); err != nil {
					return false, err
				}
				fallthrough // Triggers the iterator.
			case p.ok:
				return true, nil
			}
		}
	}
	return true, nil
}

// closeStackTo discards the choice points above b, closing each frame on the
// way out so a discarded nondeterministic builtin stops its iterator and its
// activation is marked closed instead of outliving the frame that rooted it.
func (e *Execution) closeStackTo(b int) {
	if b >= len(e.stack) {
		return
	}
	// A barrier below the bottom of the stack means the engine handed us a
	// corrupt cut pointer. Close everything rather than walk off the end: this
	// also runs from a defer while the execution is being torn down, where a
	// panic would bury whatever error got us there.
	b = max(b, 0)
	for i := len(e.stack) - 1; i >= b; i-- {
		e.stack[i].Close()
	}
	e.stack = e.stack[:b]
}

func (e *Execution) restoreState() error {
	var f stackFrame
	f, e.stack = e.stack[len(e.stack)-1], e.stack[:len(e.stack)-1]
	if err := e.unwindTrail(f.trailTop); err != nil {
		return err
	}
	tvs := slices.Collect(e.Args(f.tempVars))
	copy(e.tempVars[1:len(tvs)+1], tvs)
	e.liveRegs = len(tvs)
	e.Heap = e.Heap[:f.heapTop]
	e.cutB = f.cutB
	// S names a cell that the truncation above may have just discarded. The
	// clause we resume in re-establishes it with get_structure before any
	// unify_*, so dropping it here costs nothing and keeps GC from following a
	// dangling address.
	e.structurePointer = structurePointer{}
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

func (e *Execution) Unify(x, y term.Cell) (bool, error) {
	var (
		stack   = []term.Cell{x, y}
		visited = map[[2]term.Cell]struct{}{}
	)
	for len(stack) > 1 {
		x, y, stack = stack[len(stack)-2], stack[len(stack)-1], stack[:len(stack)-2]
		x, y = e.Deref(x), e.Deref(y)
		if _, ok := e.Variable(x); ok {
			e.trail = append(e.trail, x)
			if err := e.Bind(x, y); err != nil {
				return false, err
			}
			continue
		}
		if _, ok := e.Variable(y); ok {
			e.trail = append(e.trail, y)
			if err := e.Bind(y, x); err != nil {
				return false, err
			}
			continue
		}
		if fx, ok := e.Functor(x); ok {
			if fy, ok := e.Functor(y); ok && fx == fy {
				if _, ok := visited[[2]term.Cell{x, y}]; ok {
					continue
				}
				visited[[2]term.Cell{x, y}] = struct{}{}

				for i := fx.Arity() - 1; i >= 0; i-- {
					stack = append(stack, e.Arg(x, i), e.Arg(y, i))
				}
				continue
			}
			return false, nil
		}
		if e.Compare(x, y) != 0 {
			return false, nil
		}
	}
	return true, nil
}

func (e *Execution) pushSeqStackFrame(seq iter.Seq[Promise], arity int, activation *Activation) error {
	next, stop := iter.Pull(seq)
	tvs, err := e.PutCompound(term.NewAtom("$temp_vars"), e.tempVars[1:arity+1]...)
	if err != nil {
		return err
	}
	f := stackFrame{
		programPointer: e.programPointer,
		heapTop:        len(e.Heap),
		trailTop:       len(e.trail),
		tempVars:       tvs,
		cutB:           e.cutB,
		next:           next,
		stop:           stop,
		activation:     activation,
	}
	e.stack = append(e.stack, f)
	return nil
}

// pin keeps cells that live in a Go frame rather than in the engine alive, and
// up to date, across a call that can collect — in practice a call back into the
// engine. GC rewrites them in place, so pass pointers to the variables the frame
// goes on to read. Call the returned function once the frame is done with them.
func (e *Execution) pin(cells ...*term.Cell) (unpin func()) {
	return e.AddRoots(slices.Values(cells))
}

func (e *Execution) setNextGCThreshold() {
	e.gcThreshold = min(max(len(e.Heap)*2, cap(e.Heap)/2), cap(e.Heap))
}

// roots enumerates the cells this execution holds: the structure pointer, the
// argument registers that are live right now, the trail, and every choice
// point's saved registers along with the terms a built-in suspended on it still
// holds. Engine.roots adds the ones that belong to the engine rather than to a
// single execution.
func (e *Execution) roots() iter.Seq[*term.Cell] {
	return func(yield func(*term.Cell) bool) {
		if !yield(&e.structurePointer.term) {
			return
		}

		for i := range e.tempVars[1 : e.liveRegs+1] {
			if !yield(&e.tempVars[1+i]) {
				return
			}
		}

		for i := range e.trail {
			if !yield(&e.trail[i]) {
				return
			}
		}

		for i := range e.stack {
			f := &e.stack[i]

			if !yield(&f.tempVars) {
				return
			}

			if f.activation == nil { // Not a built-in predicate.
				continue
			}
			for _, p := range f.activation.captured {
				c := p.Value()
				if c == nil { // Go GC collected it.
					continue
				}
				if !yield(c) {
					return
				}
			}
		}
	}
}

// heapTops enumerates the saved heap tops this execution will cut the heap back
// to. GC rewrites each one, since the boundary a choice point saved moves down
// along with the survivors below it.
func (e *Execution) heapTops() iter.Seq[*int] {
	return func(yield func(*int) bool) {
		if !yield(&e.heapBacktrackPoint) {
			return
		}
		for i := range e.stack {
			if !yield(&e.stack[i].heapTop) {
				return
			}
		}
	}
}
