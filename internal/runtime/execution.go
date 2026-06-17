package runtime

import (
	"context"
	"errors"
	"iter"

	"github.com/ichiban/prolog/v2/internal/term"
	"github.com/ichiban/prolog/v2/internal/wam"
)

const (
	maxRegisters = 256
)

type stackFrame struct {
	programPointer int                       // P, next clause address
	heapTop        int                       // H, saved top of the heap
	trailTop       int                       // TR, saved top of the trail
	tempVars       [maxRegisters]term.Handle // The backing array to save An
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
		for e.programPointer < len(e.Image.Code) {
			switch inst := e.Image.Code[e.programPointer]; inst.Op {
			case wam.OpNop: // nop
				e.Next()
			case wam.OpPutVariable: // put_variable Xn, Ai
				v, err := e.PutVariable()
				if err != nil {
					_ = yield(err)
					return
				}
				e.tempVars[inst.N] = v
				e.tempVars[inst.I] = v
				e.Next()
			case wam.OpPutConstant: // put_constant c, Xi
				k := e.Constants[inst.N]
				e.tempVars[inst.I] = k
				e.Next()
			case wam.OpPutStructure: // put_structure f/n, Xi
				f := e.Engine.Image.Functors[inst.N]
				s, err := e.PutStructure(f)
				if err != nil {
					_ = yield(err)
					return
				}
				e.tempVars[inst.I] = s
				e.Next()
			case wam.OpWriteConstant: // write_constant c
				k := e.Constants[inst.N]
				if _, err := e.Put(k); err != nil {
					_ = yield(err)
					return
				}
				e.Next()
			case wam.OpGetValue: // get_value Xn, Ai
				t := e.tempVars[inst.N]
				s := e.tempVars[inst.I]
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
				k := e.Constants[inst.N]
				t := e.tempVars[inst.I]
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
				f := e.Image.Functors[inst.N]
				t := e.Deref(e.tempVars[inst.I])
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
					e.tempVars[inst.I] = e.Arg(s.term, s.argNo)
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
				e.tempVars[inst.I] = t
				e.Next()
			case wam.OpUnifyValue: // unify_value Xi
				if e.mode == wam.ModeRead {
					s := e.structurePointer
					ok, err := e.Unify(e.tempVars[inst.I], e.Arg(s.term, s.argNo))
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
				if _, err := e.Put(e.tempVars[inst.I]); err != nil {
					_ = yield(err)
					return
				}
				e.Next()
			case wam.OpExecute: // execute P
				pi := e.Functors[inst.N]
				p, ok := e.Predicates[pi]
				if !ok {
					culprit, err := e.PutFunctor(pi)
					if err != nil {
						_ = yield(err)
						return
					}
					_ = yield(&ExistenceError{
						Arena:      e.Arena,
						ObjectType: "procedure",
						Culprit:    culprit,
					})
					return
				}
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
					programPointer: int(inst.N),
					heapTop:        len(e.Heap),
					trailTop:       len(e.trail),
				}
				copy(f.tempVars[:arity], e.tempVars[:arity])
				e.stack = append(e.stack, f)
				e.heapBacktrackPoint = len(e.Heap)
				e.Next()
			case wam.OpRetryMeElse: // retry_me_else L
				arity := int(inst.I)
				e.stack[len(e.stack)-1].programPointer = int(inst.N)
				e.RestoreState(arity)
				e.stack = e.stack[:len(e.stack)+1]
				e.Next()
			case wam.OpTrustMe: // trust_me
				arity := int(inst.I)
				e.RestoreState(arity)
				e.Next()
			case wam.OpMove: // move Xi<-Xn
				e.tempVars[inst.I] = e.tempVars[inst.N]
				e.Next()
			case wam.OpNondet: // nondet
				// TODO: Don't know what to do. No-op for now.
				e.Next()
			case wam.OpSwitch: // switch
				// TODO: Implement later.
				e.Next()
			case wam.OpPutCut: // put_cut
				e.stack = e.stack[:e.cutB]
				e.Next()
			case wam.OpGetCut: // get_cut Xi
				t := e.Deref(e.tempVars[inst.I])
				n, _ := e.Integer(t)
				e.stack = e.stack[:n]
				e.Next()
			case wam.OpPushCut: // push_cut
				if _, err := e.PutInteger(int64(e.cutB)); err != nil {
					_ = yield(err)
					return
				}
				e.Next()
			default: // Builtins
				b := e.BuiltinSet.Get(int(inst.Op - wam.OpBuiltin0))
				if err := b.Proc(ctx, e); err != nil {
					_ = yield(err)
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

func (e *Execution) Backtrack() bool {
	if len(e.stack) == 0 {
		return false
	}
	e.programPointer = e.stack[len(e.stack)-1].programPointer
	return true
}

func (e *Execution) RestoreState(arity int) {
	var f stackFrame
	f, e.stack = e.stack[len(e.stack)-1], e.stack[:len(e.stack)-1]
	e.Unwind(f.trailTop)
	e.Heap = e.Heap[:f.heapTop]
	copy(e.tempVars[:arity], f.tempVars[:arity])
	e.cutB = f.cutB
}

func (e *Execution) Unwind(trailTop int) {
	// TODO: Implement this!
}
