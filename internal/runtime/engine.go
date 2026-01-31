package runtime

import (
	"context"
	"errors"
	"iter"

	"github.com/ichiban/prolog/v2/internal/term"
	"github.com/ichiban/prolog/v2/internal/wam"
)

type Engine struct {
	image *wam.Image

	programPointer int // P

	stack []stackFrame // A

	trail []term.Handle // TR
	heap  term.Heap     // H

	heapBacktrackPoint int // HB
	structurePointer   int // S

	tempVars  [256]term.Handle // Xn
	numOfArgs int              // An = tempVars[:numOfArgs]

	mode wam.Mode
}

func (e *Engine) Run(ctx context.Context) iter.Seq[error] {
	return func(yield func(error) bool) {
		for e.programPointer < len(e.module.code) {
			switch inst := e.module.code[e.programPointer]; inst.op {
			case opNop: // nop
				e.programPointer++
			case opPutVariable:

			case opPutStructure: // put_structure f/n, Xi
				s := Term{tag: termTagStructure, value: int32(len(e.heap) + 1)}
				f := e.module.constants[inst.n]
				if _, err := e.put(cast[Term, word](f)); err != nil {
					_ = yield(err)
					return
				}
				e.tempVars[inst.i] = s
				e.programPointer++
			case opGetStructure: // get_structure f/n, Xi
				f := e.module.constants[inst.n]
				t := e.Deref(e.tempVars[inst.i])
				switch t.tag {
				case termTagReference:
					id, err := e.put(cast[Term, word](f))
					if err != nil {
						_ = yield(err)
						return
					}
					ok, err := e.bind(t, Term{tag: termTagStructure, value: id}, false)
					if err != nil {
						_ = yield(err)
						return
					}
					if !ok {
						return
					}
					e.mode = modeWrite
				case termTagStructure:
					e.structurePointer = int(t.value + 1)
					e.mode = modeRead
					fallthrough
				default: // Atomic term.
					if cast[word, Term](e.heap[t.value]) != f {
						return
					}
				}
				e.programPointer++
			case opUnifyVariable: // unify_variable Xi
				if e.mode == modeRead {
					e.tempVars[inst.i] = cast[word, Term](e.heap[e.structurePointer])
					e.structurePointer++
					e.programPointer++
					break
				}
				fallthrough
			case opWriteVariable: // write_variable Xi
				t, _ := e.PutVariable()
				e.tempVars[inst.i] = t
				e.programPointer++
			case opUnifyValue: // unify_value Xi
				if e.mode == modeRead {
					ok, err := e.Unify(e.tempVars[inst.i], cast[word, Term](e.heap[e.structurePointer]))
					if err != nil {
						_ = yield(err)
						return
					}
					if !ok {
						if err := e.backtrack(); err != nil {
							_ = yield(err)
							return
						}
					}
					e.structurePointer++
					e.programPointer++
					break
				}
				fallthrough
			case opWriteValue: // write_value Xi
				if _, err := e.put(cast[Term, word](e.tempVars[inst.i])); err != nil {
					_ = yield(err)
					return
				}
				e.programPointer++
			case opExecute: // execute P
				e.b0 = len(e.stack) - 1
				e.programPointer = int(inst.n)
			case opBuiltin: // builtin ID
				e.execBuiltin(ctx, int(inst.n))
			case opProceed: // proceed
				if !yield(nil) {
					return
				}
				if err := e.backtrack(); err != nil {
					_ = yield(err)
					return
				}
			case opTryMeElse: // try_me_else L
				f := stackFrame{
					programPointer: int(inst.n),
					heapTop:        len(e.heap),
					trailTop:       e.TrailTop(),
					numOfArgs:      e.numOfArgs,
				}
				copy(f.args(), e.args())
				e.stack = append(e.stack, f)
				e.heapBacktrackPoint = len(e.heap)
				e.programPointer++
			case opRetryMeElse: // retry_me_else L
				frame := &e.stack[len(e.stack)-1]
				e.numOfArgs = frame.numOfArgs
				copy(e.args(), frame.args())
				frame.programPointer = int(inst.n)
				e.Unwind(frame.trailTop)
				e.heap = e.heap[:frame.heapTop]
				e.heapBacktrackPoint = len(e.heap)
				e.programPointer++
			case opTrustMe: // trust_me
				frame := &e.stack[len(e.stack)-1]
				e.numOfArgs = frame.numOfArgs
				copy(e.args(), frame.args())
				e.Unwind(frame.trailTop)
				e.heap = e.heap[:frame.heapTop]
				e.heapBacktrackPoint = frame.heapTop
				e.programPointer++
			case opMove: // move Xi<-Xn
				e.tempVars[inst.i] = e.tempVars[inst.n]
				e.programPointer++
			case opNondet: // nondet
				// TODO: Don't know what to do. No-op for now.
				e.programPointer++
			case opSwitch: // switch
				// TODO: Implement later.
				e.programPointer++
			case opPushCut: // push_cut
				if _, err := e.PutInteger(int64(e.cutB)); err != nil {
					_ = yield(err)
					return
				}
				e.programPointer++
			case opPutCut: // put_cut
				e.stack = e.stack[:e.cutB]
				e.programPointer++
			case opGetCut: // get_cut Xi
				t := e.Deref(e.tempVars[inst.i])
				n, err := e.Integer(t)
				if err != nil {
					_ = yield(err)
					return
				}
				e.stack = e.stack[:n]
				e.programPointer++
			}
		}
		_ = yield(errors.New("invalid end of code"))
		return
	}
}

type stackFrame struct {
	programPointer int       // P, next clause address
	heapTop        int       // H, saved top of the heap
	trailTop       TrailTop  // TR, saved top of the trail
	tempVars       [256]Term // The backing array to save An
	numOfArgs      int

	next func() (error, bool) // for built-in predicates
	stop func()               // for built-in predicates
}

func (f *stackFrame) args() []Term {
	return f.tempVars[:f.numOfArgs:f.numOfArgs]
}
