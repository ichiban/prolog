package runtime

import (
	"context"
	"iter"

	"github.com/ichiban/prolog/v2/internal/syntax"
	"github.com/ichiban/prolog/v2/internal/term"
	"github.com/ichiban/prolog/v2/internal/wam"
)

type Engine struct {
	*term.Arena
	Image        *wam.Image
	Ops          *syntax.OperatorSet
	DoubleQuotes syntax.DoubleQuotes
	Module       term.Atom
	BuiltinIndex map[term.Functor]int
	Builtins     []Builtin
}

func (e *Engine) ExpandTerm(_ context.Context, t term.Handle) iter.Seq2[term.Handle, error] {
	// TODO: Implement this!
	return func(yield func(term.Handle, error) bool) {
		_ = yield(t, nil)
	}
}

func (e *Engine) ExpandGoal(_ context.Context, t term.Handle) (term.Handle, error) {
	return t, nil // TODO: Implement this!
}

/*
	func (e *Engine) Run(ctx context.Context) iter.Seq[error] {
		return func(yield func(error) bool) {
			for e.programPointer < len(e.image.Code) {
				switch inst := e.image.Code[e.programPointer]; inst.OpCode {
				case wam.OpNop: // nop
					e.programPointer++
				case wam.OpPutVariable:

				case wam.OpPutStructure: // put_structure f/n, Xi
					f := e.image.Functors[inst.N]
					s, err := e.heap.putFunctor(f)
					if err != nil {
						_ = yield(err)
						return
					}
					e.tempVars[inst.I] = s
					e.programPointer++
				case wam.OpGetStructure: // get_structure f/n, Xi
					f := e.image.Functors[inst.N]
					t := e.tempVars[inst.I].Deref()
					if _, ok := t.Variable(); ok {
						s, err := e.heap.putFunctor(f)
						if err != nil {
							_ = yield(err)
							return
						}
						if err := t.Bind(s); err != nil {
							_ = yield(err)
							return
						}
						e.mode = wam.ModeWrite
					}
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
						e.mode = wam.ModeWrite
					case termTagStructure:
						e.structurePointer = int(t.value + 1)
						e.mode = wam.ModeRead
						fallthrough
					default: // Atomic term.
						if cast[word, Term](e.heap[t.value]) != f {
							return
						}
					}
					e.programPointer++
				case wam.OpUnifyVariable: // unify_variable Xi
					if e.mode == wam.ModeRead {
						e.tempVars[inst.i] = cast[word, Term](e.heap[e.structurePointer])
						e.structurePointer++
						e.programPointer++
						break
					}
					fallthrough
				case wam.OpWriteVariable: // write_variable Xi
					t, _ := e.PutVariable()
					e.tempVars[inst.i] = t
					e.programPointer++
				case wam.OpUnifyValue: // unify_value Xi
					if e.mode == wam.ModeRead {
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
				case wam.OpWriteValue: // write_value Xi
					if _, err := e.put(cast[Term, word](e.tempVars[inst.i])); err != nil {
						_ = yield(err)
						return
					}
					e.programPointer++
				case wam.OpExecute: // execute P
					e.b0 = len(e.stack) - 1
					e.programPointer = int(inst.n)
				case wam.OpBuiltin: // builtin ID
					e.execBuiltin(ctx, int(inst.n))
				case wam.OpProceed: // proceed
					if !yield(nil) {
						return
					}
					if err := e.backtrack(); err != nil {
						_ = yield(err)
						return
					}
				case wam.OpTryMeElse: // try_me_else L
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
				case wam.OpRetryMeElse: // retry_me_else L
					frame := &e.stack[len(e.stack)-1]
					e.numOfArgs = frame.numOfArgs
					copy(e.args(), frame.args())
					frame.programPointer = int(inst.n)
					e.Unwind(frame.trailTop)
					e.heap = e.heap[:frame.heapTop]
					e.heapBacktrackPoint = len(e.heap)
					e.programPointer++
				case wam.OpTrustMe: // trust_me
					frame := &e.stack[len(e.stack)-1]
					e.numOfArgs = frame.numOfArgs
					copy(e.args(), frame.args())
					e.Unwind(frame.trailTop)
					e.heap = e.heap[:frame.heapTop]
					e.heapBacktrackPoint = frame.heapTop
					e.programPointer++
				case wam.OpMove: // move Xi<-Xn
					e.tempVars[inst.i] = e.tempVars[inst.n]
					e.programPointer++
				case wam.OpNondet: // nondet
					// TODO: Don't know what to do. No-op for now.
					e.programPointer++
				case wam.OpSwitch: // switch
					// TODO: Implement later.
					e.programPointer++
				case wam.OpPushCut: // push_cut
					if _, err := e.PutInteger(int64(e.cutB)); err != nil {
						_ = yield(err)
						return
					}
					e.programPointer++
				case wam.OpPutCut: // put_cut
					e.stack = e.stack[:e.cutB]
					e.programPointer++
				case wam.OpGetCut: // get_cut Xi
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
*/

type State struct {
	programPointer int // P

	stack []stackFrame // A

	trail []term.Handle // TR
	heap  *term.Heap    // H

	heapBacktrackPoint int // HB
	structurePointer   int // S

	tempVars  [256]term.Handle // Xn
	numOfArgs int              // An = tempVars[:numOfArgs]

	mode wam.Mode
}

type stackFrame struct {
	programPointer int              // P, next clause address
	heapTop        int              // H, saved top of the heap
	trailTop       int              // TR, saved top of the trail
	tempVars       [256]term.Handle // The backing array to save An
	numOfArgs      int

	next func() (error, bool) // for built-in predicates
	stop func()               // for built-in predicates
}

func (f *stackFrame) args() []term.Handle {
	return f.tempVars[:f.numOfArgs:f.numOfArgs]
}
