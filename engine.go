package prolog

import (
	"context"
	"errors"
	"io/fs"
	"iter"
)

type opCode uint8

const (
	opNop opCode = iota
	opPutVariable
	opPutStructure
	opGetStructure
	opUnifyVariable
	opWriteVariable
	opUnifyValue
	opWriteValue
	opExecute
	opBuiltin
	opProceed
	opTryMeElse
	opRetryMeElse
	opTrustMe
	opMove
	opNondet
	opSwitch
	opPushCut
	opPutCut
	opGetCut
)

type instruction struct {
	op opCode
	i  uint8  // Operand for Xi, Ai
	n  uint16 // Operand for f/n, L
}

type mode uint8

const (
	modeRead mode = iota
	modeWrite
)

type CompiledProcedure struct {
	module     *Module
	entryPoint int
	arity      int
}

func (c CompiledProcedure) Call(ctx context.Context, e *Engine, args []Term) iter.Seq[Success] {
	if len(args) != c.arity {
		e.err = ErrInvalidArguments
		return nil
	}
	e.module = c.module
	e.programPointer = c.entryPoint
	e.numOfArgs = len(args)
	copy(e.args(), args)
	return e.run(ctx)
}

type stackFrame struct {
	programPointer int       // P, next clause address
	heap           int       // H, saved top of the heap
	trail          int       // TR, saved top of the trail
	tempVars       [256]Term // The backing array to save An
	numOfArgs      int

	next func() (Success, bool) // for built-in predicates
	stop func()                 // for built-in predicates
}

func (f *stackFrame) args() []Term {
	return f.tempVars[:f.numOfArgs:f.numOfArgs]
}

type EngineOptions struct {
	heapSize      int
	stackSize     int
	trailSize     int
	atomTableSize int
}

type EngineOption func(*EngineOptions)

func WithHeapSize(n int) EngineOption {
	return func(o *EngineOptions) {
		o.heapSize = n
	}
}

func WithStackSize(n int) EngineOption {
	return func(o *EngineOptions) {
		o.stackSize = n
	}
}

func WithAtomTableSize(n int) EngineOption {
	return func(o *EngineOptions) {
		o.atomTableSize = n
	}
}

func WithTrailSize(n int) EngineOption {
	return func(o *EngineOptions) {
		o.trailSize = n
	}
}

// Engine holds information enough to execute a query or compile a text.
type Engine struct {
	fs   map[Atom]fs.FS
	warn func(Term) error // TODO: Not sure what warning should look like.

	modules map[Atom]*Module
	module  *Module

	programPointer int // P

	stack []stackFrame // A

	trail []Variable // TR
	heap  []word     // H

	heapBacktrackPoint int // HB
	structurePointer   int // S

	tempVars  [256]Term // Xn
	numOfArgs int       // An = tempVars[:numOfArgs]

	mode mode

	atoms AtomTable

	b0   int
	cutB int

	// For catch and throw. i.e. ball.
	err error
}

func NewEngine(opts ...EngineOption) *Engine {
	// TODO: Set sensible default values.
	options := EngineOptions{
		heapSize:      1024,
		stackSize:     1024,
		trailSize:     1024,
		atomTableSize: 1024,
	}
	for _, opt := range opts {
		opt(&options)
	}
	var e Engine
	e.stack = make([]stackFrame, 0, options.stackSize)
	e.heap = make([]word, 0, options.heapSize)
	e.trail = make([]Variable, 0, options.trailSize)
	e.atoms.ids = make(map[Atom]int32, options.atomTableSize)
	e.atoms.entries = make([]atomTableEntry, 0, options.atomTableSize)
	return &e
}

func (e *Engine) args() []Term {
	return e.tempVars[:e.numOfArgs:e.numOfArgs]
}

func (e *Engine) SetFS(name Atom, fileSystem fs.FS) {
	if e.fs == nil {
		e.fs = make(map[Atom]fs.FS)
	}
	e.fs[name] = fileSystem
}

func (e *Engine) SetWarn(warn func(Term) error) {
	e.warn = warn
}

func (e *Engine) SetModule(module Atom) {
	e.module = e.modules[module]
}

func (e *Engine) backtrack() bool {
	for len(e.stack) > 0 {
		var f stackFrame
		f, e.stack = e.stack[len(e.stack)-1], e.stack[:len(e.stack)-1]
		if f.next != nil {
			s, ok := f.next()
			if !ok {
				continue
			}
			if !s.Last {
				e.stack = append(e.stack, f)
			}
			return true
		}
		e.programPointer = f.programPointer
		return true
	}
	return false
}

func (e *Engine) dispatch(ctx context.Context, t Term) {
	m, t := e.Unqualify(t, e.module.name)
	f, err := e.Functor(t)
	if err != nil {
		e.err = err
		return
	}

	entry, ok := e.modules[m].procedures[f]
	if !ok {
		e.err = &ExistenceError{
			ObjectType: "procedure",
			Culprit:    t,
		}
		return
	}
	for i := 0; i < f.Arity; i++ {
		e.tempVars[i] = e.Arg(t, i)
	}
	if entry.builtIn {
		e.execBuiltin(ctx, entry.offset)
		return
	}
	e.programPointer = entry.offset
}

func (e *Engine) execBuiltin(ctx context.Context, id int) {
	b := e.module.builtins[id]
	next, stop := iter.Pull(b(ctx, e))
	s, ok := next()
	if !ok {
		e.backtrack()
		return
	}
	if s.Last {
		stop()
		return
	}
	e.stack = append(e.stack, stackFrame{
		next: next,
		stop: stop,
	})
}

func (e *Engine) run(ctx context.Context) iter.Seq[Success] {
	return func(yield func(_ Success) bool) {
		for e.programPointer < len(e.module.code) {
			switch inst := e.module.code[e.programPointer]; inst.op {
			case opNop: // nop
				e.programPointer++
			case opPutVariable:

			case opPutStructure: // put_structure f/n, Xi
				s := Term{tag: termTagStructure, value: int32(len(e.heap) + 1)}
				f := e.module.constants[inst.n]
				if _, err := e.put(cast[Term, word](f)); err != nil {
					e.err = err
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
						e.err = err
						return
					}
					if !e.bind(&e.trail, t, Term{tag: termTagStructure, value: id}, false) {
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
					if !e.Unify(&e.trail, e.tempVars[inst.i], cast[word, Term](e.heap[e.structurePointer])) {
						if !e.backtrack() {
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
					e.err = err
					return
				}
				e.programPointer++
			case opExecute: // execute P
				e.b0 = len(e.stack) - 1
				e.programPointer = int(inst.n)
			case opBuiltin: // builtin ID
				e.execBuiltin(ctx, int(inst.n))
			case opProceed: // proceed
				if !yield(Success{}) {
					return
				}
				if !e.backtrack() {
					return
				}
			case opTryMeElse: // try_me_else L
				f := stackFrame{
					programPointer: int(inst.n),
					heap:           len(e.heap),
					trail:          len(e.trail),
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
				e.UnwindTrail(e.trail)
				e.trail = e.trail[:frame.trail]
				e.heap = e.heap[:frame.heap]
				e.heapBacktrackPoint = len(e.heap)
				e.programPointer++
			case opTrustMe: // trust_me
				frame := &e.stack[len(e.stack)-1]
				e.numOfArgs = frame.numOfArgs
				copy(e.args(), frame.args())
				e.UnwindTrail(e.trail)
				e.trail = e.trail[:frame.trail]
				e.heap = e.heap[:frame.heap]
				e.heapBacktrackPoint = frame.heap
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
					e.err = err
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
					e.err = err
					return
				}
				e.stack = e.stack[:n]
				e.programPointer++
			}
		}
		e.err = errors.New("invalid end of code")
		return
	}
}
