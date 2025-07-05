package prolog

import (
	"context"
	"errors"
	"fmt"
)

type opCode uint8

const (
	opNop opCode = iota
	opPutStructure
	opSetVariable
	opSetValue
	opGetStructure
	opUnifyVariable
	opUnifyValue
	opExecute
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

type labeledInstruction struct {
	instruction
	label string // Label for this instruction.
	l     string // Label argument that should be resolved.
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

func (c CompiledProcedure) Call(ctx context.Context, proc *Processor, args []Term, cont Promise) Promise {
	if len(args) != c.arity {
		return Eager(false, ErrInvalidArguments)
	}
	m := vm{
		Module: c.module,
		p:      c.entryPoint,
		heap:   &proc.Heap,
	}
	m.a = m.x[:len(args)]
	copy(m.a, args)
	return m.run(ctx, proc, cont)
}

type stackFrame struct {
	p  int    // next clause address
	h  int    // saved top of the heap
	tr int    // saved top of the trail
	a  []Term // saved argument registers
}

type vm struct {
	*Module
	p int

	heap *Heap
	hb   int
	s    int

	trail []Variable
	tr    int

	x [256]Term
	a []Term // x[:num_of_args]

	mode mode

	stack []stackFrame
	b0    int
	cutB  int
}

func (m *vm) backtrack() bool {
	if len(m.stack) == 0 {
		return false
	}
	var f stackFrame
	f, m.stack = m.stack[len(m.stack)-1], m.stack[:len(m.stack)-1]
	m.p = f.p
	return true
}

func (m *vm) run(ctx context.Context, proc *Processor, cont Promise) Promise {
	for m.p < len(m.code) {
		switch inst := m.code[m.p]; inst.op {
		case opNop: // nop
			m.p++
		case opPutStructure: // put_structure f/n, Xi
			s := Term{tag: termTagStructure, value: int32(len(m.heap.terms) + 1)}
			f := m.constants[inst.n]
			if _, err := m.heap.put(cast[Term, word](f)); err != nil {
				return Eager(false, err)
			}
			m.x[inst.i] = s
			m.p++
		case opSetVariable: // set_variable Xi
			t, _ := m.heap.PutVariable()
			m.x[inst.i] = t
			m.p++
		case opSetValue: // set_value Xi
			if _, err := m.heap.put(cast[Term, word](m.x[inst.i])); err != nil {
				return Eager(false, err)
			}
			m.p++
		case opGetStructure: // get_structure f/n, Xi
			f := m.constants[inst.n]
			t := m.heap.Deref(m.x[inst.i])
			switch t.tag {
			case termTagReference:
				id, err := m.heap.put(cast[Term, word](f))
				if err != nil {
					return Eager(false, err)
				}
				if !m.heap.bind(&m.trail, t, Term{tag: termTagStructure, value: id}, false) {
					return Eager(false, nil)
				}
				m.mode = modeWrite
			case termTagStructure:
				if cast[word, Term](m.heap.terms[t.value]) != f {
					return Eager(false, nil)
				}
				m.s = int(t.value + 1)
				m.mode = modeRead
			default:
				return Eager(false, nil)
			}
			m.p++
		case opUnifyVariable: // unify_variable Vn
			switch m.mode {
			case modeRead:
				m.x[inst.i] = cast[word, Term](m.heap.terms[m.s])
			case modeWrite:
				t, _ := m.heap.PutVariable()
				m.x[inst.i] = t
			}
			m.s++
			m.p++
		case opUnifyValue: // unify_value Vn
			switch m.mode {
			case modeRead:
				if !m.heap.unify(&m.trail, m.constants[inst.n], cast[word, Term](m.heap.terms[m.s]), false) {
					if !m.backtrack() {
						return Eager(false, nil)
					}
				}
			case modeWrite:
				if _, err := m.heap.put(cast[Term, word](m.constants[inst.n])); err != nil {
					return Eager(false, err)
				}
			}
			m.s++
			m.p++
		case opExecute: // execute P
			f := m.constants[inst.n]
			pi, _ := m.heap.Functor(f)
			e, ok := m.procedures[pi]
			if !ok {
				switch m.unknown {
				case unknownError:
					return Eager(false, &ExistenceError{ObjectType: "procedure", Culprit: f})
				case unknownFail:
					if !m.backtrack() {
						return Eager(false, nil)
					}
				case unknownWarning:
					if err := proc.Warn(f); err != nil {
						return Eager(false, err)
					}
				}
			}
			if cp, ok := e.procedure.(CompiledProcedure); ok {
				m.Module = cp.module
				m.a = m.a[:cp.arity]
				m.b0 = len(m.stack) - 1
				m.p = cp.entryPoint
			} else {
				p := e.procedure.Call(ctx, proc, m.x[:pi.Arity], cont)
				ok, err := p.Force()
				if err != nil || ok {
					return Eager(ok, err)
				}
				if !m.backtrack() {
					return Eager(false, nil)
				}
			}
		case opProceed: // proceed
			ok, err := cont.Force()
			if err != nil || ok {
				return Eager(ok, err)
			}
		case opTryMeElse: // try_me_else L
			m.stack = append(m.stack, stackFrame{
				p:  int(inst.n),
				h:  len(m.heap.terms),
				tr: m.tr,
				a:  append(make([]Term, 0, len(m.a)), m.a...), // TODO: Can we reuse the existing slice?
			})
			m.hb = len(m.heap.terms)
			m.p++
		case opRetryMeElse: // retry_me_else L
			frame := &m.stack[len(m.stack)-1]
			m.a = m.a[:len(frame.a)]
			copy(m.a, frame.a)
			frame.p = int(inst.n)
			m.heap.UnwindTrail(m.trail[m.tr:])
			m.trail = m.trail[:m.tr]
			m.tr = frame.tr
			m.heap.terms = m.heap.terms[:frame.h]
			m.hb = len(m.heap.terms)
			m.p++
		case opTrustMe: // trust_me
			frame := &m.stack[len(m.stack)-1]
			m.a = m.a[:len(frame.a)]
			copy(m.a, frame.a)
			m.heap.UnwindTrail(m.trail[m.tr:])
			m.trail = m.trail[:m.tr]
			m.tr = frame.tr
			m.heap.terms = m.heap.terms[:frame.h]
			m.hb = frame.h
			m.p++
		case opMove: // move Xi, Xn
			m.x[inst.i] = m.x[inst.n]
			m.p++
		case opNondet: // nondet
			// TODO: Don't know what to do. No-op for now.
			m.p++
		case opSwitch: // switch
			// TODO: Implement later. No-op for now.
			m.p++
		case opPushCut: // push_cut
			if _, err := m.heap.PutInteger(int64(m.cutB)); err != nil {
				return Eager(false, err)
			}
			m.p++
		case opPutCut: // put_cut
			m.stack = m.stack[:m.cutB]
			m.p++
		case opGetCut: // get_cut Xi
			t := m.heap.Deref(m.x[inst.i])
			n, err := m.heap.Integer(t)
			if err != nil {
				return Eager(false, err)
			}
			m.stack = m.stack[:n]
			m.p++
		}
	}
	return Eager(false, errors.New("invalid end of code"))
}

type headBody struct {
	head Term
	body Term
}

func compileClauses(h *Heap, m *Module, pi Functor, clauses []headBody) (CompiledProcedure, error) {
	ret := CompiledProcedure{
		module:     m,
		entryPoint: len(m.code),
		arity:      pi.Arity,
	}
	var code []labeledInstruction
	for i, clause := range clauses {
		prefix := fmt.Sprintf("%s_%d_%d", pi.Name, pi.Arity, i)
		if len(clauses) > 1 {
			switch i {
			case 0:
				code = append(code, labeledInstruction{instruction: instruction{op: opTryMeElse}, l: fmt.Sprintf("%s_%d_1", pi.Name, pi.Arity)})
			case len(clauses) - 1:
				code = append(code, labeledInstruction{label: prefix, instruction: instruction{op: opTrustMe}})
			default:
				code = append(code, labeledInstruction{label: prefix, instruction: instruction{op: opRetryMeElse}, l: fmt.Sprintf("%s_%d_%d", pi.Name, pi.Arity, i+1)})
			}
		}
		if err := compileClause(h, &code, prefix, clause.head, clause.body); err != nil {
			return ret, err
		}
	}
	return ret, nil
}

func compileClause(h *Heap, code *[]labeledInstruction, prefix string, head, body Term) error {
	return nil // TODO:
}
