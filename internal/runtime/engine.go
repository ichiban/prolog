package runtime

import (
	"context"
	"errors"
	"iter"
	"slices"

	"github.com/ichiban/prolog/v2/internal/ir"
	"github.com/ichiban/prolog/v2/internal/syntax"
	"github.com/ichiban/prolog/v2/internal/term"
	"github.com/ichiban/prolog/v2/internal/wam"
)

type Engine struct {
	*term.Arena
	wam.Image
	BuiltinSet

	Module       term.Atom
	DoubleQuotes syntax.DoubleQuotes
	Ops          *syntax.OperatorSet

	OnDiscontiguous func(pi term.Functor) error
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

func (e *Engine) LoadModule(module *ir.Module) error {
	if e.OnDiscontiguous == nil {
		e.OnDiscontiguous = func(pi term.Functor) error {
			return nil
		}
	}

	if len(e.Code) == 0 {
		e.Predicates = map[term.Functor]wam.Predicate{
			term.NewFunctor(term.NewAtom("true"), 0): {Offset: 0},
		}
		e.Code = append(e.Code, []wam.Instruction{
			{Op: wam.OpProceed},
		}...)
	}

	var (
		current term.Functor
		last    int
	)
	for i, clause := range module.Clauses {
		pi := clause.PI
		switch _, ok := e.Predicates[pi]; {
		case !ok: // The 1st clause.
			if i > 0 {
				// The last predicate needs to be closed.
				if err := e.closePredicate(last); err != nil {
					return err
				}
			}

			current = pi
			e.Predicates[pi] = wam.Predicate{
				Offset: len(e.Code),
			}

			// First argument index.
			fa := clause.FirstArg
			key := wam.FirstArgKey{
				PI:    pi,
				Term:  fa.Term,
				Arity: fa.Arity,
			}
			if _, ok := e.FirstArgIndex[key]; ok || fa == (ir.Index{}) {
				e.Code = append(e.Code, wam.Instruction{
					Op: wam.OpNondet,
				})
			} else {
				e.FirstArgIndex[key] = len(e.Code)
				e.Code = append(e.Code, wam.Instruction{
					Op: wam.OpSwitch,
					N:  uint16(pi),
				})
			}

			e.Code = append(e.Code, wam.Instruction{
				Op: wam.OpTryMeElse,
				I:  uint8(pi.Arity()),
			})
		case pi != current: // A discontiguous clause.
			if err := e.OnDiscontiguous(pi); err != nil {
				return err
			}
			current = pi
			// TODO: Overwrite the previous chunk's `execute P` to an unconditional jump to this chunk.
			fallthrough
		default:
			e.Code[last].N = uint16(len(e.Code))
			e.Code = append(e.Code, wam.Instruction{
				Op: wam.OpRetryMeElse,
				I:  uint8(pi.Arity()),
			})
		}
		last = len(e.Code) - 1

		if clause.MaxRegs >= maxRegisters {
			return errors.New("not enough registers")
		}

		for _, inst := range clause.Code {
			switch op := convertOp(inst); op {
			case wam.OpLoadVariable, wam.OpPutVariable, wam.OpGetValue, wam.OpLoadValue:
				e.Code = append(e.Code, wam.Instruction{
					Op: op,
					I:  uint8(inst.A.Index),
					N:  uint16(inst.B.Index),
				})
			case wam.OpGetStructure, wam.OpPutStructure, wam.OpPushStructure:
				fid := len(e.Functors)
				e.Functors = append(e.Functors, term.Functor(inst.B.Index))
				e.Code = append(e.Code, wam.Instruction{
					Op: op,
					I:  uint8(inst.A.Index),
					N:  uint16(fid),
				})
			case wam.OpUnifyVariable, wam.OpUnifyValue, wam.OpWriteVariable, wam.OpWriteValue:
				e.Code = append(e.Code, wam.Instruction{
					Op: op,
					I:  uint8(inst.A.Index),
				})
			case wam.OpLoadConstant, wam.OpGetConstant, wam.OpPutConstant, wam.OpUnifyConstant, wam.OpWriteConstant:
				cid := len(e.Constants)
				e.Constants = append(e.Constants, inst.B.Term)
				e.Code = append(e.Code, wam.Instruction{
					Op: op,
					I:  uint8(inst.A.Index),
					N:  uint16(cid),
				})
			case wam.OpGetVariable:
				e.Code = append(e.Code, wam.Instruction{
					Op: wam.OpMove,
					I:  uint8(inst.A.Index),
					N:  uint16(inst.B.Index),
				})
			case wam.OpPutValue:
				e.Code = append(e.Code, wam.Instruction{
					Op: wam.OpMove,
					I:  uint8(inst.B.Index),
					N:  uint16(inst.A.Index),
				})
			default:
				e.Code = append(e.Code, wam.Instruction{
					Op: op,
					N:  uint16(inst.A.Index),
				})
			}
		}

		fid := len(e.Functors)
		e.Functors = append(e.Functors, clause.Execute)
		e.Code = append(e.Code, wam.Instruction{
			Op: wam.OpExecute,
			N:  uint16(fid),
		})
	}

	return e.closePredicate(last)
}

func (e *Engine) closePredicate(last int) error {
	switch e.Code[last].Op {
	case wam.OpTryMeElse: // The last predicate has only one clause.
		// TODO: What would happen if it's discontiguous?
		e.Code = slices.Delete(e.Code, last-1, last+1)
		/*
			copy(e.Code[last-1:last+1], []wam.Instruction{
				{Op: wam.OpNop}, // Replacing wam.OpSwitch/wam.OpNondet
				{Op: wam.OpNop}, // Replacing wam.OpTryMeElse
			})
		*/
	case wam.OpRetryMeElse:
		e.Code[last] = wam.Instruction{Op: wam.OpTrustMe}
	default:
		return errors.New("invalid instruction")
	}
	return nil
}

func convertOp(inst ir.Instruction) wam.OpCode {
	type key struct {
		op  ir.OpCode
		typ ir.Type
	}
	switch (key{op: inst.OpCode, typ: inst.Type}) {
	case key{op: ir.OpPut, typ: ir.TypeVariable}:
		return wam.OpPutVariable
	case key{op: ir.OpPut, typ: ir.TypeValue}:
		return wam.OpPutValue
	case key{op: ir.OpPut, typ: ir.TypeStructure}:
		return wam.OpPutStructure
	case key{op: ir.OpPut, typ: ir.TypeConstant}:
		return wam.OpPutConstant
	case key{op: ir.OpPut, typ: ir.TypeCut}:
		return wam.OpPutCut
	case key{op: ir.OpGet, typ: ir.TypeVariable}:
		return wam.OpGetVariable
	case key{op: ir.OpGet, typ: ir.TypeValue}:
		return wam.OpGetValue
	case key{op: ir.OpGet, typ: ir.TypeStructure}:
		return wam.OpGetStructure
	case key{op: ir.OpGet, typ: ir.TypeConstant}:
		return wam.OpGetConstant
	case key{op: ir.OpGet, typ: ir.TypeCut}:
		return wam.OpGetCut
	case key{op: ir.OpUnify, typ: ir.TypeVariable}:
		return wam.OpUnifyVariable
	case key{op: ir.OpUnify, typ: ir.TypeValue}:
		return wam.OpUnifyValue
	case key{op: ir.OpUnify, typ: ir.TypeConstant}:
		return wam.OpUnifyConstant
	case key{op: ir.OpWrite, typ: ir.TypeVariable}:
		return wam.OpWriteVariable
	case key{op: ir.OpWrite, typ: ir.TypeValue}:
		return wam.OpWriteValue
	case key{op: ir.OpWrite, typ: ir.TypeConstant}:
		return wam.OpWriteConstant
	case key{op: ir.OpLoad, typ: ir.TypeVariable}:
		return wam.OpLoadVariable
	case key{op: ir.OpLoad, typ: ir.TypeValue}:
		return wam.OpLoadValue
	case key{op: ir.OpLoad, typ: ir.TypeConstant}:
		return wam.OpLoadConstant
	case key{op: ir.OpPush, typ: ir.TypeStructure}:
		return wam.OpPushStructure
	case key{op: ir.OpPush, typ: ir.TypeCut}:
		return wam.OpPushCut
	case key{op: ir.OpBuiltin, typ: ir.TypeNotApplicable}, key{op: ir.OpInline, typ: ir.TypeNotApplicable}, key{op: ir.OpArithmetic, typ: ir.TypeNotApplicable}:
		return wam.OpBuiltin0 + wam.OpCode(inst.A.Index)
	default:
		return wam.OpNop
	}
}

func (e *Engine) DefineBuiltin0(name term.Atom, fn func(context.Context) iter.Seq[error]) {

}

func (e *Engine) Call(ctx context.Context, goal term.Handle) iter.Seq[error] {
	pi := term.NewFunctor(term.NewAtom("call"), 2)
	t, err := e.PutAtom(term.NewAtom("true"))
	if err != nil {
		return func(yield func(error) bool) {
			_ = yield(err)
		}
	}
	p, ok := e.Predicates[pi]
	if !ok {
		return func(yield func(error) bool) {
			pi := term.NewFunctor(pi.Name(), pi.Arity()-1)
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
		}
	}
	exec := Execution{
		Engine:         e,
		programPointer: p.Offset,
	}
	exec.tempVars[0] = goal
	exec.tempVars[1] = t
	return exec.run(ctx)
}
