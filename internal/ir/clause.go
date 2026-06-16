package ir

import (
	"fmt"
	"math"
	"slices"
	"strings"

	"github.com/ichiban/prolog/v2/internal/syntax"
	"github.com/ichiban/prolog/v2/internal/term"
)

type LifeTime struct {
	Birth int
	Death int
}

func (l LifeTime) String() string {
	return fmt.Sprintf("%d-%d", l.Birth, l.Death)
}

func (l LifeTime) Precedes(o LifeTime) bool {
	return l.Death < o.Birth
}

func (l LifeTime) Contains(o LifeTime) bool {
	return l.Birth <= o.Birth && o.Death <= l.Death
}

type Variables map[int]Variable

func (v Variables) String() string {
	type Var struct {
		varID int
		Variable
	}
	elems := make([]Var, 0, len(v))
	for varID, elem := range v {
		elems = append(elems, Var{varID: varID, Variable: elem})
	}
	slices.SortFunc(elems, func(i, j Var) int {
		return i.varID - j.varID
	})
	var sb strings.Builder
	for _, elem := range elems {
		_, _ = fmt.Fprintf(&sb, "%4d: %s\n", elem.varID, elem.String())
	}
	return sb.String()
}

type Variable struct {
	Count int
	Reg   int
	LifeTime
}

func (v Variable) String() string {
	return fmt.Sprintf("%d reg(%d) %s", v.Count, v.Reg, v.LifeTime)
}

type Arguments []Argument

func (a Arguments) String() string {
	var sb strings.Builder
	for _, arg := range a {
		_, _ = fmt.Fprintf(&sb, "%s\n", arg.String())
	}
	return sb.String()
}

type Argument struct {
	HeadVarID int
	BodyVarID int
	LifeTime
}

func (a Argument) String() string {
	return fmt.Sprintf("var(%d) var(%d) %s", a.HeadVarID, a.BodyVarID, a.LifeTime)
}

// Index represents a first argument index.
type Index struct {
	Term  term.Handle
	Arity int
}

func (i Index) String() string {
	return fmt.Sprintf("%s/%d", &syntax.Formatter{Term: i.Term}, i.Arity)
}

type Clause struct {
	PI       term.Functor
	FirstArg Index
	MaxRegs  int
	Code     []Instruction
	Execute  term.Functor
}

func (clause *Clause) Emit(inst Instruction) {
	clause.Code = append(clause.Code, inst)
}

func (clause *Clause) CollapseArgs(args []Argument, vars map[int]Variable) {
	for i := range args {
		a := &args[i]

		var (
			h = Variable{Reg: -1, LifeTime: LifeTime{Birth: 0, Death: math.MaxInt}}
			b = Variable{Reg: -1, LifeTime: LifeTime{Birth: 0, Death: math.MaxInt}}
		)
		if a.HeadVarID >= 0 {
			h = vars[a.HeadVarID]
		}
		if a.BodyVarID >= 0 {
			b = vars[a.BodyVarID]
		}

		switch {
		case a.LifeTime.Contains(h.LifeTime) && h.LifeTime.Precedes(b.LifeTime) && a.LifeTime.Contains(b.LifeTime) && h.Reg < 0 && b.Reg < 0:
			h.Reg = i
			b.Reg = i
			vars[a.HeadVarID] = h
			vars[a.BodyVarID] = b
		case a.LifeTime.Contains(h.LifeTime) && h.Reg < 0:
			h.Reg = i
			vars[a.HeadVarID] = h
		case a.LifeTime.Contains(b.LifeTime) && b.Reg < 0:
			b.Reg = i
			vars[a.BodyVarID] = b
		}
	}
}

func (clause *Clause) Compact() {
	clause.Code = slices.DeleteFunc(clause.Code, func(inst Instruction) bool {
		var (
			getVariable  = inst.OpCode == OpGet && inst.Type == TypeVariable
			putValue     = inst.OpCode == OpPut && inst.Type == TypeValue
			sameRegister = inst.A.Kind == OperandKindArgument && inst.B.Kind == OperandKindRegister && inst.A.Index == inst.B.Index
		)
		return (getVariable || putValue) && sameRegister
	})
}

type ClauseStringer struct {
	Arena *term.Arena
	*Clause
}

func (c ClauseStringer) String() string {
	var sb strings.Builder
	_, _ = fmt.Fprintf(&sb, "\tPI: %s\n", c.PI)
	_, _ = fmt.Fprintf(&sb, "\tfirst_arg: %s\n", c.FirstArg)
	_, _ = fmt.Fprintf(&sb, "\tmax_regs: %d\n", c.MaxRegs)
	_, _ = fmt.Fprintf(&sb, "\tcode:\n")
	for i, inst := range c.Code {
		inst := InstructionStringer{
			Arena:       c.Arena,
			Instruction: inst,
		}
		_, _ = fmt.Fprintf(&sb, "\t%4d: %s\n", i, inst.String())
	}
	_, _ = fmt.Fprintf(&sb, "\texecute: %s\n", c.Execute)
	return sb.String()
}
