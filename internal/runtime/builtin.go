// Ported to Go from BinProlog (github.com/ptarau/binprolog, src/extra.pl and
// related sources), Copyright (C) Paul Tarau, licensed under Apache-2.0.
// This file has been modified: translated to Go and adapted.

package runtime

import (
	"context"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"iter"
	"maps"
	"math"
	"os"
	"slices"
	"strings"
	"unicode"
	"unicode/utf8"

	"github.com/ichiban/prolog/v2/internal/db"
	"github.com/ichiban/prolog/v2/internal/syntax"
	"github.com/ichiban/prolog/v2/internal/term"
	"github.com/ichiban/prolog/v2/internal/wam"
)

type CallingConvention int8

const (
	InHead CallingConvention = iota
	InBody
)

type Promise struct {
	ok      bool
	err     error
	delayed iter.Seq[Promise]
}

func Failure() Promise {
	return Promise{ok: false}
}

func Error(err error) Promise {
	return Promise{err: err}
}

func Delay(seq iter.Seq[Promise]) Promise {
	return Promise{delayed: seq}
}

type Builtin struct {
	PI   term.Functor
	Type CallingConvention
	Proc Procedure
}

type BuiltinSet struct {
	index   map[term.Functor]int
	entries []Builtin
}

func NewBuiltinSet() *BuiltinSet {
	var b BuiltinSet
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("true"), 1), Type: InHead, Proc: Nondeterministic0(True0)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("fail"), 1), Type: InHead, Proc: Deterministic0(Fail0)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("call"), 2), Type: InHead, Proc: Nondeterministic1(Call1)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("throw"), 2), Type: InHead, Proc: Nondeterministic1(Throw1)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("subsumes_term"), 3), Type: InHead, Proc: Deterministic2(SubsumesTerm2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("var"), 2), Type: InBody, Proc: Inline1(Var1)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("atom"), 2), Type: InBody, Proc: Inline1(Atom1)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("integer"), 2), Type: InBody, Proc: Inline1(Integer1)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("float"), 2), Type: InBody, Proc: Inline1(Float1)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("compound"), 2), Type: InBody, Proc: Inline1(Compound1)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("ground"), 2), Type: InBody, Proc: Inline1(Ground1)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("acyclic_term"), 2), Type: InBody, Proc: Inline1(AcyclicTerm1)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("compare"), 4), Type: InHead, Proc: Deterministic3(Compare3)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("sort"), 3), Type: InHead, Proc: Deterministic2(Sort2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("keysort"), 3), Type: InHead, Proc: Deterministic2(KeySort2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("functor"), 4), Type: InHead, Proc: Deterministic3(Functor3)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("arg"), 4), Type: InHead, Proc: Deterministic3(Arg3)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("=.."), 3), Type: InHead, Proc: Deterministic2(Univ2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("copy_term"), 3), Type: InHead, Proc: Deterministic2(CopyTerm2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("term_variables"), 3), Type: InHead, Proc: Deterministic2(TermVariables2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("clause"), 3), Type: InHead, Proc: Nondeterministic2(Clause2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("current_predicate"), 2), Type: InHead, Proc: Nondeterministic1(CurrentPredicate1)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("asserta"), 2), Type: InHead, Proc: Deterministic1(AssertA1)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("assertz"), 2), Type: InHead, Proc: Deterministic1(AssertZ1)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("retract"), 2), Type: InHead, Proc: Nondeterministic1(Retract1)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("abolish"), 2), Type: InHead, Proc: Deterministic1(Abolish1)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("findall"), 4), Type: InHead, Proc: Deterministic3(FindAll3)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("bagof"), 4), Type: InHead, Proc: Deterministic3(BagOf3)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("setof"), 4), Type: InHead, Proc: Deterministic3(SetOf3)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("current_input"), 2), Type: InHead, Proc: Deterministic1(CurrentInput1)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("current_output"), 2), Type: InHead, Proc: Deterministic1(CurrentOutput1)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("set_input"), 2), Type: InHead, Proc: Deterministic1(SetInput1)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("set_output"), 2), Type: InHead, Proc: Deterministic1(SetOutput1)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("open"), 5), Type: InHead, Proc: Deterministic4(Open4)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("close"), 3), Type: InHead, Proc: Deterministic2(Close2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("flush_output"), 2), Type: InHead, Proc: Deterministic1(FlushOutput1)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("stream_property"), 3), Type: InHead, Proc: Nondeterministic2(StreamProperty2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("set_stream_position"), 3), Type: InHead, Proc: Deterministic2(SetStreamPosition2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("get_char"), 3), Type: InHead, Proc: Deterministic2(GetChar2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("get_code"), 3), Type: InHead, Proc: Deterministic2(GetCode2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("peek_char"), 3), Type: InHead, Proc: Deterministic2(PeekChar2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("peek_code"), 3), Type: InHead, Proc: Deterministic2(PeekCode2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("put_char"), 3), Type: InHead, Proc: Deterministic2(PutChar2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("put_code"), 3), Type: InHead, Proc: Deterministic2(PutCode2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("get_byte"), 3), Type: InHead, Proc: Deterministic2(GetByte2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("peek_byte"), 3), Type: InHead, Proc: Deterministic2(PeekByte2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("put_byte"), 3), Type: InHead, Proc: Deterministic2(PutByte2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("read_term"), 4), Type: InHead, Proc: Deterministic3(ReadTerm3)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("write_term"), 4), Type: InHead, Proc: Deterministic3(WriteTerm3)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("op"), 4), Type: InHead, Proc: Deterministic3(Op3)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("current_op"), 4), Type: InHead, Proc: Nondeterministic3(CurrentOp3)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("char_conversion"), 3), Type: InHead, Proc: Deterministic2(CharConversion2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("current_char_conversion"), 3), Type: InHead, Proc: Nondeterministic2(CurrentCharConversion2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("call"), 3), Type: InHead, Proc: Deterministic2(Call2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("call"), 4), Type: InHead, Proc: Deterministic3(Call3)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("call"), 5), Type: InHead, Proc: Deterministic4(Call4)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("call"), 6), Type: InHead, Proc: Deterministic5(Call5)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("call"), 7), Type: InHead, Proc: Deterministic6(Call6)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("call"), 8), Type: InHead, Proc: Deterministic7(Call7)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("call"), 9), Type: InHead, Proc: Deterministic8(Call8)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("atom_length"), 3), Type: InHead, Proc: Deterministic2(AtomLength2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("atom_concat"), 4), Type: InHead, Proc: Nondeterministic3(AtomConcat3)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("sub_atom"), 6), Type: InHead, Proc: Nondeterministic5(SubAtom5)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("atom_chars"), 3), Type: InHead, Proc: Deterministic2(AtomChars2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("atom_codes"), 3), Type: InHead, Proc: Deterministic2(AtomCodes2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("char_code"), 3), Type: InHead, Proc: Deterministic2(CharCode2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("number_chars"), 3), Type: InHead, Proc: Deterministic2(NumberChars2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("number_codes"), 3), Type: InHead, Proc: Deterministic2(NumberCodes2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("set_prolog_flag"), 3), Type: InHead, Proc: Deterministic2(SetPrologFlag2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("current_prolog_flag"), 3), Type: InHead, Proc: Nondeterministic2(CurrentPrologFlag2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("halt"), 2), Type: InHead, Proc: Deterministic1(Halt1)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$dynamic"), 2), Type: InHead, Proc: Deterministic1(Dynamic1)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$multifile"), 2), Type: InHead, Proc: Deterministic1(Multifile1)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$discontiguous"), 2), Type: InHead, Proc: Deterministic1(Discontiguous1)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$get_neck_cut"), 2), Type: InBody, Proc: Inline1(GetNeckCut1)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$get_cont"), 2), Type: InHead, Proc: Deterministic1(GetCont1)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$call_cont"), 2), Type: InHead, Proc: Nondeterministic1(CallCont1)})
	// TODO: Implement optimized arithmetic calling convention in binprolog.
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$+"), 4), Type: InHead, Proc: Deterministic3(Add3)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$-"), 4), Type: InHead, Proc: Deterministic3(Sub3)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$*"), 4), Type: InHead, Proc: Deterministic3(Mul3)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$//"), 4), Type: InHead, Proc: Deterministic3(IntDiv3)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$/"), 4), Type: InHead, Proc: Deterministic3(Div3)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$rem"), 4), Type: InHead, Proc: Deterministic3(Rem3)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$mod"), 4), Type: InHead, Proc: Deterministic3(Mod3)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$-"), 3), Type: InHead, Proc: Deterministic2(Neg2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$abs"), 3), Type: InHead, Proc: Deterministic2(Abs2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$sign"), 3), Type: InHead, Proc: Deterministic2(Sign2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$float_integer_part"), 3), Type: InHead, Proc: Deterministic2(FloatIntegerPart2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$float_fractional_part"), 3), Type: InHead, Proc: Deterministic2(FloatFractionalPart2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$float"), 3), Type: InHead, Proc: Deterministic2(Float2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$floor"), 3), Type: InHead, Proc: Deterministic2(Floor2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$truncate"), 3), Type: InHead, Proc: Deterministic2(Truncate2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$round"), 3), Type: InHead, Proc: Deterministic2(Round2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$ceiling"), 3), Type: InHead, Proc: Deterministic2(Ceiling2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$div"), 4), Type: InHead, Proc: Deterministic3(FloorDiv3)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$+"), 3), Type: InHead, Proc: Deterministic2(Pos2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$**"), 4), Type: InHead, Proc: Deterministic3(Power3)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$sin"), 3), Type: InHead, Proc: Deterministic2(Sin2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$cos"), 3), Type: InHead, Proc: Deterministic2(Cos2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$atan"), 3), Type: InHead, Proc: Deterministic2(Atan2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$exp"), 3), Type: InHead, Proc: Deterministic2(Exp2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$log"), 3), Type: InHead, Proc: Deterministic2(Log2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$sqrt"), 3), Type: InHead, Proc: Deterministic2(Sqrt2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$max"), 4), Type: InHead, Proc: Deterministic3(Max3)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$min"), 4), Type: InHead, Proc: Deterministic3(Min3)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$^"), 4), Type: InHead, Proc: Deterministic3(IntegerPower3)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$asin"), 3), Type: InHead, Proc: Deterministic2(Asin2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$acos"), 3), Type: InHead, Proc: Deterministic2(Acos2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$atan2"), 4), Type: InHead, Proc: Deterministic3(Atan3)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$tan"), 3), Type: InHead, Proc: Deterministic2(Tan2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$pi"), 2), Type: InHead, Proc: Deterministic1(Pi1)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$>>"), 4), Type: InHead, Proc: Deterministic3(BitwiseRightShift3)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$<<"), 4), Type: InHead, Proc: Deterministic3(BitwiseLeftShift3)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$/\\"), 4), Type: InHead, Proc: Deterministic3(BitwiseAnd3)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$\\/"), 4), Type: InHead, Proc: Deterministic3(BitwiseOr3)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$\\"), 3), Type: InHead, Proc: Deterministic2(BitwiseComplement2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$xor"), 4), Type: InHead, Proc: Deterministic3(BitwiseXor3)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$arith_eq"), 3), Type: InHead, Proc: Deterministic2(ArithEq2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$arith_dif"), 3), Type: InHead, Proc: Deterministic2(ArithDif2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$less"), 3), Type: InHead, Proc: Deterministic2(Less2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$less_eq"), 3), Type: InHead, Proc: Deterministic2(LessEq2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$greater"), 3), Type: InHead, Proc: Deterministic2(Greater2)})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$greater_eq"), 3), Type: InHead, Proc: Deterministic2(GreaterEq2)})
	return &b
}

func (b *BuiltinSet) Lookup(pi term.Functor) (int, bool) {
	if b == nil {
		return 0, false
	}
	id, ok := b.index[pi]
	return id, ok
}

func (b *BuiltinSet) Get(id int) *Builtin {
	return &b.entries[id]
}

// DuplicateBuiltinError is returned by [BuiltinSet.Put] when PI is already registered.
// PI is binarized, i.e. its arity includes the continuation argument.
type DuplicateBuiltinError struct {
	PI term.Functor
}

func (e *DuplicateBuiltinError) Error() string {
	return fmt.Sprintf("duplicate builtin: %s", e.PI)
}

func (b *BuiltinSet) Put(entry Builtin) error {
	if _, ok := b.index[entry.PI]; ok {
		return &DuplicateBuiltinError{PI: entry.PI}
	}

	if b.index == nil {
		b.index = map[term.Functor]int{}
	}
	b.index[entry.PI] = len(b.entries)
	b.entries = append(b.entries, entry)
	return nil
}

func (b *BuiltinSet) All() iter.Seq2[term.Functor, *Builtin] {
	keys := slices.Collect(maps.Keys(b.index))
	slices.SortFunc(keys, func(a, b term.Functor) int {
		return strings.Compare(a.String(), b.String())
	})
	return func(yield func(term.Functor, *Builtin) bool) {
		for _, key := range keys {
			id := b.index[key]
			if !yield(key, &b.entries[id]) {
				return
			}
		}
	}
}

// Procedure is either [Inline1], [Deterministic1], [Nondeterministic1], and their variants.
type Procedure interface {
	Call(ctx context.Context, a *Activation) Promise
}

type Inline1 func(ctx context.Context, e *Execution, t term.Cell) (bool, error)

func (i Inline1) Call(ctx context.Context, a *Activation) Promise {
	e := a.exec
	t := e.tempVars[0]
	ok, err := i(ctx, e, t)
	if err != nil {
		return Error(err)
	}
	if ok {
		e.Next()
	}
	return Promise{ok: ok}
}

type Deterministic0 func(ctx context.Context, e *Execution, cont term.Cell) Promise

func (p Deterministic0) Call(ctx context.Context, a *Activation) Promise {
	e := a.exec
	cont := e.tempVars[1]
	return p(ctx, e, cont)
}

type Deterministic1 func(ctx context.Context, e *Execution, arg1, cont term.Cell) Promise

func (p Deterministic1) Call(ctx context.Context, a *Activation) Promise {
	e := a.exec
	arg1, cont := e.tempVars[1], e.tempVars[2]
	return p(ctx, e, arg1, cont)
}

type Deterministic2 func(ctx context.Context, e *Execution, arg1, arg2, cont term.Cell) Promise

func (p Deterministic2) Call(ctx context.Context, a *Activation) Promise {
	e := a.exec
	arg1, arg2, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]
	return p(ctx, e, arg1, arg2, cont)
}

type Deterministic3 func(ctx context.Context, e *Execution, arg1, arg2, arg3, cont term.Cell) Promise

func (p Deterministic3) Call(ctx context.Context, a *Activation) Promise {
	e := a.exec
	arg1, arg2, arg3, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4]
	return p(ctx, e, arg1, arg2, arg3, cont)
}

type Deterministic4 func(ctx context.Context, e *Execution, arg1, arg2, arg3, arg4, cont term.Cell) Promise

func (p Deterministic4) Call(ctx context.Context, a *Activation) Promise {
	e := a.exec
	arg1, arg2, arg3, arg4, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4], e.tempVars[5]
	return p(ctx, e, arg1, arg2, arg3, arg4, cont)
}

type Deterministic5 func(ctx context.Context, e *Execution, arg1, arg2, arg3, arg4, arg5, cont term.Cell) Promise

func (p Deterministic5) Call(ctx context.Context, a *Activation) Promise {
	e := a.exec
	arg1, arg2, arg3, arg4, arg5, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4], e.tempVars[5], e.tempVars[6]
	return p(ctx, e, arg1, arg2, arg3, arg4, arg5, cont)
}

type Deterministic6 func(ctx context.Context, e *Execution, arg1, arg2, arg3, arg4, arg5, arg6, cont term.Cell) Promise

func (p Deterministic6) Call(ctx context.Context, a *Activation) Promise {
	e := a.exec
	arg1, arg2, arg3, arg4, arg5, arg6, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4], e.tempVars[5], e.tempVars[6], e.tempVars[7]
	return p(ctx, e, arg1, arg2, arg3, arg4, arg5, arg6, cont)
}

type Deterministic7 func(ctx context.Context, e *Execution, arg1, arg2, arg3, arg4, arg5, arg6, arg7, cont term.Cell) Promise

func (p Deterministic7) Call(ctx context.Context, a *Activation) Promise {
	e := a.exec
	arg1, arg2, arg3, arg4, arg5, arg6, arg7, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4], e.tempVars[5], e.tempVars[6], e.tempVars[7], e.tempVars[8]
	return p(ctx, e, arg1, arg2, arg3, arg4, arg5, arg6, arg7, cont)
}

type Deterministic8 func(ctx context.Context, e *Execution, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, cont term.Cell) Promise

func (p Deterministic8) Call(ctx context.Context, a *Activation) Promise {
	e := a.exec
	arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4], e.tempVars[5], e.tempVars[6], e.tempVars[7], e.tempVars[8], e.tempVars[9]
	return p(ctx, e, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, cont)
}

type Nondeterministic0 func(ctx context.Context, a *Activation, cont Ref) Promise

func (p Nondeterministic0) Call(ctx context.Context, a *Activation) Promise {
	e := a.exec
	cont := e.tempVars[1]
	return p(ctx, a, a.ref(cont))
}

type Nondeterministic1 func(ctx context.Context, a *Activation, arg1, cont Ref) Promise

func (p Nondeterministic1) Call(ctx context.Context, a *Activation) Promise {
	e := a.exec
	arg1, cont := e.tempVars[1], e.tempVars[2]
	return p(ctx, a, a.ref(arg1), a.ref(cont))
}

type Nondeterministic2 func(ctx context.Context, a *Activation, arg1, arg2, cont Ref) Promise

func (p Nondeterministic2) Call(ctx context.Context, a *Activation) Promise {
	e := a.exec
	arg1, arg2, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]
	return p(ctx, a, a.ref(arg1), a.ref(arg2), a.ref(cont))
}

type Nondeterministic3 func(ctx context.Context, a *Activation, arg1, arg2, arg3, cont Ref) Promise

func (p Nondeterministic3) Call(ctx context.Context, a *Activation) Promise {
	e := a.exec
	arg1, arg2, arg3, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4]
	return p(ctx, a, a.ref(arg1), a.ref(arg2), a.ref(arg3), a.ref(cont))
}

type Nondeterministic4 func(ctx context.Context, a *Activation, arg1, arg2, arg3, arg4, cont Ref) Promise

func (p Nondeterministic4) Call(ctx context.Context, a *Activation) Promise {
	e := a.exec
	arg1, arg2, arg3, arg4, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4], e.tempVars[5]
	return p(ctx, a, a.ref(arg1), a.ref(arg2), a.ref(arg3), a.ref(arg4), a.ref(cont))
}

type Nondeterministic5 func(ctx context.Context, a *Activation, arg1, arg2, arg3, arg4, arg5, cont Ref) Promise

func (p Nondeterministic5) Call(ctx context.Context, a *Activation) Promise {
	e := a.exec
	arg1, arg2, arg3, arg4, arg5, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4], e.tempVars[5], e.tempVars[6]
	return p(ctx, a, a.ref(arg1), a.ref(arg2), a.ref(arg3), a.ref(arg4), a.ref(arg5), a.ref(cont))
}

type Nondeterministic6 func(ctx context.Context, a *Activation, arg1, arg2, arg3, arg4, arg5, arg6, cont Ref) Promise

func (p Nondeterministic6) Call(ctx context.Context, a *Activation) Promise {
	e := a.exec
	arg1, arg2, arg3, arg4, arg5, arg6, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4], e.tempVars[5], e.tempVars[6], e.tempVars[7]
	return p(ctx, a, a.ref(arg1), a.ref(arg2), a.ref(arg3), a.ref(arg4), a.ref(arg5), a.ref(arg6), a.ref(cont))
}

type Nondeterministic7 func(ctx context.Context, a *Activation, arg1, arg2, arg3, arg4, arg5, arg6, arg7, cont Ref) Promise

func (p Nondeterministic7) Call(ctx context.Context, a *Activation) Promise {
	e := a.exec
	arg1, arg2, arg3, arg4, arg5, arg6, arg7, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4], e.tempVars[5], e.tempVars[6], e.tempVars[7], e.tempVars[8]
	return p(ctx, a, a.ref(arg1), a.ref(arg2), a.ref(arg3), a.ref(arg4), a.ref(arg5), a.ref(arg6), a.ref(arg7), a.ref(cont))
}

type Nondeterministic8 func(ctx context.Context, a *Activation, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, cont Ref) Promise

func (p Nondeterministic8) Call(ctx context.Context, a *Activation) Promise {
	e := a.exec
	arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4], e.tempVars[5], e.tempVars[6], e.tempVars[7], e.tempVars[8], e.tempVars[9]
	return p(ctx, a, a.ref(arg1), a.ref(arg2), a.ref(arg3), a.ref(arg4), a.ref(arg5), a.ref(arg6), a.ref(arg7), a.ref(arg8), a.ref(cont))
}

func True0(ctx context.Context, a *Activation, cont Ref) Promise {
	cont = a.Deref(cont)

	bpi, ok := a.Functor(cont, term.AllowAtom(true))
	if !ok {
		return a.Throw(&TypeError{
			ValidType: term.NewAtom("continuation"),
			Culprit:   syntax.Serialize(a.exec.Arena, *cont.cell),
			Location:  a.exec.location,
		}, cont)
	}

	pi := term.NewFunctor(bpi.Name(), bpi.Arity()-1)

	p, ok, err := a.exec.Predicate(bpi)
	if err != nil {
		return a.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	if p.Dynamic {
		args := slices.Collect(a.Args(cont))
		goal, err := a.PutCompound(pi.Name(), args[:len(args)-1]...)
		if err != nil {
			return a.Throw(err, cont)
		}
		cont = args[len(args)-1]
		return a.Nondet(func(yield func(Promise) bool) {
			for r := range a.exec.DB.Select(ctx, a.exec.Arena, pi, a.exec.CurrentTime) {
				ok, err := a.exec.Unify(r.Head, *goal.cell)
				if err != nil {
					_ = yield(a.Throw(err, cont))
					return
				}
				if !ok {
					if !yield(Failure()) {
						return
					}
					continue
				}

				if !yield(Call1(ctx, a, a.ref(r.Body), cont)) {
					return
				}
			}
		})
	}

	a.exec.enter(p.Offset, a.exec.Args(*cont.cell))
	return Promise{ok: true}
}

func Fail0(_ context.Context, _ *Execution, _ term.Cell) Promise {
	return Failure()
}

func Call1(ctx context.Context, a *Activation, goal, cont Ref) Promise {
	goal = a.Deref(goal)

	// 7.8.3.1 says "When G contains ! as a subgoal, the effect of ! shall not extend outside G."
	g, err := a.exec.rewriteCutForCall(*goal.cell)
	if err != nil {
		return a.Throw(err, cont)
	}
	goal = a.ref(g)

	pi, ok := a.Functor(goal, term.AllowAtom(true))
	if !ok {
		if _, ok := a.Variable(goal); ok {
			return a.Throw(&InstantiationError{
				Location: a.exec.location,
			}, cont)
		}
		return a.Throw(&TypeError{
			ValidType: term.NewAtom("callable"),
			Culprit:   syntax.Serialize(a.exec.Arena, *goal.cell),
			Location:  a.exec.location,
		}, cont)
	}

	bpi := term.NewFunctor(pi.Name(), pi.Arity()+1)
	p, ok, err := a.exec.Predicate(bpi)
	if err != nil {
		return a.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}
	if p.Dynamic {
		call, ok := a.exec.Predicates[term.NewFunctor(term.NewAtom("call"), 2)]
		if !ok {
			c, err := a.exec.PutFunctor(term.NewFunctor(term.NewAtom("call"), 1))
			if err != nil {
				return a.Throw(err, cont)
			}
			return a.Throw(&ExistenceError{
				ObjectType: term.NewAtom("procedure"),
				Culprit:    syntax.Serialize(a.exec.Arena, c),
				Location:   a.exec.location,
			}, cont)
		}

		return a.Nondet(func(yield func(Promise) bool) {
			for r := range a.exec.DB.Select(ctx, a.exec.Arena, pi, a.exec.CurrentTime) {
				ok, err := a.exec.Unify(r.Head, *goal.cell)
				if err != nil {
					_ = yield(a.Throw(err, cont))
					return
				}
				if !ok {
					if !yield(Failure()) {
						return
					}
					continue
				}

				a.exec.enter(call.Offset, slices.Values([]term.Cell{r.Body, *cont.cell}))
				if !yield(Promise{ok: true}) {
					return
				}
			}
		})
	}

	a.exec.enter(p.Offset, slices.Values(cells(slices.Collect(concat(a.Args(goal), singleton(cont))))))
	return Promise{ok: true}
}

func (e *Execution) rewriteCutForCall(body term.Cell) (term.Cell, error) {
	body = e.Deref(body)
	switch pi, _ := e.Functor(body, term.AllowAtom(true)); pi {
	case term.NewFunctor(term.NewAtomRune(';'), 2):
		x := e.Arg(body, 0)
		if f, _ := e.Functor(x); f == term.NewFunctor(term.NewAtom("->"), 2) {
			i, t := e.Arg(x, 0), e.Arg(x, 1)
			i, err := e.rewriteCutForCall(i)
			if err != nil {
				return term.Cell{}, err
			}
			t, err = e.rewriteCutForCall(t)
			if err != nil {
				return term.Cell{}, err
			}
			x, err = e.PutCompound(term.NewAtom("->"), i, t)
			if err != nil {
				return term.Cell{}, err
			}
		}
		fallthrough
	case term.NewFunctor(term.NewAtomRune(','), 2):
		x, y := e.Arg(body, 0), e.Arg(body, 1)
		x, err := e.rewriteCutForCall(x)
		if err != nil {
			return term.Cell{}, err
		}
		y, err = e.rewriteCutForCall(y)
		if err != nil {
			return term.Cell{}, err
		}
		return e.PutCompound(pi.Name(), x, y)
	case term.NewFunctor(term.NewAtomRune('!'), 0):
		b, err := e.PutInteger(int64(len(e.stack)))
		if err != nil {
			return term.Cell{}, err
		}
		return e.PutCompound(term.NewAtom("$cut_to"), b)
	default:
		return body, nil
	}
}

func Var1(_ context.Context, e *Execution, v term.Cell) (bool, error) {
	v = e.Deref(v)
	_, ok := e.Variable(v)
	return ok, nil
}

func Atom1(_ context.Context, e *Execution, t term.Cell) (bool, error) {
	t = e.Deref(t)
	_, ok := e.Atom(t)
	return ok, nil
}

func Integer1(_ context.Context, e *Execution, t term.Cell) (bool, error) {
	t = e.Deref(t)
	_, ok := e.Integer(t)
	return ok, nil
}

func Float1(_ context.Context, e *Execution, t term.Cell) (bool, error) {
	t = e.Deref(t)
	_, ok := e.Float(t)
	return ok, nil
}

func Compound1(_ context.Context, e *Execution, t term.Cell) (bool, error) {
	t = e.Deref(t)
	_, ok := e.Functor(t)
	return ok, nil
}

func Ground1(_ context.Context, e *Execution, t term.Cell) (bool, error) {
	t = e.Deref(t)
	vs := e.VariableSet(t)
	return len(vs) == 0, nil
}

func AcyclicTerm1(_ context.Context, e *Execution, t term.Cell) (bool, error) {
	t = e.Deref(t)
	return e.Acyclic(t), nil
}

func Throw1(ctx context.Context, a *Activation, ball, cont Ref) Promise {
	ball = a.Deref(ball)
	if _, ok := a.Variable(ball); ok {
		var err error
		err = &InstantiationError{
			Location: a.exec.location,
		}
		b, err := ErrorTerm(a.exec.Arena, err)
		if err != nil {
			return a.Throw(err, cont)
		}
		ball = a.ref(b)
	}

	serialized := syntax.Serialize(a.exec.Arena, *ball.cell)

	for cont := range contChain(a.exec.Arena, *cont.cell) {
		if pi, ok := a.exec.Functor(cont); !ok || pi.Name() != term.NewAtom("$to_catch") || pi.Arity() != 5 {
			continue
		}

		k := a.ref(cont)
		catcher, recovery, cutB, cont := a.Arg(k, 0), a.Arg(k, 1), a.Arg(k, 2), a.Arg(k, 3)

		b, _ := a.MustBeInteger(cutB)
		if err := a.exec.unTrailTo(int(b)); err != nil {
			return a.Throw(err, cont)
		}

		ball, err := syntax.Deserialize(a.exec.Arena, serialized)
		if err != nil {
			return Error(fmt.Errorf("parse serialized ball(%s): %w", serialized, err))
		}

		ok, err := a.Unify(catcher, a.ref(ball))
		if err != nil {
			return a.Throw(err, cont)
		}
		if ok {
			return Call1(ctx, a, recovery, cont)
		}
	}
	return Error(&uncaughtBall{ball: serialized})
}

// uncaughtBall is a ball which found no matching catch/3 in the execution it
// was thrown in. The ball is kept serialized because Engine.Call unwinds its
// execution's trail on the way out, which would unbind the variables within it.
//
// The caller of a nested execution rethrows the ball so that it carries on
// outwards; when it reaches the host instead, 7.8.9.1 calls for a system error
// unless the ball is itself an error term.
type uncaughtBall struct {
	ball syntax.Serialized
}

func (u *uncaughtBall) Error() string {
	return fmt.Sprintf("unhandled exception: %s", u.ball)
}

// rethrow sends err on its way in this execution. A ball thrown in a nested
// execution keeps its identity; anything else is described as an error term.
func (e *Execution) rethrow(err error, cont term.Cell) Promise {
	var ball *uncaughtBall
	if errors.As(err, &ball) {
		t, err := syntax.Deserialize(e.Arena, ball.ball)
		if err != nil {
			return Error(err)
		}
		return e.throwBall(t, cont)
	}
	return e.Throw(err, cont)
}

func SubsumesTerm2(_ context.Context, e *Execution, general, specific, cont term.Cell) Promise {
	trailTop := len(e.trail)
	vs := e.VariableSet(specific)

	// Same as unify_with_occurs_check(General, Specific).
	ok, err := e.Unify(general, specific)
	if err != nil {
		return e.Throw(err, cont)
	}
	ok = ok && e.Acyclic(general)

	// Checks if the temporary bindings keep Specific intact.
	for _, v := range vs {
		w := e.Deref(v)
		ok = ok && v == w
	}

	if err := e.unwindTrail(trailTop); err != nil {
		return e.Throw(err, cont)
	}

	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func Compare3(_ context.Context, e *Execution, order, x, y, cont term.Cell) Promise {
	order, x, y = e.Deref(order), e.Deref(x), e.Deref(y)

	if _, ok := e.Variable(order); ok {
		// Do nothing.
	} else if a, ok := e.Atom(order); ok {
		switch a {
		case term.NewAtomRune('<'), term.NewAtomRune('>'), term.NewAtomRune('='):
			break
		default:
			return e.Throw(&DomainError{
				ValidDomain: term.NewAtom("order"),
				Culprit:     syntax.Serialize(e.Arena, order),
				Location:    e.location,
			}, cont)
		}
	} else {
		return e.Throw(&TypeError{
			ValidType: term.NewAtom("atom"),
			Culprit:   syntax.Serialize(e.Arena, order),
			Location:  e.location,
		}, cont)
	}

	var (
		a   term.Cell
		err error
	)
	switch o := e.Compare(x, y); {
	case o < 0:
		a, err = e.PutAtom(term.NewAtomRune('<'))
	case o > 0:
		a, err = e.PutAtom(term.NewAtomRune('>'))
	default:
		a, err = e.PutAtom(term.NewAtomRune('='))
	}
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(order, a)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func Sort2(_ context.Context, e *Execution, list, sorted, cont term.Cell) Promise {
	var ts []term.Cell
	if err := e.MustBeList(list, func(elem term.Cell) error {
		ts = append(ts, elem)
		return nil
	}); err != nil {
		return e.Throw(err, cont)
	}

	if _, err := e.canBeList(sorted, nil); err != nil {
		return e.Throw(err, cont)
	}

	slices.SortFunc(ts, e.Compare)
	ts = slices.CompactFunc(ts, func(a, b term.Cell) bool {
		return e.Compare(a, b) == 0
	})

	l, err := e.PutList(ts...)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(sorted, l)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func KeySort2(_ context.Context, e *Execution, pairs, sorted, cont term.Cell) Promise {
	var ps []term.Cell
	if err := e.MustBeList(pairs, func(pair term.Cell) error {
		pair = e.Deref(pair)
		if _, ok := e.Variable(pair); ok {
			return &InstantiationError{
				Location: e.location,
			}
		}

		if f, ok := e.Functor(pair); !ok || f != term.NewFunctor(term.NewAtomRune('-'), 2) {
			return &TypeError{
				ValidType: term.NewAtom("pair"),
				Culprit:   syntax.Serialize(e.Arena, pair),
				Location:  e.location,
			}
		}
		ps = append(ps, pair)
		return nil
	}); err != nil {
		return e.Throw(err, cont)
	}

	if _, err := e.canBeList(sorted, func(pair term.Cell) error {
		if f, ok := e.Functor(pair); !ok || f != term.NewFunctor(term.NewAtomRune('-'), 2) {
			return &TypeError{
				ValidType: term.NewAtom("pair"),
				Culprit:   syntax.Serialize(e.Arena, pair),
				Location:  e.location,
			}
		}
		return nil
	}); err != nil {
		return e.Throw(err, cont)
	}

	ts := make([]term.Cell, len(ps))
	for i, pair := range ps {
		key, value := e.Arg(pair, 0), e.Arg(pair, 1)
		p, err := e.PutInteger(int64(i))
		if err != nil {
			return e.Throw(err, cont)
		}
		t, err := e.PutCompound(term.NewAtomRune('t'), key, p, value)
		if err != nil {
			return e.Throw(err, cont)
		}
		ts[i] = t
	}

	slices.SortFunc(ts, e.Compare)

	kvs := make([]term.Cell, len(ts))
	for i, t := range ts {
		key, value := e.Arg(t, 0), e.Arg(t, 2)
		p, err := e.PutCompound(term.NewAtomRune('-'), key, value)
		if err != nil {
			return e.Throw(err, cont)
		}
		kvs[i] = p
	}

	l, err := e.PutList(kvs...)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(sorted, l)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func Functor3(_ context.Context, e *Execution, t, name, arity, cont term.Cell) Promise {
	t, name, arity = e.Deref(t), e.Deref(name), e.Deref(arity)

	if _, ok := e.Variable(t); ok {
		if _, ok := e.Variable(arity); ok {
			return e.Throw(&InstantiationError{
				Location: e.location,
			}, cont)
		} else if a, ok := e.Integer(arity); ok {
			if a < 0 {
				return e.Throw(&DomainError{
					ValidDomain: term.NewAtom("not_less_than_zero"),
					Culprit:     syntax.Serialize(e.Arena, arity),
					Location:    e.location,
				}, cont)
			}

			if _, ok := e.Variable(name); ok {
				return e.Throw(&InstantiationError{
					Location: e.location,
				}, cont)
			} else if _, ok := e.Functor(name); ok {
				return e.Throw(&TypeError{
					ValidType: term.NewAtom("atomic"),
					Culprit:   syntax.Serialize(e.Arena, name),
					Location:  e.location,
				}, cont)
			}

			if a == 0 {
				ok, err := e.Unify(t, name)
				if err != nil {
					return e.Throw(err, cont)
				}
				if !ok {
					return Failure()
				}
			} else if n, ok := e.Atom(name); ok {
				c, err := e.PutCompoundWithFreshVars(term.NewFunctor(n, int(a)))
				if err != nil {
					return e.Throw(err, cont)
				}

				ok, err = e.Unify(t, c)
				if err != nil {
					return e.Throw(err, cont)
				}
				if !ok {
					return Failure()
				}
			} else {
				return e.Throw(&TypeError{
					ValidType: term.NewAtom("atom"),
					Culprit:   syntax.Serialize(e.Arena, name),
					Location:  e.location,
				}, cont)
			}
		} else {
			return e.Throw(&TypeError{
				ValidType: term.NewAtom("integer"),
				Culprit:   syntax.Serialize(e.Arena, arity),
				Location:  e.location,
			}, cont)
		}
	} else if f, ok := e.Functor(t); ok {
		n, err := e.PutAtom(f.Name())
		if err != nil {
			return e.Throw(err, cont)
		}

		ok, err := e.Unify(name, n)
		if err != nil {
			return e.Throw(err, cont)
		}
		if !ok {
			return Failure()
		}

		a, err := e.PutInteger(int64(f.Arity()))
		if err != nil {
			return e.Throw(err, cont)
		}

		ok, err = e.Unify(arity, a)
		if err != nil {
			return e.Throw(err, cont)
		}
		if !ok {
			return Failure()
		}
	} else { // atomic
		ok, err := e.Unify(name, t)
		if err != nil {
			return e.Throw(err, cont)
		}
		if !ok {
			return Failure()
		}

		a, err := e.PutInteger(int64(0))
		if err != nil {
			return e.Throw(err, cont)
		}

		ok, err = e.Unify(arity, a)
		if err != nil {
			return e.Throw(err, cont)
		}
		if !ok {
			return Failure()
		}
	}

	return e.Success(cont)
}

func Arg3(_ context.Context, e *Execution, nth, t, arg, cont term.Cell) Promise {
	nth, t, arg = e.Deref(nth), e.Deref(t), e.Deref(arg)

	if _, ok := e.Variable(t); ok {
		return e.Throw(&InstantiationError{
			Location: e.location,
		}, cont)
	} else if f, ok := e.Functor(t); ok {
		if _, ok := e.Variable(nth); ok {
			return e.Throw(&InstantiationError{
				Location: e.location,
			}, cont)
		} else if n, ok := e.Integer(nth); ok {
			switch {
			case n == 0, int(n) > f.Arity():
				return Failure()
			case n < 0:
				return e.Throw(&DomainError{
					ValidDomain: term.NewAtom("not_less_than_zero"),
					Culprit:     syntax.Serialize(e.Arena, nth),
					Location:    e.location,
				}, cont)
			default:
				a := e.Arg(t, int(n)-1)
				ok, err := e.Unify(arg, a)
				if err != nil {
					return e.Throw(err, cont)
				}
				if !ok {
					return Failure()
				}
			}

		} else {
			return e.Throw(&TypeError{
				ValidType: term.NewAtom("integer"),
				Culprit:   syntax.Serialize(e.Arena, nth),
				Location:  e.location,
			}, cont)
		}
	} else {
		return e.Throw(&TypeError{
			ValidType: term.NewAtom("compound"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}, cont)
	}

	return e.Success(cont)
}

func Univ2(_ context.Context, e *Execution, t, list, cont term.Cell) Promise {
	t, list = e.Deref(t), e.Deref(list)

	if _, ok := e.Variable(t); ok {
		return e.univVariable(t, list, cont)
	}

	f, ok := e.Functor(t)
	if !ok { // Atomic.
		return e.univAtomic(t, list, cont)
	}

	if _, err := e.canBeList(list, nil); err != nil {
		return e.Throw(err, cont)
	}

	a, err := e.PutAtom(f.Name())
	if err != nil {
		return e.Throw(err, cont)
	}

	l, err := e.PutList(slices.Collect(concat(singleton(a), e.Args(t)))...)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err = e.Unify(list, l)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func (e *Execution) univAtomic(t, list, cont term.Cell) Promise {
	if _, err := e.canBeList(list, nil); err != nil {
		return e.Throw(err, cont)
	}

	l, err := e.PutList(t)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(list, l)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func (e *Execution) univVariable(t, list, cont term.Cell) Promise {
	var elems []term.Cell
	if err := e.mustBeNonEmptyList(list, func(elem term.Cell) error {
		elem = e.Deref(elem)
		elems = append(elems, elem)
		return nil
	}); err != nil {
		return e.Throw(err, cont)
	}

	if len(elems) == 1 {
		elem := elems[0]
		if _, ok := e.Functor(elem); !ok {
			ok, err := e.Unify(t, elem)
			if err != nil {
				return e.Throw(err, cont)
			}
			if !ok {
				return Failure()
			}

			return e.Success(cont)
		}
	}

	n, ok := e.Atom(elems[0])
	if !ok {
		return Failure()
	}

	c, err := e.PutCompound(n, elems[1:]...)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err = e.Unify(t, c)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func CopyTerm2(_ context.Context, e *Execution, t1, t2, cont term.Cell) Promise {
	c, err := term.RenamedCopy(e.Arena, e.Arena, t1)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(t2, c)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func TermVariables2(_ context.Context, e *Execution, t, vars, cont term.Cell) Promise {
	t, vars = e.Deref(t), e.Deref(vars)

	if _, err := e.canBeList(vars, nil); err != nil {
		return e.Throw(err, cont)
	}

	ret, err := e.PutList(slices.Collect(e.WitnessVariables(t))...)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(ret, vars)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func Clause2(ctx context.Context, a *Activation, head, body, cont Ref) Promise {
	pi, err := a.exec.mustBeCallable(*head.cell)
	if err != nil {
		return a.Throw(err, cont)
	}

	if _, ok := a.Variable(body); !ok {
		if _, err := a.exec.mustBeCallable(*body.cell); err != nil {
			return a.Throw(err, cont)
		}
	}

	bpi := term.NewFunctor(pi.Name(), pi.Arity()+1)
	p, ok := a.exec.Predicates[bpi]
	if !ok {
		return Failure()
	}

	if !p.Public {
		f, err := a.exec.PutFunctor(pi)
		if err != nil {
			return a.Throw(err, cont)
		}

		return a.Throw(&PermissionError{
			Operation:      term.NewAtom("access"),
			PermissionType: term.NewAtom("private_procedure"),
			Culprit:        syntax.Serialize(a.exec.Arena, f),
			Location:       a.exec.location,
		}, cont)
	}

	return a.Nondet(func(yield func(Promise) bool) {
		for r, err := range a.exec.DB.Select(ctx, a.exec.Arena, pi, a.exec.CurrentTime) {
			if err != nil {
				_ = yield(a.Throw(err, cont))
				return
			}

			ok, err := a.exec.Unify(*head.cell, r.Head)
			if err != nil {
				_ = yield(a.Throw(err, cont))
				return
			}
			if !ok {
				continue
			}

			ok, err = a.exec.Unify(*body.cell, r.Body)
			if err != nil {
				_ = yield(a.Throw(err, cont))
				return
			}
			if !ok {
				continue
			}

			if !yield(a.Success(cont)) {
				return
			}
		}
	})
}

func CurrentPredicate1(_ context.Context, a *Activation, predIndicator, cont Ref) Promise {
	predIndicator = a.Deref(predIndicator)

	switch pi, ok, err := a.exec.canBePredicateIndicator(*predIndicator.cell); {
	case err != nil:
		return a.Throw(err, cont)
	case ok:
		bpi := term.NewFunctor(pi.Name(), pi.Arity()+1)
		p, _ := a.exec.Predicates[bpi]
		if p.BuiltIn {
			return Failure()
		}

		return a.Success(cont)
	}

	pis := slices.Collect(func(yield func(pi term.Functor) bool) {
		for bpi, p := range a.exec.Predicates {
			if p.BuiltIn {
				continue
			}
			pi := term.NewFunctor(bpi.Name(), bpi.Arity()-1)
			if !yield(pi) {
				return
			}
		}
	})
	slices.SortFunc(pis, func(a, b term.Functor) int {
		if o := strings.Compare(a.Name().String(), b.Name().String()); o != 0 {
			return o
		}
		return a.Arity() - b.Arity()
	})

	return a.Nondet(func(yield func(Promise) bool) {
		for _, pi := range pis {
			c, err := a.exec.PutFunctor(pi)
			if err != nil {
				_ = yield(a.Throw(err, cont))
				return
			}

			ok, err := a.Unify(predIndicator, a.ref(c))
			if err != nil {
				_ = yield(a.Throw(err, cont))
				return
			}
			if !ok {
				if !yield(Failure()) {
					return
				}
				continue
			}

			if !yield(a.Success(cont)) {
				return
			}
		}
	})
}

func AssertA1(ctx context.Context, e *Execution, t, cont term.Cell) Promise {
	return assert1(ctx, e, t, cont, db.DB.InsertBefore)
}

func AssertZ1(ctx context.Context, e *Execution, t, cont term.Cell) Promise {
	return assert1(ctx, e, t, cont, db.DB.InsertAfter)
}

func assert1(ctx context.Context, e *Execution, t, cont term.Cell, fn func(db db.DB, ctx context.Context, arena *term.Arena, record db.Record) error) Promise {
	t = e.Deref(t)

	if _, ok := e.Variable(t); ok {
		return e.Throw(&InstantiationError{
			Location: e.location,
		}, cont)
	}

	var (
		pi   term.Functor
		head term.Cell
		body term.Cell
		err  error
	)
	pi, ok := e.Functor(t, term.AllowAtom(true))
	if !ok {
		return e.Throw(&TypeError{
			ValidType: term.NewAtom("callable"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}, cont)
	}
	if pi == term.NewFunctor(term.NewAtom(":-"), 2) {
		head, body = e.Arg(t, 0), e.Arg(t, 1)
		pi, ok = e.Functor(head, term.AllowAtom(true))
		if !ok {
			return e.Throw(&TypeError{
				ValidType: term.NewAtom("callable"),
				Culprit:   syntax.Serialize(e.Arena, t),
				Location:  e.location,
			}, cont)
		}

		if _, ok := e.Functor(body, term.AllowAtom(true)); !ok {
			return e.Throw(&TypeError{
				ValidType: term.NewAtom("callable"),
				Culprit:   syntax.Serialize(e.Arena, body),
				Location:  e.location,
			}, cont)
		}
	} else {
		head = t
		body, err = e.PutAtom(term.NewAtom("true"))
		if err != nil {
			return e.Throw(err, cont)
		}
	}

	bpi := term.NewFunctor(pi.Name(), pi.Arity()+1)
	p, ok := e.Predicates[bpi]
	if !ok {
		p = wam.Predicate{
			Public:  true,
			Dynamic: true,
		}
		if e.Predicates == nil {
			e.Predicates = map[term.Functor]wam.Predicate{}
		}
		e.Predicates[bpi] = p
	}
	if !p.Dynamic {
		c, err := e.PutFunctor(pi)
		if err != nil {
			return e.Throw(err, cont)
		}
		return e.Throw(&PermissionError{
			Operation:      term.NewAtom("modify"),
			PermissionType: term.NewAtom("static_procedure"),
			Culprit:        syntax.Serialize(e.Arena, c),
			Location:       e.location,
		}, cont)
	}

	if err := fn(e.DB, ctx, e.Arena, db.Record{
		Head:      head,
		Body:      body,
		CreatedAt: e.CurrentTime,
	}); err != nil {
		return e.Throw(err, cont)
	}
	e.CurrentTime++

	return e.Success(cont)
}

func Retract1(ctx context.Context, a *Activation, t, cont Ref) Promise {
	t = a.Deref(t)

	h, err := a.PutVariable()
	if err != nil {
		return a.Throw(err, cont)
	}

	b, err := a.PutVariable()
	if err != nil {
		return a.Throw(err, cont)
	}

	c, err := a.PutCompound(atomNeck, h, b)
	if err != nil {
		return a.Throw(err, cont)
	}

	ok, err := a.Unify(c, t)
	if err != nil {
		return a.Throw(err, cont)
	}
	if !ok {
		h = t
		b, err = a.PutAtom(term.NewAtom("true"))
		if err != nil {
			return a.Throw(err, cont)
		}
	}

	h, b = a.Deref(h), a.Deref(b)

	pi, err := a.exec.mustBeCallable(*h.cell)
	if err != nil {
		return a.Throw(err, cont)
	}

	bpi := term.NewFunctor(pi.Name(), pi.Arity()+1)
	if p, ok := a.exec.Predicates[bpi]; ok && !p.Dynamic {
		c, err := a.exec.PutFunctor(pi)
		if err != nil {
			return a.Throw(err, cont)
		}
		return a.Throw(&PermissionError{
			Operation:      term.NewAtom("modify"),
			PermissionType: term.NewAtom("static_procedure"),
			Culprit:        syntax.Serialize(a.exec.Arena, c),
			Location:       a.exec.location,
		}, cont)
	}

	return a.Nondet(func(yield func(Promise) bool) {
		before := a.exec.CurrentTime
		a.exec.CurrentTime++
		for r := range a.exec.DB.Select(ctx, a.exec.Arena, pi, before) {
			ok, err := a.exec.Unify(r.Head, *h.cell)
			if err != nil {
				_ = yield(a.Throw(err, cont))
				return
			}
			if !ok {
				if !yield(Failure()) {
					return
				}
				continue
			}

			ok, err = a.exec.Unify(r.Body, *b.cell)
			if err != nil {
				_ = yield(a.Throw(err, cont))
				return
			}
			if !ok {
				if !yield(Failure()) {
					return
				}
				continue
			}

			if err := a.exec.DB.Delete(ctx, r.ID, before); err != nil {
				_ = yield(a.Throw(err, cont))
				return
			}
			if !yield(a.Success(cont)) {
				return
			}
		}
	})
}

func Abolish1(ctx context.Context, e *Execution, pred, cont term.Cell) Promise {
	pred = e.Deref(pred)

	pi, err := e.mustBePredicateIndicator(pred)
	if err != nil {
		return e.Throw(err, cont)
	}

	bpi := term.NewFunctor(pi.Name(), pi.Arity()+1)
	if p, ok := e.Predicates[bpi]; ok {
		if !p.Dynamic {
			c, err := e.PutFunctor(pi)
			if err != nil {
				return e.Throw(err, cont)
			}
			return e.Throw(&PermissionError{
				Operation:      term.NewAtom("modify"),
				PermissionType: term.NewAtom("static_procedure"),
				Culprit:        syntax.Serialize(e.Arena, c),
				Location:       e.location,
			}, cont)
		}
		for r := range e.DB.Select(ctx, e.Arena, pi, e.CurrentTime) {
			if err := e.DB.Delete(ctx, r.ID, e.CurrentTime); err != nil {
				return e.Throw(err, cont)
			}
		}
		delete(e.Predicates, bpi)
		e.CurrentTime++
	}

	return e.Success(cont)
}

func FindAll3(ctx context.Context, e *Execution, template, goal, instances, cont term.Cell) Promise {
	if _, err := e.canBeList(instances, nil); err != nil {
		return e.Throw(err, cont)
	}

	// Everything below outlives the solving, which collects.
	defer e.pin(&template, &goal, &instances, &cont)()

	var elems []term.Cell
	if err := e.FindAll(ctx, &elems, template, goal); err != nil {
		return e.rethrow(err, cont)
	}

	l, err := e.PutList(elems...)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(instances, l)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func BagOf3(ctx context.Context, e *Execution, template, goal, instances, cont term.Cell) Promise {
	return collectionOf(ctx, e, template, goal, instances, cont, func(ts []term.Cell) (term.Cell, error) {
		return e.PutList(ts...)
	})
}

func SetOf3(ctx context.Context, e *Execution, template, goal, instances, cont term.Cell) Promise {
	return collectionOf(ctx, e, template, goal, instances, cont, func(ts []term.Cell) (term.Cell, error) {
		slices.SortFunc(ts, e.Compare)
		ts = slices.CompactFunc(ts, func(a, b term.Cell) bool {
			return e.Compare(a, b) == 0
		})
		return e.PutList(ts...)
	})
}

func collectionOf(ctx context.Context, e *Execution, template, goal, instances, cont term.Cell, agg func([]term.Cell) (term.Cell, error)) Promise {
	if _, err := e.canBeList(instances, nil); err != nil {
		return e.Throw(err, cont)
	}

	fvs := e.FreeVariableSet(goal, template)
	witness, err := e.PutCompound(term.NewAtom("$witness"), fvs...)
	if err != nil {
		return e.Throw(err, cont)
	}

	template, err = e.PutCompound(term.NewAtomRune('+'), witness, template)
	if err != nil {
		return e.Throw(err, cont)
	}

	for {
		goal = e.Deref(goal)
		f, ok := e.Functor(goal)
		if !ok || f != term.NewFunctor(term.NewAtomRune('^'), 2) {
			break
		}
		goal = e.Arg(goal, 1)
	}

	unpin := e.pin(&witness, &template, &goal, &instances, &cont)

	var s []term.Cell
	if err := e.FindAll(ctx, &s, template, goal); err != nil {
		unpin()
		return e.rethrow(err, cont)
	}

	return Delay(func(yield func(Promise) bool) {
		defer unpin()

		// The solutions still to be grouped are held here and nowhere else, and
		// every yield below hands control back to the engine, which collects.
		defer e.AddRoots(func(yield func(*term.Cell) bool) {
			for i := range s {
				if !yield(&s[i]) {
					return
				}
			}
		})()

		for len(s) > 0 {
			var wt term.Cell
			wt, s = s[0], s[1:]
			w, t := e.Arg(wt, 0), e.Arg(wt, 1) // W+T
			wl, tl := []term.Cell{w}, []term.Cell{t}
			n := 0 // https://github.com/golang/go/wiki/SliceTricks#filter-in-place
			for _, t := range s {
				ww, tt := e.Arg(t, 0), e.Arg(t, 1) // WW+TT
				if e.Variant(ww, w) {
					wl = append(wl, ww)
					tl = append(tl, tt)
				} else { // keep
					s[n] = t
					n++
				}
			}
			s = s[:n]
			for _, w := range wl {
				if _, err := e.Unify(witness, w); err != nil {
					_ = yield(e.Throw(err, cont))
					return
				}
			}
			a, err := agg(tl)
			if err != nil {
				_ = yield(e.Throw(err, cont))
				return
			}
			ok, err := e.Unify(instances, a)
			if err != nil {
				_ = yield(e.Throw(err, cont))
				return
			}
			if !ok {
				if !yield(Failure()) {
					return
				}
				continue
			}

			if !yield(e.Success(cont)) {
				return
			}
		}
	})
}

func (e *Execution) FindAll(ctx context.Context, out *[]term.Cell, template term.Cell, goal term.Cell) error {
	// Resulting instances are not accessible after each run.
	// So, escape them to a secondary memory arena for a moment, then bring them back.

	// Solving the goal runs the engine, which collects; template is read again
	// after every solution and goal after the first.
	defer e.pin(&template, &goal)()

	heapTop := len(e.TempArena.Heap)
	defer func() {
		e.TempArena.Heap = e.TempArena.Heap[:heapTop]
	}()

	var instances []term.Cell
	for err := range e.Call(ctx, goal) {
		if err != nil {
			return err
		}

		c, err := term.RenamedCopy(e.Arena, e.TempArena, template)
		if err != nil {
			return err
		}
		instances = append(instances, c)
	}
	for _, t := range instances {
		c, err := term.RenamedCopy(e.TempArena, e.Arena, t)
		if err != nil {
			return err
		}
		*out = append(*out, c)
	}
	return nil
}

func CurrentInput1(_ context.Context, e *Execution, s, cont term.Cell) Promise {
	if e.Input == (term.Cell{}) {
		return Failure()
	}

	ok, err := e.Unify(s, e.Input)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func CurrentOutput1(_ context.Context, e *Execution, s, cont term.Cell) Promise {
	if e.Output == (term.Cell{}) {
		return Failure()
	}

	ok, err := e.Unify(s, e.Output)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func SetInput1(_ context.Context, e *Execution, sOrA, cont term.Cell) Promise {
	s, err := e.mustBeStreamOrAlias(sOrA)
	if err != nil {
		return e.Throw(err, cont)
	}

	for stream := range e.OpenStreams() {
		if str, _ := e.Stream(stream); str == s {
			e.Input = stream
			break
		}
	}

	return e.Success(cont)
}

func SetOutput1(_ context.Context, e *Execution, sOrA, cont term.Cell) Promise {
	s, err := e.mustBeStreamOrAlias(sOrA)
	if err != nil {
		return e.Throw(err, cont)
	}

	for stream := range e.OpenStreams() {
		if str, _ := e.Stream(stream); str == s {
			e.Output = stream
			break
		}
	}

	return e.Success(cont)
}

func Open4(_ context.Context, e *Execution, sourceSink, mode, stream, options, cont term.Cell) Promise {
	fsID, filename, err := e.mustBeSourceSink(sourceSink)
	if err != nil {
		return e.Throw(err, cont)
	}

	m, err := e.mustBeMode(mode)
	if err != nil {
		return e.Throw(err, cont)
	}

	if _, err := e.canBeStream(stream); err != nil {
		return e.Throw(err, cont)
	}

	var flag int
	switch m {
	case term.Read:
		flag = os.O_RDONLY
	case term.Write:
		flag = os.O_WRONLY | os.O_CREATE
	case term.Append:
		flag = os.O_APPEND | os.O_WRONLY | os.O_CREATE
	}

	var (
		f     fs.File
		FS, _ = e.FSs.Get(fsID)
	)
	switch FS := FS.(type) {
	case nil:
		err = fs.ErrNotExist
	case OpenFiler:
		f, err = FS.OpenFile(filename, flag, 0644)
	default:
		if flag != os.O_RDONLY {
			err = fs.ErrPermission
			break
		}
		f, err = FS.Open(filename)
	}
	switch {
	case errors.Is(err, fs.ErrNotExist):
		return e.Throw(&ExistenceError{
			ObjectType: term.NewAtom("source_sink"),
			Culprit:    syntax.Serialize(e.Arena, sourceSink),
			Location:   e.location,
		}, cont)
	case errors.Is(err, fs.ErrPermission):
		return e.Throw(&PermissionError{
			Operation:      term.NewAtom("open"),
			PermissionType: term.NewAtom("source_sink"),
			Culprit:        syntax.Serialize(e.Arena, sourceSink),
			Location:       e.location,
		}, cont)
	case err != nil:
		return e.Throw(err, cont)
	}

	s := term.Stream{Mode: m}
	switch m {
	case term.Read:
		f, ok := f.(io.Reader)
		if !ok {
			return Error(errors.New("f does not implement io.Reader"))
		}
		s.Source = f
		if err := s.InitRead(); err != nil {
			return e.Throw(err, cont)
		}
	case term.Write, term.Append:
		f, ok := f.(io.Writer)
		if !ok {
			return Error(errors.New("f does not implement io.Writer"))
		}
		s.Sink = f
	}

	if fi, err := f.Stat(); err == nil {
		s.Reposition = fi.Mode()&fs.ModeType == 0
	}

	if err := e.MustBeList(options, func(elem term.Cell) error {
		return e.handleStreamOption(&s, elem)
	}); err != nil {
		return e.Throw(err, cont)
	}

	t, err := e.PutStream(s)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(stream, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func (e *Execution) handleStreamOption(s *term.Stream, o term.Cell) error {
	o = e.Deref(o)

	if _, ok := e.Variable(o); ok {
		return &InstantiationError{
			Location: e.location,
		}
	}

	switch f, _ := e.Functor(o); f {
	case term.NewFunctor(term.NewAtom("alias"), 1):
		return e.handleStreamOptionAlias(s, o)
	case term.NewFunctor(term.NewAtom("type"), 1):
		return e.handleStreamOptionType(s, o)
	case term.NewFunctor(term.NewAtom("reposition"), 1):
		return e.handleStreamOptionReposition(s, o)
	case term.NewFunctor(term.NewAtom("eof_action"), 1):
		return e.handleStreamOptionEOFAction(s, o)
	default:
		return &DomainError{
			ValidDomain: term.NewAtom("stream_option"),
			Culprit:     syntax.Serialize(e.Arena, o),
			Location:    e.location,
		}
	}
}

func (e *Execution) handleStreamOptionAlias(s *term.Stream, o term.Cell) error {
	alias := e.Arg(o, 0)
	alias = e.Deref(alias)

	if _, ok := e.Variable(alias); ok {
		return &InstantiationError{
			Location: e.location,
		}
	}

	a, ok := e.Atom(alias)
	if !ok {
		return &DomainError{
			ValidDomain: term.NewAtom("stream_option"),
			Culprit:     syntax.Serialize(e.Arena, o),
			Location:    e.location,
		}
	}
	i := -1
	for j, s := range e.Streams.All() {
		if s.Alias == a {
			i = j
			break
		}
	}
	if i >= 0 {
		return &PermissionError{
			Operation:      term.NewAtom("open"),
			PermissionType: term.NewAtom("source_sink"),
			Culprit:        syntax.Serialize(e.Arena, o),
			Location:       e.location,
		}
	}
	s.Alias = a
	return nil
}

func (e *Execution) handleStreamOptionType(s *term.Stream, o term.Cell) error {
	t := e.Arg(o, 0)
	t = e.Deref(t)

	if _, ok := e.Variable(t); ok {
		return &InstantiationError{
			Location: e.location,
		}
	}

	switch a, _ := e.Atom(t); a {
	case term.NewAtom("text"):
		s.StreamType = term.Text
		return nil
	case term.NewAtom("binary"):
		s.StreamType = term.Binary
		return nil
	default:
		return &DomainError{
			ValidDomain: term.NewAtom("stream_option"),
			Culprit:     syntax.Serialize(e.Arena, o),
			Location:    e.location,
		}
	}
}

func (e *Execution) handleStreamOptionReposition(s *term.Stream, o term.Cell) error {
	r := e.Arg(o, 0)
	r = e.Deref(r)

	if _, ok := e.Variable(r); ok {
		return &InstantiationError{
			Location: e.location,
		}
	}

	switch a, _ := e.Atom(r); a {
	case term.NewAtom("true"):
		s.Reposition = true
		return nil
	case term.NewAtom("false"):
		s.Reposition = false
		return nil
	default:
		return &DomainError{
			ValidDomain: term.NewAtom("stream_option"),
			Culprit:     syntax.Serialize(e.Arena, o),
			Location:    e.location,
		}
	}
}

func (e *Execution) handleStreamOptionEOFAction(s *term.Stream, o term.Cell) error {
	action := e.Arg(o, 0)
	action = e.Deref(action)

	if _, ok := e.Variable(action); ok {
		return &InstantiationError{
			Location: e.location,
		}
	}

	switch a, _ := e.Atom(action); a {
	case term.NewAtom("error"):
		s.EOFAction = term.Error
		return nil
	case term.NewAtom("eof_code"):
		s.EOFAction = term.EOFCode
		return nil
	case term.NewAtom("reset"):
		s.EOFAction = term.Reset
		return nil
	default:
		return &DomainError{
			ValidDomain: term.NewAtom("stream_option"),
			Culprit:     syntax.Serialize(e.Arena, o),
			Location:    e.location,
		}
	}
}

func Close2(_ context.Context, e *Execution, sOrA, options, cont term.Cell) Promise {
	s, err := e.mustBeStreamOrAlias(sOrA)
	if err != nil {
		return e.Throw(err, cont)
	}

	var force bool
	if err := e.MustBeList(options, func(o term.Cell) error {
		o = e.Deref(o)

		if _, ok := e.Variable(o); ok {
			return &InstantiationError{
				Location: e.location,
			}
		}

		switch f, _ := e.Functor(o); f {
		case term.NewFunctor(term.NewAtom("force"), 1):
			b := e.Arg(o, 0)
			b = e.Deref(b)

			switch b, _ := e.Atom(b); b {
			case term.NewAtom("true"):
				force = true
				return nil
			case term.NewAtom("false"):
				force = false
				return nil
			}
			fallthrough
		default:
			return &DomainError{
				ValidDomain: term.NewAtom("close_option"),
				Culprit:     syntax.Serialize(e.Arena, o),
				Location:    e.location,
			}
		}
	}); err != nil {
		return e.Throw(err, cont)
	}

	if err := s.Close(); err != nil && !force {
		return e.Throw(err, cont)
	}

	return e.Success(cont)
}

func FlushOutput1(_ context.Context, e *Execution, sOrA, cont term.Cell) Promise {
	s, err := e.mustBeStreamOrAlias(sOrA)
	if err != nil {
		return e.Throw(err, cont)
	}

	switch err := s.Flush(); {
	case errors.Is(err, term.ErrWrongIOMode):
		return e.Throw(&PermissionError{
			Operation:      term.NewAtom("operation"),
			PermissionType: term.NewAtom("stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		}, cont)
	case err != nil:
		return e.Throw(err, cont)
	}

	return e.Success(cont)
}

func StreamProperty2(_ context.Context, a *Activation, stream, property, cont Ref) Promise {
	stream = a.Deref(stream)

	var streams iter.Seq[term.Cell]
	s, err := a.exec.canBeStream(*stream.cell)
	if err != nil {
		return a.Throw(err, cont)
	}
	if s == nil {
		streams = a.exec.OpenStreams()
	} else {
		streams = singleton(*stream.cell)
	}

	if err := a.exec.canBeStreamProperty(*property.cell); err != nil {
		return a.Throw(err, cont)
	}

	return a.Nondet(func(yield func(Promise) bool) {
		for s := range streams {
			st, ok := a.exec.Stream(s)
			if !ok {
				continue
			}
			for p, err := range a.exec.properties(st) {
				if err != nil {
					_ = yield(a.Throw(err, cont))
					return
				}

				ok, err := a.exec.Unify(*stream.cell, s)
				if err != nil {
					_ = yield(a.Throw(err, cont))
					return
				}
				if !ok {
					if !yield(Failure()) {
						return
					}
					continue
				}

				ok, err = a.exec.Unify(*property.cell, p)
				if err != nil {
					_ = yield(a.Throw(err, cont))
					return
				}
				if !ok {
					if !yield(Failure()) {
						return
					}
					continue
				}

				if !yield(a.Success(cont)) {
					return
				}
			}
		}
	})
}

func (e *Execution) properties(s *term.Stream) iter.Seq2[term.Cell, error] {
	return func(yield func(term.Cell, error) bool) {
		if n := s.Name(); n != "" {
			n, err := e.PutAtom(term.NewAtom(n))
			if err != nil {
				_ = yield(term.Cell{}, err)
				return
			}
			c, err := e.PutCompound(term.NewAtom("file_name"), n)
			if err != nil {
				_ = yield(term.Cell{}, err)
				return
			}
			if !yield(c, nil) {
				return
			}
		}

		m, err := e.PutAtom(term.NewAtom(s.Mode.String()))
		if err != nil {
			_ = yield(term.Cell{}, err)
			return
		}
		c, err := e.PutCompound(term.NewAtom("mode"), m)
		if err != nil {
			_ = yield(term.Cell{}, err)
			return
		}
		if !yield(c, nil) {
			return
		}

		switch s.Mode {
		case term.Read:
			a, err := e.PutAtom(term.NewAtom("input"))
			if err != nil {
				_ = yield(term.Cell{}, err)
				return
			}
			if !yield(a, nil) {
				return
			}
		case term.Write, term.Append:
			a, err := e.PutAtom(term.NewAtom("output"))
			if err != nil {
				_ = yield(term.Cell{}, err)
				return
			}
			if !yield(a, nil) {
				return
			}
		}

		if s.Alias != (term.Atom{}) {
			a, err := e.PutAtom(s.Alias)
			if err != nil {
				_ = yield(term.Cell{}, err)
				return
			}
			c, err := e.PutCompound(term.NewAtom("alias"), a)
			if err != nil {
				_ = yield(term.Cell{}, err)
				return
			}
			if !yield(c, nil) {
				return
			}
		}

		{
			p, err := e.PutInteger(s.Position)
			if err != nil {
				_ = yield(term.Cell{}, err)
				return
			}
			c, err := e.PutCompound(term.NewAtom("position"), p)
			if err != nil {
				_ = yield(term.Cell{}, err)
				return
			}
			if !yield(c, nil) {
				return
			}
		}

		{
			eos, err := e.PutAtom(term.NewAtom(s.EndOfStream.String()))
			if err != nil {
				_ = yield(term.Cell{}, err)
				return
			}
			c, err := e.PutCompound(term.NewAtom("end_of_stream"), eos)
			if err != nil {
				_ = yield(term.Cell{}, err)
				return
			}
			if !yield(c, nil) {
				return
			}
		}

		{
			a, err := e.PutAtom(term.NewAtom(s.EOFAction.String()))
			if err != nil {
				_ = yield(term.Cell{}, err)
				return
			}
			c, err := e.PutCompound(term.NewAtom("eof_action"), a)
			if err != nil {
				_ = yield(term.Cell{}, err)
				return
			}
			if !yield(c, nil) {
				return
			}
		}

		var t term.Cell
		if s.Reposition {
			t, err = e.PutAtom(term.NewAtom("true"))
		} else {
			t, err = e.PutAtom(term.NewAtom("false"))
		}
		if err != nil {
			_ = yield(term.Cell{}, err)
			return
		}
		c, err = e.PutCompound(term.NewAtom("reposition"), t)
		if err != nil {
			_ = yield(term.Cell{}, err)
			return
		}
		if !yield(c, nil) {
			return
		}

		t, err = e.PutAtom(term.NewAtom(s.StreamType.String()))
		if err != nil {
			_ = yield(term.Cell{}, err)
			return
		}
		c, err = e.PutCompound(term.NewAtom("type"), t)
		if err != nil {
			_ = yield(term.Cell{}, err)
			return
		}
		if !yield(c, nil) {
			return
		}
	}
}

func SetStreamPosition2(_ context.Context, e *Execution, sOrA, position, cont term.Cell) Promise {
	s, err := e.mustBeStreamOrAlias(sOrA)
	if err != nil {
		return e.Throw(err, cont)
	}

	p, err := e.MustBeInteger(position)
	if err != nil {
		return e.Throw(err, cont)
	}

	switch _, err := s.Seek(p, 0); {
	case errors.Is(err, term.ErrReposition):
		return e.Throw(&PermissionError{
			Operation:      term.NewAtom("reposition"),
			PermissionType: term.NewAtom("stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		}, cont)
	case err != nil:
		return e.Throw(err, cont)
	default:
		return e.Success(cont)
	}
}

func GetChar2(_ context.Context, e *Execution, sOrA, inChar, cont term.Cell) Promise {
	s, err := e.mustBeStreamOrAlias(sOrA)
	if err != nil {
		return e.Throw(err, cont)
	}

	if _, _, err := e.canBeInChar(inChar); err != nil {
		return e.Throw(err, cont)
	}

	var c term.Cell
	switch r, _, err := s.ReadRune(); {
	case errors.Is(err, io.EOF):
		c, err = e.PutAtom(term.NewAtom("end_of_file"))
		if err != nil {
			return e.Throw(err, cont)
		}
	case errors.Is(err, term.ErrWrongIOMode):
		return e.Throw(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		}, cont)
	case errors.Is(err, term.ErrWrongStreamType):
		return e.Throw(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("binary_stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		}, cont)
	case errors.Is(err, term.ErrPastEndOfStream):
		return e.Throw(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("past_end_of_stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		}, cont)
	case err != nil:
		return e.Throw(err, cont)
	default:
		c, err = e.PutAtom(term.NewAtomRune(r))
		if err != nil {
			return e.Throw(err, cont)
		}
	}

	ok, err := e.Unify(inChar, c)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func GetCode2(_ context.Context, e *Execution, sOrA, inCharCode, cont term.Cell) Promise {
	s, err := e.mustBeStreamOrAlias(sOrA)
	if err != nil {
		return e.Throw(err, cont)
	}

	if _, _, err := e.canBeInCharCode(inCharCode); err != nil {
		return e.Throw(err, cont)
	}

	var c term.Cell
	switch r, _, err := s.ReadRune(); {
	case errors.Is(err, io.EOF):
		c, err = e.PutInteger(-1)
		if err != nil {
			return e.Throw(err, cont)
		}
	case errors.Is(err, term.ErrWrongIOMode):
		return e.Throw(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		}, cont)
	case errors.Is(err, term.ErrWrongStreamType):
		return e.Throw(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("binary_stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		}, cont)
	case errors.Is(err, term.ErrPastEndOfStream):
		return e.Throw(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("past_end_of_stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		}, cont)
	case err != nil:
		return e.Throw(err, cont)
	default:
		c, err = e.PutInteger(int64(r))
		if err != nil {
			return e.Throw(err, cont)
		}
	}

	ok, err := e.Unify(inCharCode, c)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func PeekChar2(_ context.Context, e *Execution, sOrA, inChar, cont term.Cell) Promise {
	s, err := e.mustBeStreamOrAlias(sOrA)
	if err != nil {
		return e.Throw(err, cont)
	}

	if _, _, err := e.canBeInChar(inChar); err != nil {
		return e.Throw(err, cont)
	}

	var c term.Cell
	switch r, _, err := s.ReadRune(); {
	case errors.Is(err, io.EOF):
		c, err = e.PutAtom(term.NewAtom("end_of_file"))
		if err != nil {
			return e.Throw(err, cont)
		}
	case errors.Is(err, term.ErrWrongIOMode):
		return e.Throw(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		}, cont)
	case errors.Is(err, term.ErrWrongStreamType):
		return e.Throw(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("binary_stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		}, cont)
	case errors.Is(err, term.ErrPastEndOfStream):
		return e.Throw(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("past_end_of_stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		}, cont)
	case err != nil:
		return e.Throw(err, cont)
	default:
		if err := s.UnreadRune(); err != nil {
			return e.Throw(err, cont)
		}

		if r == unicode.ReplacementChar {
			return Error(&RepresentationError{
				Flag:     term.NewAtom("character"),
				Location: e.location,
			})
		}

		c, err = e.PutAtom(term.NewAtomRune(r))
		if err != nil {
			return e.Throw(err, cont)
		}
	}

	ok, err := e.Unify(inChar, c)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func PeekCode2(_ context.Context, e *Execution, sOrA, inCharCode, cont term.Cell) Promise {
	s, err := e.mustBeStreamOrAlias(sOrA)
	if err != nil {
		return e.Throw(err, cont)
	}

	if _, _, err := e.canBeInCharCode(inCharCode); err != nil {
		return e.Throw(err, cont)
	}

	var c term.Cell
	switch r, _, err := s.ReadRune(); {
	case errors.Is(err, io.EOF):
		c, err = e.PutInteger(-1)
		if err != nil {
			return e.Throw(err, cont)
		}
	case errors.Is(err, term.ErrWrongIOMode):
		return e.Throw(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		}, cont)
	case errors.Is(err, term.ErrWrongStreamType):
		return e.Throw(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("binary_stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		}, cont)
	case errors.Is(err, term.ErrPastEndOfStream):
		return e.Throw(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("past_end_of_stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		}, cont)
	case err != nil:
		return e.Throw(err, cont)
	default:
		if err := s.UnreadRune(); err != nil {
			return e.Throw(err, cont)
		}

		if r == unicode.ReplacementChar {
			return e.Throw(&RepresentationError{
				Flag:     term.NewAtom("in_character_code"),
				Location: e.location,
			}, cont)
		}

		c, err = e.PutInteger(int64(r))
		if err != nil {
			return e.Throw(err, cont)
		}
	}

	ok, err := e.Unify(inCharCode, c)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func PutChar2(_ context.Context, e *Execution, sOrA, char, cont term.Cell) Promise {
	s, err := e.mustBeStreamOrAlias(sOrA)
	if err != nil {
		return e.Throw(err, cont)
	}

	r, err := e.MustBeChar(char)
	if err != nil {
		return e.Throw(err, cont)
	}

	switch _, err := s.WriteRune(r); {
	case errors.Is(err, term.ErrWrongIOMode):
		return e.Throw(&PermissionError{
			Operation:      term.NewAtom("output"),
			PermissionType: term.NewAtom("stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		}, cont)
	case errors.Is(err, term.ErrWrongStreamType):
		return e.Throw(&PermissionError{
			Operation:      term.NewAtom("output"),
			PermissionType: term.NewAtom("binary_stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		}, cont)
	case err != nil:
		return e.Throw(err, cont)
	}

	return e.Success(cont)
}

func PutCode2(_ context.Context, e *Execution, sOrA, code, cont term.Cell) Promise {
	s, err := e.mustBeStreamOrAlias(sOrA)
	if err != nil {
		return e.Throw(err, cont)
	}

	r, err := e.mustBeCharCode(code)
	if err != nil {
		return e.Throw(err, cont)
	}

	switch _, err := s.WriteRune(r); {
	case errors.Is(err, term.ErrWrongIOMode):
		return e.Throw(&PermissionError{
			Operation:      term.NewAtom("output"),
			PermissionType: term.NewAtom("stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		}, cont)
	case errors.Is(err, term.ErrWrongStreamType):
		return e.Throw(&PermissionError{
			Operation:      term.NewAtom("output"),
			PermissionType: term.NewAtom("binary_stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		}, cont)
	case err != nil:
		return e.Throw(err, cont)
	}

	return e.Success(cont)
}

func GetByte2(_ context.Context, e *Execution, sOrA, inByte, cont term.Cell) Promise {
	s, err := e.mustBeStreamOrAlias(sOrA)
	if err != nil {
		return e.Throw(err, cont)
	}

	if _, _, err := e.canBeInByte(inByte); err != nil {
		return e.Throw(err, cont)
	}

	var n int64
	switch b, err := s.ReadByte(); {
	case errors.Is(err, io.EOF):
		n = -1
	case errors.Is(err, term.ErrWrongIOMode):
		return e.Throw(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		}, cont)
	case errors.Is(err, term.ErrWrongStreamType):
		return e.Throw(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("text_stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		}, cont)
	case errors.Is(err, term.ErrPastEndOfStream):
		return e.Throw(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("past_end_of_stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		}, cont)
	case err != nil:
		return e.Throw(err, cont)
	default:
		n = int64(b)
	}

	i, err := e.PutInteger(n)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(inByte, i)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func PeekByte2(_ context.Context, e *Execution, sOrA, inByte, cont term.Cell) Promise {
	s, err := e.mustBeStreamOrAlias(sOrA)
	if err != nil {
		return e.Throw(err, cont)
	}

	if _, _, err := e.canBeInByte(inByte); err != nil {
		return e.Throw(err, cont)
	}

	var n int64
	switch b, err := s.ReadByte(); {
	case errors.Is(err, io.EOF):
		n = -1
	case errors.Is(err, term.ErrWrongIOMode):
		return e.Throw(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		}, cont)
	case errors.Is(err, term.ErrWrongStreamType):
		return e.Throw(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("text_stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		}, cont)
	case errors.Is(err, term.ErrPastEndOfStream):
		return e.Throw(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("past_end_of_stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		}, cont)
	case err != nil:
		return e.Throw(err, cont)
	default:
		if err := s.UnreadByte(); err != nil {
			return e.Throw(err, cont)
		}

		n = int64(b)
	}

	i, err := e.PutInteger(n)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(inByte, i)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func PutByte2(_ context.Context, e *Execution, sOrA, byt, cont term.Cell) Promise {
	s, err := e.mustBeStreamOrAlias(sOrA)
	if err != nil {
		return e.Throw(err, cont)
	}

	b, err := e.mustBeByte(byt)
	if err != nil {
		return e.Throw(err, cont)
	}

	switch err := s.WriteByte(b); {
	case errors.Is(err, term.ErrWrongIOMode):
		return e.Throw(&PermissionError{
			Operation:      term.NewAtom("output"),
			PermissionType: term.NewAtom("stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		}, cont)
	case errors.Is(err, term.ErrWrongStreamType):
		return e.Throw(&PermissionError{
			Operation:      term.NewAtom("output"),
			PermissionType: term.NewAtom("text_stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		}, cont)
	case err != nil:
		return e.Throw(err, cont)
	default:
		return e.Success(cont)
	}
}

func ReadTerm3(_ context.Context, e *Execution, sOrA, t, options, cont term.Cell) Promise {
	s, err := e.mustBeStreamOrAlias(sOrA)
	if err != nil {
		return e.Throw(err, cont)
	}

	var opts readTermOptions
	if err := e.MustBeList(options, func(elem term.Cell) error {
		return e.readTermOption(&opts, elem)
	}); err != nil {
		return e.Throw(err, cont)
	}

	var (
		vars                 []term.VariableName
		unexpectedTokenError *syntax.UnexpectedTokenError
	)
	p, err := syntax.ParseTerm(s,
		syntax.Arena(e.Arena),
		syntax.DoubleQuote(&e.DoubleQuotes),
		syntax.Operators(&e.Ops),
		syntax.VariableNames(&vars),
		syntax.CharConv(&e.CharConversion),
	)
	switch {
	case errors.Is(err, io.EOF):
		eof, err := e.PutAtom(term.NewAtom("end_of_file"))
		if err != nil {
			return e.Throw(err, cont)
		}

		ok, err := e.Unify(t, eof)
		if err != nil {
			return e.Throw(err, cont)
		}
		if !ok {
			return Failure()
		}
	case errors.Is(err, term.ErrWrongIOMode):
		return e.Throw(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		}, cont)
	case errors.Is(err, term.ErrWrongStreamType):
		return e.Throw(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("text_stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		}, cont)
	case errors.Is(err, term.ErrPastEndOfStream):
		return e.Throw(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("past_end_of_stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		}, cont)
	case errors.As(err, &unexpectedTokenError), errors.Is(err, syntax.ErrUnexpectedEOF):
		return e.Throw(&SyntaxError{
			ImpDepAtom: term.NewAtom(err.Error()),
			Location:   e.location,
		}, cont)
	case err != nil:
		return e.Throw(err, cont)
	default:
		var (
			singletons    []term.Cell
			variables     []term.Cell
			variableNames []term.Cell
		)
		for _, v := range vars {
			if opts.singletons != (term.Cell{}) && v.Count == 1 && v.Name != "_" {
				singletons = append(singletons, v.Variable)
			}
			if opts.variables != (term.Cell{}) {
				variables = append(variables, v.Variable)
			}
			if opts.variableNames != (term.Cell{}) && v.Name != "_" {
				n, err := e.PutAtom(term.NewAtom(v.Name))
				if err != nil {
					return e.Throw(err, cont)
				}
				c, err := e.PutCompound(term.NewAtomRune('='), n, v.Variable)
				if err != nil {
					return e.Throw(err, cont)
				}
				variableNames = append(variableNames, c)
			}
		}

		ok, err := e.Unify(t, p)
		if err != nil {
			return e.Throw(err, cont)
		}
		if !ok {
			return Failure()
		}

		if opts.singletons != (term.Cell{}) {
			l, err := e.PutList(singletons...)
			if err != nil {
				return e.Throw(err, cont)
			}

			ok, err := e.Unify(opts.singletons, l)
			if err != nil {
				return e.Throw(err, cont)
			}
			if !ok {
				return Failure()
			}
		}

		if opts.variables != (term.Cell{}) {
			l, err := e.PutList(variables...)
			if err != nil {
				return e.Throw(err, cont)
			}

			ok, err := e.Unify(opts.variables, l)
			if err != nil {
				return e.Throw(err, cont)
			}
			if !ok {
				return Failure()
			}
		}

		if opts.variableNames != (term.Cell{}) {
			l, err := e.PutList(variableNames...)
			if err != nil {
				return e.Throw(err, cont)
			}

			ok, err := e.Unify(opts.variableNames, l)
			if err != nil {
				return e.Throw(err, cont)
			}
			if !ok {
				return Failure()
			}
		}
	}

	return e.Success(cont)
}

type readTermOptions struct {
	singletons    term.Cell
	variables     term.Cell
	variableNames term.Cell
}

func (e *Execution) readTermOption(opts *readTermOptions, option term.Cell) error {
	option = e.Deref(option)

	if _, ok := e.Variable(option); ok {
		return &InstantiationError{
			Location: e.location,
		}
	}

	switch f, _ := e.Functor(option); f {
	case term.NewFunctor(term.NewAtom("singletons"), 1):
		opts.singletons = e.Arg(option, 0)
	case term.NewFunctor(term.NewAtom("variables"), 1):
		opts.variables = e.Arg(option, 0)
	case term.NewFunctor(term.NewAtom("variable_names"), 1):
		opts.variableNames = e.Arg(option, 0)
	default:
		return &DomainError{
			ValidDomain: term.NewAtom("read_option"),
			Culprit:     syntax.Serialize(e.Arena, option),
			Location:    e.location,
		}
	}
	return nil
}

func WriteTerm3(_ context.Context, e *Execution, sOrA, t, options, cont term.Cell) Promise {
	s, err := e.mustBeStreamOrAlias(sOrA)
	if err != nil {
		return e.Throw(err, cont)
	}

	formatter := syntax.Formatter{
		Arena: e.Arena,
		Term:  t,
		Ops:   &e.Ops,
	}
	if err := e.MustBeList(options, func(o term.Cell) error {
		o = e.Deref(o)

		if _, ok := e.Variable(o); ok {
			return &InstantiationError{
				Location: e.location,
			}
		}

		switch f, _ := e.Functor(o); f {
		case term.NewFunctor(term.NewAtom("quoted"), 1):
			return e.writeTermOptionBool(&formatter.Quoted, o)
		case term.NewFunctor(term.NewAtom("ignore_ops"), 1):
			return e.writeTermOptionBool(&formatter.IgnoreOps, o)
		case term.NewFunctor(term.NewAtom("numbervars"), 1):
			return e.writeTermOptionBool(&formatter.NumberVars, o)
		case term.NewFunctor(term.NewAtom("variable_names"), 1):
			return e.writeTermOptionVariableNames(&formatter.VariableNames, o)
		case term.NewFunctor(term.NewAtom("max_depth"), 1):
			return e.writeTermOptionInteger(&formatter.MaxDepth, o)
		}
		return &DomainError{
			ValidDomain: term.NewAtom("write_option"),
			Culprit:     syntax.Serialize(e.Arena, o),
			Location:    e.location,
		}
	}); err != nil {
		return e.Throw(err, cont)
	}

	w, err := s.TextWriter()
	switch {
	case errors.Is(err, term.ErrWrongIOMode):
		return e.Throw(&PermissionError{
			Operation:      term.NewAtom("output"),
			PermissionType: term.NewAtom("stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		}, cont)
	case errors.Is(err, term.ErrWrongStreamType):
		return e.Throw(&PermissionError{
			Operation:      term.NewAtom("output"),
			PermissionType: term.NewAtom("binary_stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		}, cont)
	case err != nil:
		return e.Throw(err, cont)
	}

	if _, err := fmt.Fprintf(w, "%s", &formatter); err != nil {
		return e.Throw(err, cont)
	}

	return e.Success(cont)
}

func (e *Execution) writeTermOptionBool(out *bool, o term.Cell) error {
	switch b, _ := e.Atom(e.Arg(o, 0)); b {
	case term.NewAtom("true"):
		*out = true
	case term.NewAtom("false"):
		*out = false
	default:
		return &DomainError{
			ValidDomain: term.NewAtom("write_option"),
			Culprit:     syntax.Serialize(e.Arena, o),
			Location:    e.location,
		}
	}
	return nil
}

func (e *Execution) writeTermOptionVariableNames(out *[]term.VariableName, o term.Cell) error {
	return e.MustBeList(e.Arg(o, 0), func(vn term.Cell) error {
		vn = e.Deref(vn)

		if _, ok := e.Variable(vn); ok {
			return &InstantiationError{
				Location: e.location,
			}
		}

		switch f, _ := e.Functor(vn); f {
		case term.NewFunctor(term.NewAtom("="), 2):
			v, n := e.Arg(vn, 0), e.Arg(vn, 1)
			v, n = e.Deref(v), e.Deref(n)

			if _, ok := e.Variable(v); !ok {
				return &DomainError{
					ValidDomain: term.NewAtom("write_option"),
					Culprit:     syntax.Serialize(e.Arena, o),
					Location:    e.location,
				}
			}

			a, ok := e.Atom(n)
			if !ok {
				return &DomainError{
					ValidDomain: term.NewAtom("write_option"),
					Culprit:     syntax.Serialize(e.Arena, o),
					Location:    e.location,
				}
			}

			*out = append(*out, term.VariableName{
				Variable: v,
				Name:     a.String(),
			})
			return nil
		default:
			return &DomainError{
				ValidDomain: term.NewAtom("write_option"),
				Culprit:     syntax.Serialize(e.Arena, o),
				Location:    e.location,
			}
		}
	})
}

func (e *Execution) writeTermOptionInteger(out *int, o term.Cell) error {
	n, ok := e.Integer(e.Arg(o, 0))
	if !ok {
		return &DomainError{
			ValidDomain: term.NewAtom("write_option"),
			Culprit:     syntax.Serialize(e.Arena, o),
			Location:    e.location,
		}
	}
	*out = int(n)
	return nil
}

func Op3(_ context.Context, e *Execution, priority, operatorSpecifier, operator, cont term.Cell) Promise {
	priority, operatorSpecifier, operator = e.Deref(priority), e.Deref(operatorSpecifier), e.Deref(operator)

	p, err := e.MustBeInteger(priority)
	if err != nil {
		return e.Throw(err, cont)
	}
	if p < 0 || p > 1200 {
		return e.Throw(&DomainError{
			ValidDomain: term.NewAtom("operator_priority"),
			Culprit:     syntax.Serialize(e.Arena, priority),
			Location:    e.location,
		}, cont)
	}

	opSpec, err := e.MustBeAtom(operatorSpecifier)
	if err != nil {
		return e.Throw(err, cont)
	}

	var spec syntax.OperatorSpecifier
	switch opSpec {
	case term.NewAtom("fx"):
		spec = syntax.FX
	case term.NewAtom("fy"):
		spec = syntax.FY
	case term.NewAtom("xf"):
		spec = syntax.XF
	case term.NewAtom("yf"):
		spec = syntax.YF
	case term.NewAtom("xfx"):
		spec = syntax.XFX
	case term.NewAtom("xfy"):
		spec = syntax.XFY
	case term.NewAtom("yfx"):
		spec = syntax.YFX
	default:
		return e.Throw(&DomainError{
			ValidDomain: term.NewAtom("operator_specifier"),
			Culprit:     syntax.Serialize(e.Arena, operatorSpecifier),
			Location:    e.location,
		}, cont)
	}

	var ops []term.Atom
	if a, ok := e.Atom(operator); ok {
		if err := e.validateOp(p, spec, operator); err != nil {
			return e.Throw(err, cont)
		}
		ops = append(ops, a)
	} else {
		if err := e.MustBeList(operator, func(elem term.Cell) error {
			a, err := e.MustBeAtom(elem)
			if err != nil {
				return err
			}

			if err := e.validateOp(p, spec, operator); err != nil {
				return err
			}

			if slices.Contains(ops, a) {
				return nil
			}
			ops = append(ops, a)

			return nil
		}); err != nil {
			return e.Throw(err, cont)
		}
	}

	for _, op := range ops {
		e.Ops.Undefine(op, spec.Class())
		e.Ops.Define(int16(p), spec, op)
	}

	return e.Success(cont)
}

func (e *Execution) validateOp(p int64, spec syntax.OperatorSpecifier, op term.Cell) error {
	name, _ := e.Atom(op)

	switch name {
	case term.NewAtomRune('.'):
		if _, ok := e.Ops.DefinedIn(name, syntax.Infix); ok {
			return &PermissionError{
				Operation:      term.NewAtom("modify"),
				PermissionType: term.NewAtom("operator"),
				Culprit:        syntax.Serialize(e.Arena, op),
				Location:       e.location,
			}
		}
	case term.NewAtomRune('|'):
		if spec.Class() != syntax.Infix || (p > 0 && p < 1001) {
			operation := term.NewAtom("create")
			if _, ok := e.Ops.DefinedIn(name, syntax.Infix); ok {
				operation = term.NewAtom("modify")
			}
			return &PermissionError{
				Operation:      operation,
				PermissionType: term.NewAtom("operator"),
				Culprit:        syntax.Serialize(e.Arena, op),
				Location:       e.location,
			}
		}
	case term.NewAtom("{}"), term.NewAtom("[]"):
		return &PermissionError{
			Operation:      term.NewAtom("create"),
			PermissionType: term.NewAtom("operator"),
			Culprit:        syntax.Serialize(e.Arena, op),
			Location:       e.location,
		}
	}

	// 6.3.4.3 There shall not be an infix and a postfix Operator with the same name.
	switch spec.Class() {
	case syntax.Infix:
		if _, ok := e.Ops.DefinedIn(name, syntax.Postfix); ok {
			return &PermissionError{
				Operation:      term.NewAtom("create"),
				PermissionType: term.NewAtom("operator"),
				Culprit:        syntax.Serialize(e.Arena, op),
				Location:       e.location,
			}
		}
	case syntax.Postfix:
		if _, ok := e.Ops.DefinedIn(name, syntax.Infix); ok {
			return &PermissionError{
				Operation:      term.NewAtom("create"),
				PermissionType: term.NewAtom("operator"),
				Culprit:        syntax.Serialize(e.Arena, op),
				Location:       e.location,
			}
		}
	}

	return nil
}

func CurrentOp3(_ context.Context, a *Activation, priority, operatorSpecifier, operator, cont Ref) Promise {
	priority, operatorSpecifier, operator = a.Deref(priority), a.Deref(operatorSpecifier), a.Deref(operator)

	switch p, ok, err := a.exec.canBeInteger(*priority.cell); {
	case err != nil:
		return a.Throw(err, cont)
	case ok && (p < 0 || p > 1200):
		return a.Throw(&DomainError{
			ValidDomain: term.NewAtom("operator_priority"),
			Culprit:     syntax.Serialize(a.exec.Arena, *priority.cell),
			Location:    a.exec.location,
		}, cont)
	}

	switch s, ok, err := a.exec.canBeAtom(*operatorSpecifier.cell); {
	case err != nil:
		return a.Throw(err, cont)
	case ok && !slices.Contains([]term.Atom{
		term.NewAtom("fx"),
		term.NewAtom("fy"),
		term.NewAtom("xf"),
		term.NewAtom("yf"),
		term.NewAtom("xfx"),
		term.NewAtom("xfy"),
		term.NewAtom("yfx"),
	}, s):
		return a.Throw(&DomainError{
			ValidDomain: term.NewAtom("operator_specifier"),
			Culprit:     syntax.Serialize(a.exec.Arena, *operatorSpecifier.cell),
			Location:    a.exec.location,
		}, cont)
	}

	switch _, _, err := a.exec.canBeAtom(*operator.cell); {
	case err != nil:
		return a.Throw(err, cont)
	}

	return a.Nondet(func(yield func(Promise) bool) {
		for _, op := range a.exec.Ops {
			p, err := a.PutInteger(int64(op.Priority))
			if err != nil {
				_ = yield(a.Throw(err, cont))
				return
			}

			ok, err := a.Unify(priority, p)
			if err != nil {
				_ = yield(a.Throw(err, cont))
				return
			}
			if !ok {
				if !yield(Failure()) {
					return
				}
				continue
			}

			s, err := a.PutAtom(term.NewAtom(op.Specifier.String()))
			if err != nil {
				_ = yield(a.Throw(err, cont))
				return
			}

			ok, err = a.Unify(operatorSpecifier, s)
			if err != nil {
				_ = yield(a.Throw(err, cont))
				return
			}
			if !ok {
				if !yield(Failure()) {
					return
				}
				continue
			}

			n, err := a.PutAtom(op.Name)
			if err != nil {
				_ = yield(a.Throw(err, cont))
				return
			}

			ok, err = a.Unify(operator, n)
			if err != nil {
				_ = yield(a.Throw(err, cont))
				return
			}
			if !ok {
				if !yield(Failure()) {
					return
				}
				continue
			}

			if !yield(a.Success(cont)) {
				return
			}
		}
	})
}

func CharConversion2(_ context.Context, e *Execution, inChar, outChar, cont term.Cell) Promise {
	in, err := e.MustBeChar(inChar)
	if err != nil {
		return e.Throw(err, cont)
	}

	out, err := e.MustBeChar(outChar)
	if err != nil {
		return e.Throw(err, cont)
	}

	e.CharConversion.Entries = slices.DeleteFunc(e.CharConversion.Entries, func(entry syntax.CharConversionEntry) bool {
		return entry.In == in
	})

	if in != out {
		e.CharConversion.Entries = append(e.CharConversion.Entries, syntax.CharConversionEntry{
			In:  in,
			Out: out,
		})
	}

	return e.Success(cont)
}

func CurrentCharConversion2(_ context.Context, a *Activation, inChar, outChar, cont Ref) Promise {
	if _, _, err := a.exec.canBeChar(*inChar.cell); err != nil {
		return a.Throw(err, cont)
	}

	if _, _, err := a.exec.canBeChar(*outChar.cell); err != nil {
		return a.Throw(err, cont)
	}

	return a.Nondet(func(yield func(Promise) bool) {
		for _, entry := range a.exec.CharConversion.Entries {
			i, err := a.PutAtom(term.NewAtomRune(entry.In))
			if err != nil {
				_ = yield(a.Throw(err, cont))
				return
			}

			ok, err := a.Unify(inChar, i)
			if err != nil {
				_ = yield(a.Throw(err, cont))
				return
			}
			if !ok {
				if !yield(Failure()) {
					return
				}
				continue
			}

			o, err := a.PutAtom(term.NewAtomRune(entry.Out))
			if err != nil {
				_ = yield(a.Throw(err, cont))
				return
			}

			ok, err = a.Unify(outChar, o)
			if err != nil {
				_ = yield(a.Throw(err, cont))
				return
			}
			if !ok {
				if !yield(Failure()) {
					return
				}
				continue
			}

			if !yield(a.Success(cont)) {
				return
			}
		}
	})
}

func Call2(_ context.Context, e *Execution, closure, arg1, cont term.Cell) Promise {
	closure = e.Deref(closure)

	f, err := e.mustBeCallable(closure)
	if err != nil {
		return e.Throw(err, cont)
	}

	cont, err = e.PutCompound(f.Name(), slices.Collect(concat(
		e.Args(closure),
		singleton(arg1),
		singleton(cont),
	))...)
	if err != nil {
		return e.Throw(err, cont)
	}

	return e.Success(cont)
}

func Call3(_ context.Context, e *Execution, closure, arg1, arg2, cont term.Cell) Promise {
	closure = e.Deref(closure)

	f, err := e.mustBeCallable(closure)
	if err != nil {
		return e.Throw(err, cont)
	}

	cont, err = e.PutCompound(f.Name(), slices.Collect(concat(
		e.Args(closure),
		singleton(arg1),
		singleton(arg2),
		singleton(cont),
	))...)
	if err != nil {
		return e.Throw(err, cont)
	}

	return e.Success(cont)
}

func Call4(_ context.Context, e *Execution, closure, arg1, arg2, arg3, cont term.Cell) Promise {
	closure = e.Deref(closure)

	f, err := e.mustBeCallable(closure)
	if err != nil {
		return e.Throw(err, cont)
	}

	cont, err = e.PutCompound(f.Name(), slices.Collect(concat(
		e.Args(closure),
		singleton(arg1),
		singleton(arg2),
		singleton(arg3),
		singleton(cont),
	))...)
	if err != nil {
		return e.Throw(err, cont)
	}

	return e.Success(cont)
}

func Call5(_ context.Context, e *Execution, closure, arg1, arg2, arg3, arg4, cont term.Cell) Promise {
	closure = e.Deref(closure)

	f, err := e.mustBeCallable(closure)
	if err != nil {
		return e.Throw(err, cont)
	}

	cont, err = e.PutCompound(f.Name(), slices.Collect(concat(
		e.Args(closure),
		singleton(arg1),
		singleton(arg2),
		singleton(arg3),
		singleton(arg4),
		singleton(cont),
	))...)
	if err != nil {
		return e.Throw(err, cont)
	}

	return e.Success(cont)
}

func Call6(_ context.Context, e *Execution, closure, arg1, arg2, arg3, arg4, arg5, cont term.Cell) Promise {
	closure = e.Deref(closure)

	f, err := e.mustBeCallable(closure)
	if err != nil {
		return e.Throw(err, cont)
	}

	cont, err = e.PutCompound(f.Name(), slices.Collect(concat(
		e.Args(closure),
		singleton(arg1),
		singleton(arg2),
		singleton(arg3),
		singleton(arg4),
		singleton(arg5),
		singleton(cont),
	))...)
	if err != nil {
		return e.Throw(err, cont)
	}

	return e.Success(cont)
}

func Call7(_ context.Context, e *Execution, closure, arg1, arg2, arg3, arg4, arg5, arg6, cont term.Cell) Promise {
	closure = e.Deref(closure)

	f, err := e.mustBeCallable(closure)
	if err != nil {
		return e.Throw(err, cont)
	}

	cont, err = e.PutCompound(f.Name(), slices.Collect(concat(
		e.Args(closure),
		singleton(arg1),
		singleton(arg2),
		singleton(arg3),
		singleton(arg4),
		singleton(arg5),
		singleton(arg6),
		singleton(cont),
	))...)
	if err != nil {
		return e.Throw(err, cont)
	}

	return e.Success(cont)
}

func Call8(_ context.Context, e *Execution, closure, arg1, arg2, arg3, arg4, arg5, arg6, arg7, cont term.Cell) Promise {
	closure = e.Deref(closure)

	f, err := e.mustBeCallable(closure)
	if err != nil {
		return e.Throw(err, cont)
	}

	cont, err = e.PutCompound(f.Name(), slices.Collect(concat(
		e.Args(closure),
		singleton(arg1),
		singleton(arg2),
		singleton(arg3),
		singleton(arg4),
		singleton(arg5),
		singleton(arg6),
		singleton(arg7),
		singleton(cont),
	))...)
	if err != nil {
		return e.Throw(err, cont)
	}

	return e.Success(cont)
}

func AtomLength2(_ context.Context, e *Execution, atom, length, cont term.Cell) Promise {
	a, err := e.MustBeAtom(atom)
	if err != nil {
		return e.Throw(err, cont)
	}

	if _, _, err := e.canBeInteger(length); err != nil {
		return e.Throw(err, cont)
	}

	l, err := e.PutInteger(int64(utf8.RuneCountInString(a.String())))
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(length, l)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func AtomConcat3(_ context.Context, a *Activation, atom1, atom2, atom3, cont Ref) Promise {
	atom1, atom2 = a.Deref(atom1), a.Deref(atom2)

	a3, ok, err := a.exec.canBeAtom(*atom3.cell)
	if err != nil {
		return a.Throw(err, cont)
	}
	if !ok {
		a1, err := a.MustBeAtom(atom1)
		if err != nil {
			return a.Throw(err, cont)
		}

		a2, err := a.MustBeAtom(atom2)
		if err != nil {
			return a.Throw(err, cont)
		}

		atom, err := a.PutAtom(term.NewAtom(a1.String() + a2.String()))
		if err != nil {
			return a.Throw(err, cont)
		}

		ok, err := a.Unify(atom3, atom)
		if err != nil {
			return a.Throw(err, cont)
		}
		if !ok {
			return Failure()
		}

		return a.Success(cont)
	}

	if _, _, err := a.exec.canBeAtom(*atom1.cell); err != nil {
		return a.Throw(err, cont)
	}

	if _, _, err := a.exec.canBeAtom(*atom2.cell); err != nil {
		return a.Throw(err, cont)
	}

	return a.Nondet(func(yield func(Promise) bool) {
		s := a3.String()
		for i := 0; i <= len(s); i += nextRuneSize(s[i:]) {
			a1, err := a.PutAtom(term.NewAtom(s[:i]))
			if err != nil {
				_ = yield(a.Throw(err, cont))
				return
			}

			ok, err := a.Unify(atom1, a1)
			if err != nil {
				_ = yield(a.Throw(err, cont))
				return
			}
			if !ok {
				if !yield(Failure()) {
					return
				}
				continue
			}

			a2, err := a.PutAtom(term.NewAtom(s[i:]))
			if err != nil {
				_ = yield(a.Throw(err, cont))
				return
			}

			ok, err = a.Unify(atom2, a2)
			if err != nil {
				_ = yield(a.Throw(err, cont))
				return
			}
			if !ok {
				if !yield(Failure()) {
					return
				}
				continue
			}

			if !yield(a.Success(cont)) {
				return
			}
		}
	})
}

func SubAtom5(_ context.Context, a *Activation, atom, before, length, after, subAtom, cont Ref) Promise {
	at, err := a.MustBeAtom(atom)
	if err != nil {
		return a.Throw(err, cont)
	}

	if _, _, err := a.exec.canBeNotLessThanZero(*before.cell); err != nil {
		return a.Throw(err, cont)
	}

	if _, _, err := a.exec.canBeNotLessThanZero(*length.cell); err != nil {
		return a.Throw(err, cont)
	}

	if _, _, err := a.exec.canBeNotLessThanZero(*after.cell); err != nil {
		return a.Throw(err, cont)
	}

	if _, _, err := a.exec.canBeAtom(*subAtom.cell); err != nil {
		return a.Throw(err, cont)
	}

	return a.Nondet(func(yield func(Promise) bool) {
		s := at.String()
		for i := 0; i <= len(s); i += nextRuneSize(s[i:]) {
			for j := i; j <= len(s); j += nextRuneSize(s[j:]) {
				b, err := a.PutInteger(int64(i))
				if err != nil {
					_ = yield(a.Throw(err, cont))
					return
				}

				ok, err := a.Unify(before, b)
				if err != nil {
					_ = yield(a.Throw(err, cont))
					return
				}
				if !ok {
					if !yield(Failure()) {
						return
					}
					continue
				}

				l, err := a.PutInteger(int64(j - i))
				if err != nil {
					_ = yield(a.Throw(err, cont))
					return
				}

				ok, err = a.Unify(length, l)
				if err != nil {
					_ = yield(a.Throw(err, cont))
					return
				}
				if !ok {
					if !yield(Failure()) {
						return
					}
					continue
				}

				af, err := a.PutInteger(int64(len(s) - j))
				if err != nil {
					_ = yield(a.Throw(err, cont))
					return
				}

				ok, err = a.Unify(after, af)
				if err != nil {
					_ = yield(a.Throw(err, cont))
					return
				}
				if !ok {
					if !yield(Failure()) {
						return
					}
					continue
				}

				sub, err := a.PutAtom(term.NewAtom(s[i:j]))
				if err != nil {
					_ = yield(a.Throw(err, cont))
					return
				}

				ok, err = a.Unify(subAtom, sub)
				if err != nil {
					_ = yield(a.Throw(err, cont))
					return
				}
				if !ok {
					if !yield(Failure()) {
						return
					}
					continue
				}

				if !yield(a.Success(cont)) {
					return
				}
			}
		}
	})
}

func nextRuneSize(s string) int {
	_, size := utf8.DecodeRuneInString(s)
	return max(size, 1)
}

func AtomChars2(_ context.Context, e *Execution, atom, chars, cont term.Cell) Promise {
	a, ok, err := e.canBeAtom(atom)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		var sb strings.Builder
		if err := e.MustBeList(chars, func(elem term.Cell) error {
			r, err := e.MustBeChar(elem)
			if err != nil {
				return err
			}
			sb.WriteRune(r)
			return nil
		}); err != nil {
			return e.Throw(err, cont)
		}

		a, err := e.PutAtom(term.NewAtom(sb.String()))
		if err != nil {
			return e.Throw(err, cont)
		}

		ok, err := e.Unify(atom, a)
		if err != nil {
			return e.Throw(err, cont)
		}
		if !ok {
			return Failure()
		}

		return e.Success(cont)
	}

	if _, err := e.canBeList(chars, func(elem term.Cell) error {
		_, _, err := e.canBeChar(elem)
		return err
	}); err != nil {
		return e.Throw(err, cont)
	}

	cs, err := e.PutCharList(a.String())
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err = e.Unify(chars, cs)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func AtomCodes2(_ context.Context, e *Execution, atom, codes, cont term.Cell) Promise {
	a, ok, err := e.canBeAtom(atom)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		var sb strings.Builder
		if err := e.MustBeList(codes, func(elem term.Cell) error {
			r, err := e.mustBeCharCode(elem)
			if err != nil {
				return err
			}
			sb.WriteRune(r)
			return nil
		}); err != nil {
			return e.Throw(err, cont)
		}

		a, err := e.PutAtom(term.NewAtom(sb.String()))
		if err != nil {
			return e.Throw(err, cont)
		}

		ok, err := e.Unify(atom, a)
		if err != nil {
			return e.Throw(err, cont)
		}
		if !ok {
			return Failure()
		}

		return e.Success(cont)
	}

	if _, err := e.canBeList(codes, func(elem term.Cell) error {
		_, _, err := e.canBeCharCode(elem)
		return err
	}); err != nil {
		return e.Throw(err, cont)
	}

	cs, err := e.PutCodeList(a.String())
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err = e.Unify(codes, cs)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func CharCode2(_ context.Context, e *Execution, char, code, cont term.Cell) Promise {
	r, ok, err := e.canBeChar(char)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		r, err := e.mustBeCharCode(code)
		if err != nil {
			return e.Throw(err, cont)
		}

		ch, err := e.PutAtom(term.NewAtomRune(r))
		if err != nil {
			return e.Throw(err, cont)
		}

		ok, err := e.Unify(char, ch)
		if err != nil {
			return e.Throw(err, cont)
		}
		if !ok {
			return Failure()
		}

		return e.Success(cont)
	}

	if _, _, err := e.canBeCharCode(code); err != nil {
		return e.Throw(err, cont)
	}

	cd, err := e.PutInteger(int64(r))
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err = e.Unify(code, cd)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func NumberChars2(_ context.Context, e *Execution, number, list, cont term.Cell) Promise {
	var sb strings.Builder
	switch ok, err := e.canBeList(list, func(elem term.Cell) error {
		r, err := e.MustBeChar(elem)
		if err != nil {
			return err
		}
		_, _ = sb.WriteRune(r)
		return nil
	}); {
	case err != nil:
		return e.Throw(err, cont)
	case !ok:
		if _, _, _, _, err := e.mustBeNumber(number); err != nil {
			return e.Throw(err, cont)
		}

		var sb strings.Builder
		_, _ = fmt.Fprintf(&sb, "%s", &syntax.Formatter{
			Arena: e.Arena,
			Term:  number,
		})

		l, err := e.PutCharList(sb.String())
		if err != nil {
			return e.Throw(err, cont)
		}

		ok, err = e.Unify(list, l)
		if err != nil {
			return e.Throw(err, cont)
		}
		if !ok {
			return Failure()
		}

		return e.Success(cont)
	}

	n, err := syntax.ParseNumber(strings.NewReader(sb.String()),
		syntax.Arena(e.Arena),
	)
	switch {
	case errors.Is(err, syntax.ErrNotANumber):
		return e.Throw(&SyntaxError{
			ImpDepAtom: term.NewAtom("not_a_number"),
			Location:   e.location,
		}, cont)
	case err != nil:
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(number, n)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func NumberCodes2(_ context.Context, e *Execution, number, list, cont term.Cell) Promise {
	var sb strings.Builder
	switch ok, err := e.canBeList(list, func(elem term.Cell) error {
		r, err := e.mustBeCharCode(elem)
		if err != nil {
			return err
		}
		_, _ = sb.WriteRune(r)
		return nil
	}); {
	case err != nil:
		return e.Throw(err, cont)
	case !ok:
		if _, _, _, _, err := e.mustBeNumber(number); err != nil {
			return e.Throw(err, cont)
		}

		var sb strings.Builder
		_, _ = fmt.Fprintf(&sb, "%s", &syntax.Formatter{
			Arena: e.Arena,
			Term:  number,
		})

		l, err := e.PutCodeList(sb.String())
		if err != nil {
			return e.Throw(err, cont)
		}

		ok, err = e.Unify(list, l)
		if err != nil {
			return e.Throw(err, cont)
		}
		if !ok {
			return Failure()
		}

		return e.Success(cont)
	}

	n, err := syntax.ParseNumber(strings.NewReader(sb.String()),
		syntax.Arena(e.Arena),
	)
	switch {
	case errors.Is(err, syntax.ErrNotANumber):
		return e.Throw(&SyntaxError{
			ImpDepAtom: term.NewAtom("not_a_number"),
			Location:   e.location,
		}, cont)
	case err != nil:
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(number, n)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

// Success continues the execution with the given continuation.
func (e *Execution) Success(cont term.Cell) Promise {
	e.tempVars[1] = cont
	e.Next()
	return Promise{ok: true}
}

func (e *Execution) Failure() Promise {
	return Failure()
}

// Throw throws an error.
func (e *Execution) Throw(err error, cont term.Cell) Promise {
	et, err := ErrorTerm(e.Arena, err)
	if err != nil {
		return Error(err)
	}
	return e.throwBall(et, cont)
}

// throwBall continues execution by throwing ball, which is already a term.
func (e *Execution) throwBall(ball, cont term.Cell) Promise {
	cont, err := e.PutCompound(term.NewAtom("throw"), ball, cont)
	if err != nil {
		return Error(err)
	}
	return e.Success(cont)
}

type flagEntry struct {
	flag term.Atom
	get  func(e *Engine) (term.Cell, error)
	set  func(e *Engine, value term.Cell) error
}

var (
	flags = []flagEntry{
		{
			flag: term.NewAtom("bounded"),
			get: func(e *Engine) (term.Cell, error) {
				return e.PutAtom(term.NewAtom("true"))
			},
		},
		{
			flag: term.NewAtom("max_integer"),
			get: func(e *Engine) (term.Cell, error) {
				return e.PutInteger(int64(math.MaxInt64))
			},
		},
		{
			flag: term.NewAtom("min_integer"),
			get: func(e *Engine) (term.Cell, error) {
				return e.PutInteger(int64(math.MinInt64))
			},
		},
		{
			flag: term.NewAtom("integer_rounding_function"),
			get: func(e *Engine) (term.Cell, error) {
				return e.PutAtom(term.NewAtom("toward_zero"))
			},
		},
		{
			flag: term.NewAtom("char_conversion"),
			get: func(e *Engine) (term.Cell, error) {
				if e.CharConversion.Disabled {
					return e.PutAtom(term.NewAtom("false"))
				}
				return e.PutAtom(term.NewAtom("true"))
			},
			set: func(e *Engine, value term.Cell) error {
				switch a, _ := e.Atom(value); a {
				case term.NewAtom("true"):
					e.CharConversion.Disabled = false
				case term.NewAtom("false"):
					e.CharConversion.Disabled = true
				default:
					return errInvalidFlagValue
				}
				return nil
			},
		},
		{
			flag: term.NewAtom("debug"),
			get: func(e *Engine) (term.Cell, error) {
				if e.debug {
					return e.PutAtom(term.NewAtom("on"))
				}
				return e.PutAtom(term.NewAtom("off"))
			},
			set: func(e *Engine, value term.Cell) error {
				value = e.Deref(value)
				switch a, _ := e.Atom(value); a {
				case term.NewAtom("on"):
					e.debug = true
				case term.NewAtom("off"):
					e.debug = false
				default:
					return errInvalidFlagValue
				}
				return nil
			},
		},
		{
			flag: term.NewAtom("max_arity"),
			get: func(e *Engine) (term.Cell, error) {
				return e.PutInteger(int64(math.MaxUint16))
			},
		},
		{
			flag: term.NewAtom("unknown"),
			get: func(e *Engine) (term.Cell, error) {
				return e.PutAtom(term.NewAtom(e.unknown.String()))
			},
			set: func(e *Engine, value term.Cell) error {
				a, _ := e.Atom(value)
				i := slices.IndexFunc(unknowActionNames[:], func(name string) bool {
					return name == a.String()
				})
				if i < 0 {
					return errInvalidFlagValue
				}
				e.unknown = unknownAction(i)
				return nil
			},
		},
		{
			flag: term.NewAtom("double_quotes"),
			get: func(e *Engine) (term.Cell, error) {
				return e.PutAtom(term.NewAtom(e.DoubleQuotes.String()))
			},
			set: func(e *Engine, value term.Cell) error {
				a, _ := e.Atom(value)
				i := slices.IndexFunc(syntax.DoubleQuoteNames[:], func(name string) bool {
					return name == a.String()
				})
				if i < 0 {
					return errInvalidFlagValue
				}
				e.DoubleQuotes = syntax.DoubleQuotes(i)
				return nil
			},
		},
	}
	errInvalidFlagValue = errors.New("invalid flag value")
)

func SetPrologFlag2(_ context.Context, e *Execution, flag, value, cont term.Cell) Promise {
	value = e.Deref(value)

	f, err := e.MustBeAtom(flag)
	if err != nil {
		return e.Throw(err, cont)
	}

	var fe flagEntry
	switch i := slices.IndexFunc(flags, func(entry flagEntry) bool {
		return entry.flag == f
	}); {
	case i < 0:
		return e.Throw(&DomainError{
			ValidDomain: term.NewAtom("flag"),
			Culprit:     syntax.Serialize(e.Arena, flag),
			Location:    e.location,
		}, cont)
	default:
		fe = flags[i]
	}

	if fe.set == nil {
		return e.Throw(&PermissionError{
			Operation:      term.NewAtom("modify"),
			PermissionType: term.NewAtom("flag"),
			Culprit:        syntax.Serialize(e.Arena, flag),
			Location:       e.location,
		}, cont)
	}

	switch err := fe.set(e.Engine, value); {
	case errors.Is(err, errInvalidFlagValue):
		p, err := e.PutCompound(term.NewAtomRune('+'), flag, value)
		if err != nil {
			return e.Throw(err, cont)
		}

		return e.Throw(&DomainError{
			ValidDomain: term.NewAtom("flag_value"),
			Culprit:     syntax.Serialize(e.Arena, p),
			Location:    e.location,
		}, cont)
	case err != nil:
		return e.Throw(err, cont)
	}

	return e.Success(cont)
}

func CurrentPrologFlag2(_ context.Context, a *Activation, flag, value, cont Ref) Promise {
	f, ok, err := a.exec.canBeAtom(*flag.cell)
	if err != nil {
		return a.Throw(err, cont)
	}
	if !ok {
		return a.Nondet(func(yield func(Promise) bool) {
			for _, fe := range flags {
				f, err := a.PutAtom(fe.flag)
				if err != nil {
					if !yield(a.Throw(err, cont)) {
						return
					}
					continue
				}

				ok, err := a.Unify(flag, f)
				if err != nil {
					if !yield(a.Throw(err, cont)) {
						return
					}
					continue
				}
				if !ok {
					if !yield(Failure()) {
						return
					}
					continue
				}

				v, err := fe.get(a.exec.Engine)
				if err != nil {
					if !yield(a.Throw(err, cont)) {
						return
					}
					continue
				}

				ok, err = a.exec.Unify(*value.cell, v)
				if err != nil {
					if !yield(a.Throw(err, cont)) {
						return
					}
					continue
				}
				if !ok {
					if !yield(Failure()) {
						return
					}
					continue
				}

				if !yield(a.Success(cont)) {
					return
				}
			}
		})
	}
	i := slices.IndexFunc(flags, func(entry flagEntry) bool {
		return entry.flag == f
	})
	if i < 0 {
		return a.Throw(&DomainError{
			ValidDomain: term.NewAtom("flag"),
			Culprit:     syntax.Serialize(a.exec.Arena, *flag.cell),
			Location:    a.exec.location,
		}, cont)
	}

	fe := flags[i]
	v, err := fe.get(a.exec.Engine)
	if err != nil {
		return a.Throw(err, cont)
	}

	ok, err = a.exec.Unify(*value.cell, v)
	if err != nil {
		return a.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return a.Success(cont)
}

func Halt1(_ context.Context, e *Execution, x, cont term.Cell) Promise {
	n, err := e.MustBeInteger(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	if e.Halt == nil {
		e.Halt = func(code int) {}
	}
	e.Halt(int(n))

	// 8.17.3.1 says "this built-in predicate neither succeeds nor fails."
	// In case Halt doesn't terminate the engine, we return a Go error here.
	return Error(fmt.Errorf("halt(%d)", n))
}

func Dynamic1(_ context.Context, e *Execution, t, cont term.Cell) Promise {
	t = e.Deref(t)

	pi, err := e.mustBePredicateIndicator(t)
	if err != nil {
		return e.Throw(err, cont)
	}

	bpi := term.NewFunctor(pi.Name(), pi.Arity()+1)
	p, _ := e.Predicates[bpi]
	p.Public = true
	p.Dynamic = true
	e.Predicates[bpi] = p

	return e.Success(cont)
}

func Multifile1(_ context.Context, e *Execution, t, cont term.Cell) Promise {
	t = e.Deref(t)

	pi, err := e.mustBePredicateIndicator(t)
	if err != nil {
		return e.Throw(err, cont)
	}

	bpi := term.NewFunctor(pi.Name(), pi.Arity()+1)
	p, _ := e.Predicates[bpi]
	p.Multifile = true
	e.Predicates[bpi] = p

	return e.Success(cont)
}

func Discontiguous1(_ context.Context, e *Execution, t, cont term.Cell) Promise {
	t = e.Deref(t)

	pi, err := e.mustBePredicateIndicator(t)
	if err != nil {
		return e.Throw(err, cont)
	}

	bpi := term.NewFunctor(pi.Name(), pi.Arity()+1)
	p, _ := e.Predicates[bpi]
	p.Discontiguous = true
	e.Predicates[bpi] = p

	return e.Success(cont)
}

func GetNeckCut1(_ context.Context, e *Execution, t term.Cell) (bool, error) {
	cutB, err := e.PutInteger(int64(e.cutB))
	if err != nil {
		return false, err
	}
	return e.Unify(t, cutB)
}

func GetCont1(_ context.Context, e *Execution, out, cont term.Cell) Promise {
	ok, err := e.Unify(out, cont)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}
	return e.Success(cont)
}

func CallCont1(ctx context.Context, a *Activation, cont, _ Ref) Promise {
	return True0(ctx, a, cont)
}

func Add3(_ context.Context, e *Execution, x, y, out, cont term.Cell) Promise {
	x, y = e.Deref(x), e.Deref(y)

	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	yi, yInt, yf, _, err := e.mustBeNumber(y)
	if err != nil {
		return e.Throw(err, cont)
	}

	var t term.Cell
	switch {
	case xInt && yInt:
		r, err := addI(xi, yi)
		if err != nil {
			return e.Throw(&EvaluationError{
				Cause:    err,
				Location: e.location,
			}, cont)
		}
		t, err = e.PutInteger(r)
		if err != nil {
			return e.Throw(err, cont)
		}
	case xInt:
		r, err := addIF(xi, yf)
		if err != nil {
			return e.Throw(&EvaluationError{
				Cause:    err,
				Location: e.location,
			}, cont)
		}
		t, err = e.PutFloat(r)
		if err != nil {
			return e.Throw(err, cont)
		}
	case yInt:
		r, err := addFI(xf, yi)
		if err != nil {
			return e.Throw(&EvaluationError{
				Cause:    err,
				Location: e.location,
			}, cont)
		}
		t, err = e.PutFloat(r)
		if err != nil {
			return e.Throw(err, cont)
		}
	default:
		r, err := addF(xf, yf)
		if err != nil {
			return e.Throw(&EvaluationError{
				Cause:    err,
				Location: e.location,
			}, cont)
		}
		t, err = e.PutFloat(r)
		if err != nil {
			return e.Throw(err, cont)
		}
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func Sub3(_ context.Context, e *Execution, x, y, out, cont term.Cell) Promise {
	x, y = e.Deref(x), e.Deref(y)

	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	yi, yInt, yf, _, err := e.mustBeNumber(y)
	if err != nil {
		return e.Throw(err, cont)
	}

	var t term.Cell
	switch {
	case xInt && yInt:
		r, err := subI(xi, yi)
		if err != nil {
			return e.Throw(&EvaluationError{
				Cause:    err,
				Location: e.location,
			}, cont)
		}
		t, err = e.PutInteger(r)
		if err != nil {
			return e.Throw(err, cont)
		}
	case xInt:
		r, err := subIF(xi, yf)
		if err != nil {
			return e.Throw(&EvaluationError{
				Cause:    err,
				Location: e.location,
			}, cont)
		}
		t, err = e.PutFloat(r)
		if err != nil {
			return e.Throw(err, cont)
		}
	case yInt:
		r, err := subFI(xf, yi)
		if err != nil {
			return e.Throw(&EvaluationError{
				Cause:    err,
				Location: e.location,
			}, cont)
		}
		t, err = e.PutFloat(r)
		if err != nil {
			return e.Throw(err, cont)
		}
	default:
		r, err := subF(xf, yf)
		if err != nil {
			return e.Throw(&EvaluationError{
				Cause:    err,
				Location: e.location,
			}, cont)
		}
		t, err = e.PutFloat(r)
		if err != nil {
			return e.Throw(err, cont)
		}
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func Mul3(_ context.Context, e *Execution, x, y, out, cont term.Cell) Promise {
	x, y = e.Deref(x), e.Deref(y)

	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	yi, yInt, yf, _, err := e.mustBeNumber(y)
	if err != nil {
		return e.Throw(err, cont)
	}

	var t term.Cell
	switch {
	case xInt && yInt:
		r, err := mulI(xi, yi)
		if err != nil {
			return e.Throw(&EvaluationError{
				Cause:    err,
				Location: e.location,
			}, cont)
		}
		t, err = e.PutInteger(r)
		if err != nil {
			return e.Throw(err, cont)
		}
	case xInt:
		r, err := mulIF(xi, yf)
		if err != nil {
			return e.Throw(&EvaluationError{
				Cause:    err,
				Location: e.location,
			}, cont)
		}
		t, err = e.PutFloat(r)
		if err != nil {
			return e.Throw(err, cont)
		}
	case yInt:
		r, err := mulFI(xf, yi)
		if err != nil {
			return e.Throw(&EvaluationError{
				Cause:    err,
				Location: e.location,
			}, cont)
		}
		t, err = e.PutFloat(r)
		if err != nil {
			return e.Throw(err, cont)
		}
	default:
		r, err := mulF(xf, yf)
		if err != nil {
			return e.Throw(&EvaluationError{
				Cause:    err,
				Location: e.location,
			}, cont)
		}
		t, err = e.PutFloat(r)
		if err != nil {
			return e.Throw(err, cont)
		}
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func IntDiv3(_ context.Context, e *Execution, x, y, out, cont term.Cell) Promise {
	x, y = e.Deref(x), e.Deref(y)

	if _, _, _, _, err := e.mustBeNumber(x); err != nil {
		return e.Throw(err, cont)
	}
	i, err := e.MustBeInteger(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	if _, _, _, _, err := e.mustBeNumber(y); err != nil {
		return e.Throw(err, cont)
	}
	j, err := e.MustBeInteger(y)
	if err != nil {
		return e.Throw(err, cont)
	}

	r, err := intDivI(i, j)
	if err != nil {
		return e.Throw(err, cont)
	}

	t, err := e.PutInteger(r)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func Div3(_ context.Context, e *Execution, x, y, out, cont term.Cell) Promise {
	x, y = e.Deref(x), e.Deref(y)

	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	yi, yInt, yf, _, err := e.mustBeNumber(y)
	if err != nil {
		return e.Throw(err, cont)
	}

	var t term.Cell
	switch {
	case xInt && yInt:
		r, err := divI(xi, yi)
		if err != nil {
			return e.Throw(&EvaluationError{
				Cause:    err,
				Location: e.location,
			}, cont)
		}
		t, err = e.PutFloat(r)
		if err != nil {
			return e.Throw(err, cont)
		}
	case xInt:
		r, err := divIF(xi, yf)
		if err != nil {
			return e.Throw(&EvaluationError{
				Cause:    err,
				Location: e.location,
			}, cont)
		}
		t, err = e.PutFloat(r)
		if err != nil {
			return e.Throw(err, cont)
		}
	case yInt:
		r, err := divFI(xf, yi)
		if err != nil {
			return e.Throw(&EvaluationError{
				Cause:    err,
				Location: e.location,
			}, cont)
		}
		t, err = e.PutFloat(r)
		if err != nil {
			return e.Throw(err, cont)
		}
	default:
		r, err := divF(xf, yf)
		if err != nil {
			return e.Throw(&EvaluationError{
				Cause:    err,
				Location: e.location,
			}, cont)
		}
		t, err = e.PutFloat(r)
		if err != nil {
			return e.Throw(err, cont)
		}
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func Rem3(_ context.Context, e *Execution, x, y, out, cont term.Cell) Promise {
	x, y = e.Deref(x), e.Deref(y)

	if _, _, _, _, err := e.mustBeNumber(x); err != nil {
		return e.Throw(err, cont)
	}
	i, err := e.MustBeInteger(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	if _, _, _, _, err := e.mustBeNumber(y); err != nil {
		return e.Throw(err, cont)
	}
	j, err := e.MustBeInteger(y)
	if err != nil {
		return e.Throw(err, cont)
	}

	r, err := remI(i, j)
	if err != nil {
		return e.Throw(&EvaluationError{
			Cause:    err,
			Location: e.location,
		}, cont)
	}

	t, err := e.PutInteger(r)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func Mod3(_ context.Context, e *Execution, x, y, out, cont term.Cell) Promise {
	x, y = e.Deref(x), e.Deref(y)

	if _, _, _, _, err := e.mustBeNumber(x); err != nil {
		return e.Throw(err, cont)
	}
	i, err := e.MustBeInteger(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	if _, _, _, _, err := e.mustBeNumber(y); err != nil {
		return e.Throw(err, cont)
	}
	j, err := e.MustBeInteger(y)
	if err != nil {
		return e.Throw(err, cont)
	}

	r, err := modI(i, j)
	if err != nil {
		return e.Throw(&EvaluationError{
			Cause:    err,
			Location: e.location,
		}, cont)
	}

	t, err := e.PutInteger(r)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func Neg2(_ context.Context, e *Execution, x, out, cont term.Cell) Promise {
	x = e.Deref(x)

	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	var t term.Cell
	if xInt {
		r, err := negI(xi)
		if err != nil {
			return e.Throw(&EvaluationError{
				Cause:    err,
				Location: e.location,
			}, cont)
		}
		t, err = e.PutInteger(r)
		if err != nil {
			return e.Throw(err, cont)
		}
	} else {
		t, err = e.PutFloat(negF(xf))
		if err != nil {
			return e.Throw(err, cont)
		}
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func Abs2(_ context.Context, e *Execution, x, out, cont term.Cell) Promise {
	x = e.Deref(x)

	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	var t term.Cell
	if xInt {
		r, err := absI(xi)
		if err != nil {
			return e.Throw(&EvaluationError{
				Cause:    err,
				Location: e.location,
			}, cont)
		}
		t, err = e.PutInteger(r)
		if err != nil {
			return e.Throw(err, cont)
		}
	} else {
		r := absF(xf)
		var err error
		t, err = e.PutFloat(r)
		if err != nil {
			return e.Throw(err, cont)
		}
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func Sign2(_ context.Context, e *Execution, x, out, cont term.Cell) Promise {
	x = e.Deref(x)

	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	var t term.Cell
	if xInt {
		r := signI(xi)
		t, err = e.PutInteger(r)
		if err != nil {
			return e.Throw(err, cont)
		}
	} else {
		r := signF(xf)
		var err error
		t, err = e.PutFloat(r)
		if err != nil {
			return e.Throw(err, cont)
		}
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func FloatIntegerPart2(_ context.Context, e *Execution, x, out, cont term.Cell) Promise {
	x = e.Deref(x)

	if _, _, _, _, err := e.mustBeNumber(x); err != nil {
		return e.Throw(err, cont)
	}
	f, err := e.MustBeFloat(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	r := intPartF(f)
	t, err := e.PutFloat(r)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func FloatFractionalPart2(_ context.Context, e *Execution, x, out, cont term.Cell) Promise {
	x = e.Deref(x)

	if _, _, _, _, err := e.mustBeNumber(x); err != nil {
		return e.Throw(err, cont)
	}
	f, err := e.MustBeFloat(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	r := fractPartF(f)
	t, err := e.PutFloat(r)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func Float2(_ context.Context, e *Execution, x, out, cont term.Cell) Promise {
	x = e.Deref(x)

	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	var r float64
	if xInt {
		r = floatItoF(xi)
	} else {
		r = floatFtoF(xf)
	}

	t, err := e.PutFloat(r)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func Floor2(_ context.Context, e *Execution, x, out, cont term.Cell) Promise {
	x = e.Deref(x)

	if _, _, _, _, err := e.mustBeNumber(x); err != nil {
		return e.Throw(err, cont)
	}
	f, err := e.MustBeFloat(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	r, err := floorFtoI(f)
	if err != nil {
		return e.Throw(&EvaluationError{
			Cause:    err,
			Location: e.location,
		}, cont)
	}

	t, err := e.PutInteger(r)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func Truncate2(_ context.Context, e *Execution, x, out, cont term.Cell) Promise {
	x = e.Deref(x)

	if _, _, _, _, err := e.mustBeNumber(x); err != nil {
		return e.Throw(err, cont)
	}
	f, err := e.MustBeFloat(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	r, err := truncateFtoI(f)
	if err != nil {
		return e.Throw(&EvaluationError{
			Cause:    err,
			Location: e.location,
		}, cont)
	}

	t, err := e.PutInteger(r)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func Round2(_ context.Context, e *Execution, x, out, cont term.Cell) Promise {
	x = e.Deref(x)

	if _, _, _, _, err := e.mustBeNumber(x); err != nil {
		return e.Throw(err, cont)
	}
	f, err := e.MustBeFloat(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	r, err := roundFtoI(f)
	if err != nil {
		return e.Throw(&EvaluationError{
			Cause:    err,
			Location: e.location,
		}, cont)
	}

	t, err := e.PutInteger(r)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func Ceiling2(_ context.Context, e *Execution, x, out, cont term.Cell) Promise {
	x = e.Deref(x)

	if _, _, _, _, err := e.mustBeNumber(x); err != nil {
		return e.Throw(err, cont)
	}
	f, err := e.MustBeFloat(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	r, err := ceilingFtoI(f)
	if err != nil {
		return e.Throw(&EvaluationError{
			Cause:    err,
			Location: e.location,
		}, cont)
	}

	t, err := e.PutInteger(r)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func FloorDiv3(_ context.Context, e *Execution, x, y, out, cont term.Cell) Promise {
	x, y = e.Deref(x), e.Deref(y)

	if _, _, _, _, err := e.mustBeNumber(x); err != nil {
		return e.Throw(err, cont)
	}
	i, err := e.MustBeInteger(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	if _, _, _, _, err := e.mustBeNumber(y); err != nil {
		return e.Throw(err, cont)
	}
	j, err := e.MustBeInteger(y)
	if err != nil {
		return e.Throw(err, cont)
	}

	r, err := intFloorDivI(i, j)
	if err != nil {
		return e.Throw(&EvaluationError{
			Cause:    err,
			Location: e.location,
		}, cont)
	}

	t, err := e.PutInteger(r)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func Pos2(_ context.Context, e *Execution, x, out, cont term.Cell) Promise {
	x = e.Deref(x)

	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	var t term.Cell
	if xInt {
		r, err := posI(xi)
		if err != nil {
			return e.Throw(&EvaluationError{
				Cause:    err,
				Location: e.location,
			}, cont)
		}
		t, err = e.PutInteger(r)
		if err != nil {
			return e.Throw(err, cont)
		}
	} else {
		r, err := posF(xf)
		if err != nil {
			return e.Throw(&EvaluationError{
				Cause:    err,
				Location: e.location,
			}, cont)
		}
		t, err = e.PutFloat(r)
		if err != nil {
			return e.Throw(err, cont)
		}
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func Power3(_ context.Context, e *Execution, x, y, out, cont term.Cell) Promise {
	x, y = e.Deref(x), e.Deref(y)

	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}
	if xInt {
		xf = float64(xi)
	}

	yi, yInt, yf, _, err := e.mustBeNumber(y)
	if err != nil {
		return e.Throw(err, cont)
	}
	if yInt {
		yf = float64(yi)
	}

	// 9.3.1.3 d) special case
	if xf == 0 && yf < 0 {
		return e.Throw(&EvaluationError{
			Cause:    Undefined,
			Location: e.location,
		}, cont)
	}

	switch r := math.Pow(xf, yf); {
	case math.IsInf(r, 0):
		return e.Throw(&EvaluationError{
			Cause:    FloatOverflow,
			Location: e.location,
		}, cont)
	case r == 0 && xf != 0: // Underflow: r can be 0 iff x = 0.
		return e.Throw(&EvaluationError{
			Cause:    Underflow,
			Location: e.location,
		}, cont)
	case math.IsNaN(r):
		return e.Throw(&EvaluationError{
			Cause:    Undefined,
			Location: e.location,
		}, cont)
	default:
		t, err := e.PutFloat(r)
		if err != nil {
			return e.Throw(err, cont)
		}

		ok, err := e.Unify(out, t)
		if err != nil {
			return e.Throw(err, cont)
		}
		if !ok {
			return Failure()
		}

		return e.Success(cont)
	}
}

func Sin2(_ context.Context, e *Execution, x, out, cont term.Cell) Promise {
	x = e.Deref(x)

	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}
	if xInt {
		xf = float64(xi)
	}

	r := math.Sin(xf)
	t, err := e.PutFloat(r)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func Cos2(_ context.Context, e *Execution, x, out, cont term.Cell) Promise {
	x = e.Deref(x)

	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}
	if xInt {
		xf = float64(xi)
	}

	r := math.Cos(xf)
	t, err := e.PutFloat(r)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func Atan2(_ context.Context, e *Execution, x, out, cont term.Cell) Promise {
	x = e.Deref(x)

	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}
	if xInt {
		xf = float64(xi)
	}

	r := math.Atan(xf)
	t, err := e.PutFloat(r)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func Exp2(_ context.Context, e *Execution, x, out, cont term.Cell) Promise {
	x = e.Deref(x)

	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}
	if xInt {
		xf = float64(xi)
	}

	// Positive overflow:
	//        e^x > max
	//   log(e^x) > log(max)
	// x * log(e) > log(max)
	//          x > log(max)
	if xf > math.Log(math.MaxFloat64) {
		return e.Throw(&EvaluationError{
			Cause:    FloatOverflow,
			Location: e.location,
		}, cont)
	}

	r := math.Exp(xf)

	if r == 0 { // e^x != 0.
		return e.Throw(&EvaluationError{
			Cause:    Underflow,
			Location: e.location,
		}, cont)
	}

	t, err := e.PutFloat(r)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func Log2(_ context.Context, e *Execution, x, out, cont term.Cell) Promise {
	x = e.Deref(x)

	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}
	if xInt {
		xf = float64(xi)
	}

	if xf <= 0 {
		return e.Throw(&EvaluationError{
			Cause:    Undefined,
			Location: e.location,
		}, cont)
	}

	r := math.Log(xf)

	t, err := e.PutFloat(r)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func Sqrt2(_ context.Context, e *Execution, x, out, cont term.Cell) Promise {
	x = e.Deref(x)

	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}
	if xInt {
		xf = float64(xi)
	}

	if xf < 0 {
		return e.Throw(&EvaluationError{
			Cause:    Undefined,
			Location: e.location,
		}, cont)
	}

	r := math.Sqrt(xf)

	t, err := e.PutFloat(r)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func Max3(_ context.Context, e *Execution, x, y, out, cont term.Cell) Promise {
	x, y = e.Deref(x), e.Deref(y)

	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	yi, yInt, yf, _, err := e.mustBeNumber(y)
	if err != nil {
		return e.Throw(err, cont)
	}

	var t term.Cell
	switch {
	case xInt && yInt:
		t, err = e.PutInteger(max(xi, yi))
		if err != nil {
			return e.Throw(err, cont)
		}
	case xInt:
		t, err = e.PutFloat(max(float64(xi), yf))
		if err != nil {
			return e.Throw(err, cont)
		}
	case yInt:
		t, err = e.PutFloat(max(xf, float64(yi)))
		if err != nil {
			return e.Throw(err, cont)
		}
	default:
		t, err = e.PutFloat(max(xf, yf))
		if err != nil {
			return e.Throw(err, cont)
		}
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func Min3(_ context.Context, e *Execution, x, y, out, cont term.Cell) Promise {
	x, y = e.Deref(x), e.Deref(y)

	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	yi, yInt, yf, _, err := e.mustBeNumber(y)
	if err != nil {
		return e.Throw(err, cont)
	}

	var t term.Cell
	switch {
	case xInt && yInt:
		t, err = e.PutInteger(min(xi, yi))
		if err != nil {
			return e.Throw(err, cont)
		}
	case xInt:
		t, err = e.PutFloat(min(float64(xi), yf))
		if err != nil {
			return e.Throw(err, cont)
		}
	case yInt:
		t, err = e.PutFloat(min(xf, float64(yi)))
		if err != nil {
			return e.Throw(err, cont)
		}
	default:
		t, err = e.PutFloat(min(xf, yf))
		if err != nil {
			return e.Throw(err, cont)
		}
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func IntegerPower3(ctx context.Context, e *Execution, x, y, out, cont term.Cell) Promise {
	x, y = e.Deref(x), e.Deref(y)

	xi, xInt, _, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	yi, yInt, _, _, err := e.mustBeNumber(y)
	if err != nil {
		return e.Throw(err, cont)
	}

	if !xInt || !yInt {
		return Power3(ctx, e, x, y, out, cont)
	}

	var r int64
	if yi < 0 {
		switch xi {
		case 0:
			return e.Throw(&EvaluationError{
				Cause:    Undefined,
				Location: e.location,
			}, cont)
		case 1, -1:
			yi, err := negI(yi) // y can be minInt
			if err != nil {
				return e.Throw(&EvaluationError{
					Cause:    err,
					Location: e.location,
				}, cont)
			}
			r, _ = intPow(xi, yi) // Since x is either 1 or -1, no errors occur.
			r, err = intDivI(1, r)
			if err != nil {
				return e.Throw(&EvaluationError{
					Cause:    err,
					Location: e.location,
				}, cont)
			}
		default:
			return e.Throw(&TypeError{
				ValidType: term.NewAtom("float"),
				Culprit:   syntax.Serialize(e.Arena, x),
				Location:  e.location,
			}, cont)
		}
	} else {
		r, err = intPow(xi, yi)
		if err != nil {
			return e.Throw(&EvaluationError{
				Cause:    err,
				Location: e.location,
			}, cont)
		}
	}

	t, err := e.PutInteger(r)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

// Loosely based on https://www.programminglogic.com/fast-exponentiation-algorithms/
func intPow(a, b int64) (int64, error) {
	var (
		r   = int64(1)
		err error
	)
	for {
		if b&1 != 0 {
			r, err = mulI(r, a)
			if err != nil {
				return 0, err
			}
		}

		b >>= 1
		if b == 0 {
			break
		}

		a, err = mulI(a, a)
		if err != nil {
			return 0, err
		}
	}
	return r, nil
}

func Asin2(_ context.Context, e *Execution, x, out, cont term.Cell) Promise {
	x = e.Deref(x)

	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}
	if xInt {
		xf = float64(xi)
	}

	if xf > 1 || xf < -1 {
		return e.Throw(&EvaluationError{
			Cause:    Undefined,
			Location: e.location,
		}, cont)
	}

	r := math.Asin(xf)
	t, err := e.PutFloat(r)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func Acos2(_ context.Context, e *Execution, x, out, cont term.Cell) Promise {
	x = e.Deref(x)

	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}
	if xInt {
		xf = float64(xi)
	}

	if xf > 1 || xf < -1 {
		return e.Throw(&EvaluationError{
			Cause:    Undefined,
			Location: e.location,
		}, cont)
	}

	r := math.Acos(xf)
	t, err := e.PutFloat(r)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func Atan3(_ context.Context, e *Execution, y, x, out, cont term.Cell) Promise {
	y, x = e.Deref(y), e.Deref(x)

	yi, yInt, yf, _, err := e.mustBeNumber(y)
	if err != nil {
		return e.Throw(err, cont)
	}
	if yInt {
		yf = float64(yi)
	}

	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}
	if xInt {
		xf = float64(xi)
	}

	if yf == 0 && xf == 0 {
		return e.Throw(&EvaluationError{
			Cause:    Undefined,
			Location: e.location,
		}, cont)
	}

	r := math.Atan2(yf, xf)
	t, err := e.PutFloat(r)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func Tan2(_ context.Context, e *Execution, x, out, cont term.Cell) Promise {
	x = e.Deref(x)

	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}
	if xInt {
		xf = float64(xi)
	}

	r := math.Tan(xf)
	t, err := e.PutFloat(r)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)

}

func Pi1(_ context.Context, e *Execution, out, cont term.Cell) Promise {
	t, err := e.PutFloat(math.Pi)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func BitwiseRightShift3(_ context.Context, e *Execution, x, y, out, cont term.Cell) Promise {
	x, y = e.Deref(x), e.Deref(y)

	i, err := e.MustBeInteger(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	j, err := e.MustBeInteger(y)
	if err != nil {
		return e.Throw(err, cont)
	}

	r := i >> j
	t, err := e.PutInteger(r)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func BitwiseLeftShift3(_ context.Context, e *Execution, x, y, out, cont term.Cell) Promise {
	x, y = e.Deref(x), e.Deref(y)

	i, err := e.MustBeInteger(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	j, err := e.MustBeInteger(y)
	if err != nil {
		return e.Throw(err, cont)
	}

	r := i << j
	t, err := e.PutInteger(r)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func BitwiseAnd3(_ context.Context, e *Execution, x, y, out, cont term.Cell) Promise {
	x, y = e.Deref(x), e.Deref(y)

	i, err := e.MustBeInteger(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	j, err := e.MustBeInteger(y)
	if err != nil {
		return e.Throw(err, cont)
	}

	r := i & j
	t, err := e.PutInteger(r)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func BitwiseOr3(_ context.Context, e *Execution, x, y, out, cont term.Cell) Promise {
	x, y = e.Deref(x), e.Deref(y)

	i, err := e.MustBeInteger(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	j, err := e.MustBeInteger(y)
	if err != nil {
		return e.Throw(err, cont)
	}

	r := i | j
	t, err := e.PutInteger(r)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func BitwiseComplement2(_ context.Context, e *Execution, x, out, cont term.Cell) Promise {
	x = e.Deref(x)

	i, err := e.MustBeInteger(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	r := ^i
	t, err := e.PutInteger(r)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func BitwiseXor3(_ context.Context, e *Execution, x, y, out, cont term.Cell) Promise {
	x, y = e.Deref(x), e.Deref(y)

	i, err := e.MustBeInteger(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	j, err := e.MustBeInteger(y)
	if err != nil {
		return e.Throw(err, cont)
	}

	r := i ^ j
	t, err := e.PutInteger(r)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err := e.Unify(out, t)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func ArithEq2(_ context.Context, e *Execution, x, y, cont term.Cell) Promise {
	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	yi, yInt, yf, _, err := e.mustBeNumber(y)
	if err != nil {
		return e.Throw(err, cont)
	}

	var ok bool
	switch {
	case xInt && yInt:
		ok = eqI(xi, yi)
	case xInt:
		ok = eqIF(xi, yf)
	case yInt:
		ok = eqFI(xf, yi)
	default:
		ok = eqF(xf, yf)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func ArithDif2(_ context.Context, e *Execution, x, y, cont term.Cell) Promise {
	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	yi, yInt, yf, _, err := e.mustBeNumber(y)
	if err != nil {
		return e.Throw(err, cont)
	}

	var ok bool
	switch {
	case xInt && yInt:
		ok = neqI(xi, yi)
	case xInt:
		ok = neqIF(xi, yf)
	case yInt:
		ok = neqFI(xf, yi)
	default:
		ok = neqF(xf, yf)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func Less2(_ context.Context, e *Execution, x, y, cont term.Cell) Promise {
	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	yi, yInt, yf, _, err := e.mustBeNumber(y)
	if err != nil {
		return e.Throw(err, cont)
	}

	var ok bool
	switch {
	case xInt && yInt:
		ok = lssI(xi, yi)
	case xInt:
		ok = lssIF(xi, yf)
	case yInt:
		ok = lssFI(xf, yi)
	default:
		ok = lssF(xf, yf)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func LessEq2(_ context.Context, e *Execution, x, y, cont term.Cell) Promise {
	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	yi, yInt, yf, _, err := e.mustBeNumber(y)
	if err != nil {
		return e.Throw(err, cont)
	}

	var ok bool
	switch {
	case xInt && yInt:
		ok = leqI(xi, yi)
	case xInt:
		ok = leqIF(xi, yf)
	case yInt:
		ok = leqFI(xf, yi)
	default:
		ok = leqF(xf, yf)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func Greater2(_ context.Context, e *Execution, x, y, cont term.Cell) Promise {
	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	yi, yInt, yf, _, err := e.mustBeNumber(y)
	if err != nil {
		return e.Throw(err, cont)
	}

	var ok bool
	switch {
	case xInt && yInt:
		ok = gtrI(xi, yi)
	case xInt:
		ok = gtrIF(xi, yf)
	case yInt:
		ok = gtrFI(xf, yi)
	default:
		ok = gtrF(xf, yf)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func GreaterEq2(_ context.Context, e *Execution, x, y, cont term.Cell) Promise {
	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	yi, yInt, yf, _, err := e.mustBeNumber(y)
	if err != nil {
		return e.Throw(err, cont)
	}

	var ok bool
	switch {
	case xInt && yInt:
		ok = geqI(xi, yi)
	case xInt:
		ok = geqIF(xi, yf)
	case yInt:
		ok = geqFI(xf, yi)
	default:
		ok = geqF(xf, yf)
	}
	if !ok {
		return Failure()
	}

	return e.Success(cont)
}

func (e *Execution) unTrailTo(b int) error {
	if b >= len(e.stack) {
		return nil
	}

	// e.stack[b] is catch/3's own choice point, so its trailTop is the trail as
	// of the catch/3 call. Recovery resumes from there: bindings Goal made are
	// undone, bindings the enclosing clause made before catch/3 are kept.
	trailTop := e.stack[b].trailTop

	e.closeStackTo(b)
	return e.unwindTrail(trailTop)
}

func contChain(arena *term.Arena, cont term.Cell) iter.Seq[term.Cell] {
	return func(yield func(term.Cell) bool) {
		for {
			if !yield(cont) {
				return
			}

			pi, ok := arena.Functor(cont, term.AllowAtom(true))
			if !ok || pi.Arity() == 0 {
				return
			}

			cont = arena.Arg(cont, pi.Arity()-1)
		}
	}
}

func indexed[T any](s iter.Seq[T]) iter.Seq2[int, T] {
	return func(yield func(int, T) bool) {
		i := 0
		for e := range s {
			if !yield(i, e) {
				return
			}
			i++
		}
	}
}

func singleton[T any](e T) iter.Seq[T] {
	return func(yield func(T) bool) {
		_ = yield(e)
	}
}

func concat[T any](ss ...iter.Seq[T]) iter.Seq[T] {
	return func(yield func(T) bool) {
		for _, s := range ss {
			for e := range s {
				if !yield(e) {
					return
				}
			}
		}
	}
}
