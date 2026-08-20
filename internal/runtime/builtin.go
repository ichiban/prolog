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

func Success() Promise {
	return Promise{ok: true}
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
	Proc func(ctx context.Context, e *Execution) Promise
}

type BuiltinSet struct {
	index   map[term.Functor]int
	entries []Builtin
}

func NewBuiltinSet() *BuiltinSet {
	var b BuiltinSet
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("true"), 1), Type: InHead, Proc: true0})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("fail"), 1), Type: InHead, Proc: fail0})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("call"), 2), Type: InHead, Proc: call1})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("throw"), 2), Type: InHead, Proc: throw1})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("subsumes_term"), 3), Type: InHead, Proc: subsumesTerm2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("var"), 2), Type: InBody, Proc: var1})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("atom"), 2), Type: InBody, Proc: atom1})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("integer"), 2), Type: InBody, Proc: integer1})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("float"), 2), Type: InBody, Proc: float1})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("compound"), 2), Type: InBody, Proc: compound1})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("ground"), 2), Type: InBody, Proc: ground1})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("acyclic_term"), 2), Type: InBody, Proc: acyclicTerm1})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("compare"), 4), Type: InHead, Proc: compare3})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("keysort"), 3), Type: InHead, Proc: keySort2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("functor"), 4), Type: InHead, Proc: functor3})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("arg"), 4), Type: InHead, Proc: arg3})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("=.."), 3), Type: InHead, Proc: univ2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("copy_term"), 3), Type: InHead, Proc: copyTerm2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("term_variables"), 3), Type: InHead, Proc: termVariables2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("clause"), 3), Type: InHead, Proc: clause2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("current_predicate"), 2), Type: InHead, Proc: currentPredicate1})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("asserta"), 2), Type: InHead, Proc: assertA1})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("assertz"), 2), Type: InHead, Proc: assertZ1})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("retract"), 2), Type: InHead, Proc: retract1})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("abolish"), 2), Type: InHead, Proc: abolish1})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("findall"), 4), Type: InHead, Proc: findAll3})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("bagof"), 4), Type: InHead, Proc: bagOf3})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("setof"), 4), Type: InHead, Proc: setOf3})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("current_input"), 2), Type: InHead, Proc: currentInput1})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("current_output"), 2), Type: InHead, Proc: currentOutput1})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("set_input"), 2), Type: InHead, Proc: setInput1})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("set_output"), 2), Type: InHead, Proc: setOutput1})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("open"), 5), Type: InHead, Proc: open4})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("close"), 3), Type: InHead, Proc: close2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("flush_output"), 2), Type: InHead, Proc: flushOutput1})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("stream_property"), 3), Type: InHead, Proc: streamProperty2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("set_stream_position"), 3), Type: InHead, Proc: setStreamPosition2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("get_char"), 3), Type: InHead, Proc: getChar2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("get_code"), 3), Type: InHead, Proc: getCode2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("peek_char"), 3), Type: InHead, Proc: peekChar2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("peek_code"), 3), Type: InHead, Proc: peekCode2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("put_char"), 3), Type: InHead, Proc: putChar2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("put_code"), 3), Type: InHead, Proc: putCode2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("get_byte"), 3), Type: InHead, Proc: getByte2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("peek_byte"), 3), Type: InHead, Proc: peekByte2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("put_byte"), 3), Type: InHead, Proc: putByte2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("read_term"), 4), Type: InHead, Proc: readTerm3})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("write_term"), 4), Type: InHead, Proc: writeTerm3})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("op"), 4), Type: InHead, Proc: op3})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("current_op"), 4), Type: InHead, Proc: currentOp3})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("char_conversion"), 3), Type: InHead, Proc: charConversion2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("current_char_conversion"), 3), Type: InHead, Proc: currentCharConversion2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("call"), 3), Type: InHead, Proc: call2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("call"), 4), Type: InHead, Proc: call3})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("call"), 5), Type: InHead, Proc: call4})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("call"), 6), Type: InHead, Proc: call5})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("call"), 7), Type: InHead, Proc: call6})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("call"), 8), Type: InHead, Proc: call7})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("call"), 9), Type: InHead, Proc: call8})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("atom_length"), 3), Type: InHead, Proc: atomLength2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("atom_concat"), 4), Type: InHead, Proc: atomConcat3})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("sub_atom"), 6), Type: InHead, Proc: subAtom5})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("atom_chars"), 3), Type: InHead, Proc: atomChars2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("atom_codes"), 3), Type: InHead, Proc: atomCodes2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("char_code"), 3), Type: InHead, Proc: charCode2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("number_chars"), 3), Type: InHead, Proc: numberChars2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("number_codes"), 3), Type: InHead, Proc: numberCodes2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("set_prolog_flag"), 3), Type: InHead, Proc: setPrologFlag2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("current_prolog_flag"), 3), Type: InHead, Proc: currentPrologFlag2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("halt"), 2), Type: InHead, Proc: halt1})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$dynamic"), 2), Type: InHead, Proc: dynamic1})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$get_neck_cut"), 2), Type: InBody, Proc: getNeckCut1})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$get_cont"), 2), Type: InBody, Proc: getCont1})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$call_cont"), 2), Type: InHead, Proc: callCont1})
	// TODO: Implement optimized arithmetic calling convention in binprolog.
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$+"), 4), Type: InHead, Proc: add3})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$-"), 4), Type: InHead, Proc: sub3})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$*"), 4), Type: InHead, Proc: mul3})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$//"), 4), Type: InHead, Proc: intDiv3})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$/"), 4), Type: InHead, Proc: div3})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$rem"), 4), Type: InHead, Proc: rem3})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$mod"), 4), Type: InHead, Proc: mod3})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$-"), 3), Type: InHead, Proc: neg2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$abs"), 3), Type: InHead, Proc: abs2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$sign"), 3), Type: InHead, Proc: sign2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$float_integer_part"), 3), Type: InHead, Proc: floatIntegerPart2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$float_fractional_part"), 3), Type: InHead, Proc: floatFractionalPart2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$float"), 3), Type: InHead, Proc: float2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$floor"), 3), Type: InHead, Proc: floor2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$truncate"), 3), Type: InHead, Proc: truncate2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$round"), 3), Type: InHead, Proc: round2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$ceiling"), 3), Type: InHead, Proc: ceiling2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$div"), 4), Type: InHead, Proc: floorDiv3})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$+"), 3), Type: InHead, Proc: pos2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$**"), 4), Type: InHead, Proc: power3})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$sin"), 3), Type: InHead, Proc: sin2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$cos"), 3), Type: InHead, Proc: cos2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$atan"), 3), Type: InHead, Proc: atan2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$exp"), 3), Type: InHead, Proc: exp2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$log"), 3), Type: InHead, Proc: log2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$sqrt"), 3), Type: InHead, Proc: sqrt2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$max"), 4), Type: InHead, Proc: max3})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$min"), 4), Type: InHead, Proc: min3})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$^"), 4), Type: InHead, Proc: integerPower3})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$asin"), 3), Type: InHead, Proc: asin2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$acos"), 3), Type: InHead, Proc: acos2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$atan2"), 4), Type: InHead, Proc: atan3})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$tan"), 3), Type: InHead, Proc: tan2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$pi"), 2), Type: InHead, Proc: pi1})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$arith_eq"), 3), Type: InHead, Proc: arithEq2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$arith_dif"), 3), Type: InHead, Proc: arithDif2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$less"), 3), Type: InHead, Proc: less2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$less_eq"), 3), Type: InHead, Proc: lessEq2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$greater"), 3), Type: InHead, Proc: greater2})
	_ = b.Put(Builtin{PI: term.NewFunctor(term.NewAtom("$greater_eq"), 3), Type: InHead, Proc: greaterEq2})
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

func (b *BuiltinSet) Put(entry Builtin) error {
	if _, ok := b.index[entry.PI]; ok {
		return fmt.Errorf("duplicate builtin: %s", entry.PI)
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

type ExceptionalValue int8

const (
	FloatOverflow ExceptionalValue = iota
	IntOverflow
	Underflow
	ZeroDivisor
	Undefined
)

func (e ExceptionalValue) Error() string {
	return exceptionalValueNames[e]
}

var exceptionalValueNames = [...]string{
	FloatOverflow: "float_overflow",
	IntOverflow:   "int_overflow",
	Underflow:     "underflow",
	ZeroDivisor:   "zero_divisor",
	Undefined:     "undefined",
}

func true0(ctx context.Context, e *Execution) Promise {
	cont := e.tempVars[1]
	cont = e.Deref(cont)

	bpi, ok := e.Functor(cont, term.AllowAtom(true))
	if !ok {
		return Error(&TypeError{
			ValidType: term.NewAtom("continuation"),
			Culprit:   syntax.Serialize(e.Arena, cont),
			Location:  e.location,
		})
	}

	pi := term.NewFunctor(bpi.Name(), bpi.Arity()-1)

	p, ok, err := e.Predicate(bpi)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	if p.Dynamic {
		call, ok := e.Predicates[term.NewFunctor(term.NewAtom("call"), 2)]
		if !ok {
			c, err := e.PutFunctor(term.NewFunctor(term.NewAtom("call"), 1))
			if err != nil {
				return Error(err)
			}
			return Error(&ExistenceError{
				ObjectType: term.NewAtom("procedure"),
				Culprit:    syntax.Serialize(e.Arena, c),
				Location:   e.location,
			})
		}
		args := slices.Collect(e.Args(cont))
		goal, err := e.PutCompound(pi.Name(), args[:len(args)-1]...)
		if err != nil {
			return Error(err)
		}
		cont = args[len(args)-1]
		err = e.pushSeqStackFrame(func(yield func(Promise) bool) {
			for r := range e.DB.Select(ctx, e.Arena, pi, e.CurrentTime) {
				ok, err := e.Unify(r.Head, goal)
				if err != nil {
					_ = yield(Error(err))
					return
				}
				if !ok {
					if !yield(Failure()) {
						return
					}
					continue
				}

				e.tempVars[1] = r.Body
				e.tempVars[2] = cont
				e.programPointer = call.Offset

				if !yield(Success()) {
					return
				}
			}
		}, 2)
		return Error(err)
	}

	e.programPointer = p.Offset
	for i, arg := range indexed(e.Args(cont)) {
		e.tempVars[i+1] = arg
	}
	return Success()
}

func fail0(_ context.Context, e *Execution) Promise {
	return Failure()
}

func call1(ctx context.Context, e *Execution) Promise {
	goal, cont := e.tempVars[1], e.tempVars[2]
	goal = e.Deref(goal)

	// 7.8.3.1 says "When G contains ! as a subgoal, the effect of ! shall not extend outside G."
	goal, err := e.rewriteCutForCall(goal)
	if err != nil {
		return Error(err)
	}

	pi, ok := e.Functor(goal, term.AllowAtom(true))
	if !ok {
		if _, ok := e.Variable(goal); ok {
			return Error(&InstantiationError{
				Location: e.location,
			})
		}
		return Error(&TypeError{
			ValidType: term.NewAtom("callable"),
			Culprit:   syntax.Serialize(e.Arena, goal),
			Location:  e.location,
		})
	}

	bpi := term.NewFunctor(pi.Name(), pi.Arity()+1)
	p, ok, err := e.Predicate(bpi)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}
	if p.Dynamic {
		call, ok := e.Predicates[term.NewFunctor(term.NewAtom("call"), 2)]
		if !ok {
			c, err := e.PutFunctor(term.NewFunctor(term.NewAtom("call"), 1))
			if err != nil {
				return Error(err)
			}
			return Error(&ExistenceError{
				ObjectType: term.NewAtom("procedure"),
				Culprit:    syntax.Serialize(e.Arena, c),
				Location:   e.location,
			})
		}
		err = e.pushSeqStackFrame(func(yield func(Promise) bool) {
			for r := range e.DB.Select(ctx, e.Arena, pi, e.CurrentTime) {
				ok, err := e.Unify(r.Head, goal)
				if err != nil {
					_ = yield(Error(err))
					return
				}
				if !ok {
					if !yield(Failure()) {
						return
					}
					continue
				}

				e.tempVars[1] = r.Body
				e.tempVars[2] = cont
				e.programPointer = call.Offset

				if !yield(Success()) {
					return
				}
			}
		}, 2)
		return Error(err)
	}
	e.programPointer = p.Offset
	for i, arg := range indexed(concat(e.Args(goal), singleton(cont))) {
		e.tempVars[i+1] = arg
	}
	return Success()
}

func (e *Execution) rewriteCutForCall(body term.Handle) (term.Handle, error) {
	body = e.Deref(body)
	switch pi, _ := e.Functor(body, term.AllowAtom(true)); pi {
	case term.NewFunctor(term.NewAtomRune(';'), 2):
		x := e.Arg(body, 0)
		if f, _ := e.Functor(x); f == term.NewFunctor(term.NewAtom("->"), 2) {
			i, t := e.Arg(x, 0), e.Arg(x, 1)
			i, err := e.rewriteCutForCall(i)
			if err != nil {
				return term.Handle{}, err
			}
			t, err = e.rewriteCutForCall(t)
			if err != nil {
				return term.Handle{}, err
			}
			x, err = e.PutCompound(term.NewAtom("->"), i, t)
			if err != nil {
				return term.Handle{}, err
			}
		}
		fallthrough
	case term.NewFunctor(term.NewAtomRune(','), 2):
		x, y := e.Arg(body, 0), e.Arg(body, 1)
		x, err := e.rewriteCutForCall(x)
		if err != nil {
			return term.Handle{}, err
		}
		y, err = e.rewriteCutForCall(y)
		if err != nil {
			return term.Handle{}, err
		}
		return e.PutCompound(pi.Name(), x, y)
	case term.NewFunctor(term.NewAtomRune('!'), 0):
		b, err := e.PutInteger(int64(len(e.stack)))
		if err != nil {
			return term.Handle{}, err
		}
		return e.PutCompound(term.NewAtom("$cut_to"), b)
	default:
		return body, nil
	}
}

func var1(_ context.Context, e *Execution) Promise {
	v := e.tempVars[0]
	v = e.Deref(v)
	if _, ok := e.Variable(v); !ok {
		return Failure()
	}
	e.Next()
	return Success()
}

func atom1(_ context.Context, e *Execution) Promise {
	t := e.tempVars[0]
	t = e.Deref(t)
	if _, ok := e.Atom(t); !ok {
		return Failure()
	}
	e.Next()
	return Success()
}

func integer1(_ context.Context, e *Execution) Promise {
	t := e.tempVars[0]
	t = e.Deref(t)
	if _, ok := e.Integer(t); !ok {
		return Failure()
	}
	e.Next()
	return Success()
}

func float1(_ context.Context, e *Execution) Promise {
	t := e.tempVars[0]
	t = e.Deref(t)
	if _, ok := e.Float(t); !ok {
		return Failure()
	}
	e.Next()
	return Success()
}

func compound1(_ context.Context, e *Execution) Promise {
	t := e.tempVars[0]
	t = e.Deref(t)
	if _, ok := e.Functor(t); !ok {
		return Failure()
	}
	e.Next()
	return Success()
}

func ground1(_ context.Context, e *Execution) Promise {
	t := e.tempVars[0]
	t = e.Deref(t)
	vs := e.VariableSet(t)
	if len(vs) > 0 {
		return Failure()
	}
	e.Next()
	return Success()
}

func acyclicTerm1(_ context.Context, e *Execution) Promise {
	t := e.tempVars[0]
	t = e.Deref(t)
	if ok := e.Acyclic(t); !ok {
		return Failure()
	}
	e.Next()
	return Success()
}

func throw1(ctx context.Context, e *Execution) Promise {
	ball, cont := e.tempVars[1], e.tempVars[2]
	ball = e.Deref(ball)
	if _, ok := e.Variable(ball); ok {
		var err error
		err = &InstantiationError{
			Location: e.location,
		}
		ball, err = ErrorTerm(e.Arena, err)
		if err != nil {
			return Error(err)
		}
	}

	serialized := syntax.Serialize(e.Arena, ball)

	for cont := range contChain(e.Arena, cont) {
		if pi, ok := e.Functor(cont); !ok || pi.Name() != term.NewAtom("$to_catch") || pi.Arity() != 5 {
			continue
		}

		catcher, recovery, cutB, cont := e.Arg(cont, 0), e.Arg(cont, 1), e.Arg(cont, 2), e.Arg(cont, 3)

		b, _ := e.Integer(cutB)
		if err := e.unTrailTo(int(b)); err != nil {
			return Error(err)
		}

		ball, err := syntax.Deserialize(e.Arena, serialized)
		if err != nil {
			return Error(fmt.Errorf("parse serialized ball(%s): %w", serialized, err))
		}

		ok, err := e.Unify(catcher, ball)
		if err != nil {
			return Error(err)
		}
		if ok {
			e.tempVars[1] = recovery
			e.tempVars[2] = cont
			return call1(ctx, e)
		}
	}
	return Error(fmt.Errorf("unhandled exception: %s", &syntax.Formatter{Arena: e.Arena, Term: ball}))
}

func subsumesTerm2(_ context.Context, e *Execution) Promise {
	general, specific, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]
	trailTop := len(e.trail)
	vs := e.VariableSet(specific)

	// Same as unify_with_occurs_check(General, Specific).
	ok, err := e.Unify(general, specific)
	if err != nil {
		return Error(err)
	}
	ok = ok && e.Acyclic(general)

	// Checks if the temporary bindings keep Specific intact.
	for _, v := range vs {
		w := e.Deref(v)
		ok = ok && v == w
	}

	if err := e.unwindTrail(trailTop); err != nil {
		return Error(err)
	}

	if !ok {
		return Failure()
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func compare3(_ context.Context, e *Execution) Promise {
	order, x, y, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4]
	order, x, y = e.Deref(order), e.Deref(x), e.Deref(y)

	if _, ok := e.Variable(order); ok {
		// Do nothing.
	} else if a, ok := e.Atom(order); ok {
		switch a {
		case term.NewAtomRune('<'), term.NewAtomRune('>'), term.NewAtomRune('='):
			break
		default:
			return Error(&DomainError{
				ValidDomain: term.NewAtom("order"),
				Culprit:     syntax.Serialize(e.Arena, order),
				Location:    e.location,
			})
		}
	} else {
		return Error(&TypeError{
			ValidType: term.NewAtom("atom"),
			Culprit:   syntax.Serialize(e.Arena, order),
			Location:  e.location,
		})
	}

	var (
		a   term.Handle
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
		return Error(err)
	}

	ok, err := e.Unify(order, a)
	if err != nil {
		return Error(err)
	}
	if !ok {
		return Failure()
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func keySort2(ctx context.Context, e *Execution) Promise {
	pairs, sorted, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]

	var ps []term.Handle
	if err := e.mustBeList(pairs, func(pair term.Handle) error {
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
		return Error(err)
	}

	if _, err := e.canBeList(sorted, func(pair term.Handle) error {
		if f, ok := e.Functor(pair); !ok || f != term.NewFunctor(term.NewAtomRune('-'), 2) {
			return &TypeError{
				ValidType: term.NewAtom("pair"),
				Culprit:   syntax.Serialize(e.Arena, pair),
				Location:  e.location,
			}
		}
		return nil
	}); err != nil {
		return Error(err)
	}

	ts := make([]term.Handle, len(ps))
	for i, pair := range ps {
		key, value := e.Arg(pair, 0), e.Arg(pair, 1)
		p, err := e.PutInteger(int64(i))
		if err != nil {
			return Error(err)
		}
		t, err := e.PutCompound(term.NewAtomRune('t'), key, p, value)
		if err != nil {
			return Error(err)
		}
		ts[i] = t
	}

	slices.SortFunc(ts, e.Compare)

	kvs := make([]term.Handle, len(ts))
	for i, t := range ts {
		key, value := e.Arg(t, 0), e.Arg(t, 2)
		p, err := e.PutCompound(term.NewAtomRune('-'), key, value)
		if err != nil {
			return Error(err)
		}
		kvs[i] = p
	}

	l, err := e.PutList(kvs...)
	if err != nil {
		return Error(err)
	}

	ok, err := e.Unify(sorted, l)
	if err != nil {
		return Error(err)
	}
	if !ok {
		return Failure()
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func functor3(_ context.Context, e *Execution) Promise {
	t, name, arity, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4]
	t, name, arity = e.Deref(t), e.Deref(name), e.Deref(arity)

	if _, ok := e.Variable(t); ok {
		if _, ok := e.Variable(arity); ok {
			return Error(&InstantiationError{
				Location: e.location,
			})
		} else if a, ok := e.Integer(arity); ok {
			if a < 0 {
				return Error(&DomainError{
					ValidDomain: term.NewAtom("not_less_than_zero"),
					Culprit:     syntax.Serialize(e.Arena, arity),
					Location:    e.location,
				})
			}

			if _, ok := e.Variable(name); ok {
				return Error(&InstantiationError{
					Location: e.location,
				})
			} else if _, ok := e.Functor(name); ok {
				return Error(&TypeError{
					ValidType: term.NewAtom("atomic"),
					Culprit:   syntax.Serialize(e.Arena, name),
					Location:  e.location,
				})
			}

			if a == 0 {
				ok, err := e.Unify(t, name)
				if !ok || err != nil {
					return Error(err)
				}
			} else if n, ok := e.Atom(name); ok {
				c, err := e.PutCompoundWithFreshVars(term.NewFunctor(n, int(a)))
				if err != nil {
					return Error(err)
				}

				ok, err = e.Unify(t, c)
				if !ok || err != nil {
					return Error(err)
				}
			} else {
				return Error(&TypeError{
					ValidType: term.NewAtom("atom"),
					Culprit:   syntax.Serialize(e.Arena, name),
					Location:  e.location,
				})
			}
		} else {
			return Error(&TypeError{
				ValidType: term.NewAtom("integer"),
				Culprit:   syntax.Serialize(e.Arena, arity),
				Location:  e.location,
			})
		}
	} else if f, ok := e.Functor(t); ok {
		n, err := e.PutAtom(f.Name())
		if err != nil {
			return Error(err)
		}

		ok, err := e.Unify(name, n)
		if !ok || err != nil {
			return Error(err)
		}

		a, err := e.PutInteger(int64(f.Arity()))
		if err != nil {
			return Error(err)
		}

		ok, err = e.Unify(arity, a)
		if !ok || err != nil {
			return Error(err)
		}
	} else { // atomic
		ok, err := e.Unify(name, t)
		if !ok || err != nil {
			return Error(err)
		}

		a, err := e.PutInteger(int64(0))
		if err != nil {
			return Error(err)
		}

		ok, err = e.Unify(arity, a)
		if !ok || err != nil {
			return Error(err)
		}
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func arg3(_ context.Context, e *Execution) Promise {
	nth, t, arg, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4]
	nth, t, arg = e.Deref(nth), e.Deref(t), e.Deref(arg)

	if _, ok := e.Variable(t); ok {
		return Error(&InstantiationError{
			Location: e.location,
		})
	} else if f, ok := e.Functor(t); ok {
		if _, ok := e.Variable(nth); ok {
			return Error(&InstantiationError{
				Location: e.location,
			})
		} else if n, ok := e.Integer(nth); ok {
			switch {
			case n == 0, int(n) > f.Arity():
				return Failure()
			case n < 0:
				return Error(&DomainError{
					ValidDomain: term.NewAtom("not_less_than_zero"),
					Culprit:     syntax.Serialize(e.Arena, nth),
					Location:    e.location,
				})
			default:
				a := e.Arg(t, int(n)-1)
				ok, err := e.Unify(arg, a)
				if !ok || err != nil {
					return Error(err)
				}
			}

		} else {
			return Error(&TypeError{
				ValidType: term.NewAtom("integer"),
				Culprit:   syntax.Serialize(e.Arena, nth),
				Location:  e.location,
			})
		}
	} else {
		return Error(&TypeError{
			ValidType: term.NewAtom("compound"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		})
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func univ2(_ context.Context, e *Execution) Promise {
	t, list, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]
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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func (e *Execution) univAtomic(t, list, cont term.Handle) Promise {
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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func (e *Execution) univVariable(t, list, cont term.Handle) Promise {
	var elems []term.Handle
	if err := e.mustBeNonEmptyList(list, func(elem term.Handle) error {
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

			e.tempVars[1] = cont
			e.Next()
			return Success()
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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func copyTerm2(_ context.Context, e *Execution) Promise {
	t1, t2, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]

	c, err := term.RenamedCopy(e.Arena, e.Arena, t1)
	if err != nil {
		return Error(err)
	}

	ok, err := e.Unify(t2, c)
	if !ok || err != nil {
		return Error(err)
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func termVariables2(_ context.Context, e *Execution) Promise {
	t, vars, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]
	t, vars = e.Deref(t), e.Deref(vars)

	if _, err := e.canBeList(vars, nil); err != nil {
		return Error(err)
	}

	ret, err := e.PutList(slices.Collect(e.WitnessVariables(t))...)
	if err != nil {
		return Error(err)
	}

	ok, err := e.Unify(ret, vars)
	if !ok || err != nil {
		return Error(err)
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func clause2(ctx context.Context, e *Execution) Promise {
	head, body, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]

	pi, err := e.mustBeCallable(head)
	if err != nil {
		return Error(err)
	}

	if _, _, err := e.canBeCallable(body); err != nil {
		return Error(err)
	}

	bpi := term.NewFunctor(pi.Name(), pi.Arity()+1)
	p, ok := e.Predicates[bpi]
	if !ok {
		return Failure()
	}

	if !p.Public {
		f, err := e.PutFunctor(pi)
		if err != nil {
			return Error(err)
		}

		return Error(&PermissionError{
			Operation:      term.NewAtom("access"),
			PermissionType: term.NewAtom("private_procedure"),
			Culprit:        syntax.Serialize(e.Arena, f),
			Location:       e.location,
		})
	}

	return Delay(func(yield func(Promise) bool) {
		for r, err := range e.DB.Select(ctx, e.Arena, pi, e.CurrentTime) {
			if err != nil {
				_ = yield(Error(err))
				return
			}

			ok, err := e.Unify(head, r.Head)
			if err != nil {
				_ = yield(Error(err))
				return
			}
			if !ok {
				continue
			}

			ok, err = e.Unify(body, r.Body)
			if err != nil {
				_ = yield(Error(err))
				return
			}
			if !ok {
				continue
			}

			e.tempVars[1] = cont
			e.Next()
			if !yield(Success()) {
				return
			}
		}
	})
}

func currentPredicate1(ctx context.Context, e *Execution) Promise {
	predIndicator, cont := e.tempVars[1], e.tempVars[2]
	predIndicator = e.Deref(predIndicator)

	switch pi, ok, err := e.canBePredicateIndicator(predIndicator); {
	case err != nil:
		return Error(err)
	case ok:
		bpi := term.NewFunctor(pi.Name(), pi.Arity()+1)
		p, _ := e.Predicates[bpi]
		if p.BuiltIn {
			return Failure()
		}

		e.tempVars[1] = cont
		e.Next()
		return Success()
	}

	pis := slices.Collect(func(yield func(pi term.Functor) bool) {
		for bpi, p := range e.Predicates {
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

	return Delay(func(yield func(Promise) bool) {
		for _, pi := range pis {
			c, err := e.PutFunctor(pi)
			if err != nil {
				_ = yield(Error(err))
				return
			}

			ok, err := e.Unify(predIndicator, c)
			if err != nil {
				_ = yield(Error(err))
				return
			}
			if !ok {
				if !yield(Failure()) {
					return
				}
				continue
			}

			e.tempVars[1] = cont
			e.Next()
			if !yield(Success()) {
				return
			}
		}
	})
}

func assertA1(ctx context.Context, e *Execution) Promise {
	return assert1(ctx, e, db.DB.InsertBefore)
}

func assertZ1(ctx context.Context, e *Execution) Promise {
	return assert1(ctx, e, db.DB.InsertAfter)
}

func assert1(ctx context.Context, e *Execution, fn func(db db.DB, ctx context.Context, arena *term.Arena, record db.Record) error) Promise {
	t, cont := e.tempVars[1], e.tempVars[2]
	t = e.Deref(t)

	if _, ok := e.Variable(t); ok {
		return Error(&InstantiationError{
			Location: e.location,
		})
	}

	var (
		pi   term.Functor
		head term.Handle
		body term.Handle
		err  error
	)
	pi, ok := e.Functor(t, term.AllowAtom(true))
	if !ok {
		return Error(&TypeError{
			ValidType: term.NewAtom("callable"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		})
	}
	if pi == term.NewFunctor(term.NewAtom(":-"), 2) {
		head, body = e.Arg(t, 0), e.Arg(t, 1)
		pi, ok = e.Functor(head, term.AllowAtom(true))
		if !ok {
			return Error(&TypeError{
				ValidType: term.NewAtom("callable"),
				Culprit:   syntax.Serialize(e.Arena, t),
				Location:  e.location,
			})
		}

		if _, ok := e.Functor(body, term.AllowAtom(true)); !ok {
			return Error(&TypeError{
				ValidType: term.NewAtom("callable"),
				Culprit:   syntax.Serialize(e.Arena, body),
				Location:  e.location,
			})
		}
	} else {
		head = t
		body, err = e.PutAtom(term.NewAtom("true"))
		if err != nil {
			return Error(err)
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
			return Error(err)
		}
		return Error(&PermissionError{
			Operation:      term.NewAtom("modify"),
			PermissionType: term.NewAtom("static_procedure"),
			Culprit:        syntax.Serialize(e.Arena, c),
			Location:       e.location,
		})
	}

	if err := fn(e.DB, ctx, e.Arena, db.Record{
		Head:      head,
		Body:      body,
		CreatedAt: e.CurrentTime,
	}); err != nil {
		return Error(err)
	}
	e.CurrentTime++

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func retract1(ctx context.Context, e *Execution) Promise {
	t, cont := e.tempVars[1], e.tempVars[2]
	t = e.Deref(t)

	h, err := e.PutVariable()
	if err != nil {
		return Error(err)
	}

	b, err := e.PutVariable()
	if err != nil {
		return Error(err)
	}

	c, err := e.PutCompound(atomNeck, h, b)
	if err != nil {
		return Error(err)
	}

	ok, err := e.Unify(c, t)
	if err != nil {
		return Error(err)
	}
	if !ok {
		h = t
		b, err = e.PutAtom(term.NewAtom("true"))
		if err != nil {
			return Error(err)
		}
	}

	h, b = e.Deref(h), e.Deref(b)

	pi, err := e.mustBeCallable(h)
	if err != nil {
		return Error(err)
	}

	bpi := term.NewFunctor(pi.Name(), pi.Arity()+1)
	if p, ok := e.Predicates[bpi]; ok && !p.Dynamic {
		c, err := e.PutFunctor(pi)
		if err != nil {
			return Error(err)
		}
		return Error(&PermissionError{
			Operation:      term.NewAtom("modify"),
			PermissionType: term.NewAtom("static_procedure"),
			Culprit:        syntax.Serialize(e.Arena, c),
			Location:       e.location,
		})
	}

	return Delay(func(yield func(Promise) bool) {
		before := e.CurrentTime
		e.CurrentTime++
		for r := range e.DB.Select(ctx, e.Arena, pi, before) {
			ok, err := e.Unify(r.Head, h)
			if err != nil {
				_ = yield(Error(err))
				return
			}
			if !ok {
				if !yield(Failure()) {
					return
				}
				continue
			}

			ok, err = e.Unify(r.Body, b)
			if err != nil {
				_ = yield(Error(err))
				return
			}
			if !ok {
				if !yield(Failure()) {
					return
				}
				continue
			}

			if err := e.DB.Delete(ctx, r.ID, before); err != nil {
				_ = yield(Error(err))
				return
			}
			e.tempVars[1] = cont
			e.Next()
			if !yield(Success()) {
				return
			}
		}
	})
}

func abolish1(ctx context.Context, e *Execution) Promise {
	pred, cont := e.tempVars[1], e.tempVars[2]
	pred = e.Deref(pred)

	pi, err := e.mustBePredicateIndicator(pred)
	if err != nil {
		return Error(err)
	}

	bpi := term.NewFunctor(pi.Name(), pi.Arity()+1)
	if p, ok := e.Predicates[bpi]; ok {
		if !p.Dynamic {
			c, err := e.PutFunctor(pi)
			if err != nil {
				return Error(err)
			}
			return Error(&PermissionError{
				Operation:      term.NewAtom("modify"),
				PermissionType: term.NewAtom("static_procedure"),
				Culprit:        syntax.Serialize(e.Arena, c),
				Location:       e.location,
			})
		}
		for r := range e.DB.Select(ctx, e.Arena, pi, e.CurrentTime) {
			if err := e.DB.Delete(ctx, r.ID, e.CurrentTime); err != nil {
				return Error(err)
			}
		}
		delete(e.Predicates, bpi)
		e.CurrentTime++
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func findAll3(ctx context.Context, e *Execution) Promise {
	template, goal, instances, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4]

	if _, err := e.canBeList(instances, nil); err != nil {
		return Error(err)
	}

	var elems []term.Handle
	if err := e.FindAll(ctx, &elems, template, goal); err != nil {
		return Error(err)
	}

	l, err := e.PutList(elems...)
	if err != nil {
		return Error(err)
	}

	ok, err := e.Unify(instances, l)
	if err != nil {
		return Error(err)
	}
	if !ok {
		return Failure()
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func bagOf3(ctx context.Context, e *Execution) Promise {
	return collectionOf(ctx, e, func(ts []term.Handle) (term.Handle, error) {
		return e.PutList(ts...)
	})
}

func setOf3(ctx context.Context, e *Execution) Promise {
	return collectionOf(ctx, e, func(ts []term.Handle) (term.Handle, error) {
		slices.SortFunc(ts, e.Compare)
		ts = slices.CompactFunc(ts, func(a, b term.Handle) bool {
			return e.Compare(a, b) == 0
		})
		return e.PutList(ts...)
	})
}

func collectionOf(ctx context.Context, e *Execution, agg func([]term.Handle) (term.Handle, error)) Promise {
	template, goal, instances, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4]

	if _, err := e.canBeList(instances, nil); err != nil {
		return Error(err)
	}

	fvs := e.FreeVariableSet(goal, template)
	witness, err := e.PutCompound(term.NewAtom("$witness"), fvs...)
	if err != nil {
		return Error(err)
	}

	template, err = e.PutCompound(term.NewAtomRune('+'), witness, template)
	if err != nil {
		return Error(err)
	}

	for {
		goal = e.Deref(goal)
		f, ok := e.Functor(goal)
		if !ok || f != term.NewFunctor(term.NewAtomRune('^'), 2) {
			break
		}
		goal = e.Arg(goal, 1)
	}

	var s []term.Handle
	if err := e.FindAll(ctx, &s, template, goal); err != nil {
		return Error(err)
	}

	return Delay(func(yield func(Promise) bool) {
		for len(s) > 0 {
			var wt term.Handle
			wt, s = s[0], s[1:]
			w, t := e.Arg(wt, 0), e.Arg(wt, 1) // W+T
			wl, tl := []term.Handle{w}, []term.Handle{t}
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
					_ = yield(Error(err))
					return
				}
			}
			a, err := agg(tl)
			if err != nil {
				_ = yield(Error(err))
				return
			}
			ok, err := e.Unify(instances, a)
			if err != nil {
				_ = yield(Error(err))
				return
			}
			if !ok {
				if !yield(Failure()) {
					return
				}
				continue
			}

			e.tempVars[1] = cont
			e.Next()
			if !yield(Success()) {
				return
			}
		}
	})
}

func (e *Execution) FindAll(ctx context.Context, out *[]term.Handle, template term.Handle, goal term.Handle) error {
	// Resulting instances are not accessible after each run.
	// So, escape them to a secondary memory arena for a moment, then bring them back.

	heapTop := len(e.TempArena.Heap)
	defer func() {
		e.TempArena.Heap = e.TempArena.Heap[:heapTop]
	}()

	var instances []term.Handle
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

func currentInput1(ctx context.Context, e *Execution) Promise {
	s, cont := e.tempVars[1], e.tempVars[2]

	if e.Input == (term.Handle{}) {
		return Failure()
	}

	ok, err := e.Unify(s, e.Input)
	if err != nil {
		return Error(err)
	}
	if !ok {
		return Failure()
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func currentOutput1(ctx context.Context, e *Execution) Promise {
	s, cont := e.tempVars[1], e.tempVars[2]

	if e.Output == (term.Handle{}) {
		return Failure()
	}

	ok, err := e.Unify(s, e.Output)
	if err != nil {
		return Error(err)
	}
	if !ok {
		return Failure()
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func setInput1(ctx context.Context, e *Execution) Promise {
	sOrA, cont := e.tempVars[1], e.tempVars[2]

	s, err := e.mustBeStreamOrAlias(sOrA)
	if err != nil {
		return Error(err)
	}

	for stream := range e.OpenStreams() {
		if str, _ := e.Stream(stream); str == s {
			e.Input = stream
			break
		}
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func setOutput1(ctx context.Context, e *Execution) Promise {
	sOrA, cont := e.tempVars[1], e.tempVars[2]

	s, err := e.mustBeStreamOrAlias(sOrA)
	if err != nil {
		return Error(err)
	}

	for stream := range e.OpenStreams() {
		if str, _ := e.Stream(stream); str == s {
			e.Output = stream
			break
		}
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func open4(ctx context.Context, e *Execution) Promise {
	sourceSink, mode, stream, options, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4], e.tempVars[5]

	filename, err := e.mustBeSourceSink(sourceSink)
	if err != nil {
		return Error(err)
	}

	m, err := e.mustBeMode(mode)
	if err != nil {
		return Error(err)
	}

	if _, err := e.canBeStream(stream); err != nil {
		return Error(err)
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
	f, err := e.FS.OpenFile(filename, flag, 0644)
	switch {
	case errors.Is(err, fs.ErrNotExist):
	case errors.Is(err, fs.ErrPermission):
	case err != nil:
		return Error(err)
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
			return Error(err)
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

	if err := e.mustBeList(options, func(elem term.Handle) error {
		return e.handleStreamOption(&s, elem)
	}); err != nil {
		return Error(err)
	}

	t, err := e.PutStream(s)
	if err != nil {
		return Error(err)
	}

	ok, err := e.Unify(stream, t)
	if err != nil {
		return Error(err)
	}
	if !ok {
		return Failure()
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func (e *Execution) handleStreamOption(s *term.Stream, o term.Handle) error {
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

func (e *Execution) handleStreamOptionAlias(s *term.Stream, o term.Handle) error {
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
			Location:    0,
		}
	}
	if i := slices.IndexFunc(e.Streams, func(s term.Stream) bool {
		return s.Alias == a
	}); i >= 0 {
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

func (e *Execution) handleStreamOptionType(s *term.Stream, o term.Handle) error {
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

func (e *Execution) handleStreamOptionReposition(s *term.Stream, o term.Handle) error {
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

func (e *Execution) handleStreamOptionEOFAction(s *term.Stream, o term.Handle) error {
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

func close2(ctx context.Context, e *Execution) Promise {
	sOrA, options, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]

	s, err := e.mustBeStreamOrAlias(sOrA)
	if err != nil {
		return Error(err)
	}

	var force bool
	if err := e.mustBeList(options, func(o term.Handle) error {
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
		return Error(err)
	}

	if err := s.Close(); err != nil && !force {
		return Error(err)
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func flushOutput1(ctx context.Context, e *Execution) Promise {
	sOrA, cont := e.tempVars[1], e.tempVars[2]

	s, err := e.mustBeStreamOrAlias(sOrA)
	if err != nil {
		return Error(err)
	}

	switch err := s.Flush(); {
	case errors.Is(err, term.ErrWrongIOMode):
		return Error(&PermissionError{
			Operation:      term.NewAtom("operation"),
			PermissionType: term.NewAtom("stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		})
	case err != nil:
		return Error(err)
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func streamProperty2(ctx context.Context, e *Execution) Promise {
	stream, property, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]

	stream = e.Deref(stream)

	var streams iter.Seq[term.Handle]
	s, err := e.canBeStream(stream)
	if err != nil {
		return Error(err)
	}
	if s == nil {
		streams = e.OpenStreams()
	} else {
		streams = singleton(stream)
	}

	if err := e.canBeStreamProperty(property); err != nil {
		return Error(err)
	}

	return Delay(func(yield func(Promise) bool) {
		for s := range streams {
			st, ok := e.Stream(s)
			if !ok {
				continue
			}
			for p, err := range e.properties(st) {
				if err != nil {
					_ = yield(Error(err))
					return
				}

				ok, err := e.Unify(stream, s)
				if err != nil {
					_ = yield(Error(err))
					return
				}
				if !ok {
					if !yield(Failure()) {
						return
					}
					continue
				}

				ok, err = e.Unify(property, p)
				if err != nil {
					_ = yield(Error(err))
					return
				}
				if !ok {
					if !yield(Failure()) {
						return
					}
					continue
				}

				e.tempVars[1] = cont
				e.Next()
				if !yield(Success()) {
					return
				}
			}
		}
	})
}

func (e *Execution) properties(s *term.Stream) iter.Seq2[term.Handle, error] {
	return func(yield func(term.Handle, error) bool) {
		if n := s.Name(); n != "" {
			n, err := e.PutAtom(term.NewAtom(n))
			if err != nil {
				_ = yield(term.Handle{}, err)
				return
			}
			c, err := e.PutCompound(term.NewAtom("file_name"), n)
			if err != nil {
				_ = yield(term.Handle{}, err)
				return
			}
			if !yield(c, nil) {
				return
			}
		}

		m, err := e.PutAtom(term.NewAtom(s.Mode.String()))
		if err != nil {
			_ = yield(term.Handle{}, err)
			return
		}
		c, err := e.PutCompound(term.NewAtom("mode"), m)
		if err != nil {
			_ = yield(term.Handle{}, err)
			return
		}
		if !yield(c, nil) {
			return
		}

		switch s.Mode {
		case term.Read:
			a, err := e.PutAtom(term.NewAtom("input"))
			if err != nil {
				_ = yield(term.Handle{}, err)
				return
			}
			if !yield(a, nil) {
				return
			}
		case term.Write, term.Append:
			a, err := e.PutAtom(term.NewAtom("output"))
			if err != nil {
				_ = yield(term.Handle{}, err)
				return
			}
			if !yield(a, nil) {
				return
			}
		}

		if s.Alias != (term.Atom{}) {
			a, err := e.PutAtom(s.Alias)
			if err != nil {
				_ = yield(term.Handle{}, err)
				return
			}
			c, err := e.PutCompound(term.NewAtom("alias"), a)
			if err != nil {
				_ = yield(term.Handle{}, err)
				return
			}
			if !yield(c, nil) {
				return
			}
		}

		{
			p, err := e.PutInteger(s.Position)
			if err != nil {
				_ = yield(term.Handle{}, err)
				return
			}
			c, err := e.PutCompound(term.NewAtom("position"), p)
			if err != nil {
				_ = yield(term.Handle{}, err)
				return
			}
			if !yield(c, nil) {
				return
			}
		}

		{
			eos, err := e.PutAtom(term.NewAtom(s.EndOfStream.String()))
			if err != nil {
				_ = yield(term.Handle{}, err)
				return
			}
			c, err := e.PutCompound(term.NewAtom("end_of_stream"), eos)
			if err != nil {
				_ = yield(term.Handle{}, err)
				return
			}
			if !yield(c, nil) {
				return
			}
		}

		{
			a, err := e.PutAtom(term.NewAtom(s.EOFAction.String()))
			if err != nil {
				_ = yield(term.Handle{}, err)
				return
			}
			c, err := e.PutCompound(term.NewAtom("eof_action"), a)
			if err != nil {
				_ = yield(term.Handle{}, err)
				return
			}
			if !yield(c, nil) {
				return
			}
		}

		var t term.Handle
		if s.Reposition {
			t, err = e.PutAtom(term.NewAtom("true"))
		} else {
			t, err = e.PutAtom(term.NewAtom("false"))
		}
		if err != nil {
			_ = yield(term.Handle{}, err)
			return
		}
		c, err = e.PutCompound(term.NewAtom("reposition"), t)
		if err != nil {
			_ = yield(term.Handle{}, err)
			return
		}
		if !yield(c, nil) {
			return
		}

		t, err = e.PutAtom(term.NewAtom(s.StreamType.String()))
		if err != nil {
			_ = yield(term.Handle{}, err)
			return
		}
		c, err = e.PutCompound(term.NewAtom("type"), t)
		if err != nil {
			_ = yield(term.Handle{}, err)
			return
		}
		if !yield(c, nil) {
			return
		}
	}
}

func setStreamPosition2(ctx context.Context, e *Execution) Promise {
	sOrA, position, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]

	s, err := e.mustBeStreamOrAlias(sOrA)
	if err != nil {
		return Error(err)
	}

	p, err := e.mustBeInteger(position)
	if err != nil {
		return Error(err)
	}

	switch _, err := s.Seek(p, 0); {
	case errors.Is(err, term.ErrReposition):
		return Error(&PermissionError{
			Operation:      term.NewAtom("reposition"),
			PermissionType: term.NewAtom("stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		})
	case err != nil:
		return Error(err)
	default:
		e.tempVars[1] = cont
		e.Next()
		return Success()
	}
}

func getChar2(ctx context.Context, e *Execution) Promise {
	sOrA, inChar, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]

	s, err := e.mustBeStreamOrAlias(sOrA)
	if err != nil {
		return Error(err)
	}

	if _, _, err := e.canBeInChar(inChar); err != nil {
		return Error(err)
	}

	var c term.Handle
	switch r, _, err := s.ReadRune(); {
	case errors.Is(err, io.EOF):
		c, err = e.PutAtom(term.NewAtom("end_of_file"))
		if err != nil {
			return Error(err)
		}
	case errors.Is(err, term.ErrWrongIOMode):
		return Error(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		})
	case errors.Is(err, term.ErrWrongStreamType):
		return Error(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("binary_stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		})
	case errors.Is(err, term.ErrPastEndOfStream):
		return Error(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("past_end_of_stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		})
	case err != nil:
		return Error(err)
	default:
		c, err = e.PutAtom(term.NewAtomRune(r))
		if err != nil {
			return Error(err)
		}
	}

	ok, err := e.Unify(inChar, c)
	if err != nil {
		return Error(err)
	}
	if !ok {
		return Failure()
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func getCode2(ctx context.Context, e *Execution) Promise {
	sOrA, inCharCode, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]

	s, err := e.mustBeStreamOrAlias(sOrA)
	if err != nil {
		return Error(err)
	}

	if _, _, err := e.canBeInCharCode(inCharCode); err != nil {
		return Error(err)
	}

	var c term.Handle
	switch r, _, err := s.ReadRune(); {
	case errors.Is(err, io.EOF):
		c, err = e.PutInteger(-1)
		if err != nil {
			return Error(err)
		}
	case errors.Is(err, term.ErrWrongIOMode):
		return Error(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		})
	case errors.Is(err, term.ErrWrongStreamType):
		return Error(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("binary_stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		})
	case errors.Is(err, term.ErrPastEndOfStream):
		return Error(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("past_end_of_stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		})
	case err != nil:
		return Error(err)
	default:
		c, err = e.PutInteger(int64(r))
		if err != nil {
			return Error(err)
		}
	}

	ok, err := e.Unify(inCharCode, c)
	if err != nil {
		return Error(err)
	}
	if !ok {
		return Failure()
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func peekChar2(ctx context.Context, e *Execution) Promise {
	sOrA, inChar, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]

	s, err := e.mustBeStreamOrAlias(sOrA)
	if err != nil {
		return Error(err)
	}

	if _, _, err := e.canBeInChar(inChar); err != nil {
		return Error(err)
	}

	var c term.Handle
	switch r, _, err := s.ReadRune(); {
	case errors.Is(err, io.EOF):
		c, err = e.PutAtom(term.NewAtom("end_of_file"))
		if err != nil {
			return Error(err)
		}
	case errors.Is(err, term.ErrWrongIOMode):
		return Error(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		})
	case errors.Is(err, term.ErrWrongStreamType):
		return Error(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("binary_stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		})
	case errors.Is(err, term.ErrPastEndOfStream):
		return Error(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("past_end_of_stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		})
	case err != nil:
		return Error(err)
	default:
		if err := s.UnreadRune(); err != nil {
			return Error(err)
		}

		if r == unicode.ReplacementChar {
			return Error(&RepresentationError{
				Flag:     term.NewAtom("character"),
				Location: e.location,
			})
		}

		c, err = e.PutAtom(term.NewAtomRune(r))
		if err != nil {
			return Error(err)
		}
	}

	ok, err := e.Unify(inChar, c)
	if err != nil {
		return Error(err)
	}
	if !ok {
		return Failure()
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func peekCode2(ctx context.Context, e *Execution) Promise {
	sOrA, inCharCode, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]

	s, err := e.mustBeStreamOrAlias(sOrA)
	if err != nil {
		return Error(err)
	}

	if _, _, err := e.canBeInCharCode(inCharCode); err != nil {
		return Error(err)
	}

	var c term.Handle
	switch r, _, err := s.ReadRune(); {
	case errors.Is(err, io.EOF):
		c, err = e.PutInteger(-1)
		if err != nil {
			return Error(err)
		}
	case errors.Is(err, term.ErrWrongIOMode):
		return Error(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		})
	case errors.Is(err, term.ErrWrongStreamType):
		return Error(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("binary_stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		})
	case errors.Is(err, term.ErrPastEndOfStream):
		return Error(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("past_end_of_stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		})
	case err != nil:
		return Error(err)
	default:
		if err := s.UnreadRune(); err != nil {
			return Error(err)
		}

		if r == unicode.ReplacementChar {
			return Error(&RepresentationError{
				Flag:     term.NewAtom("in_character_code"),
				Location: e.location,
			})
		}

		c, err = e.PutInteger(int64(r))
		if err != nil {
			return Error(err)
		}
	}

	ok, err := e.Unify(inCharCode, c)
	if err != nil {
		return Error(err)
	}
	if !ok {
		return Failure()
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func putChar2(ctx context.Context, e *Execution) Promise {
	sOrA, char, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]

	s, err := e.mustBeStreamOrAlias(sOrA)
	if err != nil {
		return Error(err)
	}

	r, err := e.mustBeChar(char)
	if err != nil {
		return Error(err)
	}

	switch _, err := s.WriteRune(r); {
	case errors.Is(err, term.ErrWrongIOMode):
		return Error(&PermissionError{
			Operation:      term.NewAtom("output"),
			PermissionType: term.NewAtom("stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		})
	case errors.Is(err, term.ErrWrongStreamType):
		return Error(&PermissionError{
			Operation:      term.NewAtom("output"),
			PermissionType: term.NewAtom("binary_stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		})
	case err != nil:
		return Error(err)
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func putCode2(ctx context.Context, e *Execution) Promise {
	sOrA, code, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]

	s, err := e.mustBeStreamOrAlias(sOrA)
	if err != nil {
		return Error(err)
	}

	r, err := e.mustBeCharCode(code)
	if err != nil {
		return Error(err)
	}

	switch _, err := s.WriteRune(r); {
	case errors.Is(err, term.ErrWrongIOMode):
		return Error(&PermissionError{
			Operation:      term.NewAtom("output"),
			PermissionType: term.NewAtom("stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		})
	case errors.Is(err, term.ErrWrongStreamType):
		return Error(&PermissionError{
			Operation:      term.NewAtom("output"),
			PermissionType: term.NewAtom("binary_stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		})
	case err != nil:
		return Error(err)
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func getByte2(ctx context.Context, e *Execution) Promise {
	sOrA, inByte, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]

	s, err := e.mustBeStreamOrAlias(sOrA)
	if err != nil {
		return Error(err)
	}

	if _, _, err := e.canBeInByte(inByte); err != nil {
		return Error(err)
	}

	var n int64
	switch b, err := s.ReadByte(); {
	case errors.Is(err, io.EOF):
		n = -1
	case errors.Is(err, term.ErrWrongIOMode):
		return Error(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		})
	case errors.Is(err, term.ErrWrongStreamType):
		return Error(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("text_stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		})
	case errors.Is(err, term.ErrPastEndOfStream):
		return Error(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("past_end_of_stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		})
	case err != nil:
		return Error(err)
	default:
		n = int64(b)
	}

	i, err := e.PutInteger(n)
	if err != nil {
		return Error(err)
	}

	ok, err := e.Unify(inByte, i)
	if err != nil {
		return Error(err)
	}
	if !ok {
		return Failure()
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func peekByte2(ctx context.Context, e *Execution) Promise {
	sOrA, inByte, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]

	s, err := e.mustBeStreamOrAlias(sOrA)
	if err != nil {
		return Error(err)
	}

	if _, _, err := e.canBeInByte(inByte); err != nil {
		return Error(err)
	}

	var n int64
	switch b, err := s.ReadByte(); {
	case errors.Is(err, io.EOF):
		n = -1
	case errors.Is(err, term.ErrWrongIOMode):
		return Error(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		})
	case errors.Is(err, term.ErrWrongStreamType):
		return Error(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("text_stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		})
	case errors.Is(err, term.ErrPastEndOfStream):
		return Error(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("past_end_of_stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		})
	case err != nil:
		return Error(err)
	default:
		if err := s.UnreadByte(); err != nil {
			return Error(err)
		}

		n = int64(b)
	}

	i, err := e.PutInteger(n)
	if err != nil {
		return Error(err)
	}

	ok, err := e.Unify(inByte, i)
	if err != nil {
		return Error(err)
	}
	if !ok {
		return Failure()
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func putByte2(ctx context.Context, e *Execution) Promise {
	sOrA, byt, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]

	s, err := e.mustBeStreamOrAlias(sOrA)
	if err != nil {
		return Error(err)
	}

	b, err := e.mustBeByte(byt)
	if err != nil {
		return Error(err)
	}

	switch err := s.WriteByte(b); {
	case errors.Is(err, term.ErrWrongIOMode):
		return Error(&PermissionError{
			Operation:      term.NewAtom("output"),
			PermissionType: term.NewAtom("stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		})
	case errors.Is(err, term.ErrWrongStreamType):
		return Error(&PermissionError{
			Operation:      term.NewAtom("output"),
			PermissionType: term.NewAtom("text_stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		})
	case err != nil:
		return Error(err)
	default:
		e.tempVars[1] = cont
		e.Next()
		return Success()
	}
}

func readTerm3(ctx context.Context, e *Execution) Promise {
	sOrA, t, options, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4]

	s, err := e.mustBeStreamOrAlias(sOrA)
	if err != nil {
		return Error(err)
	}

	var opts readTermOptions
	if err := e.mustBeList(options, func(elem term.Handle) error {
		return e.readTermOption(&opts, elem)
	}); err != nil {
		return Error(err)
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
			return Error(err)
		}

		ok, err := e.Unify(t, eof)
		if err != nil {
			return Error(err)
		}
		if !ok {
			return Failure()
		}
	case errors.Is(err, term.ErrWrongIOMode):
		return Error(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		})
	case errors.Is(err, term.ErrWrongStreamType):
		return Error(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("text_stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		})
	case errors.Is(err, term.ErrPastEndOfStream):
		return Error(&PermissionError{
			Operation:      term.NewAtom("input"),
			PermissionType: term.NewAtom("past_end_of_stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		})
	case errors.As(err, &unexpectedTokenError), errors.Is(err, syntax.ErrUnexpectedEOF):
		return Error(&SyntaxError{
			ImpDepAtom: term.NewAtom(err.Error()),
			Location:   e.location,
		})
	case err != nil:
		return Error(err)
	default:
		var (
			singletons    []term.Handle
			variables     []term.Handle
			variableNames []term.Handle
		)
		for _, v := range vars {
			if opts.singletons != (term.Handle{}) && v.Count == 1 && v.Name != "_" {
				singletons = append(singletons, v.Variable)
			}
			if opts.variables != (term.Handle{}) {
				variables = append(variables, v.Variable)
			}
			if opts.variableNames != (term.Handle{}) && v.Name != "_" {
				n, err := e.PutAtom(term.NewAtom(v.Name))
				if err != nil {
					return Error(err)
				}
				c, err := e.PutCompound(term.NewAtomRune('='), n, v.Variable)
				if err != nil {
					return Error(err)
				}
				variableNames = append(variableNames, c)
			}
		}

		ok, err := e.Unify(t, p)
		if err != nil {
			return Error(err)
		}
		if !ok {
			return Failure()
		}

		if opts.singletons != (term.Handle{}) {
			l, err := e.PutList(singletons...)
			if err != nil {
				return Error(err)
			}

			ok, err := e.Unify(opts.singletons, l)
			if err != nil {
				return Error(err)
			}
			if !ok {
				return Failure()
			}
		}

		if opts.variables != (term.Handle{}) {
			l, err := e.PutList(variables...)
			if err != nil {
				return Error(err)
			}

			ok, err := e.Unify(opts.variables, l)
			if err != nil {
				return Error(err)
			}
			if !ok {
				return Failure()
			}
		}

		if opts.variableNames != (term.Handle{}) {
			l, err := e.PutList(variableNames...)
			if err != nil {
				return Error(err)
			}

			ok, err := e.Unify(opts.variableNames, l)
			if err != nil {
				return Error(err)
			}
			if !ok {
				return Failure()
			}
		}
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

type readTermOptions struct {
	singletons    term.Handle
	variables     term.Handle
	variableNames term.Handle
}

func (e *Execution) readTermOption(opts *readTermOptions, option term.Handle) error {
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

func writeTerm3(ctx context.Context, e *Execution) Promise {
	sOrA, t, options, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4]

	s, err := e.mustBeStreamOrAlias(sOrA)
	if err != nil {
		return Error(err)
	}

	formatter := syntax.Formatter{
		Arena: e.Arena,
		Term:  t,
		Ops:   &e.Ops,
	}
	if err := e.mustBeList(options, func(o term.Handle) error {
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
		return Error(err)
	}

	w, err := s.TextWriter()
	switch {
	case errors.Is(err, term.ErrWrongIOMode):
		return Error(&PermissionError{
			Operation:      term.NewAtom("output"),
			PermissionType: term.NewAtom("stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		})
	case errors.Is(err, term.ErrWrongStreamType):
		return Error(&PermissionError{
			Operation:      term.NewAtom("output"),
			PermissionType: term.NewAtom("binary_stream"),
			Culprit:        syntax.Serialize(e.Arena, sOrA),
			Location:       e.location,
		})
	case err != nil:
		return Error(err)
	}

	if _, err := fmt.Fprintf(w, "%s", &formatter); err != nil {
		return Error(err)
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func (e *Execution) writeTermOptionBool(out *bool, o term.Handle) error {
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

func (e *Execution) writeTermOptionVariableNames(out *[]term.VariableName, o term.Handle) error {
	return e.mustBeList(e.Arg(o, 0), func(vn term.Handle) error {
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

func (e *Execution) writeTermOptionInteger(out *int, o term.Handle) error {
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

func op3(ctx context.Context, e *Execution) Promise {
	priority, operatorSpecifier, operator, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4]
	priority, operatorSpecifier, operator = e.Deref(priority), e.Deref(operatorSpecifier), e.Deref(operator)

	p, err := e.mustBeInteger(priority)
	if err != nil {
		return Error(err)
	}
	if p < 0 || p > 1200 {
		return Error(&DomainError{
			ValidDomain: term.NewAtom("operator_priority"),
			Culprit:     syntax.Serialize(e.Arena, priority),
			Location:    e.location,
		})
	}

	opSpec, err := e.mustBeAtom(operatorSpecifier)
	if err != nil {
		return Error(err)
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
		return Error(&DomainError{
			ValidDomain: term.NewAtom("operator_specifier"),
			Culprit:     syntax.Serialize(e.Arena, operatorSpecifier),
			Location:    e.location,
		})
	}

	var ops []term.Atom
	if a, ok := e.Atom(operator); ok {
		if err := e.validateOp(p, spec, operator); err != nil {
			return Error(err)
		}
		ops = append(ops, a)
	} else {
		if err := e.mustBeList(operator, func(elem term.Handle) error {
			a, err := e.mustBeAtom(elem)
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
			return Error(err)
		}
	}

	for _, op := range ops {
		e.Ops.Undefine(op, spec.Class())
		e.Ops.Define(int16(p), spec, op)
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func (e *Execution) validateOp(p int64, spec syntax.OperatorSpecifier, op term.Handle) error {
	name, _ := e.Atom(op)

	switch name {
	case term.NewAtomRune('.'):
		if _, ok := e.Ops.DefinedIn(name, syntax.Infix); ok {
			return &PermissionError{
				Operation:      term.NewAtom("modify"),
				PermissionType: term.NewAtom("operator"),
				Culprit:        syntax.Serialize(e.Arena, op),
				Location:       0,
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

func currentOp3(ctx context.Context, e *Execution) Promise {
	priority, operatorSpecifier, operator, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4]
	priority, operatorSpecifier, operator = e.Deref(priority), e.Deref(operatorSpecifier), e.Deref(operator)

	switch p, ok, err := e.canBeInteger(priority); {
	case err != nil:
		return Error(err)
	case ok && (p < 0 || p > 1200):
		return Error(&DomainError{
			ValidDomain: term.NewAtom("operator_priority"),
			Culprit:     syntax.Serialize(e.Arena, priority),
			Location:    e.location,
		})
	}

	switch s, ok, err := e.canBeAtom(operatorSpecifier); {
	case err != nil:
		return Error(err)
	case ok && !slices.Contains([]term.Atom{
		term.NewAtom("fx"),
		term.NewAtom("fy"),
		term.NewAtom("xf"),
		term.NewAtom("yf"),
		term.NewAtom("xfx"),
		term.NewAtom("xfy"),
		term.NewAtom("yfx"),
	}, s):
		return Error(&DomainError{
			ValidDomain: term.NewAtom("operator_specifier"),
			Culprit:     syntax.Serialize(e.Arena, operatorSpecifier),
			Location:    e.location,
		})
	}

	switch _, _, err := e.canBeAtom(operator); {
	case err != nil:
		return Error(err)
	}

	return Delay(func(yield func(Promise) bool) {
		for _, op := range e.Ops {
			p, err := e.PutInteger(int64(op.Priority))
			if err != nil {
				_ = yield(Error(err))
				return
			}

			ok, err := e.Unify(priority, p)
			if err != nil {
				_ = yield(Error(err))
				return
			}
			if !ok {
				if !yield(Failure()) {
					return
				}
				continue
			}

			s, err := e.PutAtom(term.NewAtom(op.Specifier.String()))
			if err != nil {
				_ = yield(Error(err))
				return
			}

			ok, err = e.Unify(operatorSpecifier, s)
			if err != nil {
				_ = yield(Error(err))
				return
			}
			if !ok {
				if !yield(Failure()) {
					return
				}
				continue
			}

			n, err := e.PutAtom(op.Name)
			if err != nil {
				_ = yield(Error(err))
				return
			}

			ok, err = e.Unify(operator, n)
			if err != nil {
				_ = yield(Error(err))
				return
			}
			if !ok {
				if !yield(Failure()) {
					return
				}
				continue
			}

			e.tempVars[1] = cont
			e.Next()
			if !yield(Success()) {
				return
			}
		}
	})
}

func charConversion2(ctx context.Context, e *Execution) Promise {
	inChar, outChar, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]

	in, err := e.mustBeChar(inChar)
	if err != nil {
		return Error(err)
	}

	out, err := e.mustBeChar(outChar)
	if err != nil {
		return Error(err)
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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func currentCharConversion2(ctx context.Context, e *Execution) Promise {
	inChar, outChar, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]

	if _, _, err := e.canBeChar(inChar); err != nil {
		return Error(err)
	}

	if _, _, err := e.canBeChar(outChar); err != nil {
		return Error(err)
	}

	return Delay(func(yield func(Promise) bool) {
		for _, entry := range e.CharConversion.Entries {
			i, err := e.PutAtom(term.NewAtomRune(entry.In))
			if err != nil {
				_ = yield(Error(err))
				return
			}

			ok, err := e.Unify(inChar, i)
			if err != nil {
				_ = yield(Error(err))
				return
			}
			if !ok {
				if !yield(Failure()) {
					return
				}
				continue
			}

			o, err := e.PutAtom(term.NewAtomRune(entry.Out))
			if err != nil {
				_ = yield(Error(err))
				return
			}

			ok, err = e.Unify(outChar, o)
			if err != nil {
				_ = yield(Error(err))
				return
			}
			if !ok {
				if !yield(Failure()) {
					return
				}
				continue
			}

			e.tempVars[1] = cont
			e.Next()
			if !yield(Success()) {
				return
			}
		}
	})
}

func call2(ctx context.Context, e *Execution) Promise {
	closure, arg1, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]
	closure = e.Deref(closure)

	f, err := e.mustBeCallable(closure)
	if err != nil {
		return Error(err)
	}

	cont, err = e.PutCompound(f.Name(), slices.Collect(concat(
		e.Args(closure),
		singleton(arg1),
		singleton(cont),
	))...)
	if err != nil {
		return Error(err)
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func call3(ctx context.Context, e *Execution) Promise {
	closure, arg1, arg2, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4]
	closure = e.Deref(closure)

	f, err := e.mustBeCallable(closure)
	if err != nil {
		return Error(err)
	}

	cont, err = e.PutCompound(f.Name(), slices.Collect(concat(
		e.Args(closure),
		singleton(arg1),
		singleton(arg2),
		singleton(cont),
	))...)
	if err != nil {
		return Error(err)
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func call4(ctx context.Context, e *Execution) Promise {
	closure, arg1, arg2, arg3, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4], e.tempVars[5]
	closure = e.Deref(closure)

	f, err := e.mustBeCallable(closure)
	if err != nil {
		return Error(err)
	}

	cont, err = e.PutCompound(f.Name(), slices.Collect(concat(
		e.Args(closure),
		singleton(arg1),
		singleton(arg2),
		singleton(arg3),
		singleton(cont),
	))...)
	if err != nil {
		return Error(err)
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func call5(ctx context.Context, e *Execution) Promise {
	closure, arg1, arg2, arg3, arg4, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4], e.tempVars[5], e.tempVars[6]
	closure = e.Deref(closure)

	f, err := e.mustBeCallable(closure)
	if err != nil {
		return Error(err)
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
		return Error(err)
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func call6(ctx context.Context, e *Execution) Promise {
	closure, arg1, arg2, arg3, arg4, arg5, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4], e.tempVars[5], e.tempVars[6], e.tempVars[7]
	closure = e.Deref(closure)

	f, err := e.mustBeCallable(closure)
	if err != nil {
		return Error(err)
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
		return Error(err)
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func call7(ctx context.Context, e *Execution) Promise {
	closure, arg1, arg2, arg3, arg4, arg5, arg6, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4], e.tempVars[5], e.tempVars[6], e.tempVars[7], e.tempVars[8]
	closure = e.Deref(closure)

	f, err := e.mustBeCallable(closure)
	if err != nil {
		return Error(err)
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
		return Error(err)
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func call8(ctx context.Context, e *Execution) Promise {
	closure, arg1, arg2, arg3, arg4, arg5, arg6, arg7, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4], e.tempVars[5], e.tempVars[6], e.tempVars[7], e.tempVars[8], e.tempVars[9]
	closure = e.Deref(closure)

	f, err := e.mustBeCallable(closure)
	if err != nil {
		return Error(err)
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
		return Error(err)
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func atomLength2(ctx context.Context, e *Execution) Promise {
	atom, length, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]

	a, err := e.mustBeAtom(atom)
	if err != nil {
		return Error(err)
	}

	if _, _, err := e.canBeInteger(length); err != nil {
		return Error(err)
	}

	l, err := e.PutInteger(int64(utf8.RuneCountInString(a.String())))
	if err != nil {
		return Error(err)
	}

	ok, err := e.Unify(length, l)
	if err != nil {
		return Error(err)
	}
	if !ok {
		return Failure()
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func atomConcat3(ctx context.Context, e *Execution) Promise {
	atom1, atom2, atom3, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4]
	atom1, atom2 = e.Deref(atom1), e.Deref(atom2)

	a3, ok, err := e.canBeAtom(atom3)
	if err != nil {
		return Error(err)
	}
	if !ok {
		a1, err := e.mustBeAtom(atom1)
		if err != nil {
			return Error(err)
		}

		a2, err := e.mustBeAtom(atom2)
		if err != nil {
			return Error(err)
		}

		a, err := e.PutAtom(term.NewAtom(a1.String() + a2.String()))
		if err != nil {
			return Error(err)
		}

		ok, err := e.Unify(atom3, a)
		if err != nil {
			return Error(err)
		}
		if !ok {
			return Failure()
		}

		e.tempVars[1] = cont
		e.Next()
		return Success()
	}

	if _, _, err := e.canBeAtom(atom1); err != nil {
		return Error(err)
	}

	if _, _, err := e.canBeAtom(atom2); err != nil {
		return Error(err)
	}

	return Delay(func(yield func(Promise) bool) {
		s := a3.String()
		for i := 0; i <= len(s); i += nextRuneSize(s[i:]) {
			a1, err := e.PutAtom(term.NewAtom(s[:i]))
			if err != nil {
				_ = yield(Error(err))
				return
			}

			ok, err := e.Unify(atom1, a1)
			if err != nil {
				_ = yield(Error(err))
				return
			}
			if !ok {
				if !yield(Failure()) {
					return
				}
				continue
			}

			a2, err := e.PutAtom(term.NewAtom(s[i:]))
			if err != nil {
				_ = yield(Error(err))
				return
			}

			ok, err = e.Unify(atom2, a2)
			if err != nil {
				_ = yield(Error(err))
				return
			}
			if !ok {
				if !yield(Failure()) {
					return
				}
				continue
			}

			e.tempVars[1] = cont
			e.Next()
			if !yield(Success()) {
				return
			}
		}
	})
}

func subAtom5(ctx context.Context, e *Execution) Promise {
	atom, before, length, after, subAtom, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4], e.tempVars[5], e.tempVars[6]

	a, err := e.mustBeAtom(atom)
	if err != nil {
		return Error(err)
	}

	if _, _, err := e.canBeNotLessThanZero(before); err != nil {
		return Error(err)
	}

	if _, _, err := e.canBeNotLessThanZero(length); err != nil {
		return Error(err)
	}

	if _, _, err := e.canBeNotLessThanZero(after); err != nil {
		return Error(err)
	}

	if _, _, err := e.canBeAtom(subAtom); err != nil {
		return Error(err)
	}

	return Delay(func(yield func(Promise) bool) {
		s := a.String()
		for i := 0; i <= len(s); i += nextRuneSize(s[i:]) {
			for j := i; j <= len(s); j += nextRuneSize(s[j:]) {
				b, err := e.PutInteger(int64(i))
				if err != nil {
					_ = yield(Error(err))
					return
				}

				ok, err := e.Unify(before, b)
				if err != nil {
					_ = yield(Error(err))
					return
				}
				if !ok {
					if !yield(Failure()) {
						return
					}
					continue
				}

				l, err := e.PutInteger(int64(j - i))
				if err != nil {
					_ = yield(Error(err))
					return
				}

				ok, err = e.Unify(length, l)
				if err != nil {
					_ = yield(Error(err))
					return
				}
				if !ok {
					if !yield(Failure()) {
						return
					}
					continue
				}

				a, err := e.PutInteger(int64(len(s) - j))
				if err != nil {
					_ = yield(Error(err))
					return
				}

				ok, err = e.Unify(after, a)
				if err != nil {
					_ = yield(Error(err))
					return
				}
				if !ok {
					if !yield(Failure()) {
						return
					}
					continue
				}

				sub, err := e.PutAtom(term.NewAtom(s[i:j]))
				if err != nil {
					_ = yield(Error(err))
					return
				}

				ok, err = e.Unify(subAtom, sub)
				if err != nil {
					_ = yield(Error(err))
					return
				}
				if !ok {
					if !yield(Failure()) {
						return
					}
					continue
				}

				e.tempVars[1] = cont
				e.Next()
				if !yield(Success()) {
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

func atomChars2(ctx context.Context, e *Execution) Promise {
	atom, chars, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]

	a, ok, err := e.canBeAtom(atom)
	if err != nil {
		return Error(err)
	}
	if !ok {
		var sb strings.Builder
		if err := e.mustBeList(chars, func(elem term.Handle) error {
			r, err := e.mustBeChar(elem)
			if err != nil {
				return err
			}
			sb.WriteRune(r)
			return nil
		}); err != nil {
			return Error(err)
		}

		a, err := e.PutAtom(term.NewAtom(sb.String()))
		if err != nil {
			return Error(err)
		}

		ok, err := e.Unify(atom, a)
		if err != nil {
			return Error(err)
		}
		if !ok {
			return Failure()
		}

		e.tempVars[1] = cont
		e.Next()
		return Success()
	}

	if _, err := e.canBeList(chars, func(elem term.Handle) error {
		_, _, err := e.canBeChar(elem)
		return err
	}); err != nil {
		return Error(err)
	}

	cs, err := e.PutCharList(a.String())
	if err != nil {
		return Error(err)
	}

	ok, err = e.Unify(chars, cs)
	if err != nil {
		return Error(err)
	}
	if !ok {
		return Failure()
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func atomCodes2(ctx context.Context, e *Execution) Promise {
	atom, codes, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]

	a, ok, err := e.canBeAtom(atom)
	if err != nil {
		return Error(err)
	}
	if !ok {
		var sb strings.Builder
		if err := e.mustBeList(codes, func(elem term.Handle) error {
			r, err := e.mustBeCharCode(elem)
			if err != nil {
				return err
			}
			sb.WriteRune(r)
			return nil
		}); err != nil {
			return Error(err)
		}

		a, err := e.PutAtom(term.NewAtom(sb.String()))
		if err != nil {
			return Error(err)
		}

		ok, err := e.Unify(atom, a)
		if err != nil {
			return Error(err)
		}
		if !ok {
			return Failure()
		}

		e.tempVars[1] = cont
		e.Next()
		return Success()
	}

	if _, err := e.canBeList(codes, func(elem term.Handle) error {
		_, _, err := e.canBeCharCode(elem)
		return err
	}); err != nil {
		return Error(err)
	}

	cs, err := e.PutCodeList(a.String())
	if err != nil {
		return Error(err)
	}

	ok, err = e.Unify(codes, cs)
	if err != nil {
		return Error(err)
	}
	if !ok {
		return Failure()
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func charCode2(ctx context.Context, e *Execution) Promise {
	char, code, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]

	r, ok, err := e.canBeChar(char)
	if err != nil {
		return Error(err)
	}
	if !ok {
		r, err := e.mustBeCharCode(code)
		if err != nil {
			return Error(err)
		}

		ch, err := e.PutAtom(term.NewAtomRune(r))
		if err != nil {
			return Error(err)
		}

		ok, err := e.Unify(char, ch)
		if err != nil {
			return Error(err)
		}
		if !ok {
			return Failure()
		}

		e.tempVars[1] = cont
		e.Next()
		return Success()
	}

	if _, _, err := e.canBeCharCode(code); err != nil {
		return Error(err)
	}

	cd, err := e.PutInteger(int64(r))
	if err != nil {
		return Error(err)
	}

	ok, err = e.Unify(code, cd)
	if err != nil {
		return Error(err)
	}
	if !ok {
		return Failure()
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func numberChars2(ctx context.Context, e *Execution) Promise {
	number, list, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]

	var sb strings.Builder
	switch ok, err := e.canBeList(list, func(elem term.Handle) error {
		r, err := e.mustBeChar(elem)
		if err != nil {
			return err
		}
		_, _ = sb.WriteRune(r)
		return nil
	}); {
	case err != nil:
		return Error(err)
	case !ok:
		if _, _, _, _, err := e.mustBeNumber(number); err != nil {
			return Error(err)
		}

		var sb strings.Builder
		_, _ = fmt.Fprintf(&sb, "%s", &syntax.Formatter{
			Arena: e.Arena,
			Term:  number,
		})

		l, err := e.PutCharList(sb.String())
		if err != nil {
			return Error(err)
		}

		ok, err = e.Unify(list, l)
		if err != nil {
			return Error(err)
		}
		if !ok {
			return Failure()
		}

		e.tempVars[1] = cont
		e.Next()
		return Success()
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
		return Error(err)
	}

	ok, err := e.Unify(number, n)
	if err != nil {
		return Error(err)
	}
	if !ok {
		return Failure()
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func numberCodes2(ctx context.Context, e *Execution) Promise {
	number, list, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]

	var sb strings.Builder
	switch ok, err := e.canBeList(list, func(elem term.Handle) error {
		r, err := e.mustBeCharCode(elem)
		if err != nil {
			return err
		}
		_, _ = sb.WriteRune(r)
		return nil
	}); {
	case err != nil:
		return Error(err)
	case !ok:
		if _, _, _, _, err := e.mustBeNumber(number); err != nil {
			return Error(err)
		}

		var sb strings.Builder
		_, _ = fmt.Fprintf(&sb, "%s", &syntax.Formatter{
			Arena: e.Arena,
			Term:  number,
		})

		l, err := e.PutCodeList(sb.String())
		if err != nil {
			return Error(err)
		}

		ok, err = e.Unify(list, l)
		if err != nil {
			return Error(err)
		}
		if !ok {
			return Failure()
		}

		e.tempVars[1] = cont
		e.Next()
		return Success()
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
		return Error(err)
	}

	ok, err := e.Unify(number, n)
	if err != nil {
		return Error(err)
	}
	if !ok {
		return Failure()
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

// Throw throws an error. TODO: Expand the use of this method.
func (e *Execution) Throw(err error, cont term.Handle) Promise {
	et, err := ErrorTerm(e.Arena, err)
	if err != nil {
		return Error(err)
	}
	cont, err = e.PutCompound(term.NewAtom("throw"), et, cont)
	if err != nil {
		return Error(err)
	}
	e.tempVars[1] = cont
	e.Next()
	return Success()
}

type flagEntry struct {
	flag term.Atom
	get  func(e *Engine) (term.Handle, error)
	set  func(e *Engine, value term.Handle) error
}

var (
	flags = []flagEntry{
		{
			flag: term.NewAtom("bounded"),
			get: func(e *Engine) (term.Handle, error) {
				return e.PutAtom(term.NewAtom("true"))
			},
		},
		{
			flag: term.NewAtom("max_integer"),
			get: func(e *Engine) (term.Handle, error) {
				return e.PutInteger(int64(math.MaxInt64))
			},
		},
		{
			flag: term.NewAtom("min_integer"),
			get: func(e *Engine) (term.Handle, error) {
				return e.PutInteger(int64(math.MinInt64))
			},
		},
		{
			flag: term.NewAtom("integer_rounding_function"),
			get: func(e *Engine) (term.Handle, error) {
				return e.PutAtom(term.NewAtom("toward_zero"))
			},
		},
		{
			flag: term.NewAtom("char_conversion"),
			get: func(e *Engine) (term.Handle, error) {
				if e.CharConversion.Disabled {
					return e.PutAtom(term.NewAtom("false"))
				}
				return e.PutAtom(term.NewAtom("true"))
			},
			set: func(e *Engine, value term.Handle) error {
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
			get: func(e *Engine) (term.Handle, error) {
				if e.debug {
					return e.PutAtom(term.NewAtom("on"))
				}
				return e.PutAtom(term.NewAtom("off"))
			},
			set: func(e *Engine, value term.Handle) error {
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
			get: func(e *Engine) (term.Handle, error) {
				return e.PutInteger(int64(math.MaxUint16))
			},
		},
		{
			flag: term.NewAtom("unknown"),
			get: func(e *Engine) (term.Handle, error) {
				return e.PutAtom(term.NewAtom(e.unknown.String()))
			},
			set: func(e *Engine, value term.Handle) error {
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
			get: func(e *Engine) (term.Handle, error) {
				return e.PutAtom(term.NewAtom(e.DoubleQuotes.String()))
			},
			set: func(e *Engine, value term.Handle) error {
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

func setPrologFlag2(ctx context.Context, e *Execution) Promise {
	flag, value, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]
	value = e.Deref(value)

	f, err := e.mustBeAtom(flag)
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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func currentPrologFlag2(ctx context.Context, e *Execution) Promise {
	flag, value, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]

	f, ok, err := e.canBeAtom(flag)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Delay(func(yield func(Promise) bool) {
			for _, fe := range flags {
				f, err := e.PutAtom(fe.flag)
				if err != nil {
					if !yield(e.Throw(err, cont)) {
						return
					}
					continue
				}

				ok, err := e.Unify(flag, f)
				if err != nil {
					if !yield(e.Throw(err, cont)) {
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

				v, err := fe.get(e.Engine)
				if err != nil {
					if !yield(e.Throw(err, cont)) {
						return
					}
					continue
				}

				ok, err = e.Unify(value, v)
				if err != nil {
					if !yield(e.Throw(err, cont)) {
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

				e.tempVars[1] = cont
				e.Next()
				if !yield(Success()) {
					return
				}
			}
		})
	}
	i := slices.IndexFunc(flags, func(entry flagEntry) bool {
		return entry.flag == f
	})
	if i < 0 {
		return e.Throw(&DomainError{
			ValidDomain: term.NewAtom("flag"),
			Culprit:     syntax.Serialize(e.Arena, flag),
			Location:    e.location,
		}, cont)
	}

	fe := flags[i]
	v, err := fe.get(e.Engine)
	if err != nil {
		return e.Throw(err, cont)
	}

	ok, err = e.Unify(value, v)
	if err != nil {
		return e.Throw(err, cont)
	}
	if !ok {
		return Failure()
	}

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

type Halt struct {
	Code int
}

func (e Halt) Error() string {
	return fmt.Sprintf("halt %d", e.Code)
}

func halt1(ctx context.Context, e *Execution) Promise {
	x, cont := e.tempVars[1], e.tempVars[2]

	n, err := e.mustBeInteger(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	// Do not throw this error as a ball.
	// Return it to the host program as is.
	return Error(&Halt{
		Code: int(n),
	})
}

func dynamic1(ctx context.Context, e *Execution) Promise {
	t, cont := e.tempVars[1], e.tempVars[2]
	t = e.Deref(t)

	pi, err := e.mustBePredicateIndicator(t)
	if err != nil {
		return Error(err)
	}

	bpi := term.NewFunctor(pi.Name(), pi.Arity()+1)
	p, _ := e.Predicates[bpi]
	p.Public = true
	p.Dynamic = true
	e.Predicates[bpi] = p

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func getNeckCut1(_ context.Context, e *Execution) Promise {
	cutB, err := e.PutInteger(int64(e.cutB))
	if err != nil {
		return Error(err)
	}
	e.tempVars[0] = cutB
	e.Next()
	return Success()
}

func getCont1(_ context.Context, e *Execution) Promise {
	out, cont := e.tempVars[1], e.tempVars[2]
	if ok, err := e.Unify(out, cont); !ok || err != nil {
		return Error(err)
	}
	e.Next()
	return Success()
}

func callCont1(ctx context.Context, e *Execution) Promise {
	// No need to move arguments.
	return true0(ctx, e)
}

func add3(ctx context.Context, e *Execution) Promise {
	x, y, out, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4]
	x, y = e.Deref(x), e.Deref(y)

	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	yi, yInt, yf, _, err := e.mustBeNumber(y)
	if err != nil {
		return e.Throw(err, cont)
	}

	var t term.Handle
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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func sub3(ctx context.Context, e *Execution) Promise {
	x, y, out, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4]
	x, y = e.Deref(x), e.Deref(y)

	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	yi, yInt, yf, _, err := e.mustBeNumber(y)
	if err != nil {
		return e.Throw(err, cont)
	}

	var t term.Handle
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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func mul3(ctx context.Context, e *Execution) Promise {
	x, y, out, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4]
	x, y = e.Deref(x), e.Deref(y)

	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	yi, yInt, yf, _, err := e.mustBeNumber(y)
	if err != nil {
		return e.Throw(err, cont)
	}

	var t term.Handle
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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func intDiv3(ctx context.Context, e *Execution) Promise {
	x, y, out, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4]
	x, y = e.Deref(x), e.Deref(y)

	if _, _, _, _, err := e.mustBeNumber(x); err != nil {
		return e.Throw(err, cont)
	}
	i, err := e.mustBeInteger(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	if _, _, _, _, err := e.mustBeNumber(y); err != nil {
		return e.Throw(err, cont)
	}
	j, err := e.mustBeInteger(y)
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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func div3(ctx context.Context, e *Execution) Promise {
	x, y, out, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4]
	x, y = e.Deref(x), e.Deref(y)

	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	yi, yInt, yf, _, err := e.mustBeNumber(y)
	if err != nil {
		return e.Throw(err, cont)
	}

	var t term.Handle
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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func rem3(ctx context.Context, e *Execution) Promise {
	x, y, out, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4]
	x, y = e.Deref(x), e.Deref(y)

	if _, _, _, _, err := e.mustBeNumber(x); err != nil {
		return e.Throw(err, cont)
	}
	i, err := e.mustBeInteger(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	if _, _, _, _, err := e.mustBeNumber(y); err != nil {
		return e.Throw(err, cont)
	}
	j, err := e.mustBeInteger(y)
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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func mod3(ctx context.Context, e *Execution) Promise {
	x, y, out, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4]
	x, y = e.Deref(x), e.Deref(y)

	if _, _, _, _, err := e.mustBeNumber(x); err != nil {
		return e.Throw(err, cont)
	}
	i, err := e.mustBeInteger(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	if _, _, _, _, err := e.mustBeNumber(y); err != nil {
		return e.Throw(err, cont)
	}
	j, err := e.mustBeInteger(y)
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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func neg2(ctx context.Context, e *Execution) Promise {
	x, out, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]
	x = e.Deref(x)

	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	var t term.Handle
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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func abs2(ctx context.Context, e *Execution) Promise {
	x, out, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]
	x = e.Deref(x)

	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	var t term.Handle
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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func sign2(ctx context.Context, e *Execution) Promise {
	x, out, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]
	x = e.Deref(x)

	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	var t term.Handle
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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func floatIntegerPart2(ctx context.Context, e *Execution) Promise {
	x, out, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]
	x = e.Deref(x)

	if _, _, _, _, err := e.mustBeNumber(x); err != nil {
		return e.Throw(err, cont)
	}
	f, err := e.mustBeFloat(x)
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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func floatFractionalPart2(ctx context.Context, e *Execution) Promise {
	x, out, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]
	x = e.Deref(x)

	if _, _, _, _, err := e.mustBeNumber(x); err != nil {
		return e.Throw(err, cont)
	}
	f, err := e.mustBeFloat(x)
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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func float2(ctx context.Context, e *Execution) Promise {
	x, out, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]
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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func floor2(ctx context.Context, e *Execution) Promise {
	x, out, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]
	x = e.Deref(x)

	if _, _, _, _, err := e.mustBeNumber(x); err != nil {
		return e.Throw(err, cont)
	}
	f, err := e.mustBeFloat(x)
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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func truncate2(ctx context.Context, e *Execution) Promise {
	x, out, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]
	x = e.Deref(x)

	if _, _, _, _, err := e.mustBeNumber(x); err != nil {
		return e.Throw(err, cont)
	}
	f, err := e.mustBeFloat(x)
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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func round2(ctx context.Context, e *Execution) Promise {
	x, out, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]
	x = e.Deref(x)

	if _, _, _, _, err := e.mustBeNumber(x); err != nil {
		return e.Throw(err, cont)
	}
	f, err := e.mustBeFloat(x)
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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func ceiling2(ctx context.Context, e *Execution) Promise {
	x, out, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]
	x = e.Deref(x)

	if _, _, _, _, err := e.mustBeNumber(x); err != nil {
		return e.Throw(err, cont)
	}
	f, err := e.mustBeFloat(x)
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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func floorDiv3(ctx context.Context, e *Execution) Promise {
	x, y, out, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4]
	x, y = e.Deref(x), e.Deref(y)

	if _, _, _, _, err := e.mustBeNumber(x); err != nil {
		return e.Throw(err, cont)
	}
	i, err := e.mustBeInteger(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	if _, _, _, _, err := e.mustBeNumber(y); err != nil {
		return e.Throw(err, cont)
	}
	j, err := e.mustBeInteger(y)
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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func pos2(ctx context.Context, e *Execution) Promise {
	x, out, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]
	x = e.Deref(x)

	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	var t term.Handle
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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func power3(ctx context.Context, e *Execution) Promise {
	x, y, out, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4]
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
			Location: 0,
		}, cont)
	}

	switch r := math.Pow(xf, yf); {
	case math.IsInf(r, 0):
		return Error(&EvaluationError{
			Cause:    FloatOverflow,
			Location: e.location,
		})
	case r == 0 && xf != 0: // Underflow: r can be 0 iff x = 0.
		return Error(&EvaluationError{
			Cause:    Underflow,
			Location: e.location,
		})
	case math.IsNaN(r):
		return Error(&EvaluationError{
			Cause:    Undefined,
			Location: e.location,
		})
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

		e.tempVars[1] = cont
		e.Next()
		return Success()
	}
}

func sin2(ctx context.Context, e *Execution) Promise {
	x, out, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]
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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func cos2(ctx context.Context, e *Execution) Promise {
	x, out, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]
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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func atan2(ctx context.Context, e *Execution) Promise {
	x, out, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]
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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func exp2(ctx context.Context, e *Execution) Promise {
	x, out, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]
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
		return Error(&EvaluationError{
			Cause:    FloatOverflow,
			Location: e.location,
		})
	}

	r := math.Exp(xf)

	if r == 0 { // e^x != 0.
		return Error(&EvaluationError{
			Cause:    Underflow,
			Location: e.location,
		})
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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func log2(ctx context.Context, e *Execution) Promise {
	x, out, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]
	x = e.Deref(x)

	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}
	if xInt {
		xf = float64(xi)
	}

	if xf <= 0 {
		return Error(&EvaluationError{
			Cause:    Undefined,
			Location: e.location,
		})
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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func sqrt2(ctx context.Context, e *Execution) Promise {
	x, out, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]
	x = e.Deref(x)

	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}
	if xInt {
		xf = float64(xi)
	}

	if xf < 0 {
		return Error(&EvaluationError{
			Cause:    Undefined,
			Location: e.location,
		})
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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func max3(ctx context.Context, e *Execution) Promise {
	x, y, out, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4]
	x, y = e.Deref(x), e.Deref(y)

	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	yi, yInt, yf, _, err := e.mustBeNumber(y)
	if err != nil {
		return e.Throw(err, cont)
	}

	var t term.Handle
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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func min3(ctx context.Context, e *Execution) Promise {
	x, y, out, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4]
	x, y = e.Deref(x), e.Deref(y)

	xi, xInt, xf, _, err := e.mustBeNumber(x)
	if err != nil {
		return e.Throw(err, cont)
	}

	yi, yInt, yf, _, err := e.mustBeNumber(y)
	if err != nil {
		return e.Throw(err, cont)
	}

	var t term.Handle
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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func integerPower3(ctx context.Context, e *Execution) Promise {
	x, y, out, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4]
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
		return power3(ctx, e)
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

	e.tempVars[1] = cont
	e.Next()
	return Success()
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

func asin2(ctx context.Context, e *Execution) Promise {
	x, out, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]
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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func acos2(ctx context.Context, e *Execution) Promise {
	x, out, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]
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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func atan3(ctx context.Context, e *Execution) Promise {
	y, x, out, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3], e.tempVars[4]
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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func tan2(ctx context.Context, e *Execution) Promise {
	x, out, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]
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

	e.tempVars[1] = cont
	e.Next()
	return Success()

}

func pi1(ctx context.Context, e *Execution) Promise {
	out, cont := e.tempVars[1], e.tempVars[2]

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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func arithEq2(ctx context.Context, e *Execution) Promise {
	x, y, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]

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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func arithDif2(ctx context.Context, e *Execution) Promise {
	x, y, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]

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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func less2(ctx context.Context, e *Execution) Promise {
	x, y, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]

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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func lessEq2(ctx context.Context, e *Execution) Promise {
	x, y, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]

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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func greater2(ctx context.Context, e *Execution) Promise {
	x, y, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]

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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func greaterEq2(ctx context.Context, e *Execution) Promise {
	x, y, cont := e.tempVars[1], e.tempVars[2], e.tempVars[3]

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

	e.tempVars[1] = cont
	e.Next()
	return Success()
}

func (e *Execution) unTrailTo(b int) error {
	if b >= len(e.stack) {
		return nil
	}

	// e.stack[b] is catch/3's own choice point, so its trailTop is the trail as
	// of the catch/3 call. Recovery resumes from there: bindings Goal made are
	// undone, bindings the enclosing clause made before catch/3 are kept.
	trailTop := e.stack[b].trailTop

	for i := len(e.stack) - 1; i >= b; i-- {
		f := e.stack[i]
		if f.stop != nil {
			f.stop()
		}
	}

	e.stack = e.stack[:b]
	return e.unwindTrail(trailTop)
}

func contChain(arena *term.Arena, cont term.Handle) iter.Seq[term.Handle] {
	return func(yield func(term.Handle) bool) {
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

func (e *Execution) canBeAtom(t term.Handle) (term.Atom, bool, error) {
	if _, ok := e.Variable(t); ok {
		return term.Atom{}, false, nil
	}
	a, ok := e.Atom(t)
	if !ok {
		return term.Atom{}, false, &TypeError{
			ValidType: term.NewAtom("atom"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}
	return a, true, nil
}

func (e *Execution) mustBeAtom(t term.Handle) (term.Atom, error) {
	if _, ok := e.Variable(t); ok {
		return term.Atom{}, &InstantiationError{
			Location: e.location,
		}
	}
	a, ok := e.Atom(t)
	if !ok {
		return term.Atom{}, &TypeError{
			ValidType: term.NewAtom("atom"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}
	return a, nil
}

func (e *Execution) canBeInChar(t term.Handle) (rune, bool, error) {
	if _, ok := e.Variable(t); ok {
		return 0, false, nil
	}
	a, ok := e.Atom(t)
	if ok && a == term.NewAtom("end_of_file") {
		return -1, true, nil
	}
	r := a.Rune()
	if !ok || r == utf8.RuneError {
		return 0, false, &TypeError{
			ValidType: term.NewAtom("in_character"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}
	return r, true, nil
}

func (e *Execution) canBeInCharCode(t term.Handle) (rune, bool, error) {
	if _, ok := e.Variable(t); ok {
		return 0, false, nil
	}
	i, ok := e.Integer(t)
	if ok && i == -1 {
		return -1, true, nil
	}
	r := rune(i)
	if !ok || !utf8.ValidRune(r) {
		return 0, false, &TypeError{
			ValidType: term.NewAtom("in_character_code"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}
	return r, true, nil
}

func (e *Execution) canBeChar(t term.Handle) (rune, bool, error) {
	if _, ok := e.Variable(t); ok {
		return 0, false, nil
	}
	a, ok := e.Atom(t)
	r := a.Rune()
	if !ok || r == utf8.RuneError {
		return 0, false, &TypeError{
			ValidType: term.NewAtom("character"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}
	return r, true, nil
}

func (e *Execution) mustBeChar(t term.Handle) (rune, error) {
	r, ok, err := e.canBeChar(t)
	if err != nil {
		return 0, err
	}
	if !ok {
		return 0, &InstantiationError{
			Location: e.location,
		}
	}
	return r, nil
}

func (e *Execution) canBeCharCode(t term.Handle) (rune, bool, error) {
	if _, ok := e.Variable(t); ok {
		return 0, false, nil
	}
	i, ok := e.Integer(t)
	r := rune(i)
	if !ok || !utf8.ValidRune(r) {
		return 0, false, &TypeError{
			ValidType: term.NewAtom("character_code"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}
	return r, true, nil
}

func (e *Execution) mustBeCharCode(t term.Handle) (rune, error) {
	r, ok, err := e.canBeCharCode(t)
	if err != nil {
		return 0, err
	}
	if !ok {
		return 0, &InstantiationError{
			Location: e.location,
		}
	}
	return r, nil
}

func (e *Execution) canBeInteger(t term.Handle) (int64, bool, error) {
	if _, ok := e.Variable(t); ok {
		return 0, false, nil
	}
	n, ok := e.Integer(t)
	if !ok {
		return 0, false, &TypeError{
			ValidType: term.NewAtom("integer"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}
	return n, true, nil
}

func (e *Execution) mustBeInteger(t term.Handle) (int64, error) {
	n, ok, err := e.canBeInteger(t)
	if err != nil {
		return 0, err
	}

	if !ok {
		return 0, &InstantiationError{
			Location: e.location,
		}
	}

	return n, nil
}

func (e *Execution) canBeNotLessThanZero(t term.Handle) (int64, bool, error) {
	i, ok, err := e.canBeInteger(t)
	if err != nil {
		return 0, false, err
	}
	if !ok {
		return 0, false, nil
	}
	if i < 0 {
		return 0, false, &DomainError{
			ValidDomain: term.NewAtom("not_less_than_zero"),
			Culprit:     syntax.Serialize(e.Arena, t),
			Location:    e.location,
		}
	}
	return i, ok, nil
}

func (e *Execution) canBeFloat(t term.Handle) (float64, bool, error) {
	if _, ok := e.Variable(t); ok {
		return 0, false, nil
	}
	f, ok := e.Float(t)
	if !ok {
		return 0, false, &TypeError{
			ValidType: term.NewAtom("float"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}
	return f, true, nil
}

func (e *Execution) mustBeFloat(t term.Handle) (float64, error) {
	f, ok, err := e.canBeFloat(t)
	if err != nil {
		return 0, err
	}

	if !ok {
		return 0, &InstantiationError{
			Location: e.location,
		}
	}

	return f, nil
}

func (e *Execution) canBeList(list term.Handle, fn func(elem term.Handle) error) (bool, error) {
	if fn == nil {
		fn = func(term.Handle) error {
			return nil
		}
	}
	for elem, ok := range e.List(list) {
		if !ok {
			if _, ok := e.Variable(elem); ok {
				return false, nil
			}

			return false, &TypeError{
				ValidType: term.NewAtom("list"),
				Culprit:   syntax.Serialize(e.Arena, list),
				Location:  e.location,
			}
		}

		if err := fn(elem); err != nil {
			return false, err
		}
	}
	return true, nil
}

func (e *Execution) mustBeList(list term.Handle, fn func(elem term.Handle) error) error {
	if fn == nil {
		fn = func(term.Handle) error {
			return nil
		}
	}
	for elem, ok := range e.List(list) {
		if !ok {
			elem = e.Deref(elem)
			if _, ok := e.Variable(elem); ok {
				return &InstantiationError{
					Location: e.location,
				}
			}
			return &TypeError{
				ValidType: term.NewAtom("list"),
				Culprit:   syntax.Serialize(e.Arena, list),
				Location:  e.location,
			}
		}

		if err := fn(elem); err != nil {
			return err
		}
	}
	return nil
}

func (e *Execution) mustBeNonEmptyList(list term.Handle, fn func(elem term.Handle) error) error {
	var ok bool
	if err := e.mustBeList(list, func(elem term.Handle) error {
		ok = true
		return fn(elem)
	}); err != nil {
		return err
	}
	if !ok {
		return &DomainError{
			ValidDomain: term.NewAtom("non_empty_list"),
			Culprit:     syntax.Serialize(e.Arena, list),
			Location:    e.location,
		}
	}
	return nil
}

func (e *Execution) mustBeAtomic(t term.Handle) error {
	if _, ok := e.Variable(t); ok {
		return &InstantiationError{
			Location: e.location,
		}
	}
	if _, ok := e.Functor(t); ok {
		return &TypeError{
			ValidType: term.NewAtom("atomic"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}
	return nil
}

func (e *Execution) mustBeNumber(t term.Handle) (int64, bool, float64, bool, error) {
	t = e.Deref(t)

	if _, ok := e.Variable(t); ok {
		return 0, false, 0, false, &InstantiationError{
			Location: e.location,
		}
	}

	if i, ok := e.Integer(t); ok {
		return i, true, 0, false, nil
	}

	if f, ok := e.Float(t); ok {
		return 0, false, f, true, nil
	}

	return 0, false, 0, false, &TypeError{
		ValidType: term.NewAtom("number"),
		Culprit:   syntax.Serialize(e.Arena, t),
		Location:  e.location,
	}
}

func (e *Execution) canBeCallable(t term.Handle) (term.Functor, bool, error) {
	if _, ok := e.Variable(t); ok {
		return 0, false, nil
	}

	f, ok := e.Functor(t, term.AllowAtom(true))
	if !ok {
		return 0, false, &TypeError{
			ValidType: term.NewAtom("callable"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}

	return f, true, nil
}

func (e *Execution) mustBeCallable(t term.Handle) (term.Functor, error) {
	f, ok, err := e.canBeCallable(t)
	if err != nil {
		return 0, err
	}
	if !ok {
		return 0, &InstantiationError{
			Location: e.location,
		}
	}
	return f, nil
}

func (e *Execution) canBePredicateIndicator(t term.Handle) (term.Functor, bool, error) {
	if _, ok := e.Variable(t); ok {
		return 0, false, nil
	}

	if f, ok := e.Functor(t, term.AllowAtom(true)); !ok || f != term.NewFunctor(term.NewAtomRune('/'), 2) {
		return 0, false, &TypeError{
			ValidType: term.NewAtom("predicate_indicator"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}

	name, arity := e.Deref(e.Arg(t, 0)), e.Deref(e.Arg(t, 1))

	n, nok, err := e.canBeAtom(name)
	if err != nil {
		return 0, false, &TypeError{
			ValidType: term.NewAtom("predicate_indicator"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}

	a, aok, err := e.canBeInteger(arity)
	if err != nil {
		return 0, false, &TypeError{
			ValidType: term.NewAtom("predicate_indicator"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}

	if !nok || !aok {
		return 0, false, nil
	}

	pi := term.NewFunctor(n, int(a))
	return pi, true, nil
}

func (e *Execution) mustBePredicateIndicator(t term.Handle) (term.Functor, error) {
	pi, ok, err := e.canBePredicateIndicator(t)
	if err != nil {
		return 0, err
	}
	if !ok {
		return 0, &InstantiationError{
			Location: e.location,
		}
	}
	return pi, nil
}

func (e *Execution) canBeStream(t term.Handle) (*term.Stream, error) {
	t = e.Deref(t)
	if _, ok := e.Variable(t); ok {
		return nil, nil
	}
	s, ok := e.Stream(t)
	if !ok {
		return nil, &TypeError{
			ValidType: term.NewAtom("stream"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}
	return s, nil
}

func (e *Execution) mustBeStream(t term.Handle) (*term.Stream, error) {
	s, err := e.canBeStream(t)
	if err != nil {
		return nil, err
	}
	if s == nil {
		return nil, &InstantiationError{
			Location: e.location,
		}
	}
	return s, nil
}

func (e *Execution) canBeSourceSink(t term.Handle) (string, error) {
	t = e.Deref(t)

	if _, ok := e.Variable(t); ok {
		return "", nil
	}

	if a, ok := e.Atom(t); ok {
		return a.String(), nil
	}

	if s, ok := e.CharList(t); ok {
		return s, nil
	}

	return "", &TypeError{
		ValidType: term.NewAtom("source_sink"),
		Culprit:   syntax.Serialize(e.Arena, t),
		Location:  e.location,
	}
}

func (e *Execution) mustBeSourceSink(t term.Handle) (string, error) {
	s, err := e.canBeSourceSink(t)
	if err != nil {
		return "", err
	}
	if s == "" {
		return "", &InstantiationError{
			Location: e.location,
		}
	}
	return s, nil
}

func (e *Execution) canBeMode(t term.Handle) (term.Mode, bool, error) {
	t = e.Deref(t)

	a, ok, err := e.canBeAtom(t)
	if err != nil || !ok {
		return 0, false, err
	}

	switch a {
	case term.NewAtom("read"):
		return term.Read, true, nil
	case term.NewAtom("write"):
		return term.Write, true, nil
	case term.NewAtom("append"):
		return term.Append, true, nil
	default:
		return 0, false, &DomainError{
			ValidDomain: term.NewAtom("mode"),
			Culprit:     syntax.Serialize(e.Arena, t),
			Location:    e.location,
		}
	}
}

func (e *Execution) mustBeMode(t term.Handle) (term.Mode, error) {
	m, ok, err := e.canBeMode(t)
	if err != nil {
		return 0, err
	}
	if !ok {
		return 0, &InstantiationError{
			Location: e.location,
		}
	}
	return m, nil
}

func (e *Execution) canBeStreamOrAlias(t term.Handle) (*term.Stream, error) {
	t = e.Deref(t)

	if _, ok := e.Variable(t); ok {
		return nil, nil
	}

	if a, ok := e.Atom(t); ok {
		for t := range e.OpenStreams() {
			s, _ := e.Stream(t)
			if s.Alias == a {
				return s, nil
			}
		}
		return nil, &ExistenceError{
			ObjectType: term.NewAtom("stream"),
			Culprit:    syntax.Serialize(e.Arena, t),
			Location:   e.location,
		}
	}

	s, ok := e.Stream(t)
	if !ok {
		return nil, &DomainError{
			ValidDomain: term.NewAtom("stream_or_alias"),
			Culprit:     syntax.Serialize(e.Arena, t),
			Location:    e.location,
		}
	}
	if s.Closed {
		return nil, &ExistenceError{
			ObjectType: term.NewAtom("stream"),
			Culprit:    syntax.Serialize(e.Arena, t),
			Location:   e.location,
		}
	}

	return s, nil
}

func (e *Execution) mustBeStreamOrAlias(t term.Handle) (*term.Stream, error) {
	s, err := e.canBeStreamOrAlias(t)
	if err != nil {
		return nil, err
	}
	if s == nil {
		return nil, &InstantiationError{
			Location: e.location,
		}
	}
	return s, nil
}

func (e *Execution) canBeStreamProperty(t term.Handle) error {
	t = e.Deref(t)

	if _, ok := e.Variable(t); ok {
		return nil
	}

	switch f, _ := e.Functor(t, term.AllowAtom(true)); f {
	case term.NewFunctor(term.NewAtom("input"), 0),
		term.NewFunctor(term.NewAtom("output"), 0):
		return nil
	case term.NewFunctor(term.NewAtom("file_name"), 1):
		arg := e.Arg(t, 0)
		arg = e.Deref(arg)
		if _, _, err := e.canBeAtom(arg); err == nil {
			return nil
		}
		break
	case term.NewFunctor(term.NewAtom("mode"), 1),
		term.NewFunctor(term.NewAtom("alias"), 1),
		term.NewFunctor(term.NewAtom("end_of_stream"), 1),
		term.NewFunctor(term.NewAtom("eof_action"), 1),
		term.NewFunctor(term.NewAtom("reposition"), 1):
		arg := e.Arg(t, 0)
		arg = e.Deref(arg)
		if _, _, err := e.canBeAtom(arg); err == nil {
			return nil
		}
		break
	case term.NewFunctor(term.NewAtom("position"), 1):
		arg := e.Arg(t, 0)
		arg = e.Deref(arg)
		if _, _, err := e.canBeInteger(arg); err == nil {
			return nil
		}
		break
	}
	return &DomainError{
		ValidDomain: term.NewAtom("stream_property"),
		Culprit:     syntax.Serialize(e.Arena, t),
		Location:    e.location,
	}
}

func (e *Execution) canBeInByte(t term.Handle) (byte, bool, error) {
	t = e.Deref(t)

	if _, ok := e.Variable(t); ok {
		return 0, false, nil
	}

	b, ok := e.Integer(t)
	if !ok || b < 0 || b > 255 {
		return 0, false, &TypeError{
			ValidType: term.NewAtom("in_byte"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}

	return byte(b), true, nil
}

func (e *Execution) canBeByte(t term.Handle) (byte, bool, error) {
	t = e.Deref(t)

	if _, ok := e.Variable(t); ok {
		return 0, false, nil
	}

	b, ok := e.Integer(t)
	if !ok || b < 0 || b > 255 {
		return 0, false, &TypeError{
			ValidType: term.NewAtom("byte"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}

	return byte(b), true, nil
}

func (e *Execution) mustBeByte(t term.Handle) (byte, error) {
	b, ok, err := e.canBeByte(t)
	if err != nil {
		return 0, err
	}
	if !ok {
		return 0, &InstantiationError{
			Location: e.location,
		}
	}
	return b, nil
}

func addI(x, y int64) (int64, error) {
	switch {
	case y > 0 && x > math.MaxInt64-y:
		return 0, IntOverflow
	case y < 0 && x < math.MinInt64-y:
		return 0, IntOverflow
	default:
		return x + y, nil
	}
}

func addF(x, y float64) (float64, error) {
	switch {
	case y > 0 && x > math.MaxFloat64-y:
		return 0, FloatOverflow
	case y < 0 && x < -math.MaxFloat64-y:
		return 0, FloatOverflow
	default:
		return x + y, nil
	}
}

func addIF(x int64, y float64) (float64, error) {
	return addF(float64(x), y)
}

func addFI(x float64, y int64) (float64, error) {
	return addF(x, float64(y))
}

func subI(x, y int64) (int64, error) {
	switch {
	case y < 0 && x > math.MaxInt64+y:
		return 0, IntOverflow
	case y > 0 && x < math.MinInt64+y:
		return 0, IntOverflow
	default:
		return x - y, nil
	}
}

func subF(x, y float64) (float64, error) {
	return addF(x, -y)
}

func subFI(x float64, n int64) (float64, error) {
	return subF(x, float64(n))
}

func subIF(n int64, x float64) (float64, error) {
	return subF(float64(n), x)
}

func mulI(x, y int64) (int64, error) {
	switch {
	case x == -1 && y == math.MinInt64:
		return 0, IntOverflow
	case x == math.MinInt64 && y == -1:
		return 0, IntOverflow
	case y == 0:
		return 0, nil
	default:
		r := x * y
		if r/y != x {
			return 0, IntOverflow
		}
		return r, nil
	}
}

func mulF(x, y float64) (float64, error) {
	switch {
	case y != 0 && math.Abs(x) > math.MaxFloat64/math.Abs(y):
		return 0, FloatOverflow
	}

	r := x * y

	// Underflow: x*y = 0 iff x = 0 or y = 0.
	if r == 0 && x != 0 && y != 0 {
		return 0, Underflow
	}

	return r, nil
}

func mulIF(n int64, x float64) (float64, error) {
	return mulF(float64(n), x)
}

func mulFI(x float64, n int64) (float64, error) {
	return mulF(x, float64(n))
}

func intDivI(x, y int64) (int64, error) {
	switch {
	case y == 0:
		return 0, ZeroDivisor
	case x == math.MinInt64 && y == -1:
		// Two's complement special case
		return 0, IntOverflow
	default:
		return x / y, nil
	}
}

func divI(n, m int64) (float64, error) {
	return divF(float64(n), float64(m))
}

func divF(x, y float64) (float64, error) {
	switch {
	case y == 0:
		return 0, ZeroDivisor
	case math.Abs(x) > math.MaxFloat64*math.Abs(y):
		return 0, FloatOverflow
	}

	r := x / y

	// Underflow: x/y = 0 iff x = 0 and y != 0.
	if r == 0 && x != 0 {
		return 0, Underflow
	}

	return r, nil
}

func divIF(n int64, x float64) (float64, error) {
	return divF(float64(n), x)
}

func divFI(x float64, n int64) (float64, error) {
	return divF(x, float64(n))
}

func remI(x, y int64) (int64, error) {
	if y == 0 {
		return 0, ZeroDivisor
	}
	return x - ((x / y) * y), nil
}

func modI(x, y int64) (int64, error) {
	if y == 0 {
		return 0, ZeroDivisor
	}
	return x - (int64(math.Floor(float64(x)/float64(y))) * y), nil
}

func negI(x int64) (int64, error) {
	// Two's complement special case
	if x == math.MinInt64 {
		return 0, IntOverflow
	}
	return -x, nil
}

func negF(x float64) float64 {
	return -x
}

func absI(x int64) (int64, error) {
	switch {
	case x == math.MinInt64:
		return 0, IntOverflow
	case x < 0:
		return -x, nil
	default:
		return x, nil
	}
}

func absF(x float64) float64 {
	return math.Abs(float64(x))
}

func signI(x int64) int64 {
	switch {
	case x > 0:
		return 1
	case x < 0:
		return -1
	default:
		return 0
	}
}

func signF(x float64) float64 {
	switch {
	case x > 0:
		return 1
	case x < 0:
		return -1
	default:
		return 0
	}
}

func posI(x int64) (int64, error) {
	return x, nil
}

func posF(x float64) (float64, error) {
	return x, nil
}

func intFloorDivI(x, y int64) (int64, error) {
	switch {
	case x == math.MinInt64 && y == -1:
		return 0, IntOverflow
	case y == 0:
		return 0, ZeroDivisor
	default:
		return int64(math.Floor(float64(x) / float64(y))), nil
	}
}

func intPartF(x float64) float64 {
	s := signF(x)
	return s * math.Floor(math.Abs(x))
}

func fractPartF(x float64) float64 {
	i := intPartF(x)
	return x - i
}

func eqI(m, n int64) bool {
	return m == n
}

func eqF(x, y float64) bool {
	return x == y
}

func eqFI(x float64, n int64) bool {
	y := floatItoF(n)
	return eqF(x, y)
}

func eqIF(n int64, y float64) bool {
	return eqFI(y, n)
}

func neqF(x, y float64) bool {
	return x != y
}

func neqI(m, n int64) bool {
	return m != n
}

func neqFI(x float64, n int64) bool {
	y := floatItoF(n)
	return neqF(x, y)
}

func neqIF(n int64, y float64) bool {
	return neqFI(y, n)
}

func lssF(x, y float64) bool {
	return x < y
}

func lssI(m, n int64) bool {
	return m < n
}

func lssFI(x float64, n int64) bool {
	y := floatItoF(n)
	return lssF(x, y)
}

func lssIF(n int64, y float64) bool {
	return gtrFI(y, n)
}

func leqF(x, y float64) bool {
	return x <= y
}

func leqI(m, n int64) bool {
	return m <= n
}

func leqFI(x float64, n int64) bool {
	y := floatItoF(n)
	return leqF(x, y)
}

func leqIF(n int64, y float64) bool {
	return geqFI(y, n)
}

func gtrF(x, y float64) bool {
	return x > y
}

func gtrI(m, n int64) bool {
	return m > n
}

func gtrFI(x float64, n int64) bool {
	y := floatItoF(n)
	return gtrF(x, y)
}

func gtrIF(n int64, y float64) bool {
	return lssFI(y, n)
}

func geqF(x, y float64) bool {
	return x >= y
}

func geqI(m, n int64) bool {
	return m >= n
}

func geqFI(x float64, n int64) bool {
	y := floatItoF(n)
	return geqF(x, y)
}

func geqIF(n int64, y float64) bool {
	return leqFI(y, n)
}

// Type conversion operations

func floatItoF(n int64) float64 {
	return float64(n)
}

func floatFtoF(x float64) float64 {
	return x
}

func floorFtoI(x float64) (int64, error) {
	f := math.Floor(x)
	if f >= float64(math.MaxInt64) || f < float64(math.MinInt64) {
		return 0, IntOverflow
	}
	return int64(f), nil
}

func truncateFtoI(x float64) (int64, error) {
	t := math.Trunc(x)
	if t >= float64(math.MaxInt64) || t < float64(math.MinInt64) {
		return 0, IntOverflow
	}
	return int64(t), nil
}

func roundFtoI(x float64) (int64, error) {
	r := math.Round(x)
	if r >= float64(math.MaxInt64) || r < float64(math.MinInt64) {
		return 0, IntOverflow
	}
	return int64(r), nil
}

func ceilingFtoI(x float64) (int64, error) {
	c := math.Ceil(x)
	if c >= float64(math.MaxInt64) || c < float64(math.MinInt64) {
		return 0, IntOverflow
	}
	return int64(c), nil
}
