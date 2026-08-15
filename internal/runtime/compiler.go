// Ported to Go from BinProlog (github.com/ptarau/binprolog, src/co.pl and
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
	"strings"

	"github.com/ichiban/prolog/v2/internal/db"
	"github.com/ichiban/prolog/v2/internal/ir"
	"github.com/ichiban/prolog/v2/internal/syntax"
	"github.com/ichiban/prolog/v2/internal/term"
)

// FIXME: String is a compound term but should be treated as a constant.

var (
	errUnhandled = errors.New("unhandled syntax")
)

var (
	atomNeck    = term.NewAtom(":-")
	atomTrue    = term.NewAtom("true")
	atomFail    = term.NewAtom("fail")
	atomCall    = term.NewAtom("call")
	atomCut     = term.NewAtomRune('!')
	atomOr      = term.NewAtomRune(';')
	atomAnd     = term.NewAtomRune(',')
	atomIfThen  = term.NewAtom("->")
	atomCutTo   = term.NewAtom("$cut_to")
	atomCompare = term.NewAtom("compare")

	atomCutSentinel = term.NewAtom("$cut")
)

var (
	functorRule   = term.NewFunctor(atomNeck, 2)
	functorAnd    = term.NewFunctor(atomAnd, 2)
	functorOr     = term.NewFunctor(atomOr, 2)
	functorIfThen = term.NewFunctor(atomIfThen, 2)
)

type Mode uint8

const (
	Get Mode = iota
	Put
)

func (m Mode) Op() ir.OpCode {
	switch m {
	case Get:
		return ir.OpGet
	case Put:
		return ir.OpPut
	default:
		return ir.OpInvalid
	}
}

type Compiler struct {
	*Engine
	OnDiscontiguous func(pi term.Functor) error

	counter      int
	todo         []term.Handle
	makeVariable func() (term.Handle, error)
}

func (c *Compiler) CompileSystem(ctx context.Context, out *ir.Module) error {
	out.Name = term.NewAtom("prolog")
	for t, err := range c.builtinClauses() {
		if err != nil {
			return err
		}

		c.schedule(t)
	}
	return c.run(ctx, out)
}

// CompileText compiles a Prolog text into a module.
func (c *Compiler) CompileText(ctx context.Context, out *ir.Module, text string) error {
	for t, err := range syntax.Parse(strings.NewReader(text),
		syntax.Arena(c.Arena),
		syntax.Operators(c.Ops),
		syntax.DoubleQuote(&c.DoubleQuotes),
	) {
		if err != nil {
			return err
		}
		c.schedule(t)
	}
	if err := c.run(ctx, out); err != nil {
		return err
	}
	if c.Module == (term.Atom{}) {
		c.Module = term.NewAtom("user")
	}
	out.Name = c.Module
	return nil
}

func (c *Compiler) schedule(t term.Handle) {
	c.todo = append(c.todo, t)
}

func (c *Compiler) run(ctx context.Context, out *ir.Module) error {
	for len(c.todo) > 0 {
		var (
			t   term.Handle
			err error
		)
		t, c.todo = c.todo[0], c.todo[1:]
		t, err = c.Engine.ExpandGoal(ctx, t) // FIXME: Is this the right place?
		if err != nil {
			return err
		}

		f, _ := c.Functor(t, term.AllowAtom(true))

		// Directive
		if f == term.NewFunctor(term.NewAtom(":-"), 1) {
			d := c.Arg(t, 0)
			switch di, _ := c.Functor(d, term.AllowAtom(true)); di {
			default:
				for err := range c.Call(ctx, d) {
					if err != nil {
						return err
					}
					break
				}
			}
			continue
		}

		bpi := term.NewFunctor(f.Name(), f.Arity()+1)
		if p, _ := c.Predicates[bpi]; p.Dynamic {
			a, err := c.PutCompound(term.NewAtom("assertz"), t)
			if err != nil {
				return err
			}
			for err := range c.Call(ctx, a) {
				if err != nil {
					return err
				}
				break
			}
			continue
		}

		head, body, err := c.rule(t)
		if err != nil {
			return err
		}

		var cl ir.Clause
		if err := c.compileClause(ctx, &cl, head, body); err != nil {
			return err
		}
		out.Clauses = append(out.Clauses, cl)
	}
	return nil
}

func (c *Compiler) compileClause(ctx context.Context, clause *ir.Clause, head, body term.Handle) error {
	cont, err := c.PutVariable()
	if err != nil {
		return err
	}

	body, err = c.ReplaceBody(body, cont)
	if err != nil {
		return err
	}

	binHead, binBody, err := c.Binarize(head, body, cont)
	if err != nil {
		return err
	}

	bpi, _ := c.Functor(binHead)
	if p, _ := c.Predicates[bpi]; p.Public {
		if err := c.DB.InsertAfter(ctx, c.Arena, db.Record{
			Head:      head,
			Body:      body,
			CreatedAt: c.CurrentTime,
		}); err != nil {
			return err
		}
	}

	return c.CompileBinaryClause(clause, binHead, binBody)
}

func (c *Compiler) builtinClauses() iter.Seq2[term.Handle, error] {
	return func(yield func(term.Handle, error) bool) {
		if c.BuiltinSet == nil {
			c.BuiltinSet = NewBuiltinSet()
		}
		for pi, b := range c.BuiltinSet.All() {
			// BuiltinSet contains binarized PIs. Here we're adding non-binarized surrogate clauses.
			pi := term.NewFunctor(pi.Name(), pi.Arity()-1)
			head, err := c.PutCompoundWithFreshVars(pi)
			if err != nil {
				_ = yield(term.Handle{}, err)
				return
			}
			var body term.Handle
			if b.Type == InHead {
				body, err = c.PutAtom(term.NewAtom("true"))
				if err != nil {
					_ = yield(term.Handle{}, err)
					return
				}
			} else {
				body = head
			}
			t, err := c.PutCompound(term.NewAtom(":-"), head, body)
			if err != nil {
				_ = yield(term.Handle{}, err)
				return
			}
			if !yield(t, nil) {
				return
			}
		}
	}
}

func (c *Compiler) clauses(ctx context.Context, text string) iter.Seq2[term.Handle, error] {
	c.todo = c.todo[:0]
	return func(yield func(term.Handle, error) bool) {
		for t, err := range syntax.Parse(strings.NewReader(text),
			syntax.Arena(c.Arena),
			syntax.Operators(c.Ops),
			syntax.DoubleQuote(&c.DoubleQuotes),
		) {
			if err != nil {
				_ = yield(term.Handle{}, err)
				return
			}

			// TODO: Process include/1 directive here?

			for t, err := range c.Engine.ExpandTerm(ctx, t) {
				if err != nil {
					_ = yield(term.Handle{}, err)
					return
				}
				t, err = c.Engine.ExpandGoal(ctx, t) // FIXME:
				if !yield(t, err) {
					return
				}
			}

			for _, t := range c.todo {
				if !yield(t, nil) {
					return
				}
			}
			c.todo = c.todo[:0]
		}
	}
}

// rule turns a term to a form of H :- B.
func (c *Compiler) rule(t term.Handle) (head, body term.Handle, err error) {
	f, ok := c.Functor(t)
	if ok && f == functorRule {
		return c.Arg(t, 0), c.Arg(t, 1), nil
	}
	b, _ := c.PutAtom(atomTrue) // Always succeeds.
	return t, b, nil
}

func (c *Compiler) ReplaceBody(goal, cont term.Handle) (term.Handle, error) {
	if c.makeVariable == nil {
		c.makeVariable = c.PutVariable
	}

	goal = c.Deref(goal)

	// X -> call(X)
	if _, ok := c.Variable(goal); ok {
		return c.PutCompound(atomCall, goal)
	}

	switch goal, err := c.replaceMacro(goal, cont); {
	case errors.Is(err, errUnhandled):
		break
	case err != nil:
		return term.Handle{}, err
	default:
		return goal, nil
	}

	var ts []term.Handle
	switch err := c.splitOp(&ts, goal); {
	case errors.Is(err, errUnhandled):
		break
	case err != nil:
		return term.Handle{}, err
	default:
		return c.PutSpine(term.NewAtomRune(','), ts...)
	}

	// TODO: implement the rest!
	// meta expansion?

	return goal, nil
}

func (c *Compiler) replaceMacro(goal, cont term.Handle) (term.Handle, error) {
	// $cont(C) -> C = Cont
	if f, ok := c.Functor(goal); ok && f == term.NewFunctor(term.NewAtom("$cont"), 1) {
		k := c.Arg(goal, 0)
		return c.PutCompound(term.NewAtomRune('='), k, cont)
	}

	// ! -> '$cut_to'('$cut')
	if a, ok := c.Atom(goal); ok && a == atomCut {
		sentinel, _ := c.PutAtom(atomCutSentinel) // Always succeeds.
		return c.PutCompound(atomCutTo, sentinel)
	}

	// var(X) -> fail if known
	if f, ok := c.Functor(goal); ok && f == term.NewFunctor(term.NewAtom("var"), 1) {
		x := c.Arg(goal, 0)
		if _, ok := c.Variable(x); ok {
			return goal, nil
		}
		return c.PutAtom(atomFail)
	}

	// nonvar(X) -> true if known
	if f, ok := c.Functor(goal); ok && f == term.NewFunctor(term.NewAtom("nonvar"), 1) {
		x := c.Arg(goal, 0)
		if _, ok := c.Variable(x); ok {
			return goal, nil
		}
		return c.PutAtom(atomTrue)
	}

	// atomic(X) -> true/fail if known
	if f, ok := c.Functor(goal); ok && f == term.NewFunctor(term.NewAtom("atomic"), 1) {
		x := c.Arg(goal, 0)
		if _, ok := c.Variable(x); ok {
			return goal, nil
		}
		a := atomTrue
		if _, ok := c.Functor(x); ok {
			a = atomFail
		}
		return c.PutAtom(a)
	}

	// TODO: No compound(X)/atom(X)/integer(X)/float(X) -> true/fail if known?

	// A,B -> traverseConjunction
	if f, ok := c.Functor(goal); ok && f == functorAnd {
		a, b := c.Arg(goal, 0), c.Arg(goal, 1)
		return c.traverseConjunction(a, b, cont)
	}

	// A;B -> replaceDisjunction
	if f, ok := c.Functor(goal); ok && f == functorOr {
		a, b := c.Arg(goal, 0), c.Arg(goal, 1)
		return c.replaceDisjunction(a, b, cont)
	}

	// A->B -> ReplaceBody(A)->ReplaceBody(B)
	if f, ok := c.Functor(goal); ok && f == term.NewFunctor(atomIfThen, 2) {
		a, b := c.Arg(goal, 0), c.Arg(goal, 1)
		a, err := c.ReplaceBody(a, cont)
		if err != nil {
			return term.Handle{}, err
		}
		b, err = c.ReplaceBody(b, cont)
		if err != nil {
			return term.Handle{}, err
		}
		return c.WithArgs(goal, a, b)
	}

	// A==B -> compare(=, A, B)
	if f, ok := c.Functor(goal); ok && f == term.NewFunctor(term.NewAtom("=="), 2) {
		a, b := c.Arg(goal, 0), c.Arg(goal, 1)
		r, _ := c.PutAtom(term.NewAtomRune('=')) // Always succeeds.
		return c.PutCompound(atomCompare, r, a, b)
	}

	// A@<B -> compare(<, A, B)
	if f, ok := c.Functor(goal); ok && f == term.NewFunctor(term.NewAtom("@<"), 2) {
		a, b := c.Arg(goal, 0), c.Arg(goal, 1)
		r, _ := c.PutAtom(term.NewAtomRune('<')) // Always succeeds.
		return c.PutCompound(atomCompare, r, a, b)
	}

	// A@>B -> compare(>, A, B)
	if f, ok := c.Functor(goal); ok && f == term.NewFunctor(term.NewAtom("@>"), 2) {
		a, b := c.Arg(goal, 0), c.Arg(goal, 1)
		r, _ := c.PutAtom(term.NewAtomRune('>')) // Always succeeds.
		return c.PutCompound(atomCompare, r, a, b)
	}

	// M:X -> module_call(M, X)
	// TODO: Do we really need to implement this?

	// findall(X, G, Xs) -> findall(X, replaceGoal(G), Xs)
	if f, ok := c.Functor(goal); ok && f == term.NewFunctor(term.NewAtom("findall"), 3) {
		x, g, xs := c.Arg(goal, 0), c.Arg(goal, 1), c.Arg(goal, 2)
		g, err := c.replaceGoal(g, cont)
		if err != nil {
			return term.Handle{}, err
		}
		return c.WithArgs(goal, x, g, xs)
	}

	// bagof(X, G, Xs) -> bagof(X, replaceGoalWithEV(G), Xs)
	if f, ok := c.Functor(goal); ok && f == term.NewFunctor(term.NewAtom("bagof"), 3) {
		x, g, xs := c.Arg(goal, 0), c.Arg(goal, 1), c.Arg(goal, 2)
		g, err := c.replaceGoalWithEV(g, cont)
		if err != nil {
			return term.Handle{}, err
		}
		return c.WithArgs(goal, x, g, xs)
	}

	// setof(X, G, Xs) -> setof(X, replaceGoalWithEV(G), Xs)
	if f, ok := c.Functor(goal); ok && f == term.NewFunctor(term.NewAtom("setof"), 3) {
		x, g, xs := c.Arg(goal, 0), c.Arg(goal, 1), c.Arg(goal, 2)
		g, err := c.replaceGoalWithEV(g, cont)
		if err != nil {
			return term.Handle{}, err
		}
		return c.WithArgs(goal, x, g, xs)
	}

	// call(G) -> ReplaceBody(G)
	if f, ok := c.Functor(goal); ok && f == term.NewFunctor(term.NewAtom("call"), 1) {
		g := c.Arg(goal, 0)
		return c.ReplaceBody(g, cont)
	}

	// \+G -> \+ReplaceBody(G)
	if f, ok := c.Functor(goal); ok && f == term.NewFunctor(term.NewAtom(`\+`), 1) {
		g := c.Arg(goal, 0)
		g, err := c.ReplaceBody(g, cont)
		if err != nil {
			return term.Handle{}, err
		}
		return c.WithArgs(goal, g)
	}

	return term.Handle{}, errUnhandled
}

func (c *Compiler) replaceGoal(goal, cont term.Handle) (term.Handle, error) {
	// X -> call(X)
	if _, ok := c.Variable(goal); ok {
		return c.PutCompound(atomCall, goal)
	}

	// A,B ->
	if f, ok := c.Functor(goal); ok && f == functorAnd {
		a, b := c.Arg(goal, 0), c.Arg(goal, 1)
		g, err := c.traverseConjunction(a, b, cont)
		if err != nil {
			return term.Handle{}, err
		}
		head, err := c.makeNewHead(g)
		if err != nil {
			return term.Handle{}, err
		}
		if err := c.compileLater(head, g); err != nil {
			return term.Handle{}, err
		}
		return head, nil
	}

	// A;B ->
	if f, ok := c.Functor(goal); ok && f == functorOr {
		a, b := c.Arg(goal, 0), c.Arg(goal, 1)
		return c.replaceDisjunction1(a, b, cont)
	}

	// G -> ReplaceBody(G)
	return c.ReplaceBody(goal, cont)
}

func (c *Compiler) replaceGoalWithEV(goal, cont term.Handle) (term.Handle, error) {
	// X^G where X is an Existential Variable.
	if f, ok := c.Functor(goal); ok && f == term.NewFunctor(term.NewAtomRune('^'), 2) {
		x, g := c.Arg(goal, 0), c.Arg(goal, 1)
		g, err := c.replaceGoalWithEV(g, cont)
		if err != nil {
			return term.Handle{}, err
		}
		return c.WithArgs(goal, x, g)
	}

	return c.replaceGoal(goal, cont)
}

func (c *Compiler) traverseConjunction(a, b, cont term.Handle) (term.Handle, error) {
	var err error
	if _, ok := c.Variable(a); ok {
		a, err = c.ReplaceBody(a, cont)
		if err != nil {
			return term.Handle{}, err
		}
	} else {
		var ts []term.Handle
		switch err := c.splitOp(&ts, a); {
		case errors.Is(err, errUnhandled):
			a, err = c.ReplaceBody(a, cont)
			if err != nil {
				return term.Handle{}, err
			}
		case err != nil:
			return term.Handle{}, err
		default:
			a, err = c.PutSpine(term.NewAtomRune(','), ts...)
			if err != nil {
				return term.Handle{}, err
			}
		}
	}
	b, err = c.ReplaceBody(b, cont)
	if err != nil {
		return term.Handle{}, err
	}
	return c.PutCompound(atomAnd, a, b)
}

func (c *Compiler) replaceDisjunction(a, b, cont term.Handle) (term.Handle, error) {
	// Avoid replacing cut.
	if c.cutFree(a) && c.cutFree(b) {
		return c.replaceDisjunction1(a, b, cont)
	}

	return c.traverseDisjunction(a, b, cont)
}

func (c *Compiler) replaceDisjunction1(a, b, cont term.Handle) (term.Handle, error) {
	t, err := c.PutCompound(term.NewAtom("or"), a, b)
	if err != nil {
		return term.Handle{}, err
	}
	head, err := c.makeNewHead(t)
	if err != nil {
		return term.Handle{}, err
	}
	g, err := c.PutCompound(atomOr, a, b)
	if err != nil {
		return term.Handle{}, err
	}
	for body := range c.disjunctionSeq(g, cont) {
		if err := c.compileLater(head, body); err != nil {
			return term.Handle{}, err
		}
	}
	return head, nil
}

func (c *Compiler) compileLater(head, body term.Handle) error {
	cl, err := c.PutCompound(atomNeck, head, body)
	if err != nil {
		return err
	}
	c.todo = append(c.todo, cl)
	return nil
}

func (c *Compiler) cutFree(t term.Handle) bool {
	t = c.Deref(t)
	if _, ok := c.Variable(t); ok {
		return true
	}
	if a, ok := c.Atom(t); ok && a == term.NewAtomRune('!') {
		return false
	}
	switch f, _ := c.Functor(t); f {
	case functorAnd,
		functorOr,
		functorIfThen:
		l, r := c.Arg(t, 0), c.Arg(t, 1)
		return c.cutFree(l) || c.cutFree(r)
	}
	return true
}

func (c *Compiler) traverseDisjunction(a, b, cont term.Handle) (term.Handle, error) {
	// A->C;B -> $if(A, C, B)
	if f, ok := c.Functor(a); ok && f == functorIfThen {
		a, d := c.Arg(a, 0), c.Arg(a, 1)
		a, err := c.ReplaceBody(a, cont)
		if err != nil {
			return term.Handle{}, err
		}
		d, err = c.ReplaceBody(d, cont)
		if err != nil {
			return term.Handle{}, err
		}
		return c.PutCompound(term.NewAtom("$if"), a, d, b)
	}

	// A;B -> $or(A, B)
	a, err := c.ReplaceBody(a, cont)
	if err != nil {
		return term.Handle{}, err
	}
	b, err = c.ReplaceBody(b, cont)
	if err != nil {
		return term.Handle{}, err
	}
	return c.PutCompound(term.NewAtom("$or"), a, b)
}

func (c *Compiler) makeNewHead(t term.Handle) (term.Handle, error) {
	// TODO: A new auxiliary predicate name should be based on t.
	vs := c.VariableSet(t)
	c.counter++
	return c.PutCompound(term.NewAtom(fmt.Sprintf("$aux%d", c.counter)), vs...)
}

func (c *Compiler) disjunctionSeq(t, cont term.Handle) iter.Seq2[term.Handle, error] {
	return func(yield func(term.Handle, error) bool) {
		t = c.Deref(t)
		switch f, _ := c.Functor(t); f {
		case functorOr:
			a, b := c.Arg(t, 0), c.Arg(t, 1)
			a, b = c.Deref(a), c.Deref(b)
			var err error
			if _, ok := c.Variable(a); ok {
				a, err = c.PutCompound(term.NewAtom("call"), a)
				if err != nil {
					_ = yield(term.Handle{}, err)
					return
				}
			}
			if _, ok := c.Variable(b); ok {
				b, err = c.PutCompound(term.NewAtom("call"), b)
				if err != nil {
					_ = yield(term.Handle{}, err)
					return
				}
			}
			for t, err := range c.disjunctionSeq(a, cont) {
				if !yield(t, err) {
					return
				}
			}
			for t, err := range c.disjunctionSeq(b, cont) {
				if !yield(t, err) {
					return
				}
			}
		case functorIfThen:
			a, b := c.Arg(t, 0), c.Arg(t, 1)
			var err error
			a, err = c.ReplaceBody(a, cont)
			if err != nil {
				_ = yield(term.Handle{}, err)
				return
			}
			cut, err := c.PutAtom(term.NewAtomRune('!'))
			if err != nil {
				_ = yield(term.Handle{}, err)
				return
			}
			b, err = c.ReplaceBody(b, cont)
			if err != nil {
				_ = yield(term.Handle{}, err)
				return
			}
			t, err := c.PutSpine(term.NewAtomRune(','), a, cut, b)
			if err != nil {
				_ = yield(term.Handle{}, err)
				return
			}
			if !yield(t, nil) {
				return
			}
		default:
			t, err := c.ReplaceBody(t, cont)
			if err != nil {
				_ = yield(term.Handle{}, err)
				return
			}
			if !yield(t, nil) {
				return
			}
		}
	}
}

// Binarize turns a clause p :- q, r into p(C) :- q(r(C)).
func (c *Compiler) Binarize(head, body, cont term.Handle) (neaHead term.Handle, neaBody term.Handle, _ error) {
	var err error
	hf, ok := c.Functor(head, term.AllowAtom(true))
	if !ok {
		return term.Handle{}, term.Handle{}, errUnhandled
	}
	args := slices.Collect(c.Args(head))
	args = append(args, cont)
	head, err = c.PutCompound(hf.Name(), args...)
	if err != nil {
		return term.Handle{}, term.Handle{}, err
	}
	body, err = c.addCont(body, cont)
	return head, body, err
}

func (c *Compiler) addCont(goal, cont term.Handle) (term.Handle, error) {
	f, ok := c.Functor(goal, term.AllowAtom(true))
	if !ok {
		return term.Handle{}, errUnhandled
	}
	switch f {
	case functorAnd:
		x, y := c.Arg(goal, 0), c.Arg(goal, 1)
		if a, ok := c.Atom(x); ok {
			switch a {
			case atomTrue:
				return c.addCont(y, cont)
			case atomFail:
				return c.PutCompound(atomFail, cont)
			}
		}
		y, err := c.addCont(y, cont)
		if err != nil {
			return term.Handle{}, err
		}
		f, ok := c.Functor(x, term.AllowAtom(true))
		if !ok {
			return term.Handle{}, errUnhandled
		}
		args := slices.Collect(c.Args(x))
		args = append(args, y)
		return c.PutCompound(f.Name(), args...)
	default:
		args := slices.Collect(c.Args(goal))
		args = append(args, cont)
		return c.PutCompound(f.Name(), args...)
	}
}

func (c *Compiler) splitOp(out *[]term.Handle, goal term.Handle) error {
	f, ok := c.Functor(goal, term.AllowAtom(true))
	if !ok {
		return errUnhandled
	}

	a, b := c.Arg(goal, 0), c.Arg(goal, 1)

	switch f {
	case term.NewFunctor(term.NewAtom("is"), 2):
		return c.splitIsRel(out, a, b)
	case term.NewFunctor(term.NewAtomRune('<'), 2):
		return c.splitRel(out, term.NewAtom("$less"), a, b)
	case term.NewFunctor(term.NewAtomRune('>'), 2):
		return c.splitRel(out, term.NewAtom("$greater"), a, b)
	case term.NewFunctor(term.NewAtom("=<"), 2):
		return c.splitRel(out, term.NewAtom("$less_eq"), a, b)
	case term.NewFunctor(term.NewAtom(">="), 2):
		return c.splitRel(out, term.NewAtom("$greater_eq"), a, b)
	case term.NewFunctor(term.NewAtom("=:="), 2):
		return c.splitRel(out, term.NewAtom("$arith_eq"), a, b)
	case term.NewFunctor(term.NewAtom(`=\=`), 2):
		return c.splitRel(out, term.NewAtom("$arith_dif"), a, b)
	default:
		return errUnhandled
	}
}

func (c *Compiler) splitIsRel(out *[]term.Handle, x, b term.Handle) error {
	if _, ok := c.Variable(b); ok {
		t, err := c.PutCompound(term.NewAtom("$expr"), b, x)
		if err != nil {
			return err
		}

		*out = append(*out, t)
		return nil
	}

	if _, ok := c.Functor(b, term.AllowAtom(true)); !ok {
		zero, err := c.PutInteger(0)
		if err != nil {
			return err
		}
		t, err := c.PutCompound(term.NewAtom("$+"), b, zero, x)
		if err != nil {
			return err
		}
		*out = append(*out, t)
		return nil
	}

	return c.splitIs(out, x, b)
}

func (c *Compiler) splitIs(out *[]term.Handle, x, a term.Handle) error {
	if _, ok := c.Variable(a); ok {
		t, err := c.PutCompound(term.NewAtom("$expr"), a, x)
		if err != nil {
			return err
		}
		*out = append(*out, t)
		return nil
	}

	f, ok := c.Functor(a, term.AllowAtom(true))
	if !ok {
		t, err := c.PutCompound(term.NewAtomRune('='), x, a)
		if err != nil {
			return err
		}
		*out = append(*out, t)
		return nil
	}

	args := make([]term.Handle, f.Arity(), f.Arity()+1)
	for i := range args {
		v, err := c.makeVariable()
		if err != nil {
			return err
		}
		args[i] = v
		if err := c.splitIs(out, v, c.Arg(a, i)); err != nil {
			return err
		}
	}
	args = append(args, x)
	t, err := c.PutCompound(term.NewAtom("$"+f.Name().String()), args...)
	if err != nil {
		return err
	}
	*out = append(*out, t)
	return nil
}

func (c *Compiler) splitRel(out *[]term.Handle, op term.Atom, a, b term.Handle) error {
	x, err := c.makeVariable()
	if err != nil {
		return err
	}

	y, err := c.makeVariable()
	if err != nil {
		return err
	}

	if err := c.splitIs(out, x, a); err != nil {
		return err
	}

	if err := c.splitIs(out, y, b); err != nil {
		return err
	}

	t, err := c.PutCompound(op, x, y)
	if err != nil {
		return err
	}

	*out = append(*out, t)
	return nil
}

func (c *Compiler) CompileBinaryClause(clause *ir.Clause, head, body term.Handle) error {
	// Turns the first argument into a functor for indexing.
	fa := c.Arg(head, 0)
	index, err := c.index(fa)
	if err != nil {
		return err
	}
	clause.FirstArg = index

	h, err := c.compileHead(clause, head)
	if err != nil {
		return err
	}
	clause.PI = h

	b, err := c.compileBody(clause, body)
	if err != nil {
		return err
	}

	var (
		maxN = max(h.Arity(), b.Arity())

		vars = ir.Variables{}
		args = make(ir.Arguments, maxN)
	)

	for i := range args {
		args[i].HeadVarID = -1
		args[i].BodyVarID = -1
		args[i].Death = math.MaxInt
	}

	// Replace variables with its variable occurrence.
	// This is where we diverge from the original binprolog.
	// Instead of recording variable occurrences first and deriving lifetime from it later,
	// we record lifetimes at the same time.
	c.findOccurrences(clause, vars)

	if err := c.fillInfo(clause, args, vars); err != nil {
		return err
	}

	clause.CollapseArgs(args, vars)

	c.allocateRegs(clause, args, vars)

	c.beautify(clause)

	return nil
}

func (c *Compiler) index(t term.Handle) (ir.Index, error) {
	t = c.Deref(t)
	if _, ok := c.Variable(t); ok {
		// We use the zero value to represent a variable first argument instead of '_'/0.
		return ir.Index{}, nil
	}

	if f, ok := c.Functor(t); ok {
		a, err := c.PutAtom(f.Name())
		if err != nil {
			return ir.Index{}, err
		}
		return ir.Index{
			Term:  a,
			Arity: f.Arity(),
		}, nil
	}
	return ir.Index{
		Term: t,
	}, nil
}

func (c *Compiler) compileHead(clause *ir.Clause, head term.Handle) (term.Functor, error) {
	f, _ := c.Functor(head)

	pi := term.NewFunctor(f.Name(), f.Arity())
	if i, ok := c.BuiltinSet.Lookup(pi); ok {
		b := c.BuiltinSet.entries[i]
		if b.Type == InHead {
			cont := c.Arg(head, f.Arity()-1)
			clause.Emit(ir.Instruction{
				OpCode: ir.OpBuiltin,
				Type:   ir.TypeNotApplicable,
				A:      ir.Operand{Kind: ir.OperandKindBuiltin, Index: i},
				B:      ir.Operand{Kind: ir.OperandKindTerm, Term: cont},
			})
			return f, nil
		}
	}

	ct, err := c.PutCompoundWithFreshVars(f)
	if err != nil {
		return f, err
	}

	if err := c.emitTopArgs(clause, Get, head, ct); err != nil {
		return f, err
	}

	return f, c.compileTopArg(clause, Get, head, ct)
}

func (c *Compiler) emitTopArgs(clause *ir.Clause, mode Mode, t, ct term.Handle) error {
	f, ok := c.Functor(t)
	if !ok {
		return errUnhandled
	}
	for i := 0; i < f.Arity(); i++ {
		a, x := c.Arg(t, i), c.Arg(ct, i)

		typ, err := c.classifyArg(x, a)
		if err != nil {
			return err
		}

		clause.Emit(ir.Instruction{
			OpCode: mode.Op(),
			Type:   typ,
			A:      ir.Operand{Kind: ir.OperandKindArgument, Index: i + 1},
			B:      ir.Operand{Kind: ir.OperandKindTerm, Term: x},
		})
	}
	return nil
}

func (c *Compiler) compileTopArg(clause *ir.Clause, mode Mode, t, ct term.Handle) error {
	f, ok := c.Functor(t)
	if !ok {
		return errUnhandled
	}
	for i := 0; i < f.Arity(); i++ {
		a, x := c.Arg(t, i), c.Arg(ct, i)
		if err := c.compileTopTerm(clause, mode, x, a); err != nil {
			return err
		}
	}
	return nil
}

func (c *Compiler) compileTopTerm(clause *ir.Clause, mode Mode, x, t term.Handle) error {
	if _, ok := c.Variable(t); ok {
		return c.Bind(x, t)
	}

	f, ok := c.Functor(t)
	if !ok {
		return c.Bind(x, t)
	}

	clause.Emit(ir.Instruction{
		OpCode: mode.Op(),
		Type:   ir.TypeStructure,
		A:      ir.Operand{Kind: ir.OperandKindFunctor, Functor: f},
		B:      ir.Operand{Kind: ir.OperandKindTerm, Term: x},
	})

	ct, err := c.PutCompoundWithFreshVars(f)
	if err != nil {
		return err
	}

	if err := c.emitArgs(clause, mode, t, ct); err != nil {
		return err
	}

	return c.compileArgs(clause, mode, t, ct)
}

func (c *Compiler) emitArgs(clause *ir.Clause, mode Mode, t, ct term.Handle) error {
	f, _ := c.Functor(t)
	for i := range f.Arity() {
		a, x := c.Arg(t, i), c.Arg(ct, i)
		typ, err := c.classifyArg(x, a)
		if err != nil {
			return err
		}

		var (
			op ir.OpCode
			k  ir.OperandKind
		)
		switch mode {
		case Get:
			op = ir.OpUnify
			k = ir.OperandKindGet
		case Put:
			if _, ok := c.Functor(a); ok {
				op = ir.OpPush
			} else {
				op = ir.OpWrite
			}
			k = ir.OperandKindPut
		default:
			return errors.New("unreachable")
		}

		clause.Emit(ir.Instruction{
			OpCode: op,
			Type:   typ,
			A:      ir.Operand{Kind: k},
			B:      ir.Operand{Kind: ir.OperandKindTerm, Term: x},
		})
	}
	return nil
}

func (c *Compiler) compileArgs(clause *ir.Clause, mode Mode, t, ct term.Handle) error {
	f, _ := c.Functor(t)
	for i := 0; i < f.Arity(); i++ {
		if err := c.compileTerm(clause, mode, c.Arg(ct, i), c.Arg(t, i)); err != nil {
			return err
		}
	}
	return nil
}

func (c *Compiler) compileTerm(clause *ir.Clause, mode Mode, x, t term.Handle) error {
	if _, ok := c.Variable(t); ok {
		return c.Bind(x, t)
	}

	f, ok := c.Functor(t)
	if !ok {
		return c.Bind(x, t)
	}

	newOp := mode.Op()
	if newOp == ir.OpPut {
		newOp = ir.OpPush
	}
	clause.Emit(ir.Instruction{
		OpCode: newOp,
		Type:   ir.TypeStructure,
		A:      ir.Operand{Kind: ir.OperandKindFunctor, Functor: f},
		B:      ir.Operand{Kind: ir.OperandKindTerm, Term: x},
	})

	ct, err := c.PutCompoundWithFreshVars(f)
	if err != nil {
		return err
	}

	if err := c.emitArgs(clause, mode, t, ct); err != nil {
		return err
	}

	return c.compileArgs(clause, mode, t, ct)
}

func (c *Compiler) compileBody(clause *ir.Clause, body term.Handle) (term.Functor, error) {
	if _, ok := c.Variable(body); ok {
		var err error
		body, err = c.PutCompound(term.NewAtom("true"), body)
		if err != nil {
			return 0, err
		}
	}

	if a, ok := c.Atom(body); ok && a == term.NewAtom("true") {
		return term.NewFunctor(a, 0), nil
	}

	pi, ok := c.Functor(body)
	if !ok {
		return 0, errUnhandled
	}

	switch pi {
	case term.NewFunctor(term.NewAtom("$cut_to"), 2):
		cut, cont := c.Arg(body, 0), c.Arg(body, 1)

		clause.Emit(ir.Instruction{
			OpCode: ir.OpPut,
			A:      ir.Operand{Kind: ir.OperandKindCutArg, Index: 1},
			B:      ir.Operand{Kind: ir.OperandKindTerm, Term: cut}, // Always `$cut`
		})
		return c.compileBody(clause, cont)
	case term.NewFunctor(term.NewAtomRune('='), 3):
		a, b, cont := c.Arg(body, 0), c.Arg(body, 1), c.Arg(body, 2)
		if err := c.compileEqual(clause, a, b); err != nil {
			return 0, err
		}
		return c.compileBody(clause, cont)
	}

	if i, ok := c.BuiltinSet.Lookup(pi); ok {
		var (
			b = c.BuiltinSet.Get(i)
		)
		switch b.Type {
		case InHead:
			break
		case InBody:
			var cont term.Handle
			switch pi.Arity() {
			case 1:
				cont = c.Arg(body, 0)
			case 2:
				cont = c.Arg(body, 1)
				arg := c.Arg(body, 0)
				v, err := c.PutVariable()
				if err != nil {
					return 0, err
				}
				if err := c.compileTopTerm(clause, Put, v, arg); err != nil {
					return 0, err
				}
				clause.Emit(ir.Instruction{
					OpCode: ir.OpPut,
					A:      ir.Operand{Kind: ir.OperandKindTemp, Index: 0},
					B:      ir.Operand{Kind: ir.OperandKindTerm, Term: v},
				})
			default:
				return 0, errors.New("can't inline a builtin with arity more than 1")
			}
			x, err := c.PutVariable()
			if err != nil {
				return 0, err
			}
			clause.Emit(ir.Instruction{
				OpCode: ir.OpInline,
				A:      ir.Operand{Kind: ir.OperandKindBuiltin, Index: i},
				B:      ir.Operand{Kind: ir.OperandKindTerm, Term: x},
			})
			return c.compileBody(clause, cont)
		}
	}

	clause.Execute = pi

	ct, err := c.PutCompoundWithFreshVars(pi)
	if err != nil {
		return 0, err
	}

	return pi, c.emitBodyTopTerm(clause, body, ct)
}

func (c *Compiler) compileEqual(clause *ir.Clause, a, b term.Handle) error {
	if _, ok := c.Variable(b); ok {
		if _, ok := c.Variable(a); !ok {
			a, b = b, a
		}
	}

	v1, err := c.PutVariable()
	if err != nil {
		return err
	}

	v2, err := c.PutVariable()
	if err != nil {
		return err
	}

	if err := c.compileTopTerm(clause, Get, v1, a); err != nil {
		return err
	}

	clause.Emit(ir.Instruction{
		OpCode: ir.OpPut,
		A:      ir.Operand{Kind: ir.OperandKindTemp},
		B:      ir.Operand{Kind: ir.OperandKindTerm, Term: v1},
	})
	clause.Emit(ir.Instruction{
		OpCode: ir.OpGet,
		A:      ir.Operand{Kind: ir.OperandKindTemp},
		B:      ir.Operand{Kind: ir.OperandKindTerm, Term: v2},
	})

	return c.compileTopTerm(clause, Get, v2, b)
}

func (c *Compiler) emitBodyTopTerm(clause *ir.Clause, t, ct term.Handle) error {
	if err := c.compileTopArg(clause, Put, t, ct); err != nil {
		return err
	}
	return c.emitTopArgs(clause, Put, t, ct)
}

func (c *Compiler) classifyArg(x, a term.Handle) (ir.Type, error) {
	if _, ok := c.Variable(a); ok {
		err := c.Bind(x, a)
		return ir.TypeUnknown, err
	}

	if _, ok := c.Functor(a); !ok {
		err := c.Bind(x, a)
		return ir.TypeConstant, err
	}

	return ir.TypeUnknown, nil
}

func (c *Compiler) findOccurrences(clause *ir.Clause, vars ir.Variables) {
	for i := range clause.Code {
		inst := &clause.Code[i]

		if inst.B.Kind != ir.OperandKindTerm {
			continue
		}

		t := inst.B.Term
		t = c.Deref(t)
		varID, ok := c.Variable(t)
		if !ok {
			continue
		}

		v, ok := vars[varID]
		if !ok {
			v = ir.Variable{
				LifeTime: ir.LifeTime{
					Birth: i,
				},
			}
		}
		v.Count++
		v.Death = i
		vars[varID] = v

		inst.B = ir.Operand{Kind: ir.OperandKindOccurrence, Term: t, Index: v.Count}
	}
}

func (c *Compiler) fillInfo(clause *ir.Clause, args []ir.Argument, vars map[int]ir.Variable) error {
	for i := range clause.Code {
		inst := &clause.Code[i]
		c.fillVarType(inst, vars)

		if inst.A.Kind != ir.OperandKindArgument {
			continue
		}

		a := &args[inst.A.Index-1]
		switch inst.OpCode {
		case ir.OpGet:
			a.Birth = i
		case ir.OpPut:
			a.Death = i
		default:
			// Do nothing.
		}

		if inst.B.Kind != ir.OperandKindOccurrence {
			continue
		}

		varID, _ := c.Variable(inst.B.Term)
		switch inst.OpCode {
		case ir.OpGet:
			a.HeadVarID = varID
		case ir.OpPut:
			a.BodyVarID = varID
		default:
			// Do nothing.
		}
	}
	return nil
}

func (c *Compiler) fillVarType(inst *ir.Instruction, vars map[int]ir.Variable) {
	if inst.Type != ir.TypeUnknown {
		return
	}

	defer func() {
		if inst.Type != ir.TypeUnknown {
			return
		}
		inst.Type = ir.TypeConstant
	}()

	if inst.B.Kind != ir.OperandKindOccurrence {
		return
	}

	o := inst.B
	t := o.Term
	varID, ok := c.Variable(t)
	if !ok {
		return
	}
	v := vars[varID]
	switch {
	case o.Index == 1 && v.Count == 1 && (inst.OpCode == ir.OpUnify || inst.OpCode == ir.OpWrite):
		inst.Type = ir.TypeVoid
	case o.Index == 1:
		inst.Type = ir.TypeVariable
	default:
		inst.Type = ir.TypeValue
	}
}

func (c *Compiler) allocateRegs(clause *ir.Clause, args []ir.Argument, vars map[int]ir.Variable) {
	var (
		n        = len(args)
		freeList []int
	)
	for i := range clause.Code {
		inst := &clause.Code[i]

		if inst.B.Kind != ir.OperandKindOccurrence {
			continue
		}

		o := inst.B
		t := o.Term
		varID, ok := c.Variable(t)
		if !ok {
			continue
		}
		v := vars[varID]
		if o.Index == 1 && v.Reg == 0 {
			v.Reg = getReg(&n, &freeList)
			vars[varID] = v
		}
		if o.Index == v.Count && v.Reg > len(args) {
			freeList = append(freeList, v.Reg)
		}
		inst.B = ir.Operand{Kind: ir.OperandKindRegister, Index: v.Reg}
	}
	clause.MaxRegs = n
}

func (c *Compiler) classifyLoad(clause *ir.Clause, x, a term.Handle) (ir.Type, error) {
	if _, ok := c.Variable(a); ok {
		return ir.TypeUnknown, c.Bind(x, a)
	}

	if _, ok := c.Functor(a); !ok {
		return ir.TypeConstant, c.Bind(x, a)
	}

	return ir.TypeUnknown, c.compileTopTerm(clause, Put, x, a)
}

func (c *Compiler) handleConstantRes(clause *ir.Clause, x, res term.Handle) error {
	if _, ok := c.Variable(res); ok {
		return c.Bind(x, res)
	}
	if _, ok := c.Functor(res); !ok {
		clause.Emit(ir.Instruction{
			OpCode: ir.OpPut,
			A:      ir.Operand{Kind: ir.OperandKindTemp, Index: 0},
			B:      ir.Operand{Kind: ir.OperandKindTerm, Term: res},
		})
		clause.Emit(ir.Instruction{
			OpCode: ir.OpGet,
			A:      ir.Operand{Kind: ir.OperandKindTemp, Index: 0},
			B:      ir.Operand{Kind: ir.OperandKindTerm, Term: x},
		})
		return nil
	}
	return c.compileTopTerm(clause, Put, x, res)
}

func getReg(n *int, freeList *[]int) int {
	if len(*freeList) > 0 {
		var (
			r int
			l = len(*freeList)
		)
		r, *freeList = (*freeList)[l-1], (*freeList)[:l-1]
		return r
	}
	*n++
	r := *n
	return r
}

func (c *Compiler) beautify(clause *ir.Clause) {
	clause.Code = rewriteSlice(clause.Code, func(inst ir.Instruction, w func(ir.Instruction)) {
		var (
			get          = inst.OpCode == ir.OpGet
			put          = inst.OpCode == ir.OpPut
			write        = inst.OpCode == ir.OpWrite
			variable     = inst.Type == ir.TypeVariable
			value        = inst.Type == ir.TypeValue
			constant     = inst.Type == ir.TypeConstant
			sameRegister = inst.A.Kind == ir.OperandKindArgument && inst.B.Kind == ir.OperandKindRegister && inst.A.Index == inst.B.Index
			cutSentinel  = func(arena *term.Arena, operand ir.Operand) bool {
				if operand.Kind != ir.OperandKindTerm {
					return false
				}
				t := operand.Term
				t = arena.Deref(t)
				a, _ := arena.Atom(t)
				return a == atomCutSentinel
			}
			operandPut = inst.A.Kind == ir.OperandKindPut
			arg        = inst.A.Kind == ir.OperandKindArgument
			temp       = inst.A.Kind == ir.OperandKindTemp
			cutArg     = inst.A.Kind == ir.OperandKindCutArg
		)
		switch {
		case arg, temp, cutArg:
			switch {
			case (get && variable || put && value) && sameRegister:
				return // skip
			case constant && cutSentinel(c.Arena, inst.B):
				w(ir.Instruction{
					OpCode: inst.OpCode,
					Type:   ir.TypeCut,
				})
			default:
				w(inst)
			}
		case write && constant && operandPut && cutSentinel(c.Arena, inst.B):
			w(ir.Instruction{
				OpCode: ir.OpPush,
				Type:   ir.TypeCut,
			})
		default:
			w(inst)
		}
	})
}

func rewriteSlice[S ~[]T, T any](s S, fn func(e T, write func(T))) S {
	var (
		j     int
		write = func(t T) {
			s[j] = t
			j++
		}
	)
	for _, e := range s {
		fn(e, write)
	}
	return s[:j]
}
