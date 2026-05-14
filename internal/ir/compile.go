package ir

import (
	"context"
	"errors"
	"fmt"
	"iter"
	"slices"

	"github.com/ichiban/prolog/v2/internal/runtime"
	"github.com/ichiban/prolog/v2/internal/syntax"
	"github.com/ichiban/prolog/v2/internal/term"
)

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

type Compiler struct {
	Engine *runtime.Engine
}

// Compile compiles a sequence of binarized clauses into an intermediate representation of module.
func (c *Compiler) Compile(ctx context.Context, text string, makeVariable func() (term.Handle, error)) (*Module, error) {
	var (
		engine = c.Engine
		h      = &engine.Heap
		m      Module
		todo   []term.Handle
	)
	for t, err := range c.clauses(ctx, text, &todo) {
		if err != nil {
			return nil, err
		}
		// TODO: Handling of already binarized clauses H ::- B ?
		head, body, err := Rule(h, t)
		if err != nil {
			return nil, err
		}
		var counter int
		body, err = ReplaceBody(h, &counter, body, &todo, makeVariable)
		if err != nil {
			return nil, err
		}
		cont, err := h.PutVariable()
		if err != nil {
			return nil, err
		}
		head, body, err = Binarize(h, head, body, cont)
		if err != nil {
			return nil, err
		}

		var c Clause
		if err := c.Compile(engine, head, body); err != nil {
			return nil, err
		}

	}
	m.Name = engine.Module
	return &m, nil
}

func (c *Compiler) clauses(ctx context.Context, text string, todo *[]term.Handle) iter.Seq2[term.Handle, error] {
	return func(yield func(term.Handle, error) bool) {
		engine := c.Engine

		for pi, id := range engine.BuiltinIndex {
			b := engine.Builtins[id]
			head, err := engine.PutCompoundWithFreshVars(pi)
			if err != nil {
				_ = yield(term.Handle{}, err)
				return
			}
			var body term.Handle
			if b.Type == runtime.BuiltinTypeInHead {
				body, err = engine.PutAtom(term.NewAtom("true"))
				if err != nil {
					_ = yield(term.Handle{}, err)
					return
				}
			} else {
				body = head
			}
			c, err := engine.PutCompound(term.NewAtom(":-"), head, body)
			if err != nil {
				_ = yield(term.Handle{}, err)
				return
			}
			if !yield(c, nil) {
				return
			}
		}

		for clause, err := range syntax.Parse(text,
			syntax.Heap(&engine.Heap),
			syntax.Operators(&engine.Ops),
			syntax.DoubleQuote(&engine.DoubleQuotes),
		) {
			if err != nil {
				_ = yield(term.Handle{}, err)
				return
			}
			for clause, err := range c.Engine.ExpandTerm(ctx, clause) {
				if err != nil {
					_ = yield(term.Handle{}, err)
					return
				}
				clause, err = c.Engine.ExpandGoal(ctx, clause)
				if !yield(clause, err) {
					return
				}
			}

			for _, clause := range *todo {
				if !yield(clause, nil) {
					return
				}
			}
		}
	}
}

// Rule turns a term to a form of H :- B.
func Rule(h *term.Heap, t term.Handle) (head, body term.Handle, err error) {
	f, ok := t.Functor()
	if ok && f == functorRule {
		return t.Arg(0), t.Arg(1), nil
	}
	b, _ := h.PutAtom(atomTrue) // Always succeeds.
	return t, b, nil
}

func ReplaceBody(h *term.Heap, counter *int, goal term.Handle, todo *[]term.Handle, makeVariable func() (term.Handle, error)) (term.Handle, error) {
	if makeVariable == nil {
		makeVariable = h.PutVariable
	}

	// X -> call(X)
	if _, ok := goal.Variable(); ok {
		return h.PutCompound(atomCall, goal)
	}

	switch goal, err := replaceMacro(h, counter, goal, todo, makeVariable); {
	case errors.Is(err, errUnhandled):
		break
	case err != nil:
		return term.Handle{}, err
	default:
		return goal, nil
	}

	var ts []term.Handle
	switch err := splitOp(&ts, h, goal, makeVariable); {
	case errors.Is(err, errUnhandled):
		break
	case err != nil:
		return term.Handle{}, err
	default:
		return h.PutSpine(term.NewAtomRune(','), ts...)
	}

	// TODO: implement the rest!
	// meta expansion?

	return goal, nil
}

func replaceMacro(h *term.Heap, counter *int, goal term.Handle, todo *[]term.Handle, makeVariable func() (term.Handle, error)) (term.Handle, error) {
	// ! -> '$cut_to'('$cut')
	// TODO: I don't know what it does. Do we really need this?
	if a, ok := goal.Atom(); ok && a == atomCut {
		sentinel, _ := h.PutAtom(atomCutSentinel) // Always succeeds.
		return h.PutCompound(atomCutTo, sentinel)
	}

	// var(X) -> fail if known
	if f, ok := goal.Functor(); ok && f == term.NewFunctor(term.NewAtom("var"), 1) {
		x := goal.Arg(0)
		if _, ok := x.Variable(); ok {
			return goal, nil
		}
		return h.PutAtom(atomFail)
	}

	// nonvar(X) -> true if known
	if f, ok := goal.Functor(); ok && f == term.NewFunctor(term.NewAtom("nonvar"), 1) {
		x := goal.Arg(0)
		if _, ok := x.Variable(); ok {
			return goal, nil
		}
		return h.PutAtom(atomTrue)
	}

	// atomic(X) -> true/fail if known
	if f, ok := goal.Functor(); ok && f == term.NewFunctor(term.NewAtom("atomic"), 1) {
		x := goal.Arg(0)
		if _, ok := x.Variable(); ok {
			return goal, nil
		}
		a := atomTrue
		if _, ok := x.Functor(); ok {
			a = atomFail
		}
		return h.PutAtom(a)
	}

	// TODO: No compound(X)/atom(X)/integer(X)/float(X) -> true/fail if known?

	// A,B -> traverseConjunction
	if f, ok := goal.Functor(); ok && f == functorAnd {
		a, b := goal.Arg(0), goal.Arg(1)
		return traverseConjunction(h, counter, a, b, todo, makeVariable)
	}

	// A;B -> replaceDisjunction
	if f, ok := goal.Functor(); ok && f == functorOr {
		a, b := goal.Arg(0), goal.Arg(1)
		return replaceDisjunction(h, counter, a, b, todo, makeVariable)
	}

	// A->B -> ReplaceBody(A)->ReplaceBody(B)
	if f, ok := goal.Functor(); ok && f == term.NewFunctor(atomIfThen, 2) {
		a, b := goal.Arg(0), goal.Arg(1)
		a, err := ReplaceBody(h, counter, a, todo, makeVariable)
		if err != nil {
			return term.Handle{}, err
		}
		b, err = ReplaceBody(h, counter, b, todo, makeVariable)
		if err != nil {
			return term.Handle{}, err
		}
		return goal.WithArgs(a, b)
	}

	// A==B -> compare(=, A, B)
	if f, ok := goal.Functor(); ok && f == term.NewFunctor(term.NewAtom("=="), 2) {
		a, b := goal.Arg(0), goal.Arg(1)
		r, _ := h.PutAtom(term.NewAtomRune('=')) // Always succeeds.
		return h.PutCompound(atomCompare, r, a, b)
	}

	// A@<B -> compare(<, A, B)
	if f, ok := goal.Functor(); ok && f == term.NewFunctor(term.NewAtom("@<"), 2) {
		a, b := goal.Arg(0), goal.Arg(1)
		r, _ := h.PutAtom(term.NewAtomRune('<')) // Always succeeds.
		return h.PutCompound(atomCompare, r, a, b)
	}

	// A@>B -> compare(>, A, B)
	if f, ok := goal.Functor(); ok && f == term.NewFunctor(term.NewAtom("@>"), 2) {
		a, b := goal.Arg(0), goal.Arg(1)
		r, _ := h.PutAtom(term.NewAtomRune('>')) // Always succeeds.
		return h.PutCompound(atomCompare, r, a, b)
	}

	// M:X -> module_call(M, X)
	// TODO: Do we really need to implement this?

	// findall(X, G, Xs) -> findall(X, replaceGoal(G), Xs)
	if f, ok := goal.Functor(); ok && f == term.NewFunctor(term.NewAtom("findall"), 3) {
		x, g, xs := goal.Arg(0), goal.Arg(1), goal.Arg(2)
		g, err := replaceGoal(h, counter, g, todo, makeVariable)
		if err != nil {
			return term.Handle{}, err
		}
		return goal.WithArgs(x, g, xs)
	}

	// bagof(X, G, Xs) -> bagof(X, replaceGoalWithEV(G), Xs)
	if f, ok := goal.Functor(); ok && f == term.NewFunctor(term.NewAtom("bagof"), 3) {
		x, g, xs := goal.Arg(0), goal.Arg(1), goal.Arg(2)
		g, err := replaceGoalWithEV(h, counter, g, todo, makeVariable)
		if err != nil {
			return term.Handle{}, err
		}
		return goal.WithArgs(x, g, xs)
	}

	// setof(X, G, Xs) -> setof(X, replaceGoalWithEV(G), Xs)
	if f, ok := goal.Functor(); ok && f == term.NewFunctor(term.NewAtom("setof"), 3) {
		x, g, xs := goal.Arg(0), goal.Arg(1), goal.Arg(2)
		g, err := replaceGoalWithEV(h, counter, g, todo, makeVariable)
		if err != nil {
			return term.Handle{}, err
		}
		return goal.WithArgs(x, g, xs)
	}

	// call(G) -> call(ReplaceBody(G))
	if f, ok := goal.Functor(); ok && f == term.NewFunctor(term.NewAtom("call"), 1) {
		g := goal.Arg(0)
		g, err := ReplaceBody(h, counter, g, todo, makeVariable)
		if err != nil {
			return term.Handle{}, err
		}
		return goal.WithArgs(g)
	}

	// \+G -> \+ReplaceBody(G)
	if f, ok := goal.Functor(); ok && f == term.NewFunctor(term.NewAtom(`\+`), 1) {
		g := goal.Arg(0)
		g, err := ReplaceBody(h, counter, g, todo, makeVariable)
		if err != nil {
			return term.Handle{}, err
		}
		return goal.WithArgs(g)
	}

	return term.Handle{}, errUnhandled
}

func replaceGoal(h *term.Heap, counter *int, goal term.Handle, todo *[]term.Handle, makeVariable func() (term.Handle, error)) (term.Handle, error) {
	// X -> call(X)
	if _, ok := goal.Variable(); ok {
		return h.PutCompound(atomCall, goal)
	}

	// A,B ->
	if f, ok := goal.Functor(); ok && f == functorAnd {
		a, b := goal.Arg(0), goal.Arg(1)
		g, err := traverseConjunction(h, counter, a, b, todo, makeVariable)
		if err != nil {
			return term.Handle{}, err
		}
		head, err := makeNewHead(h, counter, g)
		if err != nil {
			return term.Handle{}, err
		}
		if err := compileLater(h, head, g, todo); err != nil {
			return term.Handle{}, err
		}
		return head, nil
	}

	// A;B ->
	if f, ok := goal.Functor(); ok && f == functorOr {
		a, b := goal.Arg(0), goal.Arg(1)
		return replaceDisjunction1(h, counter, a, b, todo, makeVariable)
	}

	// G -> ReplaceBody(G)
	return ReplaceBody(h, counter, goal, todo, makeVariable)
}

func replaceGoalWithEV(h *term.Heap, counter *int, goal term.Handle, todo *[]term.Handle, makeVariable func() (term.Handle, error)) (term.Handle, error) {
	// X^G where X is an Existential Variable.
	if f, ok := goal.Functor(); ok && f == term.NewFunctor(term.NewAtomRune('^'), 2) {
		x, g := goal.Arg(0), goal.Arg(1)
		g, err := replaceGoalWithEV(h, counter, g, todo, makeVariable)
		if err != nil {
			return term.Handle{}, err
		}
		return goal.WithArgs(x, g)
	}

	return replaceGoal(h, counter, goal, todo, makeVariable)
}

func traverseConjunction(h *term.Heap, counter *int, a, b term.Handle, todo *[]term.Handle, makeVariable func() (term.Handle, error)) (term.Handle, error) {
	var err error
	if _, ok := a.Variable(); ok {
		a, err = ReplaceBody(h, counter, a, todo, makeVariable)
		if err != nil {
			return term.Handle{}, err
		}
	} else {
		var ts []term.Handle
		switch err := splitOp(&ts, h, a, makeVariable); {
		case errors.Is(err, errUnhandled):
			a, err = ReplaceBody(h, counter, a, todo, makeVariable)
			if err != nil {
				return term.Handle{}, err
			}
		case err != nil:
			return term.Handle{}, err
		default:
			a, err = h.PutSpine(term.NewAtomRune(','), ts...)
			if err != nil {
				return term.Handle{}, err
			}
		}
	}
	b, err = ReplaceBody(h, counter, b, todo, makeVariable)
	if err != nil {
		return term.Handle{}, err
	}
	return h.PutCompound(atomAnd, a, b)
}

func replaceDisjunction(h *term.Heap, counter *int, a, b term.Handle, todo *[]term.Handle, makeVariable func() (term.Handle, error)) (term.Handle, error) {
	// Avoid replacing cut.
	if !cutFree(a) || !cutFree(b) {
		return traverseDisjunction(h, counter, a, b, todo, makeVariable)
	}

	return replaceDisjunction1(h, counter, a, b, todo, makeVariable)
}

func replaceDisjunction1(h *term.Heap, counter *int, a term.Handle, b term.Handle, todo *[]term.Handle, makeVariable func() (term.Handle, error)) (term.Handle, error) {
	t, err := h.PutCompound(term.NewAtom("$or"), a, b)
	if err != nil {
		return term.Handle{}, err
	}
	head, err := makeNewHead(h, counter, t)
	if err != nil {
		return term.Handle{}, err
	}
	g, err := h.PutCompound(atomOr, a, b)
	if err != nil {
		return term.Handle{}, err
	}
	for body := range disjunctionSeq(h, counter, g, todo, makeVariable) {
		if err := compileLater(h, head, body, todo); err != nil {
			return term.Handle{}, err
		}
	}
	return head, nil
}

func compileLater(h *term.Heap, head term.Handle, body term.Handle, todo *[]term.Handle) error {
	c, err := h.PutCompound(atomNeck, head, body)
	if err != nil {
		return err
	}
	*todo = append(*todo, c)
	return nil
}

func cutFree(t term.Handle) bool {
	if a, ok := t.Atom(); ok && a == term.NewAtomRune('!') {
		return false
	}
	switch f, _ := t.Functor(); f {
	case functorAnd,
		functorOr,
		functorIfThen:
		l, r := t.Arg(0), t.Arg(1)
		return cutFree(l) && cutFree(r)
	}
	return true
}

func traverseDisjunction(h *term.Heap, counter *int, a, b term.Handle, todo *[]term.Handle, makeVariable func() (term.Handle, error)) (term.Handle, error) {
	// A->C;B -> $if(A, C, B)
	if f, ok := a.Functor(); ok && f == functorIfThen {
		a, c := a.Arg(0), a.Arg(1)
		a, err := ReplaceBody(h, counter, a, todo, makeVariable)
		if err != nil {
			return term.Handle{}, err
		}
		c, err = ReplaceBody(h, counter, c, todo, makeVariable)
		if err != nil {
			return term.Handle{}, err
		}
		return h.PutCompound(term.NewAtom("$if"), a, c, b)
	}

	// A;B -> $or(A, B)
	a, err := ReplaceBody(h, counter, a, todo, makeVariable)
	if err != nil {
		return term.Handle{}, err
	}
	b, err = ReplaceBody(h, counter, b, todo, makeVariable)
	if err != nil {
		return term.Handle{}, err
	}
	return h.PutCompound(term.NewAtom("$or"), a, b)
}

func makeNewHead(h *term.Heap, counter *int, t term.Handle) (term.Handle, error) {
	vs := term.VariableSet(t)
	*counter++
	return h.PutCompound(term.NewAtom(fmt.Sprintf("$aux%d", *counter)), vs...)
}

func disjunctionSeq(h *term.Heap, counter *int, t term.Handle, todo *[]term.Handle, makeVariable func() (term.Handle, error)) iter.Seq2[term.Handle, error] {
	return func(yield func(term.Handle, error) bool) {
		switch f, _ := t.Functor(); f {
		case functorOr:
			a, b := t.Arg(0), t.Arg(1)
			var err error
			if _, ok := a.Variable(); ok {
				a, err = h.PutCompound(term.NewAtom("call"), a)
				if err != nil {
					_ = yield(term.Handle{}, err)
					return
				}
			}
			if _, ok := b.Variable(); ok {
				b, err = h.PutCompound(term.NewAtom("call"), b)
				if err != nil {
					_ = yield(term.Handle{}, err)
					return
				}
			}
			for t, err := range disjunctionSeq(h, counter, a, todo, makeVariable) {
				if !yield(t, err) {
					return
				}
			}
			for t, err := range disjunctionSeq(h, counter, b, todo, makeVariable) {
				if !yield(t, err) {
					return
				}
			}
		case functorAnd:
			a, b := t.Arg(0), t.Arg(1)
			var err error
			a, err = ReplaceBody(h, counter, a, todo, makeVariable)
			if err != nil {
				_ = yield(term.Handle{}, err)
				return
			}
			cut, _ := h.PutAtom(term.NewAtomRune('!')) // Always succeeds.
			b, err = ReplaceBody(h, counter, b, todo, makeVariable)
			if err != nil {
				_ = yield(term.Handle{}, err)
				return
			}
			t, err := h.PutCompound(atomAnd, cut, b)
			if err != nil {
				_ = yield(term.Handle{}, err)
				return
			}
			t, err = h.PutCompound(atomAnd, a, t)
			if err != nil {
				_ = yield(term.Handle{}, err)
				return
			}
			if !yield(t, nil) {
				return
			}
		default:
			t, err := ReplaceBody(h, counter, t, todo, makeVariable)
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
func Binarize(h *term.Heap, head, body, cont term.Handle) (neaHead term.Handle, neaBody term.Handle, _ error) {
	var err error
	hf, ok := head.Functor(term.AllowAtom(true))
	if !ok {
		return term.Handle{}, term.Handle{}, errUnhandled
	}
	args := slices.Collect(head.Args())
	args = append(args, cont)
	head, err = h.PutCompound(hf.Name(), args...)
	if err != nil {
		return term.Handle{}, term.Handle{}, err
	}
	body, err = addCont(h, body, cont)
	return head, body, err
}

func addCont(h *term.Heap, goal, cont term.Handle) (term.Handle, error) {
	if f, ok := goal.Functor(); ok && f == functorAnd {
		x, y := goal.Arg(0), goal.Arg(1)
		if a, ok := x.Atom(); ok {
			switch a {
			case atomTrue:
				return addCont(h, y, cont)
			case atomFail:
				return h.PutCompound(atomFail, cont)
			}
		}
		y, err := addCont(h, y, cont)
		if err != nil {
			return term.Handle{}, err
		}
		f, ok := x.Functor(term.AllowAtom(true))
		if !ok {
			return term.Handle{}, errUnhandled
		}
		args := slices.Collect(x.Args())
		args = append(args, y)
		return h.PutCompound(f.Name(), args...)
	}
	f, ok := goal.Functor(term.AllowAtom(true))
	if !ok {
		return term.Handle{}, errUnhandled
	}
	args := slices.Collect(goal.Args())
	args = append(args, cont)
	return h.PutCompound(f.Name(), args...)
}

func splitOp(out *[]term.Handle, h *term.Heap, goal term.Handle, makeVariable func() (term.Handle, error)) error {
	f, ok := goal.Functor(term.AllowAtom(true))
	if !ok {
		return errUnhandled
	}

	a, b := goal.Arg(0), goal.Arg(1)

	switch f {
	case term.NewFunctor(term.NewAtom("is"), 2):
		return splitIsRel(out, h, a, b, makeVariable)
	case term.NewFunctor(term.NewAtomRune('<'), 2):
		return splitRel(out, h, term.NewAtom("$less"), a, b, makeVariable)
	case term.NewFunctor(term.NewAtomRune('>'), 2):
		return splitRel(out, h, term.NewAtom("$greater"), a, b, makeVariable)
	case term.NewFunctor(term.NewAtom("=<"), 2):
		return splitRel(out, h, term.NewAtom("$less_eq"), a, b, makeVariable)
	case term.NewFunctor(term.NewAtom(">="), 2):
		return splitRel(out, h, term.NewAtom("$greater_eq"), a, b, makeVariable)
	case term.NewFunctor(term.NewAtom("=:="), 2):
		return splitRel(out, h, term.NewAtom("$arith_eq"), a, b, makeVariable)
	case term.NewFunctor(term.NewAtom(`=\=`), 2):
		return splitRel(out, h, term.NewAtom("$arith_dif"), a, b, makeVariable)
	default:
		return errUnhandled
	}
}

func splitIsRel(out *[]term.Handle, h *term.Heap, x, b term.Handle, makeVariable func() (term.Handle, error)) error {
	if _, ok := b.Variable(); ok {
		t, err := h.PutCompound(term.NewAtom("$expr"), b, x)
		if err != nil {
			return err
		}

		*out = append(*out, t)
		return nil
	}

	if _, ok := b.Functor(term.AllowAtom(true)); !ok {
		zero, err := h.PutInteger(0)
		if err != nil {
			return err
		}
		t, err := h.PutCompound(term.NewAtom("$+"), b, zero, x)
		if err != nil {
			return err
		}
		*out = append(*out, t)
		return nil
	}

	return splitIs(out, h, x, b, makeVariable)
}

func splitIs(out *[]term.Handle, h *term.Heap, x, a term.Handle, makeVariable func() (term.Handle, error)) error {
	if _, ok := a.Variable(); ok {
		t, err := h.PutCompound(term.NewAtom("$expr"), a, x)
		if err != nil {
			return err
		}
		*out = append(*out, t)
		return nil
	}

	f, ok := a.Functor(term.AllowAtom(true))
	if !ok {
		t, err := h.PutCompound(term.NewAtomRune('='), x, a)
		if err != nil {
			return err
		}
		*out = append(*out, t)
		return nil
	}

	args := make([]term.Handle, f.Arity(), f.Arity()+1)
	for i := range args {
		v, err := makeVariable()
		if err != nil {
			return err
		}
		args[i] = v
		if err := splitIs(out, h, v, a.Arg(i), makeVariable); err != nil {
			return err
		}
	}
	args = append(args, x)
	t, err := h.PutCompound(term.NewAtom("$"+f.Name().String()), args...)
	if err != nil {
		return err
	}
	*out = append(*out, t)
	return nil
}

func splitRel(out *[]term.Handle, h *term.Heap, op term.Atom, a, b term.Handle, makeVariable func() (term.Handle, error)) error {
	x, err := makeVariable()
	if err != nil {
		return err
	}

	y, err := makeVariable()
	if err != nil {
		return err
	}

	if err := splitIs(out, h, x, a, makeVariable); err != nil {
		return err
	}

	if err := splitIs(out, h, y, b, makeVariable); err != nil {
		return err
	}

	t, err := h.PutCompound(op, x, y)
	if err != nil {
		return err
	}

	*out = append(*out, t)
	return nil
}
