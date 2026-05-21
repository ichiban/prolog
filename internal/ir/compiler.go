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
	*runtime.Engine
	counter      int
	todo         []term.Handle
	makeVariable func() (term.Handle, error)
}

// Compile compiles a sequence of binarized clauses into an intermediate representation of module.
func (c *Compiler) Compile(ctx context.Context, text string) (*Module, error) {
	var (
		arena = c.Arena
		m     Module
	)
	for t, err := range c.clauses(ctx, text) {
		if err != nil {
			return nil, err
		}
		// TODO: Handling of already binarized clauses H ::- B ?
		head, body, err := c.Rule(t)
		if err != nil {
			return nil, err
		}
		body, err = c.ReplaceBody(body)
		if err != nil {
			return nil, err
		}
		cont, err := arena.PutVariable()
		if err != nil {
			return nil, err
		}
		head, body, err = c.Binarize(head, body, cont)
		if err != nil {
			return nil, err
		}

		var cl Clause
		if err := cl.Compile(c, head, body); err != nil {
			return nil, err
		}
		m.Clauses = append(m.Clauses, cl)
	}
	m.Name = c.Module
	return &m, nil
}

func (c *Compiler) clauses(ctx context.Context, text string) iter.Seq2[term.Handle, error] {
	return func(yield func(term.Handle, error) bool) {
		c.todo = c.todo[:0]

		for pi, id := range c.BuiltinIndex {
			b := c.Builtins[id]
			head, err := c.PutCompoundWithFreshVars(pi)
			if err != nil {
				_ = yield(term.Handle{}, err)
				return
			}
			var body term.Handle
			if b.Type == runtime.BuiltinTypeInHead {
				body, err = c.PutAtom(term.NewAtom("true"))
				if err != nil {
					_ = yield(term.Handle{}, err)
					return
				}
			} else {
				body = head
			}
			c, err := c.PutCompound(term.NewAtom(":-"), head, body)
			if err != nil {
				_ = yield(term.Handle{}, err)
				return
			}
			if !yield(c, nil) {
				return
			}
		}

		for clause, err := range syntax.Parse(text,
			syntax.Arena(c.Arena),
			syntax.Operators(c.Ops),
			syntax.DoubleQuote(&c.DoubleQuotes),
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

			for _, clause := range c.todo {
				if !yield(clause, nil) {
					return
				}
			}
		}
	}
}

// Rule turns a term to a form of H :- B.
func (c *Compiler) Rule(t term.Handle) (head, body term.Handle, err error) {
	f, ok := c.Functor(t)
	if ok && f == functorRule {
		return c.Arg(t, 0), c.Arg(t, 1), nil
	}
	b, _ := c.PutAtom(atomTrue) // Always succeeds.
	return t, b, nil
}

func (c *Compiler) ReplaceBody(goal term.Handle) (term.Handle, error) {
	c.counter = 0
	if c.makeVariable == nil {
		c.makeVariable = c.PutVariable
	}

	// X -> call(X)
	if _, ok := c.Variable(goal); ok {
		return c.PutCompound(atomCall, goal)
	}

	switch goal, err := c.replaceMacro(goal); {
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

func (c *Compiler) replaceMacro(goal term.Handle) (term.Handle, error) {
	// ! -> '$cut_to'('$cut')
	// TODO: I don't know what it does. Do we really need this?
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
		return c.traverseConjunction(a, b)
	}

	// A;B -> replaceDisjunction
	if f, ok := c.Functor(goal); ok && f == functorOr {
		a, b := c.Arg(goal, 0), c.Arg(goal, 1)
		return c.replaceDisjunction(a, b)
	}

	// A->B -> ReplaceBody(A)->ReplaceBody(B)
	if f, ok := c.Functor(goal); ok && f == term.NewFunctor(atomIfThen, 2) {
		a, b := c.Arg(goal, 0), c.Arg(goal, 1)
		a, err := c.ReplaceBody(a)
		if err != nil {
			return term.Handle{}, err
		}
		b, err = c.ReplaceBody(b)
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
		g, err := c.replaceGoal(g)
		if err != nil {
			return term.Handle{}, err
		}
		return c.WithArgs(goal, x, g, xs)
	}

	// bagof(X, G, Xs) -> bagof(X, replaceGoalWithEV(G), Xs)
	if f, ok := c.Functor(goal); ok && f == term.NewFunctor(term.NewAtom("bagof"), 3) {
		x, g, xs := c.Arg(goal, 0), c.Arg(goal, 1), c.Arg(goal, 2)
		g, err := c.replaceGoalWithEV(g)
		if err != nil {
			return term.Handle{}, err
		}
		return c.WithArgs(goal, x, g, xs)
	}

	// setof(X, G, Xs) -> setof(X, replaceGoalWithEV(G), Xs)
	if f, ok := c.Functor(goal); ok && f == term.NewFunctor(term.NewAtom("setof"), 3) {
		x, g, xs := c.Arg(goal, 0), c.Arg(goal, 1), c.Arg(goal, 2)
		g, err := c.replaceGoalWithEV(g)
		if err != nil {
			return term.Handle{}, err
		}
		return c.WithArgs(goal, x, g, xs)
	}

	// call(G) -> call(ReplaceBody(G))
	if f, ok := c.Functor(goal); ok && f == term.NewFunctor(term.NewAtom("call"), 1) {
		g := c.Arg(goal, 0)
		g, err := c.ReplaceBody(g)
		if err != nil {
			return term.Handle{}, err
		}
		return c.WithArgs(goal, g)
	}

	// \+G -> \+ReplaceBody(G)
	if f, ok := c.Functor(goal); ok && f == term.NewFunctor(term.NewAtom(`\+`), 1) {
		g := c.Arg(goal, 0)
		g, err := c.ReplaceBody(g)
		if err != nil {
			return term.Handle{}, err
		}
		return c.WithArgs(goal, g)
	}

	return term.Handle{}, errUnhandled
}

func (c *Compiler) replaceGoal(goal term.Handle) (term.Handle, error) {
	// X -> call(X)
	if _, ok := c.Variable(goal); ok {
		return c.PutCompound(atomCall, goal)
	}

	// A,B ->
	if f, ok := c.Functor(goal); ok && f == functorAnd {
		a, b := c.Arg(goal, 0), c.Arg(goal, 1)
		g, err := c.traverseConjunction(a, b)
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
		return c.replaceDisjunction1(a, b)
	}

	// G -> ReplaceBody(G)
	return c.ReplaceBody(goal)
}

func (c *Compiler) replaceGoalWithEV(goal term.Handle) (term.Handle, error) {
	// X^G where X is an Existential Variable.
	if f, ok := c.Functor(goal); ok && f == term.NewFunctor(term.NewAtomRune('^'), 2) {
		x, g := c.Arg(goal, 0), c.Arg(goal, 1)
		g, err := c.replaceGoalWithEV(g)
		if err != nil {
			return term.Handle{}, err
		}
		return c.WithArgs(goal, x, g)
	}

	return c.replaceGoal(goal)
}

func (c *Compiler) traverseConjunction(a, b term.Handle) (term.Handle, error) {
	var err error
	if _, ok := c.Variable(a); ok {
		a, err = c.ReplaceBody(a)
		if err != nil {
			return term.Handle{}, err
		}
	} else {
		var ts []term.Handle
		switch err := c.splitOp(&ts, a); {
		case errors.Is(err, errUnhandled):
			a, err = c.ReplaceBody(a)
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
	b, err = c.ReplaceBody(b)
	if err != nil {
		return term.Handle{}, err
	}
	return c.PutCompound(atomAnd, a, b)
}

func (c *Compiler) replaceDisjunction(a, b term.Handle) (term.Handle, error) {
	// Avoid replacing cut.
	if !c.cutFree(a) || !c.cutFree(b) {
		return c.traverseDisjunction(a, b)
	}

	return c.replaceDisjunction1(a, b)
}

func (c *Compiler) replaceDisjunction1(a, b term.Handle) (term.Handle, error) {
	t, err := c.PutCompound(term.NewAtom("$or"), a, b)
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
	for body := range c.disjunctionSeq(g) {
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
	if a, ok := c.Atom(t); ok && a == term.NewAtomRune('!') {
		return false
	}
	switch f, _ := c.Functor(t); f {
	case functorAnd,
		functorOr,
		functorIfThen:
		l, r := c.Arg(t, 0), c.Arg(t, 1)
		return c.cutFree(l) && c.cutFree(r)
	}
	return true
}

func (c *Compiler) traverseDisjunction(a, b term.Handle) (term.Handle, error) {
	// A->C;B -> $if(A, C, B)
	if f, ok := c.Functor(a); ok && f == functorIfThen {
		a, d := c.Arg(a, 0), c.Arg(a, 1)
		a, err := c.ReplaceBody(a)
		if err != nil {
			return term.Handle{}, err
		}
		d, err = c.ReplaceBody(d)
		if err != nil {
			return term.Handle{}, err
		}
		return c.PutCompound(term.NewAtom("$if"), a, d, b)
	}

	// A;B -> $or(A, B)
	a, err := c.ReplaceBody(a)
	if err != nil {
		return term.Handle{}, err
	}
	b, err = c.ReplaceBody(b)
	if err != nil {
		return term.Handle{}, err
	}
	return c.PutCompound(term.NewAtom("$or"), a, b)
}

func (c *Compiler) makeNewHead(t term.Handle) (term.Handle, error) {
	vs := c.VariableSet(t)
	c.counter++
	return c.PutCompound(term.NewAtom(fmt.Sprintf("$aux%d", c.counter)), vs...)
}

func (c *Compiler) disjunctionSeq(t term.Handle) iter.Seq2[term.Handle, error] {
	return func(yield func(term.Handle, error) bool) {
		switch f, _ := c.Functor(t); f {
		case functorOr:
			a, b := c.Arg(t, 0), c.Arg(t, 1)
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
			for t, err := range c.disjunctionSeq(a) {
				if !yield(t, err) {
					return
				}
			}
			for t, err := range c.disjunctionSeq(b) {
				if !yield(t, err) {
					return
				}
			}
		case functorAnd:
			a, b := c.Arg(t, 0), c.Arg(t, 1)
			var err error
			a, err = c.ReplaceBody(a)
			if err != nil {
				_ = yield(term.Handle{}, err)
				return
			}
			cut, _ := c.PutAtom(term.NewAtomRune('!')) // Always succeeds.
			b, err = c.ReplaceBody(b)
			if err != nil {
				_ = yield(term.Handle{}, err)
				return
			}
			t, err := c.PutCompound(atomAnd, cut, b)
			if err != nil {
				_ = yield(term.Handle{}, err)
				return
			}
			t, err = c.PutCompound(atomAnd, a, t)
			if err != nil {
				_ = yield(term.Handle{}, err)
				return
			}
			if !yield(t, nil) {
				return
			}
		default:
			t, err := c.ReplaceBody(t)
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
	if f, ok := c.Functor(goal); ok && f == functorAnd {
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
	}
	f, ok := c.Functor(goal, term.AllowAtom(true))
	if !ok {
		return term.Handle{}, errUnhandled
	}
	args := slices.Collect(c.Args(goal))
	args = append(args, cont)
	return c.PutCompound(f.Name(), args...)
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
