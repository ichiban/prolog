package runtime

import (
	"context"
	"errors"
	"fmt"
	"iter"
	"math"
	"slices"

	"github.com/ichiban/prolog/v2/internal/ir"
	"github.com/ichiban/prolog/v2/internal/syntax"
	"github.com/ichiban/prolog/v2/internal/term"
)

// FIXME: Strings are a compound term but should be treated as a constant.

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
	*Engine
	OnDiscontiguous func(pi term.Functor) error

	counter      int
	todo         []term.Handle
	makeVariable func() (term.Handle, error)
}

// CompileModule compiles a Prolog text into a module.
func (c *Compiler) CompileModule(ctx context.Context, out *ir.Module, text string) error {
	for t, err := range c.clauses(ctx, text) {
		if err != nil {
			return err
		}

		// TODO: Handling of already binarized clauses H ::- B ?
		head, body, err := c.Rule(t)
		if err != nil {
			return err
		}
		body, err = c.ReplaceBody(body)
		if err != nil {
			return err
		}
		cont, err := c.PutVariable()
		if err != nil {
			return err
		}
		head, body, err = c.Binarize(head, body, cont)
		if err != nil {
			return err
		}

		var cl ir.Clause
		if err := c.CompileClause(&cl, head, body); err != nil {
			return err
		}
		out.Clauses = append(out.Clauses, cl)
	}
	if m := c.Module; m == (term.Atom{}) {
		c.Module = term.NewAtom("user")
	}
	out.Name = c.Module
	return nil
}

func (c *Compiler) clauses(ctx context.Context, text string) iter.Seq2[term.Handle, error] {
	return func(yield func(term.Handle, error) bool) {
		c.todo = c.todo[:0]

		for pi, id := range c.BuiltinSet.index {
			// BuiltinSet contains binarized PIs. Here we're adding non-binarized surrogate clauses.
			pi := term.NewFunctor(pi.Name(), pi.Arity()-1)
			b := c.BuiltinSet.entries[id]
			head, err := c.PutCompoundWithFreshVars(pi)
			if err != nil {
				_ = yield(term.Handle{}, err)
				return
			}
			var body term.Handle
			if b.Type == BuiltinTypeInHead {
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

			// TODO: Process include/1 directive here?

			for clause, err := range c.Engine.ExpandTerm(ctx, clause) {
				if err != nil {
					_ = yield(term.Handle{}, err)
					return
				}
				clause, err = c.Engine.ExpandGoal(ctx, clause) // FIXME:
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

func (c *Compiler) CompileClause(clause *ir.Clause, head, body term.Handle) error {
	h, ok := c.Functor(head)
	if !ok {
		return errUnhandled
	}
	b, ok := c.Functor(body)
	if !ok {
		return errUnhandled
	}

	// Turns the first argument into a functor for indexing.
	fa := c.Arg(head, 0)
	index, err := c.index(fa)
	if err != nil {
		return err
	}

	clause.PI = h
	clause.FirstArg = index

	if err := c.compileHead(clause, head); err != nil {
		return err
	}
	if err := c.compileBody(clause, body); err != nil {
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
	clause.Compact()

	return nil
}

func (c *Compiler) index(t term.Handle) (ir.Index, error) {
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

func (c *Compiler) compileHead(clause *ir.Clause, head term.Handle) error {
	f, _ := c.Functor(head)

	pi := term.NewFunctor(f.Name(), f.Arity())
	if i, ok := c.BuiltinSet.index[pi]; ok {
		b := c.BuiltinSet.entries[i]
		if b.Type == BuiltinTypeInHead {
			cont := c.Arg(head, f.Arity()-1)
			clause.Emit(ir.Instruction{
				OpCode: ir.OpBuiltin,
				Type:   ir.TypeNotApplicable,
				A:      ir.Operand{Kind: ir.OperandKindBuiltin, Index: i},
				B:      ir.Operand{Kind: ir.OperandKindTerm, Term: cont},
			})
			return nil
		}
	}

	ct, err := c.PutCompoundWithFreshVars(f)
	if err != nil {
		return err
	}

	if err := c.emitTopArgs(clause, ir.OpGet, head, ct); err != nil {
		return err
	}

	return c.compileTopArg(clause, ir.OpGet, head, ct)
}

func (c *Compiler) emitTopArgs(clause *ir.Clause, op ir.OpCode, t, ct term.Handle) error {
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
			OpCode: op,
			Type:   typ,
			A:      ir.Operand{Kind: ir.OperandKindArgument, Index: i},
			B:      ir.Operand{Kind: ir.OperandKindTerm, Term: x},
		})
	}
	return nil
}

func (c *Compiler) compileTopArg(clause *ir.Clause, op ir.OpCode, t, ct term.Handle) error {
	f, ok := c.Functor(t)
	if !ok {
		return errUnhandled
	}
	for i := 0; i < f.Arity(); i++ {
		a, x := c.Arg(t, i), c.Arg(ct, i)
		if err := c.compileTopTerm(clause, op, x, a); err != nil {
			return err
		}
	}
	return nil
}

func (c *Compiler) compileTopTerm(clause *ir.Clause, op ir.OpCode, x, t term.Handle) error {
	if _, ok := c.Variable(t); ok {
		return c.Bind(x, t)
	}

	f, ok := c.Functor(t)
	if !ok {
		return c.Bind(x, t)
	}

	clause.Emit(ir.Instruction{
		OpCode: op,
		Type:   ir.TypeStructure,
		A:      ir.Operand{Kind: ir.OperandKindFunctor, Functor: f},
		B:      ir.Operand{Kind: ir.OperandKindTerm, Term: x},
	})

	ct, err := c.PutCompoundWithFreshVars(f)
	if err != nil {
		return err
	}

	if err := c.emitArgs(clause, op, t, ct); err != nil {
		return err
	}

	return c.compileArgs(clause, op, t, ct)
}

func (c *Compiler) emitArgs(clause *ir.Clause, op ir.OpCode, t, ct term.Handle) error {
	f, _ := c.Functor(t)
	for i := range f.Arity() {
		a, x := c.Arg(t, i), c.Arg(ct, i)
		typ, err := c.classifyArg(x, a)
		if err != nil {
			return err
		}

		switch op {
		case ir.OpGet:
			op = ir.OpUnify
		case ir.OpPut:
			if _, ok := c.Functor(a); ok {
				op = ir.OpPush
			} else {
				op = ir.OpWrite
			}
		default:
			// Do nothing.
		}

		clause.Emit(ir.Instruction{
			OpCode: op,
			Type:   typ,
			A:      ir.Operand{Kind: ir.OperandKindGet},
			B:      ir.Operand{Kind: ir.OperandKindTerm, Term: x},
		})
	}
	return nil
}

func (c *Compiler) compileArgs(clause *ir.Clause, op ir.OpCode, t, ct term.Handle) error {
	f, _ := c.Functor(t)
	for i := 0; i < f.Arity(); i++ {
		if err := c.compileTerm(clause, op, c.Arg(ct, i), c.Arg(t, i)); err != nil {
			return err
		}
	}
	return nil
}

func (c *Compiler) compileTerm(clause *ir.Clause, op ir.OpCode, x, t term.Handle) error {
	if _, ok := c.Variable(t); ok {
		return c.Bind(x, t)
	}

	f, ok := c.Functor(t)
	if !ok {
		return c.Bind(x, t)
	}

	newOp := op
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

	return c.emitArgs(clause, op, t, ct)
}

func (c *Compiler) compileBody(clause *ir.Clause, body term.Handle) error {
	if _, ok := c.Variable(body); ok {
		var err error
		body, err = c.PutCompound(term.NewAtom("true"), body)
		if err != nil {
			return err
		}
	}

	if a, ok := c.Atom(body); ok && a == term.NewAtom("true") {
		return nil
	}

	pi, ok := c.Functor(body)
	if !ok {
		return errUnhandled
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
			return err
		}
		return c.compileBody(clause, cont)
	}

	if i, ok := c.BuiltinSet.index[pi]; ok {
		var (
			b = c.BuiltinSet.entries[i]
		)
		switch b.Type {
		case BuiltinTypeInHead:
			break
		case BuiltinTypeArithmetic0:
			var (
				cont = c.Arg(body, pi.Arity()-1)
				args = term.NewFunctor(pi.Name(), pi.Arity()-1)
			)
			newOpArgs, err := c.PutCompoundWithFreshVars(args)
			if err != nil {
				return err
			}
			for i := range args.Arity() {
				a, x := c.Arg(body, i), c.Arg(newOpArgs, i)
				typ, err := c.classifyLoad(clause, x, a)
				if err != nil {
					return err
				}
				clause.Emit(ir.Instruction{
					OpCode: ir.OpLoad,
					Type:   typ,
					A:      ir.Operand{Kind: ir.OperandKindArgument, Index: i},
					B:      ir.Operand{Kind: ir.OperandKindTerm, Term: x},
				})
			}
			zero, err := c.PutInteger(0)
			if err != nil {
				return err
			}
			clause.Emit(ir.Instruction{
				OpCode: ir.OpArithmetic,
				A:      ir.Operand{Kind: ir.OperandKindBuiltin, Index: i},
				B:      ir.Operand{Kind: ir.OperandKindTerm, Term: zero},
			})
			return c.compileBody(clause, cont)
		case BuiltinTypeArithmetic1:
			var (
				cont = c.Arg(body, pi.Arity()-1)
				args = term.NewFunctor(pi.Name(), pi.Arity()-2)
				res  = c.Arg(body, pi.Arity()-2)
			)
			varRes, err := c.PutVariable()
			if err != nil {
				return err
			}
			if err := c.handleConstantRes(clause, varRes, res); err != nil {
				return err
			}
			newOpArgs, err := c.PutCompoundWithFreshVars(args)
			if err != nil {
				return err
			}
			for i := range args.Arity() {
				a, x := c.Arg(body, i), c.Arg(newOpArgs, i)
				typ, err := c.classifyLoad(clause, x, a)
				if err != nil {
					return err
				}
				clause.Emit(ir.Instruction{
					OpCode: ir.OpLoad,
					Type:   typ,
					A:      ir.Operand{Kind: ir.OperandKindArgument, Index: i},
					B:      ir.Operand{Kind: ir.OperandKindTerm, Term: x},
				})
			}
			clause.Emit(ir.Instruction{
				OpCode: ir.OpArithmetic,
				A:      ir.Operand{Kind: ir.OperandKindBuiltin, Index: i},
				B:      ir.Operand{Kind: ir.OperandKindTerm, Term: varRes},
			})
			return c.compileBody(clause, cont)
		case BuiltinTypeInline:
			var cont term.Handle
			switch pi.Arity() {
			case 1:
				cont = c.Arg(body, 0)
			case 2:
				cont = c.Arg(body, 1)
				arg := c.Arg(body, 0)
				v, err := c.PutVariable()
				if err != nil {
					return err
				}
				if err := c.compileTopTerm(clause, ir.OpPut, v, arg); err != nil {
					return err
				}
				clause.Emit(ir.Instruction{
					OpCode: ir.OpPut,
					A:      ir.Operand{Kind: ir.OperandKindTemp, Index: 0},
					B:      ir.Operand{Kind: ir.OperandKindTerm, Term: v},
				})
			default:
				return errors.New("can't inline a builtin with arity more than 1")
			}
			x, err := c.PutVariable()
			if err != nil {
				return err
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
		return err
	}
	return c.emitBodyTopTerm(clause, body, ct)
}

func (c *Compiler) compileEqual(clause *ir.Clause, a, b term.Handle) error {
	if _, ok := c.Variable(b); ok {
		if _, ok := c.Functor(a); !ok {
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

	if err := c.compileTopTerm(clause, ir.OpGet, v1, a); err != nil {
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

	return c.compileTopTerm(clause, ir.OpPut, v2, b)
}

func (c *Compiler) emitBodyTopTerm(clause *ir.Clause, t, ct term.Handle) error {
	if err := c.compileTopArg(clause, ir.OpPut, t, ct); err != nil {
		return err
	}
	return c.emitTopArgs(clause, ir.OpPut, t, ct)
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
				Reg: -1,
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

		a := &args[inst.A.Index]
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
		if o.Index == 1 && v.Reg < 0 {
			v.Reg = getReg(&n, &freeList)
			vars[varID] = v
		}
		if o.Index == v.Count && v.Reg >= len(args) {
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

	return ir.TypeUnknown, c.compileTopTerm(clause, ir.OpPut, x, a)
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
	return c.compileTopTerm(clause, ir.OpPut, x, res)
}

func getReg(n *int, freeList *[]int) int {
	if len(*freeList) > 0 {
		var (
			r int
			l = len(*freeList)
		)
		r, *freeList = (*freeList)[l-1], (*freeList)[:l]
		return r
	}
	r := *n
	*n++
	return r
}
