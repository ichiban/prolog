// Ported to Go from BinProlog (github.com/ptarau/binprolog, src/co.pl and
// related sources), Copyright (C) Paul Tarau, licensed under Apache-2.0.
// This file has been modified: translated to Go and adapted.

package runtime

import (
	"bufio"
	"context"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"iter"
	"math"
	"slices"
	"strings"

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

	// Source is the source module: where the predicates of the text being
	// compiled are defined and where its goals are called. It's the module of
	// a module declaration, or the type-in module for a non module file.
	Source term.Atom

	// File is the name of the file being compiled, recorded with the module it
	// defines.
	File string

	// headVars holds the variables that appear in a meta expandable argument
	// position of the head of the clause being compiled. A goal argument that
	// is one of them is already expanded by the caller and mustn't be expanded
	// again.
	headVars []term.Cell

	counter      int
	todo         []term.Cell
	makeVariable func() (term.Cell, error)
}

func (c *Compiler) CompileSystem(ctx context.Context, out *ir.Module) error {
	c.Source = atomPrologModule
	out.Name = atomPrologModule

	// The system's own directives belong to the prolog module, the same way a
	// loaded text's belong to the module it declares.
	typein := c.Module
	c.Module = c.Source
	defer func() { c.Module = typein }()

	for t, err := range c.builtinClauses() {
		if err != nil {
			return err
		}

		if err := c.read(ctx, out, t); err != nil {
			return err
		}
	}
	return c.run(ctx, out)
}

// CompileText compiles a Prolog text into a module.
func (c *Compiler) CompileText(ctx context.Context, out *ir.Module, text string) error {
	if c.Source == (term.Atom{}) {
		c.Source = atomUserModule
	}

	// Directives run as the text is read, and what they do -- asserting,
	// declaring a predicate dynamic -- has to land in the module the text is
	// loaded into. The report does this by changing the type-in module for the
	// duration of the load.
	//
	// The caller's module has to be saved before the first directive runs: a
	// module declaration moves the type-in module itself, so a save taken after
	// the text was read would capture the declared module and restore that
	// instead of what the caller was in.
	typein := c.Module
	c.Module = c.Source
	defer func() { c.Module = typein }()

	for t, err := range syntax.Parse(strings.NewReader(text),
		syntax.Arena(c.Arena),
		syntax.Operators(&c.Ops),
		syntax.DoubleQuote(&c.DoubleQuotes),
		syntax.CharConv(&c.CharConversion),
	) {
		if err != nil {
			return err
		}

		if err := c.read(ctx, out, t); err != nil {
			return err
		}
	}
	out.Name = c.Source

	if err := c.run(ctx, out); err != nil {
		return err
	}
	// A module declaration in the text moves it to the module it declares.
	out.Name = c.Source
	return nil
}

func (c *Compiler) schedule(t term.Cell) {
	c.todo = append(c.todo, t)
}

// read takes one term of a Prolog text as it comes off the parser. A directive
// runs right away, so that what it changes -- the operator table, the character
// conversions, the type-in module -- is in effect for the terms that follow it
// in the same text. Anything else is queued for compilation.
func (c *Compiler) read(ctx context.Context, out *ir.Module, t term.Cell) error {
	t, err := c.Engine.ExpandGoal(ctx, t)
	if err != nil {
		return err
	}
	switch ok, err := c.directive(ctx, out, t); {
	case err != nil:
		return err
	case ok:
		return nil
	}
	c.schedule(t)
	return nil
}

func (c *Compiler) directive(ctx context.Context, out *ir.Module, t term.Cell) (bool, error) {
	if f, _ := c.Functor(t); f != term.NewFunctor(term.NewAtom(":-"), 1) {
		return false, nil
	}

	d := c.Arg(t, 0)
	switch di, _ := c.Functor(d, term.AllowAtom(true)); di {
	case term.NewFunctor(term.NewAtom("module"), 2):
		if err := c.declareModule(ctx, out, c.Arg(d, 0), c.Arg(d, 1)); err != nil {
			return false, err
		}
	case term.NewFunctor(term.NewAtom("use_module"), 1):
		if err := c.useModule(ctx, c.Arg(d, 0), term.Cell{}); err != nil {
			return false, err
		}
	case term.NewFunctor(term.NewAtom("use_module"), 2):
		if err := c.useModule(ctx, c.Arg(d, 0), c.Arg(d, 1)); err != nil {
			return false, err
		}
	case term.NewFunctor(term.NewAtom("meta_predicate"), 1):
		if err := c.declareMeta(c.Arg(d, 0)); err != nil {
			return false, err
		}
	case term.NewFunctor(term.NewAtom("initialization"), 1):
		g := c.Arg(d, 0)
		out.Initialization = append(out.Initialization, g)
	case term.NewFunctor(term.NewAtom("include"), 1):
		fn := c.Arg(d, 0)
		fn = c.Deref(fn)
		fsName, filename, err := c.mustBeSourceSink(fn)
		if err != nil {
			return false, err
		}
		fsy, ok := c.FSs.Get(fsName)
		if !ok {
			return false, fs.ErrNotExist
		}
		f, err := fsy.Open(filename)
		if err != nil {
			return false, err
		}
		err = c.include(ctx, out, bufio.NewReader(f))
		_ = f.Close()
		if err != nil {
			return false, err
		}
	case term.NewFunctor(term.NewAtom("ensure_loaded"), 1):
		fn := c.Arg(d, 0)
		fn = c.Deref(fn)
		fsName, filename, err := c.sourceFile(fn)
		if err != nil {
			return false, err
		}
		if _, ok := c.Loaded[loadedKey{
			fsName:   fsName,
			filename: filename,
		}]; ok {
			break
		}
		m, err := c.LoadFile(ctx, fsName, filename)
		if err != nil {
			return false, err
		}
		// Loading a module file imports all of its exported
		// predicates into the module that asked for it.
		c.Import(m, c.Source, nil)
	default:
		for err := range c.Call(ctx, d) {
			if err != nil {
				return false, err
			}
			break
		}
	}

	return true, nil
}

// include reads an included text where the include/1 directive stands. The
// caller is midway through reading the including text, so scheduling each term
// as it arrives is what puts them in the right place.
func (c *Compiler) include(ctx context.Context, out *ir.Module, r io.RuneReader) error {
	for t, err := range syntax.Parse(r,
		syntax.Arena(c.Arena),
		syntax.Operators(&c.Ops),
		syntax.DoubleQuote(&c.DoubleQuotes),
		syntax.CharConv(&c.CharConversion),
	) {
		if err != nil {
			return err
		}

		if err := c.read(ctx, out, t); err != nil {
			return err
		}
	}
	return nil
}

func (c *Compiler) run(ctx context.Context, out *ir.Module) error {
	// A directive runs on the engine and can collect. The terms still queued and
	// the module compiled so far are reachable from nowhere else, so they have
	// to be roots until compilation is over.
	defer c.AddRoots(func(yield func(*term.Cell) bool) {
		for i := range c.todo {
			if !yield(&c.todo[i]) {
				return
			}
		}
		for t := range out.Cells() {
			if !yield(t) {
				return
			}
		}
	})()

	for len(c.todo) > 0 {
		var (
			t   term.Cell
			err error
		)
		t, c.todo = c.todo[0], c.todo[1:]

		f, _ := c.Functor(t, term.AllowAtom(true))

		bpi := term.NewProcedure(c.Source, term.NewFunctor(f.Name(), f.Arity()+1))
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

		head, body, err := c.Rule(t)
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

func (c *Compiler) compileClause(ctx context.Context, clause *ir.Clause, head, body term.Cell) error {
	c.headVars = c.headVariables(head)
	defer func() { c.headVars = nil }()

	pi, ok := c.Functor(head, term.AllowAtom(true))
	if !ok {
		return errors.New("clause head is not callable")
	}

	// The database keeps the clause as it was read. What follows rewrites the
	// body for the machine -- a cut becomes a barrier, a meta call gets its
	// module -- and clause/2 and retract/1 have to give back what was written.
	bpi := term.NewProcedure(c.Source, term.NewFunctor(pi.Name(), pi.Arity()+1))
	if p, _ := c.Predicates[bpi]; p.Public {
		cl, err := c.PutCompound(atomNeck, head, body)
		if err != nil {
			return err
		}
		payload := []byte(syntax.Serialize(c.Arena, cl))
		if err := c.DB.InsertAfter(ctx, c.Source, pi.Name(), pi.Arity(), payload); err != nil {
			return err
		}
	}

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

	return c.CompileBinaryClause(clause, binHead, binBody)
}

func (c *Compiler) builtinClauses() iter.Seq2[term.Cell, error] {
	return func(yield func(term.Cell, error) bool) {
		if c.BuiltinSet == nil {
			c.BuiltinSet = NewBuiltinSet()
		}
		for pi, b := range c.BuiltinSet.All() {
			// BuiltinSet contains binarized PIs. Here we're adding non-binarized surrogate clauses.
			pi := term.NewFunctor(pi.Name(), pi.Arity()-1)
			head, err := c.PutCompoundWithFreshVars(pi)
			if err != nil {
				_ = yield(term.Cell{}, err)
				return
			}
			var body term.Cell
			if b.Type == InHead {
				body, err = c.PutAtom(term.NewAtom("true"))
				if err != nil {
					_ = yield(term.Cell{}, err)
					return
				}
			} else {
				body = head
			}
			t, err := c.PutCompound(term.NewAtom(":-"), head, body)
			if err != nil {
				_ = yield(term.Cell{}, err)
				return
			}
			if !yield(t, nil) {
				return
			}
		}
	}
}

func (c *Compiler) clauses(ctx context.Context, text string) iter.Seq2[term.Cell, error] {
	c.todo = c.todo[:0]
	return func(yield func(term.Cell, error) bool) {
		for t, err := range syntax.Parse(strings.NewReader(text),
			syntax.Arena(c.Arena),
			syntax.Operators(&c.Ops),
			syntax.DoubleQuote(&c.DoubleQuotes),
			syntax.CharConv(&c.CharConversion),
		) {
			if err != nil {
				_ = yield(term.Cell{}, err)
				return
			}

			for t, err := range c.Engine.ExpandTerm(ctx, t) {
				if err != nil {
					_ = yield(term.Cell{}, err)
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

// ReplaceBody turns a goal of a clause body into the form the binarizer takes,
// and module name expands the arguments of a meta predicate call.
func (c *Compiler) ReplaceBody(goal, cont term.Cell) (term.Cell, error) {
	g, err := c.replaceBody(goal, cont)
	if err != nil {
		return term.Cell{}, err
	}
	return c.metaExpand(g)
}

// metaExpand prefixes the meta expandable arguments of a goal with the source
// module, as 2.6 of the report has the compiler do. An argument that carries a
// prefix already is left alone, and so is a variable that appears in a meta
// expandable position of the clause head: the caller has expanded it already.
func (c *Compiler) metaExpand(goal term.Cell) (term.Cell, error) {
	goal = c.Deref(goal)
	f, ok := c.Functor(goal)
	if !ok {
		return goal, nil
	}
	spec := c.MetaSpec(c.Source, f)
	if spec == nil {
		return goal, nil
	}

	args := slices.Collect(c.Args(goal))
	var changed bool
	for i, expand := range spec {
		if !expand || i >= len(args) {
			continue
		}
		a := c.Deref(args[i])
		if _, ok := c.Variable(a); ok && slices.Contains(c.headVars, a) {
			continue
		}
		q, err := c.Qualify(c.Source, a)
		if err != nil {
			return term.Cell{}, err
		}
		if q != args[i] {
			args[i], changed = q, true
		}
	}
	if !changed {
		return goal, nil
	}
	return c.PutCompound(f.Name(), args...)
}

func (c *Compiler) replaceBody(goal, cont term.Cell) (term.Cell, error) {
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
		return term.Cell{}, err
	default:
		return goal, nil
	}

	var ts []term.Cell
	switch err := c.splitOp(&ts, goal); {
	case errors.Is(err, errUnhandled):
		break
	case err != nil:
		return term.Cell{}, err
	default:
		return c.PutSpine(term.NewAtomRune(','), ts...)
	}

	// TODO: implement the rest!
	// meta expansion?

	return goal, nil
}

func (c *Compiler) replaceMacro(goal, cont term.Cell) (term.Cell, error) {
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
			return term.Cell{}, err
		}
		b, err = c.ReplaceBody(b, cont)
		if err != nil {
			return term.Cell{}, err
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
			return term.Cell{}, err
		}
		return c.WithArgs(goal, x, g, xs)
	}

	// bagof(X, G, Xs) -> bagof(X, replaceGoalWithEV(G), Xs)
	if f, ok := c.Functor(goal); ok && f == term.NewFunctor(term.NewAtom("bagof"), 3) {
		x, g, xs := c.Arg(goal, 0), c.Arg(goal, 1), c.Arg(goal, 2)
		g, err := c.replaceGoalWithEV(g, cont)
		if err != nil {
			return term.Cell{}, err
		}
		return c.WithArgs(goal, x, g, xs)
	}

	// setof(X, G, Xs) -> setof(X, replaceGoalWithEV(G), Xs)
	if f, ok := c.Functor(goal); ok && f == term.NewFunctor(term.NewAtom("setof"), 3) {
		x, g, xs := c.Arg(goal, 0), c.Arg(goal, 1), c.Arg(goal, 2)
		g, err := c.replaceGoalWithEV(g, cont)
		if err != nil {
			return term.Cell{}, err
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
			return term.Cell{}, err
		}
		return c.WithArgs(goal, g)
	}

	return term.Cell{}, errUnhandled
}

func (c *Compiler) replaceGoal(goal, cont term.Cell) (term.Cell, error) {
	// X -> call(X)
	if _, ok := c.Variable(goal); ok {
		return c.PutCompound(atomCall, goal)
	}

	// A,B ->
	if f, ok := c.Functor(goal); ok && f == functorAnd {
		a, b := c.Arg(goal, 0), c.Arg(goal, 1)
		g, err := c.traverseConjunction(a, b, cont)
		if err != nil {
			return term.Cell{}, err
		}
		head, err := c.makeNewHead(g)
		if err != nil {
			return term.Cell{}, err
		}
		if err := c.compileLater(head, g); err != nil {
			return term.Cell{}, err
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

func (c *Compiler) replaceGoalWithEV(goal, cont term.Cell) (term.Cell, error) {
	// X^G where X is an Existential Variable.
	if f, ok := c.Functor(goal); ok && f == term.NewFunctor(term.NewAtomRune('^'), 2) {
		x, g := c.Arg(goal, 0), c.Arg(goal, 1)
		g, err := c.replaceGoalWithEV(g, cont)
		if err != nil {
			return term.Cell{}, err
		}
		return c.WithArgs(goal, x, g)
	}

	return c.replaceGoal(goal, cont)
}

func (c *Compiler) traverseConjunction(a, b, cont term.Cell) (term.Cell, error) {
	var err error
	if _, ok := c.Variable(a); ok {
		a, err = c.ReplaceBody(a, cont)
		if err != nil {
			return term.Cell{}, err
		}
	} else {
		var ts []term.Cell
		switch err := c.splitOp(&ts, a); {
		case errors.Is(err, errUnhandled):
			a, err = c.ReplaceBody(a, cont)
			if err != nil {
				return term.Cell{}, err
			}
		case err != nil:
			return term.Cell{}, err
		default:
			a, err = c.PutSpine(term.NewAtomRune(','), ts...)
			if err != nil {
				return term.Cell{}, err
			}
		}
	}
	b, err = c.ReplaceBody(b, cont)
	if err != nil {
		return term.Cell{}, err
	}
	return c.PutCompound(atomAnd, a, b)
}

func (c *Compiler) replaceDisjunction(a, b, cont term.Cell) (term.Cell, error) {
	// Avoid replacing cut.
	if c.cutFree(a) && c.cutFree(b) {
		return c.replaceDisjunction1(a, b, cont)
	}

	return c.traverseDisjunction(a, b, cont)
}

func (c *Compiler) replaceDisjunction1(a, b, cont term.Cell) (term.Cell, error) {
	t, err := c.PutCompound(term.NewAtom("or"), a, b)
	if err != nil {
		return term.Cell{}, err
	}
	head, err := c.makeNewHead(t)
	if err != nil {
		return term.Cell{}, err
	}
	g, err := c.PutCompound(atomOr, a, b)
	if err != nil {
		return term.Cell{}, err
	}
	for body := range c.disjunctionSeq(g, cont) {
		if err := c.compileLater(head, body); err != nil {
			return term.Cell{}, err
		}
	}
	return head, nil
}

func (c *Compiler) compileLater(head, body term.Cell) error {
	cl, err := c.PutCompound(atomNeck, head, body)
	if err != nil {
		return err
	}
	c.todo = append(c.todo, cl)
	return nil
}

func (c *Compiler) cutFree(t term.Cell) bool {
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

func (c *Compiler) traverseDisjunction(a, b, cont term.Cell) (term.Cell, error) {
	// A->C;B -> $if(A, C, B)
	if f, ok := c.Functor(a); ok && f == functorIfThen {
		a, d := c.Arg(a, 0), c.Arg(a, 1)
		a, err := c.ReplaceBody(a, cont)
		if err != nil {
			return term.Cell{}, err
		}
		d, err = c.ReplaceBody(d, cont)
		if err != nil {
			return term.Cell{}, err
		}
		return c.PutCompound(term.NewAtom("$if"), a, d, b)
	}

	// A;B -> $or(A, B)
	a, err := c.ReplaceBody(a, cont)
	if err != nil {
		return term.Cell{}, err
	}
	b, err = c.ReplaceBody(b, cont)
	if err != nil {
		return term.Cell{}, err
	}
	return c.PutCompound(term.NewAtom("$or"), a, b)
}

func (c *Compiler) makeNewHead(t term.Cell) (term.Cell, error) {
	// TODO: A new auxiliary predicate name should be based on t.
	vs := c.VariableSet(t)
	c.counter++
	return c.PutCompound(term.NewAtom(fmt.Sprintf("$aux%d", c.counter)), vs...)
}

func (c *Compiler) disjunctionSeq(t, cont term.Cell) iter.Seq2[term.Cell, error] {
	return func(yield func(term.Cell, error) bool) {
		t = c.Deref(t)
		switch f, _ := c.Functor(t); f {
		case functorOr:
			a, b := c.Arg(t, 0), c.Arg(t, 1)
			a, b = c.Deref(a), c.Deref(b)
			var err error
			if _, ok := c.Variable(a); ok {
				a, err = c.PutCompound(term.NewAtom("call"), a)
				if err != nil {
					_ = yield(term.Cell{}, err)
					return
				}
			}
			if _, ok := c.Variable(b); ok {
				b, err = c.PutCompound(term.NewAtom("call"), b)
				if err != nil {
					_ = yield(term.Cell{}, err)
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
				_ = yield(term.Cell{}, err)
				return
			}
			cut, err := c.PutAtom(term.NewAtomRune('!'))
			if err != nil {
				_ = yield(term.Cell{}, err)
				return
			}
			b, err = c.ReplaceBody(b, cont)
			if err != nil {
				_ = yield(term.Cell{}, err)
				return
			}
			t, err := c.PutSpine(term.NewAtomRune(','), a, cut, b)
			if err != nil {
				_ = yield(term.Cell{}, err)
				return
			}
			if !yield(t, nil) {
				return
			}
		default:
			t, err := c.ReplaceBody(t, cont)
			if err != nil {
				_ = yield(term.Cell{}, err)
				return
			}
			if !yield(t, nil) {
				return
			}
		}
	}
}

// Binarize turns a clause p :- q, r into p(C) :- q(r(C)).
func (c *Compiler) Binarize(head, body, cont term.Cell) (neaHead term.Cell, neaBody term.Cell, _ error) {
	if c.Source == (term.Atom{}) {
		c.Source = atomUserModule
	}
	var err error
	hf, ok := c.Functor(head, term.AllowAtom(true))
	if !ok {
		return term.Cell{}, term.Cell{}, errUnhandled
	}
	args := slices.Collect(c.Args(head))
	args = append(args, cont)
	head, err = c.PutCompound(hf.Name(), args...)
	if err != nil {
		return term.Cell{}, term.Cell{}, err
	}
	body, err = c.addCont(body, cont)
	return head, body, err
}

// addCont appends the continuation to a goal, turning a conjunction into the
// chain of goal structures a binarized clause hands on.
//
// Every link but the first is prefixed with the source module: a continuation
// is a term used as a procedure reference, and in a procedure based module
// system such a term has to carry the module of the clause that built it,
// because the predicate that eventually executes it -- true/1, an arbitrary
// number of calls away -- has no other way to know where its name is defined.
// The first link is the clause's own execute instruction, whose module the
// image records, so it stays bare.
//
// A link of the prolog module stays bare too, and an unprefixed link is read
// as prolog's: the system's own predicates are the ones every module can see
// anyway, and they are the bulk of every continuation built.
func (c *Compiler) addCont(goal, cont term.Cell) (term.Cell, error) {
	f, ok := c.Functor(goal, term.AllowAtom(true))
	if !ok {
		return term.Cell{}, errUnhandled
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
			return term.Cell{}, err
		}
		if c.Source != atomPrologModule {
			y, err = c.Qualify(c.Source, y)
			if err != nil {
				return term.Cell{}, err
			}
		}
		f, ok := c.Functor(x, term.AllowAtom(true))
		if !ok {
			return term.Cell{}, errUnhandled
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

func (c *Compiler) splitOp(out *[]term.Cell, goal term.Cell) error {
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

func (c *Compiler) splitIsRel(out *[]term.Cell, x, b term.Cell) error {
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

func (c *Compiler) splitIs(out *[]term.Cell, x, a term.Cell) error {
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

	args := make([]term.Cell, f.Arity(), f.Arity()+1)
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

func (c *Compiler) splitRel(out *[]term.Cell, op term.Atom, a, b term.Cell) error {
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

func (c *Compiler) CompileBinaryClause(clause *ir.Clause, head, body term.Cell) error {
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

func (c *Compiler) index(t term.Cell) (ir.Index, error) {
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

func (c *Compiler) compileHead(clause *ir.Clause, head term.Cell) (term.Functor, error) {
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

func (c *Compiler) emitTopArgs(clause *ir.Clause, mode Mode, t, ct term.Cell) error {
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

func (c *Compiler) compileTopArg(clause *ir.Clause, mode Mode, t, ct term.Cell) error {
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

func (c *Compiler) compileTopTerm(clause *ir.Clause, mode Mode, x, t term.Cell) error {
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

func (c *Compiler) emitArgs(clause *ir.Clause, mode Mode, t, ct term.Cell) error {
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

func (c *Compiler) compileArgs(clause *ir.Clause, mode Mode, t, ct term.Cell) error {
	f, _ := c.Functor(t)
	for i := 0; i < f.Arity(); i++ {
		if err := c.compileTerm(clause, mode, c.Arg(ct, i), c.Arg(t, i)); err != nil {
			return err
		}
	}
	return nil
}

func (c *Compiler) compileTerm(clause *ir.Clause, mode Mode, x, t term.Cell) error {
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

func (c *Compiler) compileBody(clause *ir.Clause, body term.Cell) (term.Functor, error) {
	// The link is compiled into this clause, so its prefix says nothing the
	// image doesn't already record: drop it and compile the goal itself.
	body, _ = c.Unqualify(body, atomPrologModule)

	if _, ok := c.Variable(body); ok {
		var err error
		body, err = c.PutCompound(term.NewAtom("true"), body)
		if err != nil {
			return term.Functor{}, err
		}
	}

	if a, ok := c.Atom(body); ok && a == term.NewAtom("true") {
		return term.NewFunctor(a, 0), nil
	}

	pi, ok := c.Functor(body)
	if !ok {
		return term.Functor{}, errUnhandled
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
			return term.Functor{}, err
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
			var cont term.Cell
			switch pi.Arity() {
			case 1:
				cont = c.Arg(body, 0)
			case 2:
				cont = c.Arg(body, 1)
				arg := c.Arg(body, 0)
				v, err := c.PutVariable()
				if err != nil {
					return term.Functor{}, err
				}
				if err := c.compileTopTerm(clause, Put, v, arg); err != nil {
					return term.Functor{}, err
				}
				clause.Emit(ir.Instruction{
					OpCode: ir.OpPut,
					A:      ir.Operand{Kind: ir.OperandKindTemp, Index: 0},
					B:      ir.Operand{Kind: ir.OperandKindTerm, Term: v},
				})
			default:
				return term.Functor{}, errors.New("can't inline a builtin with arity more than 1")
			}
			x, err := c.PutVariable()
			if err != nil {
				return term.Functor{}, err
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
		return term.Functor{}, err
	}

	return pi, c.emitBodyTopTerm(clause, body, ct)
}

func (c *Compiler) compileEqual(clause *ir.Clause, a, b term.Cell) error {
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

func (c *Compiler) emitBodyTopTerm(clause *ir.Clause, t, ct term.Cell) error {
	if err := c.compileTopArg(clause, Put, t, ct); err != nil {
		return err
	}
	return c.emitTopArgs(clause, Put, t, ct)
}

func (c *Compiler) classifyArg(x, a term.Cell) (ir.Type, error) {
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

func (c *Compiler) classifyLoad(clause *ir.Clause, x, a term.Cell) (ir.Type, error) {
	if _, ok := c.Variable(a); ok {
		return ir.TypeUnknown, c.Bind(x, a)
	}

	if _, ok := c.Functor(a); !ok {
		return ir.TypeConstant, c.Bind(x, a)
	}

	return ir.TypeUnknown, c.compileTopTerm(clause, Put, x, a)
}

func (c *Compiler) handleConstantRes(clause *ir.Clause, x, res term.Cell) error {
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

// declareModule processes a module declaration. It must come first in a file:
// the text that follows is loaded into the module it names, and the predicates
// of the public list are exported.
func (c *Compiler) declareModule(ctx context.Context, out *ir.Module, name, publics term.Cell) error {
	m, err := c.mustBeModule(name)
	if err != nil {
		return err
	}

	switch m {
	case atomPrologModule, atomUserModule:
		return fmt.Errorf("cannot redefine the %s module", m)
	}

	if len(out.Clauses) > 0 {
		return errors.New("a module declaration must come first in the file")
	}

	mod := c.module(m)
	if mod.File != "" && mod.File != c.File {
		return fmt.Errorf("module %s is already defined in %s", m, mod.File)
	}

	exports := map[term.Functor]struct{}{}
	for pi, err := range c.predicateIndicators(publics) {
		if err != nil {
			return err
		}
		exports[term.NewFunctor(pi.Name(), pi.Arity()+1)] = struct{}{}
	}

	// A predicate that other modules import and this declaration drops from
	// the public list leaves them importing something that is no longer
	// exported, which is worth saying out loud.
	if c.Warn == nil {
		c.Warn = func(error) {}
	}
	for f := range mod.Exports {
		if _, ok := exports[f]; ok {
			continue
		}
		for _, other := range c.Modules {
			if from, ok := other.Imports[f]; ok && from == m {
				c.Warn(fmt.Errorf("%s:%s is imported by %s but no longer exported", m, unbinarize(f), other.Name))
			}
		}
	}

	if err := c.eraseModule(ctx, m); err != nil {
		return err
	}
	mod.File = c.File
	mod.Exports = exports

	// The predicates that follow are this module's, and so is everything its
	// directives do.
	c.Source = m
	c.Module = m
	out.Name = m
	return nil
}

// declareMeta records a meta_predicate declaration: which arguments of the
// named predicates are module name expanded.
func (c *Compiler) declareMeta(spec term.Cell) error {
	mod := c.module(c.Source)
	for s := range c.listOrSingleton(spec) {
		s = c.Deref(s)
		f, ok := c.Functor(s)
		if !ok {
			return fmt.Errorf("invalid meta_predicate specification: %s", c.Inspect(s))
		}
		args := make([]bool, f.Arity())
		for i := range args {
			a := c.Deref(c.Arg(s, i))
			// ':' and an integer mean expand; anything else, +, - or ? say,
			// means leave alone.
			if x, ok := c.Atom(a); ok && x == atomColon {
				args[i] = true
			}
			if _, ok := c.Integer(a); ok {
				args[i] = true
			}
		}
		mod.Meta[f] = args
	}
	return nil
}

// headVariables collects the variables the head holds in a meta expandable
// argument position, if the predicate is a meta predicate.
func (c *Compiler) headVariables(head term.Cell) []term.Cell {
	head = c.Deref(head)
	f, ok := c.Functor(head)
	if !ok {
		return nil
	}
	spec := c.MetaSpec(c.Source, f)
	if spec == nil {
		return nil
	}
	var vs []term.Cell
	for i, expand := range spec {
		if !expand || i >= f.Arity() {
			continue
		}
		a := c.Deref(c.Arg(head, i))
		if _, ok := c.Variable(a); ok {
			vs = append(vs, a)
		}
	}
	return vs
}
