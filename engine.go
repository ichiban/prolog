package prolog

import (
	"errors"
	"fmt"
	"strings"

	"github.com/sirupsen/logrus"
)

const (
	opVoid byte = iota
	opEnter
	opCall
	opExit
	opConst
	opVar
	opFunctor
	opPop
)

type Engine struct {
	operators  operators
	procedures map[string]procedure
}

func NewEngine() (*Engine, error) {
	var e Engine
	e.Register0("!", Cut)
	e.Register0("repeat", Repeat)
	e.Register1("call", e.Call)
	e.Register1("assertz", e.Assertz)
	e.Register1("var", TypeVar)
	e.Register1("atom", TypeAtom)
	e.Register1("integer", TypeInteger)
	e.Register1("compound", TypeCompound)
	e.Register2("=", Unify)
	e.Register2("=..", Univ)
	e.Register2("copy_term", CopyTerm)
	e.Register3("arg", Arg)
	e.Register3("functor", Functor)
	e.Register3("op", e.Op)
	e.Register3("current_op", e.CurrentOp)
	err := e.Load(`
/*
 *  bootstrap script
 */

% operators
:-(op(1200, xfx, :-)).
:-(op(1200, xfx, -->)).
:-(op(1200, fx, :-)).
:-(op(1200, fx, ?-)).
:-(op(1100, xfy, ;)).
:-(op(1050, xfy, ->)).
:-(op(1000, xfy, ,)).
:-(op(900, fy, \+)).
:-(op(700, xfx, =)).
:-(op(700, xfx, \=)).
:-(op(700, xfx, ==)).
:-(op(700, xfx, \==)).
:-(op(700, xfx, @<)).
:-(op(700, xfx, @=<)).
:-(op(700, xfx, @>)).
:-(op(700, xfx, @>=)).
:-(op(700, xfx, is)).
:-(op(700, xfx, =:=)).
:-(op(700, xfx, =\=)).
:-(op(700, xfx, <)).
:-(op(700, xfx, =<)).
:-(op(700, xfx, =\=)).
:-(op(700, xfx, >)).
:-(op(700, xfx, >=)).
:-(op(700, xfx, =..)).
:-(op(500, yfx, +)).
:-(op(500, yfx, -)).
:-(op(500, yfx, /\)).
:-(op(500, yfx, \/)).
:-(op(400, yfx, *)).
:-(op(400, yfx, /)).
:-(op(400, yfx, //)).
:-(op(400, yfx, rem)).
:-(op(400, yfx, mod)).
:-(op(400, yfx, <<)).
:-(op(400, yfx, >>)).
:-(op(200, xfx, **)).
:-(op(200, xfy, ^)).
:-(op(200, fy, \)).
:-(op(200, fy, +)).
:-(op(200, fy, -)).
:-(op(100, xfx, @)).
:-(op(50, xfx, :)).

% conjunction
P, Q :- call(P), call(Q).

% disjunction
P; Q :- call(P).
P; Q :- call(Q).

% true/false
true.
false :- a = b.

% logic and control
\+P :- call(P), !, false.
\+P :- true.
once(P) :- call(P), !.

% not unifiable
X \= Y :- \+(X = Y).

% type testing
atomic(X) :- atom(X).
atomic(X) :- integer(X).
nonvar(X) :- \+var(X).
number(X) :- integer(X).
`)
	return &e, err
}

type procedure interface {
	Call(*Engine, Term, func() (bool, error)) (bool, error)
}

func (e *Engine) Load(s string) error {
	if e.procedures == nil {
		e.procedures = map[string]procedure{}
	}

	p := NewParser(s, &e.operators)
	for {
		if _, err := p.accept(TokenEOS); err == nil {
			return nil
		}

		t, err := p.Clause()
		if err != nil {
			return err
		}

		if _, err := e.Assertz(t, done); err != nil {
			return err
		}
	}
}

func (e *Engine) Query(s string, cb func([]Variable) bool) (bool, error) {
	if cb == nil {
		cb = func([]Variable) bool { return true }
	}

	t, err := NewParser(s, &e.operators).Clause()
	if err != nil {
		return false, err
	}

	a := newAssignment(t)

	ok, err := e.Call(t, func() (bool, error) {
		simp := make([]Variable, 0, len(a))
		for _, v := range a {
			v := Variable{
				Name: v.Name,
				Ref:  Simplify(v.Ref),
			}
			if v.Ref == nil {
				continue
			}
			simp = append(simp, v)
		}
		return cb(simp), nil
	})
	if err != nil {
		if errors.Is(err, errCut) {
			return ok, nil
		}
		return false, err
	}
	return ok, nil
}

func (e *Engine) StringTerm(t Term) string {
	return t.TermString(e.operators)
}

func (e *Engine) Register0(name string, p func(func() (bool, error)) (bool, error)) {
	if e.procedures == nil {
		e.procedures = map[string]procedure{}
	}
	e.procedures[fmt.Sprintf("%s/0", name)] = predicate0(p)
}

func (e *Engine) Register1(name string, p func(Term, func() (bool, error)) (bool, error)) {
	if e.procedures == nil {
		e.procedures = map[string]procedure{}
	}
	e.procedures[fmt.Sprintf("%s/1", name)] = predicate1(p)
}

func (e *Engine) Register2(name string, p func(Term, Term, func() (bool, error)) (bool, error)) {
	if e.procedures == nil {
		e.procedures = map[string]procedure{}
	}
	e.procedures[fmt.Sprintf("%s/2", name)] = predicate2(p)
}

func (e *Engine) Register3(name string, p func(Term, Term, Term, func() (bool, error)) (bool, error)) {
	if e.procedures == nil {
		e.procedures = map[string]procedure{}
	}
	e.procedures[fmt.Sprintf("%s/3", name)] = predicate3(p)
}

func (e *Engine) arrive(name string, args Term, k func() (bool, error)) (bool, error) {
	logrus.WithFields(logrus.Fields{
		"name": name,
		"args": args,
	}).Debug("arrive")
	p := e.procedures[name]
	if p == nil {
		return false, fmt.Errorf("unknown procedure: %s", name)
	}
	return p.Call(e, args, k)
}

func (e *Engine) exec(pc bytecode, xr []Term, vars []*Variable, k func() (bool, error), args, astack Term) (bool, error) {
	for len(pc) != 0 {
		log := logrus.WithFields(logrus.Fields{
			"pc":     pc,
			"xr":     xr,
			"vars":   vars,
			"args":   args,
			"astack": astack,
		})
		switch pc[0] {
		case opVoid:
			log.Debug("void")
			pc = pc[1:]
		case opConst:
			log.Debug("const")
			x := xr[pc[1]]
			var arest Variable
			if !args.Unify(Cons(x, &arest)) {
				return false, nil
			}
			pc = pc[2:]
			args = &arest
		case opVar:
			log.Debug("var")
			v := vars[pc[1]]
			var arest Variable
			if !args.Unify(Cons(v, &arest)) {
				return false, nil
			}
			pc = pc[2:]
			args = &arest
		case opFunctor:
			log.Debug("functor")
			x := xr[pc[1]]
			var arg, arest Variable
			if !args.Unify(Cons(&arg, &arest)) {
				return false, nil
			}
			var fatom, farity Variable
			if !x.Unify(&Compound{
				Functor: "/",
				Args:    []Term{&fatom, &farity},
			}) {
				return false, nil
			}
			ok, err := Functor(&arg, &fatom, &farity, done)
			if err != nil {
				return false, err
			}
			if !ok {
				return false, nil
			}
			pc = pc[2:]
			args = &Variable{}
			ok, err = Univ(&arg, Cons(&fatom, args), done)
			if err != nil {
				return false, err
			}
			if !ok {
				return false, nil
			}
			astack = Cons(&arest, astack)
		case opPop:
			log.Debug("pop")
			if !args.Unify(List()) {
				return false, nil
			}
			pc = pc[1:]
			var a, arest Variable
			if !astack.Unify(Cons(&a, &arest)) {
				return false, nil
			}
			args = &a
			astack = &arest
		case opEnter:
			log.Debug("enter")
			if !args.Unify(List()) {
				return false, nil
			}
			if !astack.Unify(List()) {
				return false, nil
			}
			pc = pc[1:]
			var v Variable
			args = &v
			astack = &v
		case opCall:
			log.Debug("call")
			x := xr[pc[1]]
			if !args.Unify(List()) {
				return false, nil
			}
			pc = pc[2:]
			return e.arrive(x.TermString(e.operators), astack, func() (bool, error) {
				var v Variable
				return e.exec(pc, xr, vars, k, &v, &v)
			})
		case opExit:
			log.Debug("exit")
			return k()
		default:
			return false, fmt.Errorf("unknown(%d)", pc[0])
		}
	}
	return false, errors.New("non-exit end of bytecode")
}

type clauses []clause

func (cs clauses) Call(e *Engine, args Term, k func() (bool, error)) (bool, error) {
	if len(cs) == 0 {
		return false, nil
	}

	a := newAssignment(args)

	log := logrus.WithFields(logrus.Fields{
		"name": cs[0].name,
		"args": args,
	})

	for i, c := range cs {
		log = log.WithField("choice", i)

		switch i {
		case 0:
			log.Info("call")
		default:
			log.Info("redo")
		}

		vars := make([]*Variable, len(c.vars))
		for i := range c.vars {
			vars[i] = &Variable{}
		}

		ok, err := e.exec(c.bytecode, c.xrTable, vars, k, args, List())
		if err != nil {
			if errors.Is(err, errCut) {
				log.Info("cut")
				return ok, err
			}
			log.Info("exception")
			return false, err
		}
		if ok {
			log.Info("exit")
			return true, nil
		}

		a.reset()
	}

	log.Info("fail")
	return false, nil
}

type clause struct {
	name     string
	xrTable  []Term
	vars     []string
	bytecode bytecode
}

func (c *clause) compile(t Term) error {
	switch t := Resolve(t).(type) {
	case Atom:
		return c.compileClause(t, nil)
	case *Compound:
		if t.Functor == ":-" {
			return c.compileClause(t.Args[0], t.Args[1])
		}
		return c.compileClause(t, nil)
	default:
		return fmt.Errorf("not a compound: %s", t)
	}
}

func (c *clause) compileClause(head Term, body Term) error {
	switch head := head.(type) {
	case Atom:
	case *Compound:
		for _, a := range head.Args {
			if err := c.compileArg(a); err != nil {
				return err
			}
		}
	default:
		return fmt.Errorf("not an atom nor compound: %s", head)
	}
	if body != nil {
		c.bytecode = append(c.bytecode, opEnter)
		for {
			p, ok := body.(*Compound)
			if !ok || p.Functor != "," || len(p.Args) != 2 {
				break
			}
			if err := c.compilePred(p.Args[0]); err != nil {
				return err
			}
			body = p.Args[1]
		}
		if err := c.compilePred(body); err != nil {
			return err
		}
	}
	c.bytecode = append(c.bytecode, opExit)
	return nil
}

func (c *clause) compilePred(p Term) error {
	switch p := p.(type) {
	case Atom:
		c.bytecode = append(c.bytecode, opCall, c.xrOffset(&Compound{
			Functor: "/",
			Args:    []Term{p, Integer(0)},
		}))
		return nil
	case *Compound:
		for _, a := range p.Args {
			if err := c.compileArg(a); err != nil {
				return err
			}
		}
		c.bytecode = append(c.bytecode, opCall, c.xrOffset(&Compound{
			Functor: "/",
			Args:    []Term{p.Functor, Integer(len(p.Args))},
		}))
		return nil
	default:
		return errors.New("not a predicate")
	}
}

func (c *clause) compileArg(a Term) error {
	switch a := a.(type) {
	case Atom:
		c.bytecode = append(c.bytecode, opConst, c.xrOffset(a))
	case Integer:
		c.bytecode = append(c.bytecode, opConst, c.xrOffset(a))
	case *Variable:
		c.bytecode = append(c.bytecode, opVar, c.varOffset(a))
	case *Compound:
		c.bytecode = append(c.bytecode, opFunctor, c.xrOffset(&Compound{
			Functor: "/",
			Args:    []Term{a.Functor, Integer(len(a.Args))},
		}))
		for _, n := range a.Args {
			if err := c.compileArg(n); err != nil {
				return err
			}
		}
		c.bytecode = append(c.bytecode, opPop)
	default:
		return errors.New("unknown")
	}
	return nil
}

func (c *clause) xrOffset(o Term) byte {
	for i, r := range c.xrTable {
		if r.Unify(o) {
			return byte(i)
		}
	}
	c.xrTable = append(c.xrTable, o)
	return byte(len(c.xrTable) - 1)
}

func (c *clause) varOffset(o *Variable) byte {
	for i, v := range c.vars {
		if v == o.Name {
			return byte(i)
		}
	}
	c.vars = append(c.vars, o.Name)
	return byte(len(c.vars) - 1)
}

type bytecode []byte

func (b bytecode) String() string {
	ret := make([]string, 0, len(b))
	for i := 0; i < len(b); i++ {
		switch b[i] {
		case opVoid:
			ret = append(ret, "void")
		case opConst:
			i++
			ret = append(ret, fmt.Sprintf("const %d", b[i]))
		case opVar:
			i++
			ret = append(ret, fmt.Sprintf("var %d", b[i]))
		case opFunctor:
			i++
			ret = append(ret, fmt.Sprintf("functor %d", b[i]))
		case opPop:
			ret = append(ret, "pop")
		case opEnter:
			ret = append(ret, "enter")
		case opCall:
			i++
			ret = append(ret, fmt.Sprintf("call %d", b[i]))
		case opExit:
			ret = append(ret, "exit")
		default:
			ret = append(ret, fmt.Sprintf("unknown(%d)", b[i]))
		}
	}
	return strings.Join(ret, "; ")
}

type predicate0 func(func() (bool, error)) (bool, error)

func (p predicate0) Call(e *Engine, args Term, k func() (bool, error)) (bool, error) {
	if !args.Unify(List()) {
		return false, errors.New("wrong number of arguments")
	}

	return p(func() (bool, error) {
		return e.exec([]byte{opExit}, nil, nil, k, nil, nil)
	})
}

type predicate1 func(Term, func() (bool, error)) (bool, error)

func (p predicate1) Call(e *Engine, args Term, k func() (bool, error)) (bool, error) {
	var v1 Variable
	if !args.Unify(List(&v1)) {
		return false, fmt.Errorf("wrong number of arguments: %s", args)
	}

	return p(&v1, func() (bool, error) {
		return e.exec([]byte{opExit}, nil, nil, k, nil, nil)
	})
}

type predicate2 func(Term, Term, func() (bool, error)) (bool, error)

func (p predicate2) Call(e *Engine, args Term, k func() (bool, error)) (bool, error) {
	var v1, v2 Variable
	if !args.Unify(List(&v1, &v2)) {
		return false, errors.New("wrong number of arguments")
	}

	return p(&v1, &v2, func() (bool, error) {
		return e.exec([]byte{opExit}, nil, nil, k, nil, nil)
	})
}

type predicate3 func(Term, Term, Term, func() (bool, error)) (bool, error)

func (p predicate3) Call(e *Engine, args Term, k func() (bool, error)) (bool, error) {
	var v1, v2, v3 Variable
	if !args.Unify(List(&v1, &v2, &v3)) {
		return false, errors.New("wrong number of arguments")
	}

	return p(&v1, &v2, &v3, func() (bool, error) {
		return e.exec([]byte{opExit}, nil, nil, k, nil, nil)
	})
}

type assignment []*Variable

func newAssignment(ts ...Term) assignment {
	var a assignment
	for _, t := range ts {
		a.add(t)
	}
	return a
}

func (a *assignment) add(t Term) {
	switch t := t.(type) {
	case *Variable:
		if t.Ref != nil {
			a.add(t.Ref)
			return
		}
		for _, v := range *a {
			if v == t {
				return
			}
		}
		*a = append(*a, t)
	case *Compound:
		for _, arg := range t.Args {
			a.add(arg)
		}
	}
}

func (a assignment) reset() {
	logrus.WithField("vars", a).Debug("reset")
	for _, v := range a {
		v.Ref = nil
	}
}

func done() (bool, error) {
	return true, nil
}
