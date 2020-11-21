package prolog

import (
	"errors"
	"fmt"
	"strings"

	"github.com/sirupsen/logrus"
)

const (
	nop byte = iota
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
	e.Register2("=", Unify)
	e.Register2("=..", Univ)
	e.Register3("functor", Functor)
	e.Register3("op", Op(&e))
	e.Register3("current_op", CurrentOp(&e))
	err := e.Load(`
:-(op(1200, xfx, :-)).
:-(op(1200, xfx, -->)).
:-(op(1200, fx, :-)).
:-(op(1200, fx, ?-)).
:-(op(1100, xfy, ;)).
:-(op(1050, xfy, ->)).
:-(op(1000, xfy, ,)).
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
`)
	return &e, err
}

type procedure interface {
	Call(*Engine, Term, []continuation) (bool, error)
}

func (e *Engine) Load(s string) error {
	if e.procedures == nil {
		e.procedures = map[string]procedure{}
	}

	p := NewParser(s, e.operators)
	ts, err := p.Program()
	if err != nil {
		return err
	}
	for _, t := range ts {
		var name string
		switch t := t.(type) {
		case Atom:
			name = fmt.Sprintf("%s/0", t)
		case *Compound:
			type pf struct {
				functor Atom
				arity   int
			}
			switch (pf{functor: t.Functor, arity: len(t.Args)}) {
			case pf{functor: ":-", arity: 2}:
				switch h := t.Args[0].(type) {
				case Atom:
					name = fmt.Sprintf("%s/0", h)
				case *Compound:
					name = fmt.Sprintf("%s/%d", h.Functor, len(h.Args))
				default:
					return errors.New("not a clause")
				}
			case pf{functor: ":-", arity: 1}:
				ok, err := e.call(t.Args[0])
				if err != nil {
					return err
				}
				if !ok {
					return errors.New("directive failed")
				}
				continue
			default:
				name = fmt.Sprintf("%s/%d", t.Functor, len(t.Args))
			}
		default:
			return errors.New("not a clause")
		}

		p, ok := e.procedures[name]
		if !ok {
			p = clauses{}
		}

		switch p := p.(type) {
		case clauses:
			var c clause
			if err := c.compile(t); err != nil {
				return err
			}

			e.procedures[name] = append(p, c)
		default:
			return errors.New("builtin")
		}
	}
	return nil
}

func (e *Engine) Query(s string, cb func([]*Variable) bool) (bool, error) {
	if cb == nil {
		cb = func([]*Variable) bool { return true }
	}

	t, err := NewParser(s, e.operators).Clause()
	if err != nil {
		return false, err
	}

	var vars []*Variable
	variables(&vars, t)

	ok, err := e.call(t)
	if err != nil {
		return false, err
	}

	if !ok {
		return false, nil
	}

	for _, v := range vars {
		v.Ref = v.Ref.Simplify()
	}

	return cb(vars), nil
}

func (e *Engine) StringTerm(t Term) string {
	return t.TermString(e.operators)
}

func (e *Engine) call(t Term) (bool, error) {
	var name string
	var args Term
	switch f := t.(type) {
	case Atom:
		name = fmt.Sprintf("%s/0", f)
		args = List()
	case *Compound:
		name = fmt.Sprintf("%s/%d", f.Functor, len(f.Args))
		args = List(f.Args...)
	default:
		return false, errors.New("not callable")
	}

	p, ok := e.procedures[name]
	if !ok {
		return false, fmt.Errorf("unknown procedure: %s", name)
	}

	return p.Call(e, args, nil)
}

func (e *Engine) Register0(name string, p func() (bool, error)) {
	if e.procedures == nil {
		e.procedures = map[string]procedure{}
	}
	e.procedures[fmt.Sprintf("%s/0", name)] = predicate0(p)
}

func (e *Engine) Register1(name string, p func(Term) (bool, error)) {
	if e.procedures == nil {
		e.procedures = map[string]procedure{}
	}
	e.procedures[fmt.Sprintf("%s/1", name)] = predicate1(p)
}

func (e *Engine) Register2(name string, p func(Term, Term) (bool, error)) {
	if e.procedures == nil {
		e.procedures = map[string]procedure{}
	}
	e.procedures[fmt.Sprintf("%s/2", name)] = predicate2(p)
}

func (e *Engine) Register3(name string, p func(Term, Term, Term) (bool, error)) {
	if e.procedures == nil {
		e.procedures = map[string]procedure{}
	}
	e.procedures[fmt.Sprintf("%s/3", name)] = predicate3(p)
}

func (e *Engine) arrive(name string, args Term, cont []continuation) (bool, error) {
	log := logrus.WithFields(logrus.Fields{
		"name": name,
		"args": args,
		"cont": cont,
	})
	log.Debug("arrive")
	p := e.procedures[name]
	return p.Call(e, args, cont)
}

func (e *Engine) exec(pc bytecode, xr []Term, vars []*Variable, cont []continuation, args Term) (bool, error) {
	astack := List()
	for len(pc) != 0 {
		log := logrus.WithFields(logrus.Fields{
			"pc":     pc,
			"xr":     xr,
			"vars":   vars,
			"cont":   cont,
			"args":   args,
			"astack": astack,
		})
		switch pc[0] {
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
			ok, err := Functor(&arg, &fatom, &farity)
			if err != nil {
				return false, err
			}
			if !ok {
				return false, nil
			}
			pc = pc[2:]
			args = &Variable{}
			ok, err = Univ(&arg, Cons(&fatom, args))
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
			pc = pc[2:]
			return e.arrive(x.TermString(e.operators), astack, append(cont, continuation{pc: pc, xr: xr, vars: vars}))
		case opExit:
			log.Debug("exit")
			if len(cont) == 0 {
				return true, nil
			}
			var c continuation
			c, cont = cont[len(cont)-1], cont[:len(cont)-1]
			pc, xr, vars = c.pc, c.xr, c.vars
		default:
			return false, fmt.Errorf("unknown(%d)", pc[0])
		}
	}
	return false, errors.New("non-exit end of bytecode")
}

type clauses []clause

func (cs clauses) Call(e *Engine, args Term, cont []continuation) (bool, error) {
	for _, c := range cs {
		vars := make([]*Variable, len(c.vars))
		for i, n := range c.vars {
			vars[i] = &Variable{Name: n}
		}
		ok, err := e.exec(c.bytecode, c.xrTable, vars, cont, args)
		if err != nil {
			return false, err
		}
		if ok {
			return true, nil
		}
	}
	return false, nil
}

type clause struct {
	xrTable  []Term
	vars     []string
	bytecode bytecode
}

func (c *clause) compile(t Term) error {
	switch t := t.(type) {
	case *Compound:
		if t.Functor == ":-" {
			return c.compileClause(t.Args[0], t.Args[1])
		}
		return c.compileClause(t, nil)
	default:
		return errors.New("")
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
			if ok || p.Functor != "," || len(p.Args) != 2 {
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
		case nop:
			ret = append(ret, "nop")
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

type continuation struct {
	pc   bytecode
	xr   []Term
	vars []*Variable
}

type predicate0 func() (bool, error)

func (p predicate0) Call(e *Engine, args Term, cont []continuation) (bool, error) {
	if !args.Unify(List()) {
		return false, errors.New("wrong number of arguments")
	}
	ok, err := p()
	if err != nil {
		return false, err
	}
	if !ok {
		return false, nil
	}
	return e.exec([]byte{opExit}, nil, nil, cont, &Variable{})
}

type predicate1 func(Term) (bool, error)

func (p predicate1) Call(e *Engine, args Term, cont []continuation) (bool, error) {
	var v1 Variable
	if !args.Unify(List(&v1)) {
		return false, errors.New("wrong number of arguments")
	}
	t, err := p(&v1)
	if err != nil {
		return false, err
	}
	if !t {
		return false, nil
	}
	return e.exec([]byte{opExit}, nil, nil, cont, &Variable{})
}

type predicate2 func(Term, Term) (bool, error)

func (p predicate2) Call(e *Engine, args Term, cont []continuation) (bool, error) {
	var v1, v2 Variable
	if !args.Unify(List(&v1, &v2)) {
		return false, errors.New("wrong number of arguments")
	}
	ok, err := p(&v1, &v2)
	if err != nil {
		return false, err
	}
	if !ok {
		return false, nil
	}
	return e.exec([]byte{opExit}, nil, nil, cont, &Variable{})
}

type predicate3 func(Term, Term, Term) (bool, error)

func (p predicate3) Call(e *Engine, args Term, cont []continuation) (bool, error) {
	var v1, v2, v3 Variable
	if !args.Unify(List(&v1, &v2, &v3)) {
		return false, errors.New("wrong number of arguments")
	}
	ok, err := p(&v1, &v2, &v3)
	if err != nil {
		return false, err
	}
	if !ok {
		return false, nil
	}
	return e.exec([]byte{opExit}, nil, nil, cont, &Variable{})
}

func variables(vs *[]*Variable, t Term) {
	switch t := t.(type) {
	case *Variable:
		*vs = append(*vs, t)
	case *Compound:
		for _, a := range t.Args {
			variables(vs, a)
		}
	}
}
