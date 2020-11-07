package prolog

import (
	"errors"
	"fmt"
	"strings"

	"github.com/sirupsen/logrus"
)

const (
	Nop byte = iota
	Enter
	Call
	Exit
	Const
	Var
	Functor
	Pop
)

type Engine struct {
	procedures map[pFunctor][]clause
}

func (e *Engine) Compile(s string) error {
	p := NewParser(s)
	ts, err := p.Clauses()
	if err != nil {
		return err
	}
	for _, t := range ts {
		var c clause
		if err := c.compile(t); err != nil {
			return err
		}
		if e.procedures == nil {
			e.procedures = map[pFunctor][]clause{}
		}
		e.procedures[c.pFunctor] = append(e.procedures[c.pFunctor], c)
	}
	return nil
}

func (e *Engine) Query(s string) ([]*Variable, error) {
	p := NewParser(s)
	t, err := p.Clause()
	if err != nil {
		return nil, err
	}
	var c clause
	if err := c.compile(&Compound{
		Functor: ":-",
		Args:    []Term{Atom("user"), t},
	}); err != nil {
		return nil, err
	}
	if e.procedures == nil {
		e.procedures = map[pFunctor][]clause{}
	}
	e.procedures[c.pFunctor] = []clause{c}
	vars, err := e.arrive(c.pFunctor, List(), nil)
	if err != nil {
		return nil, err
	}
	for i, v := range vars {
		if v.Ref == nil {
			continue
		}
		vars[i].Ref = v.Ref.Simplify()
	}
	return vars, nil
}

func (e *Engine) arrive(pf pFunctor, args Term, cont []continuation) ([]*Variable, error) {
	cs := e.procedures[pf]
	for _, c := range cs {
		vars := make([]*Variable, len(c.vars))
		for i, n := range c.vars {
			vars[i] = &Variable{Name: n}
		}
		if err := e.exec(c.bytecode, c.xrTable, vars, cont, args); err != nil {
			logrus.WithField("err", err).Debug("failed")
			continue
		}
		return vars, nil
	}
	return nil, errors.New("failed")
}

func (e *Engine) exec(pc bytecode, xr []xrRecord, vars []*Variable, cont []continuation, args Term) error {
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
		case Const:
			log.Debug("const")
			x := xr[pc[1]].(Term)
			var arest Variable
			if !args.Unify(Cons(x, &arest)) {
				return errors.New("const")
			}
			pc = pc[2:]
			args = &arest
		case Var:
			log.Debug("var")
			v := vars[pc[1]]
			var arest Variable
			if !args.Unify(Cons(v, &arest)) {
				return errors.New("var")
			}
			pc = pc[2:]
			args = &arest
		case Functor:
			log.Debug("functor")
			x, ok := xr[pc[1]].(*pFunctor)
			if !ok {
				return errors.New("not a principal functor")
			}
			var arg, arest Variable
			if !args.Unify(Cons(&arg, &arest)) {
				return errors.New("functor")
			}
			c := Compound{
				Functor: x.functor,
				Args:    make([]Term, x.arity),
			}
			for i := 0; i < x.arity; i++ {
				var arg Variable
				c.Args[i] = &arg
			}
			if !c.Unify(&arg) {
				return errors.New("not a functor")
			}
			pc = pc[2:]
			args = List(c.Args...)
			astack = Cons(&arest, astack)
		case Pop:
			log.Debug("pop")
			if !args.Unify(List()) {
				return errors.New("args is not empty")
			}
			pc = pc[1:]
			var a, arest Variable
			if !astack.Unify(Cons(&a, &arest)) {
				return errors.New("pop")
			}
			args = &a
			astack = &arest
		case Enter:
			log.Debug("enter")
			if !args.Unify(List()) {
				return errors.New("args is not empty")
			}
			if !astack.Unify(List()) {
				return errors.New("astack is not empty")
			}
			pc = pc[1:]
			var v Variable
			args = &v
			astack = &v
		case Call:
			log.Debug("call")
			x := xr[pc[1]].(*pFunctor)
			pc = pc[2:]
			_, err := e.arrive(*x, astack, append(cont, continuation{pc: pc, xr: xr, vars: vars}))
			return err
		case Exit:
			log.Debug("exit")
			if len(cont) == 0 {
				return nil
			}
			var c continuation
			c, cont = cont[len(cont)-1], cont[:len(cont)-1]
			pc, xr, vars = c.pc, c.xr, c.vars
		default:
			return fmt.Errorf("unknown(%d)", pc[0])
		}
	}
	return nil
}

type clause struct {
	pFunctor
	xrTable  []xrRecord
	vars     []string
	bytecode bytecode
}

func (c *clause) compile(t Term) error {
	switch t := t.(type) {
	case *Compound:
		if t.Functor == ":-" {
			return c.compileClause(t.Args[0], t.Args[1:])
		}
		return c.compileClause(t, nil)
	default:
		return errors.New("")
	}
}

func (c *clause) compileClause(head Term, body []Term) error {
	switch head := head.(type) {
	case Atom:
		c.pFunctor = *head.PrincipalFunctor()
	case *Compound:
		c.pFunctor = *head.PrincipalFunctor()
		for _, a := range head.Args {
			if err := c.compileArg(a); err != nil {
				return err
			}
		}
	default:
		return errors.New("not an atom nor compound")
	}
	if len(body) != 0 {
		c.bytecode = append(c.bytecode, Enter)
	}
	for _, b := range body {
		if err := c.compilePred(b.(*Compound)); err != nil {
			return err
		}
	}
	c.bytecode = append(c.bytecode, Exit)
	return nil
}

func (c *clause) compilePred(p *Compound) error {
	for _, a := range p.Args {
		if err := c.compileArg(a); err != nil {
			return err
		}
	}
	c.bytecode = append(c.bytecode, Call, c.xrOffset(&pFunctor{
		functor: p.Functor,
		arity:   len(p.Args),
	}))
	return nil
}

func (c *clause) compileArg(a Term) error {
	switch a := a.(type) {
	case Atom:
		c.bytecode = append(c.bytecode, Const, c.xrOffset(a))
	case Integer:
		c.bytecode = append(c.bytecode, Const, c.xrOffset(a))
	case *Variable:
		c.bytecode = append(c.bytecode, Var, c.varOffset(a))
	case *Compound:
		c.bytecode = append(c.bytecode, Functor, c.xrOffset(&pFunctor{functor: a.Functor, arity: len(a.Args)}))
		for _, n := range a.Args {
			if err := c.compileArg(n); err != nil {
				return err
			}
		}
		c.bytecode = append(c.bytecode, Pop)
	default:
		return errors.New("unknown")
	}
	return nil
}

func (c *clause) xrOffset(o xrRecord) byte {
	for i, r := range c.xrTable {
		if r.Equal(o) {
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
		case Nop:
			ret = append(ret, "nop")
		case Const:
			i++
			ret = append(ret, fmt.Sprintf("const %d", b[i]))
		case Var:
			i++
			ret = append(ret, fmt.Sprintf("var %d", b[i]))
		case Functor:
			i++
			ret = append(ret, fmt.Sprintf("functor %d", b[i]))
		case Pop:
			ret = append(ret, "pop")
		case Enter:
			ret = append(ret, "enter")
		case Call:
			i++
			ret = append(ret, fmt.Sprintf("call %d", b[i]))
		case Exit:
			ret = append(ret, "exit")
		default:
			ret = append(ret, fmt.Sprintf("unknown(%d)", b[i]))
		}
	}
	return strings.Join(ret, "; ")
}

type xrRecord interface {
	Equal(xrRecord) bool
}

// Principal Functor
type pFunctor struct {
	functor Atom
	arity   int
}

func (p *pFunctor) String() string {
	return fmt.Sprintf("%s/%d", p.functor, p.arity)
}

func (p *pFunctor) Equal(r xrRecord) bool {
	v, ok := r.(*pFunctor)
	return ok && p.functor == v.functor && p.arity == v.arity
}

type continuation struct {
	pc   bytecode
	xr   []xrRecord
	vars []*Variable
}
