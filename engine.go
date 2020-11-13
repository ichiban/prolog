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
	procedures map[string][]clause
}

func (e *Engine) Compile(s string) error {
	p := NewParser(s, DefaultOperators)
	ts, err := p.Program()
	if err != nil {
		return err
	}
	for _, t := range ts {
		var c clause
		if err := c.compile(t); err != nil {
			return err
		}
		if e.procedures == nil {
			e.procedures = map[string][]clause{}
		}
		e.procedures[c.name] = append(e.procedures[c.name], c)
	}
	return nil
}

func (e *Engine) Query(s string) ([]*Variable, error) {
	p := NewParser(s, DefaultOperators)
	t, err := p.clause()
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
		e.procedures = map[string][]clause{}
	}
	e.procedures[c.name] = []clause{c}
	vars, err := e.arrive(c.name, List(), nil)
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

func (e *Engine) arrive(name string, args Term, cont []continuation) ([]*Variable, error) {
	log := logrus.WithFields(logrus.Fields{
		"name": name,
		"args": args,
		"cont": cont,
	})
	log.Debug("arrive")
	cs := e.procedures[name]
	for _, c := range cs {
		vars := make([]*Variable, len(c.vars))
		for i, n := range c.vars {
			vars[i] = &Variable{Name: n}
		}
		if err := e.exec(c.bytecode, c.xrTable, vars, cont, args); err != nil {
			log.WithField("err", err).Debug("failed")
			continue
		}
		return vars, nil
	}
	return nil, errors.New("failed")
}

func (e *Engine) exec(pc bytecode, xr []Term, vars []*Variable, cont []continuation, args Term) error {
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
			x := xr[pc[1]]
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
			x := xr[pc[1]]
			var arg, arest Variable
			if !args.Unify(Cons(&arg, &arest)) {
				return errors.New("functor")
			}
			var fatom, farity Variable
			if !x.Unify(&Compound{
				Functor: "/",
				Args:    []Term{&fatom, &farity},
			}) {
				return errors.New("functor")
			}
			ok, err := PFunctor(&arg, &fatom, &farity)
			if err != nil {
				return err
			}
			if !ok {
				return errors.New("functor")
			}
			pc = pc[2:]
			args = &Variable{}
			ok, err = PUniv(&arg, Cons(&fatom, args))
			if err != nil {
				return err
			}
			if !ok {
				return errors.New("functor")
			}
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
			x := xr[pc[1]]
			pc = pc[2:]
			_, err := e.arrive(x.String(), astack, append(cont, continuation{pc: pc, xr: xr, vars: vars}))
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
	name     string
	xrTable  []Term
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
		c.name = fmt.Sprintf("%s/0", head)
	case *Compound:
		c.name = fmt.Sprintf("%s/%d", head.Functor, len(head.Args))
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
	c.bytecode = append(c.bytecode, Call, c.xrOffset(&Compound{
		Functor: "/",
		Args:    []Term{p.Functor, Integer(len(p.Args))},
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
		c.bytecode = append(c.bytecode, Functor, c.xrOffset(&Compound{
			Functor: "/",
			Args:    []Term{a.Functor, Integer(len(a.Args))},
		}))
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

type continuation struct {
	pc   bytecode
	xr   []Term
	vars []*Variable
}
