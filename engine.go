package prolog

import (
	"errors"
	"fmt"

	"github.com/sirupsen/logrus"
)

type Engine struct {
	procedures map[PrincipalFunctor][]Clause
}

func (e *Engine) Compile(s string) error {
	p := NewParser(s)
	ts, err := p.Clauses()
	if err != nil {
		return err
	}
	for _, t := range ts {
		c, err := NewClause(t)
		if err != nil {
			return err
		}
		if e.procedures == nil {
			e.procedures = map[PrincipalFunctor][]Clause{}
		}
		e.procedures[c.PrincipalFunctor] = append(e.procedures[c.PrincipalFunctor], *c)
	}
	return nil
}

func (e *Engine) Query(s string) ([]*Variable, error) {
	p := NewParser(s)
	t, err := p.Clause()
	if err != nil {
		return nil, err
	}
	c, err := NewClause(&Compound{
		Functor: ":-",
		Args: []Term{
			Atom("user"),
			t,
		},
	})
	if err != nil {
		return nil, err
	}
	if e.procedures == nil {
		e.procedures = map[PrincipalFunctor][]Clause{}
	}
	e.procedures[c.PrincipalFunctor] = []Clause{*c}
	vars, err := e.arrive(c.PrincipalFunctor, List(), nil)
	if err != nil {
		return nil, err
	}
	for i, v := range vars {
		vars[i].Ref = v.Ref.Simplify()
	}
	return vars, nil
}

func (e *Engine) arrive(pf PrincipalFunctor, args Term, cont []continuation) ([]*Variable, error) {
	cs := e.procedures[pf]
	for _, c := range cs {
		vars := make([]*Variable, len(c.Vars))
		for i, n := range c.Vars {
			vars[i] = &Variable{Name: n}
		}
		if err := e.exec(c.Bytecode, c.XRTable, vars, cont, args); err != nil {
			logrus.WithField("err", err).Debug("failed")
			continue
		}
		return vars, nil
	}
	return nil, errors.New("failed")
}

func (e *Engine) exec(pc Bytecode, xr []xrRecord, vars []*Variable, cont []continuation, args Term) error {
	astack := Term(Atom("[]"))
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
			arest := &Variable{Name: "Arest"}
			if !args.Unify(Cons(x, arest)) {
				return errors.New("const")
			}
			pc = pc[2:]
			args = arest
		case Var:
			log.Debug("var")
			v := vars[pc[1]]
			arest := &Variable{Name: "Arest"}
			if !args.Unify(Cons(v, arest)) {
				return errors.New("var")
			}
			pc = pc[2:]
			args = arest
		case Functor:
			log.Debug("functor")
			x, ok := xr[pc[1]].(*PrincipalFunctor)
			if !ok {
				return errors.New("not a principal functor")
			}
			arg := &Variable{Name: "Arg"}
			arest := &Variable{Name: "Arest"}
			if !args.Unify(Cons(arg, arest)) {
				return errors.New("functor")
			}
			c := Compound{
				Functor: x.Functor,
				Args:    make([]Term, x.Arity),
			}
			for i := 0; i < x.Arity; i++ {
				c.Args[i] = &Variable{Name: fmt.Sprintf("Arg%d", i)}
			}
			if !c.Unify(arg) {
				return errors.New("not a functor")
			}
			pc = pc[2:]
			args = List(c.Args...)
			astack = Cons(arest, astack)
		case Pop:
			log.Debug("pop")
			if !args.Unify(Atom("[]")) {
				return errors.New("args is not empty")
			}
			pc = pc[1:]
			a := &Variable{Name: "Args"}
			arest := &Variable{Name: "Astack"}
			if !astack.Unify(Cons(a, arest)) {
				return errors.New("pop")
			}
			args = a
			astack = arest
		case Enter:
			log.Debug("enter")
			if !args.Unify(Atom("[]")) {
				return errors.New("args is not empty")
			}
			if !astack.Unify(Atom("[]")) {
				return errors.New("astack is not empty")
			}
			pc = pc[1:]
			v := Variable{Name: "Args"}
			args = &v
			astack = &v
		case Call:
			log.Debug("call")
			x := xr[pc[1]].(*PrincipalFunctor)
			pc = pc[2:]
			_, err := e.arrive(*x, astack, append(cont, continuation{
				pc:   pc,
				xr:   xr,
				vars: vars,
			}))
			return err
		case Exit:
			log.Debug("exit")
			if len(cont) == 0 {
				return nil
			}
			var c continuation
			c, cont = cont[len(cont)-1], cont[:len(cont)-1]
			pc = c.pc
			xr = c.xr
			vars = c.vars
		default:
			return fmt.Errorf("unknown(%d)", pc[0])
		}
	}
	return nil
}

type xrRecord interface {
	Equal(xrRecord) bool
}

type PrincipalFunctor struct {
	Functor Atom
	Arity   int
}

func (p *PrincipalFunctor) String() string {
	return fmt.Sprintf("%s/%d", p.Functor, p.Arity)
}

func (p *PrincipalFunctor) Equal(r xrRecord) bool {
	v, ok := r.(*PrincipalFunctor)
	return ok && p.Functor == v.Functor && p.Arity == v.Arity
}

type continuation struct {
	pc   Bytecode
	xr   []xrRecord
	vars []*Variable
}
