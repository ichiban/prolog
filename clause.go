package prolog

import "errors"

type Clause struct {
	PrincipalFunctor
	XRTable  []xrRecord
	Vars     []string
	Bytecode Bytecode
}

func NewClause(t Term) (*Clause, error) {
	var c Clause
	if err := c.compile(t); err != nil {
		return nil, err
	}
	return &c, nil
}

func (c *Clause) compile(t Term) error {
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

func (c *Clause) compileClause(head Term, body []Term) error {
	switch head := head.(type) {
	case Atom:
		c.PrincipalFunctor = *head.PrincipalFunctor()
	case *Compound:
		c.PrincipalFunctor = *head.PrincipalFunctor()
		for _, a := range head.Args {
			if err := c.compileArg(a); err != nil {
				return err
			}
		}
	default:
		return errors.New("not an atom nor compound")
	}
	if len(body) != 0 {
		c.Bytecode = append(c.Bytecode, Enter)
	}
	for _, b := range body {
		if err := c.compilePred(b.(*Compound)); err != nil {
			return err
		}
	}
	c.Bytecode = append(c.Bytecode, Exit)
	return nil
}

func (c *Clause) compilePred(p *Compound) error {
	for _, a := range p.Args {
		if err := c.compileArg(a); err != nil {
			return err
		}
	}
	c.Bytecode = append(c.Bytecode, Call, c.xrOffset(&PrincipalFunctor{
		Functor: p.Functor,
		Arity:   len(p.Args),
	}))
	return nil
}

func (c *Clause) compileArg(a Term) error {
	switch a := a.(type) {
	case Atom:
		c.Bytecode = append(c.Bytecode, Const, c.xrOffset(a))
		return nil
	case Integer:
		c.Bytecode = append(c.Bytecode, Const, c.xrOffset(a))
		return nil
	case *Variable:
		c.Bytecode = append(c.Bytecode, Var, c.varOffset(a))
		return nil
	case *Compound:
		c.Bytecode = append(c.Bytecode, Functor, c.xrOffset(&PrincipalFunctor{
			Functor: a.Functor,
			Arity:   len(a.Args),
		}))
		for _, n := range a.Args {
			if err := c.compileArg(n); err != nil {
				return err
			}
		}
		c.Bytecode = append(c.Bytecode, Pop)
		return nil
	default:
		return errors.New("unknown")
	}
}

func (c *Clause) xrOffset(o xrRecord) byte {
	for i, r := range c.XRTable {
		if r.Equal(o) {
			return byte(i)
		}
	}
	c.XRTable = append(c.XRTable, o)
	return byte(len(c.XRTable) - 1)
}

func (c *Clause) varOffset(o *Variable) byte {
	for i, v := range c.Vars {
		if v == o.Name {
			return byte(i)
		}
	}
	c.Vars = append(c.Vars, o.Name)
	return byte(len(c.Vars) - 1)
}
