package engine

import (
	"context"
	"errors"
)

type userDefined struct {
	public        bool
	dynamic       bool
	multifile     bool
	discontiguous bool

	// 7.4.3 says "If no clauses are defined for a procedure indicated by a directive ... then the procedure shall exist but have no clauses."
	clauses
}

type clauses []clause

func (cs clauses) call(vm *VM, args []Term, k Cont, env *Env) *Promise {
	var p *Promise
	ks := make([]func(context.Context) *Promise, len(cs))
	for i := range cs {
		i, c := i, cs[i]
		ks[i] = func(context.Context) *Promise {
			vars := make([]Variable, len(c.vars))
			for i := range vars {
				vars[i] = NewVariable()
			}
			return vm.exec(registers{
				pc:        c.bytecode,
				xr:        c.xrTable,
				vars:      vars,
				cont:      k,
				args:      List(args...),
				astack:    List(),
				env:       env,
				cutParent: p,
			})
		}
	}
	p = Delay(ks...)
	return p
}

func compile(t Term, env *Env) (clauses, error) {
	t = env.Resolve(t)
	if t, ok := t.(Compound); ok && t.Functor() == atomIf && t.Arity() == 2 {
		var cs clauses
		head, body := t.Arg(0), t.Arg(1)
		iter := altIterator{Alt: body, Env: env}
		for iter.Next() {
			c, err := compileClause(head, iter.Current(), env)
			if err != nil {
				return nil, typeError(validTypeCallable, body, env)
			}
			c.raw = t
			cs = append(cs, c)
		}
		return cs, nil
	}

	c, err := compileClause(t, nil, env)
	c.raw = env.simplify(t)
	return []clause{c}, err
}

type clause struct {
	pi       procedureIndicator
	raw      Term
	xrTable  []Term
	vars     []Variable
	bytecode bytecode
}

func compileClause(head Term, body Term, env *Env) (clause, error) {
	var c clause
	switch head := env.Resolve(head).(type) {
	case Atom:
		c.pi = procedureIndicator{name: head, arity: 0}
	case Compound:
		c.pi = procedureIndicator{name: head.Functor(), arity: Integer(head.Arity())}
		for i := 0; i < head.Arity(); i++ {
			c.compileArg(head.Arg(i), env)
		}
	}
	if body != nil {
		if err := c.compileBody(body, env); err != nil {
			return c, typeError(validTypeCallable, body, env)
		}
	}
	c.bytecode = append(c.bytecode, instruction{opcode: opExit})
	return c, nil
}

func (c *clause) compileBody(body Term, env *Env) error {
	c.bytecode = append(c.bytecode, instruction{opcode: opEnter})
	iter := seqIterator{Seq: body, Env: env}
	for iter.Next() {
		if err := c.compilePred(iter.Current(), env); err != nil {
			return err
		}
	}
	return nil
}

var errNotCallable = errors.New("not callable")

func (c *clause) compilePred(p Term, env *Env) error {
	switch p := env.Resolve(p).(type) {
	case Variable:
		return c.compilePred(atomCall.Apply(p), env)
	case Atom:
		switch p {
		case atomCut:
			c.bytecode = append(c.bytecode, instruction{opcode: opCut})
			return nil
		}
		c.bytecode = append(c.bytecode, instruction{opcode: opCall, operand: c.xrOffset(procedureIndicator{name: p, arity: 0})})
		return nil
	case Compound:
		for i := 0; i < p.Arity(); i++ {
			c.compileArg(p.Arg(i), env)
		}
		c.bytecode = append(c.bytecode, instruction{opcode: opCall, operand: c.xrOffset(procedureIndicator{name: p.Functor(), arity: Integer(p.Arity())})})
		return nil
	default:
		return errNotCallable
	}
}

func (c *clause) compileArg(a Term, env *Env) {
	switch a := env.Resolve(a).(type) {
	case Variable:
		c.bytecode = append(c.bytecode, instruction{opcode: opVar, operand: c.varOffset(a)})
	case charList, codeList: // Treat them as if they're atomic.
		c.bytecode = append(c.bytecode, instruction{opcode: opConst, operand: c.xrOffset(a)})
	case list:
		c.bytecode = append(c.bytecode, instruction{opcode: opList, operand: c.xrOffset(Integer(len(a)))})
		for _, arg := range a {
			c.compileArg(arg, env)
		}
		c.bytecode = append(c.bytecode, instruction{opcode: opPop})
	case *partial:
		prefix := a.vector
		c.bytecode = append(c.bytecode, instruction{opcode: opPartial, operand: c.xrOffset(Integer(prefix.Len()))})
		c.compileArg(*a.tail, env)
		iter := ListIterator{List: prefix, Env: env}
		for iter.Next() {
			c.compileArg(iter.Current(), env)
		}
		c.bytecode = append(c.bytecode, instruction{opcode: opPop})
	case Compound:
		c.bytecode = append(c.bytecode, instruction{opcode: opFunctor, operand: c.xrOffset(procedureIndicator{name: a.Functor(), arity: Integer(a.Arity())})})
		for i := 0; i < a.Arity(); i++ {
			c.compileArg(a.Arg(i), env)
		}
		c.bytecode = append(c.bytecode, instruction{opcode: opPop})
	default:
		c.bytecode = append(c.bytecode, instruction{opcode: opConst, operand: c.xrOffset(a)})
	}
}

func (c *clause) xrOffset(o Term) byte {
	oid := id(o)
	for i, r := range c.xrTable {
		if id(r) == oid {
			return byte(i)
		}
	}
	c.xrTable = append(c.xrTable, o)
	return byte(len(c.xrTable) - 1)
}

func (c *clause) varOffset(o Variable) byte {
	for i, v := range c.vars {
		if v == o {
			return byte(i)
		}
	}
	c.vars = append(c.vars, o)
	return byte(len(c.vars) - 1)
}
