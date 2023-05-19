package engine

import (
	"context"
	"errors"
)

// 7.4.3 says "If no clauses are defined for a procedure indicated by a directive ... then the procedure shall exist but have no clauses."
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
			return vm.exec(c.bytecode, vars, k, args, nil, env, p)
		}
	}
	p = Delay(ks...)
	return p
}

func compile(module Atom, t Term, env *Env) (clauses, error) {
	t = env.Resolve(t)
	if t, ok := t.(Compound); ok && t.Functor() == atomIf && t.Arity() == 2 {
		var cs clauses
		head, body := t.Arg(0), t.Arg(1)
		iter := altIterator{Alt: body, Env: env}
		for iter.Next() {
			c, err := compileClause(module, head, iter.Current(), env)
			if err != nil {
				return nil, typeError(validTypeCallable, body, env)
			}
			c.raw = t
			cs = append(cs, c)
		}
		return cs, nil
	}

	c, err := compileClause(module, t, nil, env)
	c.raw = env.simplify(t)
	return []clause{c}, err
}

type clause struct {
	pi       procedureIndicator
	raw      Term
	vars     []Variable
	bytecode bytecode
}

func compileClause(module Atom, head Term, body Term, env *Env) (clause, error) {
	var c clause
	c.compileHead(head, env)
	if body != nil {
		if err := c.compileBody(module, body, env); err != nil {
			return c, typeError(validTypeCallable, body, env)
		}
	}
	c.bytecode = append(c.bytecode, instruction{opcode: opExit})
	return c, nil
}

func (c *clause) compileHead(head Term, env *Env) {
	switch head := env.Resolve(head).(type) {
	case Atom:
		c.pi = procedureIndicator{name: head, arity: 0}
	case Compound:
		c.pi = procedureIndicator{name: head.Functor(), arity: Integer(head.Arity())}
		for i := 0; i < head.Arity(); i++ {
			c.compileHeadArg(head.Arg(i), env)
		}
	}
}

func (c *clause) compileBody(module Atom, body Term, env *Env) error {
	c.bytecode = append(c.bytecode, instruction{opcode: opEnter})
	iter := seqIterator{Seq: body, Env: env}
	for iter.Next() {
		if err := c.compilePred(module, iter.Current(), env); err != nil {
			return err
		}
	}
	return nil
}

var errNotCallable = errors.New("not callable")

func (c *clause) compilePred(module Atom, p Term, env *Env) error {
	switch p := env.Resolve(p).(type) {
	case Variable:
		return c.compilePred(module, atomCall.Apply(p), env)
	case Atom:
		switch p {
		case atomCut:
			c.bytecode = append(c.bytecode, instruction{opcode: opCut})
			return nil
		}
		pi := procedureIndicator{module: module, name: p, arity: 0}
		c.bytecode = append(c.bytecode, instruction{opcode: opCall, operand: pi})
		return nil
	case Compound:
		if p.Functor() == atomColon && p.Arity() == 2 {
			switch m := env.Resolve(p.Arg(0)).(type) {
			case Variable:
				return c.compilePred(module, atomCall.Apply(p), env)
			case Atom:
				return c.compilePred(m, p.Arg(1), env)
			default:
				break
			}
		}
		for i := 0; i < p.Arity(); i++ {
			c.compileBodyArg(p.Arg(i), env)
		}
		pi := procedureIndicator{module: module, name: p.Functor(), arity: Integer(p.Arity())}
		c.bytecode = append(c.bytecode, instruction{opcode: opCall, operand: pi})
		return nil
	default:
		return errNotCallable
	}
}

func (c *clause) compileHeadArg(a Term, env *Env) {
	switch a := env.Resolve(a).(type) {
	case Variable:
		c.bytecode = append(c.bytecode, instruction{opcode: opGetVar, operand: c.varOffset(a)})
	case charList, codeList: // Treat them as if they're atomic.
		c.bytecode = append(c.bytecode, instruction{opcode: opGetConst, operand: a})
	case list:
		c.bytecode = append(c.bytecode, instruction{opcode: opGetList, operand: Integer(len(a))})
		for _, arg := range a {
			c.compileHeadArg(arg, env)
		}
		c.bytecode = append(c.bytecode, instruction{opcode: opPop})
	case *partial:
		prefix := a.Compound.(list)
		c.bytecode = append(c.bytecode, instruction{opcode: opGetPartial, operand: Integer(len(prefix))})
		c.compileHeadArg(*a.tail, env)
		for _, arg := range prefix {
			c.compileHeadArg(arg, env)
		}
		c.bytecode = append(c.bytecode, instruction{opcode: opPop})
	case Compound:
		c.bytecode = append(c.bytecode, instruction{opcode: opGetFunctor, operand: procedureIndicator{name: a.Functor(), arity: Integer(a.Arity())}})
		for i := 0; i < a.Arity(); i++ {
			c.compileHeadArg(a.Arg(i), env)
		}
		c.bytecode = append(c.bytecode, instruction{opcode: opPop})
	default:
		c.bytecode = append(c.bytecode, instruction{opcode: opGetConst, operand: a})
	}
}

func (c *clause) compileBodyArg(a Term, env *Env) {
	switch a := env.Resolve(a).(type) {
	case Variable:
		c.bytecode = append(c.bytecode, instruction{opcode: opPutVar, operand: c.varOffset(a)})
	case charList, codeList: // Treat them as if they're atomic.
		c.bytecode = append(c.bytecode, instruction{opcode: opPutConst, operand: a})
	case list:
		c.bytecode = append(c.bytecode, instruction{opcode: opPutList, operand: Integer(len(a))})
		for _, arg := range a {
			c.compileBodyArg(arg, env)
		}
		c.bytecode = append(c.bytecode, instruction{opcode: opPop})
	case *partial:
		var l int
		iter := ListIterator{List: a.Compound}
		for iter.Next() {
			l++
		}
		c.bytecode = append(c.bytecode, instruction{opcode: opPutPartial, operand: Integer(l)})
		c.compileBodyArg(*a.tail, env)
		iter = ListIterator{List: a.Compound}
		for iter.Next() {
			c.compileBodyArg(iter.Current(), env)
		}
		c.bytecode = append(c.bytecode, instruction{opcode: opPop})
	case Compound:
		c.bytecode = append(c.bytecode, instruction{opcode: opPutFunctor, operand: procedureIndicator{name: a.Functor(), arity: Integer(a.Arity())}})
		for i := 0; i < a.Arity(); i++ {
			c.compileBodyArg(a.Arg(i), env)
		}
		c.bytecode = append(c.bytecode, instruction{opcode: opPop})
	default:
		c.bytecode = append(c.bytecode, instruction{opcode: opPutConst, operand: a})
	}
}

func (c *clause) varOffset(o Variable) Integer {
	for i, v := range c.vars {
		if v == o {
			return Integer(i)
		}
	}
	c.vars = append(c.vars, o)
	return Integer(len(c.vars) - 1)
}
