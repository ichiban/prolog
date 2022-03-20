package engine

import (
	"context"
	"errors"
)

type clauses []clause

func (cs clauses) Call(vm *VM, args []Term, k func(*Env) *Promise, env *Env) *Promise {
	if len(cs) == 0 {
		return Bool(false)
	}

	if vm.OnCall == nil {
		vm.OnCall = func(pi ProcedureIndicator, args []Term, env *Env) {}
	}
	if vm.OnExit == nil {
		vm.OnExit = func(pi ProcedureIndicator, args []Term, env *Env) {}
	}
	if vm.OnFail == nil {
		vm.OnFail = func(pi ProcedureIndicator, args []Term, env *Env) {}
	}
	if vm.OnRedo == nil {
		vm.OnRedo = func(pi ProcedureIndicator, args []Term, env *Env) {}
	}

	var p *Promise
	ks := make([]func(context.Context) *Promise, len(cs))
	for i := range cs {
		i, c := i, cs[i]
		ks[i] = func(context.Context) *Promise {
			if i == 0 {
				vm.OnCall(c.pi, args, env)
			} else {
				vm.OnRedo(c.pi, args, env)
			}
			vars := make([]Variable, len(c.vars))
			for i := range vars {
				vars[i] = NewVariable()
			}
			return Delay(func(context.Context) *Promise {
				env := env
				return vm.exec(registers{
					pc:   c.bytecode,
					xr:   c.xrTable,
					vars: vars,
					cont: func(env *Env) *Promise {
						vm.OnExit(c.pi, args, env)
						return k(env)
					},
					args:      List(args...),
					astack:    List(),
					pi:        c.piTable,
					env:       env,
					cutParent: p,
				})
			}, func(context.Context) *Promise {
				env := env
				vm.OnFail(c.pi, args, env)
				return Bool(false)
			})
		}
	}
	p = Delay(ks...)
	return p
}

// some variants of clauses.
// clauses itself is user-defined dynamic.
type (
	builtin struct{ clauses } // builtin static.
	static  struct{ clauses } // user-defined static.
)

type clause struct {
	pi       ProcedureIndicator
	raw      Term
	xrTable  []Term
	piTable  []ProcedureIndicator
	vars     []Variable
	bytecode bytecode
}

func compile(t Term) (clauses, error) {
	if t, ok := t.(*Compound); ok && t.Functor == ":-" {
		var cs []clause
		head, body := t.Args[0], t.Args[1]
		if err := EachAlternative(body, func(elem Term) error {
			c, err := compileClause(head, elem)
			if err != nil {
				return err
			}
			c.raw = t
			cs = append(cs, c)
			return nil
		}, nil); err != nil {
			return nil, TypeErrorCallable(body)
		}
		return cs, nil
	}

	c, err := compileClause(t, nil)
	c.raw = t
	return []clause{c}, err
}

func compileClause(head Term, body Term) (clause, error) {
	var c clause
	switch head := head.(type) {
	case Atom:
		c.pi = ProcedureIndicator{Name: head, Arity: 0}
	case *Compound:
		c.pi = ProcedureIndicator{Name: head.Functor, Arity: Integer(len(head.Args))}
		for _, a := range head.Args {
			c.compileArg(a)
		}
	}
	if body != nil {
		if err := c.compileBody(body); err != nil {
			return c, TypeErrorCallable(body)
		}
	}
	c.bytecode = append(c.bytecode, instruction{opcode: opExit})
	return c, nil
}

func (c *clause) compileBody(body Term) error {
	c.bytecode = append(c.bytecode, instruction{opcode: opEnter})
	return EachSeq(body, ",", func(elem Term) error {
		return c.compilePred(elem)
	}, nil)
}

var errNotCallable = errors.New("not callable")

func (c *clause) compilePred(p Term) error {
	switch p := p.(type) {
	case Variable:
		return c.compilePred(&Compound{
			Functor: "call",
			Args:    []Term{p},
		})
	case Atom:
		switch p {
		case "!":
			c.bytecode = append(c.bytecode, instruction{opcode: opCut})
			return nil
		}
		c.bytecode = append(c.bytecode, instruction{opcode: opCall, operand: c.piOffset(ProcedureIndicator{Name: p, Arity: 0})})
		return nil
	case *Compound:
		for _, a := range p.Args {
			c.compileArg(a)
		}
		c.bytecode = append(c.bytecode, instruction{opcode: opCall, operand: c.piOffset(ProcedureIndicator{Name: p.Functor, Arity: Integer(len(p.Args))})})
		return nil
	default:
		return errNotCallable
	}
}

func (c *clause) compileArg(a Term) {
	switch a := a.(type) {
	case Variable:
		c.bytecode = append(c.bytecode, instruction{opcode: opVar, operand: c.varOffset(a)})
	case *Compound:
		c.bytecode = append(c.bytecode, instruction{opcode: opFunctor, operand: c.piOffset(ProcedureIndicator{Name: a.Functor, Arity: Integer(len(a.Args))})})
		for _, n := range a.Args {
			c.compileArg(n)
		}
		c.bytecode = append(c.bytecode, instruction{opcode: opPop})
	default:
		c.bytecode = append(c.bytecode, instruction{opcode: opConst, operand: c.xrOffset(a)})
	}
}

func (c *clause) xrOffset(o Term) byte {
	for i, r := range c.xrTable {
		if _, ok := r.Unify(o, false, nil); ok {
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

func (c *clause) piOffset(o ProcedureIndicator) byte {
	for i, r := range c.piTable {
		if r == o {
			return byte(i)
		}
	}
	c.piTable = append(c.piTable, o)
	return byte(len(c.piTable) - 1)
}
