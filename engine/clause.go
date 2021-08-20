package engine

import (
	"fmt"

	"github.com/ichiban/prolog/nondet"
	"github.com/ichiban/prolog/term"
)

type clauses []clause

func (cs clauses) Call(vm *VM, args term.Interface, k func(term.Env) *nondet.Promise, env *term.Env) *nondet.Promise {
	if len(cs) == 0 {
		return nondet.Bool(false)
	}

	if vm.OnCall == nil {
		vm.OnCall = func(pi string, args term.Interface, env term.Env) {}
	}
	if vm.OnExit == nil {
		vm.OnExit = func(pi string, args term.Interface, env term.Env) {}
	}
	if vm.OnFail == nil {
		vm.OnFail = func(pi string, args term.Interface, env term.Env) {}
	}
	if vm.OnRedo == nil {
		vm.OnRedo = func(pi string, args term.Interface, env term.Env) {}
	}

	var p *nondet.Promise
	ks := make([]func() *nondet.Promise, len(cs))
	for i := range cs {
		i, c := i, cs[i]
		ks[i] = func() *nondet.Promise {
			if i == 0 {
				vm.OnCall(c.pi.String(), args, *env)
			} else {
				vm.OnRedo(c.pi.String(), args, *env)
			}
			vars := make([]term.Variable, len(c.vars))
			for i := range c.vars {
				vars[i] = term.NewVariable()
			}
			return nondet.Delay(func() *nondet.Promise {
				env := *env
				return vm.exec(registers{
					pc:     c.bytecode,
					xr:     c.xrTable,
					pi:     c.piTable,
					vars:   vars,
					args:   args,
					astack: term.List(),
					exit: func(env term.Env) *nondet.Promise {
						vm.OnExit(c.pi.String(), args, env)
						return k(env)
					},
					fail: func(env term.Env) *nondet.Promise {
						vm.OnFail(c.pi.String(), args, env)
						return nondet.Bool(false)
					},
					env:       &env,
					cutParent: p,
				})
			})
		}
	}
	p = nondet.Delay(ks...)
	return p
}

type clause struct {
	pi       procedureIndicator
	raw      term.Interface
	xrTable  []term.Interface
	piTable  []procedureIndicator
	vars     []term.Variable
	bytecode bytecode
}

func (c *clause) compile(t term.Interface, env term.Env) error {
	t = env.Resolve(t)
	c.raw = t
	switch t := t.(type) {
	case term.Variable:
		return instantiationError(t)
	case term.Atom:
		return c.compileClause(t, nil, env)
	case *term.Compound:
		if t.Functor == ":-" {
			return c.compileClause(t.Args[0], t.Args[1], env)
		}
		return c.compileClause(t, nil, env)
	default:
		return typeErrorCallable(t)
	}
}

func (c *clause) compileClause(head term.Interface, body term.Interface, env term.Env) error {
	switch head := env.Resolve(head).(type) {
	case term.Variable:
		return instantiationError(head)
	case term.Atom:
	case *term.Compound:
		for _, a := range head.Args {
			if err := c.compileArg(a); err != nil {
				return err
			}
		}
	default:
		return typeErrorCallable(head)
	}
	if body != nil {
		c.bytecode = append(c.bytecode, instruction{opcode: opEnter})
		for {
			p, ok := body.(*term.Compound)
			if !ok || p.Functor != "," || len(p.Args) != 2 {
				break
			}
			if err := c.compilePred(p.Args[0], env); err != nil {
				return err
			}
			body = p.Args[1]
		}
		if err := c.compilePred(body, env); err != nil {
			return err
		}
	}
	c.bytecode = append(c.bytecode, instruction{opcode: opExit})
	return nil
}

func (c *clause) compilePred(p term.Interface, env term.Env) error {
	switch p := env.Resolve(p).(type) {
	case term.Variable:
		return instantiationError(p)
	case term.Atom:
		if p == "!" {
			c.bytecode = append(c.bytecode, instruction{opcode: opCut})
			return nil
		}
		c.bytecode = append(c.bytecode, instruction{opcode: opCall, operand: c.piOffset(procedureIndicator{name: p, arity: 0})})
		return nil
	case *term.Compound:
		for _, a := range p.Args {
			if err := c.compileArg(a); err != nil {
				return err
			}
		}
		c.bytecode = append(c.bytecode, instruction{opcode: opCall, operand: c.piOffset(procedureIndicator{name: p.Functor, arity: term.Integer(len(p.Args))})})
		return nil
	default:
		return typeErrorCallable(p)
	}
}

func (c *clause) compileArg(a term.Interface) error {
	switch a := a.(type) {
	case term.Variable:
		c.bytecode = append(c.bytecode, instruction{opcode: opVar, operand: c.varOffset(a)})
	case term.Float, term.Integer, term.Atom:
		c.bytecode = append(c.bytecode, instruction{opcode: opConst, operand: c.xrOffset(a)})
	case *term.Compound:
		c.bytecode = append(c.bytecode, instruction{opcode: opFunctor, operand: c.piOffset(procedureIndicator{name: a.Functor, arity: term.Integer(len(a.Args))})})
		for _, n := range a.Args {
			if err := c.compileArg(n); err != nil {
				return err
			}
		}
		c.bytecode = append(c.bytecode, instruction{opcode: opPop})
	default:
		return systemError(fmt.Errorf("unknown argument: %s", a))
	}
	return nil
}

func (c *clause) xrOffset(o term.Interface) byte {
	for i, r := range c.xrTable {
		if r.Unify(o, false, nil) {
			return byte(i)
		}
	}
	c.xrTable = append(c.xrTable, o)
	return byte(len(c.xrTable) - 1)
}

func (c *clause) varOffset(o term.Variable) byte {
	for i, v := range c.vars {
		if v == o {
			return byte(i)
		}
	}
	c.vars = append(c.vars, o)
	return byte(len(c.vars) - 1)
}

func (c *clause) piOffset(o procedureIndicator) byte {
	for i, r := range c.piTable {
		if r == o {
			return byte(i)
		}
	}
	c.piTable = append(c.piTable, o)
	return byte(len(c.piTable) - 1)
}
