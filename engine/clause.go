package engine

import (
	"context"
	"errors"
	"fmt"

	"github.com/ichiban/prolog/nondet"
	"github.com/ichiban/prolog/term"
)

type clauses []clause

func (cs clauses) Call(vm *VM, args []term.Interface, k func(*term.Env) *nondet.Promise, env *term.Env) *nondet.Promise {
	if len(cs) == 0 {
		return nondet.Bool(false)
	}

	if vm.OnCall == nil {
		vm.OnCall = func(pi ProcedureIndicator, args []term.Interface, env *term.Env) {}
	}
	if vm.OnExit == nil {
		vm.OnExit = func(pi ProcedureIndicator, args []term.Interface, env *term.Env) {}
	}
	if vm.OnFail == nil {
		vm.OnFail = func(pi ProcedureIndicator, args []term.Interface, env *term.Env) {}
	}
	if vm.OnRedo == nil {
		vm.OnRedo = func(pi ProcedureIndicator, args []term.Interface, env *term.Env) {}
	}

	var p *nondet.Promise
	ks := make([]func(context.Context) *nondet.Promise, len(cs))
	for i := range cs {
		i, c := i, cs[i]
		ks[i] = func(context.Context) *nondet.Promise {
			if i == 0 {
				vm.OnCall(c.pi, args, env)
			} else {
				vm.OnRedo(c.pi, args, env)
			}
			vars := make([]term.Variable, len(c.vars))
			for i := range vars {
				vars[i] = term.NewVariable()
			}
			return nondet.Delay(func(context.Context) *nondet.Promise {
				env := env
				return vm.exec(registers{
					pc:   c.bytecode,
					xr:   c.xrTable,
					vars: vars,
					cont: func(env *term.Env) *nondet.Promise {
						vm.OnExit(c.pi, args, env)
						return k(env)
					},
					args:      term.List(args...),
					astack:    term.List(),
					pi:        c.piTable,
					env:       env,
					cutParent: p,
				})
			}, func(context.Context) *nondet.Promise {
				env := env
				vm.OnFail(c.pi, args, env)
				return nondet.Bool(false)
			})
		}
	}
	p = nondet.Delay(ks...)
	return p
}

type clause struct {
	pi       ProcedureIndicator
	raw      term.Interface
	xrTable  []term.Interface
	piTable  []ProcedureIndicator
	vars     []term.Variable
	bytecode bytecode
}

func compile(t term.Interface, env *term.Env) (clauses, error) {
	t = env.Simplify(t)
	switch t := t.(type) {
	case term.Variable:
		return nil, InstantiationError(t)
	case term.Atom:
		c, err := compileClause(t, nil, env)
		if err != nil {
			return nil, err
		}
		c.raw = t
		return []clause{c}, nil
	case *term.Compound:
		if t.Functor == ":-" {
			var cs []clause
			head, body := env.Resolve(t.Args[0]), env.Resolve(t.Args[1])
			exp := body
			for {
				e, ok := exp.(*term.Compound)
				if !ok {
					break
				}

				if e.Functor != ";" || len(e.Args) != 2 {
					break
				}

				// if-then-else construct
				if if_, ok := e.Args[0].(*term.Compound); ok && if_.Functor == "->" && len(if_.Args) == 2 {
					break
				}

				c, err := compileClause(head, e.Args[0], env)
				switch err {
				case nil:
					break
				case errNotCallable:
					return nil, typeErrorCallable(body)
				default:
					return nil, err
				}
				c.raw = t
				cs = append(cs, c)

				exp = env.Resolve(e.Args[1])
			}

			c, err := compileClause(head, exp, env)
			switch err {
			case nil:
				break
			case errNotCallable:
				return nil, typeErrorCallable(body)
			default:
				return nil, err
			}
			c.raw = t
			cs = append(cs, c)

			return cs, nil
		}
		c, err := compileClause(t, nil, env)
		switch err {
		case nil:
			break
		case errNotCallable:
			return nil, typeErrorCallable(t)
		default:
			return nil, err
		}
		c.raw = t
		return []clause{c}, nil
	default:
		return nil, typeErrorCallable(t)
	}
}

func compileClause(head term.Interface, body term.Interface, env *term.Env) (clause, error) {
	var c clause
	switch head := env.Resolve(head).(type) {
	case term.Variable:
		return c, InstantiationError(head)
	case term.Atom:
		c.pi = ProcedureIndicator{Name: head, Arity: 0}
	case *term.Compound:
		c.pi = ProcedureIndicator{Name: head.Functor, Arity: term.Integer(len(head.Args))}
		for _, a := range head.Args {
			if err := c.compileArg(a, env); err != nil {
				return c, err
			}
		}
	default:
		return c, errNotCallable
	}
	if body != nil {
		err := c.compileBody(body, env)
		switch err {
		case nil:
			break
		case errNotCallable:
			return c, errNotCallable
		default:
			return c, err
		}
	}
	c.bytecode = append(c.bytecode, instruction{opcode: opExit})
	return c, nil
}

func (c *clause) compileBody(body term.Interface, env *term.Env) error {
	c.bytecode = append(c.bytecode, instruction{opcode: opEnter})
	for {
		p, ok := env.Resolve(body).(*term.Compound)
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
	return nil
}

var errNotCallable = errors.New("not callable")

func (c *clause) compilePred(p term.Interface, env *term.Env) error {
	switch p := env.Resolve(p).(type) {
	case term.Variable:
		return c.compilePred(&term.Compound{
			Functor: "call",
			Args:    []term.Interface{p},
		}, env)
	case term.Atom:
		switch p {
		case "!":
			c.bytecode = append(c.bytecode, instruction{opcode: opCut})
			return nil
		}
		c.bytecode = append(c.bytecode, instruction{opcode: opCall, operand: c.piOffset(ProcedureIndicator{Name: p, Arity: 0})})
		return nil
	case *term.Compound:
		for _, a := range p.Args {
			if err := c.compileArg(a, env); err != nil {
				return err
			}
		}
		c.bytecode = append(c.bytecode, instruction{opcode: opCall, operand: c.piOffset(ProcedureIndicator{Name: p.Functor, Arity: term.Integer(len(p.Args))})})
		return nil
	default:
		return errNotCallable
	}
}

func (c *clause) compileArg(a term.Interface, env *term.Env) error {
	switch a := a.(type) {
	case term.Variable:
		c.bytecode = append(c.bytecode, instruction{opcode: opVar, operand: c.varOffset(a)})
	case term.Float, term.Integer, term.Atom, *term.Stream:
		c.bytecode = append(c.bytecode, instruction{opcode: opConst, operand: c.xrOffset(a)})
	case *term.Compound:
		c.bytecode = append(c.bytecode, instruction{opcode: opFunctor, operand: c.piOffset(ProcedureIndicator{Name: a.Functor, Arity: term.Integer(len(a.Args))})})
		for _, n := range a.Args {
			if err := c.compileArg(n, env); err != nil {
				return err
			}
		}
		c.bytecode = append(c.bytecode, instruction{opcode: opPop})
	default:
		return SystemError(fmt.Errorf("unknown argument: %s", a))
	}
	return nil
}

func (c *clause) xrOffset(o term.Interface) byte {
	for i, r := range c.xrTable {
		if _, ok := r.Unify(o, false, nil); ok {
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

func (c *clause) piOffset(o ProcedureIndicator) byte {
	for i, r := range c.piTable {
		if r == o {
			return byte(i)
		}
	}
	c.piTable = append(c.piTable, o)
	return byte(len(c.piTable) - 1)
}
