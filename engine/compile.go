package engine

import (
	"context"
	"errors"
	"fmt"
	"strings"
)

func (vm *VM) Compile(ctx context.Context, text string) (Atom, error) {
	{
		m := vm.typeIn
		defer vm.SetModule(m)
	}

	// Skip a shebang line if exists.
	if strings.HasPrefix(text, "#!") {
		switch i := strings.IndexRune(text, '\n'); i {
		case -1:
			text = ""
		default:
			text = text[i+1:]
		}
	}

	r := strings.NewReader(text)
	p := NewParser(vm.TypeInModule, r)
	for p.More() {
		p.Vars = p.Vars[:]
		t, err := p.Term()
		if err != nil {
			return 0, err
		}

		et, err := expand(vm, t, nil)
		if err != nil {
			return 0, err
		}

		pi, arg, err := piArg(et, nil)
		if err != nil {
			return 0, err
		}
		switch pi {
		case predicateIndicator{name: atomColonMinus, arity: 1}: // Directive
			if err := vm.TypeInModule().flushClauseBuf(); err != nil {
				return 0, err
			}

			ok, err := Call(vm, arg(0), Success, nil).Force(ctx)
			if err != nil {
				return 0, err
			}
			if !ok {
				return 0, p.unexpected()
			}

			continue
		case predicateIndicator{name: atomColonMinus, arity: 2}: // Rule
			pi, arg, err = piArg(arg(0), nil)
			if err != nil {
				return 0, err
			}

			fallthrough
		default:
			m := vm.TypeInModule()
			if len(m.buf) > 0 && pi != m.buf[0].pi {
				if err := m.flushClauseBuf(); err != nil {
					return 0, err
				}
			}

			cs, err := vm.compileTerm(ctx, et, nil)
			if err != nil {
				return 0, err
			}

			m.buf = append(m.buf, cs...)
		}
	}

	m := vm.TypeInModule()
	if err := m.flushClauseBuf(); err != nil {
		return 0, err
	}

	for _, g := range m.initGoals {
		ok, err := Call(vm, g, Success, nil).Force(ctx)
		if err != nil {
			return 0, err
		}
		if !ok {
			var sb strings.Builder
			s := NewOutputTextStream(&sb)
			_, _ = WriteTerm(vm, s, g, List(atomQuoted.Apply(atomTrue)), Success, nil).Force(ctx)
			return 0, fmt.Errorf("failed initialization goal: %s", sb.String())
		}
	}
	m.initGoals = m.initGoals[:0]

	return vm.typeIn, nil
}

type clause struct {
	pi       predicateIndicator
	raw      Term
	vars     []Variable
	bytecode bytecode
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
			return vm.exec(c.bytecode, vars, k, args, nil, env, p)
		}
	}
	p = Delay(ks...)
	return p
}

func (vm *VM) compileTerm(ctx context.Context, t Term, env *Env) (clauses, error) {
	t = env.Resolve(t)
	if t, ok := t.(Compound); ok && t.Functor() == atomColonMinus && t.Arity() == 2 {
		var cs clauses
		head, body := t.Arg(0), t.Arg(1)
		iter := altIterator{Alt: body, Env: env}
		for iter.Next() {
			c, err := vm.compileClause(ctx, head, iter.Current(), env)
			if err != nil {
				return nil, typeError(validTypeCallable, body, env)
			}
			c.raw = t
			cs = append(cs, c)
		}
		return cs, nil
	}

	c, err := vm.compileClause(ctx, t, nil, env)
	c.raw = env.simplify(t)
	return []clause{c}, err
}

func (vm *VM) compileClause(ctx context.Context, head Term, body Term, env *Env) (clause, error) {
	var c clause
	vm.compileHead(&c, head, env)
	if body != nil {
		if err := vm.compileBody(ctx, &c, body, env); err != nil {
			return c, typeError(validTypeCallable, body, env)
		}
	}
	c.bytecode = append(c.bytecode, instruction{opcode: opExit})
	return c, nil
}

func (vm *VM) compileHead(c *clause, head Term, env *Env) {
	switch head := env.Resolve(head).(type) {
	case Atom:
		c.pi = predicateIndicator{name: head, arity: 0}
	case Compound:
		c.pi = predicateIndicator{name: head.Functor(), arity: Integer(head.Arity())}
		for i := 0; i < head.Arity(); i++ {
			c.compileHeadArg(head.Arg(i), env)
		}
	}
}

func (vm *VM) compileBody(ctx context.Context, c *clause, body Term, env *Env) error {
	c.bytecode = append(c.bytecode, instruction{opcode: opEnter})
	iter := seqIterator{Seq: body, Env: env}
	for iter.Next() {
		if err := vm.compileGoal(ctx, c, iter.Current(), env); err != nil {
			return err
		}
	}
	return nil
}

var errNotCallable = errors.New("not callable")

func (vm *VM) compileGoal(ctx context.Context, c *clause, goal Term, env *Env) error {
	module, goal := qmut(vm.typeIn, goal, env)
	switch g := env.Resolve(goal).(type) {
	case Variable:
		return vm.compileGoal(ctx, c, atomCall.Apply(g), env)
	case Atom:
		switch g {
		case atomCut:
			c.bytecode = append(c.bytecode, instruction{opcode: opCut})
			return nil
		}
		c.bytecode = append(c.bytecode, instruction{opcode: opCall, operand: qualifiedPredicateIndicator{
			module:             module,
			predicateIndicator: predicateIndicator{name: g, arity: 0},
		}})
		return nil
	case Compound:
		m := vm.modules[module]
		pi := predicateIndicator{name: g.Functor(), arity: Integer(g.Arity())}
		e := m.procedures[pi]
		for i := 0; i < g.Arity(); i++ {
			arg := g.Arg(i)
			if e.metaPredicate != nil && e.metaPredicate[i].needsModuleNameExpansion() {
				arg = atomColon.Apply(vm.typeIn, arg)
			}
			c.compileBodyArg(arg, env)
		}
		c.bytecode = append(c.bytecode, instruction{opcode: opCall, operand: qualifiedPredicateIndicator{
			module:             module,
			predicateIndicator: pi,
		}})
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
		c.bytecode = append(c.bytecode, instruction{opcode: opGetFunctor, operand: predicateIndicator{name: a.Functor(), arity: Integer(a.Arity())}})
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
		c.bytecode = append(c.bytecode, instruction{opcode: opPutFunctor, operand: predicateIndicator{name: a.Functor(), arity: Integer(a.Arity())}})
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
