package internal

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
	p := NewParser(&vm.Terms, vm.TypeInModule, r)
	for p.More() {
		p.Vars = p.Vars[:]
		t, err := p.Term()
		if err != nil {
			return 0, err
		}

		et, err := expand(ctx, vm, t)
		if err != nil {
			return 0, err
		}

		pi, arg, _ := vm.Terms.Compound(et)
		switch pi {
		case Functor{Name: NewAtom(":-"), Arity: 1}: // Directive
			if err := vm.TypeInModule().flushClauseBuf(&vm.Terms); err != nil {
				return 0, err
			}

			a, err := arg(0)
			if err != nil {
				return 0, err
			}

			ok, err := vm.Call(ctx, a, Success).Force(vm.Stack)
			if err != nil {
				return 0, err
			}
			if !ok {
				return 0, p.unexpected()
			}

			continue
		case Functor{Name: NewAtom(":-"), Arity: 2}: // Rule
			a, err := arg(0)
			if err != nil {
				return 0, err
			}

			pi, arg, _ = vm.Terms.Compound(a)
			fallthrough
		default:
			m := vm.TypeInModule()
			if len(m.buf) > 0 && pi != m.buf[0].pi {
				if err := m.flushClauseBuf(&vm.Terms); err != nil {
					return 0, err
				}
			}

			cs, err := vm.compileTerm(ctx, et)
			if err != nil {
				return 0, err
			}

			m.buf = append(m.buf, cs...)
		}
	}

	m := vm.TypeInModule()
	if err := m.flushClauseBuf(&vm.Terms); err != nil {
		return 0, err
	}

	for _, g := range m.initGoals {
		ok, err := vm.Call(ctx, g, Success).Force(vm.Stack)
		if err != nil {
			return 0, err
		}
		if !ok {
			var sb strings.Builder
			_ = vm.Terms.Write(&sb, g, &WriteOptions{
				quoted: true,
			})
			return 0, fmt.Errorf("failed initialization goal: %s", sb.String())
		}
	}
	m.initGoals = m.initGoals[:0]

	return vm.typeIn, nil
}

func expand(ctx context.Context, vm *VM, term Term) (Term, error) {
	m := vm.TypeInModule()
	termExpansion := NewAtom("term_expansion")
	if _, ok := m.procedures[Functor{Name: termExpansion, Arity: 2}]; !ok {
		return term, nil
	}

	v, err := vm.Terms.PutVariable(NewVariable(&vm.Terms))
	if err != nil {
		return 0, err
	}

	g, err := vm.Terms.PutCompound(termExpansion, term, v)
	if err != nil {
		return 0, err
	}

	ok, err := vm.Call(ctx, g, Success).Force(vm.Stack)
	if err != nil {
		return 0, err
	}
	if !ok {
		return term, nil
	}

	return vm.Terms.RenamedCopy(v)
}

type clause struct {
	pi       Functor
	raw      Term
	vars     []Variable
	bytecode bytecode
}

type clauses []clause

func (cs clauses) call(ctx context.Context, vm *VM, args []Term, k Cont) Promise {
	return Delay(func(yield func(thunk func() Promise)) {
		for _, c := range cs {
			yield(func() Promise {
				vars := make([]Term, len(c.vars))
				for i := range vars {
					v, err := vm.Terms.PutVariable(NewVariable(&vm.Terms))
					if err != nil {
						return Error(err)
					}
					vars[i] = v
				}
				return vm.exec(ctx, c.bytecode, vars, k, args, nil)
			})
		}
	})
}

func (vm *VM) compileTerm(ctx context.Context, t Term) (clauses, error) {
	if f, arg, ok := vm.Terms.Compound(t); ok && f == (Functor{Name: NewAtom(":-"), Arity: 2}) {
		head, err := arg(0)
		if err != nil {
			return nil, err
		}

		body, err := arg(1)
		if err != nil {
			return nil, err
		}

		var cs clauses
		iter := altIterator{TermPool: &vm.Terms, Alt: body}
		for iter.Next() {
			c, err := vm.compileClause(ctx, head, iter.Current())
			if err != nil {
				return nil, &TypeError{Type: NewAtom("callable"), Culprit: body}
			}
			c.raw = t
			cs = append(cs, c)
		}
		return cs, nil
	}

	c, err := vm.compileClause(ctx, t, 0)
	c.raw, err = vm.Terms.RenamedCopy(t)
	if err != nil {
		return nil, err
	}
	return []clause{c}, err
}

func (vm *VM) compileClause(ctx context.Context, head, body Term) (clause, error) {
	var c clause
	if err := vm.compileHead(&c, head); err != nil {
		return c, err
	}
	if body != 0 {
		if err := vm.compileBody(ctx, &c, body); err != nil {
			return c, &TypeError{Type: NewAtom("callable"), Culprit: body}
		}
	}
	c.bytecode = append(c.bytecode, instruction{opcode: opExit})
	return c, nil
}

func (vm *VM) compileHead(c *clause, head Term) error {
	if a, ok := vm.Terms.Atom(head); ok {
		c.pi = Functor{Name: a, Arity: 2}
	}

	f, arg, ok := vm.Terms.Compound(head)
	if !ok {
		return errNotCallable
	}

	c.pi = f
	for i := 0; i < f.Arity; i++ {
		a, err := arg(i)
		if err != nil {
			return err
		}
		if err := vm.compileHeadArg(c, a); err != nil {
			return err
		}
	}
	return nil
}

func (vm *VM) compileBody(ctx context.Context, c *clause, body Term) error {
	c.bytecode = append(c.bytecode, instruction{opcode: opEnter})
	iter := seqIterator{TermPool: &vm.Terms, Seq: body}
	for iter.Next() {
		if err := vm.compileGoal(ctx, c, iter.Current()); err != nil {
			return err
		}
	}
	return nil
}

var errNotCallable = errors.New("not callable")

func (vm *VM) compileGoal(ctx context.Context, c *clause, goal Term) error {
	module, goal := vm.Terms.Unqualify(vm.typeIn, goal)

	if _, ok := vm.Terms.Variable(goal); ok {
		g, err := vm.Terms.PutCompound(NewAtom("call"), goal)
		if err != nil {
			return err
		}
		return vm.compileGoal(ctx, c, g)
	}

	if g, ok := vm.Terms.Atom(goal); ok {
		if g == Atom('!') {
			c.bytecode = append(c.bytecode, instruction{opcode: opCut})
			return nil
		}

		var ok bool
		vm.Terms.functors, ok = cappend(vm.Terms.functors, Functor{
			Module: module,
			Name:   g,
			Arity:  0,
		})
		if !ok {
			return errTooManyFunctors
		}

		c.bytecode = append(c.bytecode, instruction{opcode: opCall, operand: Term(len(vm.Terms.functors) - 1)})
		return nil
	}

	pi, arg, ok := vm.Terms.Compound(goal)
	if !ok {
		return errNotCallable
	}

	var e procedureEntry
	if m, ok := vm.modules[module]; ok {
		e = m.procedures[pi]
	}
	for i := 0; i < pi.Arity; i++ {
		a, err := arg(i)
		if err != nil {
			return err
		}
		if e.metaPredicate != nil && e.metaPredicate[i].needsModuleNameExpansion() {
			m, err := vm.Terms.PutAtom(vm.typeIn)
			if err != nil {
				return err
			}

			a, err = vm.Terms.PutCompound(Atom(':'), m, a)
			if err != nil {
				return err
			}
		}
		if err := vm.compileBodyArg(c, a); err != nil {
			return err
		}
	}

	vm.Terms.functors, ok = cappend(vm.Terms.functors, Functor{
		Module: module,
		Name:   pi.Name,
		Arity:  pi.Arity,
	})
	if !ok {
		return errTooManyFunctors
	}

	c.bytecode = append(c.bytecode, instruction{opcode: opCall, operand: Term(len(vm.Terms.functors) - 1)})
	return nil

}

func (vm *VM) compileHeadArg(c *clause, a Term) error {
	if v, ok := vm.Terms.Variable(a); ok {
		c.bytecode = append(c.bytecode, instruction{opcode: opGetVar, operand: Term(c.varOffset(v))})
		return nil
	}

	if ok := vm.Terms.Atomic(a); ok {
		c.bytecode = append(c.bytecode, instruction{opcode: opGetConst, operand: a})
		return nil
	}

	f, arg, _ := vm.Terms.Compound(a)

	var ok bool
	vm.Terms.functors, ok = cappend(vm.Terms.functors, f)
	if !ok {
		return errTooManyFunctors
	}

	c.bytecode = append(c.bytecode, instruction{opcode: opGetFunctor, operand: Term(len(vm.Terms.functors) - 1)})
	for i := 1; i <= f.Arity; i++ {
		a, err := arg(i)
		if err != nil {
			return err
		}
		if err := vm.compileHeadArg(c, a); err != nil {
			return err
		}
	}
	c.bytecode = append(c.bytecode, instruction{opcode: opPop})
	return nil
}

func (vm *VM) compileBodyArg(c *clause, a Term) error {
	if v, ok := vm.Terms.Variable(a); ok {
		c.bytecode = append(c.bytecode, instruction{opcode: opPutVar, operand: Term(c.varOffset(v))})
		return nil
	}

	if ok := vm.Terms.Atomic(a); ok {
		c.bytecode = append(c.bytecode, instruction{opcode: opPutConst, operand: a})
		return nil
	}

	// TODO: Optimization for lists, strings, etc.

	f, arg, _ := vm.Terms.Compound(a)

	var ok bool
	vm.Terms.functors, ok = cappend(vm.Terms.functors, f)
	if !ok {
		return errTooManyFunctors
	}

	c.bytecode = append(c.bytecode, instruction{opcode: opPutFunctor, operand: Term(len(vm.Terms.functors) - 1)})
	for i := 0; i < f.Arity; i++ {
		a, err := arg(i)
		if err != nil {
			return err
		}
		if err := vm.compileBodyArg(c, a); err != nil {
			return err
		}
	}
	c.bytecode = append(c.bytecode, instruction{opcode: opPop})
	return nil
}

func (c *clause) varOffset(o Variable) int {
	for i, v := range c.vars {
		if v == o {
			return i
		}
	}
	c.vars = append(c.vars, o)
	return len(c.vars) - 1
}
