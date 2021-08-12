package engine

import (
	"bufio"
	"bytes"
	"errors"
	"fmt"
	"io"
	"math"
	"os"
	"sort"
	"strings"
	"unicode"
	"unicode/utf8"

	"github.com/ichiban/prolog/internal"
)

// Conjunction executes the given goals p then q.
// Note that ,/2 in clause body (H :- B1, B2, ..., Bn.) is compiled and doesn't involve this method.
func (vm *VM) Conjunction(p, q Term, k func(*Env) Promise, env *Env) Promise {
	return Delay(func() Promise {
		env := NewEnv(env)
		pi, args, err := piArgs(p, env)
		if err != nil {
			return Error(err)
		}

		return vm.arrive(pi, args, func(env *Env) Promise {
			pi, args, err := piArgs(q, env)
			if err != nil {
				return Error(err)
			}

			return Delay(func() Promise {
				env := NewEnv(env)
				return vm.arrive(pi, args, k, env)
			})
		}, env)
	})
}

func (vm *VM) Disjunction(p, q Term, k func(*Env) Promise, env *Env) Promise {
	if c, ok := env.Resolve(p).(*Compound); ok && c.Functor == "->" && len(c.Args) == 2 {
		return vm.IfThenElse(c.Args[0], c.Args[1], q, k, env)
	}

	return Delay(
		func() Promise {
			env := NewEnv(env)
			pi, args, err := piArgs(p, env)
			if err != nil {
				return Error(err)
			}
			return vm.arrive(pi, args, k, env)
		},
		func() Promise {
			env := NewEnv(env)
			pi, args, err := piArgs(q, env)
			if err != nil {
				return Error(err)
			}
			return vm.arrive(pi, args, k, env)
		},
	)
}

func (vm *VM) Negation(goal Term, k func(*Env) Promise, env *Env) Promise {
	ok, err := vm.Call(goal, Success, NewEnv(env)).Force()
	if err != nil {
		return Error(err)
	}
	if ok {
		return Bool(false)
	}
	return k(env)
}

func (vm *VM) IfThenElse(if_, then_, else_ Term, k func(*Env) Promise, env *Env) Promise {
	ok, err := vm.Call(if_, func(_ *Env) Promise {
		return Bool(true)
	}, NewEnv(env)).Force()
	if err != nil {
		return Error(err)
	}
	if ok {
		return Delay(func() Promise {
			env := NewEnv(env)
			return vm.Call(then_, k, env)
		})
	} else {
		return Delay(func() Promise {
			env := NewEnv(env)
			return vm.Call(else_, k, env)
		})
	}
}

// Call executes goal. it succeeds if goal followed by k succeeds. A cut inside goal doesn't affect outside of Call.
func (vm *VM) Call(goal Term, k func(*Env) Promise, env *Env) Promise {
	var c clause
	if err := c.compileClause(Atom(""), goal, env); err != nil {
		return Error(err)
	}

	return Opaque(vm.exec(c.bytecode, c.xrTable, c.vars, cont{
		exit: k,
		fail: func(_ *Env) Promise {
			return Bool(false)
		},
	}, List(), List(), env))
}

// Unify unifies t1 and t2 without occurs check (i.e., X = f(X) is allowed).
func Unify(t1, t2 Term, k func(*Env) Promise, env *Env) Promise {
	if !t1.Unify(t2, false, env) {
		return Bool(false)
	}
	return k(env)
}

// UnifyWithOccursCheck unifies t1 and t2 with occurs check (i.e., X = f(X) is not allowed).
func UnifyWithOccursCheck(t1, t2 Term, k func(*Env) Promise, env *Env) Promise {
	if !t1.Unify(t2, true, env) {
		return Bool(false)
	}
	return k(env)
}

// TypeVar checks if t is a variable.
func TypeVar(t Term, k func(*Env) Promise, env *Env) Promise {
	if _, ok := env.Resolve(t).(Variable); !ok {
		return Bool(false)
	}
	return k(env)
}

// TypeFloat checks if t is a floating-point number.
func TypeFloat(t Term, k func(*Env) Promise, env *Env) Promise {
	if _, ok := env.Resolve(t).(Float); !ok {
		return Bool(false)
	}
	return k(env)
}

// TypeInteger checks if t is an integer.
func TypeInteger(t Term, k func(*Env) Promise, env *Env) Promise {
	if _, ok := env.Resolve(t).(Integer); !ok {
		return Bool(false)
	}
	return k(env)
}

// TypeAtom checks if t is an atom.
func TypeAtom(t Term, k func(*Env) Promise, env *Env) Promise {
	if _, ok := env.Resolve(t).(Atom); !ok {
		return Bool(false)
	}
	return k(env)
}

// TypeCompound checks if t is a compound term.
func TypeCompound(t Term, k func(*Env) Promise, env *Env) Promise {
	if _, ok := env.Resolve(t).(*Compound); !ok {
		return Bool(false)
	}
	return k(env)
}

// Functor extracts the name and arity of term, or unifies term with an atomic/compound term of name and arity with
// fresh variables as arguments.
func Functor(term, name, arity Term, k func(*Env) Promise, env *Env) Promise {
	term = env.Resolve(term)
	switch t := env.Resolve(term).(type) {
	case Variable:
		a, ok := env.Resolve(arity).(Integer)
		if !ok {
			return Error(typeErrorInteger(arity))
		}
		switch {
		case a < 0:
			return Error(domainErrorNotLessThanZero(a))
		case a == 0:
			return Unify(t, name, k, env)
		}

		n, ok := env.Resolve(name).(Atom)
		if !ok {
			return Error(typeErrorAtom(name))
		}

		vs := make([]Term, a)
		for i := range vs {
			vs[i] = NewVariable()
		}
		return Delay(func() Promise {
			env := NewEnv(env)
			return Unify(t, &Compound{
				Functor: n,
				Args:    vs,
			}, k, env)
		})
	case *Compound:
		pattern := Compound{Args: []Term{name, arity}}
		return Delay(func() Promise {
			env := NewEnv(env)
			return Unify(&pattern, &Compound{Args: []Term{t.Functor, Integer(len(t.Args))}}, k, env)
		})
	default: // atomic
		pattern := Compound{Args: []Term{name, arity}}
		return Delay(func() Promise {
			env := NewEnv(env)
			return Unify(&pattern, &Compound{Args: []Term{t, Integer(0)}}, k, env)
		})
	}
}

// Arg extracts nth argument of term as arg, or finds the argument position of arg in term as nth.
func Arg(nth, term, arg Term, k func(*Env) Promise, env *Env) Promise {
	t, ok := env.Resolve(term).(*Compound)
	if !ok {
		return Error(typeErrorCompound(term))
	}

	switch n := env.Resolve(nth).(type) {
	case Variable:
		pattern := Compound{Args: []Term{n, arg}}
		ks := make([]func() Promise, len(t.Args))
		for i := range t.Args {
			n := Integer(i + 1)
			arg := t.Args[i]
			ks[i] = func() Promise {
				env := NewEnv(env)
				return Unify(&pattern, &Compound{Args: []Term{n, arg}}, k, env)
			}
		}
		return Delay(ks...)
	case Integer:
		if n == 0 || int(n) >= len(t.Args) {
			return Bool(false)
		}
		if n < 0 {
			return Error(domainErrorNotLessThanZero(n))
		}
		return Delay(func() Promise {
			env := NewEnv(env)
			return Unify(arg, t.Args[int(n)-1], k, env)
		})
	default:
		return Error(typeErrorInteger(n))
	}
}

// Univ constructs list as a list which first element is the functor of term and the rest is the arguments of term, or construct a compound from list as term.
func Univ(term, list Term, k func(*Env) Promise, env *Env) Promise {
	switch t := env.Resolve(term).(type) {
	case Variable:
		list = env.Resolve(list)
		if list == Atom("[]") {
			return Error(domainErrorNotEmptyList(list))
		}
		cons, ok := list.(*Compound)
		if !ok || cons.Functor != "." || len(cons.Args) != 2 {
			return Error(typeErrorList(list))
		}

		f, ok := cons.Args[0].(Atom)
		if !ok {
			return Error(typeErrorAtom(cons.Args[0]))
		}

		var args []Term
		if err := Each(cons.Args[1], func(elem Term) error {
			args = append(args, elem)
			return nil
		}, env); err != nil {
			return Error(err)
		}

		return Delay(func() Promise {
			env := NewEnv(env)
			return Unify(term, &Compound{
				Functor: f,
				Args:    args,
			}, k, env)
		})
	case *Compound:
		return Delay(func() Promise {
			env := NewEnv(env)
			return Unify(list, List(append([]Term{t.Functor}, t.Args...)...), k, env)
		})
	default:
		return Delay(func() Promise {
			env := NewEnv(env)
			return Unify(list, List(t), k, env)
		})
	}
}

// CopyTerm clones in as out.
func CopyTerm(in, out Term, k func(*Env) Promise, env *Env) Promise {
	return Unify(copyTerm(in, nil, env), out, k, env)
}

func copyTerm(t Term, vars map[Variable]Variable, env *Env) Term {
	if vars == nil {
		vars = map[Variable]Variable{}
	}
	switch t := env.Resolve(t).(type) {
	case Variable:
		v, ok := vars[t]
		if !ok {
			v = NewVariable()
			vars[t] = v
		}
		return v
	case *Compound:
		c := Compound{
			Functor: t.Functor,
			Args:    make([]Term, len(t.Args)),
		}
		for i, a := range t.Args {
			c.Args[i] = copyTerm(a, vars, env)
		}
		return &c
	default:
		return t
	}
}

// Op defines operator with priority and specifier, or removes when priority is 0.
func (vm *VM) Op(priority, specifier, operator Term, k func(*Env) Promise, env *Env) Promise {
	p, ok := env.Resolve(priority).(Integer)
	if !ok {
		return Error(typeErrorInteger(priority))
	}
	if p < 0 || p > 1200 {
		return Error(domainErrorOperatorPriority(priority))
	}

	s, ok := env.Resolve(specifier).(Atom)
	if !ok {
		return Error(typeErrorAtom(specifier))
	}
	switch s {
	case "xf", "yf", "xfx", "xfy", "yfx", "fx", "fy":
		break
	default:
		return Error(domainErrorOperatorSpecifier(s))
	}

	o, ok := env.Resolve(operator).(Atom)
	if !ok {
		return Error(typeErrorAtom(operator))
	}

	// already defined?
	for i, op := range vm.operators {
		if op.Specifier != s || op.Name != o {
			continue
		}

		// remove it first so that we can insert it again in the right position
		copy(vm.operators[i:], vm.operators[i+1:])
		vm.operators[len(vm.operators)-1] = Operator{}
		vm.operators = vm.operators[:len(vm.operators)-1]

		// or keep it removed.
		if p == 0 {
			return k(env)
		}
	}

	// insert
	i := sort.Search(len(vm.operators), func(i int) bool {
		return vm.operators[i].Priority >= p
	})
	vm.operators = append(vm.operators, Operator{})
	copy(vm.operators[i+1:], vm.operators[i:])
	vm.operators[i] = Operator{
		Priority:  p,
		Specifier: s,
		Name:      o,
	}

	return k(env)
}

// CurrentOp succeeds if operator is defined with priority and specifier.
func (vm *VM) CurrentOp(priority, specifier, operator Term, k func(*Env) Promise, env *Env) Promise {
	switch p := env.Resolve(priority).(type) {
	case Variable:
		break
	case Integer:
		if p < 0 || p > 1200 {
			return Error(domainErrorOperatorPriority(priority))
		}
		break
	default:
		return Error(domainErrorOperatorPriority(priority))
	}

	switch s := env.Resolve(specifier).(type) {
	case Variable:
		break
	case Atom:
		switch s {
		case "xf", "yf", "xfx", "xfy", "yfx", "fx", "fy":
			break
		default:
			return Error(domainErrorOperatorSpecifier(s))
		}
	default:
		return Error(domainErrorOperatorSpecifier(s))
	}

	switch env.Resolve(operator).(type) {
	case Variable, Atom:
		break
	default:
		return Error(typeErrorAtom(operator))
	}

	pattern := Compound{Args: []Term{priority, specifier, operator}}
	ks := make([]func() Promise, len(vm.operators))
	for i := range vm.operators {
		op := vm.operators[i]
		ks[i] = func() Promise {
			env := NewEnv(env)
			return Unify(&pattern, &Compound{Args: []Term{op.Priority, op.Specifier, op.Name}}, k, env)
		}
	}
	return Delay(ks...)
}

// Assertz appends t to the database.
func (vm *VM) Assertz(t Term, k func(*Env) Promise, env *Env) Promise {
	return vm.assert(t, k, func(cs clauses, c clause) clauses {
		return append(cs, c)
	}, env)
}

// Asserta prepends t to the database.
func (vm *VM) Asserta(t Term, k func(*Env) Promise, env *Env) Promise {
	return vm.assert(t, k, func(cs clauses, c clause) clauses {
		return append(clauses{c}, cs...)
	}, env)
}

func (vm *VM) assert(t Term, k func(*Env) Promise, merge func(clauses, clause) clauses, env *Env) Promise {
	pi, args, err := piArgs(t, env)
	if err != nil {
		return Error(err)
	}

	switch pi {
	case procedureIndicator{name: ":-", arity: 1}: // directive
		name, args, err := piArgs(args.(*Compound).Args[0], env)
		if err != nil {
			return Error(err)
		}
		return Delay(func() Promise {
			env := NewEnv(env)
			return vm.arrive(name, args, k, env)
		})
	case procedureIndicator{name: ":-", arity: 2}:
		pi, _, err = piArgs(args.(*Compound).Args[0], env)
		if err != nil {
			return Error(err)
		}
	}

	if vm.procedures == nil {
		vm.procedures = map[procedureIndicator]procedure{}
	}
	p, ok := vm.procedures[pi]
	if !ok {
		p = clauses{}
	}

	cs, ok := p.(clauses)
	if !ok {
		return Error(permissionErrorModifyStaticProcedure(&Compound{
			Functor: "/",
			Args:    []Term{pi.name, pi.arity},
		}))
	}
	c := clause{pi: pi}
	if err := c.compile(t, env); err != nil {
		return Error(err)
	}

	vm.procedures[pi] = merge(cs, c)
	return k(env)
}

// Repeat enforces k until it returns true.
func Repeat(k func(*Env) Promise, env *Env) Promise {
	for {
		ok, err := k(env).Force()
		if err != nil {
			return Error(err)
		}
		if ok {
			return Bool(true)
		}
	}
}

// BagOf collects all the solutions of goal as instances, which unify with template. instances may contain duplications.
func (vm *VM) BagOf(template, goal, instances Term, k func(*Env) Promise, env *Env) Promise {
	return vm.collectionOf(template, goal, instances, k, List, env)
}

// SetOf collects all the solutions of goal as instances, which unify with template. instances don't contain duplications.
func (vm *VM) SetOf(template, goal, instances Term, k func(*Env) Promise, env *Env) Promise {
	return vm.collectionOf(template, goal, instances, k, Set, env)
}

func (vm *VM) collectionOf(template, goal, instances Term, k func(*Env) Promise, agg func(...Term) Term, env *Env) Promise {
	if _, ok := env.Resolve(goal).(Variable); ok {
		return Error(instantiationError(goal))
	}

	qualifier, body := NewVariable(), NewVariable()
	if goal.Unify(&Compound{
		Functor: "^",
		Args:    []Term{qualifier, body},
	}, false, env) {
		goal = body
	}

	fvs := env.FreeVariables(goal)

	freeVariables := env.FreeVariables(template, qualifier)
	groupingVariables := make([]Variable, 0, len(fvs))
grouping:
	for _, v := range fvs {
		for _, w := range freeVariables {
			if v == w {
				continue grouping
			}
		}
		groupingVariables = append(groupingVariables, v)
	}

	type solution struct {
		snapshots []Term // snapshot of grouping variable values
		bag       []Term
	}

	var solutions []solution
	_, err := vm.Call(goal, func(env *Env) Promise {
		snapshots := make([]Term, len(groupingVariables))
		for i, v := range groupingVariables {
			snapshots[i] = env.Resolve(v)
		}

	solutions:
		for i, s := range solutions {
			env := NewEnv(env)
			for i := range groupingVariables {
				ok, err := Compare(Atom("="), s.snapshots[i], snapshots[i], Success, env).Force()
				if err != nil {
					return Error(err)
				}
				if !ok {
					continue solutions
				}
			}
			solutions[i].bag = append(s.bag, copyTerm(template, nil, env))
			return Bool(false) // ask for more solutions
		}

		solutions = append(solutions, solution{
			snapshots: snapshots,
			bag:       []Term{copyTerm(template, nil, env)},
		})
		return Bool(false) // ask for more solutions
	}, env).Force()
	if err != nil {
		return Error(err)
	}

	if len(solutions) == 0 {
		return Bool(false)
	}

	ks := make([]func() Promise, len(solutions))
	for i := range solutions {
		s := solutions[i]
		ks[i] = func() Promise {
			env := NewEnv(env)
			// revert to snapshot
			for i, v := range groupingVariables {
				env.Bind(v, s.snapshots[i])
			}

			return Unify(instances, agg(s.bag...), k, env)
		}
	}
	return Delay(ks...)
}

// Compare compares term1 and term2 and unifies order with <, =, or >.
func Compare(order, term1, term2 Term, k func(*Env) Promise, env *Env) Promise {
	switch o := env.Resolve(order).(type) {
	case Variable:
		break
	case Atom:
		switch o {
		case "<", "=", ">":
			break
		default:
			return Error(domainErrorOrder(order))
		}
		break
	default:
		return Error(typeErrorAtom(order))
	}

	d := compare(env.Resolve(term1), env.Resolve(term2))
	switch {
	case d < 0:
		return Unify(Atom("<"), order, k, env)
	case d > 0:
		return Unify(Atom(">"), order, k, env)
	default: // d == 0:
		return Unify(Atom("="), order, k, env)
	}
}

func compare(a, b Term) int64 {
	switch a := a.(type) {
	case Variable:
		switch b := b.(type) {
		case Variable:
			return int64(strings.Compare(string(a), string(b)))
		default:
			return -1
		}
	case Float:
		switch b := b.(type) {
		case Variable:
			return 1
		case Float:
			return int64(a - b)
		case Integer:
			d := int64(a - Float(b))
			if d == 0 {
				return -1
			}
			return d
		default:
			return -1
		}
	case Integer:
		switch b := b.(type) {
		case Variable:
			return 1
		case Float:
			d := int64(Float(a) - b)
			if d == 0 {
				return 1
			}
			return d
		case Integer:
			return int64(a - b)
		default:
			return -1
		}
	case Atom:
		switch b := b.(type) {
		case Variable, Float, Integer:
			return 1
		case Atom:
			return int64(strings.Compare(string(a), string(b)))
		default:
			return -1
		}
	case *Compound:
		switch b := b.(type) {
		case *Compound:
			d := len(a.Args) - len(b.Args)
			if d != 0 {
				return int64(d)
			}

			d = strings.Compare(string(a.Functor), string(b.Functor))
			if d != 0 {
				return int64(d)
			}

			for i := range a.Args {
				d := compare(a.Args[i], b.Args[i])
				if d != 0 {
					return d
				}
			}

			return 0
		default:
			return 1
		}
	default:
		return 1
	}
}

// Throw throws ball as an exception.
func Throw(ball Term, _ func(*Env) Promise, env *Env) Promise {
	if _, ok := env.Resolve(ball).(Variable); ok {
		return Error(instantiationError(ball))
	}
	return Error(&Exception{Term: copyTerm(env.Resolve(ball), nil, env)})
}

// Catch calls goal. If an exception is thrown and unifies with catcher, it calls recover.
func (vm *VM) Catch(goal, catcher, recover Term, k func(*Env) Promise, env *Env) Promise {
	ok, err := vm.Call(goal, k, env).Force()
	if err != nil {
		if ex, ok := err.(*Exception); ok && catcher.Unify(ex.Term, false, env) {
			return Delay(func() Promise {
				env := NewEnv(env)
				return vm.Call(recover, k, env)
			})
		}
		return Error(err)
	}
	return Bool(ok)
}

// CurrentPredicate matches pi with a predicate indicator of the user-defined procedures in the database.
func (vm *VM) CurrentPredicate(pi Term, k func(*Env) Promise, env *Env) Promise {
	switch pi := env.Resolve(pi).(type) {
	case Variable:
		break
	case *Compound:
		if pi.Functor != "/" || len(pi.Args) != 2 {
			return Error(typeErrorPredicateIndicator(pi))
		}
		if _, ok := env.Resolve(pi.Args[0]).(Atom); !ok {
			return Error(typeErrorPredicateIndicator(pi))
		}
		if _, ok := env.Resolve(pi.Args[1]).(Integer); !ok {
			return Error(typeErrorPredicateIndicator(pi))
		}
		break
	default:
		return Error(typeErrorPredicateIndicator(pi))
	}

	ks := make([]func() Promise, 0, len(vm.procedures))
	for key := range vm.procedures {
		c := Compound{Functor: "/", Args: []Term{key.name, key.arity}}
		ks = append(ks, func() Promise {
			env := NewEnv(env)
			return Unify(pi, &c, k, env)
		})
	}
	return Delay(ks...)
}

// Retract removes a clause which matches with t.
func (vm *VM) Retract(t Term, k func(*Env) Promise, env *Env) Promise {
	t = Rulify(t, env)

	h := t.(*Compound).Args[0]
	pi, _, err := piArgs(h, env)
	if err != nil {
		return Error(err)
	}

	p, ok := vm.procedures[pi]
	if !ok {
		return Bool(false)
	}

	cs, ok := p.(clauses)
	if !ok {
		return Error(permissionErrorModifyStaticProcedure(&Compound{
			Functor: "/",
			Args:    []Term{pi.name, pi.arity},
		}))
	}

	updated := make(clauses, 0, len(cs))
	defer func() { vm.procedures[pi] = updated }()

	for i, c := range cs {
		env := NewEnv(env)

		raw := Rulify(c.raw, env)

		if !t.Unify(raw, false, env) {
			updated = append(updated, c)
			continue
		}

		ok, err := k(env).Force()
		if err != nil {
			updated = append(updated, cs[i+1:]...)
			return Error(err)
		}
		if ok {
			updated = append(updated, cs[i+1:]...)
			return Bool(true)
		}
	}

	return Bool(false)
}

// Abolish removes the procedure indicated by pi from the database.
func (vm *VM) Abolish(pi Term, k func(*Env) Promise, env *Env) Promise {
	if _, ok := env.Resolve(pi).(Variable); ok {
		return Error(instantiationError(pi))
	}

	c, ok := env.Resolve(pi).(*Compound)
	if !ok || c.Functor != "/" || len(c.Args) != 2 {
		return Error(typeErrorPredicateIndicator(pi))
	}

	if _, ok := env.Resolve(c.Args[0]).(Variable); ok {
		return Error(instantiationError(c.Args[0]))
	}

	name, ok := env.Resolve(c.Args[0]).(Atom)
	if !ok {
		return Error(typeErrorAtom(c.Args[0]))
	}

	if _, ok := env.Resolve(c.Args[1]).(Variable); ok {
		return Error(instantiationError(c.Args[1]))
	}

	arity, ok := env.Resolve(c.Args[1]).(Integer)
	if !ok {
		return Error(typeErrorInteger(c.Args[1]))
	}
	if arity < 0 {
		return Error(domainErrorNotLessThanZero(c.Args[1]))
	}

	key := procedureIndicator{name: name, arity: arity}
	if _, ok := vm.procedures[key].(clauses); !ok {
		return Error(permissionErrorModifyStaticProcedure(&Compound{
			Functor: "/",
			Args:    []Term{name, arity},
		}))
	}
	delete(vm.procedures, key)
	return k(env)
}

// CurrentInput unifies stream with the current input stream.
func (vm *VM) CurrentInput(stream Term, k func(*Env) Promise, env *Env) Promise {
	switch env.Resolve(stream).(type) {
	case Variable, *Stream:
		break
	default:
		return Error(domainErrorStream(stream))
	}

	return Delay(func() Promise {
		env := NewEnv(env)
		return Unify(stream, vm.input, k, env)
	})
}

// CurrentOutput unifies stream with the current output stream.
func (vm *VM) CurrentOutput(stream Term, k func(*Env) Promise, env *Env) Promise {
	switch env.Resolve(stream).(type) {
	case Variable, *Stream:
		break
	default:
		return Error(domainErrorStream(stream))
	}

	return Delay(func() Promise {
		env := NewEnv(env)
		return Unify(stream, vm.output, k, env)
	})
}

// SetInput sets streamOrAlias as the current input stream.
func (vm *VM) SetInput(streamOrAlias Term, k func(*Env) Promise, env *Env) Promise {
	s, err := vm.stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	if s.source == nil {
		return Error(permissionErrorInputStream(streamOrAlias))
	}

	vm.input = s
	return k(env)
}

// SetOutput sets streamOrAlias as the current output stream.
func (vm *VM) SetOutput(streamOrAlias Term, k func(*Env) Promise, env *Env) Promise {
	s, err := vm.stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	if s.sink == nil {
		return Error(permissionErrorOutputStream(streamOrAlias))
	}

	vm.output = s
	return k(env)
}

// Open opens sourceSink in mode and unifies with stream.
func (vm *VM) Open(sourceSink, mode, stream, options Term, k func(*Env) Promise, env *Env) Promise {
	var n Atom
	switch s := env.Resolve(sourceSink).(type) {
	case Variable:
		return Error(instantiationError(sourceSink))
	case Atom:
		n = s
	default:
		return Error(domainErrorSourceSink(sourceSink))
	}

	var (
		s Stream

		flag   int
		perm   os.FileMode
		buffer bool
	)
	switch m := env.Resolve(mode).(type) {
	case Variable:
		return Error(instantiationError(mode))
	case Atom:
		switch m {
		case "read":
			s.mode = streamModeRead
			flag = os.O_RDONLY
			buffer = true
		case "write":
			s.mode = streamModeWrite
			flag = os.O_CREATE | os.O_WRONLY
			perm = 0644
		case "append":
			s.mode = streamModeAppend
			flag = os.O_APPEND | os.O_CREATE | os.O_WRONLY
			perm = 0644
		default:
			return Error(domainErrorIOMode(m))
		}
	default:
		return Error(typeErrorAtom(mode))
	}

	if _, ok := env.Resolve(stream).(Variable); !ok {
		return Error(typeErrorVariable(stream))
	}

	if err := Each(env.Resolve(options), func(option Term) error {
		switch o := env.Resolve(option).(type) {
		case Variable:
			return instantiationError(option)
		case *Compound:
			if len(o.Args) != 1 {
				return domainErrorStreamOption(option)
			}
			arg := o.Args[0]
			switch o.Functor {
			case "type":
				switch t := env.Resolve(arg).(type) {
				case Variable:
					return instantiationError(arg)
				case Atom:
					switch t {
					case "text":
						s.streamType = streamTypeText
						return nil
					case "binary":
						s.streamType = streamTypeBinary
						return nil
					default:
						return domainErrorStreamOption(option)
					}
				default:
					return typeErrorAtom(arg)
				}
			case "reposition":
				switch b := env.Resolve(arg).(type) {
				case Variable:
					return instantiationError(arg)
				case Atom:
					switch b {
					case "true":
						s.reposition = true
						return nil
					case "false":
						s.reposition = false
						return nil
					default:
						return domainErrorStreamOption(option)
					}
				default:
					return typeErrorAtom(arg)
				}
			case "alias":
				switch a := env.Resolve(arg).(type) {
				case Variable:
					return instantiationError(arg)
				case Atom:
					if _, ok := vm.streams[a]; ok {
						return permissionError(Atom("open"), Atom("source_sink"), option, Atom(fmt.Sprintf("%s is already defined as an alias.", a)))
					}
					s.alias = a
					return nil
				default:
					return domainErrorStreamOption(option)
				}
			case "eof_action":
				switch a := env.Resolve(arg).(type) {
				case Variable:
					return instantiationError(arg)
				case Atom:
					switch a {
					case "error":
						s.eofAction = eofActionError
						return nil
					case "eof_code":
						s.eofAction = eofActionEOFCode
						return nil
					case "reset":
						s.eofAction = eofActionReset
						return nil
					default:
						return domainErrorStreamOption(option)
					}
				default:
					return domainErrorStreamOption(option)
				}
			default:
				return domainErrorStreamOption(option)
			}
		default:
			return domainErrorStreamOption(option)
		}
	}, env); err != nil {
		return Error(err)
	}

	f, err := os.OpenFile(string(n), flag, perm)
	if err != nil {
		switch {
		case os.IsNotExist(err):
			return Error(existenceErrorSourceSink(sourceSink))
		case os.IsPermission(err):
			return Error(permissionError(Atom("open"), Atom("source_sink"), sourceSink, Atom(fmt.Sprintf("%s cannot be opened.", sourceSink))))
		default:
			return Error(systemError(err))
		}
	}

	switch s.mode {
	case streamModeRead:
		s.source = f
		if buffer {
			s.source = bufio.NewReader(s.source)
		}
	case streamModeWrite, streamModeAppend:
		s.sink = f
		if buffer {
			s.sink = bufio.NewWriter(s.sink)
		}
	}
	s.closer = f

	if vm.streams == nil {
		vm.streams = map[Term]*Stream{}
	}
	if s.alias == "" {
		// we can't use alias for the key but all the open streams should be in streams map anyways.
		vm.streams[&s] = &s
	} else {
		vm.streams[s.alias] = &s
	}

	return Delay(func() Promise {
		env := NewEnv(env)
		return Unify(stream, &s, k, env)
	})
}

// Close closes a stream specified by streamOrAlias.
func (vm *VM) Close(streamOrAlias, options Term, k func(*Env) Promise, env *Env) Promise {
	s, err := vm.stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	var force bool
	if err := Each(env.Resolve(options), func(option Term) error {
		if _, ok := env.Resolve(option).(Variable); ok {
			return instantiationError(option)
		}

		switch {
		case option.Unify(&Compound{Functor: "force", Args: []Term{Atom("false")}}, false, env):
			force = false
		case option.Unify(&Compound{Functor: "force", Args: []Term{Atom("true")}}, false, env):
			force = true
		default:
			return domainErrorStreamOption(option)
		}
		return nil
	}, env); err != nil {
		return Error(err)
	}

	if err := s.closer.Close(); err != nil && !force {
		return Error(resourceError(streamOrAlias, Atom(err.Error())))
	}

	if s.alias == "" {
		delete(vm.streams, s)
	} else {
		delete(vm.streams, s.alias)
	}

	return k(env)
}

// FlushOutput sends any buffered output to the stream.
func (vm *VM) FlushOutput(streamOrAlias Term, k func(*Env) Promise, env *Env) Promise {
	s, err := vm.stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	if s.sink == nil {
		return Error(permissionErrorOutputStream(streamOrAlias))
	}

	type flusher interface {
		Flush() error
	}

	if f, ok := s.sink.(flusher); ok {
		if err := f.Flush(); err != nil {
			return Error(err)
		}
	}

	return k(env)
}

// WriteTerm outputs term to stream with options.
func (vm *VM) WriteTerm(streamOrAlias, term, options Term, k func(*Env) Promise, env *Env) Promise {
	s, err := vm.stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	if s.sink == nil {
		return Error(permissionErrorOutputStream(streamOrAlias))
	}

	if s.streamType == streamTypeBinary {
		return Error(permissionErrorOutputBinaryStream(streamOrAlias))
	}

	opts := WriteTermOptions{Ops: vm.operators}
	if err := Each(env.Resolve(options), func(option Term) error {
		if _, ok := env.Resolve(option).(Variable); ok {
			return instantiationError(option)
		}

		switch {
		case option.Unify(&Compound{Functor: "quoted", Args: []Term{Atom("false")}}, false, env):
			opts.Quoted = false
		case option.Unify(&Compound{Functor: "quoted", Args: []Term{Atom("true")}}, false, env):
			opts.Quoted = true
		case option.Unify(&Compound{Functor: "ignore_ops", Args: []Term{Atom("false")}}, false, env):
			opts.Ops = vm.operators
		case option.Unify(&Compound{Functor: "ignore_ops", Args: []Term{Atom("true")}}, false, env):
			opts.Ops = nil
		case option.Unify(&Compound{Functor: "numbervars", Args: []Term{Atom("false")}}, false, env):
			opts.NumberVars = false
		case option.Unify(&Compound{Functor: "numbervars", Args: []Term{Atom("true")}}, false, env):
			opts.NumberVars = true
		default:
			return domainErrorWriteOption(option)
		}
		return nil
	}, env); err != nil {
		return Error(err)
	}

	if err := env.Resolve(term).WriteTerm(s.sink, opts, env); err != nil {
		return Error(err)
	}

	return k(env)
}

// CharCode converts a single-rune Atom char to an Integer code, or vice versa.
func CharCode(char, code Term, k func(*Env) Promise, env *Env) Promise {
	switch ch := env.Resolve(char).(type) {
	case Variable:
		switch cd := env.Resolve(code).(type) {
		case Variable:
			return Error(instantiationError(&Compound{
				Functor: ",",
				Args:    []Term{char, code},
			}))
		case Integer:
			r := rune(cd)

			if !utf8.ValidRune(r) {
				return Error(representationError(Atom("character_code"), Atom(fmt.Sprintf("%d is not a valid unicode code point.", r))))
			}

			return Delay(func() Promise {
				env := NewEnv(env)
				return Unify(ch, Atom(r), k, env)
			})
		default:
			return Error(typeErrorInteger(code))
		}
	case Atom:
		rs := []rune(ch)
		if len(rs) != 1 {
			return Error(typeErrorCharacter(char))
		}

		return Delay(func() Promise {
			env := NewEnv(env)
			return Unify(code, Integer(rs[0]), k, env)
		})
	default:
		return Error(typeErrorCharacter(char))
	}
}

// PutByte outputs an integer byte to a stream represented by streamOrAlias.
func (vm *VM) PutByte(streamOrAlias, byt Term, k func(*Env) Promise, env *Env) Promise {
	s, err := vm.stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	if s.sink == nil {
		return Error(permissionErrorOutputStream(streamOrAlias))
	}

	if s.streamType == streamTypeText {
		return Error(permissionErrorOutputTextStream(streamOrAlias))
	}

	switch b := env.Resolve(byt).(type) {
	case Variable:
		return Error(instantiationError(byt))
	case Integer:
		if 0 > b || 255 < b {
			return Error(typeErrorByte(byt))
		}

		if _, err := s.sink.Write([]byte{byte(b)}); err != nil {
			return Error(systemError(err))
		}

		return k(env)
	default:
		return Error(typeErrorByte(byt))
	}
}

// PutCode outputs code to the stream represented by streamOrAlias.
func (vm *VM) PutCode(streamOrAlias, code Term, k func(*Env) Promise, env *Env) Promise {
	s, err := vm.stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	if s.sink == nil {
		return Error(permissionErrorOutputStream(streamOrAlias))
	}

	if s.streamType == streamTypeBinary {
		return Error(permissionErrorOutputBinaryStream(streamOrAlias))
	}

	switch c := env.Resolve(code).(type) {
	case Variable:
		return Error(instantiationError(code))
	case Integer:
		r := rune(c)

		if !utf8.ValidRune(r) {
			return Error(representationError(Atom("character_code"), Atom(fmt.Sprintf("%s is not a valid unicode code point.", c))))
		}

		if _, err := s.sink.Write([]byte(string(r))); err != nil {
			return Error(systemError(err))
		}

		return k(env)
	default:
		return Error(typeErrorInteger(code))
	}
}

// ReadTerm reads from the stream represented by streamOrAlias and unifies with stream.
func (vm *VM) ReadTerm(streamOrAlias, term, options Term, k func(*Env) Promise, env *Env) Promise {
	s, err := vm.stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	if s.source == nil {
		return Error(permissionErrorInputStream(streamOrAlias))
	}

	if s.streamType == streamTypeBinary {
		return Error(permissionErrorInputBinaryStream(streamOrAlias))
	}

	var opts readTermOptions
	if err := Each(env.Resolve(options), func(option Term) error {
		if _, ok := env.Resolve(option).(Variable); ok {
			return instantiationError(option)
		}

		v := NewVariable()
		switch {
		case option.Unify(&Compound{Functor: "singletons", Args: []Term{v}}, false, env):
			opts.singletons = v
		case option.Unify(&Compound{Functor: "variables", Args: []Term{v}}, false, env):
			opts.variables = v
		case option.Unify(&Compound{Functor: "variable_names", Args: []Term{v}}, false, env):
			opts.variableNames = v
		default:
			return domainErrorReadOption(option)
		}
		return nil
	}, env); err != nil {
		return Error(err)
	}

	br, ok := s.source.(*bufio.Reader)
	if !ok {
		return Error(errors.New("not a buffered stream"))
	}

	p := NewParser(vm, br)
	t, err := p.Term()
	if err != nil {
		switch {
		case errors.Is(err, io.EOF):
			switch s.eofAction {
			case eofActionError:
				return Error(permissionErrorInputPastEndOfStream(streamOrAlias))
			case eofActionEOFCode:
				return Delay(func() Promise {
					env := NewEnv(env)
					return Unify(term, Atom("end_of_file"), k, env)
				})
			case eofActionReset:
				return Delay(func() Promise {
					env := NewEnv(env)
					return vm.ReadTerm(streamOrAlias, term, options, k, env)
				})
			default:
				return Error(systemError(fmt.Errorf("unknown EOF action: %d", s.eofAction)))
			}
		case errors.Is(err, internal.ErrInsufficient):
			return Error(syntaxErrorInsufficient())
		case errors.As(err, &internal.UnexpectedRuneError{}):
			return Error(syntaxErrorUnexpectedChar(Atom(err.Error())))
		default:
			return Error(systemError(err))
		}
	}

	var singletons, variables, variableNames []Term
	for _, vc := range p.vars {
		if vc.Count == 1 {
			singletons = append(singletons, vc.variable)
		}
		variables = append(variables, vc.variable)
		variableNames = append(variableNames, &Compound{
			Functor: "=",
			Args:    []Term{Atom(vc.variable), vc.variable},
		})
		vc.variable = NewVariable()
	}

	if opts.singletons != "" && !opts.singletons.Unify(List(singletons...), false, env) {
		return Bool(false)
	}

	if opts.variables != "" && !opts.variables.Unify(List(variables...), false, env) {
		return Bool(false)
	}

	if opts.variableNames != "" && !opts.variableNames.Unify(List(variableNames...), false, env) {
		return Bool(false)
	}

	return Delay(func() Promise {
		env := NewEnv(env)
		return Unify(term, t, k, env)
	})
}

// GetByte reads a byte from the stream represented by streamOrAlias and unifies it with inByte.
func (vm *VM) GetByte(streamOrAlias, inByte Term, k func(*Env) Promise, env *Env) Promise {
	s, err := vm.stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	if s.source == nil {
		return Error(permissionErrorInputStream(streamOrAlias))
	}

	if s.streamType == streamTypeText {
		return Error(permissionErrorInputTextStream(streamOrAlias))
	}

	switch b := env.Resolve(inByte).(type) {
	case Variable:
		break
	case Integer:
		if b < 0 || b > 255 {
			Error(typeErrorInByte(inByte))
		}
		break
	default:
		return Error(typeErrorInByte(inByte))
	}

	b := make([]byte, 1)
	_, err = s.source.Read(b)
	switch err {
	case nil:
		return Delay(func() Promise {
			env := NewEnv(env)
			return Unify(inByte, Integer(b[0]), k, env)
		})
	case io.EOF:
		switch s.eofAction {
		case eofActionError:
			return Error(permissionErrorInputPastEndOfStream(streamOrAlias))
		case eofActionEOFCode:
			return Delay(func() Promise {
				env := NewEnv(env)
				return Unify(inByte, Integer(-1), k, env)
			})
		case eofActionReset:
			return Delay(func() Promise {
				env := NewEnv(env)
				return vm.GetByte(streamOrAlias, inByte, k, env)
			})
		default:
			return Error(systemError(fmt.Errorf("unknown EOF action: %d", s.eofAction)))
		}
	default:
		return Error(err)
	}
}

// GetChar reads a character from the stream represented by streamOrAlias and unifies it with char.
func (vm *VM) GetChar(streamOrAlias, char Term, k func(*Env) Promise, env *Env) Promise {
	s, err := vm.stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	if s.source == nil {
		return Error(permissionErrorInputStream(streamOrAlias))
	}

	if s.streamType == streamTypeBinary {
		return Error(permissionErrorInputBinaryStream(streamOrAlias))
	}

	br, ok := s.source.(*bufio.Reader)
	if !ok {
		return Error(permissionErrorInputBufferedStream(streamOrAlias))
	}

	switch c := env.Resolve(char).(type) {
	case Variable:
		break
	case Atom:
		if len([]rune(c)) != 1 {
			return Error(typeErrorInCharacter(char))
		}
		break
	default:
		return Error(typeErrorInCharacter(char))
	}

	r, _, err := br.ReadRune()
	switch err {
	case nil:
		if r == unicode.ReplacementChar {
			return Error(representationError(Atom("character"), Atom("invalid character.")))
		}

		return Delay(func() Promise {
			env := NewEnv(env)
			return Unify(char, Atom(r), k, env)
		})
	case io.EOF:
		switch s.eofAction {
		case eofActionError:
			return Error(permissionErrorInputPastEndOfStream(streamOrAlias))
		case eofActionEOFCode:
			return Delay(func() Promise {
				env := NewEnv(env)
				return Unify(char, Atom("end_of_file"), k, env)
			})
		case eofActionReset:
			return Delay(func() Promise {
				env := NewEnv(env)
				return vm.GetChar(streamOrAlias, char, k, env)
			})
		default:
			return Error(systemError(fmt.Errorf("unknown EOF action: %d", s.eofAction)))
		}
	default:
		return Error(systemError(err))
	}
}

// PeekByte peeks a byte from the stream represented by streamOrAlias and unifies it with inByte.
func (vm *VM) PeekByte(streamOrAlias, inByte Term, k func(*Env) Promise, env *Env) Promise {
	s, err := vm.stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	if s.source == nil {
		return Error(permissionErrorInputStream(streamOrAlias))
	}

	if s.streamType == streamTypeText {
		return Error(permissionErrorInputTextStream(streamOrAlias))
	}

	br, ok := s.source.(*bufio.Reader)
	if !ok {
		return Error(permissionErrorInputBufferedStream(streamOrAlias))
	}

	switch b := env.Resolve(inByte).(type) {
	case Variable:
		break
	case Integer:
		if b < 0 || b > 255 {
			return Error(typeErrorInByte(inByte))
		}
		break
	default:
		return Error(typeErrorInByte(inByte))
	}

	b, err := br.Peek(1)
	switch err {
	case nil:
		return Delay(func() Promise {
			env := NewEnv(env)
			return Unify(inByte, Integer(b[0]), k, env)
		})
	case io.EOF:
		switch s.eofAction {
		case eofActionError:
			return Error(permissionErrorInputPastEndOfStream(streamOrAlias))
		case eofActionEOFCode:
			return Delay(func() Promise {
				env := NewEnv(env)
				return Unify(inByte, Integer(-1), k, env)
			})
		case eofActionReset:
			return Delay(func() Promise {
				env := NewEnv(env)
				return vm.PeekByte(streamOrAlias, inByte, k, env)
			})
		default:
			return Error(systemError(fmt.Errorf("unknown EOF action: %d", s.eofAction)))
		}
	default:
		return Error(systemError(err))
	}
}

// PeekChar peeks a rune from the stream represented by streamOrAlias and unifies it with char.
func (vm *VM) PeekChar(streamOrAlias, char Term, k func(*Env) Promise, env *Env) Promise {
	s, err := vm.stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	if s.source == nil {
		return Error(permissionErrorInputStream(streamOrAlias))
	}

	if s.streamType == streamTypeBinary {
		return Error(permissionErrorInputBinaryStream(streamOrAlias))
	}

	br, ok := s.source.(*bufio.Reader)
	if !ok {
		return Error(permissionErrorInputBufferedStream(streamOrAlias))
	}

	switch c := env.Resolve(char).(type) {
	case Variable:
		break
	case Atom:
		if len([]rune(c)) != 1 {
			return Error(typeErrorInCharacter(char))
		}
		break
	default:
		return Error(typeErrorInCharacter(char))
	}

	r, _, err := br.ReadRune()
	switch err {
	case nil:
		if err := br.UnreadRune(); err != nil {
			return Error(systemError(err))
		}

		if r == unicode.ReplacementChar {
			return Error(representationError(Atom("character"), Atom("invalid character.")))
		}

		return Delay(func() Promise {
			env := NewEnv(env)
			return Unify(char, Atom(r), k, env)
		})
	case io.EOF:
		switch s.eofAction {
		case eofActionError:
			return Error(permissionErrorInputPastEndOfStream(streamOrAlias))
		case eofActionEOFCode:
			return Delay(func() Promise {
				env := NewEnv(env)
				return Unify(char, Atom("end_of_file"), k, env)
			})
		case eofActionReset:
			return Delay(func() Promise {
				env := NewEnv(env)
				return vm.PeekChar(streamOrAlias, char, k, env)
			})
		default:
			return Error(systemError(fmt.Errorf("unknown EOF action: %d", s.eofAction)))
		}
	default:
		return Error(systemError(err))
	}
}

var osExit = os.Exit

// Halt exits the process with exit code of n.
func (vm *VM) Halt(n Term, k func(*Env) Promise, env *Env) Promise {
	if vm.OnHalt == nil {
		vm.OnHalt = func() {}
	}
	switch code := env.Resolve(n).(type) {
	case Variable:
		return Error(instantiationError(n))
	case Integer:
		vm.OnHalt()
		osExit(int(code))
		return k(env)
	default:
		return Error(typeErrorInteger(n))
	}
}

// Clause unifies head and body with H and B respectively where H :- B is in the database.
func (vm *VM) Clause(head, body Term, k func(*Env) Promise, env *Env) Promise {
	pi, _, err := piArgs(head, env)
	if err != nil {
		return Error(err)
	}

	switch env.Resolve(body).(type) {
	case Variable, Atom, *Compound:
		break
	default:
		return Error(typeErrorCallable(body))
	}

	cs, _ := vm.procedures[pi].(clauses)
	ks := make([]func() Promise, len(cs))
	for i := range cs {
		r := Rulify(copyTerm(cs[i].raw, nil, env), env)
		ks[i] = func() Promise {
			env := NewEnv(env)
			return Unify(&Compound{
				Functor: ":-",
				Args:    []Term{head, body},
			}, r, k, env)
		}
	}
	return Delay(ks...)
}

// AtomLength counts the runes in atom and unifies the result with length.
func AtomLength(atom, length Term, k func(*Env) Promise, env *Env) Promise {
	switch a := env.Resolve(atom).(type) {
	case Variable:
		return Error(instantiationError(atom))
	case Atom:
		switch l := env.Resolve(length).(type) {
		case Variable:
			break
		case Integer:
			if l < 0 {
				return Error(domainErrorNotLessThanZero(length))
			}
			break
		default:
			return Error(typeErrorInteger(length))
		}

		return Delay(func() Promise {
			env := NewEnv(env)
			return Unify(length, Integer(len([]rune(a))), k, env)
		})
	default:
		return Error(typeErrorAtom(atom))
	}
}

// AtomConcat concatenates atom1 and atom2 and unifies it with atom3.
func AtomConcat(atom1, atom2, atom3 Term, k func(*Env) Promise, env *Env) Promise {
	switch a3 := env.Resolve(atom3).(type) {
	case Variable:
		switch a1 := env.Resolve(atom1).(type) {
		case Variable:
			return Error(instantiationError(&Compound{
				Functor: ",",
				Args:    []Term{atom1, atom3},
			}))
		case Atom:
			switch a2 := env.Resolve(atom2).(type) {
			case Variable:
				return Error(instantiationError(&Compound{
					Functor: ",",
					Args:    []Term{atom2, atom3},
				}))
			case Atom:
				return Delay(func() Promise {
					env := NewEnv(env)
					return Unify(a1+a2, a3, k, env)
				})
			default:
				return Error(typeErrorAtom(atom2))
			}
		default:
			return Error(typeErrorAtom(atom1))
		}
	case Atom:
		switch env.Resolve(atom1).(type) {
		case Variable, Atom:
			break
		default:
			return Error(typeErrorAtom(atom1))
		}

		switch env.Resolve(atom2).(type) {
		case Variable, Atom:
			break
		default:
			return Error(typeErrorAtom(atom2))
		}

		pattern := Compound{Args: []Term{atom1, atom2}}
		ks := make([]func() Promise, 0, len(a3)+1)
		for i := range a3 {
			a1, a2 := a3[:i], a3[i:]
			ks = append(ks, func() Promise {
				env := NewEnv(env)
				return Unify(&pattern, &Compound{Args: []Term{a1, a2}}, k, env)
			})
		}
		ks = append(ks, func() Promise {
			env := NewEnv(env)
			return Unify(&pattern, &Compound{Args: []Term{a3, Atom("")}}, k, env)
		})
		return Delay(ks...)
	default:
		return Error(typeErrorAtom(atom3))
	}
}

// SubAtom unifies subAtom with a sub atom of atom of length which appears with before runes preceding it and after runes following it.
func SubAtom(atom, before, length, after, subAtom Term, k func(*Env) Promise, env *Env) Promise {
	switch whole := env.Resolve(atom).(type) {
	case Variable:
		return Error(instantiationError(atom))
	case Atom:
		rs := []rune(whole)

		switch b := env.Resolve(before).(type) {
		case Variable:
			break
		case Integer:
			if b < 0 {
				return Error(domainErrorNotLessThanZero(before))
			}
			break
		default:
			return Error(typeErrorInteger(before))
		}

		switch l := env.Resolve(length).(type) {
		case Variable:
			break
		case Integer:
			if l < 0 {
				return Error(domainErrorNotLessThanZero(length))
			}
			break
		default:
			return Error(typeErrorInteger(length))
		}

		switch a := env.Resolve(after).(type) {
		case Variable:
			break
		case Integer:
			if a < 0 {
				return Error(domainErrorNotLessThanZero(after))
			}
			break
		default:
			return Error(typeErrorInteger(after))
		}

		switch env.Resolve(subAtom).(type) {
		case Variable, Atom:
			break
		default:
			return Error(typeErrorAtom(subAtom))
		}

		pattern := Compound{Args: []Term{before, length, after, subAtom}}
		var ks []func() Promise
		for i := 0; i <= len(rs); i++ {
			for j := i; j <= len(rs); j++ {
				before, length, after, subAtom := Integer(i), Integer(j-i), Integer(len(rs)-j), Atom(rs[i:j])
				ks = append(ks, func() Promise {
					env := NewEnv(env)
					return Unify(&pattern, &Compound{Args: []Term{before, length, after, subAtom}}, k, env)
				})
			}
		}
		return Delay(ks...)
	default:
		return Error(typeErrorAtom(atom))
	}
}

// AtomChars breaks down atom into list of characters and unifies with chars, or constructs an atom from a list of
// characters chars and unifies it with atom.
func AtomChars(atom, chars Term, k func(*Env) Promise, env *Env) Promise {
	switch a := env.Resolve(atom).(type) {
	case Variable:
		var sb strings.Builder
		if err := Each(env.Resolve(chars), func(elem Term) error {
			switch e := env.Resolve(elem).(type) {
			case Variable:
				return instantiationError(elem)
			case Atom:
				if len([]rune(e)) != 1 {
					return typeErrorCharacter(elem)
				}
				if _, err := sb.WriteString(string(e)); err != nil {
					return systemError(err)
				}
				return nil
			default:
				return typeErrorCharacter(elem)
			}
		}, env); err != nil {
			return Error(err)
		}
		return Delay(func() Promise {
			env := NewEnv(env)
			return Unify(atom, Atom(sb.String()), k, env)
		})
	case Atom:
		rs := []rune(a)
		cs := make([]Term, len(rs))
		for i, r := range rs {
			cs[i] = Atom(r)
		}
		return Delay(func() Promise {
			env := NewEnv(env)
			return Unify(chars, List(cs...), k, env)
		})
	default:
		return Error(typeErrorAtom(atom))
	}
}

// AtomCodes breaks up atom into a list of runes and unifies it with codes, or constructs an atom from the list of runes
// and unifies it with atom.
func AtomCodes(atom, codes Term, k func(*Env) Promise, env *Env) Promise {
	switch a := env.Resolve(atom).(type) {
	case Variable:
		var sb strings.Builder
		if err := Each(env.Resolve(codes), func(elem Term) error {
			switch e := env.Resolve(elem).(type) {
			case Variable:
				return instantiationError(elem)
			case Integer:
				if _, err := sb.WriteRune(rune(e)); err != nil {
					return systemError(err)
				}
				return nil
			default:
				return representationError(Atom("character_code"), Atom("invalid character code."))
			}
		}, env); err != nil {
			return Error(err)
		}
		return Delay(func() Promise {
			env := NewEnv(env)
			return Unify(atom, Atom(sb.String()), k, env)
		})
	case Atom:
		rs := []rune(a)
		cs := make([]Term, len(rs))
		for i, r := range rs {
			cs[i] = Integer(r)
		}
		return Delay(func() Promise {
			env := NewEnv(env)
			return Unify(codes, List(cs...), k, env)
		})
	default:
		return Error(typeErrorAtom(atom))
	}
}

// NumberChars breaks up an atom representation of a number num into a list of characters and unifies it with chars, or
// constructs a number from a list of characters chars and unifies it with num.
func NumberChars(num, chars Term, k func(*Env) Promise, env *Env) Promise {
	switch n := env.Resolve(num).(type) {
	case Variable:
		var sb strings.Builder
		if err := Each(env.Resolve(chars), func(elem Term) error {
			switch e := env.Resolve(elem).(type) {
			case Variable:
				return instantiationError(elem)
			case Atom:
				if len([]rune(e)) != 1 {
					return typeErrorCharacter(elem)
				}
				if _, err := sb.WriteString(string(e)); err != nil {
					return systemError(err)
				}
				return nil
			default:
				return typeErrorCharacter(elem)
			}
		}, env); err != nil {
			return Error(err)
		}

		var vm VM
		p := NewParser(&vm, bufio.NewReader(strings.NewReader(sb.String())))
		t, err := p.Number()
		if err != nil {
			return Error(err)
		}
		return Delay(func() Promise {
			env := NewEnv(env)
			return Unify(num, t, k, env)
		})
	case Integer, Float:
		var buf bytes.Buffer
		if err := n.WriteTerm(&buf, defaultWriteTermOptions, env); err != nil {
			return Error(err)
		}
		rs := []rune(buf.String())
		cs := make([]Term, len(rs))
		for i, r := range rs {
			cs[i] = Atom(r)
		}
		return Delay(func() Promise {
			env := NewEnv(env)
			return Unify(chars, List(cs...), k, env)
		})
	default:
		return Error(typeErrorNumber(num))
	}
}

// NumberCodes breaks up an atom representation of a number num into a list of runes and unifies it with codes, or
// constructs a number from a list of runes codes and unifies it with num.
func NumberCodes(num, codes Term, k func(*Env) Promise, env *Env) Promise {
	switch n := env.Resolve(num).(type) {
	case Variable:
		var sb strings.Builder
		if err := Each(env.Resolve(codes), func(elem Term) error {
			switch e := env.Resolve(elem).(type) {
			case Variable:
				return instantiationError(elem)
			case Integer:
				if _, err := sb.WriteRune(rune(e)); err != nil {
					return systemError(err)
				}
				return nil
			default:
				return representationError(Atom("character_code"), Atom(fmt.Sprintf("%s is not a valid character code.", elem)))
			}
		}, env); err != nil {
			return Error(err)
		}

		var vm VM
		p := NewParser(&vm, bufio.NewReader(strings.NewReader(sb.String())))
		t, err := p.Number()
		if err != nil {
			return Error(err)
		}
		return Delay(func() Promise {
			env := NewEnv(env)
			return Unify(num, t, k, env)
		})
	case Integer, Float:
		var buf bytes.Buffer
		if err := n.WriteTerm(&buf, defaultWriteTermOptions, env); err != nil {
			return Error(err)
		}
		rs := []rune(buf.String())
		cs := make([]Term, len(rs))
		for i, r := range rs {
			cs[i] = Integer(r)
		}
		return Delay(func() Promise {
			env := NewEnv(env)
			return Unify(codes, List(cs...), k, env)
		})
	default:
		return Error(typeErrorNumber(num))
	}
}

// FunctionSet is a set of unary/binary functions.
type FunctionSet struct {
	Unary  map[Atom]func(x Term, env *Env) (Term, error)
	Binary map[Atom]func(x, y Term, env *Env) (Term, error)
}

// Is evaluates expression and unifies the result with result.
func (fs FunctionSet) Is(result, expression Term, k func(*Env) Promise, env *Env) Promise {
	v, err := fs.eval(expression, env)
	if err != nil {
		return Error(err)
	}
	return Delay(func() Promise {
		env := NewEnv(env)
		return Unify(result, v, k, env)
	})
}

// Equal succeeds iff lhs equals to rhs.
func (fs FunctionSet) Equal(lhs, rhs Term, k func(*Env) Promise, env *Env) Promise {
	return fs.compare(lhs, rhs, k, func(i Integer, j Integer) bool {
		return i == j
	}, func(f Float, g Float) bool {
		return f == g
	}, env)
}

// NotEqual succeeds iff lhs doesn't equal to rhs.
func (fs FunctionSet) NotEqual(lhs, rhs Term, k func(*Env) Promise, env *Env) Promise {
	return fs.compare(lhs, rhs, k, func(i Integer, j Integer) bool {
		return i != j
	}, func(f Float, g Float) bool {
		return f != g
	}, env)
}

// LessThan succeeds iff lhs is less than rhs.
func (fs FunctionSet) LessThan(lhs, rhs Term, k func(*Env) Promise, env *Env) Promise {
	return fs.compare(lhs, rhs, k, func(i Integer, j Integer) bool {
		return i < j
	}, func(f Float, g Float) bool {
		return f < g
	}, env)
}

// GreaterThan succeeds iff lhs is greater than rhs.
func (fs FunctionSet) GreaterThan(lhs, rhs Term, k func(*Env) Promise, env *Env) Promise {
	return fs.compare(lhs, rhs, k, func(i Integer, j Integer) bool {
		return i > j
	}, func(f Float, g Float) bool {
		return f > g
	}, env)
}

// LessThanOrEqual succeeds iff lhs is less than or equal to rhs.
func (fs FunctionSet) LessThanOrEqual(lhs, rhs Term, k func(*Env) Promise, env *Env) Promise {
	return fs.compare(lhs, rhs, k, func(i Integer, j Integer) bool {
		return i <= j
	}, func(f Float, g Float) bool {
		return f <= g
	}, env)
}

// GreaterThanOrEqual succeeds iff lhs is greater than or equal to rhs.
func (fs FunctionSet) GreaterThanOrEqual(lhs, rhs Term, k func(*Env) Promise, env *Env) Promise {
	return fs.compare(lhs, rhs, k, func(i Integer, j Integer) bool {
		return i >= j
	}, func(f Float, g Float) bool {
		return f >= g
	}, env)
}

func (fs FunctionSet) compare(lhs, rhs Term, k func(*Env) Promise, pi func(Integer, Integer) bool, pf func(Float, Float) bool, env *Env) Promise {
	l, err := fs.eval(lhs, env)
	if err != nil {
		return Error(err)
	}

	r, err := fs.eval(rhs, env)
	if err != nil {
		return Error(err)
	}

	switch l := l.(type) {
	case Integer:
		switch r := r.(type) {
		case Integer:
			if !pi(l, r) {
				return Bool(false)
			}
			return k(env)
		case Float:
			if !pf(Float(l), r) {
				return Bool(false)
			}
			return k(env)
		default:
			return Error(typeErrorEvaluable(r))
		}
	case Float:
		switch r := r.(type) {
		case Integer:
			if !pf(l, Float(r)) {
				return Bool(false)
			}
			return k(env)
		case Float:
			if !pf(l, r) {
				return Bool(false)
			}
			return k(env)
		default:
			return Error(typeErrorEvaluable(r))
		}
	default:
		return Error(typeErrorEvaluable(l))
	}
}

func (fs FunctionSet) eval(expression Term, env *Env) (_ Term, err error) {
	defer func() {
		if r := recover(); r != nil {
			if e, ok := r.(error); ok {
				if e.Error() == "runtime error: integer divide by zero" {
					err = evaluationErrorZeroDivisor()
					return
				}
			}
			panic(r)
		}
	}()

	switch t := env.Resolve(expression).(type) {
	case Variable:
		return nil, instantiationError(expression)
	case Atom:
		return nil, typeErrorEvaluable(expression) // TODO: constants?
	case Integer, Float:
		return t, nil
	case *Compound:
		switch len(t.Args) {
		case 1:
			f, ok := fs.Unary[t.Functor]
			if !ok {
				return nil, typeErrorEvaluable(expression)
			}
			x, err := fs.eval(t.Args[0], env)
			if err != nil {
				return nil, err
			}
			return f(x, env)
		case 2:
			f, ok := fs.Binary[t.Functor]
			if !ok {
				return nil, typeErrorEvaluable(expression)
			}
			x, err := fs.eval(t.Args[0], env)
			if err != nil {
				return nil, err
			}
			y, err := fs.eval(t.Args[1], env)
			if err != nil {
				return nil, err
			}
			return f(x, y, env)
		default:
			return nil, typeErrorEvaluable(expression)
		}
	default:
		return nil, typeErrorEvaluable(expression)
	}
}

// DefaultFunctionSet is a FunctionSet with builtin functions.
var DefaultFunctionSet = FunctionSet{
	Unary: map[Atom]func(Term, *Env) (Term, error){
		"-":        unaryNumber(func(i int64) int64 { return -1 * i }, func(n float64) float64 { return -1 * n }),
		"abs":      unaryFloat(math.Abs),
		"atan":     unaryFloat(math.Atan),
		"ceiling":  unaryFloat(math.Ceil),
		"cos":      unaryFloat(math.Cos),
		"exp":      unaryFloat(math.Exp),
		"sqrt":     unaryFloat(math.Sqrt),
		"sign":     unaryNumber(sgn, sgnf),
		"float":    unaryFloat(func(n float64) float64 { return n }),
		"floor":    unaryFloat(math.Floor),
		"log":      unaryFloat(math.Log),
		"sin":      unaryFloat(math.Sin),
		"truncate": unaryFloat(math.Trunc),
		"round":    unaryFloat(math.Round),
		"\\":       unaryInteger(func(i int64) int64 { return ^i }),
	},
	Binary: map[Atom]func(Term, Term, *Env) (Term, error){
		"+":   binaryNumber(func(i, j int64) int64 { return i + j }, func(n, m float64) float64 { return n + m }),
		"-":   binaryNumber(func(i, j int64) int64 { return i - j }, func(n, m float64) float64 { return n - m }),
		"*":   binaryNumber(func(i, j int64) int64 { return i * j }, func(n, m float64) float64 { return n * m }),
		"/":   binaryFloat(func(n float64, m float64) float64 { return n / m }),
		"//":  binaryInteger(func(i, j int64) int64 { return i / j }),
		"rem": binaryInteger(func(i, j int64) int64 { return i % j }),
		"mod": binaryInteger(func(i, j int64) int64 { return (i%j + j) % j }),
		"**":  binaryFloat(math.Pow),
		">>":  binaryInteger(func(i, j int64) int64 { return i >> j }),
		"<<":  binaryInteger(func(i, j int64) int64 { return i << j }),
		"/\\": binaryInteger(func(i, j int64) int64 { return i & j }),
		"\\/": binaryInteger(func(i, j int64) int64 { return i | j }),
	},
}

func sgn(i int64) int64 {
	return i>>63 | int64(uint64(-i)>>63)
}

func sgnf(f float64) float64 {
	switch {
	case f < 0:
		return -1
	case f == 0:
		return 0
	case f > 0:
		return 1
	default: // NaN
		return f
	}
}

func unaryInteger(f func(i int64) int64) func(Term, *Env) (Term, error) {
	return func(x Term, env *Env) (Term, error) {
		i, ok := env.Resolve(x).(Integer)
		if !ok {
			return nil, typeErrorInteger(x)
		}

		return Integer(f(int64(i))), nil
	}
}

func binaryInteger(f func(i, j int64) int64) func(Term, Term, *Env) (Term, error) {
	return func(x, y Term, env *Env) (Term, error) {
		i, ok := env.Resolve(x).(Integer)
		if !ok {
			return nil, typeErrorInteger(x)
		}

		j, ok := env.Resolve(y).(Integer)
		if !ok {
			return nil, typeErrorInteger(y)
		}

		return Integer(f(int64(i), int64(j))), nil
	}
}

func unaryFloat(f func(n float64) float64) func(Term, *Env) (Term, error) {
	return func(x Term, env *Env) (Term, error) {
		switch x := env.Resolve(x).(type) {
		case Integer:
			return Float(f(float64(x))), nil
		case Float:
			return Float(f(float64(x))), nil
		default:
			return nil, typeErrorEvaluable(x)
		}
	}
}

func binaryFloat(f func(n float64, m float64) float64) func(Term, Term, *Env) (Term, error) {
	return func(x, y Term, env *Env) (Term, error) {
		switch x := env.Resolve(x).(type) {
		case Integer:
			switch y := env.Resolve(y).(type) {
			case Integer:
				return Float(f(float64(x), float64(y))), nil
			case Float:
				return Float(f(float64(x), float64(y))), nil
			default:
				return nil, typeErrorEvaluable(y)
			}
		case Float:
			switch y := env.Resolve(y).(type) {
			case Integer:
				return Float(f(float64(x), float64(y))), nil
			case Float:
				return Float(f(float64(x), float64(y))), nil
			default:
				return nil, typeErrorEvaluable(y)
			}
		default:
			return nil, typeErrorEvaluable(x)
		}
	}
}

func unaryNumber(fi func(i int64) int64, ff func(n float64) float64) func(Term, *Env) (Term, error) {
	return func(x Term, env *Env) (Term, error) {
		switch x := env.Resolve(x).(type) {
		case Integer:
			return Integer(fi(int64(x))), nil
		case Float:
			return Float(ff(float64(x))), nil
		default:
			return nil, typeErrorEvaluable(x)
		}
	}
}

func binaryNumber(fi func(i, j int64) int64, ff func(n, m float64) float64) func(Term, Term, *Env) (Term, error) {
	return func(x, y Term, env *Env) (Term, error) {
		switch x := env.Resolve(x).(type) {
		case Integer:
			switch y := env.Resolve(y).(type) {
			case Integer:
				return Integer(fi(int64(x), int64(y))), nil
			case Float:
				return Float(ff(float64(x), float64(y))), nil
			default:
				return nil, typeErrorEvaluable(y)
			}
		case Float:
			switch y := env.Resolve(y).(type) {
			case Integer:
				return Float(ff(float64(x), float64(y))), nil
			case Float:
				return Float(ff(float64(x), float64(y))), nil
			default:
				return nil, typeErrorEvaluable(y)
			}
		default:
			return nil, typeErrorEvaluable(x)
		}
	}
}

// StreamProperty succeeds iff the stream represented by streamOrAlias has the stream property property.
func (vm *VM) StreamProperty(streamOrAlias, property Term, k func(*Env) Promise, env *Env) Promise {
	streams := make([]*Stream, 0, len(vm.streams))
	switch s := env.Resolve(streamOrAlias).(type) {
	case Variable:
		for _, v := range vm.streams {
			streams = append(streams, v)
		}
	case Atom: // ISO standard stream_property/2 doesn't take an alias but why not?
		v, ok := vm.streams[s]
		if !ok {
			return Error(existenceErrorStream(streamOrAlias))
		}
		streams = append(streams, v)
	case *Stream:
		streams = append(streams, s)
	default:
		return Error(domainErrorStreamOrAlias(streamOrAlias))
	}

	switch p := env.Resolve(property).(type) {
	case Variable:
		break
	case Atom:
		switch p {
		case "input", "output":
			break
		default:
			return Error(domainErrorStreamProperty(property))
		}
	case *Compound:
		if len(p.Args) != 1 {
			return Error(domainErrorStreamProperty(property))
		}
		arg := p.Args[0]
		switch p.Functor {
		case "file_name", "mode", "alias", "end_of_stream", "eof_action", "reposition":
			switch env.Resolve(arg).(type) {
			case Variable, Atom:
				break
			default:
				return Error(typeErrorAtom(arg))
			}
		case "position":
			if len(p.Args) != 1 {
				return Error(domainErrorStreamProperty(property))
			}
			switch env.Resolve(p.Args[0]).(type) {
			case Variable, Integer:
				break
			default:
				return Error(typeErrorAtom(arg))
			}
		default:
			return Error(domainErrorStreamProperty(property))
		}
	default:
		return Error(domainErrorStreamProperty(property))
	}

	var ks []func() Promise
	for _, s := range streams {
		var properties []Term

		switch s.mode {
		case streamModeRead:
			properties = append(properties, &Compound{Functor: "mode", Args: []Term{Atom("read")}})
		case streamModeWrite:
			properties = append(properties, &Compound{Functor: "mode", Args: []Term{Atom("write")}})
		case streamModeAppend:
			properties = append(properties, &Compound{Functor: "mode", Args: []Term{Atom("append")}})
		}

		if s.alias != "" {
			properties = append(properties, &Compound{Functor: "alias", Args: []Term{s.alias}})
		}

		switch s.eofAction {
		case eofActionError:
			properties = append(properties, &Compound{Functor: "eof_action", Args: []Term{Atom("error")}})
		case eofActionEOFCode:
			properties = append(properties, &Compound{Functor: "eof_action", Args: []Term{Atom("eof_code")}})
		case eofActionReset:
			properties = append(properties, &Compound{Functor: "eof_action", Args: []Term{Atom("reset")}})
		}

		if s.source != nil {
			properties = append(properties, Atom("input"))
			if _, ok := s.source.(*bufio.Reader); ok {
				properties = append(properties, &Compound{Functor: "buffer", Args: []Term{Atom("true")}})
			} else {
				properties = append(properties, &Compound{Functor: "buffer", Args: []Term{Atom("false")}})
			}
		}

		if s.sink != nil {
			properties = append(properties, Atom("output"))
			if _, ok := s.sink.(*bufio.Writer); ok {
				properties = append(properties, &Compound{Functor: "buffer", Args: []Term{Atom("true")}})
			} else {
				properties = append(properties, &Compound{Functor: "buffer", Args: []Term{Atom("false")}})
			}
		}

		if f, ok := s.closer.(*os.File); ok {
			pos, err := f.Seek(0, 1)
			if err != nil {
				return Error(err)
			}
			if br, ok := s.source.(*bufio.Reader); ok {
				pos -= int64(br.Buffered())
			}

			fi, err := f.Stat()
			if err != nil {
				return Error(err)
			}

			eos := "not"
			switch {
			case pos == fi.Size():
				eos = "at"
			case pos > fi.Size():
				eos = "past"
			}

			properties = append(properties,
				&Compound{Functor: "file_name", Args: []Term{Atom(f.Name())}},
				&Compound{Functor: "position", Args: []Term{Integer(pos)}},
				&Compound{Functor: "end_of_stream", Args: []Term{Atom(eos)}},
			)
		}

		if s.reposition {
			properties = append(properties, &Compound{Functor: "reposition", Args: []Term{Atom("true")}})
		} else {
			properties = append(properties, &Compound{Functor: "reposition", Args: []Term{Atom("false")}})
		}

		switch s.streamType {
		case streamTypeText:
			properties = append(properties, &Compound{Functor: "type", Args: []Term{Atom("text")}})
		case streamTypeBinary:
			properties = append(properties, &Compound{Functor: "type", Args: []Term{Atom("false")}})
		}

		for i := range properties {
			p := properties[i]
			ks = append(ks, func() Promise {
				env := NewEnv(env)
				return Unify(property, p, k, env)
			})
		}
	}
	return Delay(ks...)
}

// SetStreamPosition sets the position property of the stream represented by streamOrAlias.
func (vm *VM) SetStreamPosition(streamOrAlias, position Term, k func(*Env) Promise, env *Env) Promise {
	s, err := vm.stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	switch p := env.Resolve(position).(type) {
	case Variable:
		return Error(instantiationError(position))
	case Integer:
		f, ok := s.closer.(*os.File)
		if !ok {
			return Error(permissionError(Atom("reposition"), Atom("stream"), streamOrAlias, Atom(fmt.Sprintf("%s is not a file.", streamOrAlias))))
		}

		if _, err := f.Seek(int64(p), 0); err != nil {
			return Error(systemError(err))
		}

		if br, ok := s.source.(*bufio.Reader); ok {
			br.Reset(f)
		}

		return k(env)
	default:
		return Error(typeErrorInteger(position))
	}
}

// CharConversion registers a character conversion from inChar to outChar, or remove the conversion if inChar = outChar.
func (vm *VM) CharConversion(inChar, outChar Term, k func(*Env) Promise, env *Env) Promise {
	switch in := env.Resolve(inChar).(type) {
	case Variable:
		return Error(instantiationError(inChar))
	case Atom:
		i := []rune(in)
		if len(i) != 1 {
			return Error(representationError(Atom("character"), Atom(fmt.Sprintf("%s is not a character.", inChar))))
		}

		switch out := env.Resolve(outChar).(type) {
		case Variable:
			return Error(instantiationError(outChar))
		case Atom:
			o := []rune(out)
			if len(o) != 1 {
				return Error(representationError(Atom("character"), Atom(fmt.Sprintf("%s is not a character.", outChar))))
			}

			if vm.charConversions == nil {
				vm.charConversions = map[rune]rune{}
			}
			if i[0] == o[0] {
				delete(vm.charConversions, i[0])
				return k(env)
			}
			vm.charConversions[i[0]] = o[0]
			return k(env)
		default:
			return Error(representationError(Atom("character"), Atom(fmt.Sprintf("%s is not a character.", outChar))))
		}
	default:
		return Error(representationError(Atom("character"), Atom(fmt.Sprintf("%s is not a character.", inChar))))
	}
}

// CurrentCharConversion succeeds iff a conversion from inChar to outChar is defined.
func (vm *VM) CurrentCharConversion(inChar, outChar Term, k func(*Env) Promise, env *Env) Promise {
	switch in := env.Resolve(inChar).(type) {
	case Variable:
		break
	case Atom:
		i := []rune(in)
		if len(i) != 1 {
			return Error(representationError(Atom("character"), Atom(fmt.Sprintf("%s is not a character.", inChar))))
		}
	default:
		return Error(representationError(Atom("character"), Atom(fmt.Sprintf("%s is not a character.", inChar))))
	}

	switch out := env.Resolve(outChar).(type) {
	case Variable:
		break
	case Atom:
		o := []rune(out)
		if len(o) != 1 {
			return Error(representationError(Atom("character"), Atom(fmt.Sprintf("%s is not a character.", outChar))))
		}
	default:
		return Error(representationError(Atom("character"), Atom(fmt.Sprintf("%s is not a character.", outChar))))
	}

	if c1, ok := env.Resolve(inChar).(Atom); ok {
		r := []rune(c1)
		if r, ok := vm.charConversions[r[0]]; ok {
			return Delay(func() Promise {
				env := NewEnv(env)
				return Unify(outChar, Atom(r), k, env)
			})
		}
		return Delay(func() Promise {
			env := NewEnv(env)
			return Unify(outChar, c1, k, env)
		})
	}

	pattern := Compound{Args: []Term{inChar, outChar}}
	ks := make([]func() Promise, 256)
	for i := 0; i < 256; i++ {
		r := rune(i)
		cr, ok := vm.charConversions[r]
		if !ok {
			cr = r
		}

		ks[i] = func() Promise {
			env := NewEnv(env)
			return Unify(&pattern, &Compound{Args: []Term{Atom(r), Atom(cr)}}, k, env)
		}
	}
	return Delay(ks...)
}

// SetPrologFlag sets flag to value.
func (vm *VM) SetPrologFlag(flag, value Term, k func(*Env) Promise, env *Env) Promise {
	switch f := env.Resolve(flag).(type) {
	case Variable:
		return Error(instantiationError(flag))
	case Atom:
		switch f {
		case "bounded", "max_integer", "min_integer", "integer_rounding_function", "max_arity":
			return Error(permissionError(Atom("modify"), Atom("flag"), f, Atom(fmt.Sprintf("%s is not modifiable.", f))))
		case "char_conversion":
			switch a := env.Resolve(value).(type) {
			case Variable:
				return Error(instantiationError(value))
			case Atom:
				switch a {
				case "on":
					vm.charConvEnabled = true
					return k(env)
				case "off":
					vm.charConvEnabled = false
					return k(env)
				default:
					return Error(domainErrorFlagValue(&Compound{
						Functor: "+",
						Args:    []Term{flag, value},
					}))
				}
			default:
				return Error(domainErrorFlagValue(&Compound{
					Functor: "+",
					Args:    []Term{flag, value},
				}))
			}
		case "debug":
			switch a := env.Resolve(value).(type) {
			case Variable:
				return Error(instantiationError(value))
			case Atom:
				switch a {
				case "on":
					vm.debug = true
					return k(env)
				case "off":
					vm.debug = false
					return k(env)
				default:
					return Error(domainErrorFlagValue(&Compound{
						Functor: "+",
						Args:    []Term{flag, value},
					}))
				}
			default:
				return Error(domainErrorFlagValue(&Compound{
					Functor: "+",
					Args:    []Term{flag, value},
				}))
			}
		case "unknown":
			switch a := env.Resolve(value).(type) {
			case Variable:
				return Error(instantiationError(value))
			case Atom:
				switch a {
				case "error":
					vm.unknown = unknownError
					return k(env)
				case "warning":
					vm.unknown = unknownWarning
					return k(env)
				case "fail":
					vm.unknown = unknownFail
					return k(env)
				default:
					return Error(domainErrorFlagValue(&Compound{
						Functor: "+",
						Args:    []Term{flag, value},
					}))
				}
			default:
				return Error(domainErrorFlagValue(&Compound{
					Functor: "+",
					Args:    []Term{flag, value},
				}))
			}
		default:
			return Error(domainErrorPrologFlag(flag))
		}
	default:
		return Error(typeErrorAtom(flag))
	}
}

// CurrentPrologFlag succeeds iff flag is set to value.
func (vm *VM) CurrentPrologFlag(flag, value Term, k func(*Env) Promise, env *Env) Promise {
	switch f := env.Resolve(flag).(type) {
	case Variable:
		break
	case Atom:
		switch f {
		case "bounded", "max_integer", "min_integer", "integer_rounding_function", "char_conversion", "debug", "max_arity", "unknown":
			break
		default:
			return Error(domainErrorPrologFlag(flag))
		}
	default:
		return Error(typeErrorAtom(flag))
	}

	pattern := Compound{Args: []Term{flag, value}}
	flags := []Term{
		&Compound{Args: []Term{Atom("bounded"), Atom("true")}},
		&Compound{Args: []Term{Atom("max_integer"), Integer(math.MaxInt64)}},
		&Compound{Args: []Term{Atom("min_integer"), Integer(math.MinInt64)}},
		&Compound{Args: []Term{Atom("integer_rounding_function"), Atom("toward_zero")}},
		&Compound{Args: []Term{Atom("char_conversion"), onOff(vm.charConvEnabled)}},
		&Compound{Args: []Term{Atom("debug"), onOff(vm.debug)}},
		&Compound{Args: []Term{Atom("max_arity"), Atom("unbounded")}},
		&Compound{Args: []Term{Atom("unknown"), Atom(vm.unknown.String())}},
	}
	ks := make([]func() Promise, len(flags))
	for i := range flags {
		f := flags[i]
		ks[i] = func() Promise {
			env := NewEnv(env)
			return Unify(&pattern, f, k, env)
		}
	}
	return Delay(ks...)
}

func onOff(b bool) Atom {
	if b {
		return "on"
	}
	return "off"
}

func (vm *VM) stream(streamOrAlias Term, env *Env) (*Stream, error) {
	switch s := env.Resolve(streamOrAlias).(type) {
	case Variable:
		return nil, instantiationError(streamOrAlias)
	case Atom:
		v, ok := vm.streams[s]
		if !ok {
			return nil, existenceErrorStream(streamOrAlias)
		}
		return v, nil
	case *Stream:
		return s, nil
	default:
		return nil, domainErrorStreamOrAlias(streamOrAlias)
	}
}

func (vm *VM) Dynamic(pi Term, k func(*Env) Promise, env *Env) Promise {
	switch p := env.Resolve(pi).(type) {
	case Variable:
		return Error(instantiationError(pi))
	case *Compound:
		if p.Functor != "/" || len(p.Args) != 2 {
			return Error(typeErrorPredicateIndicator(pi))
		}
		switch f := env.Resolve(p.Args[0]).(type) {
		case Variable:
			return Error(instantiationError(pi))
		case Atom:
			switch a := env.Resolve(p.Args[1]).(type) {
			case Variable:
				return Error(instantiationError(pi))
			case Integer:
				pi := procedureIndicator{name: f, arity: a}
				p, ok := vm.procedures[pi]
				if !ok {
					vm.procedures[pi] = clauses{}
					return k(env)
				}
				if _, ok := p.(clauses); !ok {
					return Bool(false)
				}
				return k(env)
			default:
				return Error(typeErrorPredicateIndicator(pi))
			}
		default:
			return Error(typeErrorPredicateIndicator(pi))
		}
	default:
		return Error(typeErrorPredicateIndicator(pi))
	}
}
