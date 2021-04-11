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

	"github.com/ichiban/prolog/nondet"
)

// Call executes goal. it succeeds if goal followed by k succeeds. A cut inside goal doesn't affect outside of Call.
func (vm *VM) Call(goal Term, k nondet.Promise) nondet.Promise {
	pi, args, err := piArgs(goal)
	if err != nil {
		return nondet.Error(err)
	}

	// Force() to restrict the scope of cut.
	ok, err := vm.arrive(pi, args, k).Force()
	if err != nil {
		return nondet.Error(err)
	}
	return nondet.Bool(ok)
}

// Unify unifies t1 and t2 without occurs check (i.e., X = f(X) is allowed).
func Unify(t1, t2 Term, k nondet.Promise) nondet.Promise {
	fvs := FreeVariables(t1, t2)
	if !t1.Unify(t2, false) {
		ResetVariables(fvs...)
		return nondet.Bool(false)
	}
	return k
}

// UnifyWithOccursCheck unifies t1 and t2 with occurs check (i.e., X = f(X) is not allowed).
func UnifyWithOccursCheck(t1, t2 Term, k nondet.Promise) nondet.Promise {
	fvs := FreeVariables(t1, t2)
	if !t1.Unify(t2, true) {
		ResetVariables(fvs...)
		return nondet.Bool(false)
	}
	return k
}

// TypeVar checks if t is a variable.
func TypeVar(t Term, k nondet.Promise) nondet.Promise {
	if _, ok := Resolve(t).(*Variable); !ok {
		return nondet.Bool(false)
	}
	return k
}

// TypeFloat checks if t is a floating-point number.
func TypeFloat(t Term, k nondet.Promise) nondet.Promise {
	if _, ok := Resolve(t).(Float); !ok {
		return nondet.Bool(false)
	}
	return k
}

// TypeInteger checks if t is an integer.
func TypeInteger(t Term, k nondet.Promise) nondet.Promise {
	if _, ok := Resolve(t).(Integer); !ok {
		return nondet.Bool(false)
	}
	return k
}

// TypeAtom checks if t is an atom.
func TypeAtom(t Term, k nondet.Promise) nondet.Promise {
	if _, ok := Resolve(t).(Atom); !ok {
		return nondet.Bool(false)
	}
	return k
}

// TypeCompound checks if t is a compound term.
func TypeCompound(t Term, k nondet.Promise) nondet.Promise {
	if _, ok := Resolve(t).(*Compound); !ok {
		return nondet.Bool(false)
	}
	return k
}

// Functor extracts the name and arity of term, or unifies term with an atomic/compound term of name and arity with
// fresh variables as arguments.
func Functor(term, name, arity Term, k nondet.Promise) nondet.Promise {
	term = Resolve(term)
	switch t := Resolve(term).(type) {
	case *Variable:
		break
	case *Compound:
		pattern := Compound{Args: []Term{name, arity}}
		return nondet.Delay(func() nondet.Promise {
			return Unify(&pattern, &Compound{Args: []Term{t.Functor, Integer(len(t.Args))}}, k)
		})
	default: // atomic
		pattern := Compound{Args: []Term{name, arity}}
		return nondet.Delay(func() nondet.Promise {
			return Unify(&pattern, &Compound{Args: []Term{t, Integer(0)}}, k)
		})
	}

	t := term.(*Variable)

	a, ok := Resolve(arity).(Integer)
	if !ok {
		return nondet.Error(typeErrorInteger(arity))
	}
	switch {
	case a < 0:
		return nondet.Error(domainErrorNotLessThanZero(a))
	case a == 0:
		return Unify(t, name, k)
	}

	n, ok := Resolve(name).(Atom)
	if !ok {
		return nondet.Error(typeErrorAtom(name))
	}

	vs := make([]Term, a)
	for i := range vs {
		vs[i] = &Variable{}
	}
	return nondet.Delay(func() nondet.Promise {
		return Unify(t, &Compound{
			Functor: n,
			Args:    vs,
		}, k)
	})
}

// Arg extracts nth argument of term as arg, or finds the argument position of arg in term as nth.
func Arg(nth, term, arg Term, k nondet.Promise) nondet.Promise {
	t, ok := Resolve(term).(*Compound)
	if !ok {
		return nondet.Error(typeErrorCompound(term))
	}

	switch n := Resolve(nth).(type) {
	case *Variable:
		pattern := Compound{Args: []Term{n, arg}}
		fvs := FreeVariables(n, term, arg)
		ks := make([]func() nondet.Promise, len(t.Args))
		for i := range t.Args {
			n := Integer(i + 1)
			arg := t.Args[i]
			ks[i] = func() nondet.Promise {
				ResetVariables(fvs...)
				return Unify(&pattern, &Compound{Args: []Term{n, arg}}, k)
			}
		}
		return nondet.Delay(ks...)
	case Integer:
		if n == 0 || int(n) >= len(t.Args) {
			return nondet.Bool(false)
		}
		if n < 0 {
			return nondet.Error(domainErrorNotLessThanZero(n))
		}
		return nondet.Delay(func() nondet.Promise {
			return Unify(arg, t.Args[int(n)-1], k)
		})
	default:
		return nondet.Error(typeErrorInteger(n))
	}
}

// Univ constructs list as a list which first element is the functor of term and the rest is the arguments of term, or construct a compound from list as term.
func Univ(term, list Term, k nondet.Promise) nondet.Promise {
	switch t := Resolve(term).(type) {
	case *Variable:
		list = Resolve(list)
		if list == Atom("[]") {
			return nondet.Error(domainErrorNotEmptyList(list))
		}
		cons, ok := list.(*Compound)
		if !ok || cons.Functor != "." || len(cons.Args) != 2 {
			return nondet.Error(typeErrorList(list))
		}

		f, ok := cons.Args[0].(Atom)
		if !ok {
			return nondet.Error(typeErrorAtom(cons.Args[0]))
		}

		var args []Term
		if err := Each(cons.Args[1], func(elem Term) error {
			args = append(args, elem)
			return nil
		}); err != nil {
			return nondet.Error(err)
		}

		return nondet.Delay(func() nondet.Promise {
			return Unify(term, &Compound{
				Functor: f,
				Args:    args,
			}, k)
		})
	case *Compound:
		return nondet.Delay(func() nondet.Promise {
			return Unify(list, List(append([]Term{t.Functor}, t.Args...)...), k)
		})
	default:
		return nondet.Delay(func() nondet.Promise {
			return Unify(list, List(t), k)
		})
	}
}

// CopyTerm clones in as out.
func CopyTerm(in, out Term, k nondet.Promise) nondet.Promise {
	return Unify(in.Copy(), out, k)
}

// Op defines operator with priority and specifier, or removes when priority is 0.
func (vm *VM) Op(priority, specifier, operator Term, k nondet.Promise) nondet.Promise {
	p, ok := Resolve(priority).(Integer)
	if !ok {
		return nondet.Error(typeErrorInteger(priority))
	}
	if p < 0 || p > 1200 {
		return nondet.Error(domainErrorOperatorPriority(priority))
	}

	s, ok := Resolve(specifier).(Atom)
	if !ok {
		return nondet.Error(typeErrorAtom(specifier))
	}
	switch s {
	case "xf", "yf", "xfx", "xfy", "yfx", "fx", "fy":
		break
	default:
		return nondet.Error(domainErrorOperatorSpecifier(s))
	}

	o, ok := Resolve(operator).(Atom)
	if !ok {
		return nondet.Error(typeErrorAtom(operator))
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
			return k
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

	return k
}

// CurrentOp succeeds if operator is defined with priority and specifier.
func (vm *VM) CurrentOp(priority, specifier, operator Term, k nondet.Promise) nondet.Promise {
	switch p := Resolve(priority).(type) {
	case *Variable:
		break
	case Integer:
		if p < 0 || p > 1200 {
			return nondet.Error(domainErrorOperatorPriority(priority))
		}
		break
	default:
		return nondet.Error(domainErrorOperatorPriority(priority))
	}

	switch s := Resolve(specifier).(type) {
	case *Variable:
		break
	case Atom:
		switch s {
		case "xf", "yf", "xfx", "xfy", "yfx", "fx", "fy":
			break
		default:
			return nondet.Error(domainErrorOperatorSpecifier(s))
		}
	default:
		return nondet.Error(domainErrorOperatorSpecifier(s))
	}

	switch Resolve(operator).(type) {
	case *Variable, Atom:
		break
	default:
		return nondet.Error(typeErrorAtom(operator))
	}

	pattern := Compound{Args: []Term{priority, specifier, operator}}
	fvs := FreeVariables(priority, specifier, operator)
	ks := make([]func() nondet.Promise, len(vm.operators))
	for i := range vm.operators {
		op := vm.operators[i]
		ks[i] = func() nondet.Promise {
			ResetVariables(fvs...)
			return Unify(&pattern, &Compound{Args: []Term{op.Priority, op.Specifier, op.Name}}, k)
		}
	}
	return nondet.Delay(ks...)
}

// Assertz appends t to the database.
func (vm *VM) Assertz(t Term, k nondet.Promise) nondet.Promise {
	return vm.assert(t, k, func(cs clauses, c clause) clauses {
		return append(cs, c)
	})
}

// Asserta prepends t to the database.
func (vm *VM) Asserta(t Term, k nondet.Promise) nondet.Promise {
	return vm.assert(t, k, func(cs clauses, c clause) clauses {
		return append(clauses{c}, cs...)
	})
}

func (vm *VM) assert(t Term, k nondet.Promise, merge func(clauses, clause) clauses) nondet.Promise {
	pi, args, err := piArgs(t)
	if err != nil {
		return nondet.Error(err)
	}

	switch pi {
	case procedureIndicator{name: ":-", arity: 1}: // directive
		name, args, err := piArgs(args.(*Compound).Args[0])
		if err != nil {
			return nondet.Error(err)
		}
		return nondet.Delay(func() nondet.Promise {
			return vm.arrive(name, args, k)
		})
	case procedureIndicator{name: ":-", arity: 2}:
		pi, _, err = piArgs(args.(*Compound).Args[0])
		if err != nil {
			return nondet.Error(err)
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
		return nondet.Error(permissionErrorModifyStaticProcedure(&Compound{
			Functor: "/",
			Args:    []Term{pi.name, pi.arity},
		}))
	}
	c := clause{pf: pi}
	if err := c.compile(t); err != nil {
		return nondet.Error(err)
	}

	vm.procedures[pi] = merge(cs, c)
	return k
}

// Repeat enforces k until it returns true.
func Repeat(k nondet.Promise) nondet.Promise {
	for {
		ok, err := k.Force()
		if err != nil {
			return nondet.Error(err)
		}
		if ok {
			return nondet.Bool(true)
		}
	}
}

// BagOf collects all the solutions of goal as instances, which unify with template. instances may contain duplications.
func (vm *VM) BagOf(template, goal, instances Term, k nondet.Promise) nondet.Promise {
	return vm.collectionOf(template, goal, instances, k, List)
}

// SetOf collects all the solutions of goal as instances, which unify with template. instances don't contain duplications.
func (vm *VM) SetOf(template, goal, instances Term, k nondet.Promise) nondet.Promise {
	return vm.collectionOf(template, goal, instances, k, Set)
}

func (vm *VM) collectionOf(template, goal, instances Term, k nondet.Promise, agg func(...Term) Term) nondet.Promise {
	if _, ok := Resolve(goal).(*Variable); ok {
		return nondet.Error(instantiationError(goal))
	}

	var qualifier, body Variable
	if goal.Unify(&Compound{
		Functor: "^",
		Args:    []Term{&qualifier, &body},
	}, false) {
		goal = body.Ref
	}

	fvs := FreeVariables(goal)

	freeVariables := FreeVariables(template, &qualifier)
	groupingVariables := make([]*Variable, 0, len(fvs))
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
	_, err := vm.Call(goal, nondet.Delay(func() nondet.Promise {
		snapshots := make([]Term, len(groupingVariables))
		for i, v := range groupingVariables {
			snapshots[i] = v.Ref
		}

	solutions:
		for i, s := range solutions {
			for i := range groupingVariables {
				ok, err := Compare(Atom("="), s.snapshots[i], snapshots[i], nondet.Bool(true)).Force()
				if err != nil {
					return nondet.Error(err)
				}
				if !ok {
					continue solutions
				}
			}
			solutions[i].bag = append(s.bag, template.Copy())
			return nondet.Bool(false) // ask for more solutions
		}

		solutions = append(solutions, solution{
			snapshots: snapshots,
			bag:       []Term{template.Copy()},
		})
		return nondet.Bool(false) // ask for more solutions
	})).Force()
	if err != nil {
		return nondet.Error(err)
	}

	ResetVariables(freeVariables...)

	if len(solutions) == 0 {
		return nondet.Bool(false)
	}

	b := FreeVariables(instances)
	ks := make([]func() nondet.Promise, len(solutions))
	for i := range solutions {
		s := solutions[i]
		ks[i] = func() nondet.Promise {
			ResetVariables(b...)

			// revert to snapshot
			for i, v := range groupingVariables {
				v.Ref = s.snapshots[i]
			}

			return Unify(instances, agg(s.bag...), k)
		}
	}
	return nondet.Delay(ks...)
}

// Compare compares term1 and term2 and unifies order with <, =, or >.
func Compare(order, term1, term2 Term, k nondet.Promise) nondet.Promise {
	switch o := Resolve(order).(type) {
	case *Variable:
		break
	case Atom:
		switch o {
		case "<", "=", ">":
			break
		default:
			return nondet.Error(domainErrorOrder(order))
		}
		break
	default:
		return nondet.Error(typeErrorAtom(order))
	}

	d := compare(term1, term2)
	switch {
	case d < 0:
		return Unify(Atom("<"), order, k)
	case d > 0:
		return Unify(Atom(">"), order, k)
	default: // d == 0:
		return Unify(Atom("="), order, k)
	}
}

func compare(a, b Term) int64 {
	a, b = Resolve(a), Resolve(b)
	switch a := a.(type) {
	case *Variable:
		switch b := b.(type) {
		case *Variable:
			return int64(strings.Compare(fmt.Sprintf("%p", a), fmt.Sprintf("%p", b)))
		default:
			return -1
		}
	case Float:
		switch b := b.(type) {
		case *Variable:
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
		case *Variable:
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
		case *Variable, Float, Integer:
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
func Throw(ball Term, _ nondet.Promise) nondet.Promise {
	if _, ok := Resolve(ball).(*Variable); ok {
		return nondet.Error(instantiationError(ball))
	}
	return nondet.Error(&Exception{Term: Resolve(ball).Copy()})
}

// Catch calls goal. If an exception is thrown and unifies with catcher, it calls recover.
func (vm *VM) Catch(goal, catcher, recover Term, k nondet.Promise) nondet.Promise {
	ok, err := vm.Call(goal, k).Force()
	if err != nil {
		if ex, ok := err.(*Exception); ok && catcher.Unify(ex.Term, false) {
			return nondet.Delay(func() nondet.Promise {
				return vm.Call(recover, k)
			})
		}
		return nondet.Error(err)
	}
	return nondet.Bool(ok)
}

// CurrentPredicate matches pi with a predicate indicator of the user-defined procedures in the database.
func (vm *VM) CurrentPredicate(pi Term, k nondet.Promise) nondet.Promise {
	switch pi := Resolve(pi).(type) {
	case *Variable:
		break
	case *Compound:
		if pi.Functor != "/" || len(pi.Args) != 2 {
			return nondet.Error(typeErrorPredicateIndicator(pi))
		}
		if _, ok := Resolve(pi.Args[0]).(Atom); !ok {
			return nondet.Error(typeErrorPredicateIndicator(pi))
		}
		if _, ok := Resolve(pi.Args[1]).(Integer); !ok {
			return nondet.Error(typeErrorPredicateIndicator(pi))
		}
		break
	default:
		return nondet.Error(typeErrorPredicateIndicator(pi))
	}

	fvs := FreeVariables(pi)
	ks := make([]func() nondet.Promise, 0, len(vm.procedures))
	for key := range vm.procedures {
		c := Compound{Functor: "/", Args: []Term{key.name, key.arity}}
		ks = append(ks, func() nondet.Promise {
			ResetVariables(fvs...)
			return Unify(pi, &c, k)
		})
	}
	return nondet.Delay(ks...)
}

// Retract removes a clause which matches with t.
func (vm *VM) Retract(t Term, k nondet.Promise) nondet.Promise {
	t = Rulify(t)

	h := t.(*Compound).Args[0]
	pi, _, err := piArgs(h)
	if err != nil {
		return nondet.Error(err)
	}

	p, ok := vm.procedures[pi]
	if !ok {
		return nondet.Bool(false)
	}

	cs, ok := p.(clauses)
	if !ok {
		return nondet.Error(permissionErrorModifyStaticProcedure(&Compound{
			Functor: "/",
			Args:    []Term{pi.name, pi.arity},
		}))
	}

	updated := make(clauses, 0, len(cs))
	defer func() { vm.procedures[pi] = updated }()

	for i, c := range cs {
		raw := Rulify(c.raw)
		fvs := FreeVariables(raw, t)

		if !t.Unify(raw, false) {
			updated = append(updated, c)
			ResetVariables(fvs...)
			continue
		}

		ok, err := k.Force()
		if err != nil {
			updated = append(updated, cs[i+1:]...)
			ResetVariables(fvs...)
			return nondet.Error(err)
		}
		if ok {
			updated = append(updated, cs[i+1:]...)
			ResetVariables(fvs...)
			return nondet.Bool(true)
		}

		ResetVariables(fvs...)
	}

	return nondet.Bool(false)
}

// Abolish removes the procedure indicated by pi from the database.
func (vm *VM) Abolish(pi Term, k nondet.Promise) nondet.Promise {
	if _, ok := Resolve(pi).(*Variable); ok {
		return nondet.Error(instantiationError(pi))
	}

	c, ok := Resolve(pi).(*Compound)
	if !ok || c.Functor != "/" || len(c.Args) != 2 {
		return nondet.Error(typeErrorPredicateIndicator(pi))
	}

	if _, ok := Resolve(c.Args[0]).(*Variable); ok {
		return nondet.Error(instantiationError(c.Args[0]))
	}

	name, ok := Resolve(c.Args[0]).(Atom)
	if !ok {
		return nondet.Error(typeErrorAtom(c.Args[0]))
	}

	if _, ok := Resolve(c.Args[1]).(*Variable); ok {
		return nondet.Error(instantiationError(c.Args[1]))
	}

	arity, ok := Resolve(c.Args[1]).(Integer)
	if !ok {
		return nondet.Error(typeErrorInteger(c.Args[1]))
	}
	if arity < 0 {
		return nondet.Error(domainErrorNotLessThanZero(c.Args[1]))
	}

	key := procedureIndicator{name: name, arity: arity}
	if _, ok := vm.procedures[key].(clauses); !ok {
		return nondet.Error(permissionErrorModifyStaticProcedure(&Compound{
			Functor: "/",
			Args:    []Term{name, arity},
		}))
	}
	delete(vm.procedures, key)
	return k
}

// CurrentInput unifies stream with the current input stream.
func (vm *VM) CurrentInput(stream Term, k nondet.Promise) nondet.Promise {
	switch Resolve(stream).(type) {
	case *Variable, *Stream:
		break
	default:
		return nondet.Error(domainErrorStream(stream))
	}

	return nondet.Delay(func() nondet.Promise {
		return Unify(stream, vm.input, k)
	})
}

// CurrentOutput unifies stream with the current output stream.
func (vm *VM) CurrentOutput(stream Term, k nondet.Promise) nondet.Promise {
	switch Resolve(stream).(type) {
	case *Variable, *Stream:
		break
	default:
		return nondet.Error(domainErrorStream(stream))
	}

	return nondet.Delay(func() nondet.Promise {
		return Unify(stream, vm.output, k)
	})
}

// SetInput sets streamOrAlias as the current input stream.
func (vm *VM) SetInput(streamOrAlias Term, k nondet.Promise) nondet.Promise {
	s, err := vm.stream(streamOrAlias)
	if err != nil {
		return nondet.Error(err)
	}

	if s.source == nil {
		return nondet.Error(permissionErrorInputStream(streamOrAlias))
	}

	vm.input = s
	return k
}

// SetOutput sets streamOrAlias as the current output stream.
func (vm *VM) SetOutput(streamOrAlias Term, k nondet.Promise) nondet.Promise {
	s, err := vm.stream(streamOrAlias)
	if err != nil {
		return nondet.Error(err)
	}

	if s.sink == nil {
		return nondet.Error(permissionErrorOutputStream(streamOrAlias))
	}

	vm.output = s
	return k
}

// Open opens sourceSink in mode and unifies with stream.
func (vm *VM) Open(sourceSink, mode, stream, options Term, k nondet.Promise) nondet.Promise {
	var n Atom
	switch s := Resolve(sourceSink).(type) {
	case *Variable:
		return nondet.Error(instantiationError(sourceSink))
	case Atom:
		n = s
	default:
		return nondet.Error(domainErrorSourceSink(sourceSink))
	}

	var (
		s Stream

		flag   int
		perm   os.FileMode
		buffer bool
	)
	switch m := Resolve(mode).(type) {
	case *Variable:
		return nondet.Error(instantiationError(mode))
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
			return nondet.Error(domainErrorIOMode(m))
		}
	default:
		return nondet.Error(typeErrorAtom(mode))
	}

	if _, ok := Resolve(stream).(*Variable); !ok {
		return nondet.Error(typeErrorVariable(stream))
	}

	if err := Each(Resolve(options), func(option Term) error {
		switch o := Resolve(option).(type) {
		case *Variable:
			return instantiationError(option)
		case *Compound:
			if len(o.Args) != 1 {
				return domainErrorStreamOption(option)
			}
			arg := o.Args[0]
			switch o.Functor {
			case "type":
				switch t := Resolve(arg).(type) {
				case *Variable:
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
				switch b := Resolve(arg).(type) {
				case *Variable:
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
				switch a := Resolve(arg).(type) {
				case *Variable:
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
				switch a := Resolve(arg).(type) {
				case *Variable:
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
	}); err != nil {
		return nondet.Error(err)
	}

	f, err := os.OpenFile(string(n), flag, perm)
	if err != nil {
		switch {
		case os.IsNotExist(err):
			return nondet.Error(existenceErrorSourceSink(sourceSink))
		case os.IsPermission(err):
			return nondet.Error(permissionError(Atom("open"), Atom("source_sink"), sourceSink, Atom(fmt.Sprintf("%s cannot be opened.", sourceSink))))
		default:
			return nondet.Error(systemError(err))
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

	return nondet.Delay(func() nondet.Promise {
		return Unify(stream, &s, k)
	})
}

// Close closes a stream specified by streamOrAlias.
func (vm *VM) Close(streamOrAlias, options Term, k nondet.Promise) nondet.Promise {
	s, err := vm.stream(streamOrAlias)
	if err != nil {
		return nondet.Error(err)
	}

	var force bool
	if err := Each(Resolve(options), func(option Term) error {
		if _, ok := Resolve(option).(*Variable); ok {
			return instantiationError(option)
		}

		switch {
		case option.Unify(&Compound{Functor: "force", Args: []Term{Atom("false")}}, false):
			force = false
		case option.Unify(&Compound{Functor: "force", Args: []Term{Atom("true")}}, false):
			force = true
		default:
			return domainErrorStreamOption(option)
		}
		return nil
	}); err != nil {
		return nondet.Error(err)
	}

	if err := s.closer.Close(); err != nil && !force {
		return nondet.Error(resourceError(streamOrAlias, Atom(err.Error())))
	}

	if s.alias == "" {
		delete(vm.streams, s)
	} else {
		delete(vm.streams, s.alias)
	}

	return k
}

// FlushOutput sends any buffered output to the stream.
func (vm *VM) FlushOutput(streamOrAlias Term, k nondet.Promise) nondet.Promise {
	s, err := vm.stream(streamOrAlias)
	if err != nil {
		return nondet.Error(err)
	}

	if s.sink == nil {
		return nondet.Error(permissionErrorOutputStream(streamOrAlias))
	}

	type flusher interface {
		Flush() error
	}

	if f, ok := s.sink.(flusher); ok {
		if err := f.Flush(); err != nil {
			return nondet.Error(err)
		}
	}

	return k
}

// WriteTerm outputs term to stream with options.
func (vm *VM) WriteTerm(streamOrAlias, term, options Term, k nondet.Promise) nondet.Promise {
	s, err := vm.stream(streamOrAlias)
	if err != nil {
		return nondet.Error(err)
	}

	if s.sink == nil {
		return nondet.Error(permissionErrorOutputStream(streamOrAlias))
	}

	if s.streamType == streamTypeBinary {
		return nondet.Error(permissionErrorOutputBinaryStream(streamOrAlias))
	}

	opts := WriteTermOptions{Ops: vm.operators}
	if err := Each(Resolve(options), func(option Term) error {
		if _, ok := Resolve(option).(*Variable); ok {
			return instantiationError(option)
		}

		switch {
		case option.Unify(&Compound{Functor: "quoted", Args: []Term{Atom("false")}}, false):
			opts.Quoted = false
		case option.Unify(&Compound{Functor: "quoted", Args: []Term{Atom("true")}}, false):
			opts.Quoted = true
		case option.Unify(&Compound{Functor: "ignore_ops", Args: []Term{Atom("false")}}, false):
			opts.Ops = vm.operators
		case option.Unify(&Compound{Functor: "ignore_ops", Args: []Term{Atom("true")}}, false):
			opts.Ops = nil
		case option.Unify(&Compound{Functor: "numbervars", Args: []Term{Atom("false")}}, false):
			opts.NumberVars = false
		case option.Unify(&Compound{Functor: "numbervars", Args: []Term{Atom("true")}}, false):
			opts.NumberVars = true
		default:
			return domainErrorWriteOption(option)
		}
		return nil
	}); err != nil {
		return nondet.Error(err)
	}

	if err := Resolve(term).WriteTerm(s.sink, opts); err != nil {
		return nondet.Error(err)
	}

	return k
}

// CharCode converts a single-rune Atom char to an Integer code, or vice versa.
func CharCode(char, code Term, k nondet.Promise) nondet.Promise {
	switch ch := Resolve(char).(type) {
	case *Variable:
		switch cd := Resolve(code).(type) {
		case *Variable:
			return nondet.Error(instantiationError(&Compound{
				Functor: ",",
				Args:    []Term{char, code},
			}))
		case Integer:
			r := rune(cd)

			if !utf8.ValidRune(r) {
				return nondet.Error(representationError(Atom("character_code"), Atom(fmt.Sprintf("%d is not a valid unicode code point.", r))))
			}

			return nondet.Delay(func() nondet.Promise {
				return Unify(ch, Atom(r), k)
			})
		default:
			return nondet.Error(typeErrorInteger(code))
		}
	case Atom:
		rs := []rune(ch)
		if len(rs) != 1 {
			return nondet.Error(typeErrorCharacter(char))
		}

		return nondet.Delay(func() nondet.Promise {
			return Unify(code, Integer(rs[0]), k)
		})
	default:
		return nondet.Error(typeErrorCharacter(char))
	}
}

// PutByte outputs an integer byte to a stream represented by streamOrAlias.
func (vm *VM) PutByte(streamOrAlias, byt Term, k nondet.Promise) nondet.Promise {
	s, err := vm.stream(streamOrAlias)
	if err != nil {
		return nondet.Error(err)
	}

	if s.sink == nil {
		return nondet.Error(permissionErrorOutputStream(streamOrAlias))
	}

	if s.streamType == streamTypeText {
		return nondet.Error(permissionErrorOutputTextStream(streamOrAlias))
	}

	switch b := Resolve(byt).(type) {
	case *Variable:
		return nondet.Error(instantiationError(byt))
	case Integer:
		if 0 > b || 255 < b {
			return nondet.Error(typeErrorByte(byt))
		}

		if _, err := s.sink.Write([]byte{byte(b)}); err != nil {
			return nondet.Error(systemError(err))
		}

		return k
	default:
		return nondet.Error(typeErrorByte(byt))
	}
}

// PutCode outputs code to the stream represented by streamOrAlias.
func (vm *VM) PutCode(streamOrAlias, code Term, k nondet.Promise) nondet.Promise {
	s, err := vm.stream(streamOrAlias)
	if err != nil {
		return nondet.Error(err)
	}

	if s.sink == nil {
		return nondet.Error(permissionErrorOutputStream(streamOrAlias))
	}

	if s.streamType == streamTypeBinary {
		return nondet.Error(permissionErrorOutputBinaryStream(streamOrAlias))
	}

	switch c := Resolve(code).(type) {
	case *Variable:
		return nondet.Error(instantiationError(code))
	case Integer:
		r := rune(c)

		if !utf8.ValidRune(r) {
			return nondet.Error(representationError(Atom("character_code"), Atom(fmt.Sprintf("%s is not a valid unicode code point.", c))))
		}

		if _, err := s.sink.Write([]byte(string(r))); err != nil {
			return nondet.Error(systemError(err))
		}

		return k
	default:
		return nondet.Error(typeErrorInteger(code))
	}
}

// ReadTerm reads from the stream represented by streamOrAlias and unifies with stream.
func (vm *VM) ReadTerm(streamOrAlias, term, options Term, k nondet.Promise) nondet.Promise {
	s, err := vm.stream(streamOrAlias)
	if err != nil {
		return nondet.Error(err)
	}

	if s.source == nil {
		return nondet.Error(permissionErrorInputStream(streamOrAlias))
	}

	if s.streamType == streamTypeBinary {
		return nondet.Error(permissionErrorInputBinaryStream(streamOrAlias))
	}

	var opts readTermOptions
	if err := Each(Resolve(options), func(option Term) error {
		if _, ok := Resolve(option).(*Variable); ok {
			return instantiationError(option)
		}

		var v Variable
		switch {
		case option.Unify(&Compound{Functor: "singletons", Args: []Term{&v}}, false):
			opts.singletons = &v
		case option.Unify(&Compound{Functor: "variables", Args: []Term{&v}}, false):
			opts.variables = &v
		case option.Unify(&Compound{Functor: "variable_names", Args: []Term{&v}}, false):
			opts.variableNames = &v
		default:
			return domainErrorReadOption(option)
		}
		return nil
	}); err != nil {
		return nondet.Error(err)
	}

	br, ok := s.source.(*bufio.Reader)
	if !ok {
		return nondet.Error(errors.New("not a buffered stream"))
	}

	p := NewParser(vm, br)
	t, err := p.Term()
	if err != nil {
		switch {
		case errors.Is(err, io.EOF):
			switch s.eofAction {
			case eofActionError:
				return nondet.Error(permissionErrorInputPastEndOfStream(streamOrAlias))
			case eofActionEOFCode:
				return nondet.Delay(func() nondet.Promise {
					return Unify(term, Atom("end_of_file"), k)
				})
			case eofActionReset:
				return nondet.Delay(func() nondet.Promise {
					return vm.ReadTerm(streamOrAlias, term, options, k)
				})
			default:
				return nondet.Error(systemError(fmt.Errorf("unknown EOF action: %d", s.eofAction)))
			}
		case errors.Is(err, internal.ErrInsufficient):
			return nondet.Error(syntaxErrorInsufficient())
		case errors.As(err, &internal.UnexpectedRuneError{}):
			return nondet.Error(syntaxErrorUnexpectedChar(Atom(err.Error())))
		default:
			return nondet.Error(systemError(err))
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
			Args:    []Term{Atom(vc.variable.Name), vc.variable},
		})
		vc.variable.Name = ""
	}

	if opts.singletons != nil && !opts.singletons.Unify(List(singletons...), false) {
		return nondet.Bool(false)
	}

	if opts.variables != nil && !opts.variables.Unify(List(variables...), false) {
		return nondet.Bool(false)
	}

	if opts.variableNames != nil && !opts.variableNames.Unify(List(variableNames...), false) {
		return nondet.Bool(false)
	}

	return nondet.Delay(func() nondet.Promise {
		return Unify(term, t, k)
	})
}

// GetByte reads a byte from the stream represented by streamOrAlias and unifies it with inByte.
func (vm *VM) GetByte(streamOrAlias, inByte Term, k nondet.Promise) nondet.Promise {
	s, err := vm.stream(streamOrAlias)
	if err != nil {
		return nondet.Error(err)
	}

	if s.source == nil {
		return nondet.Error(permissionErrorInputStream(streamOrAlias))
	}

	if s.streamType == streamTypeText {
		return nondet.Error(permissionErrorInputTextStream(streamOrAlias))
	}

	switch b := Resolve(inByte).(type) {
	case *Variable:
		break
	case Integer:
		if b < 0 || b > 255 {
			nondet.Error(typeErrorInByte(inByte))
		}
		break
	default:
		return nondet.Error(typeErrorInByte(inByte))
	}

	b := make([]byte, 1)
	_, err = s.source.Read(b)
	switch err {
	case nil:
		return nondet.Delay(func() nondet.Promise {
			return Unify(inByte, Integer(b[0]), k)
		})
	case io.EOF:
		switch s.eofAction {
		case eofActionError:
			return nondet.Error(permissionErrorInputPastEndOfStream(streamOrAlias))
		case eofActionEOFCode:
			return nondet.Delay(func() nondet.Promise {
				return Unify(inByte, Integer(-1), k)
			})
		case eofActionReset:
			return nondet.Delay(func() nondet.Promise {
				return vm.GetByte(streamOrAlias, inByte, k)
			})
		default:
			return nondet.Error(systemError(fmt.Errorf("unknown EOF action: %d", s.eofAction)))
		}
	default:
		return nondet.Error(err)
	}
}

// GetChar reads a character from the stream represented by streamOrAlias and unifies it with char.
func (vm *VM) GetChar(streamOrAlias, char Term, k nondet.Promise) nondet.Promise {
	s, err := vm.stream(streamOrAlias)
	if err != nil {
		return nondet.Error(err)
	}

	if s.source == nil {
		return nondet.Error(permissionErrorInputStream(streamOrAlias))
	}

	if s.streamType == streamTypeBinary {
		return nondet.Error(permissionErrorInputBinaryStream(streamOrAlias))
	}

	br, ok := s.source.(*bufio.Reader)
	if !ok {
		return nondet.Error(permissionErrorInputBufferedStream(streamOrAlias))
	}

	switch c := Resolve(char).(type) {
	case *Variable:
		break
	case Atom:
		if len([]rune(c)) != 1 {
			return nondet.Error(typeErrorInCharacter(char))
		}
		break
	default:
		return nondet.Error(typeErrorInCharacter(char))
	}

	r, _, err := br.ReadRune()
	switch err {
	case nil:
		if r == unicode.ReplacementChar {
			return nondet.Error(representationError(Atom("character"), Atom("invalid character.")))
		}

		return nondet.Delay(func() nondet.Promise {
			return Unify(char, Atom(r), k)
		})
	case io.EOF:
		switch s.eofAction {
		case eofActionError:
			return nondet.Error(permissionErrorInputPastEndOfStream(streamOrAlias))
		case eofActionEOFCode:
			return nondet.Delay(func() nondet.Promise {
				return Unify(char, Atom("end_of_file"), k)
			})
		case eofActionReset:
			return nondet.Delay(func() nondet.Promise {
				return vm.GetChar(streamOrAlias, char, k)
			})
		default:
			return nondet.Error(systemError(fmt.Errorf("unknown EOF action: %d", s.eofAction)))
		}
	default:
		return nondet.Error(systemError(err))
	}
}

// PeekByte peeks a byte from the stream represented by streamOrAlias and unifies it with inByte.
func (vm *VM) PeekByte(streamOrAlias, inByte Term, k nondet.Promise) nondet.Promise {
	s, err := vm.stream(streamOrAlias)
	if err != nil {
		return nondet.Error(err)
	}

	if s.source == nil {
		return nondet.Error(permissionErrorInputStream(streamOrAlias))
	}

	if s.streamType == streamTypeText {
		return nondet.Error(permissionErrorInputTextStream(streamOrAlias))
	}

	br, ok := s.source.(*bufio.Reader)
	if !ok {
		return nondet.Error(permissionErrorInputBufferedStream(streamOrAlias))
	}

	switch b := Resolve(inByte).(type) {
	case *Variable:
		break
	case Integer:
		if b < 0 || b > 255 {
			return nondet.Error(typeErrorInByte(inByte))
		}
		break
	default:
		return nondet.Error(typeErrorInByte(inByte))
	}

	b, err := br.Peek(1)
	switch err {
	case nil:
		return nondet.Delay(func() nondet.Promise {
			return Unify(inByte, Integer(b[0]), k)
		})
	case io.EOF:
		switch s.eofAction {
		case eofActionError:
			return nondet.Error(permissionErrorInputPastEndOfStream(streamOrAlias))
		case eofActionEOFCode:
			return nondet.Delay(func() nondet.Promise {
				return Unify(inByte, Integer(-1), k)
			})
		case eofActionReset:
			return nondet.Delay(func() nondet.Promise {
				return vm.PeekByte(streamOrAlias, inByte, k)
			})
		default:
			return nondet.Error(systemError(fmt.Errorf("unknown EOF action: %d", s.eofAction)))
		}
	default:
		return nondet.Error(systemError(err))
	}
}

// PeekChar peeks a rune from the stream represented by streamOrAlias and unifies it with char.
func (vm *VM) PeekChar(streamOrAlias, char Term, k nondet.Promise) nondet.Promise {
	s, err := vm.stream(streamOrAlias)
	if err != nil {
		return nondet.Error(err)
	}

	if s.source == nil {
		return nondet.Error(permissionErrorInputStream(streamOrAlias))
	}

	if s.streamType == streamTypeBinary {
		return nondet.Error(permissionErrorInputBinaryStream(streamOrAlias))
	}

	br, ok := s.source.(*bufio.Reader)
	if !ok {
		return nondet.Error(permissionErrorInputBufferedStream(streamOrAlias))
	}

	switch c := Resolve(char).(type) {
	case *Variable:
		break
	case Atom:
		if len([]rune(c)) != 1 {
			return nondet.Error(typeErrorInCharacter(char))
		}
		break
	default:
		return nondet.Error(typeErrorInCharacter(char))
	}

	r, _, err := br.ReadRune()
	switch err {
	case nil:
		if err := br.UnreadRune(); err != nil {
			return nondet.Error(systemError(err))
		}

		if r == unicode.ReplacementChar {
			return nondet.Error(representationError(Atom("character"), Atom("invalid character.")))
		}

		return nondet.Delay(func() nondet.Promise {
			return Unify(char, Atom(r), k)
		})
	case io.EOF:
		switch s.eofAction {
		case eofActionError:
			return nondet.Error(permissionErrorInputPastEndOfStream(streamOrAlias))
		case eofActionEOFCode:
			return nondet.Delay(func() nondet.Promise {
				return Unify(char, Atom("end_of_file"), k)
			})
		case eofActionReset:
			return nondet.Delay(func() nondet.Promise {
				return vm.PeekChar(streamOrAlias, char, k)
			})
		default:
			return nondet.Error(systemError(fmt.Errorf("unknown EOF action: %d", s.eofAction)))
		}
	default:
		return nondet.Error(systemError(err))
	}
}

var osExit = os.Exit

// Halt exits the process with exit code of n.
func (vm *VM) Halt(n Term, k nondet.Promise) nondet.Promise {
	switch code := Resolve(n).(type) {
	case *Variable:
		return nondet.Error(instantiationError(n))
	case Integer:
		for _, f := range vm.OnHalt {
			f()
		}

		osExit(int(code))

		return k
	default:
		return nondet.Error(typeErrorInteger(n))
	}
}

// Clause unifies head and body with H and B respectively where H :- B is in the database.
func (vm *VM) Clause(head, body Term, k nondet.Promise) nondet.Promise {
	pi, _, err := piArgs(head)
	if err != nil {
		return nondet.Error(err)
	}

	switch Resolve(body).(type) {
	case *Variable, Atom, *Compound:
		break
	default:
		return nondet.Error(typeErrorCallable(body))
	}

	fvs := FreeVariables(head, body)

	cs, _ := vm.procedures[pi].(clauses)
	ks := make([]func() nondet.Promise, len(cs))
	for i := range cs {
		r := Rulify(cs[i].raw.Copy())
		ks[i] = func() nondet.Promise {
			ResetVariables(fvs...)
			return Unify(&Compound{
				Functor: ":-",
				Args:    []Term{head, body},
			}, r, k)
		}
	}
	return nondet.Delay(ks...)
}

// AtomLength counts the runes in atom and unifies the result with length.
func AtomLength(atom, length Term, k nondet.Promise) nondet.Promise {
	switch a := Resolve(atom).(type) {
	case *Variable:
		return nondet.Error(instantiationError(atom))
	case Atom:
		switch l := Resolve(length).(type) {
		case *Variable:
			break
		case Integer:
			if l < 0 {
				return nondet.Error(domainErrorNotLessThanZero(length))
			}
			break
		default:
			return nondet.Error(typeErrorInteger(length))
		}

		return nondet.Delay(func() nondet.Promise {
			return Unify(length, Integer(len([]rune(a))), k)
		})
	default:
		return nondet.Error(typeErrorAtom(atom))
	}
}

// AtomConcat concatenates atom1 and atom2 and unifies it with atom3.
func AtomConcat(atom1, atom2, atom3 Term, k nondet.Promise) nondet.Promise {
	switch a3 := Resolve(atom3).(type) {
	case *Variable:
		switch a1 := Resolve(atom1).(type) {
		case *Variable:
			return nondet.Error(instantiationError(&Compound{
				Functor: ",",
				Args:    []Term{atom1, atom3},
			}))
		case Atom:
			switch a2 := Resolve(atom2).(type) {
			case *Variable:
				return nondet.Error(instantiationError(&Compound{
					Functor: ",",
					Args:    []Term{atom2, atom3},
				}))
			case Atom:
				return nondet.Delay(func() nondet.Promise {
					return Unify(a1+a2, a3, k)
				})
			default:
				return nondet.Error(typeErrorAtom(atom2))
			}
		default:
			return nondet.Error(typeErrorAtom(atom1))
		}
	case Atom:
		switch Resolve(atom1).(type) {
		case *Variable, Atom:
			break
		default:
			return nondet.Error(typeErrorAtom(atom1))
		}

		switch Resolve(atom2).(type) {
		case *Variable, Atom:
			break
		default:
			return nondet.Error(typeErrorAtom(atom2))
		}

		pattern := Compound{Args: []Term{atom1, atom2}}
		fvs := FreeVariables(atom1, atom2)
		ks := make([]func() nondet.Promise, 0, len(a3)+1)
		for i := range a3 {
			a1, a2 := a3[:i], a3[i:]
			ks = append(ks, func() nondet.Promise {
				ResetVariables(fvs...)
				return Unify(&pattern, &Compound{Args: []Term{a1, a2}}, k)
			})
		}
		ks = append(ks, func() nondet.Promise {
			ResetVariables(fvs...)
			return Unify(&pattern, &Compound{Args: []Term{a3, Atom("")}}, k)
		})
		return nondet.Delay(ks...)
	default:
		return nondet.Error(typeErrorAtom(atom3))
	}
}

// SubAtom unifies subAtom with a sub atom of atom of length which appears with before runes preceding it and after runes following it.
func SubAtom(atom, before, length, after, subAtom Term, k nondet.Promise) nondet.Promise {
	switch whole := Resolve(atom).(type) {
	case *Variable:
		return nondet.Error(instantiationError(atom))
	case Atom:
		rs := []rune(whole)

		switch b := Resolve(before).(type) {
		case *Variable:
			break
		case Integer:
			if b < 0 {
				return nondet.Error(domainErrorNotLessThanZero(before))
			}
			break
		default:
			return nondet.Error(typeErrorInteger(before))
		}

		switch l := Resolve(length).(type) {
		case *Variable:
			break
		case Integer:
			if l < 0 {
				return nondet.Error(domainErrorNotLessThanZero(length))
			}
			break
		default:
			return nondet.Error(typeErrorInteger(length))
		}

		switch a := Resolve(after).(type) {
		case *Variable:
			break
		case Integer:
			if a < 0 {
				return nondet.Error(domainErrorNotLessThanZero(after))
			}
			break
		default:
			return nondet.Error(typeErrorInteger(after))
		}

		switch Resolve(subAtom).(type) {
		case *Variable, Atom:
			break
		default:
			return nondet.Error(typeErrorAtom(subAtom))
		}

		pattern := Compound{Args: []Term{before, length, after, subAtom}}
		fvs := FreeVariables(before, length, after, subAtom)
		var ks []func() nondet.Promise
		for i := 0; i <= len(rs); i++ {
			for j := i; j <= len(rs); j++ {
				before, length, after, subAtom := Integer(i), Integer(j-i), Integer(len(rs)-j), Atom(rs[i:j])
				ks = append(ks, func() nondet.Promise {
					ResetVariables(fvs...)
					return Unify(&pattern, &Compound{Args: []Term{before, length, after, subAtom}}, k)
				})
			}
		}
		return nondet.Delay(ks...)
	default:
		return nondet.Error(typeErrorAtom(atom))
	}
}

// AtomChars breaks down atom into list of characters and unifies with chars, or constructs an atom from a list of
// characters chars and unifies it with atom.
func AtomChars(atom, chars Term, k nondet.Promise) nondet.Promise {
	switch a := Resolve(atom).(type) {
	case *Variable:
		var sb strings.Builder
		if err := Each(Resolve(chars), func(elem Term) error {
			switch e := Resolve(elem).(type) {
			case *Variable:
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
		}); err != nil {
			return nondet.Error(err)
		}
		return nondet.Delay(func() nondet.Promise {
			return Unify(atom, Atom(sb.String()), k)
		})
	case Atom:
		rs := []rune(a)
		cs := make([]Term, len(rs))
		for i, r := range rs {
			cs[i] = Atom(r)
		}
		return nondet.Delay(func() nondet.Promise {
			return Unify(chars, List(cs...), k)
		})
	default:
		return nondet.Error(typeErrorAtom(atom))
	}
}

// AtomCodes breaks up atom into a list of runes and unifies it with codes, or constructs an atom from the list of runes
// and unifies it with atom.
func AtomCodes(atom, codes Term, k nondet.Promise) nondet.Promise {
	switch a := Resolve(atom).(type) {
	case *Variable:
		var sb strings.Builder
		if err := Each(Resolve(codes), func(elem Term) error {
			switch e := Resolve(elem).(type) {
			case *Variable:
				return instantiationError(elem)
			case Integer:
				if _, err := sb.WriteRune(rune(e)); err != nil {
					return systemError(err)
				}
				return nil
			default:
				return representationError(Atom("character_code"), Atom("invalid character code."))
			}
		}); err != nil {
			return nondet.Error(err)
		}
		return nondet.Delay(func() nondet.Promise {
			return Unify(atom, Atom(sb.String()), k)
		})
	case Atom:
		rs := []rune(a)
		cs := make([]Term, len(rs))
		for i, r := range rs {
			cs[i] = Integer(r)
		}
		return nondet.Delay(func() nondet.Promise {
			return Unify(codes, List(cs...), k)
		})
	default:
		return nondet.Error(typeErrorAtom(atom))
	}
}

// NumberChars breaks up an atom representation of a number num into a list of characters and unifies it with chars, or
// constructs a number from a list of characters chars and unifies it with num.
func NumberChars(num, chars Term, k nondet.Promise) nondet.Promise {
	switch n := Resolve(num).(type) {
	case *Variable:
		var sb strings.Builder
		if err := Each(Resolve(chars), func(elem Term) error {
			switch e := Resolve(elem).(type) {
			case *Variable:
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
		}); err != nil {
			return nondet.Error(err)
		}

		var vm VM
		p := NewParser(&vm, bufio.NewReader(strings.NewReader(sb.String())))
		t, err := p.Number()
		if err != nil {
			return nondet.Error(err)
		}
		return nondet.Delay(func() nondet.Promise {
			return Unify(num, t, k)
		})
	case Integer, Float:
		var buf bytes.Buffer
		if err := n.WriteTerm(&buf, defaultWriteTermOptions); err != nil {
			return nondet.Error(err)
		}
		rs := []rune(buf.String())
		cs := make([]Term, len(rs))
		for i, r := range rs {
			cs[i] = Atom(r)
		}
		return nondet.Delay(func() nondet.Promise {
			return Unify(chars, List(cs...), k)
		})
	default:
		return nondet.Error(typeErrorNumber(num))
	}
}

// NumberCodes breaks up an atom representation of a number num into a list of runes and unifies it with codes, or
// constructs a number from a list of runes codes and unifies it with num.
func NumberCodes(num, codes Term, k nondet.Promise) nondet.Promise {
	switch n := Resolve(num).(type) {
	case *Variable:
		var sb strings.Builder
		if err := Each(Resolve(codes), func(elem Term) error {
			switch e := Resolve(elem).(type) {
			case *Variable:
				return instantiationError(elem)
			case Integer:
				if _, err := sb.WriteRune(rune(e)); err != nil {
					return systemError(err)
				}
				return nil
			default:
				return representationError(Atom("character_code"), Atom(fmt.Sprintf("%s is not a valid character code.", elem)))
			}
		}); err != nil {
			return nondet.Error(err)
		}

		var vm VM
		p := NewParser(&vm, bufio.NewReader(strings.NewReader(sb.String())))
		t, err := p.Number()
		if err != nil {
			return nondet.Error(err)
		}
		return nondet.Delay(func() nondet.Promise {
			return Unify(num, t, k)
		})
	case Integer, Float:
		var buf bytes.Buffer
		if err := n.WriteTerm(&buf, defaultWriteTermOptions); err != nil {
			return nondet.Error(err)
		}
		rs := []rune(buf.String())
		cs := make([]Term, len(rs))
		for i, r := range rs {
			cs[i] = Integer(r)
		}
		return nondet.Delay(func() nondet.Promise {
			return Unify(codes, List(cs...), k)
		})
	default:
		return nondet.Error(typeErrorNumber(num))
	}
}

// FunctionSet is a set of unary/binary functions.
type FunctionSet struct {
	Unary  map[Atom]func(x Term) (Term, error)
	Binary map[Atom]func(x, y Term) (Term, error)
}

// Is evaluates expression and unifies the result with result.
func (fs FunctionSet) Is(result, expression Term, k nondet.Promise) nondet.Promise {
	v, err := fs.eval(expression)
	if err != nil {
		return nondet.Error(err)
	}
	return nondet.Delay(func() nondet.Promise {
		return Unify(result, v, k)
	})
}

// Equal succeeds iff lhs equals to rhs.
func (fs FunctionSet) Equal(lhs, rhs Term, k nondet.Promise) nondet.Promise {
	return fs.compare(lhs, rhs, k, func(i Integer, j Integer) bool {
		return i == j
	}, func(f Float, g Float) bool {
		return f == g
	})
}

// NotEqual succeeds iff lhs doesn't equal to rhs.
func (fs FunctionSet) NotEqual(lhs, rhs Term, k nondet.Promise) nondet.Promise {
	return fs.compare(lhs, rhs, k, func(i Integer, j Integer) bool {
		return i != j
	}, func(f Float, g Float) bool {
		return f != g
	})
}

// LessThan succeeds iff lhs is less than rhs.
func (fs FunctionSet) LessThan(lhs, rhs Term, k nondet.Promise) nondet.Promise {
	return fs.compare(lhs, rhs, k, func(i Integer, j Integer) bool {
		return i < j
	}, func(f Float, g Float) bool {
		return f < g
	})
}

// GreaterThan succeeds iff lhs is greater than rhs.
func (fs FunctionSet) GreaterThan(lhs, rhs Term, k nondet.Promise) nondet.Promise {
	return fs.compare(lhs, rhs, k, func(i Integer, j Integer) bool {
		return i > j
	}, func(f Float, g Float) bool {
		return f > g
	})
}

// LessThanOrEqual succeeds iff lhs is less than or equal to rhs.
func (fs FunctionSet) LessThanOrEqual(lhs, rhs Term, k nondet.Promise) nondet.Promise {
	return fs.compare(lhs, rhs, k, func(i Integer, j Integer) bool {
		return i <= j
	}, func(f Float, g Float) bool {
		return f <= g
	})
}

// GreaterThanOrEqual succeeds iff lhs is greater than or equal to rhs.
func (fs FunctionSet) GreaterThanOrEqual(lhs, rhs Term, k nondet.Promise) nondet.Promise {
	return fs.compare(lhs, rhs, k, func(i Integer, j Integer) bool {
		return i >= j
	}, func(f Float, g Float) bool {
		return f >= g
	})
}

func (fs FunctionSet) compare(lhs, rhs Term, k nondet.Promise, pi func(Integer, Integer) bool, pf func(Float, Float) bool) nondet.Promise {
	l, err := fs.eval(lhs)
	if err != nil {
		return nondet.Error(err)
	}

	r, err := fs.eval(rhs)
	if err != nil {
		return nondet.Error(err)
	}

	switch l := l.(type) {
	case Integer:
		switch r := r.(type) {
		case Integer:
			if !pi(l, r) {
				return nondet.Bool(false)
			}
			return k
		case Float:
			if !pf(Float(l), r) {
				return nondet.Bool(false)
			}
			return k
		default:
			return nondet.Error(typeErrorEvaluable(r))
		}
	case Float:
		switch r := r.(type) {
		case Integer:
			if !pf(l, Float(r)) {
				return nondet.Bool(false)
			}
			return k
		case Float:
			if !pf(l, r) {
				return nondet.Bool(false)
			}
			return k
		default:
			return nondet.Error(typeErrorEvaluable(r))
		}
	default:
		return nondet.Error(typeErrorEvaluable(l))
	}
}

func (fs FunctionSet) eval(expression Term) (_ Term, err error) {
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

	switch t := Resolve(expression).(type) {
	case *Variable:
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
			x, err := fs.eval(t.Args[0])
			if err != nil {
				return nil, err
			}
			return f(x)
		case 2:
			f, ok := fs.Binary[t.Functor]
			if !ok {
				return nil, typeErrorEvaluable(expression)
			}
			x, err := fs.eval(t.Args[0])
			if err != nil {
				return nil, err
			}
			y, err := fs.eval(t.Args[1])
			if err != nil {
				return nil, err
			}
			return f(x, y)
		default:
			return nil, typeErrorEvaluable(expression)
		}
	default:
		return nil, typeErrorEvaluable(expression)
	}
}

// DefaultFunctionSet is a FunctionSet with builtin functions.
var DefaultFunctionSet = FunctionSet{
	Unary: map[Atom]func(Term) (Term, error){
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
	Binary: map[Atom]func(Term, Term) (Term, error){
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

func unaryInteger(f func(i int64) int64) func(Term) (Term, error) {
	return func(x Term) (Term, error) {
		i, ok := Resolve(x).(Integer)
		if !ok {
			return nil, typeErrorInteger(x)
		}

		return Integer(f(int64(i))), nil
	}
}

func binaryInteger(f func(i, j int64) int64) func(Term, Term) (Term, error) {
	return func(x, y Term) (Term, error) {
		i, ok := Resolve(x).(Integer)
		if !ok {
			return nil, typeErrorInteger(x)
		}

		j, ok := Resolve(y).(Integer)
		if !ok {
			return nil, typeErrorInteger(y)
		}

		return Integer(f(int64(i), int64(j))), nil
	}
}

func unaryFloat(f func(n float64) float64) func(Term) (Term, error) {
	return func(x Term) (Term, error) {
		switch x := Resolve(x).(type) {
		case Integer:
			return Float(f(float64(x))), nil
		case Float:
			return Float(f(float64(x))), nil
		default:
			return nil, typeErrorEvaluable(x)
		}
	}
}

func binaryFloat(f func(n float64, m float64) float64) func(Term, Term) (Term, error) {
	return func(x, y Term) (Term, error) {
		switch x := Resolve(x).(type) {
		case Integer:
			switch y := Resolve(y).(type) {
			case Integer:
				return Float(f(float64(x), float64(y))), nil
			case Float:
				return Float(f(float64(x), float64(y))), nil
			default:
				return nil, typeErrorEvaluable(y)
			}
		case Float:
			switch y := Resolve(y).(type) {
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

func unaryNumber(fi func(i int64) int64, ff func(n float64) float64) func(Term) (Term, error) {
	return func(x Term) (Term, error) {
		switch x := Resolve(x).(type) {
		case Integer:
			return Integer(fi(int64(x))), nil
		case Float:
			return Float(ff(float64(x))), nil
		default:
			return nil, typeErrorEvaluable(x)
		}
	}
}

func binaryNumber(fi func(i, j int64) int64, ff func(n, m float64) float64) func(Term, Term) (Term, error) {
	return func(x, y Term) (Term, error) {
		switch x := Resolve(x).(type) {
		case Integer:
			switch y := Resolve(y).(type) {
			case Integer:
				return Integer(fi(int64(x), int64(y))), nil
			case Float:
				return Float(ff(float64(x), float64(y))), nil
			default:
				return nil, typeErrorEvaluable(y)
			}
		case Float:
			switch y := Resolve(y).(type) {
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
func (vm *VM) StreamProperty(streamOrAlias, property Term, k nondet.Promise) nondet.Promise {
	streams := make([]*Stream, 0, len(vm.streams))
	switch s := Resolve(streamOrAlias).(type) {
	case *Variable:
		for _, v := range vm.streams {
			streams = append(streams, v)
		}
	case Atom: // ISO standard stream_property/2 doesn't take an alias but why not?
		v, ok := vm.streams[s]
		if !ok {
			return nondet.Error(existenceErrorStream(streamOrAlias))
		}
		streams = append(streams, v)
	case *Stream:
		streams = append(streams, s)
	default:
		return nondet.Error(domainErrorStreamOrAlias(streamOrAlias))
	}

	switch p := Resolve(property).(type) {
	case *Variable:
		break
	case Atom:
		switch p {
		case "input", "output":
			break
		default:
			return nondet.Error(domainErrorStreamProperty(property))
		}
	case *Compound:
		if len(p.Args) != 1 {
			return nondet.Error(domainErrorStreamProperty(property))
		}
		arg := p.Args[0]
		switch p.Functor {
		case "file_name", "mode", "alias", "end_of_stream", "eof_action", "reposition":
			switch Resolve(arg).(type) {
			case *Variable, Atom:
				break
			default:
				return nondet.Error(typeErrorAtom(arg))
			}
		case "position":
			if len(p.Args) != 1 {
				return nondet.Error(domainErrorStreamProperty(property))
			}
			switch Resolve(p.Args[0]).(type) {
			case *Variable, Integer:
				break
			default:
				return nondet.Error(typeErrorAtom(arg))
			}
		default:
			return nondet.Error(domainErrorStreamProperty(property))
		}
	default:
		return nondet.Error(domainErrorStreamProperty(property))
	}

	var ks []func() nondet.Promise
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
				return nondet.Error(err)
			}
			if br, ok := s.source.(*bufio.Reader); ok {
				pos -= int64(br.Buffered())
			}

			fi, err := f.Stat()
			if err != nil {
				return nondet.Error(err)
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

		fvs := FreeVariables(property)
		for i := range properties {
			p := properties[i]
			ks = append(ks, func() nondet.Promise {
				ResetVariables(fvs...)
				return Unify(property, p, k)
			})
		}
	}
	return nondet.Delay(ks...)
}

// SetStreamPosition sets the position property of the stream represented by streamOrAlias.
func (vm *VM) SetStreamPosition(streamOrAlias, position Term, k nondet.Promise) nondet.Promise {
	s, err := vm.stream(streamOrAlias)
	if err != nil {
		return nondet.Error(err)
	}

	switch p := Resolve(position).(type) {
	case *Variable:
		return nondet.Error(instantiationError(position))
	case Integer:
		f, ok := s.closer.(*os.File)
		if !ok {
			return nondet.Error(permissionError(Atom("reposition"), Atom("stream"), streamOrAlias, Atom(fmt.Sprintf("%s is not a file.", streamOrAlias))))
		}

		if _, err := f.Seek(int64(p), 0); err != nil {
			return nondet.Error(systemError(err))
		}

		if br, ok := s.source.(*bufio.Reader); ok {
			br.Reset(f)
		}

		return k
	default:
		return nondet.Error(typeErrorInteger(position))
	}
}

// CharConversion registers a character conversion from inChar to outChar, or remove the conversion if inChar = outChar.
func (vm *VM) CharConversion(inChar, outChar Term, k nondet.Promise) nondet.Promise {
	switch in := Resolve(inChar).(type) {
	case *Variable:
		return nondet.Error(instantiationError(inChar))
	case Atom:
		i := []rune(in)
		if len(i) != 1 {
			return nondet.Error(representationError(Atom("character"), Atom(fmt.Sprintf("%s is not a character.", inChar))))
		}

		switch out := Resolve(outChar).(type) {
		case *Variable:
			return nondet.Error(instantiationError(outChar))
		case Atom:
			o := []rune(out)
			if len(o) != 1 {
				return nondet.Error(representationError(Atom("character"), Atom(fmt.Sprintf("%s is not a character.", outChar))))
			}

			if vm.charConversions == nil {
				vm.charConversions = map[rune]rune{}
			}
			if i[0] == o[0] {
				delete(vm.charConversions, i[0])
				return k
			}
			vm.charConversions[i[0]] = o[0]
			return k
		default:
			return nondet.Error(representationError(Atom("character"), Atom(fmt.Sprintf("%s is not a character.", outChar))))
		}
	default:
		return nondet.Error(representationError(Atom("character"), Atom(fmt.Sprintf("%s is not a character.", inChar))))
	}
}

// CurrentCharConversion succeeds iff a conversion from inChar to outChar is defined.
func (vm *VM) CurrentCharConversion(inChar, outChar Term, k nondet.Promise) nondet.Promise {
	switch in := Resolve(inChar).(type) {
	case *Variable:
		break
	case Atom:
		i := []rune(in)
		if len(i) != 1 {
			return nondet.Error(representationError(Atom("character"), Atom(fmt.Sprintf("%s is not a character.", inChar))))
		}
	default:
		return nondet.Error(representationError(Atom("character"), Atom(fmt.Sprintf("%s is not a character.", inChar))))
	}

	switch out := Resolve(outChar).(type) {
	case *Variable:
		break
	case Atom:
		o := []rune(out)
		if len(o) != 1 {
			return nondet.Error(representationError(Atom("character"), Atom(fmt.Sprintf("%s is not a character.", outChar))))
		}
	default:
		return nondet.Error(representationError(Atom("character"), Atom(fmt.Sprintf("%s is not a character.", outChar))))
	}

	if c1, ok := Resolve(inChar).(Atom); ok {
		r := []rune(c1)
		if r, ok := vm.charConversions[r[0]]; ok {
			return nondet.Delay(func() nondet.Promise {
				return Unify(outChar, Atom(r), k)
			})
		}
		return nondet.Delay(func() nondet.Promise {
			return Unify(outChar, c1, k)
		})
	}

	fvs := FreeVariables(inChar, outChar)
	pattern := Compound{Args: []Term{inChar, outChar}}
	ks := make([]func() nondet.Promise, 256)
	for i := 0; i < 256; i++ {
		r := rune(i)
		cr, ok := vm.charConversions[r]
		if !ok {
			cr = r
		}

		ks[i] = func() nondet.Promise {
			ResetVariables(fvs...)
			return Unify(&pattern, &Compound{Args: []Term{Atom(r), Atom(cr)}}, k)
		}
	}
	return nondet.Delay(ks...)
}

// SetPrologFlag sets flag to value.
func (vm *VM) SetPrologFlag(flag, value Term, k nondet.Promise) nondet.Promise {
	switch f := Resolve(flag).(type) {
	case *Variable:
		return nondet.Error(instantiationError(flag))
	case Atom:
		switch f {
		case "bounded", "max_integer", "min_integer", "integer_rounding_function", "max_arity":
			return nondet.Error(permissionError(Atom("modify"), Atom("flag"), f, Atom(fmt.Sprintf("%s is not modifiable.", f))))
		case "char_conversion":
			switch a := Resolve(value).(type) {
			case *Variable:
				return nondet.Error(instantiationError(value))
			case Atom:
				switch a {
				case "on":
					vm.charConvEnabled = true
					return k
				case "off":
					vm.charConvEnabled = false
					return k
				default:
					return nondet.Error(domainErrorFlagValue(&Compound{
						Functor: "+",
						Args:    []Term{flag, value},
					}))
				}
			default:
				return nondet.Error(domainErrorFlagValue(&Compound{
					Functor: "+",
					Args:    []Term{flag, value},
				}))
			}
		case "debug":
			switch a := Resolve(value).(type) {
			case *Variable:
				return nondet.Error(instantiationError(value))
			case Atom:
				switch a {
				case "on":
					vm.debug = true
					return k
				case "off":
					vm.debug = false
					return k
				default:
					return nondet.Error(domainErrorFlagValue(&Compound{
						Functor: "+",
						Args:    []Term{flag, value},
					}))
				}
			default:
				return nondet.Error(domainErrorFlagValue(&Compound{
					Functor: "+",
					Args:    []Term{flag, value},
				}))
			}
		case "unknown":
			switch a := Resolve(value).(type) {
			case *Variable:
				return nondet.Error(instantiationError(value))
			case Atom:
				switch a {
				case "error":
					vm.unknown = unknownError
					return k
				case "warning":
					vm.unknown = unknownWarning
					return k
				case "fail":
					vm.unknown = unknownFail
					return k
				default:
					return nondet.Error(domainErrorFlagValue(&Compound{
						Functor: "+",
						Args:    []Term{flag, value},
					}))
				}
			default:
				return nondet.Error(domainErrorFlagValue(&Compound{
					Functor: "+",
					Args:    []Term{flag, value},
				}))
			}
		default:
			return nondet.Error(domainErrorPrologFlag(flag))
		}
	default:
		return nondet.Error(typeErrorAtom(flag))
	}
}

// CurrentPrologFlag succeeds iff flag is set to value.
func (vm *VM) CurrentPrologFlag(flag, value Term, k nondet.Promise) nondet.Promise {
	switch f := Resolve(flag).(type) {
	case *Variable:
		break
	case Atom:
		switch f {
		case "bounded", "max_integer", "min_integer", "integer_rounding_function", "char_conversion", "debug", "max_arity", "unknown":
			break
		default:
			return nondet.Error(domainErrorPrologFlag(flag))
		}
	default:
		return nondet.Error(typeErrorAtom(flag))
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
	fvs := FreeVariables(flag, value)
	ks := make([]func() nondet.Promise, len(flags))
	for i := range flags {
		f := flags[i]
		ks[i] = func() nondet.Promise {
			ResetVariables(fvs...)
			return Unify(&pattern, f, k)
		}
	}
	return nondet.Delay(ks...)
}

func onOff(b bool) Atom {
	if b {
		return "on"
	}
	return "off"
}

func (vm *VM) stream(streamOrAlias Term) (*Stream, error) {
	switch s := Resolve(streamOrAlias).(type) {
	case *Variable:
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

func (vm *VM) Dynamic(pi Term, k nondet.Promise) nondet.Promise {
	switch p := Resolve(pi).(type) {
	case *Variable:
		return nondet.Error(instantiationError(pi))
	case *Compound:
		if p.Functor != "/" || len(p.Args) != 2 {
			return nondet.Error(typeErrorPredicateIndicator(pi))
		}
		switch f := Resolve(p.Args[0]).(type) {
		case *Variable:
			return nondet.Error(instantiationError(pi))
		case Atom:
			switch a := Resolve(p.Args[1]).(type) {
			case *Variable:
				return nondet.Error(instantiationError(pi))
			case Integer:
				pi := procedureIndicator{name: f, arity: a}
				p, ok := vm.procedures[pi]
				if !ok {
					vm.procedures[pi] = clauses{}
					return k
				}
				if _, ok := p.(clauses); !ok {
					return nondet.Bool(false)
				}
				return k
			default:
				return nondet.Error(typeErrorPredicateIndicator(pi))
			}
		default:
			return nondet.Error(typeErrorPredicateIndicator(pi))
		}
	default:
		return nondet.Error(typeErrorPredicateIndicator(pi))
	}
}
