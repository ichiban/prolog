package prolog

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
)

// Call executes goal. it succeeds if goal followed by k succeeds. A cut inside goal doesn't affect outside of Call.
func (e *EngineState) Call(goal Term, k func() Promise) Promise {
	pi, args, err := piArgs(goal)
	if err != nil {
		return Error(err)
	}

	// Force() to restrict the scope of cut.
	return NewPromise(e.arrive(pi, args, k).Force())
}

// Unify unifies t1 and t2 without occurs check (i.e., X = f(X) is allowed).
func Unify(t1, t2 Term, k func() Promise) Promise {
	a := newAssignment(t1, t2)
	if !t1.Unify(t2, false) {
		a.reset()
		return Bool(false)
	}
	return Delay(k)
}

// UnifyWithOccursCheck unifies t1 and t2 with occurs check (i.e., X = f(X) is not allowed).
func UnifyWithOccursCheck(t1, t2 Term, k func() Promise) Promise {
	a := newAssignment(t1, t2)
	if !t1.Unify(t2, true) {
		a.reset()
		return Bool(false)
	}
	return Delay(k)
}

// TypeVar checks if t is a variable.
func TypeVar(t Term, k func() Promise) Promise {
	if _, ok := Resolve(t).(*Variable); !ok {
		return Bool(false)
	}
	return Delay(k)
}

// TypeFloat checks if t is a floating-point number.
func TypeFloat(t Term, k func() Promise) Promise {
	if _, ok := Resolve(t).(Float); !ok {
		return Bool(false)
	}
	return Delay(k)
}

// TypeInteger checks if t is an integer.
func TypeInteger(t Term, k func() Promise) Promise {
	if _, ok := Resolve(t).(Integer); !ok {
		return Bool(false)
	}
	return Delay(k)
}

// TypeAtom checks if t is an atom.
func TypeAtom(t Term, k func() Promise) Promise {
	if _, ok := Resolve(t).(Atom); !ok {
		return Bool(false)
	}
	return Delay(k)
}

// TypeCompound checks if t is a compound term.
func TypeCompound(t Term, k func() Promise) Promise {
	if _, ok := Resolve(t).(*Compound); !ok {
		return Bool(false)
	}
	return Delay(k)
}

// Functor extracts the name and arity of term, or unifies term with an atomic/compound term of name and arity with
// fresh variables as arguments.
func Functor(term, name, arity Term, k func() Promise) Promise {
	term = Resolve(term)
	switch t := Resolve(term).(type) {
	case *Variable:
		break
	case *Compound:
		pattern := Compound{Args: []Term{name, arity}}
		return Delay(func() Promise {
			return Unify(&pattern, &Compound{Args: []Term{t.Functor, Integer(len(t.Args))}}, k)
		})
	default: // atomic
		pattern := Compound{Args: []Term{name, arity}}
		return Delay(func() Promise {
			return Unify(&pattern, &Compound{Args: []Term{t, Integer(0)}}, k)
		})
	}

	t := term.(*Variable)

	a, ok := Resolve(arity).(Integer)
	if !ok {
		return Error(TypeErrorInteger(arity))
	}
	switch {
	case a < 0:
		return Error(DomainErrorNotLessThanZero(a))
	case a == 0:
		return Unify(t, name, k)
	}

	n, ok := Resolve(name).(Atom)
	if !ok {
		return Error(TypeErrorAtom(name))
	}

	vs := make([]Term, a)
	for i := range vs {
		vs[i] = &Variable{}
	}
	return Delay(func() Promise {
		return Unify(t, &Compound{
			Functor: n,
			Args:    vs,
		}, k)
	})
}

// Arg extracts nth argument of term as arg, or finds the argument position of arg in term as nth.
func Arg(nth, term, arg Term, k func() Promise) Promise {
	t, ok := Resolve(term).(*Compound)
	if !ok {
		return Error(TypeErrorCompound(term))
	}

	switch n := Resolve(nth).(type) {
	case *Variable:
		pattern := Compound{Args: []Term{n, arg}}
		a := newAssignment(n, term, arg)
		ks := make([]func() Promise, len(t.Args))
		for i := range t.Args {
			n := Integer(i + 1)
			arg := t.Args[i]
			ks[i] = func() Promise {
				a.reset()
				return Unify(&pattern, &Compound{Args: []Term{n, arg}}, k)
			}
		}
		return Delay(ks...)
	case Integer:
		if n == 0 || int(n) >= len(t.Args) {
			return Bool(false)
		}
		if n < 0 {
			return Error(DomainErrorNotLessThanZero(n))
		}
		return Delay(func() Promise {
			return Unify(arg, t.Args[int(n)-1], k)
		})
	default:
		return Error(TypeErrorInteger(n))
	}
}

// Univ constructs list as a list which first element is the functor of term and the rest is the arguments of term, or construct a compound from list as term.
func Univ(term, list Term, k func() Promise) Promise {
	switch t := Resolve(term).(type) {
	case *Variable:
		list = Resolve(list)
		if list == Atom("[]") {
			return Error(DomainErrorNotEmptyList(list))
		}
		cons, ok := list.(*Compound)
		if !ok || cons.Functor != "." || len(cons.Args) != 2 {
			return Error(TypeErrorList(list))
		}

		f, ok := cons.Args[0].(Atom)
		if !ok {
			return Error(TypeErrorAtom(cons.Args[0]))
		}

		var args []Term
		if err := Each(cons.Args[1], func(elem Term) error {
			args = append(args, elem)
			return nil
		}); err != nil {
			return Error(err)
		}

		return Delay(func() Promise {
			return Unify(term, &Compound{
				Functor: f,
				Args:    args,
			}, k)
		})
	case *Compound:
		return Delay(func() Promise {
			return Unify(list, List(append([]Term{t.Functor}, t.Args...)...), k)
		})
	default:
		return Delay(func() Promise {
			return Unify(list, List(t), k)
		})
	}
}

// CopyTerm clones in as out.
func CopyTerm(in, out Term, k func() Promise) Promise {
	return Unify(in.Copy(), out, k)
}

// Op defines operator with priority and specifier, or removes when priority is 0.
func (e *EngineState) Op(priority, specifier, operator Term, k func() Promise) Promise {
	p, ok := Resolve(priority).(Integer)
	if !ok {
		return Error(TypeErrorInteger(priority))
	}
	if p < 0 || p > 1200 {
		return Error(DomainErrorOperatorPriority(priority))
	}

	s, ok := Resolve(specifier).(Atom)
	if !ok {
		return Error(TypeErrorAtom(specifier))
	}
	switch s {
	case "xf", "yf", "xfx", "xfy", "yfx", "fx", "fy":
		break
	default:
		return Error(DomainErrorOperatorSpecifier(s))
	}

	o, ok := Resolve(operator).(Atom)
	if !ok {
		return Error(TypeErrorAtom(operator))
	}

	// already defined?
	for i, op := range e.operators {
		if op.Specifier != s || op.Name != o {
			continue
		}

		// remove it first so that we can insert it again in the right position
		copy(e.operators[i:], e.operators[i+1:])
		e.operators[len(e.operators)-1] = Operator{}
		e.operators = e.operators[:len(e.operators)-1]

		// or keep it removed.
		if p == 0 {
			return Delay(k)
		}
	}

	// insert
	i := sort.Search(len(e.operators), func(i int) bool {
		return e.operators[i].Priority >= p
	})
	e.operators = append(e.operators, Operator{})
	copy(e.operators[i+1:], e.operators[i:])
	e.operators[i] = Operator{
		Priority:  p,
		Specifier: s,
		Name:      o,
	}

	return Delay(k)
}

// CurrentOp succeeds if operator is defined with priority and specifier.
func (e *EngineState) CurrentOp(priority, specifier, operator Term, k func() Promise) Promise {
	switch p := Resolve(priority).(type) {
	case *Variable:
		break
	case Integer:
		if p < 0 || p > 1200 {
			return Error(DomainErrorOperatorPriority(priority))
		}
		break
	default:
		return Error(DomainErrorOperatorPriority(priority))
	}

	switch s := Resolve(specifier).(type) {
	case *Variable:
		break
	case Atom:
		switch s {
		case "xf", "yf", "xfx", "xfy", "yfx", "fx", "fy":
			break
		default:
			return Error(DomainErrorOperatorSpecifier(s))
		}
	default:
		return Error(DomainErrorOperatorSpecifier(s))
	}

	switch Resolve(operator).(type) {
	case *Variable, Atom:
		break
	default:
		return Error(TypeErrorAtom(operator))
	}

	pattern := Compound{Args: []Term{priority, specifier, operator}}
	a := newAssignment(priority, specifier, operator)
	ks := make([]func() Promise, len(e.operators))
	for i := range e.operators {
		op := e.operators[i]
		ks[i] = func() Promise {
			a.reset()
			return Unify(&pattern, &Compound{Args: []Term{op.Priority, op.Specifier, op.Name}}, k)
		}
	}
	return Delay(ks...)
}

// Assertz appends t to the database.
func (e *EngineState) Assertz(t Term, k func() Promise) Promise {
	return e.assert(t, k, func(cs clauses, c clause) clauses {
		return append(cs, c)
	})
}

// Asserta prepends t to the database.
func (e *EngineState) Asserta(t Term, k func() Promise) Promise {
	return e.assert(t, k, func(cs clauses, c clause) clauses {
		return append(clauses{c}, cs...)
	})
}

func (e *EngineState) assert(t Term, k func() Promise, merge func(clauses, clause) clauses) Promise {
	pi, args, err := piArgs(t)
	if err != nil {
		return Error(err)
	}

	switch pi {
	case procedureIndicator{name: ":-", arity: 1}: // directive
		name, args, err := piArgs(args.(*Compound).Args[0])
		if err != nil {
			return Error(err)
		}
		return Delay(func() Promise {
			return e.arrive(name, args, k)
		})
	case procedureIndicator{name: ":-", arity: 2}:
		pi, _, err = piArgs(args.(*Compound).Args[0])
		if err != nil {
			return Error(err)
		}
	}

	if e.procedures == nil {
		e.procedures = map[procedureIndicator]procedure{}
	}
	p, ok := e.procedures[pi]
	if !ok {
		p = clauses{}
	}

	cs, ok := p.(clauses)
	if !ok {
		return Error(PermissionError(Atom("modify"), Atom("static_procedure"), &Compound{
			Functor: "/",
			Args:    []Term{pi.name, pi.arity},
		}, Atom(fmt.Sprintf("%s is static.", pi))))
	}
	c := clause{pf: pi}
	if err := c.compile(t); err != nil {
		return Error(err)
	}

	e.procedures[pi] = merge(cs, c)
	return Delay(k)
}

// Repeat enforces k until it returns true.
func Repeat(k func() Promise) Promise {
	for {
		ok, err := k().Force()
		if err != nil {
			return Error(err)
		}
		if ok {
			return Bool(true)
		}
	}
}

// BagOf collects all the solutions of goal as instances, which unify with template. instances may contain duplications.
func (e *EngineState) BagOf(template, goal, instances Term, k func() Promise) Promise {
	return e.collectionOf(template, goal, instances, k, List)
}

// SetOf collects all the solutions of goal as instances, which unify with template. instances don't contain duplications.
func (e *EngineState) SetOf(template, goal, instances Term, k func() Promise) Promise {
	return e.collectionOf(template, goal, instances, k, Set)
}

func (e *EngineState) collectionOf(template, goal, instances Term, k func() Promise, agg func(...Term) Term) Promise {
	if _, ok := Resolve(goal).(*Variable); ok {
		return Error(InstantiationError(goal))
	}

	var qualifier, body Variable
	if goal.Unify(&Compound{
		Functor: "^",
		Args:    []Term{&qualifier, &body},
	}, false) {
		goal = body.Ref
	}

	a := newAssignment(goal)

	freeVariables := newAssignment(template, &qualifier)
	groupingVariables := make(assignment, 0, len(a))
	for _, v := range a {
		if freeVariables.contains(v) {
			continue
		}
		groupingVariables = append(groupingVariables, v)
	}

	type solution struct {
		snapshots []Term // snapshot of grouping variable values
		bag       []Term
	}

	var solutions []solution
	_, err := e.Call(goal, func() Promise {
		snapshots := make([]Term, len(groupingVariables))
		for i, v := range groupingVariables {
			snapshots[i] = v.Ref
		}

	solutions:
		for i, s := range solutions {
			for i := range groupingVariables {
				ok, err := Compare(Atom("="), s.snapshots[i], snapshots[i], Done).Force()
				if err != nil {
					return Error(err)
				}
				if !ok {
					continue solutions
				}
			}
			solutions[i].bag = append(s.bag, template.Copy())
			return Bool(false) // ask for more solutions
		}

		solutions = append(solutions, solution{
			snapshots: snapshots,
			bag:       []Term{template.Copy()},
		})
		return Bool(false) // ask for more solutions
	}).Force()
	if err != nil {
		return Error(err)
	}

	freeVariables.reset()

	if len(solutions) == 0 {
		return Bool(false)
	}

	b := newAssignment(instances)
	ks := make([]func() Promise, len(solutions))
	for i := range solutions {
		s := solutions[i]
		ks[i] = func() Promise {
			b.reset()

			// revert to snapshot
			for i, v := range groupingVariables {
				v.Ref = s.snapshots[i]
			}

			return Unify(instances, agg(s.bag...), k)
		}
	}
	return Delay(ks...)
}

// Compare compares term1 and term2 and unifies order with <, =, or >.
func Compare(order, term1, term2 Term, k func() Promise) Promise {
	switch o := Resolve(order).(type) {
	case *Variable:
		break
	case Atom:
		switch o {
		case "<", "=", ">":
			break
		default:
			return Error(DomainErrorOrder(order))
		}
		break
	default:
		return Error(TypeErrorAtom(order))
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
func Throw(ball Term, _ func() Promise) Promise {
	if _, ok := Resolve(ball).(*Variable); ok {
		return Error(InstantiationError(ball))
	}
	return Error(&Exception{Term: Resolve(ball).Copy()})
}

// Catch calls goal. If an exception is thrown and unifies with catcher, it calls recover.
func (e *EngineState) Catch(goal, catcher, recover Term, k func() Promise) Promise {
	ok, err := e.Call(goal, k).Force()
	if err != nil {
		if ex, ok := err.(*Exception); ok && catcher.Unify(ex.Term, false) {
			return Delay(func() Promise {
				return e.Call(recover, k)
			})
		}
		return Error(err)
	}
	return Bool(ok)
}

// CurrentPredicate matches pi with a predicate indicator of the user-defined procedures in the database.
func (e *EngineState) CurrentPredicate(pi Term, k func() Promise) Promise {
	switch pi := Resolve(pi).(type) {
	case *Variable:
		break
	case *Compound:
		if pi.Functor != "/" || len(pi.Args) != 2 {
			return Error(TypeErrorPredicateIndicator(pi))
		}
		if _, ok := Resolve(pi.Args[0]).(Atom); !ok {
			return Error(TypeErrorPredicateIndicator(pi))
		}
		if _, ok := Resolve(pi.Args[1]).(Integer); !ok {
			return Error(TypeErrorPredicateIndicator(pi))
		}
		break
	default:
		return Error(TypeErrorPredicateIndicator(pi))
	}

	a := newAssignment(pi)
	ks := make([]func() Promise, 0, len(e.procedures))
	for key := range e.procedures {
		c := Compound{Functor: "/", Args: []Term{key.name, key.arity}}
		ks = append(ks, func() Promise {
			a.reset()
			return Unify(pi, &c, k)
		})
	}
	return Delay(ks...)
}

// Retract removes a clause which matches with t.
func (e *EngineState) Retract(t Term, k func() Promise) Promise {
	t = Rulify(t)

	h := t.(*Compound).Args[0]
	pi, _, err := piArgs(h)
	if err != nil {
		return Error(err)
	}

	p, ok := e.procedures[pi]
	if !ok {
		return Bool(false)
	}

	cs, ok := p.(clauses)
	if !ok {
		return Error(PermissionError(Atom("modify"), Atom("static_procedure"), &Compound{
			Functor: "/",
			Args:    []Term{pi.name, pi.arity},
		}, Atom(fmt.Sprintf("%s is static.", pi))))
	}

	updated := make(clauses, 0, len(cs))
	defer func() { e.procedures[pi] = updated }()

	for i, c := range cs {
		raw := Rulify(c.raw)
		a := newAssignment(raw, t)

		if !t.Unify(raw, false) {
			updated = append(updated, c)
			a.reset()
			continue
		}

		ok, err := k().Force()
		if err != nil {
			updated = append(updated, cs[i+1:]...)
			a.reset()
			return Error(err)
		}
		if ok {
			updated = append(updated, cs[i+1:]...)
			a.reset()
			return Bool(true)
		}

		a.reset()
	}

	return Bool(false)
}

// Abolish removes the procedure indicated by pi from the database.
func (e *EngineState) Abolish(pi Term, k func() Promise) Promise {
	if _, ok := Resolve(pi).(*Variable); ok {
		return Error(InstantiationError(pi))
	}

	c, ok := Resolve(pi).(*Compound)
	if !ok || c.Functor != "/" || len(c.Args) != 2 {
		return Error(TypeErrorPredicateIndicator(pi))
	}

	if _, ok := Resolve(c.Args[0]).(*Variable); ok {
		return Error(InstantiationError(c.Args[0]))
	}

	name, ok := Resolve(c.Args[0]).(Atom)
	if !ok {
		return Error(TypeErrorAtom(c.Args[0]))
	}

	if _, ok := Resolve(c.Args[1]).(*Variable); ok {
		return Error(InstantiationError(c.Args[1]))
	}

	arity, ok := Resolve(c.Args[1]).(Integer)
	if !ok {
		return Error(TypeErrorInteger(c.Args[1]))
	}
	if arity < 0 {
		return Error(DomainErrorNotLessThanZero(c.Args[1]))
	}

	key := procedureIndicator{name: name, arity: arity}
	if _, ok := e.procedures[key].(clauses); !ok {
		return Error(PermissionError(Atom("modify"), Atom("static_procedure"), &Compound{
			Functor: "/",
			Args:    []Term{name, arity},
		}, Atom(fmt.Sprintf("%s is static.", key))))
	}
	delete(e.procedures, key)
	return Delay(k)
}

// CurrentInput unifies stream with the current input stream.
func (e *EngineState) CurrentInput(stream Term, k func() Promise) Promise {
	switch Resolve(stream).(type) {
	case *Variable, *Stream:
		break
	default:
		return Error(DomainErrorStream(stream))
	}

	return Delay(func() Promise {
		return Unify(stream, e.input, k)
	})
}

// CurrentOutput unifies stream with the current output stream.
func (e *EngineState) CurrentOutput(stream Term, k func() Promise) Promise {
	switch Resolve(stream).(type) {
	case *Variable, *Stream:
		break
	default:
		return Error(DomainErrorStream(stream))
	}

	return Delay(func() Promise {
		return Unify(stream, e.output, k)
	})
}

// SetInput sets streamOrAlias as the current input stream.
func (e *EngineState) SetInput(streamOrAlias Term, k func() Promise) Promise {
	s, err := e.stream(streamOrAlias)
	if err != nil {
		return Error(err)
	}

	if s.Reader == nil {
		return Error(PermissionError(Atom("input"), Atom("stream"), streamOrAlias, Atom(fmt.Sprintf("%s is not an input stream.", streamOrAlias))))
	}

	e.input = s
	return Delay(k)
}

// SetOutput sets streamOrAlias as the current output stream.
func (e *EngineState) SetOutput(streamOrAlias Term, k func() Promise) Promise {
	s, err := e.stream(streamOrAlias)
	if err != nil {
		return Error(err)
	}

	if s.Writer == nil {
		return Error(PermissionError(Atom("input"), Atom("stream"), streamOrAlias, Atom(fmt.Sprintf("%s is not an output stream.", streamOrAlias))))
	}

	e.output = s
	return Delay(k)
}

// Open opens sourceSink in mode and unifies with stream.
func (e *EngineState) Open(sourceSink, mode, stream, options Term, k func() Promise) Promise {
	var n Atom
	switch s := Resolve(sourceSink).(type) {
	case *Variable:
		return Error(InstantiationError(sourceSink))
	case Atom:
		n = s
	default:
		return Error(DomainErrorSourceSink(sourceSink))
	}

	var (
		flag   int
		perm   os.FileMode
		buffer bool
	)
	switch m := Resolve(mode).(type) {
	case *Variable:
		return Error(InstantiationError(mode))
	case Atom:
		switch m {
		case "read":
			flag = os.O_RDONLY
			buffer = true
		case "write":
			flag = os.O_CREATE | os.O_WRONLY
			perm = 0644
		case "append":
			flag = os.O_APPEND | os.O_CREATE | os.O_WRONLY
			perm = 0644
		default:
			return Error(DomainErrorIOMode(m))
		}
	default:
		return Error(TypeErrorAtom(mode))
	}

	if _, ok := Resolve(stream).(*Variable); !ok {
		return Error(TypeErrorVariable(stream))
	}

	var (
		typ       = Atom("text")
		alias     Atom
		eofAction = Atom("error")
	)
	if err := Each(Resolve(options), func(option Term) error {
		if _, ok := Resolve(option).(*Variable); ok {
			return InstantiationError(option)
		}

		var arg Variable
		switch {
		case option.Unify(&Compound{Functor: "type", Args: []Term{Atom("text")}}, false):
			typ = "text"
		case option.Unify(&Compound{Functor: "type", Args: []Term{Atom("binary")}}, false):
			typ = "binary"
		case option.Unify(&Compound{Functor: "reposition", Args: []Term{Atom("true")}}, false):
			// TODO: don't know what to do.
		case option.Unify(&Compound{Functor: "reposition", Args: []Term{Atom("false")}}, false):
			// TODO: don't know what to do.
		case option.Unify(&Compound{Functor: "alias", Args: []Term{&arg}}, false):
			n, ok := Resolve(arg.Ref).(Atom)
			if !ok {
				return errors.New("not an atom")
			}
			if _, ok := e.streams[n]; ok {
				return PermissionError(Atom("open"), Atom("source_sink"), option, Atom(fmt.Sprintf("%s is already defined as an alias.", n)))
			}
			alias = n
		case option.Unify(&Compound{Functor: "eof_action", Args: []Term{Atom("error")}}, false):
			eofAction = "error"
		case option.Unify(&Compound{Functor: "eof_action", Args: []Term{Atom("eof_code")}}, false):
			eofAction = "eof_code"
		case option.Unify(&Compound{Functor: "eof_action", Args: []Term{Atom("reset")}}, false):
			eofAction = "reset"
		case option.Unify(&Compound{Functor: "buffer", Args: []Term{Atom("true")}}, false):
			buffer = true
		case option.Unify(&Compound{Functor: "buffer", Args: []Term{Atom("false")}}, false):
			buffer = false
		default:
			return DomainErrorStreamOption(option)
		}
		return nil
	}); err != nil {
		return Error(err)
	}

	f, err := os.OpenFile(string(n), flag, perm)
	if err != nil {
		switch {
		case os.IsNotExist(err):
			return Error(ExistenceErrorSourceSink(sourceSink))
		case os.IsPermission(err):
			return Error(PermissionError(Atom("open"), Atom("source_sink"), sourceSink, Atom(fmt.Sprintf("%s cannot be opened.", sourceSink))))
		default:
			return Error(SystemError(err))
		}
	}

	s := Stream{
		Closer:    f,
		mode:      mode.(Atom),
		alias:     alias,
		eofAction: eofAction,
		typ:       typ,
	}
	switch mode {
	case Atom("read"):
		s.Reader = f
		if buffer {
			s.Reader = bufio.NewReader(s.Reader)
		}
	case Atom("write"), Atom("append"):
		s.Writer = f
		if buffer {
			s.Writer = bufio.NewWriter(s.Writer)
		}
	}

	if e.streams == nil {
		e.streams = map[Term]*Stream{}
	}
	if alias == "" {
		// we can't use alias for the key but all the open streams should be in streams map anyways.
		e.streams[&s] = &s
	} else {
		e.streams[alias] = &s
	}

	return Delay(func() Promise {
		return Unify(stream, &s, k)
	})
}

// Close closes a stream specified by streamOrAlias.
func (e *EngineState) Close(streamOrAlias, options Term, k func() Promise) Promise {
	s, err := e.stream(streamOrAlias)
	if err != nil {
		return Error(err)
	}

	var force bool
	if err := Each(Resolve(options), func(option Term) error {
		if _, ok := Resolve(option).(*Variable); ok {
			return InstantiationError(option)
		}

		switch {
		case option.Unify(&Compound{Functor: "force", Args: []Term{Atom("false")}}, false):
			force = false
		case option.Unify(&Compound{Functor: "force", Args: []Term{Atom("true")}}, false):
			force = true
		default:
			return DomainErrorStreamOption(option)
		}
		return nil
	}); err != nil {
		return Error(err)
	}

	if err := s.Close(); err != nil && !force {
		return Error(ResourceError(streamOrAlias, Atom(err.Error())))
	}

	if s.alias == "" {
		delete(e.streams, s)
	} else {
		delete(e.streams, s.alias)
	}

	return Delay(k)
}

// FlushOutput sends any buffered output to the stream.
func (e *EngineState) FlushOutput(streamOrAlias Term, k func() Promise) Promise {
	s, err := e.stream(streamOrAlias)
	if err != nil {
		return Error(err)
	}

	if s.Writer == nil {
		return Error(PermissionError(Atom("output"), Atom("stream"), streamOrAlias, Atom(fmt.Sprintf("%s is not an output stream.", streamOrAlias))))
	}

	type flusher interface {
		Flush() error
	}

	if f, ok := s.Writer.(flusher); ok {
		if err := f.Flush(); err != nil {
			return Error(err)
		}
	}

	return Delay(k)
}

// WriteTerm outputs term to stream with options.
func (e *EngineState) WriteTerm(streamOrAlias, term, options Term, k func() Promise) Promise {
	s, err := e.stream(streamOrAlias)
	if err != nil {
		return Error(err)
	}

	opts := WriteTermOptions{Ops: e.operators}
	if err := Each(Resolve(options), func(option Term) error {
		if _, ok := Resolve(option).(*Variable); ok {
			return InstantiationError(option)
		}

		switch {
		case option.Unify(&Compound{Functor: "quoted", Args: []Term{Atom("false")}}, false):
			opts.Quoted = false
		case option.Unify(&Compound{Functor: "quoted", Args: []Term{Atom("true")}}, false):
			opts.Quoted = true
		case option.Unify(&Compound{Functor: "ignore_ops", Args: []Term{Atom("false")}}, false):
			opts.Ops = e.operators
		case option.Unify(&Compound{Functor: "ignore_ops", Args: []Term{Atom("true")}}, false):
			opts.Ops = nil
		case option.Unify(&Compound{Functor: "numbervars", Args: []Term{Atom("false")}}, false):
			opts.NumberVars = false
		case option.Unify(&Compound{Functor: "numbervars", Args: []Term{Atom("true")}}, false):
			opts.NumberVars = true
		default:
			return DomainErrorWriteOption(option)
		}
		return nil
	}); err != nil {
		return Error(err)
	}

	if s.Writer == nil {
		return Error(PermissionError(Atom("output"), Atom("stream"), streamOrAlias, Atom(fmt.Sprintf("%s is not an output stream.", streamOrAlias))))
	}

	if err := Resolve(term).WriteTerm(s, opts); err != nil {
		return Error(err)
	}

	return Delay(k)
}

// CharCode converts a single-rune Atom char to an Integer code, or vice versa.
func CharCode(char, code Term, k func() Promise) Promise {
	switch ch := Resolve(char).(type) {
	case *Variable:
		switch cd := Resolve(code).(type) {
		case *Variable:
			return Error(InstantiationError(&Compound{
				Functor: ",",
				Args:    []Term{char, code},
			}))
		case Integer:
			r := rune(cd)

			if !utf8.ValidRune(r) {
				return Error(RepresentationError(Atom("character_code"), Atom(fmt.Sprintf("%d is not a valid unicode code point.", r))))
			}

			return Delay(func() Promise {
				return Unify(ch, Atom(r), k)
			})
		default:
			return Error(TypeErrorInteger(code))
		}
	case Atom:
		rs := []rune(ch)
		if len(rs) != 1 {
			return Error(TypeErrorCharacter(char))
		}

		return Delay(func() Promise {
			return Unify(code, Integer(rs[0]), k)
		})
	default:
		return Error(TypeErrorCharacter(char))
	}
}

// PutByte outputs an integer byte to a stream represented by streamOrAlias.
func (e *EngineState) PutByte(streamOrAlias, byt Term, k func() Promise) Promise {
	s, err := e.stream(streamOrAlias)
	if err != nil {
		return Error(err)
	}

	if s.Writer == nil {
		return Error(PermissionError(Atom("output"), Atom("stream"), streamOrAlias, Atom(fmt.Sprintf("%s is not an output stream.", streamOrAlias))))
	}

	switch b := Resolve(byt).(type) {
	case *Variable:
		return Error(InstantiationError(byt))
	case Integer:
		if 0 > b || 255 < b {
			return Error(TypeErrorByte(byt))
		}

		if _, err := s.Write([]byte{byte(b)}); err != nil {
			return Error(SystemError(err))
		}

		return Delay(k)
	default:
		return Error(TypeErrorByte(byt))
	}
}

// PutCode outputs code to the stream represented by streamOrAlias.
func (e *EngineState) PutCode(streamOrAlias, code Term, k func() Promise) Promise {
	s, err := e.stream(streamOrAlias)
	if err != nil {
		return Error(err)
	}

	if s.Writer == nil {
		return Error(PermissionError(Atom("output"), Atom("stream"), streamOrAlias, Atom(fmt.Sprintf("%s is not an output stream.", streamOrAlias))))
	}

	switch c := Resolve(code).(type) {
	case *Variable:
		return Error(InstantiationError(code))
	case Integer:
		r := rune(c)

		if !utf8.ValidRune(r) {
			return Error(RepresentationError(Atom("character_code"), Atom(fmt.Sprintf("%s is not a valid unicode code point.", c))))
		}

		if _, err := s.Write([]byte(string(r))); err != nil {
			return Error(SystemError(err))
		}

		return Delay(k)
	default:
		return Error(TypeErrorInteger(code))
	}
}

// ReadTerm reads from the stream represented by streamOrAlias and unifies with stream.
func (e *EngineState) ReadTerm(streamOrAlias, term, options Term, k func() Promise) Promise {
	s, err := e.stream(streamOrAlias)
	if err != nil {
		return Error(err)
	}

	var opts readTermOptions
	if err := Each(Resolve(options), func(option Term) error {
		if _, ok := Resolve(option).(*Variable); ok {
			return InstantiationError(option)
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
			return DomainErrorReadOption(option)
		}
		return nil
	}); err != nil {
		return Error(err)
	}

	if s.Reader == nil {
		return Error(PermissionError(Atom("input"), Atom("stream"), streamOrAlias, Atom(fmt.Sprintf("%s is not an input stream.", streamOrAlias))))
	}

	br, ok := s.Reader.(*bufio.Reader)
	if !ok {
		return Error(errors.New("not a buffered stream"))
	}

	var conv map[rune]rune
	if e.charConvEnabled {
		conv = e.charConversions
	}
	p := NewParser(br, &e.operators, conv)

	t, err := p.Clause()
	if err != nil {
		return Error(err)
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
		return Bool(false)
	}

	if opts.variables != nil && !opts.variables.Unify(List(variables...), false) {
		return Bool(false)
	}

	if opts.variableNames != nil && !opts.variableNames.Unify(List(variableNames...), false) {
		return Bool(false)
	}

	return Delay(func() Promise {
		return Unify(term, t, k)
	})
}

// GetByte reads a byte from the stream represented by streamOrAlias and unifies it with inByte.
func (e *EngineState) GetByte(streamOrAlias, inByte Term, k func() Promise) Promise {
	s, err := e.stream(streamOrAlias)
	if err != nil {
		return Error(err)
	}

	if s.Reader == nil {
		return Error(PermissionError(Atom("input"), Atom("stream"), streamOrAlias, Atom(fmt.Sprintf("%s is not an input stream.", streamOrAlias))))
	}

	switch b := Resolve(inByte).(type) {
	case *Variable:
		break
	case Integer:
		if b < 0 || b > 255 {
			Error(TypeErrorInByte(inByte))
		}
		break
	default:
		return Error(TypeErrorInByte(inByte))
	}

	b := make([]byte, 1)
	_, err = s.Read(b)
	switch err {
	case nil:
		return Delay(func() Promise {
			return Unify(inByte, Integer(b[0]), k)
		})
	case io.EOF:
		return Delay(func() Promise {
			return Unify(inByte, Integer(-1), k)
		})
	default:
		return Error(err)
	}
}

// GetChar reads a character from the stream represented by streamOrAlias and unifies it with char.
func (e *EngineState) GetChar(streamOrAlias, char Term, k func() Promise) Promise {
	s, err := e.stream(streamOrAlias)
	if err != nil {
		return Error(err)
	}

	if s.Reader == nil {
		return Error(PermissionError(Atom("input"), Atom("stream"), streamOrAlias, Atom(fmt.Sprintf("%s is not an input stream.", streamOrAlias))))
	}

	br, ok := s.Reader.(*bufio.Reader)
	if !ok {
		return Error(PermissionError(Atom("input"), Atom("buffered_stream"), streamOrAlias, Atom(fmt.Sprintf("%s is not a buffered stream.", streamOrAlias))))
	}

	switch c := Resolve(char).(type) {
	case *Variable:
		break
	case Atom:
		if len([]rune(c)) != 1 {
			return Error(TypeErrorInCharacter(char))
		}
		break
	default:
		return Error(TypeErrorInCharacter(char))
	}

	r, _, err := br.ReadRune()
	switch err {
	case nil:
		if r == unicode.ReplacementChar {
			return Error(RepresentationError(Atom("character"), Atom("invalid character.")))
		}

		return Delay(func() Promise {
			return Unify(char, Atom(r), k)
		})
	case io.EOF:
		return Delay(func() Promise {
			return Unify(char, Atom("end_of_file"), k)
		})
	default:
		return Error(SystemError(err))
	}
}

// PeekByte peeks a byte from the stream represented by streamOrAlias and unifies it with inByte.
func (e *EngineState) PeekByte(streamOrAlias, inByte Term, k func() Promise) Promise {
	s, err := e.stream(streamOrAlias)
	if err != nil {
		return Error(err)
	}

	if s.Reader == nil {
		return Error(PermissionError(Atom("input"), Atom("stream"), streamOrAlias, Atom(fmt.Sprintf("%s is not an input stream.", streamOrAlias))))
	}

	br, ok := s.Reader.(*bufio.Reader)
	if !ok {
		return Error(PermissionError(Atom("input"), Atom("buffered_stream"), streamOrAlias, Atom(fmt.Sprintf("%s is not a buffered stream.", streamOrAlias))))
	}

	switch b := Resolve(inByte).(type) {
	case *Variable:
		break
	case Integer:
		if b < 0 || b > 255 {
			return Error(TypeErrorInByte(inByte))
		}
		break
	default:
		return Error(TypeErrorInByte(inByte))
	}

	b, err := br.Peek(1)
	switch err {
	case nil:
		return Delay(func() Promise {
			return Unify(inByte, Integer(b[0]), k)
		})
	case io.EOF:
		return Delay(func() Promise {
			return Unify(inByte, Integer(-1), k)
		})
	default:
		return Error(SystemError(err))
	}
}

// PeekChar peeks a rune from the stream represented by streamOrAlias and unifies it with char.
func (e *EngineState) PeekChar(streamOrAlias, char Term, k func() Promise) Promise {
	s, err := e.stream(streamOrAlias)
	if err != nil {
		return Error(err)
	}

	if s.Reader == nil {
		return Error(PermissionError(Atom("input"), Atom("stream"), streamOrAlias, Atom(fmt.Sprintf("%s is not an input stream.", streamOrAlias))))
	}

	br, ok := s.Reader.(*bufio.Reader)
	if !ok {
		return Error(PermissionError(Atom("input"), Atom("buffered_stream"), streamOrAlias, Atom(fmt.Sprintf("%s is not a buffered stream.", streamOrAlias))))
	}

	switch c := Resolve(char).(type) {
	case *Variable:
		break
	case Atom:
		if len([]rune(c)) != 1 {
			return Error(TypeErrorInCharacter(char))
		}
		break
	default:
		return Error(TypeErrorInCharacter(char))
	}

	r, _, err := br.ReadRune()
	switch err {
	case nil:
		if err := br.UnreadRune(); err != nil {
			return Error(SystemError(err))
		}

		if r == unicode.ReplacementChar {
			return Error(RepresentationError(Atom("character"), Atom("invalid character.")))
		}

		return Delay(func() Promise {
			return Unify(char, Atom(r), k)
		})
	case io.EOF:
		return Delay(func() Promise {
			return Unify(char, Atom("end_of_file"), k)
		})
	default:
		return Error(SystemError(err))
	}
}

var osExit = os.Exit

// Halt exits the process with exit code of n.
func (e *EngineState) Halt(n Term, k func() Promise) Promise {
	switch code := Resolve(n).(type) {
	case *Variable:
		return Error(InstantiationError(n))
	case Integer:
		for _, f := range e.BeforeHalt {
			f()
		}

		osExit(int(code))

		return Delay(k)
	default:
		return Error(TypeErrorInteger(n))
	}
}

// Clause unifies head and body with H and B respectively where H :- B is in the database.
func (e *EngineState) Clause(head, body Term, k func() Promise) Promise {
	pi, _, err := piArgs(head)
	if err != nil {
		return Error(err)
	}

	switch Resolve(body).(type) {
	case *Variable, Atom, *Compound:
		break
	default:
		return Error(TypeErrorCallable(body))
	}

	a := newAssignment(head, body)

	cs, _ := e.procedures[pi].(clauses)
	ks := make([]func() Promise, len(cs))
	for i := range cs {
		r := Rulify(cs[i].raw.Copy())
		ks[i] = func() Promise {
			a.reset()
			return Unify(&Compound{
				Functor: ":-",
				Args:    []Term{head, body},
			}, r, k)
		}
	}
	return Delay(ks...)
}

// AtomLength counts the runes in atom and unifies the result with length.
func AtomLength(atom, length Term, k func() Promise) Promise {
	switch a := Resolve(atom).(type) {
	case *Variable:
		return Error(InstantiationError(atom))
	case Atom:
		switch l := Resolve(length).(type) {
		case *Variable:
			break
		case Integer:
			if l < 0 {
				return Error(DomainErrorNotLessThanZero(length))
			}
			break
		default:
			return Error(TypeErrorInteger(length))
		}

		return Delay(func() Promise {
			return Unify(length, Integer(len([]rune(a))), k)
		})
	default:
		return Error(TypeErrorAtom(atom))
	}
}

// AtomConcat concatenates atom1 and atom2 and unifies it with atom3.
func AtomConcat(atom1, atom2, atom3 Term, k func() Promise) Promise {
	switch a3 := Resolve(atom3).(type) {
	case *Variable:
		switch a1 := Resolve(atom1).(type) {
		case *Variable:
			return Error(InstantiationError(&Compound{
				Functor: ",",
				Args:    []Term{atom1, atom3},
			}))
		case Atom:
			switch a2 := Resolve(atom2).(type) {
			case *Variable:
				return Error(InstantiationError(&Compound{
					Functor: ",",
					Args:    []Term{atom2, atom3},
				}))
			case Atom:
				return Delay(func() Promise {
					return Unify(a1+a2, a3, k)
				})
			default:
				return Error(TypeErrorAtom(atom2))
			}
		default:
			return Error(TypeErrorAtom(atom1))
		}
	case Atom:
		switch Resolve(atom1).(type) {
		case *Variable, Atom:
			break
		default:
			return Error(TypeErrorAtom(atom1))
		}

		switch Resolve(atom2).(type) {
		case *Variable, Atom:
			break
		default:
			return Error(TypeErrorAtom(atom2))
		}

		pattern := Compound{Args: []Term{atom1, atom2}}
		a := newAssignment(atom1, atom2)
		ks := make([]func() Promise, 0, len(a3)+1)
		for i := range a3 {
			a1, a2 := a3[:i], a3[i:]
			ks = append(ks, func() Promise {
				a.reset()
				return Unify(&pattern, &Compound{Args: []Term{a1, a2}}, k)
			})
		}
		ks = append(ks, func() Promise {
			a.reset()
			return Unify(&pattern, &Compound{Args: []Term{a3, Atom("")}}, k)
		})
		return Delay(ks...)
	default:
		return Error(TypeErrorAtom(atom3))
	}
}

// SubAtom unifies subAtom with a sub atom of atom of length which appears with before runes preceding it and after runes following it.
func SubAtom(atom, before, length, after, subAtom Term, k func() Promise) Promise {
	switch whole := Resolve(atom).(type) {
	case *Variable:
		return Error(InstantiationError(atom))
	case Atom:
		rs := []rune(whole)

		switch b := Resolve(before).(type) {
		case *Variable:
			break
		case Integer:
			if b < 0 {
				return Error(DomainErrorNotLessThanZero(before))
			}
			break
		default:
			return Error(TypeErrorInteger(before))
		}

		switch l := Resolve(length).(type) {
		case *Variable:
			break
		case Integer:
			if l < 0 {
				return Error(DomainErrorNotLessThanZero(length))
			}
			break
		default:
			return Error(TypeErrorInteger(length))
		}

		switch a := Resolve(after).(type) {
		case *Variable:
			break
		case Integer:
			if a < 0 {
				return Error(DomainErrorNotLessThanZero(after))
			}
			break
		default:
			return Error(TypeErrorInteger(after))
		}

		switch Resolve(subAtom).(type) {
		case *Variable, Atom:
			break
		default:
			return Error(TypeErrorAtom(subAtom))
		}

		pattern := Compound{Args: []Term{before, length, after, subAtom}}
		a := newAssignment(before, length, after, subAtom)
		var ks []func() Promise
		for i := 0; i <= len(rs); i++ {
			for j := i; j <= len(rs); j++ {
				before, length, after, subAtom := Integer(i), Integer(j-i), Integer(len(rs)-j), Atom(rs[i:j])
				ks = append(ks, func() Promise {
					a.reset()
					return Unify(&pattern, &Compound{Args: []Term{before, length, after, subAtom}}, k)
				})
			}
		}
		return Delay(ks...)
	default:
		return Error(TypeErrorAtom(atom))
	}
}

// AtomChars breaks down atom into list of characters and unifies with chars, or constructs an atom from a list of
// characters chars and unifies it with atom.
func AtomChars(atom, chars Term, k func() Promise) Promise {
	switch a := Resolve(atom).(type) {
	case *Variable:
		var sb strings.Builder
		if err := Each(Resolve(chars), func(elem Term) error {
			switch e := Resolve(elem).(type) {
			case *Variable:
				return InstantiationError(elem)
			case Atom:
				if len([]rune(e)) != 1 {
					return TypeErrorCharacter(elem)
				}
				if _, err := sb.WriteString(string(e)); err != nil {
					return SystemError(err)
				}
				return nil
			default:
				return TypeErrorCharacter(elem)
			}
		}); err != nil {
			return Error(err)
		}
		return Delay(func() Promise {
			return Unify(atom, Atom(sb.String()), k)
		})
	case Atom:
		rs := []rune(a)
		cs := make([]Term, len(rs))
		for i, r := range rs {
			cs[i] = Atom(r)
		}
		return Delay(func() Promise {
			return Unify(chars, List(cs...), k)
		})
	default:
		return Error(TypeErrorAtom(atom))
	}
}

// AtomCodes breaks up atom into a list of runes and unifies it with codes, or constructs an atom from the list of runes
// and unifies it with atom.
func AtomCodes(atom, codes Term, k func() Promise) Promise {
	switch a := Resolve(atom).(type) {
	case *Variable:
		var sb strings.Builder
		if err := Each(Resolve(codes), func(elem Term) error {
			switch e := Resolve(elem).(type) {
			case *Variable:
				return InstantiationError(elem)
			case Integer:
				if _, err := sb.WriteRune(rune(e)); err != nil {
					return SystemError(err)
				}
				return nil
			default:
				return RepresentationError(Atom("character_code"), Atom("invalid character code."))
			}
		}); err != nil {
			return Error(err)
		}
		return Delay(func() Promise {
			return Unify(atom, Atom(sb.String()), k)
		})
	case Atom:
		rs := []rune(a)
		cs := make([]Term, len(rs))
		for i, r := range rs {
			cs[i] = Integer(r)
		}
		return Delay(func() Promise {
			return Unify(codes, List(cs...), k)
		})
	default:
		return Error(TypeErrorAtom(atom))
	}
}

// NumberChars breaks up an atom representation of a number num into a list of characters and unifies it with chars, or
// constructs a number from a list of characters chars and unifies it with num.
func NumberChars(num, chars Term, k func() Promise) Promise {
	switch n := Resolve(num).(type) {
	case *Variable:
		var sb strings.Builder
		if err := Each(Resolve(chars), func(elem Term) error {
			switch e := Resolve(elem).(type) {
			case *Variable:
				return InstantiationError(elem)
			case Atom:
				if len([]rune(e)) != 1 {
					return TypeErrorCharacter(elem)
				}
				if _, err := sb.WriteString(string(e)); err != nil {
					return SystemError(err)
				}
				return nil
			default:
				return TypeErrorCharacter(elem)
			}
		}); err != nil {
			return Error(err)
		}

		p := NewParser(bufio.NewReader(strings.NewReader(sb.String())), &Operators{}, map[rune]rune{})
		t, err := p.Number()
		if err != nil {
			return Error(err)
		}
		return Delay(func() Promise {
			return Unify(num, t, k)
		})
	case Integer, Float:
		var buf bytes.Buffer
		if err := n.WriteTerm(&buf, defaultWriteTermOptions); err != nil {
			return Error(err)
		}
		rs := []rune(buf.String())
		cs := make([]Term, len(rs))
		for i, r := range rs {
			cs[i] = Atom(r)
		}
		return Delay(func() Promise {
			return Unify(chars, List(cs...), k)
		})
	default:
		return Error(TypeErrorNumber(num))
	}
}

// NumberCodes breaks up an atom representation of a number num into a list of runes and unifies it with codes, or
// constructs a number from a list of runes codes and unifies it with num.
func NumberCodes(num, codes Term, k func() Promise) Promise {
	switch n := Resolve(num).(type) {
	case *Variable:
		var sb strings.Builder
		if err := Each(Resolve(codes), func(elem Term) error {
			switch e := Resolve(elem).(type) {
			case *Variable:
				return InstantiationError(elem)
			case Integer:
				if _, err := sb.WriteRune(rune(e)); err != nil {
					return SystemError(err)
				}
				return nil
			default:
				return RepresentationError(Atom("character_code"), Atom(fmt.Sprintf("%s is not a valid character code.", elem)))
			}
		}); err != nil {
			return Error(err)
		}

		p := NewParser(bufio.NewReader(strings.NewReader(sb.String())), &Operators{}, map[rune]rune{})
		t, err := p.Number()
		if err != nil {
			return Error(err)
		}
		return Delay(func() Promise {
			return Unify(num, t, k)
		})
	case Integer, Float:
		var buf bytes.Buffer
		if err := n.WriteTerm(&buf, defaultWriteTermOptions); err != nil {
			return Error(err)
		}
		rs := []rune(buf.String())
		cs := make([]Term, len(rs))
		for i, r := range rs {
			cs[i] = Integer(r)
		}
		return Delay(func() Promise {
			return Unify(codes, List(cs...), k)
		})
	default:
		return Error(TypeErrorNumber(num))
	}
}

// FunctionSet is a set of unary/binary functions.
type FunctionSet struct {
	Unary  map[Atom]func(x Term) (Term, error)
	Binary map[Atom]func(x, y Term) (Term, error)
}

// Is evaluates expression and unifies the result with result.
func (fs FunctionSet) Is(result, expression Term, k func() Promise) Promise {
	v, err := fs.eval(expression)
	if err != nil {
		return Error(err)
	}
	return Delay(func() Promise {
		return Unify(result, v, k)
	})
}

// Equal succeeds iff lhs equals to rhs.
func (fs FunctionSet) Equal(lhs, rhs Term, k func() Promise) Promise {
	return fs.compare(lhs, rhs, k, func(i Integer, j Integer) bool {
		return i == j
	}, func(f Float, g Float) bool {
		return f == g
	})
}

// NotEqual succeeds iff lhs doesn't equal to rhs.
func (fs FunctionSet) NotEqual(lhs, rhs Term, k func() Promise) Promise {
	return fs.compare(lhs, rhs, k, func(i Integer, j Integer) bool {
		return i != j
	}, func(f Float, g Float) bool {
		return f != g
	})
}

// LessThan succeeds iff lhs is less than rhs.
func (fs FunctionSet) LessThan(lhs, rhs Term, k func() Promise) Promise {
	return fs.compare(lhs, rhs, k, func(i Integer, j Integer) bool {
		return i < j
	}, func(f Float, g Float) bool {
		return f < g
	})
}

// GreaterThan succeeds iff lhs is greater than rhs.
func (fs FunctionSet) GreaterThan(lhs, rhs Term, k func() Promise) Promise {
	return fs.compare(lhs, rhs, k, func(i Integer, j Integer) bool {
		return i > j
	}, func(f Float, g Float) bool {
		return f > g
	})
}

// LessThanOrEqual succeeds iff lhs is less than or equal to rhs.
func (fs FunctionSet) LessThanOrEqual(lhs, rhs Term, k func() Promise) Promise {
	return fs.compare(lhs, rhs, k, func(i Integer, j Integer) bool {
		return i <= j
	}, func(f Float, g Float) bool {
		return f <= g
	})
}

// GreaterThanOrEqual succeeds iff lhs is greater than or equal to rhs.
func (fs FunctionSet) GreaterThanOrEqual(lhs, rhs Term, k func() Promise) Promise {
	return fs.compare(lhs, rhs, k, func(i Integer, j Integer) bool {
		return i >= j
	}, func(f Float, g Float) bool {
		return f >= g
	})
}

func (fs FunctionSet) compare(lhs, rhs Term, k func() Promise, pi func(Integer, Integer) bool, pf func(Float, Float) bool) Promise {
	l, err := fs.eval(lhs)
	if err != nil {
		return Error(err)
	}

	r, err := fs.eval(rhs)
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
			return Delay(k)
		case Float:
			if !pf(Float(l), r) {
				return Bool(false)
			}
			return Delay(k)
		default:
			return Error(TypeErrorEvaluable(r))
		}
	case Float:
		switch r := r.(type) {
		case Integer:
			if !pf(l, Float(r)) {
				return Bool(false)
			}
			return Delay(k)
		case Float:
			if !pf(l, r) {
				return Bool(false)
			}
			return Delay(k)
		default:
			return Error(TypeErrorEvaluable(r))
		}
	default:
		return Error(TypeErrorEvaluable(l))
	}
}

func (fs FunctionSet) eval(expression Term) (_ Term, err error) {
	defer func() {
		if r := recover(); r != nil {
			if e, ok := r.(error); ok {
				if e.Error() == "runtime error: integer divide by zero" {
					err = EvaluationErrorZeroDivisor()
					return
				}
			}
			panic(r)
		}
	}()

	switch t := Resolve(expression).(type) {
	case *Variable:
		return nil, InstantiationError(expression)
	case Atom:
		return nil, TypeErrorEvaluable(expression) // TODO: constants?
	case Integer, Float:
		return t, nil
	case *Compound:
		switch len(t.Args) {
		case 1:
			f, ok := fs.Unary[t.Functor]
			if !ok {
				return nil, TypeErrorEvaluable(expression)
			}
			x, err := fs.eval(t.Args[0])
			if err != nil {
				return nil, err
			}
			return f(x)
		case 2:
			f, ok := fs.Binary[t.Functor]
			if !ok {
				return nil, TypeErrorEvaluable(expression)
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
			return nil, TypeErrorEvaluable(expression)
		}
	default:
		return nil, TypeErrorEvaluable(expression)
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
			return nil, TypeErrorInteger(x)
		}

		return Integer(f(int64(i))), nil
	}
}

func binaryInteger(f func(i, j int64) int64) func(Term, Term) (Term, error) {
	return func(x, y Term) (Term, error) {
		i, ok := Resolve(x).(Integer)
		if !ok {
			return nil, TypeErrorInteger(x)
		}

		j, ok := Resolve(y).(Integer)
		if !ok {
			return nil, TypeErrorInteger(y)
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
			return nil, TypeErrorEvaluable(x)
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
				return nil, TypeErrorEvaluable(y)
			}
		case Float:
			switch y := Resolve(y).(type) {
			case Integer:
				return Float(f(float64(x), float64(y))), nil
			case Float:
				return Float(f(float64(x), float64(y))), nil
			default:
				return nil, TypeErrorEvaluable(y)
			}
		default:
			return nil, TypeErrorEvaluable(x)
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
			return nil, TypeErrorEvaluable(x)
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
				return nil, TypeErrorEvaluable(y)
			}
		case Float:
			switch y := Resolve(y).(type) {
			case Integer:
				return Float(ff(float64(x), float64(y))), nil
			case Float:
				return Float(ff(float64(x), float64(y))), nil
			default:
				return nil, TypeErrorEvaluable(y)
			}
		default:
			return nil, TypeErrorEvaluable(x)
		}
	}
}

// StreamProperty succeeds iff the stream represented by streamOrAlias has the stream property property.
func (e *EngineState) StreamProperty(streamOrAlias, property Term, k func() Promise) Promise {
	streams := make([]*Stream, 0, len(e.streams))
	switch s := Resolve(streamOrAlias).(type) {
	case *Variable:
		for _, v := range e.streams {
			streams = append(streams, v)
		}
	case Atom: // ISO standard stream_property/2 doesn't take an alias but why not?
		v, ok := e.streams[s]
		if !ok {
			return Error(ExistenceErrorStream(streamOrAlias))
		}
		streams = append(streams, v)
	case *Stream:
		streams = append(streams, s)
	default:
		return Error(DomainErrorStreamOrAlias(streamOrAlias))
	}

	switch p := Resolve(property).(type) {
	case *Variable:
		break
	case Atom:
		switch p {
		case "input", "output":
			break
		default:
			return Error(DomainErrorStreamProperty(property))
		}
	case *Compound:
		switch p.Functor {
		case "file_name", "mode", "alias", "end_of_stream", "eof_action", "reposition":
			if len(p.Args) != 1 {
				return Error(DomainErrorStreamProperty(property))
			}
			switch Resolve(p.Args[0]).(type) {
			case *Variable, Atom:
				break
			default:
				return Error(TypeErrorAtom(p.Args[0]))
			}
		case "position":
			if len(p.Args) != 1 {
				return Error(DomainErrorStreamProperty(property))
			}
			switch Resolve(p.Args[0]).(type) {
			case *Variable, Integer:
				break
			default:
				return Error(TypeErrorAtom(p.Args[0]))
			}
		default:
			return Error(DomainErrorStreamProperty(property))
		}
	default:
		return Error(DomainErrorStreamProperty(property))
	}

	var ks []func() Promise
	for _, s := range streams {
		properties := []Term{
			&Compound{Functor: "mode", Args: []Term{s.mode}},
			&Compound{Functor: "alias", Args: []Term{s.alias}},
			&Compound{Functor: "eof_action", Args: []Term{s.eofAction}},
			&Compound{Functor: "type", Args: []Term{s.typ}},
		}

		if s.Reader != nil {
			if _, ok := s.Reader.(*bufio.Reader); ok {
				properties = append(properties, &Compound{Functor: "buffer", Args: []Term{Atom("true")}})
			} else {
				properties = append(properties, &Compound{Functor: "buffer", Args: []Term{Atom("false")}})
			}
		}

		if s.Writer != nil {
			if _, ok := s.Writer.(*bufio.Writer); ok {
				properties = append(properties, &Compound{Functor: "buffer", Args: []Term{Atom("true")}})
			} else {
				properties = append(properties, &Compound{Functor: "buffer", Args: []Term{Atom("false")}})
			}
		}

		if f, ok := s.Closer.(*os.File); ok {
			pos, err := f.Seek(0, 1)
			if err != nil {
				return Error(err)
			}
			if br, ok := s.Reader.(*bufio.Reader); ok {
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
				&Compound{Functor: "reposition", Args: []Term{Atom("true")}},
			)
		}

		a := newAssignment(property)
		for i := range properties {
			p := properties[i]
			ks = append(ks, func() Promise {
				a.reset()
				return Unify(property, p, k)
			})
		}
	}
	return Delay(ks...)
}

// SetStreamPosition sets the position property of the stream represented by streamOrAlias.
func (e *EngineState) SetStreamPosition(streamOrAlias, position Term, k func() Promise) Promise {
	s, err := e.stream(streamOrAlias)
	if err != nil {
		return Error(err)
	}

	switch p := Resolve(position).(type) {
	case *Variable:
		return Error(InstantiationError(position))
	case Integer:
		f, ok := s.Closer.(*os.File)
		if !ok {
			return Error(PermissionError(Atom("reposition"), Atom("stream"), streamOrAlias, Atom(fmt.Sprintf("%s is not a file.", streamOrAlias))))
		}

		if _, err := f.Seek(int64(p), 0); err != nil {
			return Error(SystemError(err))
		}

		if br, ok := s.Reader.(*bufio.Reader); ok {
			br.Reset(f)
		}

		return Delay(k)
	default:
		return Error(TypeErrorInteger(position))
	}
}

// CharConversion registers a character conversion from inChar to outChar, or remove the conversion if inChar = outChar.
func (e *EngineState) CharConversion(inChar, outChar Term, k func() Promise) Promise {
	switch in := Resolve(inChar).(type) {
	case *Variable:
		return Error(InstantiationError(inChar))
	case Atom:
		i := []rune(in)
		if len(i) != 1 {
			return Error(RepresentationError(Atom("character"), Atom(fmt.Sprintf("%s is not a character.", inChar))))
		}

		switch out := Resolve(outChar).(type) {
		case *Variable:
			return Error(InstantiationError(outChar))
		case Atom:
			o := []rune(out)
			if len(o) != 1 {
				return Error(RepresentationError(Atom("character"), Atom(fmt.Sprintf("%s is not a character.", outChar))))
			}

			if e.charConversions == nil {
				e.charConversions = map[rune]rune{}
			}
			if i[0] == o[0] {
				delete(e.charConversions, i[0])
				return Delay(k)
			}
			e.charConversions[i[0]] = o[0]
			return Delay(k)
		default:
			return Error(RepresentationError(Atom("character"), Atom(fmt.Sprintf("%s is not a character.", outChar))))
		}
	default:
		return Error(RepresentationError(Atom("character"), Atom(fmt.Sprintf("%s is not a character.", inChar))))
	}
}

// CurrentCharConversion succeeds iff a conversion from inChar to outChar is defined.
func (e *EngineState) CurrentCharConversion(inChar, outChar Term, k func() Promise) Promise {
	switch in := Resolve(inChar).(type) {
	case *Variable:
		break
	case Atom:
		i := []rune(in)
		if len(i) != 1 {
			return Error(RepresentationError(Atom("character"), Atom(fmt.Sprintf("%s is not a character.", inChar))))
		}
	default:
		return Error(RepresentationError(Atom("character"), Atom(fmt.Sprintf("%s is not a character.", inChar))))
	}

	switch out := Resolve(outChar).(type) {
	case *Variable:
		break
	case Atom:
		o := []rune(out)
		if len(o) != 1 {
			return Error(RepresentationError(Atom("character"), Atom(fmt.Sprintf("%s is not a character.", outChar))))
		}
	default:
		return Error(RepresentationError(Atom("character"), Atom(fmt.Sprintf("%s is not a character.", outChar))))
	}

	if c1, ok := Resolve(inChar).(Atom); ok {
		r := []rune(c1)
		if r, ok := e.charConversions[r[0]]; ok {
			return Delay(func() Promise {
				return Unify(outChar, Atom(r), k)
			})
		}
		return Delay(func() Promise {
			return Unify(outChar, c1, k)
		})
	}

	a := newAssignment(inChar, outChar)
	pattern := Compound{Args: []Term{inChar, outChar}}
	ks := make([]func() Promise, 256)
	for i := 0; i < 256; i++ {
		r := rune(i)
		cr, ok := e.charConversions[r]
		if !ok {
			cr = r
		}

		ks[i] = func() Promise {
			a.reset()
			return Unify(&pattern, &Compound{Args: []Term{Atom(r), Atom(cr)}}, k)
		}
	}
	return Delay(ks...)
}

// SetPrologFlag sets flag to value.
func (e *EngineState) SetPrologFlag(flag, value Term, k func() Promise) Promise {
	switch f := Resolve(flag).(type) {
	case *Variable:
		return Error(InstantiationError(flag))
	case Atom:
		switch f {
		case "bounded", "max_integer", "min_integer", "integer_rounding_function", "max_arity":
			return Error(PermissionError(Atom("modify"), Atom("flag"), f, Atom(fmt.Sprintf("%s is not modifiable.", f))))
		case "char_conversion":
			switch a := Resolve(value).(type) {
			case *Variable:
				return Error(InstantiationError(value))
			case Atom:
				switch a {
				case "on":
					e.charConvEnabled = true
					return Delay(k)
				case "off":
					e.charConvEnabled = false
					return Delay(k)
				default:
					return Error(DomainErrorFlagValue(&Compound{
						Functor: "+",
						Args:    []Term{flag, value},
					}))
				}
			default:
				return Error(DomainErrorFlagValue(&Compound{
					Functor: "+",
					Args:    []Term{flag, value},
				}))
			}
		case "debug":
			switch a := Resolve(value).(type) {
			case *Variable:
				return Error(InstantiationError(value))
			case Atom:
				switch a {
				case "on":
					e.debug = true
					return Delay(k)
				case "off":
					e.debug = false
					return Delay(k)
				default:
					return Error(DomainErrorFlagValue(&Compound{
						Functor: "+",
						Args:    []Term{flag, value},
					}))
				}
			default:
				return Error(DomainErrorFlagValue(&Compound{
					Functor: "+",
					Args:    []Term{flag, value},
				}))
			}
		case "unknown":
			switch a := Resolve(value).(type) {
			case *Variable:
				return Error(InstantiationError(value))
			case Atom:
				switch a {
				case "error":
					e.unknown = unknownError
					return Delay(k)
				case "warning":
					e.unknown = unknownWarning
					return Delay(k)
				case "fail":
					e.unknown = unknownFail
					return Delay(k)
				default:
					return Error(DomainErrorFlagValue(&Compound{
						Functor: "+",
						Args:    []Term{flag, value},
					}))
				}
			default:
				return Error(DomainErrorFlagValue(&Compound{
					Functor: "+",
					Args:    []Term{flag, value},
				}))
			}
		default:
			return Error(DomainErrorPrologFlag(flag))
		}
	default:
		return Error(TypeErrorAtom(flag))
	}
}

// CurrentPrologFlag succeeds iff flag is set to value.
func (e *EngineState) CurrentPrologFlag(flag, value Term, k func() Promise) Promise {
	switch f := Resolve(flag).(type) {
	case *Variable:
		break
	case Atom:
		switch f {
		case "bounded", "max_integer", "min_integer", "integer_rounding_function", "char_conversion", "debug", "max_arity", "unknown":
			break
		default:
			return Error(DomainErrorPrologFlag(flag))
		}
	default:
		return Error(TypeErrorAtom(flag))
	}

	pattern := Compound{Args: []Term{flag, value}}
	flags := []Term{
		&Compound{Args: []Term{Atom("bounded"), Atom("true")}},
		&Compound{Args: []Term{Atom("max_integer"), Integer(math.MaxInt64)}},
		&Compound{Args: []Term{Atom("min_integer"), Integer(math.MinInt64)}},
		&Compound{Args: []Term{Atom("integer_rounding_function"), Atom("toward_zero")}},
		&Compound{Args: []Term{Atom("char_conversion"), onOff(e.charConvEnabled)}},
		&Compound{Args: []Term{Atom("debug"), onOff(e.debug)}},
		&Compound{Args: []Term{Atom("max_arity"), Atom("unbounded")}},
		&Compound{Args: []Term{Atom("unknown"), Atom(e.unknown.String())}},
	}
	a := newAssignment(flag, value)
	ks := make([]func() Promise, len(flags))
	for i := range flags {
		f := flags[i]
		ks[i] = func() Promise {
			a.reset()
			return Unify(&pattern, f, k)
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

func (e *EngineState) stream(streamOrAlias Term) (*Stream, error) {
	switch s := Resolve(streamOrAlias).(type) {
	case *Variable:
		return nil, InstantiationError(streamOrAlias)
	case Atom:
		v, ok := e.streams[s]
		if !ok {
			return nil, ExistenceErrorStream(streamOrAlias)
		}
		return v, nil
	case *Stream:
		return s, nil
	default:
		return nil, DomainErrorStreamOrAlias(streamOrAlias)
	}
}
