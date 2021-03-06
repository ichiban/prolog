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
)

func (e *EngineState) Call(goal Term, k func() Promise) Promise {
	name, args, err := nameArgs(goal)
	if err != nil {
		return Error(err)
	}
	return Delay(func() Promise {
		return e.arrive(name, args, k)
	})
}

func Unify(t1, t2 Term, k func() Promise) Promise {
	a := newAssignment(t1, t2)
	if !t1.Unify(t2, false) {
		a.reset()
		return Bool(false)
	}
	return Delay(k)
}

func UnifyWithOccursCheck(t1, t2 Term, k func() Promise) Promise {
	a := newAssignment(t1, t2)
	if !t1.Unify(t2, true) {
		a.reset()
		return Bool(false)
	}
	return Delay(k)
}

func TypeVar(t Term, k func() Promise) Promise {
	if _, ok := Resolve(t).(*Variable); !ok {
		return Bool(false)
	}
	return Delay(k)
}

func TypeFloat(t Term, k func() Promise) Promise {
	if _, ok := Resolve(t).(Float); !ok {
		return Bool(false)
	}
	return Delay(k)
}

func TypeInteger(t Term, k func() Promise) Promise {
	if _, ok := Resolve(t).(Integer); !ok {
		return Bool(false)
	}
	return Delay(k)
}

func TypeAtom(t Term, k func() Promise) Promise {
	if _, ok := Resolve(t).(Atom); !ok {
		return Bool(false)
	}
	return Delay(k)
}

func TypeCompound(t Term, k func() Promise) Promise {
	if _, ok := Resolve(t).(*Compound); !ok {
		return Bool(false)
	}
	return Delay(k)
}

func Functor(term, name, arity Term, k func() Promise) Promise {
	var v *Variable
	for v == nil {
		switch t := term.(type) {
		case Atom:
			if !t.Unify(name, false) || !Integer(0).Unify(arity, false) {
				return Bool(false)
			}
			return Delay(k)
		case *Variable:
			if t.Ref == nil {
				v = t
				break
			}
			term = t.Ref
		case *Compound:
			if !t.Functor.Unify(name, false) || !Integer(len(t.Args)).Unify(arity, false) {
				return Bool(false)
			}
			return Delay(k)
		default:
			return Bool(false)
		}
	}

	var n *Atom
	for n == nil {
		switch t := name.(type) {
		case Atom:
			n = &t
		case *Variable:
			if t.Ref == nil {
				return Error(errors.New("invalid arguments: atom is not instantiated"))
			}
			name = t.Ref
		default:
			return Error(fmt.Errorf("invalid arguments: name is %T", name))
		}
	}

	var a *Integer
	for a == nil {
		switch t := arity.(type) {
		case Integer:
			a = &t
		case *Variable:
			if t.Ref == nil {
				return Error(errors.New("invalid arguments"))
			}
			arity = t.Ref
		default:
			return Error(errors.New("invalid arguments"))
		}
	}

	if *a == 0 {
		if !v.Unify(*a, false) {
			return Bool(false)
		}
		return Delay(k)
	}

	vars := make([]Term, *a)
	for i := range vars {
		var v Variable
		vars[i] = &v
	}

	if !v.Unify(&Compound{
		Functor: *n,
		Args:    vars,
	}, false) {
		return Bool(false)
	}
	return Delay(k)
}

func Arg(arg, term, value Term, k func() Promise) Promise {
	c, ok := Resolve(term).(*Compound)
	if !ok {
		return Error(errors.New("term should be compound"))
	}

	n, ok := Resolve(arg).(Integer)
	if !ok {
		pattern := Compound{Args: []Term{arg, value}}
		a := newAssignment(arg, term, value)
		ks := make([]func() Promise, len(c.Args))
		for i, t := range c.Args {
			ks[i] = func() Promise {
				defer a.reset()
				return Unify(&pattern, &Compound{Args: []Term{Integer(i + 1), t}}, k)
			}
		}
		return Delay(ks...)
	}
	if n == 0 || int(n) >= len(c.Args) {
		return Bool(false)
	}
	if n < 0 {
		return Error(errors.New("arg shouldn't be negative"))
	}

	if !c.Args[int(n)-1].Unify(value, false) {
		return Bool(false)
	}

	return Delay(k)
}

func Univ(term, list Term, k func() Promise) Promise {
	var c *Compound
	for c == nil {
		switch t := term.(type) {
		case *Variable:
			if t.Ref == nil {
				var car, cdr Variable
				if !list.Unify(Cons(&car, &cdr), false) {
					return Error(errors.New("invalid argument"))
				}
				var f *Atom
				for f == nil {
					switch t := car.Ref.(type) {
					case Atom:
						f = &t
					case *Variable:
						if t.Ref == nil {
							return Error(errors.New("invalid argument"))
						}
						car.Ref = t.Ref
					default:
						return Error(errors.New("invalid argument"))
					}
				}

				list = cdr.Ref

				var args []Term
				for list != Atom("[]") {
					var car, cdr Variable
					if !list.Unify(Cons(&car, &cdr), false) {
						return Error(errors.New("invalid argument"))
					}
					args = append(args, car.Ref)
					list = cdr.Ref
				}

				if !term.Unify(&Compound{
					Functor: *f,
					Args:    args,
				}, false) {
					return Bool(false)
				}
				return Delay(k)
			}
			term = t.Ref
		case *Compound:
			c = t
		default:
			return Error(errors.New("invalid argument"))
		}
	}

	l := List()
	for i := len(c.Args) - 1; i >= 0; i-- {
		l = Cons(c.Args[i], l)
	}
	if !list.Unify(Cons(c.Functor, l), false) {
		return Bool(false)
	}
	return Delay(k)
}

func CopyTerm(in, out Term, k func() Promise) Promise {
	return Unify(in.Copy(), out, k)
}

func (e *EngineState) Op(precedence, typ, name Term, k func() Promise) Promise {
	p, ok := Resolve(precedence).(Integer)
	if !ok {
		return Error(fmt.Errorf("invalid precedence: %s", precedence))
	}

	t, ok := Resolve(typ).(Atom)
	if !ok {
		return Error(fmt.Errorf("invalid type: %s", typ))
	}

	n, ok := Resolve(name).(Atom)
	if !ok {
		return Error(fmt.Errorf("invalid name: %s", name))
	}

	// already defined?
	for i, o := range e.operators {
		if o.Type != t || o.Name != n {
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
		return e.operators[i].Precedence >= p
	})
	e.operators = append(e.operators, Operator{})
	copy(e.operators[i+1:], e.operators[i:])
	e.operators[i] = Operator{
		Precedence: p,
		Type:       t,
		Name:       n,
	}

	return Delay(k)
}

func (e *EngineState) CurrentOp(precedence, typ, name Term, k func() Promise) Promise {
	pattern := Compound{Args: []Term{precedence, typ, name}}
	a := newAssignment(precedence, typ, name)
	ks := make([]func() Promise, len(e.operators))
	for i, op := range e.operators {
		ks[i] = func() Promise {
			a.reset()
			return Unify(&pattern, &Compound{Args: []Term{op.Precedence, op.Type, op.Name}}, k)
		}
	}
	return Delay(ks...)
}

func (e *EngineState) Assertz(t Term, k func() Promise) Promise {
	return e.assert(t, k, func(cs clauses, c clause) clauses {
		return append(cs, c)
	})
}

func (e *EngineState) Asserta(t Term, k func() Promise) Promise {
	return e.assert(t, k, func(cs clauses, c clause) clauses {
		return append(clauses{c}, cs...)
	})
}

func (e *EngineState) assert(t Term, k func() Promise, merge func(clauses, clause) clauses) Promise {
	name, args, err := nameArgs(t)
	if err != nil {
		return Error(err)
	}

	switch name {
	case "(:-)/1": // directive
		var d Variable
		args.Unify(Cons(&d, &Variable{}), false)
		name, args, err := nameArgs(&d)
		if err != nil {
			return Error(err)
		}
		return Delay(func() Promise {
			return e.arrive(name, args, k)
		})
	case "(:-)/2":
		var h Variable
		args.Unify(Cons(&h, &Variable{}), false)
		name, _, err = nameArgs(&h)
		if err != nil {
			return Error(err)
		}
	}

	if e.procedures == nil {
		e.procedures = map[string]procedure{}
	}
	p, ok := e.procedures[name]
	if !ok {
		p = clauses{}
	}

	cs, ok := p.(clauses)
	if !ok {
		return Error(errors.New("builtin"))
	}
	c := clause{name: name}
	if err := c.compile(t); err != nil {
		return Error(err)
	}

	e.procedures[name] = merge(cs, c)
	return Delay(k)
}

func nameArgs(t Term) (string, Term, error) {
	switch f := Resolve(t).(type) {
	case Atom:
		return PrincipalFunctor(f, 0).String(), List(), nil
	case *Compound:
		return PrincipalFunctor(f.Functor, Integer(len(f.Args))).String(), List(f.Args...), nil
	default:
		return "", nil, errors.New("not callable")
	}
}

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

func (e *EngineState) BagOf(template, goal, bag Term, k func() Promise) Promise {
	return e.collectionOf(template, goal, bag, k, List)
}

func (e *EngineState) SetOf(template, goal, bag Term, k func() Promise) Promise {
	return e.collectionOf(template, goal, bag, k, Set)
}

func (e *EngineState) collectionOf(template, goal, collection Term, k func() Promise, agg func(...Term) Term) Promise {
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

	b := newAssignment(collection)
	ks := make([]func() Promise, len(solutions))
	for i := range solutions {
		s := solutions[i]
		ks[i] = func() Promise {
			b.reset()

			// revert to snapshot
			for i, v := range groupingVariables {
				v.Ref = s.snapshots[i]
			}

			return Unify(collection, agg(s.bag...), k)
		}
	}
	return Delay(ks...)
}

func Compare(order, term1, term2 Term, k func() Promise) Promise {
	d := compare(term1, term2)
	switch {
	case d < 0:
		return Unify(Atom("<"), order, k)
	case d == 0:
		return Unify(Atom("="), order, k)
	case d > 0:
		return Unify(Atom(">"), order, k)
	default:
		return Error(errors.New("unreachable"))
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

type Exception struct {
	Term
}

func (e *Exception) Error() string {
	return e.String()
}

func Throw(t Term, _ func() Promise) Promise {
	return Error(&Exception{Term: Resolve(t).Copy()})
}

func (e *EngineState) Catch(goal, catcher, recover Term, k func() Promise) Promise {
	ok, err := e.Call(goal, Done).Force()
	if err != nil {
		if ex, ok := err.(*Exception); ok && catcher.Unify(ex.Term, false) {
			return e.Call(recover, k)
		}
		return Error(err)
	}
	if !ok {
		return Bool(false)
	}
	return Delay(k)
}

func (e *EngineState) CurrentPredicate(pf Term, k func() Promise) Promise {
	var conv map[rune]rune
	if e.charConvEnabled {
		conv = e.charConversions
	}

	a := newAssignment(pf)
	ks := make([]func() Promise, 0, len(e.procedures))
	for key := range e.procedures {
		p := NewParser(bufio.NewReader(strings.NewReader(key)), &Operators{
			{Precedence: 400, Type: "yfx", Name: "/"},
		}, conv)
		t, err := p.Term()
		if err != nil {
			return Error(err)
		}

		ks = append(ks, func() Promise {
			a.reset()
			return Unify(pf, t, k)
		})
	}
	return Delay(ks...)
}

func (e *EngineState) Retract(t Term, k func() Promise) Promise {
	t = Rulify(t)

	h := t.(*Compound).Args[0]
	name, _, err := nameArgs(h)
	if err != nil {
		return Error(err)
	}

	p, ok := e.procedures[name]
	if !ok {
		return Bool(false)
	}

	cs, ok := p.(clauses)
	if !ok {
		return Error(errors.New("not retractable"))
	}

	updated := make(clauses, 0, len(cs))
	defer func() { e.procedures[name] = updated }()

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

func (e *EngineState) Abolish(t Term, k func() Promise) Promise {
	delete(e.procedures, t.String())
	return Delay(k)
}

func (e *EngineState) CurrentInput(stream Term, k func() Promise) Promise {
	return Delay(func() Promise {
		return Unify(stream, e.input, k)
	})
}

func (e *EngineState) CurrentOutput(stream Term, k func() Promise) Promise {
	return Delay(func() Promise {
		return Unify(stream, e.output, k)
	})
}

func (e *EngineState) SetInput(stream Term, k func() Promise) Promise {
	s, err := e.stream(stream)
	if err != nil {
		return Error(err)
	}
	e.input = s
	return Delay(k)
}

func (e *EngineState) SetOutput(stream Term, k func() Promise) Promise {
	s, err := e.stream(stream)
	if err != nil {
		return Error(err)
	}
	e.output = s
	return Delay(k)
}

func (e *EngineState) Open(filename, mode, stream, options Term, k func() Promise) Promise {
	filename, mode, options = Resolve(filename), Resolve(mode), Resolve(options)

	n, ok := filename.(Atom)
	if !ok {
		return Error(errors.New("not an atom"))
	}

	var (
		flag   int
		perm   os.FileMode
		buffer bool

		typ       = Atom("text")
		alias     Atom
		eofAction = Atom("error")
	)

	switch mode {
	case Atom("read"):
		flag = os.O_RDONLY
		buffer = true
	case Atom("write"):
		flag = os.O_CREATE | os.O_WRONLY
		perm = 0644
	case Atom("append"):
		flag = os.O_APPEND | os.O_CREATE | os.O_WRONLY
		perm = 0644
	default:
		return Error(errors.New("unknown mode"))
	}

	if err := Each(options, func(option Term) error {
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
			return errors.New("unknown option")
		}
		return nil
	}); err != nil {
		return Error(err)
	}

	f, err := os.OpenFile(string(n), flag, perm)
	if err != nil {
		return Error(err)
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

	if alias != "" {
		if e.streams == nil {
			e.streams = map[Atom]*Stream{}
		}
		e.streams[alias] = &s
	}

	return Delay(func() Promise {
		return Unify(stream, &s, k)
	})
}

func (e *EngineState) Close(stream, options Term, k func() Promise) Promise {
	s, err := e.stream(stream)
	if err != nil {
		return Error(err)
	}

	var force bool
	if err := Each(Resolve(options), func(option Term) error {
		switch {
		case option.Unify(&Compound{Functor: "force", Args: []Term{Atom("false")}}, false):
			force = false
		case option.Unify(&Compound{Functor: "force", Args: []Term{Atom("true")}}, false):
			force = true
		default:
			return errors.New("unknown option")
		}
		return nil
	}); err != nil {
		return Error(err)
	}

	if err := s.Close(); err != nil && !force {
		return Error(err)
	}

	return Delay(k)
}

func (e *EngineState) FlushOutput(stream Term, k func() Promise) Promise {
	s, err := e.stream(stream)
	if err != nil {
		return Error(err)
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

func (e *EngineState) WriteTerm(stream, term, options Term, k func() Promise) Promise {
	s, err := e.stream(stream)
	if err != nil {
		return Error(err)
	}

	opts := WriteTermOptions{Ops: e.operators}
	if err := Each(Resolve(options), func(option Term) error {
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
			return errors.New("unknown option")
		}
		return nil
	}); err != nil {
		return Error(err)
	}

	if err := Resolve(term).WriteTerm(s, opts); err != nil {
		return Error(err)
	}

	return Delay(k)
}

func CharCode(char, code Term, k func() Promise) Promise {
	char, code = Resolve(char), Resolve(code)

	if c, ok := char.(Atom); ok {
		rs := []rune(c)
		if len(rs) != 1 {
			return Error(errors.New("not a character"))
		}

		return Delay(func() Promise {
			return Unify(code, Integer(rs[0]), k)
		})
	}

	c, ok := code.(Integer)
	if !ok {
		return Error(errors.New("not a code"))
	}

	return Delay(func() Promise {
		return Unify(char, Atom(rune(c)), k)
	})
}

func (e *EngineState) PutByte(stream, byt Term, k func() Promise) Promise {
	s, err := e.stream(stream)
	if err != nil {
		return Error(err)
	}

	b, ok := Resolve(byt).(Integer)
	if !ok || 0 > b || 255 < b {
		return Error(errors.New("not a byte"))
	}

	if _, err := s.Write([]byte{byte(b)}); err != nil {
		return Error(err)
	}

	return Delay(k)
}

func (e *EngineState) PutCode(stream, code Term, k func() Promise) Promise {
	s, err := e.stream(stream)
	if err != nil {
		return Error(err)
	}

	c, ok := Resolve(code).(Integer)
	if !ok {
		return Error(errors.New("not an integer"))
	}

	if _, err := s.Write([]byte(string(rune(c)))); err != nil {
		return Error(err)
	}

	return Delay(k)
}

func (e *EngineState) ReadTerm(stream, term, options Term, k func() Promise) Promise {
	s, err := e.stream(stream)
	if err != nil {
		return Error(err)
	}

	var opts readTermOptions
	if err := Each(Resolve(options), func(option Term) error {
		var v Variable
		switch {
		case option.Unify(&Compound{Functor: "singletons", Args: []Term{&v}}, false):
			opts.singletons = &v
		case option.Unify(&Compound{Functor: "variables", Args: []Term{&v}}, false):
			opts.variables = &v
		case option.Unify(&Compound{Functor: "variable_names", Args: []Term{&v}}, false):
			opts.variableNames = &v
		default:
			return errors.New("unknown option")
		}
		return nil
	}); err != nil {
		return Error(err)
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

func (e *EngineState) GetByte(stream, byt Term, k func() Promise) Promise {
	s, err := e.stream(stream)
	if err != nil {
		return Error(err)
	}

	b := make([]byte, 1)
	_, err = s.Read(b)
	switch err {
	case nil:
		return Delay(func() Promise {
			return Unify(byt, Integer(b[0]), k)
		})
	case io.EOF:
		return Delay(func() Promise {
			return Unify(byt, Integer(-1), k)
		})
	default:
		return Error(err)
	}
}

func (e *EngineState) GetCode(stream, code Term, k func() Promise) Promise {
	s, err := e.stream(stream)
	if err != nil {
		return Error(err)
	}

	if s.Reader == nil {
		return Error(errors.New("not an input stream"))
	}

	br, ok := s.Reader.(*bufio.Reader)
	if !ok {
		return Error(errors.New("not a buffered stream"))
	}

	r, _, err := br.ReadRune()
	switch err {
	case nil:
		return Delay(func() Promise {
			return Unify(code, Integer(r), k)
		})
	case io.EOF:
		return Delay(func() Promise {
			return Unify(code, Integer(-1), k)
		})
	default:
		return Error(err)
	}
}

func (e *EngineState) PeekByte(stream, byt Term, k func() Promise) Promise {
	s, err := e.stream(stream)
	if err != nil {
		return Error(err)
	}

	if s.Reader == nil {
		return Error(errors.New("not an input stream"))
	}

	br, ok := s.Reader.(*bufio.Reader)
	if !ok {
		return Error(errors.New("not a buffered stream"))
	}

	b, err := br.Peek(1)
	switch err {
	case nil:
		return Delay(func() Promise {
			return Unify(byt, Integer(b[0]), k)
		})
	case io.EOF:
		return Delay(func() Promise {
			return Unify(byt, Integer(-1), k)
		})
	default:
		return Error(err)
	}
}

func (e *EngineState) PeekCode(stream, code Term, k func() Promise) Promise {
	s, err := e.stream(stream)
	if err != nil {
		return Error(err)
	}

	if s.Reader == nil {
		return Error(errors.New("not an input stream"))
	}

	br, ok := s.Reader.(*bufio.Reader)
	if !ok {
		return Error(errors.New("not a buffered stream"))
	}

	r, _, err := br.ReadRune()
	switch err {
	case nil:
		if err := br.UnreadRune(); err != nil {
			return Error(err)
		}
		return Delay(func() Promise {
			return Unify(code, Integer(r), k)
		})
	case io.EOF:
		return Delay(func() Promise {
			return Unify(code, Integer(-1), k)
		})
	default:
		return Error(err)
	}
}

func (e *EngineState) Halt(n Term, k func() Promise) Promise {
	code, ok := Resolve(n).(Integer)
	if !ok {
		return Error(errors.New("not an integer"))
	}

	for _, f := range e.BeforeHalt {
		f()
	}

	os.Exit(int(code))

	return Delay(k)
}

func (e *EngineState) Clause(head, body Term, k func() Promise) Promise {
	head, body = Resolve(head), Resolve(body)
	pattern := &Compound{Functor: ":-", Args: []Term{head, body}}
	a := newAssignment(head, body)

	for _, p := range e.procedures {
		cs, ok := p.(clauses)
		if !ok {
			continue
		}

		ks := make([]func() Promise, len(cs))
		for i, c := range cs {
			ks[i] = func() Promise {
				a.reset()
				return Unify(pattern, Rulify(c.raw), k)
			}
		}
		return Delay(ks...)
	}

	return Bool(false)
}

func AtomLength(atom, integer Term, k func() Promise) Promise {
	a, ok := Resolve(atom).(Atom)
	if !ok {
		return Error(errors.New("not an atom"))
	}

	return Delay(func() Promise {
		return Unify(integer, Integer(len([]rune(a))), k)
	})
}

func AtomConcat(atom1, atom2, atom3 Term, k func() Promise) Promise {
	if a1, ok := Resolve(atom1).(Atom); ok {
		if a2, ok := Resolve(atom2).(Atom); ok {
			return Delay(func() Promise {
				return Unify(a1+a2, atom3, k)
			})
		}
	}

	a3, ok := Resolve(atom3).(Atom)
	if !ok {
		return Error(errors.New("not an atom"))
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
}

func SubAtom(atom, before, length, after, subAtom Term, k func() Promise) Promise {
	whole, ok := Resolve(atom).(Atom)
	if !ok {
		return Error(errors.New("not an atom"))
	}

	rs := []rune(whole)

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
}

func AtomChars(atom, chars Term, k func() Promise) Promise {
	a, ok := Resolve(atom).(Atom)
	if !ok {
		var sb strings.Builder
		if err := Each(Resolve(chars), func(elem Term) error {
			e, ok := Resolve(elem).(Atom)
			if !ok {
				return errors.New("not an atom")
			}
			_, err := sb.WriteString(string(e))
			return err
		}); err != nil {
			return Error(err)
		}
		return Delay(func() Promise {
			return Unify(atom, Atom(sb.String()), k)
		})
	}

	rs := []rune(a)
	cs := make([]Term, len(rs))
	for i, r := range rs {
		cs[i] = Atom(r)
	}
	return Delay(func() Promise {
		return Unify(chars, List(cs...), k)
	})
}

func AtomCodes(atom, codes Term, k func() Promise) Promise {
	a, ok := Resolve(atom).(Atom)
	if !ok {
		var sb strings.Builder
		if err := Each(Resolve(codes), func(elem Term) error {
			e, ok := Resolve(elem).(Integer)
			if !ok {
				return errors.New("not an atom")
			}
			_, err := sb.WriteRune(rune(e))
			return err
		}); err != nil {
			return Error(err)
		}
		return Delay(func() Promise {
			return Unify(atom, Atom(sb.String()), k)
		})
	}

	rs := []rune(a)
	cs := make([]Term, len(rs))
	for i, r := range rs {
		cs[i] = Integer(r)
	}
	return Delay(func() Promise {
		return Unify(codes, List(cs...), k)
	})
}

func NumberChars(num, chars Term, k func() Promise) Promise {
	switch n := Resolve(num).(type) {
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
		var sb strings.Builder
		if err := Each(Resolve(chars), func(elem Term) error {
			e, ok := Resolve(elem).(Atom)
			if !ok {
				return errors.New("not an atom")
			}
			_, err := sb.WriteString(string(e))
			return err
		}); err != nil {
			return Error(err)
		}

		p := NewParser(bufio.NewReader(strings.NewReader(sb.String())), &Operators{}, map[rune]rune{})
		t, err := p.Term()
		if err != nil {
			return Error(err)
		}

		switch t := t.(type) {
		case Integer, Float:
			return Delay(func() Promise {
				return Unify(num, t, k)
			})
		default:
			return Error(errors.New("not a number"))
		}
	}
}

func NumberCodes(num, chars Term, k func() Promise) Promise {
	switch n := Resolve(num).(type) {
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
			return Unify(chars, List(cs...), k)
		})
	default:
		var sb strings.Builder
		if err := Each(Resolve(chars), func(elem Term) error {
			e, ok := Resolve(elem).(Integer)
			if !ok {
				return errors.New("not an integer")
			}
			_, err := sb.WriteRune(rune(e))
			return err
		}); err != nil {
			return Error(err)
		}

		p := NewParser(bufio.NewReader(strings.NewReader(sb.String())), &Operators{}, map[rune]rune{})
		t, err := p.Term()
		if err != nil {
			return Error(err)
		}

		switch t := t.(type) {
		case Integer, Float:
			return Delay(func() Promise {
				return Unify(num, t, k)
			})
		default:
			return Error(errors.New("not a number"))
		}
	}
}

type FunctionSet struct {
	Unary  map[Atom]func(x Term) (Term, error)
	Binary map[Atom]func(x, y Term) (Term, error)
}

func (fs FunctionSet) Is(lhs, rhs Term, k func() Promise) Promise {
	v, err := fs.eval(rhs)
	if err != nil {
		return Error(err)
	}
	return Delay(func() Promise {
		return Unify(lhs, v, k)
	})
}

func (fs FunctionSet) Equal(lhs, rhs Term, k func() Promise) Promise {
	return fs.compare(lhs, rhs, k, func(i Integer, j Integer) bool {
		return i == j
	}, func(f Float, g Float) bool {
		return f == g
	})
}

func (fs FunctionSet) NotEqual(lhs, rhs Term, k func() Promise) Promise {
	return fs.compare(lhs, rhs, k, func(i Integer, j Integer) bool {
		return i != j
	}, func(f Float, g Float) bool {
		return f != g
	})
}

func (fs FunctionSet) LessThan(lhs, rhs Term, k func() Promise) Promise {
	return fs.compare(lhs, rhs, k, func(i Integer, j Integer) bool {
		return i < j
	}, func(f Float, g Float) bool {
		return f < g
	})
}

func (fs FunctionSet) GreaterThan(lhs, rhs Term, k func() Promise) Promise {
	return fs.compare(lhs, rhs, k, func(i Integer, j Integer) bool {
		return i > j
	}, func(f Float, g Float) bool {
		return f > g
	})
}

func (fs FunctionSet) LessThanOrEqual(lhs, rhs Term, k func() Promise) Promise {
	return fs.compare(lhs, rhs, k, func(i Integer, j Integer) bool {
		return i <= j
	}, func(f Float, g Float) bool {
		return f <= g
	})
}

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
			return Error(errors.New("not a number"))
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
			return Error(errors.New("not a number"))
		}
	default:
		return Error(errors.New("not a number"))
	}
}

func (fs FunctionSet) eval(t Term) (Term, error) {
	switch t := Resolve(t).(type) {
	case Atom:
		return t, nil // TODO: constants?
	case Integer, Float:
		return t, nil
	case *Compound:
		switch len(t.Args) {
		case 1:
			f, ok := fs.Unary[t.Functor]
			if !ok {
				return nil, fmt.Errorf("unknown unary function %s", t.Functor)
			}
			x, err := fs.eval(t.Args[0])
			if err != nil {
				return nil, err
			}
			return f(x)
		case 2:
			f, ok := fs.Binary[t.Functor]
			if !ok {
				return nil, fmt.Errorf("unknown binary function %s", t.Functor)
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
			return nil, fmt.Errorf("invalid arity %s/%d", t.Functor, len(t.Args))
		}
	default:
		return nil, errors.New("failed to evaluate")
	}
}

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
			return nil, errors.New("not an integer")
		}

		return Integer(f(int64(i))), nil
	}
}

func binaryInteger(f func(i, j int64) int64) func(Term, Term) (Term, error) {
	return func(x, y Term) (Term, error) {
		i, ok := Resolve(x).(Integer)
		if !ok {
			return nil, errors.New("not an integer")
		}

		j, ok := Resolve(y).(Integer)
		if !ok {
			return nil, errors.New("not an integer")
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
			return nil, errors.New("not a number")
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
				return nil, errors.New("not a number")
			}
		case Float:
			switch y := Resolve(y).(type) {
			case Integer:
				return Float(f(float64(x), float64(y))), nil
			case Float:
				return Float(f(float64(x), float64(y))), nil
			default:
				return nil, errors.New("not a number")
			}
		default:
			return nil, errors.New("not a number")
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
			return nil, errors.New("not a number")
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
				return nil, errors.New("not a number")
			}
		case Float:
			switch y := Resolve(y).(type) {
			case Integer:
				return Float(ff(float64(x), float64(y))), nil
			case Float:
				return Float(ff(float64(x), float64(y))), nil
			default:
				return nil, errors.New("not a number")
			}
		default:
			return nil, errors.New("not a number")
		}
	}
}

func (e *EngineState) StreamProperty(stream, property Term, k func() Promise) Promise {
	s, err := e.stream(stream)
	if err != nil {
		return Error(err)
	}

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
	ks := make([]func() Promise, len(properties))
	for i := range properties {
		p := properties[i]
		ks[i] = func() Promise {
			a.reset()
			return Unify(property, p, k)
		}
	}
	return Delay(ks...)
}

func (e *EngineState) SetStreamPosition(stream, pos Term, k func() Promise) Promise {
	s, err := e.stream(stream)
	if err != nil {
		return Error(err)
	}

	p, ok := Resolve(pos).(Integer)
	if !ok {
		return Error(errors.New("not an integer"))
	}

	f, ok := s.Closer.(*os.File)
	if !ok {
		return Error(errors.New("not a repositionable stream"))
	}

	if _, err := f.Seek(int64(p), 0); err != nil {
		return Error(err)
	}

	if br, ok := s.Reader.(*bufio.Reader); ok {
		br.Reset(f)
	}

	return Delay(k)
}

func (e *EngineState) CharConversion(char1, char2 Term, k func() Promise) Promise {
	c1, ok := Resolve(char1).(Atom)
	if !ok {
		return Error(errors.New("not an atom"))
	}

	r1 := []rune(c1)
	if len(r1) != 1 {
		return Error(errors.New("not a char"))
	}

	c2, ok := Resolve(char2).(Atom)
	if !ok {
		return Error(errors.New("not an atom"))
	}

	r2 := []rune(c2)
	if len(r2) != 1 {
		return Error(errors.New("not a char"))
	}

	if e.charConversions == nil {
		e.charConversions = map[rune]rune{}
	}
	if r1[0] == r2[0] {
		delete(e.charConversions, r1[0])
		return Delay(k)
	}
	e.charConversions[r1[0]] = r2[0]
	return Delay(k)
}

func (e *EngineState) CurrentCharConversion(char1, char2 Term, k func() Promise) Promise {
	if c1, ok := Resolve(char1).(Atom); ok {
		r := []rune(c1)
		if len(r) != 1 {
			return Error(errors.New("not a char"))
		}
		if r, ok := e.charConversions[r[0]]; ok {
			return Delay(func() Promise {
				return Unify(char2, Atom(r), k)
			})
		}
		return Delay(func() Promise {
			return Unify(char2, c1, k)
		})
	}

	a := newAssignment(char1, char2)
	pattern := Compound{Args: []Term{char1, char2}}
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

func (e *EngineState) SetPrologFlag(flag, value Term, k func() Promise) Promise {
	f, ok := Resolve(flag).(Atom)
	if !ok {
		return Error(errors.New("not an atom"))
	}

	switch f {
	case "bounded", "max_integer", "min_integer", "integer_rounding_function", "max_arity":
		return Error(errors.New("unchangeable flag"))
	case "char_conversion":
		a, ok := Resolve(value).(Atom)
		if !ok {
			return Error(errors.New("not an atom"))
		}
		switch a {
		case "on":
			e.charConvEnabled = true
			return Delay(k)
		case "off":
			e.charConvEnabled = false
			return Delay(k)
		default:
			return Error(errors.New("unknown value"))
		}
	case "debug":
		a, ok := Resolve(value).(Atom)
		if !ok {
			return Error(errors.New("not an atom"))
		}
		switch a {
		case "on":
			e.debug = true
			return Delay(k)
		case "off":
			e.debug = false
			return Delay(k)
		default:
			return Error(errors.New("unknown value"))
		}
	case "unknown":
		a, ok := Resolve(value).(Atom)
		if !ok {
			return Error(errors.New("not an atom"))
		}
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
			return Error(errors.New("unknown value"))
		}
	default:
		return Error(errors.New("unknown flag"))
	}
}

func (e *EngineState) CurrentPrologFlag(flag, value Term, k func() Promise) Promise {
	flag, value = Resolve(flag), Resolve(value)
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

func (e *EngineState) stream(streamOrAtom Term) (*Stream, error) {
	switch s := Resolve(streamOrAtom).(type) {
	case Atom:
		v, ok := e.streams[s]
		if !ok {
			return nil, errors.New("unknown stream alias")
		}
		return v, nil
	case *Stream:
		return s, nil
	default:
		return nil, errors.New("not a stream")
	}
}
