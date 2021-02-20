package prolog

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"math"
	"os"
	"sort"
	"strings"
)

func (e *Engine) Call(t Term, k func() (bool, error)) (bool, error) {
	name, args, err := nameArgs(t)
	if err != nil {
		return false, err
	}
	return e.arrive(name, args, k)
}

var errCut = errors.New("cut")

func Cut(k func() (bool, error)) (bool, error) {
	ok, err := k()
	if err != nil {
		if errors.Is(err, errCut) {
			return ok, err
		}
		return false, err
	}
	return ok, errCut
}

func Unify(t1, t2 Term, k func() (bool, error)) (bool, error) {
	a := newAssignment(t1, t2)
	if !t1.Unify(t2, false) {
		a.reset()
		return false, nil
	}
	return k()
}

func UnifyWithOccursCheck(t1, t2 Term, k func() (bool, error)) (bool, error) {
	a := newAssignment(t1, t2)
	if !t1.Unify(t2, true) {
		a.reset()
		return false, nil
	}
	return k()
}

func TypeVar(t Term, k func() (bool, error)) (bool, error) {
	if _, ok := Resolve(t).(*Variable); !ok {
		return false, nil
	}
	return k()
}

func TypeFloat(t Term, k func() (bool, error)) (bool, error) {
	if _, ok := Resolve(t).(Float); !ok {
		return false, nil
	}
	return k()
}

func TypeInteger(t Term, k func() (bool, error)) (bool, error) {
	if _, ok := Resolve(t).(Integer); !ok {
		return false, nil
	}
	return k()
}

func TypeAtom(t Term, k func() (bool, error)) (bool, error) {
	if _, ok := Resolve(t).(Atom); !ok {
		return false, nil
	}
	return k()
}

func TypeCompound(t Term, k func() (bool, error)) (bool, error) {
	if _, ok := Resolve(t).(*Compound); !ok {
		return false, nil
	}
	return k()
}

func Functor(term, name, arity Term, k func() (bool, error)) (bool, error) {
	var v *Variable
	for v == nil {
		switch t := term.(type) {
		case Atom:
			if !t.Unify(name, false) || !Integer(0).Unify(arity, false) {
				return false, nil
			}
			return k()
		case *Variable:
			if t.Ref == nil {
				v = t
				break
			}
			term = t.Ref
		case *Compound:
			if !t.Functor.Unify(name, false) || !Integer(len(t.Args)).Unify(arity, false) {
				return false, nil
			}
			return k()
		default:
			return false, nil
		}
	}

	var n *Atom
	for n == nil {
		switch t := name.(type) {
		case Atom:
			n = &t
		case *Variable:
			if t.Ref == nil {
				return false, errors.New("invalid arguments: atom is not instantiated")
			}
			name = t.Ref
		default:
			return false, fmt.Errorf("invalid arguments: name is %T", name)
		}
	}

	var a *Integer
	for a == nil {
		switch t := arity.(type) {
		case Integer:
			a = &t
		case *Variable:
			if t.Ref == nil {
				return false, errors.New("invalid arguments")
			}
			arity = t.Ref
		default:
			return false, errors.New("invalid arguments")
		}
	}

	if *a == 0 {
		if !v.Unify(*a, false) {
			return false, nil
		}
		return k()
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
		return false, nil
	}
	return k()
}

func Arg(arg, term, value Term, k func() (bool, error)) (bool, error) {
	c, ok := Resolve(term).(*Compound)
	if !ok {
		return false, errors.New("term should be compound")
	}

	n, ok := Resolve(arg).(Integer)
	if !ok {
		a := newAssignment(arg, term, value)
		for i, t := range c.Args {
			if arg.Unify(Integer(i+1), false) && value.Unify(t, false) {
				ok, err := k()
				if err != nil {
					if errors.Is(err, errCut) {
						return ok, err
					}
					return false, err
				}
				if ok {
					return true, nil
				}
			}
			a.reset()
		}
		return false, nil
	}
	if n == 0 || int(n) >= len(c.Args) {
		return false, nil
	}
	if n < 0 {
		return false, errors.New("arg shouldn't be negative")
	}

	if !c.Args[int(n)-1].Unify(value, false) {
		return false, nil
	}

	return k()
}

func Univ(term, list Term, k func() (bool, error)) (bool, error) {
	var c *Compound
	for c == nil {
		switch t := term.(type) {
		case *Variable:
			if t.Ref == nil {
				var car, cdr Variable
				if !list.Unify(Cons(&car, &cdr), false) {
					return false, errors.New("invalid argument")
				}
				var f *Atom
				for f == nil {
					switch t := car.Ref.(type) {
					case Atom:
						f = &t
					case *Variable:
						if t.Ref == nil {
							return false, errors.New("invalid argument")
						}
						car.Ref = t.Ref
					default:
						return false, errors.New("invalid argument")
					}
				}

				list = cdr.Ref

				var args []Term
				for list != Atom("[]") {
					var car, cdr Variable
					if !list.Unify(Cons(&car, &cdr), false) {
						return false, errors.New("invalid argument")
					}
					args = append(args, car.Ref)
					list = cdr.Ref
				}

				if !term.Unify(&Compound{
					Functor: *f,
					Args:    args,
				}, false) {
					return false, nil
				}
				return k()
			}
			term = t.Ref
		case *Compound:
			c = t
		default:
			return false, errors.New("invalid argument")
		}
	}

	l := List()
	for i := len(c.Args) - 1; i >= 0; i-- {
		l = Cons(c.Args[i], l)
	}
	if !list.Unify(Cons(c.Functor, l), false) {
		return false, nil
	}
	return k()
}

func CopyTerm(in, out Term, k func() (bool, error)) (bool, error) {
	return Unify(in.Copy(), out, k)
}

func (e *Engine) Op(precedence, typ, name Term, k func() (bool, error)) (bool, error) {
	p, ok := Resolve(precedence).(Integer)
	if !ok {
		return false, fmt.Errorf("invalid precedence: %s", precedence)
	}

	t, ok := Resolve(typ).(Atom)
	if !ok {
		return false, fmt.Errorf("invalid type: %s", typ)
	}

	n, ok := Resolve(name).(Atom)
	if !ok {
		return false, fmt.Errorf("invalid name: %s", name)
	}

	// already defined?
	for i, o := range e.operators {
		if o.Type != t || o.Name != n {
			continue
		}

		// remove it first so that we can insert it again in the right position
		copy(e.operators[i:], e.operators[i+1:])
		e.operators[len(e.operators)-1] = operator{}
		e.operators = e.operators[:len(e.operators)-1]

		// or keep it removed.
		if p == 0 {
			return k()
		}
	}

	// insert
	i := sort.Search(len(e.operators), func(i int) bool {
		return e.operators[i].Precedence >= p
	})
	e.operators = append(e.operators, operator{})
	copy(e.operators[i+1:], e.operators[i:])
	e.operators[i] = operator{
		Precedence: p,
		Type:       t,
		Name:       n,
	}

	return k()
}

func (e *Engine) CurrentOp(precedence, typ, name Term, k func() (bool, error)) (bool, error) {
	a := newAssignment(precedence, typ, name)

	for _, op := range e.operators {
		if op.Precedence.Unify(precedence, false) && op.Type.Unify(typ, false) && op.Name.Unify(name, false) {
			ok, err := k()
			if err != nil {
				if errors.Is(err, errCut) {
					return ok, err
				}
				return false, err
			}
			if ok {
				return true, nil
			}
		}
		a.reset()
	}

	return false, nil
}

func (e *Engine) Assertz(t Term, k func() (bool, error)) (bool, error) {
	return e.assert(t, k, func(cs clauses, c clause) clauses {
		return append(cs, c)
	})
}

func (e *Engine) Asserta(t Term, k func() (bool, error)) (bool, error) {
	return e.assert(t, k, func(cs clauses, c clause) clauses {
		return append(clauses{c}, cs...)
	})
}

func (e *Engine) assert(t Term, k func() (bool, error), merge func(clauses, clause) clauses) (bool, error) {
	name, args, err := nameArgs(t)
	if err != nil {
		return false, err
	}

	switch name {
	case "(:-)/1": // directive
		var d Variable
		args.Unify(Cons(&d, &Variable{}), false)
		name, args, err := nameArgs(&d)
		if err != nil {
			return false, err
		}
		ok, err := e.arrive(name, args, k)
		if err != nil {
			return false, err
		}
		return ok, nil
	case "(:-)/2":
		var h Variable
		args.Unify(Cons(&h, &Variable{}), false)
		name, _, err = nameArgs(&h)
		if err != nil {
			return false, err
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
		return false, errors.New("builtin")
	}
	c := clause{name: name}
	if err := c.compile(t); err != nil {
		return false, err
	}

	e.procedures[name] = merge(cs, c)
	return k()
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

func Repeat(k func() (bool, error)) (bool, error) {
	for {
		ok, err := k()
		if err != nil {
			if errors.Is(err, errCut) {
				return ok, err
			}
			return false, err
		}
		if ok {
			return true, nil
		}
	}
}

func (e *Engine) BagOf(template, goal, bag Term, k func() (bool, error)) (bool, error) {
	return e.collectionOf(template, goal, bag, k, List)
}

func (e *Engine) SetOf(template, goal, bag Term, k func() (bool, error)) (bool, error) {
	return e.collectionOf(template, goal, bag, k, Set)
}

func (e *Engine) collectionOf(template, goal, collection Term, k func() (bool, error), agg func(...Term) Term) (bool, error) {
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
	_, err := e.Call(goal, func() (bool, error) {
		snapshots := make([]Term, len(groupingVariables))
		for i, v := range groupingVariables {
			snapshots[i] = v.Ref
		}

	solutions:
		for i, s := range solutions {
			for i := range groupingVariables {
				ok, err := Compare(Atom("="), s.snapshots[i], snapshots[i], Done)
				if err != nil {
					return false, err
				}
				if !ok {
					continue solutions
				}
			}
			solutions[i].bag = append(s.bag, template.Copy())
			return false, nil // ask for more solutions
		}

		solutions = append(solutions, solution{
			snapshots: snapshots,
			bag:       []Term{template.Copy()},
		})
		return false, nil // ask for more solutions
	})
	if err != nil {
		return false, err
	}

	if len(solutions) == 0 {
		return false, nil
	}

	b := newAssignment(collection)
	for _, s := range solutions {
		// revert to snapshot
		for i, v := range groupingVariables {
			v.Ref = s.snapshots[i]
		}

		if collection.Unify(agg(s.bag...), false) {
			ok, err := k()
			if err != nil {
				return false, err
			}
			if ok {
				return ok, nil
			}
		}

		b.reset()
	}

	return false, nil
}

func Compare(order, term1, term2 Term, k func() (bool, error)) (bool, error) {
	d := compare(term1, term2)
	switch {
	case d < 0:
		return Unify(Atom("<"), order, k)
	case d == 0:
		return Unify(Atom("="), order, k)
	case d > 0:
		return Unify(Atom(">"), order, k)
	default:
		return false, errors.New("unreachable")
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

func Throw(t Term, _ func() (bool, error)) (bool, error) {
	return false, &Exception{Term: Resolve(t).Copy()}
}

func (e *Engine) Catch(goal, catcher, recover Term, k func() (bool, error)) (bool, error) {
	ok, err := e.Call(goal, Done)
	if err != nil {
		if ex, ok := err.(*Exception); ok && catcher.Unify(ex.Term, false) {
			return e.Call(recover, k)
		}
		return false, err
	}
	if !ok {
		return false, nil
	}
	return k()
}

func (e *Engine) CurrentPredicate(pf Term, k func() (bool, error)) (bool, error) {
	a := newAssignment(pf)
	for key := range e.procedures {
		p := NewParser(strings.NewReader(key), &operators{
			{Precedence: 400, Type: "yfx", Name: "/"},
		})
		t, err := p.Term()
		if err != nil {
			return false, err
		}

		if pf.Unify(t, false) {
			ok, err := k()
			if err != nil {
				return false, err
			}
			if ok {
				return true, nil
			}
		}

		a.reset()
	}
	return false, nil
}

func (e *Engine) Retract(t Term, k func() (bool, error)) (bool, error) {
	t = Rulify(t)

	h := t.(*Compound).Args[0]
	name, _, err := nameArgs(h)
	if err != nil {
		return false, err
	}

	p, ok := e.procedures[name]
	if !ok {
		return false, nil
	}

	cs, ok := p.(clauses)
	if !ok {
		return false, errors.New("not retractable")
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

		ok, err := k()
		if err != nil {
			updated = append(updated, cs[i+1:]...)
			a.reset()
			return false, err
		}
		if ok {
			updated = append(updated, cs[i+1:]...)
			a.reset()
			return true, nil
		}

		a.reset()
	}

	return false, nil
}

func (e *Engine) Abolish(t Term, k func() (bool, error)) (bool, error) {
	delete(e.procedures, t.String())
	return k()
}

func (e *Engine) CurrentInput(stream Term, k func() (bool, error)) (bool, error) {
	return Unify(stream, e.input, k)
}

func (e *Engine) CurrentOutput(stream Term, k func() (bool, error)) (bool, error) {
	return Unify(stream, e.output, k)
}

func (e *Engine) SetInput(stream Term, k func() (bool, error)) (bool, error) {
	stream = Resolve(stream)

	if a, ok := stream.(Atom); ok {
		stream, ok = e.globalVars[a]
		if !ok {
			return false, errors.New("unknown global variable")
		}
	}

	s, ok := stream.(Stream)
	if !ok {
		return false, errors.New("not a stream")
	}
	e.input = s

	return k()
}

func (e *Engine) SetOutput(stream Term, k func() (bool, error)) (bool, error) {
	stream = Resolve(stream)

	if a, ok := stream.(Atom); ok {
		stream, ok = e.globalVars[a]
		if !ok {
			return false, errors.New("unknown global variable")
		}
	}

	s, ok := stream.(Stream)
	if !ok {
		return false, errors.New("not a stream")
	}
	e.output = s

	return k()
}

func (e *Engine) Open(filename, mode, stream, options Term, k func() (bool, error)) (bool, error) {
	filename, mode, options = Resolve(filename), Resolve(mode), Resolve(options)

	n, ok := filename.(Atom)
	if !ok {
		return false, errors.New("not an atom")
	}

	var (
		flag int
		perm os.FileMode
	)
	switch mode {
	case Atom("read"):
		flag = os.O_RDONLY
	case Atom("write"):
		flag = os.O_CREATE | os.O_WRONLY
		perm = 0644
	case Atom("append"):
		flag = os.O_APPEND | os.O_CREATE | os.O_WRONLY
		perm = 0644
	default:
		return false, errors.New("unknown mode")
	}

	var alias Atom
	if err := Each(options, func(option Term) error {
		var arg Variable
		switch {
		case option.Unify(&Compound{Functor: "type", Args: []Term{Atom("text")}}, false):
			// TODO: don't know what to do.
		case option.Unify(&Compound{Functor: "type", Args: []Term{Atom("binary")}}, false):
			// TODO: don't know what to do.
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
			// TODO:
		case option.Unify(&Compound{Functor: "eof_action", Args: []Term{Atom("eof_code")}}, false):
			// TODO:
		case option.Unify(&Compound{Functor: "eof_action", Args: []Term{Atom("reset")}}, false):
			// TODO:
		default:
			return errors.New("unknown option")
		}
		return nil
	}); err != nil {
		return false, err
	}

	f, err := os.OpenFile(string(n), flag, perm)
	if err != nil {
		return false, err
	}

	s := Stream{ReadWriteCloser: f}

	if alias != "" {
		if e.globalVars == nil {
			e.globalVars = map[Atom]Term{}
		}
		e.globalVars[alias] = s
	}

	return Unify(stream, s, k)
}

func (e *Engine) Close(stream, options Term, k func() (bool, error)) (bool, error) {
	stream, options = Resolve(stream), Resolve(options)

	if a, ok := stream.(Atom); ok {
		v, ok := e.globalVars[a]
		if !ok {
			return false, errors.New("unknown global variable")
		}
		stream = v
	}

	s, ok := stream.(Stream)
	if !ok {
		return false, errors.New("not a stream")
	}

	var force bool
	if err := Each(options, func(option Term) error {
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
		return false, err
	}

	if err := s.Close(); err != nil && !force {
		return false, err
	}

	return k()
}

func (e *Engine) FlushOutput(stream Term, k func() (bool, error)) (bool, error) {
	stream = Resolve(stream)

	if a, ok := stream.(Atom); ok {
		v, ok := e.globalVars[a]
		if !ok {
			return false, errors.New("unknown global variable")
		}
		stream = v
	}

	s, ok := stream.(Stream)
	if !ok {
		return false, errors.New("not a stream")
	}

	if f, ok := s.ReadWriteCloser.(Flusher); ok {
		if err := f.Flush(); err != nil {
			return false, err
		}
	}

	return k()
}

type Flusher interface {
	Flush() error
}

func (e *Engine) WriteTerm(stream, term, options Term, k func() (bool, error)) (bool, error) {
	stream, term, options = Resolve(stream), Resolve(term), Resolve(options)

	if a, ok := stream.(Atom); ok {
		v, ok := e.globalVars[a]
		if !ok {
			return false, errors.New("unknown global variable")
		}
		stream = v
	}

	s, ok := stream.(Stream)
	if !ok {
		return false, errors.New("not a stream")
	}

	opts := WriteTermOptions{ops: e.operators}
	if err := Each(options, func(option Term) error {
		switch {
		case option.Unify(&Compound{Functor: "quoted", Args: []Term{Atom("false")}}, false):
			opts.quoted = false
		case option.Unify(&Compound{Functor: "quoted", Args: []Term{Atom("true")}}, false):
			opts.quoted = true
		case option.Unify(&Compound{Functor: "ignore_ops", Args: []Term{Atom("false")}}, false):
			opts.ops = e.operators
		case option.Unify(&Compound{Functor: "ignore_ops", Args: []Term{Atom("true")}}, false):
			opts.ops = nil
		case option.Unify(&Compound{Functor: "numbervars", Args: []Term{Atom("false")}}, false):
			opts.numberVars = false
		case option.Unify(&Compound{Functor: "numbervars", Args: []Term{Atom("true")}}, false):
			opts.numberVars = true
		default:
			return errors.New("unknown option")
		}
		return nil
	}); err != nil {
		return false, err
	}

	if err := term.WriteTerm(s, opts); err != nil {
		return false, err
	}

	return k()
}

func CharCode(char, code Term, k func() (bool, error)) (bool, error) {
	char, code = Resolve(char), Resolve(code)

	if c, ok := char.(Atom); ok {
		rs := []rune(c)
		if len(rs) != 1 {
			return false, errors.New("not a character")
		}

		return Unify(code, Integer(rs[0]), k)
	}

	c, ok := code.(Integer)
	if !ok {
		return false, errors.New("not a code")
	}

	return Unify(char, Atom(rune(c)), k)
}

func (e *Engine) PutByte(stream, byt Term, k func() (bool, error)) (bool, error) {
	stream, byt = Resolve(stream), Resolve(byt)

	if a, ok := stream.(Atom); ok {
		v, ok := e.globalVars[a]
		if !ok {
			return false, errors.New("unknown global variable")
		}
		stream = v
	}

	s, ok := stream.(Stream)
	if !ok {
		return false, errors.New("not a stream")
	}

	b, ok := byt.(Integer)
	if !ok || 0 > b || 255 < b {
		return false, errors.New("not a byte")
	}

	_, err := s.Write([]byte{byte(b)})
	if err != nil {
		return false, err
	}

	return k()
}

func (e *Engine) ReadTerm(stream, term, options Term, k func() (bool, error)) (bool, error) {
	stream, options = Resolve(stream), Resolve(options)

	if a, ok := stream.(Atom); ok {
		v, ok := e.globalVars[a]
		if !ok {
			return false, errors.New("unknown global variable")
		}
		stream = v
	}

	s, ok := stream.(Stream)
	if !ok {
		return false, errors.New("not a stream")
	}

	var opts ReadTermOptions
	if err := Each(options, func(option Term) error {
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
		return false, err
	}

	p := NewParser(s, &e.operators)

	t, err := p.Clause()
	if err != nil {
		return false, err
	}

	var singletons, variables, variableNames []Term
	for _, vc := range p.vars {
		if vc.count == 1 {
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
		return false, nil
	}

	if opts.variables != nil && !opts.variables.Unify(List(variables...), false) {
		return false, nil
	}

	if opts.variableNames != nil && !opts.variableNames.Unify(List(variableNames...), false) {
		return false, nil
	}

	return Unify(term, t, k)
}

func (e *Engine) GetByte(stream, byt Term, k func() (bool, error)) (bool, error) {
	stream = Resolve(stream)

	if a, ok := stream.(Atom); ok {
		v, ok := e.globalVars[a]
		if !ok {
			return false, errors.New("unknown global variable")
		}
		stream = v
	}

	s, ok := stream.(Stream)
	if !ok {
		return false, errors.New("not a stream")
	}

	b := make([]byte, 1)
	_, err := s.Read(b)
	switch err {
	case nil:
		return Unify(byt, Integer(b[0]), k)
	case io.EOF:
		return Unify(byt, Integer(-1), k)
	default:
		return false, err
	}
}

func (e *Engine) Halt(n Term, k func() (bool, error)) (bool, error) {
	code, ok := Resolve(n).(Integer)
	if !ok {
		return false, errors.New("not an integer")
	}

	if f := e.AtHalt; f != nil {
		f()
	}

	os.Exit(int(code))

	return k()
}

func (e *Engine) Clause(head, body Term, k func() (bool, error)) (bool, error) {
	head, body = Resolve(head), Resolve(body)
	a := newAssignment(head, body)
	pattern := &Compound{
		Functor: ":-",
		Args:    []Term{head, body},
	}

	for _, p := range e.procedures {
		cs, ok := p.(clauses)
		if !ok {
			continue
		}

		for _, c := range cs {
			ok, err := Unify(pattern, Rulify(c.raw), k)
			if err != nil {
				return false, err
			}
			if ok {
				return true, nil
			}
			a.reset()
		}
	}

	return false, nil
}

func AtomLength(atom, integer Term, k func() (bool, error)) (bool, error) {
	a, ok := Resolve(atom).(Atom)
	if !ok {
		return false, errors.New("not an atom")
	}

	return Unify(integer, Integer(len([]rune(a))), k)
}

func AtomConcat(atom1, atom2, atom3 Term, k func() (bool, error)) (bool, error) {
	if a1, ok := Resolve(atom1).(Atom); ok {
		if a2, ok := Resolve(atom2).(Atom); ok {
			return Unify(a1+a2, atom3, k)
		}
	}

	a3, ok := Resolve(atom3).(Atom)
	if !ok {
		return false, errors.New("not an atom")
	}

	pattern := Compound{
		Args: []Term{atom1, atom2},
	}
	a := newAssignment(atom1, atom2)
	for i := range a3 {
		ok, err := Unify(&pattern, &Compound{
			Args: []Term{a3[:i], a3[i:]},
		}, k)
		if err != nil {
			return false, err
		}
		if ok {
			return ok, nil
		}
		a.reset()
	}

	return Unify(&pattern, &Compound{
		Args: []Term{a3, Atom("")},
	}, k)
}

func SubAtom(atom, before, length, after, subAtom Term, k func() (bool, error)) (bool, error) {
	whole, ok := Resolve(atom).(Atom)
	if !ok {
		return false, errors.New("not an atom")
	}

	a := newAssignment(before, length, after, subAtom)

	pattern := Compound{
		Args: []Term{before, length, after, subAtom},
	}

	rs := []rune(whole)
	for i := 0; i <= len(rs); i++ {
		for j := i; j <= len(rs); j++ {
			ok, err := Unify(&pattern, &Compound{
				Args: []Term{Integer(i), Integer(j - i), Integer(len(rs) - j), Atom(rs[i:j])},
			}, k)
			if err != nil {
				return false, err
			}
			if ok {
				return true, nil
			}
			a.reset()
		}
	}

	return false, nil
}

func AtomChars(atom, chars Term, k func() (bool, error)) (bool, error) {
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
			return false, err
		}
		return Unify(atom, Atom(sb.String()), k)
	}

	rs := []rune(a)
	cs := make([]Term, len(rs))
	for i, r := range rs {
		cs[i] = Atom(r)
	}
	return Unify(chars, List(cs...), k)
}

func AtomCodes(atom, codes Term, k func() (bool, error)) (bool, error) {
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
			return false, err
		}
		return Unify(atom, Atom(sb.String()), k)
	}

	rs := []rune(a)
	cs := make([]Term, len(rs))
	for i, r := range rs {
		cs[i] = Integer(r)
	}
	return Unify(codes, List(cs...), k)
}

func NumberChars(num, chars Term, k func() (bool, error)) (bool, error) {
	switch n := Resolve(num).(type) {
	case Integer, Float:
		var buf bytes.Buffer
		if err := n.WriteTerm(&buf, defaultWriteTermOptions); err != nil {
			return false, err
		}
		rs := []rune(buf.String())
		cs := make([]Term, len(rs))
		for i, r := range rs {
			cs[i] = Atom(r)
		}
		return Unify(chars, List(cs...), k)
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
			return false, err
		}

		p := NewParser(strings.NewReader(sb.String()), &operators{})
		t, err := p.Term()
		if err != nil {
			return false, err
		}

		switch t := t.(type) {
		case Integer, Float:
			return Unify(num, t, k)
		default:
			return false, errors.New("not a number")
		}
	}
}

func NumberCodes(num, chars Term, k func() (bool, error)) (bool, error) {
	switch n := Resolve(num).(type) {
	case Integer, Float:
		var buf bytes.Buffer
		if err := n.WriteTerm(&buf, defaultWriteTermOptions); err != nil {
			return false, err
		}
		rs := []rune(buf.String())
		cs := make([]Term, len(rs))
		for i, r := range rs {
			cs[i] = Integer(r)
		}
		return Unify(chars, List(cs...), k)
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
			return false, err
		}

		p := NewParser(strings.NewReader(sb.String()), &operators{})
		t, err := p.Term()
		if err != nil {
			return false, err
		}

		switch t := t.(type) {
		case Integer, Float:
			return Unify(num, t, k)
		default:
			return false, errors.New("not a number")
		}
	}
}

type FunctionSet struct {
	Unary  map[Atom]func(x Term) (Term, error)
	Binary map[Atom]func(x, y Term) (Term, error)
}

func (fs FunctionSet) Is(lhs, rhs Term, k func() (bool, error)) (bool, error) {
	v, err := fs.eval(rhs)
	if err != nil {
		return false, err
	}
	return Unify(lhs, v, k)
}

func (fs FunctionSet) Equal(lhs, rhs Term, k func() (bool, error)) (bool, error) {
	return fs.compare(lhs, rhs, k, func(i Integer, j Integer) bool {
		return i == j
	}, func(f Float, g Float) bool {
		return f == g
	})
}

func (fs FunctionSet) compare(lhs, rhs Term, k func() (bool, error), pi func(Integer, Integer) bool, pf func(Float, Float) bool) (bool, error) {
	l, err := fs.eval(lhs)
	if err != nil {
		return false, err
	}

	r, err := fs.eval(rhs)
	if err != nil {
		return false, err
	}

	switch l := l.(type) {
	case Integer:
		switch r := r.(type) {
		case Integer:
			if pi(l, r) {
				return k()
			} else {
				return false, nil
			}
		case Float:
			if pf(Float(l), r) {
				return k()
			} else {
				return false, nil
			}
		default:
			return false, errors.New("not a number")
		}
	case Float:
		switch r := r.(type) {
		case Integer:
			if pf(l, Float(r)) {
				return k()
			} else {
				return false, nil
			}
		case Float:
			if pf(l, r) {
				return k()
			} else {
				return false, nil
			}
		default:
			return false, errors.New("not a number")
		}
	default:
		return false, errors.New("not a number")
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
