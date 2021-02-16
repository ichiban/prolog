package prolog

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/sirupsen/logrus"
)

const (
	opVoid byte = iota
	opEnter
	opCall
	opExit
	opConst
	opVar
	opFunctor
	opPop
)

type Engine struct {
	operators     operators
	procedures    map[string]procedure
	globalVars    map[Atom]Term
	input, output Stream
	AtHalt        func()
}

func NewEngine(in io.Reader, out io.Writer) (*Engine, error) {
	if in == nil {
		in = os.Stdin
	}
	if out == nil {
		out = os.Stdout
	}
	input, output := Stream{ReadWriteCloser: &input{Reader: in}}, Stream{ReadWriteCloser: &output{Writer: out}}
	e := Engine{
		globalVars: map[Atom]Term{
			"user_input":  input,
			"user_output": output,
		},
		input:  input,
		output: output,
	}
	e.Register0("!", Cut)
	e.Register0("repeat", Repeat)
	e.Register1("call", e.Call)
	e.Register1("current_predicate", e.CurrentPredicate)
	e.Register1("assertz", e.Assertz)
	e.Register1("asserta", e.Asserta)
	e.Register1("retract", e.Retract)
	e.Register1("abolish", e.Abolish)
	e.Register1("var", TypeVar)
	e.Register1("float", TypeFloat)
	e.Register1("integer", TypeInteger)
	e.Register1("atom", TypeAtom)
	e.Register1("compound", TypeCompound)
	e.Register1("throw", Throw)
	e.Register2("=", Unify)
	e.Register2("unify_with_occurs_check", UnifyWithOccursCheck)
	e.Register2("=..", Univ)
	e.Register2("copy_term", CopyTerm)
	e.Register3("arg", Arg)
	e.Register3("bagof", e.BagOf)
	e.Register3("setof", e.SetOf)
	e.Register3("catch", e.Catch)
	e.Register3("functor", Functor)
	e.Register3("op", e.Op)
	e.Register3("compare", Compare)
	e.Register3("current_op", e.CurrentOp)
	e.Register1("current_input", e.CurrentInput)
	e.Register1("current_output", e.CurrentOutput)
	e.Register1("set_input", e.SetInput)
	e.Register1("set_output", e.SetOutput)
	e.Register4("open", e.Open)
	e.Register2("close", e.Close)
	e.Register1("flush_output", e.FlushOutput)
	e.Register3("write_term", e.WriteTerm)
	e.Register2("char_code", CharCode)
	e.Register2("put_byte", e.PutByte)
	e.Register3("read_term", e.ReadTerm)
	e.Register2("get_byte", e.GetByte)
	e.Register1("halt", e.Halt)
	e.Register2("clause", e.Clause)
	e.Register2("atom_length", AtomLength)
	e.Register3("atom_concat", AtomConcat)
	e.Register5("sub_atom", SubAtom)
	e.Register2("atom_chars", AtomChars)
	e.Register2("number_chars", NumberChars)
	err := e.Load(`
/*
 *  bootstrap script
 */

% operators
:-(op(1200, xfx, :-)).
:-(op(1200, xfx, -->)).
:-(op(1200, fx, :-)).
:-(op(1200, fx, ?-)).
:-(op(1100, xfy, ;)).
:-(op(1050, xfy, ->)).
:-(op(1000, xfy, ,)).
:-(op(900, fy, \+)).
:-(op(700, xfx, =)).
:-(op(700, xfx, \=)).
:-(op(700, xfx, ==)).
:-(op(700, xfx, \==)).
:-(op(700, xfx, @<)).
:-(op(700, xfx, @=<)).
:-(op(700, xfx, @>)).
:-(op(700, xfx, @>=)).
:-(op(700, xfx, is)).
:-(op(700, xfx, =:=)).
:-(op(700, xfx, =\=)).
:-(op(700, xfx, <)).
:-(op(700, xfx, =<)).
:-(op(700, xfx, =\=)).
:-(op(700, xfx, >)).
:-(op(700, xfx, >=)).
:-(op(700, xfx, =..)).
:-(op(500, yfx, +)).
:-(op(500, yfx, -)).
:-(op(500, yfx, /\)).
:-(op(500, yfx, \/)).
:-(op(400, yfx, *)).
:-(op(400, yfx, /)).
:-(op(400, yfx, //)).
:-(op(400, yfx, rem)).
:-(op(400, yfx, mod)).
:-(op(400, yfx, <<)).
:-(op(400, yfx, >>)).
:-(op(200, xfx, **)).
:-(op(200, xfy, ^)).
:-(op(200, fy, \)).
:-(op(200, fy, +)).
:-(op(200, fy, -)).
:-(op(100, xfx, @)).
:-(op(50, xfx, :)).

% conjunction
P, Q :- call(P), call(Q).

% disjunction
P; Q :- call(P).
P; Q :- call(Q).

% true/false
true.
false :- a = b.
fail :- false.

% if then else
If -> Then; Else :- call(If), !, call(Then).
If -> Then; Else :- !, call(Else).
If -> Then :- call(If), !, call(Then).

% logic and control
\+P :- call(P), !, false.
\+P :- true.
once(P) :- call(P), !.

% not unifiable
X \= Y :- \+(X = Y).

% term comparison
X == Y :- compare(=, X, Y).
X \== Y :- \+(X == Y).
X @< Y :- compare(<, X, Y).
X @=< Y :- compare(=, X, Y).
X @=< Y :- compare(<, X, Y).
X @> Y :- compare(>, X, Y).
X @>= Y :- compare(>, X, Y).
X @>= Y :- compare(=, X, Y).

nonvar(X) :- \+var(X).

number(X) :- float(X).
number(X) :- integer(X).

atomic(X) :- nonvar(X), \+compound(X).

findall(Term, Goal, List) :- bagof(Term, _^Goal, List), !.
findall(Term, Goal, []).

open(Filename, Mode, Stream) :- open(Filename, Mode, Stream, []).

close(Stream) :- close(Stream, []).

flush_output :- current_output(S), flush_output(S).

write_term(Term, Options) :- current_output(S), write_term(S, Term, Options).

write(Stream, Term) :- write_term(Stream, Term, [numbervars(true)]).

write(Term) :- current_output(S), write(S, Term).

write_canonical(Stream, Term) :- write_term(Stream, Term, [quoted(true), ignore_ops(true)]).

write_canonical(Term) :- current_output(S), write_canonical(S, Term).

writeq(Stream, Term) :- write_term(Stream, Term, [quoted(true), numbervars(true)]).

writeq(Term) :- current_output(S), writeq(S, Term).

nl(Stream) :- write_term(Stream, '\n', []).

nl :- current_output(S), nl(S).

put_char(Stream, Char) :- write_term(Stream, Char, []).

put_char(Char) :- current_output(S), put_char(S, Char).

put_code(Stream, Code) :- char_code(C, Code), put_char(Stream, C).

put_code(Code) :- current_output(S), put_code(S, Code).

put_byte(Byte) :- current_output(S), put_byte(S, Byte).

read_term(Term, Options) :- current_input(S), read_term(S, Term, Options).

read(Stream, Term) :- read_term(Stream, Term, []).

read(Term) :- current_input(S), read(S, Term).

get_byte(Byte) :- current_input(S), get_byte(S, Byte).

halt :- halt(0).

atom_codes(Atom, []) :- atom_chars(Atom, []), !.
atom_codes(Atom, [Code|Codes]) :-
  sub_atom(Atom, 0, 1, _, Char),
  sub_atom(Atom, 1, _, 0, Chars),
  char_code(Char, Code),
  atom_codes(Chars, Codes).
`)
	return &e, err
}

type procedure interface {
	Call(*Engine, Term, func() (bool, error)) (bool, error)
}

func (e *Engine) Load(s string) error {
	p := NewParser(strings.NewReader(s), &e.operators)
	for {
		if _, err := p.accept(TokenEOS); err == nil {
			return nil
		}

		t, err := p.Clause()
		if err != nil {
			return err
		}

		if _, err := e.Assertz(t, Done); err != nil {
			return err
		}
	}
}

func (e *Engine) Query(s string, cb func(vars []*Variable) bool) (bool, error) {
	if cb == nil {
		cb = func([]*Variable) bool { return true }
	}

	t, err := NewParser(strings.NewReader(s), &e.operators).Clause()
	if err != nil {
		return false, err
	}

	a := newAssignment(t)

	ok, err := e.Call(t, func() (bool, error) {
		return cb(a), nil
	})
	if err != nil {
		if errors.Is(err, errCut) {
			return ok, nil
		}
		return false, err
	}
	return ok, nil
}

func (e *Engine) TermString(t Term) string {
	var buf bytes.Buffer
	_ = t.WriteTerm(&buf, WriteTermOptions{quoted: true, ops: e.operators})
	return buf.String()
}

func (e *Engine) Register0(name string, p func(func() (bool, error)) (bool, error)) {
	if e.procedures == nil {
		e.procedures = map[string]procedure{}
	}
	e.procedures[PrincipalFunctor(Atom(name), 0).String()] = predicate0(p)
}

func (e *Engine) Register1(name string, p func(Term, func() (bool, error)) (bool, error)) {
	if e.procedures == nil {
		e.procedures = map[string]procedure{}
	}
	e.procedures[PrincipalFunctor(Atom(name), 1).String()] = predicate1(p)
}

func (e *Engine) Register2(name string, p func(Term, Term, func() (bool, error)) (bool, error)) {
	if e.procedures == nil {
		e.procedures = map[string]procedure{}
	}
	e.procedures[PrincipalFunctor(Atom(name), 2).String()] = predicate2(p)
}

func (e *Engine) Register3(name string, p func(Term, Term, Term, func() (bool, error)) (bool, error)) {
	if e.procedures == nil {
		e.procedures = map[string]procedure{}
	}
	e.procedures[PrincipalFunctor(Atom(name), 3).String()] = predicate3(p)
}

func (e *Engine) Register4(name string, p func(Term, Term, Term, Term, func() (bool, error)) (bool, error)) {
	if e.procedures == nil {
		e.procedures = map[string]procedure{}
	}
	e.procedures[PrincipalFunctor(Atom(name), 4).String()] = predicate4(p)
}

func (e *Engine) Register5(name string, p func(Term, Term, Term, Term, Term, func() (bool, error)) (bool, error)) {
	if e.procedures == nil {
		e.procedures = map[string]procedure{}
	}
	e.procedures[PrincipalFunctor(Atom(name), 5).String()] = predicate5(p)
}

func (e *Engine) arrive(name string, args Term, k func() (bool, error)) (bool, error) {
	logrus.WithFields(logrus.Fields{
		"name": name,
		"args": loggableTerm{term: args},
	}).Debug("arrive")
	p := e.procedures[name]
	if p == nil {
		return false, fmt.Errorf("unknown procedure: %s", name)
	}
	return p.Call(e, args, k)
}

type loggableTerm struct {
	term Term
}

func (l loggableTerm) String() string {
	if l.term == nil {
		return "nil"
	}

	opts := defaultWriteTermOptions
	opts.debug = true

	var buf bytes.Buffer
	_ = l.term.WriteTerm(&buf, opts)
	return buf.String()
}

type loggableVars struct {
	vars []*Variable
}

func (l loggableVars) String() string {
	opts := defaultWriteTermOptions
	opts.debug = true

	ret := make([]string, len(l.vars))
	for i, v := range l.vars {
		var buf bytes.Buffer
		_ = v.WriteTerm(&buf, opts)
		ret[i] = buf.String()
	}
	return fmt.Sprint(ret)
}

func (e *Engine) exec(pc bytecode, xr []Term, vars []*Variable, k func() (bool, error), args, astack Term) (bool, error) {
	for len(pc) != 0 {
		log := logrus.WithFields(logrus.Fields{
			"xr":     xr,
			"vars":   loggableVars{vars: vars},
			"args":   loggableTerm{term: args},
			"astack": loggableTerm{term: astack},
		})
		switch pc[0] {
		case opVoid:
			log.Debug("void")
			pc = pc[1:]
		case opConst:
			log.Debugf("const %d", pc[1])
			x := xr[pc[1]]
			var arest Variable
			if !args.Unify(Cons(x, &arest), false) {
				return false, nil
			}
			pc = pc[2:]
			args = &arest
		case opVar:
			log.Debugf("var %d", pc[1])
			v := vars[pc[1]]
			var arest Variable
			if !args.Unify(Cons(v, &arest), false) {
				return false, nil
			}
			pc = pc[2:]
			args = &arest
		case opFunctor:
			log.Debugf("functor %d", pc[1])
			x := xr[pc[1]]
			var arg, arest Variable
			if !args.Unify(Cons(&arg, &arest), false) {
				return false, nil
			}
			var fatom, farity Variable
			if !x.Unify(&Compound{
				Functor: "/",
				Args:    []Term{&fatom, &farity},
			}, false) {
				return false, nil
			}
			ok, err := Functor(&arg, &fatom, &farity, Done)
			if err != nil {
				return false, err
			}
			if !ok {
				return false, nil
			}
			pc = pc[2:]
			args = &Variable{}
			ok, err = Univ(&arg, Cons(&fatom, args), Done)
			if err != nil {
				return false, err
			}
			if !ok {
				return false, nil
			}
			astack = Cons(&arest, astack)
		case opPop:
			log.Debug("pop")
			if !args.Unify(List(), false) {
				return false, nil
			}
			pc = pc[1:]
			var a, arest Variable
			if !astack.Unify(Cons(&a, &arest), false) {
				return false, nil
			}
			args = &a
			astack = &arest
		case opEnter:
			log.Debug("enter")
			if !args.Unify(List(), false) {
				return false, nil
			}
			if !astack.Unify(List(), false) {
				return false, nil
			}
			pc = pc[1:]
			var v Variable
			args = &v
			astack = &v
		case opCall:
			log.Debugf("call %d", pc[1])
			x := xr[pc[1]]
			if !args.Unify(List(), false) {
				return false, nil
			}
			pc = pc[2:]
			return e.arrive(x.String(), astack, func() (bool, error) {
				var v Variable
				return e.exec(pc, xr, vars, k, &v, &v)
			})
		case opExit:
			log.Debug("exit")
			return k()
		default:
			return false, fmt.Errorf("unknown(%d)", pc[0])
		}
	}
	return false, errors.New("non-exit end of bytecode")
}

type clauses []clause

func (cs clauses) Call(e *Engine, args Term, k func() (bool, error)) (bool, error) {
	if len(cs) == 0 {
		return false, nil
	}

	a := newAssignment(args)

	log := logrus.WithFields(logrus.Fields{
		"name": cs[0].name,
		"args": args,
	})

	for i, c := range cs {
		log = log.WithField("choice", i)

		switch i {
		case 0:
			log.Info("call")
		default:
			log.Info("redo")
		}

		vars := make([]*Variable, len(c.vars))
		for i := range c.vars {
			vars[i] = &Variable{}
		}

		ok, err := e.exec(c.bytecode, c.xrTable, vars, k, args, List())
		if err != nil {
			if errors.Is(err, errCut) {
				log.Info("cut")
				return ok, err
			}
			log.Info("exception")
			return false, err
		}
		if ok {
			log.Info("exit")
			return true, nil
		}

		a.reset()
	}

	log.Info("fail")
	return false, nil
}

type clause struct {
	name     string
	raw      Term
	xrTable  []Term
	vars     []*Variable
	bytecode bytecode
}

func (c *clause) compile(t Term) error {
	t = Resolve(t)
	c.raw = t
	switch t := t.(type) {
	case Atom:
		return c.compileClause(t, nil)
	case *Compound:
		if t.Functor == ":-" {
			return c.compileClause(t.Args[0], t.Args[1])
		}
		return c.compileClause(t, nil)
	default:
		return fmt.Errorf("not a compound: %s", t)
	}
}

func (c *clause) compileClause(head Term, body Term) error {
	switch head := head.(type) {
	case Atom:
	case *Compound:
		for _, a := range head.Args {
			if err := c.compileArg(a); err != nil {
				return err
			}
		}
	default:
		return fmt.Errorf("not an atom nor compound: %s", head)
	}
	if body != nil {
		c.bytecode = append(c.bytecode, opEnter)
		for {
			p, ok := body.(*Compound)
			if !ok || p.Functor != "," || len(p.Args) != 2 {
				break
			}
			if err := c.compilePred(p.Args[0]); err != nil {
				return err
			}
			body = p.Args[1]
		}
		if err := c.compilePred(body); err != nil {
			return err
		}
	}
	c.bytecode = append(c.bytecode, opExit)
	return nil
}

func (c *clause) compilePred(p Term) error {
	switch p := p.(type) {
	case Atom:
		c.bytecode = append(c.bytecode, opCall, c.xrOffset(&Compound{
			Functor: "/",
			Args:    []Term{p, Integer(0)},
		}))
		return nil
	case *Compound:
		for _, a := range p.Args {
			if err := c.compileArg(a); err != nil {
				return err
			}
		}
		c.bytecode = append(c.bytecode, opCall, c.xrOffset(&Compound{
			Functor: "/",
			Args:    []Term{p.Functor, Integer(len(p.Args))},
		}))
		return nil
	default:
		return errors.New("not a predicate")
	}
}

func (c *clause) compileArg(a Term) error {
	switch a := a.(type) {
	case Atom:
		c.bytecode = append(c.bytecode, opConst, c.xrOffset(a))
	case Integer:
		c.bytecode = append(c.bytecode, opConst, c.xrOffset(a))
	case *Variable:
		c.bytecode = append(c.bytecode, opVar, c.varOffset(a))
	case *Compound:
		c.bytecode = append(c.bytecode, opFunctor, c.xrOffset(&Compound{
			Functor: "/",
			Args:    []Term{a.Functor, Integer(len(a.Args))},
		}))
		for _, n := range a.Args {
			if err := c.compileArg(n); err != nil {
				return err
			}
		}
		c.bytecode = append(c.bytecode, opPop)
	default:
		return errors.New("unknown")
	}
	return nil
}

func (c *clause) xrOffset(o Term) byte {
	for i, r := range c.xrTable {
		if r.Unify(o, false) {
			return byte(i)
		}
	}
	c.xrTable = append(c.xrTable, o)
	return byte(len(c.xrTable) - 1)
}

func (c *clause) varOffset(o *Variable) byte {
	for i, v := range c.vars {
		if v == o {
			return byte(i)
		}
	}
	o.Name = ""
	c.vars = append(c.vars, o)
	return byte(len(c.vars) - 1)
}

type bytecode []byte

func (b bytecode) String() string {
	ret := make([]string, 0, len(b))
	for i := 0; i < len(b); i++ {
		switch b[i] {
		case opVoid:
			ret = append(ret, "void")
		case opConst:
			i++
			ret = append(ret, fmt.Sprintf("const %d", b[i]))
		case opVar:
			i++
			ret = append(ret, fmt.Sprintf("var %d", b[i]))
		case opFunctor:
			i++
			ret = append(ret, fmt.Sprintf("functor %d", b[i]))
		case opPop:
			ret = append(ret, "pop")
		case opEnter:
			ret = append(ret, "enter")
		case opCall:
			i++
			ret = append(ret, fmt.Sprintf("call %d", b[i]))
		case opExit:
			ret = append(ret, "exit")
		default:
			ret = append(ret, fmt.Sprintf("unknown(%d)", b[i]))
		}
	}
	return strings.Join(ret, "; ")
}

type predicate0 func(func() (bool, error)) (bool, error)

func (p predicate0) Call(e *Engine, args Term, k func() (bool, error)) (bool, error) {
	if !args.Unify(List(), false) {
		return false, errors.New("wrong number of arguments")
	}

	return p(func() (bool, error) {
		return e.exec([]byte{opExit}, nil, nil, k, nil, nil)
	})
}

type predicate1 func(Term, func() (bool, error)) (bool, error)

func (p predicate1) Call(e *Engine, args Term, k func() (bool, error)) (bool, error) {
	var v1 Variable
	if !args.Unify(List(&v1), false) {
		return false, fmt.Errorf("wrong number of arguments: %s", args)
	}

	return p(&v1, func() (bool, error) {
		return e.exec([]byte{opExit}, nil, nil, k, nil, nil)
	})
}

type predicate2 func(Term, Term, func() (bool, error)) (bool, error)

func (p predicate2) Call(e *Engine, args Term, k func() (bool, error)) (bool, error) {
	var v1, v2 Variable
	if !args.Unify(List(&v1, &v2), false) {
		return false, errors.New("wrong number of arguments")
	}

	return p(&v1, &v2, func() (bool, error) {
		return e.exec([]byte{opExit}, nil, nil, k, nil, nil)
	})
}

type predicate3 func(Term, Term, Term, func() (bool, error)) (bool, error)

func (p predicate3) Call(e *Engine, args Term, k func() (bool, error)) (bool, error) {
	var v1, v2, v3 Variable
	if !args.Unify(List(&v1, &v2, &v3), false) {
		return false, errors.New("wrong number of arguments")
	}

	return p(&v1, &v2, &v3, func() (bool, error) {
		return e.exec([]byte{opExit}, nil, nil, k, nil, nil)
	})
}

type predicate4 func(Term, Term, Term, Term, func() (bool, error)) (bool, error)

func (p predicate4) Call(e *Engine, args Term, k func() (bool, error)) (bool, error) {
	var v1, v2, v3, v4 Variable
	if !args.Unify(List(&v1, &v2, &v3, &v4), false) {
		return false, errors.New("wrong number of arguments")
	}

	return p(&v1, &v2, &v3, &v4, func() (bool, error) {
		return e.exec([]byte{opExit}, nil, nil, k, nil, nil)
	})
}

type predicate5 func(Term, Term, Term, Term, Term, func() (bool, error)) (bool, error)

func (p predicate5) Call(e *Engine, args Term, k func() (bool, error)) (bool, error) {
	var v1, v2, v3, v4, v5 Variable
	if !args.Unify(List(&v1, &v2, &v3, &v4, &v5), false) {
		return false, errors.New("wrong number of arguments")
	}

	return p(&v1, &v2, &v3, &v4, &v5, func() (bool, error) {
		return e.exec([]byte{opExit}, nil, nil, k, nil, nil)
	})
}

type assignment []*Variable

func newAssignment(ts ...Term) assignment {
	var a assignment
	for _, t := range ts {
		a.add(t)
	}
	return a
}

func (a *assignment) add(t Term) {
	switch t := t.(type) {
	case *Variable:
		if t.Ref != nil {
			a.add(t.Ref)
			return
		}
		for _, v := range *a {
			if v == t {
				return
			}
		}
		*a = append(*a, t)
	case *Compound:
		for _, arg := range t.Args {
			a.add(arg)
		}
	}
}

func (a assignment) reset() {
	logrus.WithField("vars", loggableVars{vars: a}).Debug("reset")
	for _, v := range a {
		v.Ref = nil
	}
}

func (a assignment) contains(v *Variable) bool {
	for _, e := range a {
		if e == v {
			return true
		}
	}
	return false
}

func Done() (bool, error) {
	return true, nil
}

type input struct {
	io.Reader
}

func (i *input) Write(p []byte) (int, error) {
	w, ok := i.Reader.(io.Writer)
	if !ok {
		return 0, errors.New("not a writer")
	}
	return w.Write(p)
}

func (i *input) Close() error {
	c, ok := i.Reader.(io.Closer)
	if !ok {
		return errors.New("not a closer")

	}
	return c.Close()
}

type output struct {
	io.Writer
}

func (o *output) Read(p []byte) (int, error) {
	r, ok := o.Writer.(io.Reader)
	if !ok {
		return 0, errors.New("not a reader")
	}
	return r.Read(p)
}

func (o *output) Close() error {
	c, ok := o.Writer.(io.Closer)
	if !ok {
		return errors.New("not a closer")

	}
	return c.Close()
}
