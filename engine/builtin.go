package engine

import (
	"bufio"
	"bytes"
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"sort"
	"strings"
	"unicode"
	"unicode/utf8"
)

// State represents the internal state of an interpreter.
type State struct {
	VM

	// Internal/external expression
	operators       operators
	charConversions map[rune]rune
	charConvEnabled bool
	doubleQuotes    doubleQuotes

	// I/O
	streams       map[Term]*Stream
	input, output *Stream

	// Misc
	debug bool
}

var errNotSupported = errors.New("not supported")

type rwc struct {
	r io.Reader
	w io.Writer
	c io.Closer
}

func (a *rwc) Read(p []byte) (int, error) {
	if a.r == nil {
		return 0, errNotSupported
	}
	return a.r.Read(p)
}

func (a *rwc) Write(p []byte) (int, error) {
	if a.w == nil {
		return 0, errNotSupported
	}
	return a.w.Write(p)
}

func (a *rwc) Close() error {
	if a.c == nil {
		return errNotSupported
	}
	return a.c.Close()
}

func readWriteCloser(i interface{}) io.ReadWriteCloser {
	if f, ok := i.(io.ReadWriteCloser); ok {
		return f
	}
	var f rwc
	f.r, _ = i.(io.Reader)
	f.w, _ = i.(io.Writer)
	f.c, _ = i.(io.Closer)
	return &f
}

// SetUserInput sets the given reader as a stream with an alias of user_input.
func (state *State) SetUserInput(r io.Reader, opts ...StreamOption) {
	opts = append(opts, WithAlias(state, "user_input"))
	state.input = NewStream(readWriteCloser(r), StreamModeRead, opts...)
}

// SetUserOutput sets the given writer as a stream with an alias of user_output.
func (state *State) SetUserOutput(w io.Writer, opts ...StreamOption) {
	opts = append(opts, WithAlias(state, "user_output"))
	state.output = NewStream(readWriteCloser(w), StreamModeWrite, opts...)
}

// Parser creates a new parser from the current State and io.Reader.
// If non-nil, vars will hold the information on variables it parses.
func (state *State) Parser(r io.Reader, vars *[]ParsedVariable) *Parser {
	br, ok := r.(*bufio.Reader)
	if !ok {
		br = bufio.NewReader(r)
	}
	return newParser(br,
		withCharConversions(state.charConversions),
		withOperators(&state.operators),
		withDoubleQuotes(state.doubleQuotes),
		withParsedVars(vars),
	)
}

// Repeat repeats the continuation until it succeeds.
func (state *State) Repeat(k func(*Env) *Promise, env *Env) *Promise {
	return Repeat(func(ctx context.Context) *Promise {
		return k(env)
	})
}

// Negation calls goal and returns false if it succeeds. Otherwise, invokes the continuation.
func (state *State) Negation(goal Term, k func(*Env) *Promise, env *Env) *Promise {
	return Delay(func(ctx context.Context) *Promise {
		ok, err := state.Call(goal, Success, env).Force(ctx)
		if err != nil {
			return Error(err)
		}
		if ok {
			return Bool(false)
		}
		return k(env)
	})
}

// Call executes goal. it succeeds if goal followed by k succeeds. A cut inside goal doesn't affect outside of Call.
func (state *State) Call(goal Term, k func(*Env) *Promise, env *Env) *Promise {
	switch g := env.Resolve(goal).(type) {
	case Variable:
		return Error(InstantiationError(env))
	default:
		fvs := env.FreeVariables(g)
		args := make([]Term, len(fvs))
		for i, fv := range fvs {
			args[i] = fv
		}
		const call = Atom("$call")
		cs, err := compile(env.Simplify(&Compound{
			Functor: ":-",
			Args: []Term{
				call.Apply(args...),
				g,
			},
		}))
		if err != nil {
			return Error(err)
		}

		return cs.Call(&state.VM, args, k, env)
	}
}

// Call1 succeeds if closure with an additional argument succeeds.
func (state *State) Call1(closure, arg1 Term, k func(*Env) *Promise, env *Env) *Promise {
	pi, args, err := piArgs(closure, env)
	if err != nil {
		return Error(err)
	}
	return state.Call(pi.Name.Apply(append(args, arg1)...), k, env)
}

// Call2 succeeds if closure with 2 additional arguments succeeds.
func (state *State) Call2(closure, arg1, arg2 Term, k func(*Env) *Promise, env *Env) *Promise {
	pi, args, err := piArgs(closure, env)
	if err != nil {
		return Error(err)
	}
	return state.Call(pi.Name.Apply(append(args, arg1, arg2)...), k, env)
}

// Call3 succeeds if closure with 3 additional arguments succeeds.
func (state *State) Call3(closure, arg1, arg2, arg3 Term, k func(*Env) *Promise, env *Env) *Promise {
	pi, args, err := piArgs(closure, env)
	if err != nil {
		return Error(err)
	}
	return state.Call(pi.Name.Apply(append(args, arg1, arg2, arg3)...), k, env)
}

// Call4 succeeds if closure with 4 additional arguments succeeds.
func (state *State) Call4(closure, arg1, arg2, arg3, arg4 Term, k func(*Env) *Promise, env *Env) *Promise {
	pi, args, err := piArgs(closure, env)
	if err != nil {
		return Error(err)
	}
	return state.Call(pi.Name.Apply(append(args, arg1, arg2, arg3, arg4)...), k, env)
}

// Call5 succeeds if closure with 5 additional arguments succeeds.
func (state *State) Call5(closure, arg1, arg2, arg3, arg4, arg5 Term, k func(*Env) *Promise, env *Env) *Promise {
	pi, args, err := piArgs(closure, env)
	if err != nil {
		return Error(err)
	}
	return state.Call(pi.Name.Apply(append(args, arg1, arg2, arg3, arg4, arg5)...), k, env)
}

// Call6 succeeds if closure with 6 additional arguments succeeds.
func (state *State) Call6(closure, arg1, arg2, arg3, arg4, arg5, arg6 Term, k func(*Env) *Promise, env *Env) *Promise {
	pi, args, err := piArgs(closure, env)
	if err != nil {
		return Error(err)
	}
	return state.Call(pi.Name.Apply(append(args, arg1, arg2, arg3, arg4, arg5, arg6)...), k, env)
}

// Call7 succeeds if closure with 7 additional arguments succeeds.
func (state *State) Call7(closure, arg1, arg2, arg3, arg4, arg5, arg6, arg7 Term, k func(*Env) *Promise, env *Env) *Promise {
	pi, args, err := piArgs(closure, env)
	if err != nil {
		return Error(err)
	}
	return state.Call(pi.Name.Apply(append(args, arg1, arg2, arg3, arg4, arg5, arg6, arg7)...), k, env)
}

// CallNth succeeds iff goal succeeds and nth unifies with the number of re-execution.
// See http://www.complang.tuwien.ac.at/ulrich/iso-prolog/call_nth
func (state *State) CallNth(goal, nth Term, k func(*Env) *Promise, env *Env) *Promise {
	nth = env.Resolve(nth)
	switch nth := nth.(type) {
	case Variable:
		break
	case Integer:
		switch {
		case nth < 0:
			return Error(DomainError(ValidDomainNotLessThanZero, nth, env))
		case nth == 0:
			return Bool(false)
		}
	default:
		return Error(TypeError(ValidTypeInteger, nth, env))
	}

	var (
		p         *Promise
		n         Integer
		err       error
		parentEnv = env
	)
	p = state.Call(goal, func(env *Env) *Promise {
		n, err = addI(n, Integer(1))
		if err != nil {
			return Error(RepresentationError(FlagMaxInteger, parentEnv))
		}

		u := Unify(n, nth, k, env)
		if nth, ok := nth.(Integer); ok && nth <= n {
			return Cut(p, func(context.Context) *Promise {
				return u
			})
		}
		return u
	}, env)
	return p
}

// Unify unifies t1 and t2 without occurs check (i.e., X = f(X) is allowed).
func Unify(t1, t2 Term, k func(*Env) *Promise, env *Env) *Promise {
	env, ok := t1.Unify(t2, false, env)
	if !ok {
		return Bool(false)
	}
	return k(env)
}

// UnifyWithOccursCheck unifies t1 and t2 with occurs check (i.e., X = f(X) is not allowed).
func UnifyWithOccursCheck(t1, t2 Term, k func(*Env) *Promise, env *Env) *Promise {
	env, ok := t1.Unify(t2, true, env)
	if !ok {
		return Bool(false)
	}
	return k(env)
}

// SubsumesTerm succeeds if general and specific are unifiable without binding variables in specific.
func SubsumesTerm(general, specific Term, k func(*Env) *Promise, env *Env) *Promise {
	theta, ok := general.Unify(specific, true, env)
	if !ok {
		return Bool(false)
	}

	if d := theta.Simplify(general).Compare(specific, env); d != 0 {
		return Bool(false)
	}

	return k(env)
}

// TypeVar checks if t is a variable.
func TypeVar(t Term, k func(*Env) *Promise, env *Env) *Promise {
	if _, ok := env.Resolve(t).(Variable); !ok {
		return Bool(false)
	}
	return k(env)
}

// TypeFloat checks if t is a floating-point number.
func TypeFloat(t Term, k func(*Env) *Promise, env *Env) *Promise {
	if _, ok := env.Resolve(t).(Float); !ok {
		return Bool(false)
	}
	return k(env)
}

// TypeInteger checks if t is an integer.
func TypeInteger(t Term, k func(*Env) *Promise, env *Env) *Promise {
	if _, ok := env.Resolve(t).(Integer); !ok {
		return Bool(false)
	}
	return k(env)
}

// TypeAtom checks if t is an atom.
func TypeAtom(t Term, k func(*Env) *Promise, env *Env) *Promise {
	if _, ok := env.Resolve(t).(Atom); !ok {
		return Bool(false)
	}
	return k(env)
}

// TypeCompound checks if t is a compound term.
func TypeCompound(t Term, k func(*Env) *Promise, env *Env) *Promise {
	if _, ok := env.Resolve(t).(*Compound); !ok {
		return Bool(false)
	}
	return k(env)
}

// AcyclicTerm checks if t is acyclic.
func AcyclicTerm(t Term, k func(*Env) *Promise, env *Env) *Promise {
	if cyclicTerm(t, nil, env) {
		return Bool(false)
	}
	return k(env)
}

func cyclicTerm(t Term, visited []Term, env *Env) bool {
	t = env.Resolve(t)

	for _, v := range visited {
		if t == v {
			return true
		}
	}
	visited = append(visited, t)

	if c, ok := t.(*Compound); ok {
		for _, a := range c.Args {
			if cyclicTerm(a, visited, env) {
				return true
			}
		}
	}

	return false
}

// Functor extracts the name and arity of term, or unifies term with an atomic/compound term of name and arity with
// fresh variables as arguments.
func Functor(t, name, arity Term, k func(*Env) *Promise, env *Env) *Promise {
	switch t := env.Resolve(t).(type) {
	case Variable:
		switch arity := env.Resolve(arity).(type) {
		case Variable:
			return Error(InstantiationError(env))
		case Integer:
			switch {
			case arity < 0:
				return Error(DomainError(ValidDomainNotLessThanZero, arity, env))
			case arity == 0:
				return Unify(t, name, k, env)
			}

			switch name := env.Resolve(name).(type) {
			case Variable:
				return Error(InstantiationError(env))
			case *Compound:
				return Error(TypeError(ValidTypeAtomic, name, env))
			case Atom:
				vs := make([]Term, arity)
				for i := range vs {
					vs[i] = NewVariable()
				}
				return Unify(t, &Compound{
					Functor: name,
					Args:    vs,
				}, k, env)
			default:
				return Error(TypeError(ValidTypeAtom, name, env))
			}
		default:
			return Error(TypeError(ValidTypeInteger, arity, env))
		}
	case *Compound:
		pattern := Compound{Args: []Term{name, arity}}
		return Unify(&pattern, &Compound{Args: []Term{t.Functor, Integer(len(t.Args))}}, k, env)
	default: // atomic
		pattern := Compound{Args: []Term{name, arity}}
		return Unify(&pattern, &Compound{Args: []Term{t, Integer(0)}}, k, env)
	}
}

// Arg extracts nth argument of term as arg, or finds the argument position of arg in term as nth.
func Arg(nth, t, arg Term, k func(*Env) *Promise, env *Env) *Promise {
	switch c := env.Resolve(t).(type) {
	case Variable:
		return Error(InstantiationError(env))
	case *Compound:
		switch n := env.Resolve(nth).(type) {
		case Variable:
			return Error(InstantiationError(env))
		case Integer:
			if n == 0 || int(n) > len(c.Args) {
				return Bool(false)
			}
			if n < 0 {
				return Error(DomainError(ValidDomainNotLessThanZero, n, env))
			}
			return Delay(func(context.Context) *Promise {
				return Unify(arg, c.Args[int(n)-1], k, env)
			})
		default:
			return Error(TypeError(ValidTypeInteger, n, env))
		}
	default:
		return Error(TypeError(ValidTypeCompound, t, env))
	}
}

// Univ constructs list as a list which first element is the functor of term and the rest is the arguments of term, or construct a compound from list as term.
func Univ(t, list Term, k func(*Env) *Promise, env *Env) *Promise {
	switch t := env.Resolve(t).(type) {
	case Variable:
		list = env.Resolve(list)
		if list == Atom("[]") {
			return Error(DomainError(ValidDomainNonEmptyList, list, env))
		}
		cons, ok := list.(*Compound)
		if !ok || cons.Functor != "." || len(cons.Args) != 2 {
			return Error(TypeError(ValidTypeList, list, env))
		}

		f, ok := env.Resolve(cons.Args[0]).(Atom)
		if !ok {
			return Error(TypeError(ValidTypeAtom, cons.Args[0], env))
		}

		var args []Term
		iter := ListIterator{List: cons.Args[1], Env: env}
		for iter.Next() {
			args = append(args, iter.Current())
		}
		if err := iter.Err(); err != nil {
			return Error(err)
		}

		return Delay(func(context.Context) *Promise {
			return Unify(t, &Compound{
				Functor: f,
				Args:    args,
			}, k, env)
		})
	case *Compound:
		return Delay(func(context.Context) *Promise {
			return Unify(list, List(append([]Term{t.Functor}, t.Args...)...), k, env)
		})
	default:
		return Delay(func(context.Context) *Promise {
			return Unify(list, List(t), k, env)
		})
	}
}

// CopyTerm clones in as out.
func CopyTerm(in, out Term, k func(*Env) *Promise, env *Env) *Promise {
	return Unify(copyTerm(in, nil, env), out, k, env)
}

func copyTerm(t Term, copied map[Term]Term, env *Env) Term {
	if copied == nil {
		copied = map[Term]Term{}
	}
	t = env.Resolve(t)
	if c, ok := copied[t]; ok {
		return c
	}
	switch t := t.(type) {
	case Variable:
		v := NewVariable()
		copied[t] = v
		return v
	case *Compound:
		c := Compound{
			Functor: t.Functor,
			Args:    make([]Term, len(t.Args)),
		}
		copied[t] = &c
		for i, a := range t.Args {
			c.Args[i] = copyTerm(a, copied, env)
		}
		return &c
	default:
		return t
	}
}

// TermVariables succeeds if vars unifies with a list of variables in term.
func TermVariables(term, vars Term, k func(*Env) *Promise, env *Env) *Promise {
	var (
		witness  = map[Variable]struct{}{}
		ret      []Term
		t        Term
		traverse = []Term{term}
	)
	for len(traverse) > 0 {
		t, traverse = traverse[0], traverse[1:]
		switch t := env.Resolve(t).(type) {
		case Variable:
			if _, ok := witness[t]; !ok {
				ret = append(ret, t)
			}
			witness[t] = struct{}{}
		case *Compound:
			traverse = append(t.Args, traverse...)
		}
	}

	return Unify(vars, List(ret...), k, env)
}

// Op defines operator with priority and specifier, or removes when priority is 0.
func (state *State) Op(priority, specifier, op Term, k func(*Env) *Promise, env *Env) *Promise {
	p, ok := env.Resolve(priority).(Integer)
	if !ok {
		return Error(TypeError(ValidTypeInteger, priority, env))
	}
	if p < 0 || p > 1200 {
		return Error(DomainError(ValidDomainOperatorPriority, priority, env))
	}

	s, ok := env.Resolve(specifier).(Atom)
	if !ok {
		return Error(TypeError(ValidTypeAtom, specifier, env))
	}

	spec, ok := map[Atom]operatorSpecifier{
		"fx":  operatorSpecifierFX,
		"fy":  operatorSpecifierFY,
		"xf":  operatorSpecifierXF,
		"yf":  operatorSpecifierYF,
		"xfx": operatorSpecifierXFX,
		"xfy": operatorSpecifierXFY,
		"yfx": operatorSpecifierYFX,
	}[s]
	if !ok {
		return Error(DomainError(ValidDomainOperatorSpecifier, s, env))
	}

	o, ok := env.Resolve(op).(Atom)
	if !ok {
		return Error(TypeError(ValidTypeAtom, op, env))
	}

	// already defined?
	for i, op := range state.operators {
		if op.specifier != spec || op.name != o {
			continue
		}

		// remove it first so that we can insert it again in the right position
		copy(state.operators[i:], state.operators[i+1:])
		state.operators[len(state.operators)-1] = operator{}
		state.operators = state.operators[:len(state.operators)-1]

		// or keep it removed.
		if p == 0 {
			return k(env)
		}
	}

	// insert
	i := sort.Search(len(state.operators), func(i int) bool {
		return state.operators[i].priority >= p
	})
	state.operators = append(state.operators, operator{})
	copy(state.operators[i+1:], state.operators[i:])
	state.operators[i] = operator{
		priority:  p,
		specifier: spec,
		name:      o,
	}

	return k(env)
}

// CurrentOp succeeds if operator is defined with priority and specifier.
func (state *State) CurrentOp(priority, specifier, operator Term, k func(*Env) *Promise, env *Env) *Promise {
	switch p := env.Resolve(priority).(type) {
	case Variable:
		break
	case Integer:
		if p < 0 || p > 1200 {
			return Error(DomainError(ValidDomainOperatorPriority, priority, env))
		}
	default:
		return Error(DomainError(ValidDomainOperatorPriority, priority, env))
	}

	switch s := env.Resolve(specifier).(type) {
	case Variable:
		break
	case Atom:
		if _, ok := map[Atom]struct{}{
			"xf":  {},
			"yf":  {},
			"xfx": {},
			"xfy": {},
			"yfx": {},
			"fx":  {},
			"fy":  {},
		}[s]; !ok {
			return Error(DomainError(ValidDomainOperatorSpecifier, s, env))
		}
	default:
		return Error(DomainError(ValidDomainOperatorSpecifier, s, env))
	}

	switch env.Resolve(operator).(type) {
	case Variable, Atom:
		break
	default:
		return Error(TypeError(ValidTypeAtom, operator, env))
	}

	pattern := Compound{Args: []Term{priority, specifier, operator}}
	ks := make([]func(context.Context) *Promise, len(state.operators))
	for i := range state.operators {
		op := state.operators[i]
		ks[i] = func(context.Context) *Promise {
			return Unify(&pattern, &Compound{Args: []Term{op.priority, op.specifier.term(), op.name}}, k, env)
		}
	}
	return Delay(ks...)
}

// Assertz appends t to the database.
func (state *State) Assertz(t Term, k func(*Env) *Promise, env *Env) *Promise {
	if err := state.assert(t, false, func(existing clauses, new clauses) clauses {
		return append(existing, new...)
	}, env); err != nil {
		return Error(err)
	}
	return k(env)
}

// Asserta prepends t to the database.
func (state *State) Asserta(t Term, k func(*Env) *Promise, env *Env) *Promise {
	if err := state.assert(t, false, func(existing clauses, new clauses) clauses {
		return append(new, existing...)
	}, env); err != nil {
		return Error(err)
	}
	return k(env)
}

// Assert appends t to the database.
func (state *State) Assert(t Term, env *Env) error {
	return state.assert(t, true, func(existing clauses, new clauses) clauses {
		return append(existing, new...)
	}, env)
}

func (state *State) assert(t Term, force bool, merge func(clauses, clauses) clauses, env *Env) error {
	pi, args, err := piArgs(t, env)
	if err != nil {
		return err
	}

	if pi == (ProcedureIndicator{Name: ":-", Arity: 2}) {
		pi, _, err = piArgs(args[0], env)
		if err != nil {
			return err
		}
	}

	if state.procedures == nil {
		state.procedures = map[ProcedureIndicator]procedure{}
	}
	p, ok := state.procedures[pi]
	if !ok {
		if force {
			p = static{}
		} else {
			p = clauses{}
		}
	}

	added, err := compile(env.Simplify(t))
	if err != nil {
		return err
	}

	switch existing := p.(type) {
	case clauses:
		state.procedures[pi] = merge(existing, added)
		return nil
	case builtin:
		if !force {
			return PermissionError(OperationModify, PermissionTypeStaticProcedure, pi.Term(), env)
		}
		state.procedures[pi] = builtin{merge(existing.clauses, added)}
		return nil
	case static:
		if !force {
			return PermissionError(OperationModify, PermissionTypeStaticProcedure, pi.Term(), env)
		}
		state.procedures[pi] = static{merge(existing.clauses, added)}
		return nil
	default:
		return PermissionError(OperationModify, PermissionTypeStaticProcedure, pi.Term(), env)
	}
}

// BagOf collects all the solutions of goal as instances, which unify with template. instances may contain duplications.
func (state *State) BagOf(template, goal, instances Term, k func(*Env) *Promise, env *Env) *Promise {
	return state.collectionOf(List, template, goal, instances, k, env)
}

// SetOf collects all the solutions of goal as instances, which unify with template. instances don't contain duplications.
func (state *State) SetOf(template, goal, instances Term, k func(*Env) *Promise, env *Env) *Promise {
	return state.collectionOf(env.Set, template, goal, instances, k, env)
}

func (state *State) collectionOf(agg func(...Term) Term, template, goal, instances Term, k func(*Env) *Promise, env *Env) *Promise {
	var qualifier, body Term
	switch goal := env.Resolve(goal).(type) {
	case Variable:
		return Error(InstantiationError(env))
	case *Compound:
		if goal.Functor != "^" || len(goal.Args) != 2 {
			qualifier = Atom("")
			body = goal
			break
		}
		qualifier = goal.Args[0]
		body = goal.Args[1]
	default:
		qualifier = Atom("")
		body = goal
	}

	groupingVariables := variables(env.FreeVariables(body)).except(env.FreeVariables(template, qualifier))

	return Delay(func(ctx context.Context) *Promise {
		const (
			hyphen  = Atom("-")
			vars    = Atom("vars")
			answers = Variable("Answers")
		)

		type solution struct {
			vars      Term
			instances []Term
		}
		var solutions []solution

		template = hyphen.Apply(vars.Apply(groupingVariables.terms()...), template)
		if _, err := state.FindAll(template, body, answers, func(env *Env) *Promise {
			iter := ListIterator{List: answers, Env: env}
		it:
			for iter.Next() {
				answer := iter.Current().(*Compound)
				vars, instance := answer.Args[0], answer.Args[1]
				for i := range solutions {
					if solutions[i].vars.Compare(vars, env) == 0 {
						solutions[i].instances = append(solutions[i].instances, instance)
						continue it
					}
				}
				solutions = append(solutions, solution{vars: vars, instances: []Term{instance}})
			}
			// FindAll returns a proper list so no need to check iter.Err().
			return Bool(true)
		}, env).Force(ctx); err != nil {
			return Error(err)
		}

		sort.Slice(solutions, func(i, j int) bool {
			return solutions[i].vars.Compare(solutions[j].vars, env) < 0
		})

		ks := make([]func(context.Context) *Promise, len(solutions))
		for i, s := range solutions {
			switch vars := s.vars.(type) {
			case *Compound:
				bag := s.instances
				ks[i] = func(ctx context.Context) *Promise {
					env := env
					for j, v := range groupingVariables {
						env = env.Bind(v, vars.Args[j])
					}
					return Unify(instances, agg(bag...), k, env)
				}
			default:
				bag := s.instances
				ks[i] = func(ctx context.Context) *Promise {
					return Unify(instances, agg(bag...), k, env)
				}
			}
		}
		return Delay(ks...)
	})
}

// FindAll collects all the solutions of goal as instances, which unify with template. instances may contain duplications.
func (state *State) FindAll(template, goal, instances Term, k func(*Env) *Promise, env *Env) *Promise {
	return Delay(func(ctx context.Context) *Promise {
		var answers []Term
		if _, err := state.Call(goal, func(env *Env) *Promise {
			answers = append(answers, env.Simplify(template))
			return Bool(false) // ask for more solutions
		}, env).Force(ctx); err != nil {
			return Error(err)
		}
		return Unify(instances, List(answers...), k, env)
	})
}

// Compare compares term1 and term2 and unifies order with <, =, or >.
func Compare(order, term1, term2 Term, k func(*Env) *Promise, env *Env) *Promise {
	switch o := env.Resolve(order).(type) {
	case Variable:
		break
	case Atom:
		switch o {
		case "<", "=", ">":
			break
		default:
			return Error(DomainError(ValidDomainOrder, order, env))
		}
	default:
		return Error(TypeError(ValidTypeAtom, order, env))
	}

	d := term1.Compare(term2, env)
	switch {
	case d < 0:
		return Unify(Atom("<"), order, k, env)
	case d > 0:
		return Unify(Atom(">"), order, k, env)
	default: // d == 0:
		return Unify(Atom("="), order, k, env)
	}
}

// Between succeeds when lower, upper, and value are all integers, and lower <= value <= upper.
// If value is a variable, it is unified with successive integers from lower to upper.
func Between(lower, upper, value Term, k func(*Env) *Promise, env *Env) *Promise {
	var low, high Integer

	switch lower := env.Resolve(lower).(type) {
	case Integer:
		low = lower
	case Variable:
		return Error(InstantiationError(env))
	default:
		return Error(TypeError(ValidTypeInteger, lower, env))
	}

	switch upper := env.Resolve(upper).(type) {
	case Integer:
		high = upper
	case Variable:
		return Error(InstantiationError(env))
	default:
		return Error(TypeError(ValidTypeInteger, upper, env))
	}

	if low > high {
		return Bool(false)
	}

	switch value := env.Resolve(value).(type) {
	case Integer:
		if value < low || value > high {
			return Bool(false)
		}
		return k(env)
	case Variable:
		ks := make([]func(context.Context) *Promise, 0, 2)
		ks = append(ks, func(context.Context) *Promise {
			return Unify(value, low, k, env)
		})
		if low < high {
			ks = append(ks, func(context.Context) *Promise {
				return Between(low+1, upper, value, k, env)
			})
		}
		return Delay(ks...)
	default:
		return Error(TypeError(ValidTypeInteger, value, env))
	}
}

// Sort succeeds if sorted list of elements of list unifies with sorted.
func Sort(list, sorted Term, k func(*Env) *Promise, env *Env) *Promise {
	var elems []Term
	iter := ListIterator{List: list, Env: env}
	for iter.Next() {
		elems = append(elems, env.Resolve(iter.Current()))
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}

	switch s := env.Resolve(sorted).(type) {
	case Variable:
		break
	case *Compound:
		if s.Functor == "." && len(s.Args) == 2 {
			break
		}
		return Error(TypeError(ValidTypeList, sorted, env))
	default:
		return Error(TypeError(ValidTypeList, sorted, env))
	}

	return Unify(sorted, env.Set(elems...), k, env)
}

// KeySort succeeds if sorted is a sorted list of pairs based on their keys.
func KeySort(pairs, sorted Term, k func(*Env) *Promise, env *Env) *Promise {
	var elems []Term
	iter := ListIterator{List: pairs, Env: env}
	for iter.Next() {
		switch e := env.Resolve(iter.Current()).(type) {
		case Variable:
			return Error(InstantiationError(env))
		case *Compound:
			if e.Functor != "-" || len(e.Args) != 2 {
				return Error(TypeError(ValidTypePair, e, env))
			}
			elems = append(elems, e)
		default:
			return Error(TypeError(ValidTypePair, e, env))
		}
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}

	switch s := env.Resolve(sorted).(type) {
	case Variable:
		break
	default:
		iter := ListIterator{List: s, Env: env}
		for iter.Next() {
			switch e := env.Resolve(iter.Current()).(type) {
			case Variable:
				continue
			case *Compound:
				if e.Functor != "-" || len(e.Args) != 2 {
					return Error(TypeError(ValidTypePair, e, env))
				}
			default:
				return Error(TypeError(ValidTypePair, e, env))
			}
		}
		if err := iter.Err(); err != nil {
			return Error(err)
		}
	}

	sort.SliceStable(elems, func(i, j int) bool {
		return elems[i].(*Compound).Args[0].Compare(elems[j].(*Compound).Args[0], env) < 0
	})

	return Unify(sorted, List(elems...), k, env)
}

// Throw throws ball as an exception.
func Throw(ball Term, _ func(*Env) *Promise, env *Env) *Promise {
	switch b := env.Resolve(ball).(type) {
	case Variable:
		return Error(InstantiationError(env))
	default:
		return Error(NewException(b, env))
	}
}

// Catch calls goal. If an exception is thrown and unifies with catcher, it calls recover.
func (state *State) Catch(goal, catcher, recover Term, k func(*Env) *Promise, env *Env) *Promise {
	return Catch(func(err error) *Promise {
		var e Exception
		if !errors.As(err, &e) {
			return nil
		}

		env, ok := catcher.Unify(e.term, false, env)
		if !ok {
			return nil
		}

		return state.Call(recover, k, env)
	}, func(ctx context.Context) *Promise {
		return state.Call(goal, k, env)
	})
}

// CurrentPredicate matches pi with a predicate indicator of the user-defined procedures in the database.
func (state *State) CurrentPredicate(pi Term, k func(*Env) *Promise, env *Env) *Promise {
	switch pi := env.Resolve(pi).(type) {
	case Variable:
		break
	case *Compound:
		if pi.Functor != "/" || len(pi.Args) != 2 {
			return Error(TypeError(ValidTypePredicateIndicator, pi, env))
		}
		if _, ok := env.Resolve(pi.Args[0]).(Atom); !ok {
			return Error(TypeError(ValidTypePredicateIndicator, pi, env))
		}
		if _, ok := env.Resolve(pi.Args[1]).(Integer); !ok {
			return Error(TypeError(ValidTypePredicateIndicator, pi, env))
		}
	default:
		return Error(TypeError(ValidTypePredicateIndicator, pi, env))
	}

	ks := make([]func(context.Context) *Promise, 0, len(state.procedures))
	for key, p := range state.procedures {
		switch p.(type) {
		case clauses, static:
		default:
			continue
		}
		c := key.Term()
		ks = append(ks, func(context.Context) *Promise {
			return Unify(pi, c, k, env)
		})
	}
	return Delay(ks...)
}

// Retract removes the first clause that matches with t.
func (state *State) Retract(t Term, k func(*Env) *Promise, env *Env) *Promise {
	t = Rulify(t, env)

	h := t.(*Compound).Args[0]
	pi, _, err := piArgs(h, env)
	if err != nil {
		return Error(err)
	}

	p, ok := state.procedures[pi]
	if !ok {
		return Bool(false)
	}

	cs, ok := p.(clauses)
	if !ok {
		return Error(PermissionError(OperationModify, PermissionTypeStaticProcedure, pi.Term(), env))
	}

	deleted := 0
	ks := make([]func(context.Context) *Promise, len(cs))
	for i, c := range cs {
		i := i
		raw := Rulify(c.raw, env)
		ks[i] = func(_ context.Context) *Promise {
			return Unify(t, raw, func(env *Env) *Promise {
				j := i - deleted
				cs, cs[len(cs)-1] = append(cs[:j], cs[j+1:]...), clause{}
				deleted++
				state.procedures[pi] = cs
				return k(env)
			}, env)
		}
	}
	return Delay(ks...)
}

// Abolish removes the procedure indicated by pi from the database.
func (state *State) Abolish(pi Term, k func(*Env) *Promise, env *Env) *Promise {
	switch pi := env.Resolve(pi).(type) {
	case Variable:
		return Error(InstantiationError(env))
	case *Compound:
		if pi.Functor != "/" || len(pi.Args) != 2 {
			return Error(TypeError(ValidTypePredicateIndicator, pi, env))
		}

		name, arity := pi.Args[0], pi.Args[1]

		switch name := env.Resolve(name).(type) {
		case Variable:
			return Error(InstantiationError(env))
		case Atom:
			switch arity := env.Resolve(arity).(type) {
			case Variable:
				return Error(InstantiationError(env))
			case Integer:
				if arity < 0 {
					return Error(DomainError(ValidDomainNotLessThanZero, arity, env))
				}
				key := ProcedureIndicator{Name: name, Arity: arity}
				if _, ok := state.procedures[key].(clauses); !ok {
					return Error(PermissionError(OperationModify, PermissionTypeStaticProcedure, key.Term(), env))
				}
				delete(state.procedures, key)
				return k(env)
			default:
				return Error(TypeError(ValidTypeInteger, arity, env))
			}
		default:
			return Error(TypeError(ValidTypeAtom, name, env))
		}
	default:
		return Error(TypeError(ValidTypePredicateIndicator, pi, env))
	}
}

// CurrentInput unifies stream with the current input stream.
func (state *State) CurrentInput(stream Term, k func(*Env) *Promise, env *Env) *Promise {
	switch env.Resolve(stream).(type) {
	case Variable, *Stream:
		break
	default:
		return Error(DomainError(ValidDomainStream, stream, env))
	}

	return Delay(func(context.Context) *Promise {
		return Unify(stream, state.input, k, env)
	})
}

// CurrentOutput unifies stream with the current output stream.
func (state *State) CurrentOutput(stream Term, k func(*Env) *Promise, env *Env) *Promise {
	switch env.Resolve(stream).(type) {
	case Variable, *Stream:
		break
	default:
		return Error(DomainError(ValidDomainStream, stream, env))
	}

	return Delay(func(context.Context) *Promise {
		return Unify(stream, state.output, k, env)
	})
}

// SetInput sets streamOrAlias as the current input stream.
func (state *State) SetInput(streamOrAlias Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := state.stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	if s.mode != StreamModeRead {
		return Error(PermissionError(OperationInput, PermissionTypeStream, streamOrAlias, env))
	}

	state.input = s
	return k(env)
}

// SetOutput sets streamOrAlias as the current output stream.
func (state *State) SetOutput(streamOrAlias Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := state.stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	if s.mode != StreamModeWrite && s.mode != StreamModeAppend {
		return Error(PermissionError(OperationOutput, PermissionTypeStream, streamOrAlias, env))
	}

	state.output = s
	return k(env)
}

// Open opens SourceSink in mode and unifies with stream.
func (state *State) Open(SourceSink, mode, stream, options Term, k func(*Env) *Promise, env *Env) *Promise {
	var n Atom
	switch s := env.Resolve(SourceSink).(type) {
	case Variable:
		return Error(InstantiationError(env))
	case Atom:
		n = s
	default:
		return Error(DomainError(ValidDomainSourceSink, SourceSink, env))
	}

	var streamMode StreamMode
	switch m := env.Resolve(mode).(type) {
	case Variable:
		return Error(InstantiationError(env))
	case Atom:
		var ok bool
		streamMode, ok = map[Atom]StreamMode{
			"read":   StreamModeRead,
			"write":  StreamModeWrite,
			"append": StreamModeAppend,
		}[m]
		if !ok {
			return Error(DomainError(ValidDomainIOMode, m, env))
		}
	default:
		return Error(TypeError(ValidTypeAtom, mode, env))
	}

	if _, ok := env.Resolve(stream).(Variable); !ok {
		return Error(InstantiationError(env))
	}

	var opts []StreamOption
	iter := ListIterator{List: options, Env: env}
	for iter.Next() {
		opt, err := streamOption(state, iter.Current(), env)
		if err != nil {
			return Error(err)
		}

		opts = append(opts, opt)
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}

	s, err := Open(n, streamMode, opts...)
	if err != nil {
		return Error(err)
	}

	return Delay(func(context.Context) *Promise {
		return Unify(stream, s, k, env)
	})
}

func streamOption(state *State, option Term, env *Env) (StreamOption, error) {
	switch o := env.Resolve(option).(type) {
	case Variable:
		return nil, InstantiationError(env)
	case *Compound:
		if len(o.Args) != 1 {
			return nil, DomainError(ValidDomainStreamOption, option, env)
		}
		switch o.Functor {
		case "alias":
			return streamOptionAlias(state, o, env)
		case "type":
			return streamOptionType(o, env)
		case "reposition":
			return streamOptionReposition(o, env)
		case "eof_action":
			return streamOptionEOFAction(o, env)
		default:
			return nil, DomainError(ValidDomainStreamOption, option, env)
		}
	default:
		return nil, DomainError(ValidDomainStreamOption, option, env)
	}
}

func streamOptionAlias(state *State, o *Compound, env *Env) (StreamOption, error) {
	switch a := env.Resolve(o.Args[0]).(type) {
	case Variable:
		return nil, InstantiationError(env)
	case Atom:
		if _, ok := state.streams[a]; ok {
			return nil, PermissionError(OperationOpen, PermissionTypeSourceSink, o, env)
		}
		return WithAlias(state, a), nil
	default:
		return nil, DomainError(ValidDomainStreamOption, o, env)
	}
}

func streamOptionType(o *Compound, env *Env) (StreamOption, error) {
	switch t := env.Resolve(o.Args[0]).(type) {
	case Variable:
		return nil, InstantiationError(env)
	case Atom:
		switch t {
		case "text":
			return WithStreamType(StreamTypeText), nil
		case "binary":
			return WithStreamType(StreamTypeBinary), nil
		}
	}
	return nil, DomainError(ValidDomainStreamOption, o, env)
}

func streamOptionReposition(o *Compound, env *Env) (StreamOption, error) {
	switch r := env.Resolve(o.Args[0]).(type) {
	case Variable:
		return nil, InstantiationError(env)
	case Atom:
		switch r {
		case "true":
			return WithReposition(true), nil
		case "false":
			return WithReposition(false), nil
		}
	}
	return nil, DomainError(ValidDomainStreamOption, o, env)
}

func streamOptionEOFAction(o *Compound, env *Env) (StreamOption, error) {
	switch e := env.Resolve(o.Args[0]).(type) {
	case Variable:
		return nil, InstantiationError(env)
	case Atom:
		switch e {
		case "error":
			return WithEOFAction(EOFActionError), nil
		case "eof_code":
			return WithEOFAction(EOFActionEOFCode), nil
		case "reset":
			return WithEOFAction(EOFActionReset), nil
		}
	}
	return nil, DomainError(ValidDomainStreamOption, o, env)
}

// Close closes a stream specified by streamOrAlias.
func (state *State) Close(streamOrAlias, options Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := state.stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	var force bool
	iter := ListIterator{List: options, Env: env}
	for iter.Next() {
		switch option := env.Resolve(iter.Current()).(type) {
		case Variable:
			return Error(InstantiationError(env))
		case *Compound:
			switch option.Functor {
			case "force":
				if len(option.Args) != 1 {
					return Error(DomainError(ValidDomainStreamOption, option, env))
				}

				switch v := env.Resolve(option.Args[0]).(type) {
				case Atom:
					switch v {
					case "false":
						force = false
					case "true":
						force = true
					default:
						return Error(DomainError(ValidDomainStreamOption, option, env))
					}
				default:
					return Error(DomainError(ValidDomainStreamOption, option, env))
				}
			}
		default:
			return Error(DomainError(ValidDomainStreamOption, option, env))
		}
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}

	if err := s.Close(); err != nil && !force {
		return Error(SystemError(err))
	}

	if s.alias == "" {
		delete(state.streams, s)
	} else {
		delete(state.streams, s.alias)
	}

	return k(env)
}

var sync = (*os.File).Sync

// FlushOutput sends any buffered output to the stream.
func (state *State) FlushOutput(streamOrAlias Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := state.stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	if s.mode != StreamModeWrite && s.mode != StreamModeAppend {
		return Error(PermissionError(OperationOutput, PermissionTypeStream, streamOrAlias, env))
	}

	if f, ok := s.file.(*os.File); ok {
		if err := sync(f); err != nil {
			return Error(err)
		}
	}

	return k(env)
}

// WriteTerm outputs term to stream with options.
func (state *State) WriteTerm(streamOrAlias, t, options Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := state.stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	if s.mode != StreamModeWrite && s.mode != StreamModeAppend {
		return Error(PermissionError(OperationOutput, PermissionTypeStream, streamOrAlias, env))
	}

	if s.streamType == StreamTypeBinary {
		return Error(PermissionError(OperationOutput, PermissionTypeBinaryStream, streamOrAlias, env))
	}

	var opts WriteOptions
	iter := ListIterator{List: options, Env: env}
	for iter.Next() {
		if err := writeTermOption(&opts, iter.Current(), env); err != nil {
			return Error(err)
		}
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}

	if err := state.Write(s.file, env.Resolve(t), &opts, env); err != nil {
		return Error(err)
	}

	return k(env)
}

// Write outputs term to the writer.
func (state *State) Write(w io.Writer, t Term, opts *WriteOptions, env *Env) error {
	opts.ops = state.operators
	opts.priority = 1200
	return t.WriteTerm(w, opts, env)
}

func writeTermOption(opts *WriteOptions, option Term, env *Env) error {
	switch o := env.Resolve(option).(type) {
	case Variable:
		return InstantiationError(env)
	case *Compound:
		if len(o.Args) != 1 {
			return DomainError(ValidDomainWriteOption, o, env)
		}

		if o.Functor == "variable_names" {
			vns, err := variableNames(o, env)
			if err != nil {
				return err
			}
			opts.VariableNames = vns
			return nil
		}

		var b bool
		switch v := env.Resolve(o.Args[0]).(type) {
		case Variable:
			return InstantiationError(env)
		case Atom:
			switch v {
			case "true":
				b = true
			case "false":
				b = false
			default:
				return DomainError(ValidDomainWriteOption, o, env)
			}
		default:
			return DomainError(ValidDomainWriteOption, o, env)
		}

		switch o.Functor {
		case "quoted":
			opts.Quoted = b
			return nil
		case "ignore_ops":
			opts.IgnoreOps = b
			return nil
		case "numbervars":
			opts.NumberVars = b
			return nil
		default:
			return DomainError(ValidDomainWriteOption, o, env)
		}
	default:
		return DomainError(ValidDomainWriteOption, o, env)
	}
}

func variableNames(option *Compound, env *Env) (map[Variable]Atom, error) {
	vns := map[Variable]Atom{}
	iter := ListIterator{List: option.Args[0], Env: env}
	for iter.Next() {
		var vn *Compound
		switch elem := env.Resolve(iter.Current()).(type) {
		case Variable:
			return nil, InstantiationError(env)
		case *Compound:
			if elem.Functor != "=" || len(elem.Args) != 2 {
				return nil, DomainError(ValidDomainWriteOption, option, env)
			}
			vn = elem
		default:
			return nil, DomainError(ValidDomainWriteOption, option, env)
		}

		var n Atom
		switch arg := env.Resolve(vn.Args[0]).(type) {
		case Variable:
			return nil, InstantiationError(env)
		case Atom:
			n = arg
		default:
			return nil, DomainError(ValidDomainWriteOption, option, env)
		}

		var v Variable
		switch arg := env.Resolve(vn.Args[1]).(type) {
		case Variable:
			v = arg
		default:
			continue
		}

		if _, ok := vns[v]; ok {
			continue
		}
		vns[v] = n
	}

	switch s := iter.Suffix().(type) {
	case Variable:
		return nil, InstantiationError(env)
	case Atom:
		if s != "[]" {
			return nil, DomainError(ValidDomainWriteOption, option, env)
		}
		return vns, nil
	default:
		return nil, DomainError(ValidDomainWriteOption, option, env)
	}
}

// CharCode converts a single-rune Atom char to an Integer code, or vice versa.
func CharCode(char, code Term, k func(*Env) *Promise, env *Env) *Promise {
	switch ch := env.Resolve(char).(type) {
	case Variable:
		switch cd := env.Resolve(code).(type) {
		case Variable:
			return Error(InstantiationError(env))
		case Integer:
			r := rune(cd)

			if !utf8.ValidRune(r) {
				return Error(RepresentationError(FlagCharacterCode, env))
			}

			return Delay(func(context.Context) *Promise {
				return Unify(ch, Atom(r), k, env)
			})
		default:
			return Error(TypeError(ValidTypeInteger, code, env))
		}
	case Atom:
		switch code := env.Resolve(code).(type) {
		case Variable, Integer:
			break
		default:
			return Error(TypeError(ValidTypeInteger, code, env))
		}

		rs := []rune(ch)
		if len(rs) != 1 {
			return Error(TypeError(ValidTypeCharacter, ch, env))
		}

		return Delay(func(context.Context) *Promise {
			return Unify(code, Integer(rs[0]), k, env)
		})
	default:
		return Error(TypeError(ValidTypeCharacter, ch, env))
	}
}

var write = io.Writer.Write

// PutByte outputs an integer byte to a stream represented by streamOrAlias.
func (state *State) PutByte(streamOrAlias, byt Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := state.stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	if s.mode != StreamModeWrite && s.mode != StreamModeAppend {
		return Error(PermissionError(OperationOutput, PermissionTypeStream, streamOrAlias, env))
	}

	if s.streamType == StreamTypeText {
		return Error(PermissionError(OperationOutput, PermissionTypeTextStream, streamOrAlias, env))
	}

	switch b := env.Resolve(byt).(type) {
	case Variable:
		return Error(InstantiationError(env))
	case Integer:
		if 0 > b || 255 < b {
			return Error(TypeError(ValidTypeByte, byt, env))
		}

		if _, err := write(s.file, []byte{byte(b)}); err != nil {
			return Error(SystemError(err))
		}

		return k(env)
	default:
		return Error(TypeError(ValidTypeByte, byt, env))
	}
}

// PutCode outputs code to the stream represented by streamOrAlias.
func (state *State) PutCode(streamOrAlias, code Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := state.stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	if s.mode != StreamModeWrite && s.mode != StreamModeAppend {
		return Error(PermissionError(OperationOutput, PermissionTypeStream, streamOrAlias, env))
	}

	if s.streamType == StreamTypeBinary {
		return Error(PermissionError(OperationOutput, PermissionTypeBinaryStream, streamOrAlias, env))
	}

	switch c := env.Resolve(code).(type) {
	case Variable:
		return Error(InstantiationError(env))
	case Integer:
		r := rune(c)

		if !utf8.ValidRune(r) {
			return Error(RepresentationError(FlagCharacterCode, env))
		}

		if _, err := write(s.file, []byte(string(r))); err != nil {
			return Error(SystemError(err))
		}

		return k(env)
	default:
		return Error(TypeError(ValidTypeInteger, code, env))
	}
}

type readTermOptions struct {
	singletons    Term
	variables     Term
	variableNames Term
}

// ReadTerm reads from the stream represented by streamOrAlias and unifies with stream.
func (state *State) ReadTerm(streamOrAlias, out, options Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := state.stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	if s.mode != StreamModeRead {
		return Error(PermissionError(OperationInput, PermissionTypeStream, streamOrAlias, env))
	}

	if s.streamType == StreamTypeBinary {
		return Error(PermissionError(OperationInput, PermissionTypeBinaryStream, streamOrAlias, env))
	}

	opts := readTermOptions{
		singletons:    NewVariable(),
		variables:     NewVariable(),
		variableNames: NewVariable(),
	}
	iter := ListIterator{List: options, Env: env}
	for iter.Next() {
		if err := readTermOption(&opts, iter.Current(), env); err != nil {
			return Error(err)
		}
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}

	var vars []ParsedVariable
	p := state.Parser(s.buf, &vars)

	t, err := p.Term()
	if err != nil {
		switch {
		case errors.Is(err, io.EOF):
			return [...]*Promise{
				EOFActionError: Error(PermissionError(OperationInput, PermissionTypePastEndOfStream, streamOrAlias, env)),
				EOFActionEOFCode: Delay(func(context.Context) *Promise {
					return Unify(out, Atom("end_of_file"), k, env)
				}),
				EOFActionReset: Delay(func(context.Context) *Promise {
					return state.ReadTerm(streamOrAlias, out, options, k, env)
				}),
			}[s.eofAction]
		default:
			return Error(SyntaxError(err, env))
		}
	}

	var singletons, variables, variableNames []Term
	for _, v := range vars {
		if v.Count == 1 {
			singletons = append(singletons, v.Variable)
		}
		variables = append(variables, v.Variable)
		variableNames = append(variableNames, &Compound{
			Functor: "=",
			Args:    []Term{v.Name, v.Variable},
		})
	}

	env, ok := (&Compound{Args: []Term{
		opts.singletons,
		opts.variables,
		opts.variableNames,
	}}).Unify(&Compound{Args: []Term{
		List(singletons...),
		List(variables...),
		List(variableNames...),
	}}, false, env)
	if !ok {
		return Bool(false)
	}

	return Unify(out, t, k, env)
}

func readTermOption(opts *readTermOptions, option Term, env *Env) error {
	switch option := env.Resolve(option).(type) {
	case Variable:
		return InstantiationError(env)
	case *Compound:
		if len(option.Args) != 1 {
			return DomainError(ValidDomainReadOption, option, env)
		}

		v := env.Resolve(option.Args[0])
		switch option.Functor {
		case "singletons":
			opts.singletons = v
		case "variables":
			opts.variables = v
		case "variable_names":
			opts.variableNames = v
		default:
			return DomainError(ValidDomainReadOption, option, env)
		}
		return nil
	default:
		return DomainError(ValidDomainReadOption, option, env)
	}
}

var readByte = (*bufio.Reader).ReadByte

// GetByte reads a byte from the stream represented by streamOrAlias and unifies it with inByte.
func (state *State) GetByte(streamOrAlias, inByte Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := state.stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	if s.mode != StreamModeRead {
		return Error(PermissionError(OperationInput, PermissionTypeStream, streamOrAlias, env))
	}

	if s.streamType == StreamTypeText {
		return Error(PermissionError(OperationInput, PermissionTypeTextStream, streamOrAlias, env))
	}

	switch b := env.Resolve(inByte).(type) {
	case Variable:
		break
	case Integer:
		if b < 0 || b > 255 {
			return Error(TypeError(ValidTypeInByte, inByte, env))
		}
	default:
		return Error(TypeError(ValidTypeInByte, inByte, env))
	}

	b, err := readByte(s.buf)
	switch err {
	case nil:
		return Delay(func(context.Context) *Promise {
			return Unify(inByte, Integer(b), k, env)
		})
	case io.EOF:
		switch s.eofAction {
		case EOFActionError:
			return Error(PermissionError(OperationInput, PermissionTypePastEndOfStream, streamOrAlias, env))
		case EOFActionEOFCode:
			return Delay(func(context.Context) *Promise {
				return Unify(inByte, Integer(-1), k, env)
			})
		case EOFActionReset:
			return Delay(func(context.Context) *Promise {
				return state.GetByte(streamOrAlias, inByte, k, env)
			})
		default:
			return Error(SystemError(fmt.Errorf("unknown EOF action: %d", s.eofAction)))
		}
	default:
		return Error(err)
	}
}

var readRune = (*bufio.Reader).ReadRune

// GetChar reads a character from the stream represented by streamOrAlias and unifies it with char.
func (state *State) GetChar(streamOrAlias, char Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := state.stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	if s.mode != StreamModeRead {
		return Error(PermissionError(OperationInput, PermissionTypeStream, streamOrAlias, env))
	}

	if s.streamType == StreamTypeBinary {
		return Error(PermissionError(OperationInput, PermissionTypeBinaryStream, streamOrAlias, env))
	}

	switch c := env.Resolve(char).(type) {
	case Variable:
		break
	case Atom:
		if len([]rune(c)) != 1 {
			return Error(TypeError(ValidTypeInCharacter, char, env))
		}
	default:
		return Error(TypeError(ValidTypeInCharacter, char, env))
	}

	r, _, err := readRune(s.buf)
	switch err {
	case nil:
		if r == unicode.ReplacementChar {
			return Error(RepresentationError(FlagCharacter, env))
		}

		return Delay(func(context.Context) *Promise {
			return Unify(char, Atom(r), k, env)
		})
	case io.EOF:
		switch s.eofAction {
		case EOFActionError:
			return Error(PermissionError(OperationInput, PermissionTypePastEndOfStream, streamOrAlias, env))
		case EOFActionEOFCode:
			return Delay(func(context.Context) *Promise {
				return Unify(char, Atom("end_of_file"), k, env)
			})
		case EOFActionReset:
			return Delay(func(context.Context) *Promise {
				return state.GetChar(streamOrAlias, char, k, env)
			})
		default:
			return Error(SystemError(fmt.Errorf("unknown EOF action: %d", s.eofAction)))
		}
	default:
		return Error(SystemError(err))
	}
}

var peek = (*bufio.Reader).Peek

// PeekByte peeks a byte from the stream represented by streamOrAlias and unifies it with inByte.
func (state *State) PeekByte(streamOrAlias, inByte Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := state.stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	if s.mode != StreamModeRead {
		return Error(PermissionError(OperationInput, PermissionTypeStream, streamOrAlias, env))
	}

	if s.streamType == StreamTypeText {
		return Error(PermissionError(OperationInput, PermissionTypeTextStream, streamOrAlias, env))
	}

	switch b := env.Resolve(inByte).(type) {
	case Variable:
		break
	case Integer:
		if b < 0 || b > 255 {
			return Error(TypeError(ValidTypeInByte, inByte, env))
		}
	default:
		return Error(TypeError(ValidTypeInByte, inByte, env))
	}

	b, err := peek(s.buf, 1)
	switch err {
	case nil:
		return Delay(func(context.Context) *Promise {
			return Unify(inByte, Integer(b[0]), k, env)
		})
	case io.EOF:
		switch s.eofAction {
		case EOFActionError:
			return Error(PermissionError(OperationInput, PermissionTypePastEndOfStream, streamOrAlias, env))
		case EOFActionEOFCode:
			return Delay(func(context.Context) *Promise {
				return Unify(inByte, Integer(-1), k, env)
			})
		case EOFActionReset:
			return Delay(func(context.Context) *Promise {
				return state.PeekByte(streamOrAlias, inByte, k, env)
			})
		default:
			return Error(SystemError(fmt.Errorf("unknown EOF action: %d", s.eofAction)))
		}
	default:
		return Error(SystemError(err))
	}
}

var unreadRune = (*bufio.Reader).UnreadRune

// PeekChar peeks a rune from the stream represented by streamOrAlias and unifies it with char.
func (state *State) PeekChar(streamOrAlias, char Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := state.stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	if s.mode != StreamModeRead {
		return Error(PermissionError(OperationInput, PermissionTypeStream, streamOrAlias, env))
	}

	if s.streamType == StreamTypeBinary {
		return Error(PermissionError(OperationInput, PermissionTypeBinaryStream, streamOrAlias, env))
	}

	switch c := env.Resolve(char).(type) {
	case Variable:
		break
	case Atom:
		if len([]rune(c)) != 1 {
			return Error(TypeError(ValidTypeInCharacter, char, env))
		}
	default:
		return Error(TypeError(ValidTypeInCharacter, char, env))
	}

	r, _, err := readRune(s.buf)
	switch err {
	case nil:
		if err := unreadRune(s.buf); err != nil {
			return Error(SystemError(err))
		}

		if r == unicode.ReplacementChar {
			return Error(RepresentationError(FlagCharacter, env))
		}

		return Delay(func(context.Context) *Promise {
			return Unify(char, Atom(r), k, env)
		})
	case io.EOF:
		switch s.eofAction {
		case EOFActionError:
			return Error(PermissionError(OperationInput, PermissionTypePastEndOfStream, streamOrAlias, env))
		case EOFActionEOFCode:
			return Delay(func(context.Context) *Promise {
				return Unify(char, Atom("end_of_file"), k, env)
			})
		case EOFActionReset:
			return Delay(func(context.Context) *Promise {
				return state.PeekChar(streamOrAlias, char, k, env)
			})
		default:
			return Error(SystemError(fmt.Errorf("unknown EOF action: %d", s.eofAction)))
		}
	default:
		return Error(SystemError(err))
	}
}

var osExit = os.Exit

// Halt exits the process with exit code of n.
func Halt(n Term, k func(*Env) *Promise, env *Env) *Promise {
	switch code := env.Resolve(n).(type) {
	case Variable:
		return Error(InstantiationError(env))
	case Integer:
		osExit(int(code))
		return k(env)
	default:
		return Error(TypeError(ValidTypeInteger, n, env))
	}
}

// Clause unifies head and body with H and B respectively where H :- B is in the database.
func (state *State) Clause(head, body Term, k func(*Env) *Promise, env *Env) *Promise {
	pi, _, err := piArgs(head, env)
	if err != nil {
		return Error(err)
	}

	switch env.Resolve(body).(type) {
	case Variable, Atom, *Compound:
		break
	default:
		return Error(TypeError(ValidTypeCallable, body, env))
	}

	p, ok := state.procedures[pi]
	if !ok {
		return Bool(false)
	}

	cs, ok := p.(clauses)
	if !ok {
		return Error(PermissionError(OperationAccess, PermissionTypePrivateProcedure, pi.Term(), env))
	}

	ks := make([]func(context.Context) *Promise, len(cs))
	for i := range cs {
		r := Rulify(copyTerm(cs[i].raw, nil, env), env)
		ks[i] = func(context.Context) *Promise {
			return Unify(&Compound{
				Functor: ":-",
				Args:    []Term{head, body},
			}, r, k, env)
		}
	}
	return Delay(ks...)
}

// AtomLength counts the runes in atom and unifies the result with length.
func AtomLength(atom, length Term, k func(*Env) *Promise, env *Env) *Promise {
	switch a := env.Resolve(atom).(type) {
	case Variable:
		return Error(InstantiationError(env))
	case Atom:
		switch l := env.Resolve(length).(type) {
		case Variable:
			break
		case Integer:
			if l < 0 {
				return Error(DomainError(ValidDomainNotLessThanZero, length, env))
			}
		default:
			return Error(TypeError(ValidTypeInteger, length, env))
		}

		return Delay(func(context.Context) *Promise {
			return Unify(length, Integer(len([]rune(a))), k, env)
		})
	default:
		return Error(TypeError(ValidTypeAtom, atom, env))
	}
}

// AtomConcat concatenates atom1 and atom2 and unifies it with atom3.
func AtomConcat(atom1, atom2, atom3 Term, k func(*Env) *Promise, env *Env) *Promise {
	switch a3 := env.Resolve(atom3).(type) {
	case Variable:
		switch a1 := env.Resolve(atom1).(type) {
		case Variable:
			return Error(InstantiationError(env))
		case Atom:
			switch a2 := env.Resolve(atom2).(type) {
			case Variable:
				return Error(InstantiationError(env))
			case Atom:
				return Delay(func(context.Context) *Promise {
					return Unify(a1+a2, a3, k, env)
				})
			default:
				return Error(TypeError(ValidTypeAtom, atom2, env))
			}
		default:
			return Error(TypeError(ValidTypeAtom, atom1, env))
		}
	case Atom:
		switch env.Resolve(atom1).(type) {
		case Variable, Atom:
			break
		default:
			return Error(TypeError(ValidTypeAtom, atom1, env))
		}

		switch env.Resolve(atom2).(type) {
		case Variable, Atom:
			break
		default:
			return Error(TypeError(ValidTypeAtom, atom2, env))
		}

		pattern := Compound{Args: []Term{atom1, atom2}}
		ks := make([]func(context.Context) *Promise, 0, len(a3)+1)
		for i := range a3 {
			a1, a2 := a3[:i], a3[i:]
			ks = append(ks, func(context.Context) *Promise {
				return Unify(&pattern, &Compound{Args: []Term{a1, a2}}, k, env)
			})
		}
		ks = append(ks, func(context.Context) *Promise {
			return Unify(&pattern, &Compound{Args: []Term{a3, Atom("")}}, k, env)
		})
		return Delay(ks...)
	default:
		return Error(TypeError(ValidTypeAtom, atom3, env))
	}
}

// SubAtom unifies subAtom with a sub atom of atom of length which appears with before runes preceding it and after runes following it.
func SubAtom(atom, before, length, after, subAtom Term, k func(*Env) *Promise, env *Env) *Promise {
	switch whole := env.Resolve(atom).(type) {
	case Variable:
		return Error(InstantiationError(env))
	case Atom:
		rs := []rune(whole)

		if err := checkPositiveInteger(before, env); err != nil {
			return Error(err)
		}

		if err := checkPositiveInteger(length, env); err != nil {
			return Error(err)
		}

		if err := checkPositiveInteger(after, env); err != nil {
			return Error(err)
		}

		switch env.Resolve(subAtom).(type) {
		case Variable, Atom:
			break
		default:
			return Error(TypeError(ValidTypeAtom, subAtom, env))
		}

		const subAtomPattern = Atom("$sub_atom_pattern")
		pattern := subAtomPattern.Apply(before, length, after, subAtom)
		var ks []func(context.Context) *Promise
		for i := 0; i <= len(rs); i++ {
			for j := i; j <= len(rs); j++ {
				before, length, after, subAtom := Integer(i), Integer(j-i), Integer(len(rs)-j), Atom(rs[i:j])
				ks = append(ks, func(context.Context) *Promise {
					return Unify(pattern, subAtomPattern.Apply(before, length, after, subAtom), k, env)
				})
			}
		}
		return Delay(ks...)
	default:
		return Error(TypeError(ValidTypeAtom, atom, env))
	}
}

func checkPositiveInteger(n Term, env *Env) error {
	switch b := env.Resolve(n).(type) {
	case Variable:
		return nil
	case Integer:
		if b < 0 {
			return DomainError(ValidDomainNotLessThanZero, n, env)
		}
		return nil
	default:
		return TypeError(ValidTypeInteger, n, env)
	}
}

// AtomChars breaks down atom into list of characters and unifies with chars, or constructs an atom from a list of
// characters chars and unifies it with atom.
func AtomChars(atom, chars Term, k func(*Env) *Promise, env *Env) *Promise {
	switch a := env.Resolve(atom).(type) {
	case Variable:
		var sb strings.Builder
		iter := ListIterator{List: chars, Env: env}
		for iter.Next() {
			switch e := env.Resolve(iter.Current()).(type) {
			case Variable:
				return Error(InstantiationError(env))
			case Atom:
				if len([]rune(e)) != 1 {
					return Error(TypeError(ValidTypeCharacter, e, env))
				}
				_, _ = sb.WriteString(string(e))
			default:
				return Error(TypeError(ValidTypeCharacter, e, env))
			}
		}
		if err := iter.Err(); err != nil {
			return Error(err)
		}
		return Delay(func(context.Context) *Promise {
			return Unify(atom, Atom(sb.String()), k, env)
		})
	case Atom:
		rs := []rune(a)
		cs := make([]Term, len(rs))
		for i, r := range rs {
			cs[i] = Atom(r)
		}
		return Delay(func(context.Context) *Promise {
			return Unify(chars, List(cs...), k, env)
		})
	default:
		return Error(TypeError(ValidTypeAtom, a, env))
	}
}

// AtomCodes breaks up atom into a list of runes and unifies it with codes, or constructs an atom from the list of runes
// and unifies it with atom.
func AtomCodes(atom, codes Term, k func(*Env) *Promise, env *Env) *Promise {
	switch a := env.Resolve(atom).(type) {
	case Variable:
		var sb strings.Builder
		iter := ListIterator{List: codes, Env: env}
		for iter.Next() {
			switch e := env.Resolve(iter.Current()).(type) {
			case Variable:
				return Error(InstantiationError(env))
			case Integer:
				_, _ = sb.WriteRune(rune(e))
			default:
				return Error(RepresentationError(FlagCharacterCode, env))
			}
		}
		if err := iter.Err(); err != nil {
			return Error(err)
		}
		return Delay(func(context.Context) *Promise {
			return Unify(atom, Atom(sb.String()), k, env)
		})
	case Atom:
		rs := []rune(a)
		cs := make([]Term, len(rs))
		for i, r := range rs {
			cs[i] = Integer(r)
		}
		return Delay(func(context.Context) *Promise {
			return Unify(codes, List(cs...), k, env)
		})
	default:
		return Error(TypeError(ValidTypeAtom, atom, env))
	}
}

// NumberChars breaks up an atom representation of a number num into a list of characters and unifies it with chars, or
// constructs a number from a list of characters chars and unifies it with num.
func NumberChars(num, chars Term, k func(*Env) *Promise, env *Env) *Promise {
	var (
		sb   strings.Builder
		iter = ListIterator{List: chars, Env: env}
	)
	for iter.Next() {
		switch e := env.Resolve(iter.Current()).(type) {
		case Variable:
			return numberCharsWrite(num, chars, k, env)
		case Atom:
			if len([]rune(e)) != 1 {
				return Error(TypeError(ValidTypeCharacter, e, env))
			}
			_, _ = sb.WriteString(string(e))
		default:
			return Error(TypeError(ValidTypeCharacter, e, env))
		}
	}
	if err := iter.Err(); err != nil {
		if _, ok := iter.Suffix().(Variable); ok {
			return numberCharsWrite(num, chars, k, env)
		}
		return Error(err)
	}

	p := newParser(bufio.NewReader(strings.NewReader(sb.String())))
	t, err := p.Number()
	if err != nil {
		return Error(SyntaxError(err, env))
	}

	switch n := env.Resolve(num).(type) {
	case Variable, Number:
		return Unify(n, t, k, env)
	default:
		return Error(TypeError(ValidTypeNumber, n, env))
	}
}

func numberCharsWrite(num, chars Term, k func(*Env) *Promise, env *Env) *Promise {
	switch n := env.Resolve(num).(type) {
	case Variable:
		return Error(InstantiationError(env))
	case Number:
		iter := ListIterator{List: chars, Env: env}
		for iter.Next() {
			switch e := env.Resolve(iter.Current()).(type) {
			case Variable:
				break
			case Atom:
				if len(e) != 1 {
					return Error(TypeError(ValidTypeCharacter, e, env))
				}
			default:
				return Error(TypeError(ValidTypeCharacter, e, env))
			}
		}
		if err := iter.Err(); err != nil {
			if _, ok := iter.Suffix().(Variable); !ok {
				return Error(err)
			}
		}

		var buf bytes.Buffer
		_ = n.WriteTerm(&buf, &defaultWriteOptions, nil)
		rs := []rune(buf.String())

		cs := make([]Term, len(rs))
		for i, r := range rs {
			cs[i] = Atom(r)
		}
		return Unify(chars, List(cs...), k, env)
	default:
		return Error(TypeError(ValidTypeNumber, n, env))
	}
}

// NumberCodes breaks up an atom representation of a number num into a list of runes and unifies it with codes, or
// constructs a number from a list of runes codes and unifies it with num.
func NumberCodes(num, codes Term, k func(*Env) *Promise, env *Env) *Promise {
	switch codes := env.Resolve(codes).(type) {
	case Variable:
		break
	default:
		switch n := env.Resolve(num).(type) {
		case Variable, Integer, Float:
			break
		default:
			return Error(TypeError(ValidTypeNumber, n, env))
		}

		var sb strings.Builder
		iter := ListIterator{List: codes, Env: env}
		for iter.Next() {
			switch e := env.Resolve(iter.Current()).(type) {
			case Variable:
				return Error(InstantiationError(env))
			case Integer:
				_, _ = sb.WriteRune(rune(e))
			default:
				return Error(RepresentationError(FlagCharacterCode, env))
			}
		}
		if err := iter.Err(); err != nil {
			return Error(err)
		}

		p := newParser(bufio.NewReader(strings.NewReader(sb.String())))
		t, err := p.Number()
		if err != nil {
			return Error(SyntaxError(err, env))
		}

		return Delay(func(context.Context) *Promise {
			return Unify(num, t, k, env)
		})
	}

	switch n := env.Resolve(num).(type) {
	case Variable:
		return Error(InstantiationError(env))
	case Integer, Float:
		var buf bytes.Buffer
		_ = n.WriteTerm(&buf, &defaultWriteOptions, nil)
		rs := []rune(buf.String())
		cs := make([]Term, len(rs))
		for i, r := range rs {
			cs[i] = Integer(r)
		}
		return Delay(func(context.Context) *Promise {
			return Unify(codes, List(cs...), k, env)
		})
	default:
		return Error(TypeError(ValidTypeNumber, num, env))
	}
}

// StreamProperty succeeds iff the stream represented by streamOrAlias has the stream property property.
func (state *State) StreamProperty(streamOrAlias, property Term, k func(*Env) *Promise, env *Env) *Promise {
	streams := make([]*Stream, 0, len(state.streams))
	switch s := env.Resolve(streamOrAlias).(type) {
	case Variable:
		for _, v := range state.streams {
			streams = append(streams, v)
		}
	case Atom: // ISO standard stream_property/2 doesn't take an alias but why not?
		v, ok := state.streams[s]
		if !ok {
			return Error(ExistenceError(ObjectTypeStream, streamOrAlias, env))
		}
		streams = append(streams, v)
	case *Stream:
		streams = append(streams, s)
	default:
		return Error(DomainError(ValidDomainStreamOrAlias, streamOrAlias, env))
	}

	if err := checkStreamProperty(property, env); err != nil {
		return Error(err)
	}

	var ks []func(context.Context) *Promise
	for _, s := range streams {
		properties, err := s.properties()
		if err != nil {
			return Error(err)
		}

		for i := range properties {
			p := properties[i]
			ks = append(ks, func(context.Context) *Promise {
				return Unify(property, p, k, env)
			})
		}
	}
	return Delay(ks...)
}

func checkStreamProperty(property Term, env *Env) error {
	switch p := env.Resolve(property).(type) {
	case Variable:
		return nil
	case Atom:
		switch p {
		case "input", "output":
			return nil
		default:
			return DomainError(ValidDomainStreamProperty, property, env)
		}
	case *Compound:
		if len(p.Args) != 1 {
			return DomainError(ValidDomainStreamProperty, property, env)
		}
		arg := p.Args[0]
		switch p.Functor {
		case "file_name", "mode", "alias", "end_of_stream", "eof_action", "reposition":
			return checkAtom(arg, env)
		case "position":
			return checkInteger(arg, env)
		default:
			return DomainError(ValidDomainStreamProperty, property, env)
		}
	default:
		return DomainError(ValidDomainStreamProperty, property, env)
	}
}

func checkAtom(t Term, env *Env) error {
	switch env.Resolve(t).(type) {
	case Variable, Atom:
		return nil
	default:
		return TypeError(ValidTypeAtom, t, env)
	}
}

func checkInteger(t Term, env *Env) error {
	switch env.Resolve(t).(type) {
	case Variable, Integer:
		return nil
	default:
		return TypeError(ValidTypeAtom, t, env)
	}
}

var seek = io.Seeker.Seek

// SetStreamPosition sets the position property of the stream represented by streamOrAlias.
func (state *State) SetStreamPosition(streamOrAlias, position Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := state.stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	if !s.reposition {
		return Error(PermissionError(OperationReposition, PermissionTypeStream, streamOrAlias, env))
	}

	switch p := env.Resolve(position).(type) {
	case Variable:
		return Error(InstantiationError(env))
	case Integer:
		if f, ok := s.file.(io.Seeker); ok {
			if _, err := seek(f, int64(p), 0); err != nil {
				return Error(SystemError(err))
			}

			s.buf.Reset(s.file)
		}

		return k(env)
	default:
		return Error(TypeError(ValidTypeInteger, position, env))
	}
}

// CharConversion registers a character conversion from inChar to outChar, or remove the conversion if inChar = outChar.
func (state *State) CharConversion(inChar, outChar Term, k func(*Env) *Promise, env *Env) *Promise {
	switch in := env.Resolve(inChar).(type) {
	case Variable:
		return Error(InstantiationError(env))
	case Atom:
		i := []rune(in)
		if len(i) != 1 {
			return Error(RepresentationError(FlagCharacter, env))
		}

		switch out := env.Resolve(outChar).(type) {
		case Variable:
			return Error(InstantiationError(env))
		case Atom:
			o := []rune(out)
			if len(o) != 1 {
				return Error(RepresentationError(FlagCharacter, env))
			}

			if state.charConversions == nil {
				state.charConversions = map[rune]rune{}
			}
			if i[0] == o[0] {
				delete(state.charConversions, i[0])
				return k(env)
			}
			state.charConversions[i[0]] = o[0]
			return k(env)
		default:
			return Error(RepresentationError(FlagCharacter, env))
		}
	default:
		return Error(RepresentationError(FlagCharacter, env))
	}
}

// CurrentCharConversion succeeds iff a conversion from inChar to outChar is defined.
func (state *State) CurrentCharConversion(inChar, outChar Term, k func(*Env) *Promise, env *Env) *Promise {
	switch in := env.Resolve(inChar).(type) {
	case Variable:
		break
	case Atom:
		i := []rune(in)
		if len(i) != 1 {
			return Error(RepresentationError(FlagCharacter, env))
		}
	default:
		return Error(RepresentationError(FlagCharacter, env))
	}

	switch out := env.Resolve(outChar).(type) {
	case Variable:
		break
	case Atom:
		o := []rune(out)
		if len(o) != 1 {
			return Error(RepresentationError(FlagCharacter, env))
		}
	default:
		return Error(RepresentationError(FlagCharacter, env))
	}

	if c1, ok := env.Resolve(inChar).(Atom); ok {
		r := []rune(c1)
		if r, ok := state.charConversions[r[0]]; ok {
			return Delay(func(context.Context) *Promise {
				return Unify(outChar, Atom(r), k, env)
			})
		}
		return Delay(func(context.Context) *Promise {
			return Unify(outChar, c1, k, env)
		})
	}

	pattern := Compound{Args: []Term{inChar, outChar}}
	ks := make([]func(context.Context) *Promise, 256)
	for i := 0; i < 256; i++ {
		r := rune(i)
		cr, ok := state.charConversions[r]
		if !ok {
			cr = r
		}

		ks[i] = func(context.Context) *Promise {
			return Unify(&pattern, &Compound{Args: []Term{Atom(r), Atom(cr)}}, k, env)
		}
	}
	return Delay(ks...)
}

// SetPrologFlag sets flag to value.
func (state *State) SetPrologFlag(flag, value Term, k func(*Env) *Promise, env *Env) *Promise {
	switch f := env.Resolve(flag).(type) {
	case Variable:
		return Error(InstantiationError(env))
	case Atom:
		var modify func(value Atom) error
		switch f {
		case "bounded", "max_integer", "min_integer", "integer_rounding_function", "max_arity":
			return Error(PermissionError(OperationModify, PermissionTypeFlag, f, env))
		case "char_conversion":
			modify = state.modifyCharConversion
		case "debug":
			modify = state.modifyDebug
		case "unknown":
			modify = state.modifyUnknown
		case "double_quotes":
			modify = state.modifyDoubleQuotes
		default:
			return Error(DomainError(ValidDomainPrologFlag, f, env))
		}

		switch v := env.Resolve(value).(type) {
		case Variable:
			return Error(InstantiationError(env))
		case Atom:
			if err := modify(v); err != nil {
				return Error(err)
			}
			return k(env)
		default:
			return Error(DomainError(ValidDomainFlagValue, &Compound{
				Functor: "+",
				Args:    []Term{flag, value},
			}, env))
		}
	default:
		return Error(TypeError(ValidTypeAtom, f, env))
	}
}

func (state *State) modifyCharConversion(value Atom) error {
	switch value {
	case "on":
		state.charConvEnabled = true
	case "off":
		state.charConvEnabled = false
	default:
		return DomainError(ValidDomainFlagValue, &Compound{
			Functor: "+",
			Args:    []Term{Atom("char_conversion"), value},
		}, nil)
	}
	return nil
}

func (state *State) modifyDebug(value Atom) error {
	switch value {
	case "on":
		state.debug = true
	case "off":
		state.debug = false
	default:
		return DomainError(ValidDomainFlagValue, &Compound{
			Functor: "+",
			Args:    []Term{Atom("debug"), value},
		}, nil)
	}
	return nil
}

func (state *State) modifyUnknown(value Atom) error {
	switch value {
	case "error":
		state.unknown = unknownError
	case "warning":
		state.unknown = unknownWarning
	case "fail":
		state.unknown = unknownFail
	default:
		return DomainError(ValidDomainFlagValue, &Compound{
			Functor: "+",
			Args:    []Term{Atom("unknown"), value},
		}, nil)
	}
	return nil
}

func (state *State) modifyDoubleQuotes(value Atom) error {
	switch value {
	case "codes":
		state.doubleQuotes = doubleQuotesCodes
	case "chars":
		state.doubleQuotes = doubleQuotesChars
	case "atom":
		state.doubleQuotes = doubleQuotesAtom
	default:
		return DomainError(ValidDomainFlagValue, &Compound{
			Functor: "+",
			Args:    []Term{Atom("double_quotes"), value},
		}, nil)
	}
	return nil
}

// CurrentPrologFlag succeeds iff flag is set to value.
func (state *State) CurrentPrologFlag(flag, value Term, k func(*Env) *Promise, env *Env) *Promise {
	switch f := env.Resolve(flag).(type) {
	case Variable:
		break
	case Atom:
		switch f {
		case "bounded", "max_integer", "min_integer", "integer_rounding_function", "char_conversion", "debug", "max_arity", "unknown", "double_quotes":
			break
		default:
			return Error(DomainError(ValidDomainPrologFlag, f, env))
		}
	default:
		return Error(TypeError(ValidTypeAtom, f, env))
	}

	pattern := Compound{Args: []Term{flag, value}}
	flags := []Term{
		&Compound{Args: []Term{Atom("bounded"), Atom("true")}},
		&Compound{Args: []Term{Atom("max_integer"), maxInt}},
		&Compound{Args: []Term{Atom("min_integer"), minInt}},
		&Compound{Args: []Term{Atom("integer_rounding_function"), Atom("toward_zero")}},
		&Compound{Args: []Term{Atom("char_conversion"), onOff(state.charConvEnabled)}},
		&Compound{Args: []Term{Atom("debug"), onOff(state.debug)}},
		&Compound{Args: []Term{Atom("max_arity"), Atom("unbounded")}},
		&Compound{Args: []Term{Atom("unknown"), Atom(state.unknown.String())}},
		&Compound{Args: []Term{Atom("double_quotes"), Atom(state.doubleQuotes.String())}},
	}
	ks := make([]func(context.Context) *Promise, len(flags))
	for i := range flags {
		f := flags[i]
		ks[i] = func(context.Context) *Promise {
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

func (state *State) stream(streamOrAlias Term, env *Env) (*Stream, error) {
	switch s := env.Resolve(streamOrAlias).(type) {
	case Variable:
		return nil, InstantiationError(env)
	case Atom:
		v, ok := state.streams[s]
		if !ok {
			return nil, ExistenceError(ObjectTypeStream, streamOrAlias, env)
		}
		return v, nil
	case *Stream:
		return s, nil
	default:
		return nil, DomainError(ValidDomainStreamOrAlias, streamOrAlias, env)
	}
}

// Dynamic declares a procedure indicated by pi is user-defined dynamic.
func (state *State) Dynamic(pi Term, k func(*Env) *Promise, env *Env) *Promise {
	iter := AnyIterator{Any: pi, Env: env}
	for iter.Next() {
		elem := iter.Current()
		key, err := NewProcedureIndicator(elem, env)
		if err != nil {
			return Error(err)
		}
		if state.procedures == nil {
			state.procedures = map[ProcedureIndicator]procedure{}
		}
		p, ok := state.procedures[key]
		if !ok {
			state.procedures[key] = clauses{}
			continue
		}
		if _, ok := p.(clauses); !ok {
			return Error(PermissionError(OperationModify, PermissionTypeStaticProcedure, elem, env))
		}
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}
	return k(env)
}

// BuiltIn declares a procedure indicated by pi is built-in and static.
func (state *State) BuiltIn(pi Term, k func(*Env) *Promise, env *Env) *Promise {
	iter := AnyIterator{Any: pi, Env: env}
	for iter.Next() {
		elem := iter.Current()
		key, err := NewProcedureIndicator(elem, env)
		if err != nil {
			return Error(err)
		}
		if state.procedures == nil {
			state.procedures = map[ProcedureIndicator]procedure{}
		}
		p, ok := state.procedures[key]
		if !ok {
			state.procedures[key] = builtin{}
			continue
		}
		if _, ok := p.(builtin); !ok {
			return Error(PermissionError(OperationModify, PermissionTypeStaticProcedure, elem, env))
		}
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}
	return k(env)
}

// ExpandTerm transforms term1 according to term_expansion/2 and DCG rules then unifies with term2.
func (state *State) ExpandTerm(term1, term2 Term, k func(*Env) *Promise, env *Env) *Promise {
	t, err := state.Expand(term1, env)
	if err != nil {
		return Error(err)
	}

	return Unify(t, term2, k, env)
}

// Expand expands term according to term_expansion/2 and DCG rules.
func (state *State) Expand(term Term, env *Env) (Term, error) {
	const termExpansion = Atom("term_expansion")

	if _, ok := state.procedures[ProcedureIndicator{Name: termExpansion, Arity: 2}]; ok {
		var ret Term
		v := NewVariable()
		ok, err := state.Call(termExpansion.Apply(term, v), func(env *Env) *Promise {
			ret = env.Simplify(v)
			return Bool(true)
		}, env).Force(context.Background())
		if err != nil {
			return nil, err
		}
		if ok {
			return ret, nil
		}
	}

	t, err := expandDCG(term, env)
	if errors.Is(err, errDCGNotApplicable) {
		return term, nil
	}
	return t, err
}

// Environ succeeds if an environment variable key has value.
func Environ(key, value Term, k func(*Env) *Promise, env *Env) *Promise {
	lines := os.Environ()
	ks := make([]func(ctx context.Context) *Promise, len(lines))
	for i, l := range lines {
		kv := strings.SplitN(l, "=", 2)
		ks[i] = func(ctx context.Context) *Promise {
			return Unify(&Compound{
				Args: []Term{key, value},
			}, &Compound{
				Args: []Term{Atom(kv[0]), Atom(kv[1])},
			}, k, env)
		}
	}
	return Delay(ks...)
}

// Nth0 succeeds if elem is the n-th element of list, counting from 0.
func Nth0(n, list, elem Term, k func(*Env) *Promise, env *Env) *Promise {
	return nth(0, n, list, elem, k, env)
}

// Nth1 succeeds if elem is the n-th element of list, counting from 1.
func Nth1(n, list, elem Term, k func(*Env) *Promise, env *Env) *Promise {
	return nth(1, n, list, elem, k, env)
}

func nth(base Integer, n, list, elem Term, k func(*Env) *Promise, env *Env) *Promise {
	switch n := env.Resolve(n).(type) {
	case Variable:
		const idx = Atom("$idx")
		var ks []func(context.Context) *Promise
		iter := ListIterator{List: list, Env: env}
		for i := base; iter.Next(); i++ {
			i, e := i, iter.Current()
			ks = append(ks, func(context.Context) *Promise {
				return Unify(idx.Apply(n, elem), idx.Apply(i, e), k, env)
			})
		}
		if err := iter.Err(); err != nil {
			return Error(err)
		}
		return Delay(ks...)
	case Integer:
		if n < base {
			return Bool(false)
		}
		iter := ListIterator{List: list, Env: env}
		for i := base; iter.Next(); i++ {
			if i == n {
				return Unify(elem, iter.Current(), k, env)
			}
		}
		if err := iter.Err(); err != nil {
			return Error(err)
		}
		return Bool(false)
	default:
		return Error(TypeError(ValidTypeInteger, n, env))
	}
}

// Succ succeeds if s is the successor of non-negative integer x.
func Succ(x, s Term, k func(*Env) *Promise, env *Env) *Promise {
	switch x := x.(type) {
	case Variable:
		switch s := s.(type) {
		case Variable:
			return Error(InstantiationError(env))
		case Integer:
			switch {
			case s < Integer(0):
				return Error(DomainError(ValidDomainNotLessThanZero, s, env))
			case s == Integer(0):
				return Bool(false)
			default:
				return Unify(x, s-Integer(1), k, env)
			}
		default:
			return Error(TypeError(ValidTypeInteger, s, env))
		}
	case Integer:
		if x < Integer(0) {
			return Error(DomainError(ValidDomainNotLessThanZero, x, env))
		}

		r, err := Add(x, Integer(1))
		if err != nil {
			var ev ExceptionalValue
			if errors.As(err, &ev) {
				return Error(EvaluationError(ev, env))
			}
			return Error(err)
		}

		switch s := s.(type) {
		case Variable:
			return Unify(s, r, k, env)
		case Integer:
			if s < Integer(0) {
				return Error(DomainError(ValidDomainNotLessThanZero, s, env))
			}
			return Unify(s, r, k, env)
		default:
			return Error(TypeError(ValidTypeInteger, s, env))
		}
	default:
		return Error(TypeError(ValidTypeInteger, x, env))
	}
}

// Length succeeds iff list is a list of length.
func Length(list, length Term, k func(*Env) *Promise, env *Env) *Promise {
	// https://github.com/mthom/scryer-prolog/issues/1325#issue-1160713156
	// Note that it's a bit simpler since we don't have attributed variables (yet).

	n := env.Resolve(length)
	switch n := n.(type) {
	case Variable:
		break
	case Integer:
		if n < 0 {
			return Error(DomainError(ValidDomainNotLessThanZero, n, env))
		}
	default:
		return Error(TypeError(ValidTypeInteger, n, env))
	}

	var (
		skipped = NewVariable()
		suffix  = NewVariable()
	)
	return SkipMaxList(skipped, n, list, suffix, func(env *Env) *Promise {
		skipped := env.Resolve(skipped).(Integer)

		switch suffix := env.Resolve(suffix).(type) {
		case Variable: // partial list
			if n, ok := n.(Integer); ok {
				return lengthRundown(suffix, n-skipped, k, env)
			}

			n := n.(Variable)

			if n == suffix {
				return Error(ResourceError(ResourceFiniteMemory, env))
			}

			return lengthAddendum(Atom("[]"), skipped, suffix, n, k, env)
		case Atom: // list or non-list terminated by an atom
			if suffix != "[]" {
				return Bool(false)
			}

			return Unify(n, skipped, k, env)
		case *Compound: // non-list including infinite list
			if suffix.Functor != "." || len(suffix.Args) != 2 {
				return Bool(false)
			}

			if _, ok := n.(Variable); !ok {
				return Bool(false)
			}

			return Error(ResourceError(ResourceFiniteMemory, env))
		default: // non-list terminated by a term that is neither an atom nor a compound
			return Bool(false)
		}
	}, env)
}

func lengthRundown(list Variable, n Integer, k func(*Env) *Promise, env *Env) *Promise {
	ret := Term(Atom("[]"))
	for i := Integer(0); i < n; i++ {
		ret = Atom(".").Apply(NewVariable(), ret)
	}
	return Unify(list, ret, k, env)
}

func lengthAddendum(suffix Term, offset Integer, list, length Variable, k func(*Env) *Promise, env *Env) *Promise {
	return Delay(func(context.Context) *Promise {
		const a = Atom("$addendum")
		return Unify(a.Apply(list, length), a.Apply(suffix, offset), k, env)
	}, func(context.Context) *Promise {
		suffix := Atom(".").Apply(NewVariable(), suffix)
		offset, err := addI(offset, 1)
		if err != nil {
			return Error(RepresentationError(FlagMaxInteger, env))
		}
		return lengthAddendum(suffix, offset, list, length, k, env)
	})
}

// SkipMaxList iterates over list up to max elements and unifies the number of skipped elements with skip and the rest with suffix.
func SkipMaxList(skip, max, list, suffix Term, k func(*Env) *Promise, env *Env) *Promise {
	m := maxInt
	switch max := env.Resolve(max).(type) {
	case Variable:
		break
	case Integer:
		if max < 0 {
			return Error(DomainError(ValidDomainNotLessThanZero, max, env))
		}
		m = max
	default:
		return Error(TypeError(ValidTypeInteger, max, env))
	}

	var (
		iter = ListIterator{List: list, Env: env}
		n    = Integer(0)
	)
	for n < m && iter.Next() {
		n++
	}

	const s = Atom("$skipped")
	return Unify(s.Apply(skip, suffix), s.Apply(n, iter.Suffix()), k, env)
}
