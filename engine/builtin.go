package engine

import (
	"bufio"
	"bytes"
	"context"
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
	return newParser(br, state.charConversions,
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
		return Error(ErrInstantiation)
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
			return Error(ErrInstantiation)
		case Integer:
			switch {
			case arity < 0:
				return Error(domainErrorNotLessThanZero(arity))
			case arity == 0:
				return Unify(t, name, k, env)
			}

			switch name := env.Resolve(name).(type) {
			case Variable:
				return Error(ErrInstantiation)
			case *Compound:
				return Error(TypeErrorAtomic(name))
			case Atom:
				vs := make([]Term, arity)
				for i := range vs {
					vs[i] = NewVariable()
				}
				return Delay(func(context.Context) *Promise {
					return Unify(t, &Compound{
						Functor: name,
						Args:    vs,
					}, k, env)
				})
			default:
				return Error(TypeErrorAtom(name))
			}
		default:
			return Error(TypeErrorInteger(arity))
		}
	case *Compound:
		pattern := Compound{Args: []Term{name, arity}}
		return Delay(func(context.Context) *Promise {
			return Unify(&pattern, &Compound{Args: []Term{t.Functor, Integer(len(t.Args))}}, k, env)
		})
	default: // atomic
		pattern := Compound{Args: []Term{name, arity}}
		return Delay(func(context.Context) *Promise {
			return Unify(&pattern, &Compound{Args: []Term{t, Integer(0)}}, k, env)
		})
	}
}

// Arg extracts nth argument of term as arg, or finds the argument position of arg in term as nth.
func Arg(nth, t, arg Term, k func(*Env) *Promise, env *Env) *Promise {
	switch c := env.Resolve(t).(type) {
	case Variable:
		return Error(ErrInstantiation)
	case *Compound:
		switch n := env.Resolve(nth).(type) {
		case Variable:
			return Error(ErrInstantiation)
		case Integer:
			if n == 0 || int(n) >= len(c.Args) {
				return Bool(false)
			}
			if n < 0 {
				return Error(domainErrorNotLessThanZero(n))
			}
			return Delay(func(context.Context) *Promise {
				return Unify(arg, c.Args[int(n)-1], k, env)
			})
		default:
			return Error(TypeErrorInteger(n))
		}
	default:
		return Error(TypeErrorCompound(t))
	}
}

// Univ constructs list as a list which first element is the functor of term and the rest is the arguments of term, or construct a compound from list as term.
func Univ(t, list Term, k func(*Env) *Promise, env *Env) *Promise {
	switch t := env.Resolve(t).(type) {
	case Variable:
		list = env.Resolve(list)
		if list == Atom("[]") {
			return Error(domainErrorNotEmptyList(list))
		}
		cons, ok := list.(*Compound)
		if !ok || cons.Functor != "." || len(cons.Args) != 2 {
			return Error(TypeErrorList(list))
		}

		f, ok := env.Resolve(cons.Args[0]).(Atom)
		if !ok {
			return Error(TypeErrorAtom(cons.Args[0]))
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
		return Error(TypeErrorInteger(priority))
	}
	if p < 0 || p > 1200 {
		return Error(domainErrorOperatorPriority(priority))
	}

	s, ok := env.Resolve(specifier).(Atom)
	if !ok {
		return Error(TypeErrorAtom(specifier))
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
		return Error(domainErrorOperatorSpecifier(s))
	}

	o, ok := env.Resolve(op).(Atom)
	if !ok {
		return Error(TypeErrorAtom(op))
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
			return Error(domainErrorOperatorPriority(priority))
		}
	default:
		return Error(domainErrorOperatorPriority(priority))
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
			return Error(domainErrorOperatorSpecifier(s))
		}
	default:
		return Error(domainErrorOperatorSpecifier(s))
	}

	switch env.Resolve(operator).(type) {
	case Variable, Atom:
		break
	default:
		return Error(TypeErrorAtom(operator))
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
			return permissionErrorModifyStaticProcedure(pi.Term())
		}
		state.procedures[pi] = builtin{merge(existing.clauses, added)}
		return nil
	case static:
		if !force {
			return permissionErrorModifyStaticProcedure(pi.Term())
		}
		state.procedures[pi] = static{merge(existing.clauses, added)}
		return nil
	default:
		return permissionErrorModifyStaticProcedure(pi.Term())
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
		return Error(ErrInstantiation)
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
			return Error(domainErrorOrder(order))
		}
	default:
		return Error(TypeErrorAtom(order))
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
		return Error(ErrInstantiation)
	default:
		return Error(TypeErrorInteger(lower))
	}

	switch upper := env.Resolve(upper).(type) {
	case Integer:
		high = upper
	case Variable:
		return Error(ErrInstantiation)
	default:
		return Error(TypeErrorInteger(upper))
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
		return Error(TypeErrorInteger(value))
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
		return Error(TypeErrorList(sorted))
	default:
		return Error(TypeErrorList(sorted))
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
			return Error(ErrInstantiation)
		case *Compound:
			if e.Functor != "-" || len(e.Args) != 2 {
				return Error(TypeErrorPair(e))
			}
			elems = append(elems, e)
		default:
			return Error(TypeErrorPair(e))
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
					return Error(TypeErrorPair(e))
				}
			default:
				return Error(TypeErrorPair(e))
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
	if _, ok := env.Resolve(ball).(Variable); ok {
		return Error(ErrInstantiation)
	}
	return Error(&Exception{
		Term: copyTerm(env.Resolve(ball), nil, env),
	})
}

// Catch calls goal. If an exception is thrown and unifies with catcher, it calls recover.
func (state *State) Catch(goal, catcher, recover Term, k func(*Env) *Promise, env *Env) *Promise {
	return Catch(func(err error) *Promise {
		var e *Exception
		if !errors.As(err, &e) {
			return nil
		}

		env, ok := catcher.Unify(e.Term, false, env)
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
			return Error(TypeErrorPredicateIndicator(pi))
		}
		if _, ok := env.Resolve(pi.Args[0]).(Atom); !ok {
			return Error(TypeErrorPredicateIndicator(pi))
		}
		if _, ok := env.Resolve(pi.Args[1]).(Integer); !ok {
			return Error(TypeErrorPredicateIndicator(pi))
		}
	default:
		return Error(TypeErrorPredicateIndicator(pi))
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
		return Error(permissionErrorModifyStaticProcedure(pi.Term()))
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
		return Error(ErrInstantiation)
	case *Compound:
		if pi.Functor != "/" || len(pi.Args) != 2 {
			return Error(TypeErrorPredicateIndicator(pi))
		}

		name, arity := pi.Args[0], pi.Args[1]

		switch name := env.Resolve(name).(type) {
		case Variable:
			return Error(ErrInstantiation)
		case Atom:
			switch arity := env.Resolve(arity).(type) {
			case Variable:
				return Error(ErrInstantiation)
			case Integer:
				if arity < 0 {
					return Error(domainErrorNotLessThanZero(arity))
				}
				key := ProcedureIndicator{Name: name, Arity: arity}
				if _, ok := state.procedures[key].(clauses); !ok {
					return Error(permissionErrorModifyStaticProcedure(&Compound{
						Functor: "/",
						Args:    []Term{name, arity},
					}))
				}
				delete(state.procedures, key)
				return k(env)
			default:
				return Error(TypeErrorInteger(arity))
			}
		default:
			return Error(TypeErrorAtom(name))
		}
	default:
		return Error(TypeErrorPredicateIndicator(pi))
	}
}

// CurrentInput unifies stream with the current input stream.
func (state *State) CurrentInput(stream Term, k func(*Env) *Promise, env *Env) *Promise {
	switch env.Resolve(stream).(type) {
	case Variable, *Stream:
		break
	default:
		return Error(domainErrorStream(stream))
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
		return Error(domainErrorStream(stream))
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
		return Error(permissionErrorInputStream(streamOrAlias))
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
		return Error(permissionErrorOutputStream(streamOrAlias))
	}

	state.output = s
	return k(env)
}

// Open opens SourceSink in mode and unifies with stream.
func (state *State) Open(SourceSink, mode, stream, options Term, k func(*Env) *Promise, env *Env) *Promise {
	var n Atom
	switch s := env.Resolve(SourceSink).(type) {
	case Variable:
		return Error(ErrInstantiation)
	case Atom:
		n = s
	default:
		return Error(domainErrorSourceSink(SourceSink))
	}

	var streamMode StreamMode
	switch m := env.Resolve(mode).(type) {
	case Variable:
		return Error(ErrInstantiation)
	case Atom:
		var ok bool
		streamMode, ok = map[Atom]StreamMode{
			"read":   StreamModeRead,
			"write":  StreamModeWrite,
			"append": StreamModeAppend,
		}[m]
		if !ok {
			return Error(domainErrorIOMode(m))
		}
	default:
		return Error(TypeErrorAtom(mode))
	}

	if _, ok := env.Resolve(stream).(Variable); !ok {
		return Error(ErrInstantiation)
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
	type optionIndicator struct {
		functor Atom
		arg     Atom
	}

	var oi optionIndicator
	switch o := env.Resolve(option).(type) {
	case Variable:
		return nil, ErrInstantiation
	case *Compound:
		if len(o.Args) != 1 {
			return nil, domainErrorStreamOption(option)
		}
		switch a := env.Resolve(o.Args[0]).(type) {
		case Variable:
			return nil, ErrInstantiation
		case Atom:
			oi = optionIndicator{
				functor: o.Functor,
				arg:     a,
			}
		default:
			return nil, TypeErrorAtom(a)
		}
	default:
		return nil, domainErrorStreamOption(option)
	}

	// alias is a bit different.
	if oi.functor == "alias" {
		if _, ok := state.streams[oi.arg]; ok {
			return nil, PermissionError("open", "source_sink", option)
		}

		return WithAlias(state, oi.arg), nil
	}

	switch oi {
	case optionIndicator{functor: "type", arg: "text"}:
		return WithStreamType(StreamTypeText), nil
	case optionIndicator{functor: "type", arg: "binary"}:
		return WithStreamType(StreamTypeBinary), nil
	case optionIndicator{functor: "reposition", arg: "true"}:
		return WithReposition(true), nil
	case optionIndicator{functor: "reposition", arg: "false"}:
		return WithReposition(false), nil
	case optionIndicator{functor: "eof_action", arg: "error"}:
		return WithEOFAction(EOFActionError), nil
	case optionIndicator{functor: "eof_action", arg: "eof_code"}:
		return WithEOFAction(EOFActionEOFCode), nil
	case optionIndicator{functor: "eof_action", arg: "reset"}:
		return WithEOFAction(EOFActionReset), nil
	default:
		return nil, domainErrorStreamOption(option)
	}
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
			return Error(ErrInstantiation)
		case *Compound:
			switch option.Functor {
			case "force":
				if len(option.Args) != 1 {
					return Error(domainErrorStreamOption(option))
				}

				switch v := env.Resolve(option.Args[0]).(type) {
				case Variable:
					return Error(ErrInstantiation)
				case Atom:
					switch v {
					case "false":
						force = false
					case "true":
						force = true
					default:
						return Error(domainErrorStreamOption(option))
					}
				default:
					return Error(domainErrorStreamOption(option))
				}
			}
		default:
			return Error(domainErrorStreamOption(option))
		}
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}

	if err := s.Close(); err != nil && !force {
		return Error(resourceError(streamOrAlias, Atom(err.Error())))
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
		return Error(permissionErrorOutputStream(streamOrAlias))
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
		return Error(permissionErrorOutputStream(streamOrAlias))
	}

	if s.streamType == StreamTypeBinary {
		return Error(permissionErrorOutputBinaryStream(streamOrAlias))
	}

	var opts []WriteOption
	iter := ListIterator{List: options, Env: env}
	for iter.Next() {
		opt, err := writeTermOption(state, iter.Current(), env)
		if err != nil {
			return Error(err)
		}
		opts = append(opts, opt)
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}

	if err := state.Write(s.file, env.Resolve(t), env, opts...); err != nil {
		return Error(err)
	}

	return k(env)
}

// Write outputs term to the writer.
func (state *State) Write(w io.Writer, t Term, env *Env, opts ...WriteOption) error {
	opts = append([]WriteOption{withOps(state.operators), WithPriority(1200)}, opts...)
	return Write(w, t, env, opts...)
}

func writeTermOption(state *State, option Term, env *Env) (WriteOption, error) {
	type optionIndicator struct {
		functor, arg Atom
	}

	var oi optionIndicator
	switch option := env.Resolve(option).(type) {
	case Variable:
		return nil, ErrInstantiation
	case *Compound:
		if len(option.Args) != 1 {
			return nil, domainErrorWriteOption(option)
		}

		switch v := env.Resolve(option.Args[0]).(type) {
		case Variable:
			return nil, ErrInstantiation
		case Atom:
			oi = optionIndicator{functor: option.Functor, arg: v}
		default:
			return nil, domainErrorWriteOption(option)
		}
	default:
		return nil, domainErrorWriteOption(option)
	}

	switch oi {
	case optionIndicator{functor: "quoted", arg: "true"}:
		return WithQuoted(true), nil
	case optionIndicator{functor: "quoted", arg: "false"}:
		return WithQuoted(false), nil
	case optionIndicator{functor: "ignore_ops", arg: "true"}:
		return state.WithIgnoreOps(true), nil
	case optionIndicator{functor: "ignore_ops", arg: "false"}:
		return state.WithIgnoreOps(false), nil
	case optionIndicator{functor: "numbervars", arg: "true"}:
		return WithNumberVars(true), nil
	case optionIndicator{functor: "numbervars", arg: "false"}:
		return WithNumberVars(false), nil
	default:
		return nil, domainErrorWriteOption(option)
	}
}

// CharCode converts a single-rune Atom char to an Integer code, or vice versa.
func CharCode(char, code Term, k func(*Env) *Promise, env *Env) *Promise {
	switch ch := env.Resolve(char).(type) {
	case Variable:
		switch cd := env.Resolve(code).(type) {
		case Variable:
			return Error(ErrInstantiation)
		case Integer:
			r := rune(cd)

			if !utf8.ValidRune(r) {
				return Error(representationError("character_code"))
			}

			return Delay(func(context.Context) *Promise {
				return Unify(ch, Atom(r), k, env)
			})
		default:
			return Error(TypeErrorInteger(code))
		}
	case Atom:
		switch code := env.Resolve(code).(type) {
		case Variable, Integer:
			break
		default:
			return Error(TypeErrorInteger(code))
		}

		rs := []rune(ch)
		if len(rs) != 1 {
			return Error(TypeErrorCharacter(ch))
		}

		return Delay(func(context.Context) *Promise {
			return Unify(code, Integer(rs[0]), k, env)
		})
	default:
		return Error(TypeErrorCharacter(ch))
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
		return Error(permissionErrorOutputStream(streamOrAlias))
	}

	if s.streamType == StreamTypeText {
		return Error(permissionErrorOutputTextStream(streamOrAlias))
	}

	switch b := env.Resolve(byt).(type) {
	case Variable:
		return Error(ErrInstantiation)
	case Integer:
		if 0 > b || 255 < b {
			return Error(TypeErrorByte(byt))
		}

		if _, err := write(s.file, []byte{byte(b)}); err != nil {
			return Error(SystemError(err))
		}

		return k(env)
	default:
		return Error(TypeErrorByte(byt))
	}
}

// PutCode outputs code to the stream represented by streamOrAlias.
func (state *State) PutCode(streamOrAlias, code Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := state.stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	if s.mode != StreamModeWrite && s.mode != StreamModeAppend {
		return Error(permissionErrorOutputStream(streamOrAlias))
	}

	if s.streamType == StreamTypeBinary {
		return Error(permissionErrorOutputBinaryStream(streamOrAlias))
	}

	switch c := env.Resolve(code).(type) {
	case Variable:
		return Error(ErrInstantiation)
	case Integer:
		r := rune(c)

		if !utf8.ValidRune(r) {
			return Error(representationError("character_code"))
		}

		if _, err := write(s.file, []byte(string(r))); err != nil {
			return Error(SystemError(err))
		}

		return k(env)
	default:
		return Error(TypeErrorInteger(code))
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
		return Error(permissionErrorInputStream(streamOrAlias))
	}

	if s.streamType == StreamTypeBinary {
		return Error(permissionErrorInputBinaryStream(streamOrAlias))
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
		var unexpectedToken *unexpectedTokenError
		switch {
		case errors.Is(err, io.EOF):
			return [...]*Promise{
				EOFActionError: Error(permissionErrorInputPastEndOfStream(streamOrAlias)),
				EOFActionEOFCode: Delay(func(context.Context) *Promise {
					return Unify(out, Atom("end_of_file"), k, env)
				}),
				EOFActionReset: Delay(func(context.Context) *Promise {
					return state.ReadTerm(streamOrAlias, out, options, k, env)
				}),
			}[s.eofAction]
		case errors.Is(err, ErrInsufficient):
			return Error(syntaxErrorInsufficient())
		case errors.As(err, &unexpectedToken):
			return Error(syntaxErrorUnexpectedToken(Atom(err.Error())))
		default:
			return Error(SystemError(err))
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

	return Delay(func(context.Context) *Promise {
		return Unify(out, t, k, env)
	})
}

func readTermOption(opts *readTermOptions, option Term, env *Env) error {
	switch option := env.Resolve(option).(type) {
	case Variable:
		return ErrInstantiation
	case *Compound:
		if len(option.Args) != 1 {
			return domainErrorReadOption(option)
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
			return domainErrorReadOption(option)
		}
		return nil
	default:
		return domainErrorReadOption(option)
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
		return Error(permissionErrorInputStream(streamOrAlias))
	}

	if s.streamType == StreamTypeText {
		return Error(permissionErrorInputTextStream(streamOrAlias))
	}

	switch b := env.Resolve(inByte).(type) {
	case Variable:
		break
	case Integer:
		if b < 0 || b > 255 {
			Error(TypeErrorInByte(inByte))
		}
	default:
		return Error(TypeErrorInByte(inByte))
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
			return Error(permissionErrorInputPastEndOfStream(streamOrAlias))
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
		return Error(permissionErrorInputStream(streamOrAlias))
	}

	if s.streamType == StreamTypeBinary {
		return Error(permissionErrorInputBinaryStream(streamOrAlias))
	}

	switch c := env.Resolve(char).(type) {
	case Variable:
		break
	case Atom:
		if len([]rune(c)) != 1 {
			return Error(TypeErrorInCharacter(char))
		}
	default:
		return Error(TypeErrorInCharacter(char))
	}

	r, _, err := readRune(s.buf)
	switch err {
	case nil:
		if r == unicode.ReplacementChar {
			return Error(representationError("character"))
		}

		return Delay(func(context.Context) *Promise {
			return Unify(char, Atom(r), k, env)
		})
	case io.EOF:
		switch s.eofAction {
		case EOFActionError:
			return Error(permissionErrorInputPastEndOfStream(streamOrAlias))
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
		return Error(permissionErrorInputStream(streamOrAlias))
	}

	if s.streamType == StreamTypeText {
		return Error(permissionErrorInputTextStream(streamOrAlias))
	}

	switch b := env.Resolve(inByte).(type) {
	case Variable:
		break
	case Integer:
		if b < 0 || b > 255 {
			return Error(TypeErrorInByte(inByte))
		}
	default:
		return Error(TypeErrorInByte(inByte))
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
			return Error(permissionErrorInputPastEndOfStream(streamOrAlias))
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
		return Error(permissionErrorInputStream(streamOrAlias))
	}

	if s.streamType == StreamTypeBinary {
		return Error(permissionErrorInputBinaryStream(streamOrAlias))
	}

	switch c := env.Resolve(char).(type) {
	case Variable:
		break
	case Atom:
		if len([]rune(c)) != 1 {
			return Error(TypeErrorInCharacter(char))
		}
	default:
		return Error(TypeErrorInCharacter(char))
	}

	r, _, err := readRune(s.buf)
	switch err {
	case nil:
		if err := unreadRune(s.buf); err != nil {
			return Error(SystemError(err))
		}

		if r == unicode.ReplacementChar {
			return Error(representationError("character"))
		}

		return Delay(func(context.Context) *Promise {
			return Unify(char, Atom(r), k, env)
		})
	case io.EOF:
		switch s.eofAction {
		case EOFActionError:
			return Error(permissionErrorInputPastEndOfStream(streamOrAlias))
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
		return Error(ErrInstantiation)
	case Integer:
		osExit(int(code))
		return k(env)
	default:
		return Error(TypeErrorInteger(n))
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
		return Error(TypeErrorCallable(body))
	}

	p, ok := state.procedures[pi]
	if !ok {
		return Bool(false)
	}

	cs, ok := p.(clauses)
	if !ok {
		return Error(permissionErrorAccessPrivateProcedure(pi.Term()))
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
		return Error(ErrInstantiation)
	case Atom:
		switch l := env.Resolve(length).(type) {
		case Variable:
			break
		case Integer:
			if l < 0 {
				return Error(domainErrorNotLessThanZero(length))
			}
		default:
			return Error(TypeErrorInteger(length))
		}

		return Delay(func(context.Context) *Promise {
			return Unify(length, Integer(len([]rune(a))), k, env)
		})
	default:
		return Error(TypeErrorAtom(atom))
	}
}

// AtomConcat concatenates atom1 and atom2 and unifies it with atom3.
func AtomConcat(atom1, atom2, atom3 Term, k func(*Env) *Promise, env *Env) *Promise {
	switch a3 := env.Resolve(atom3).(type) {
	case Variable:
		switch a1 := env.Resolve(atom1).(type) {
		case Variable:
			return Error(ErrInstantiation)
		case Atom:
			switch a2 := env.Resolve(atom2).(type) {
			case Variable:
				return Error(ErrInstantiation)
			case Atom:
				return Delay(func(context.Context) *Promise {
					return Unify(a1+a2, a3, k, env)
				})
			default:
				return Error(TypeErrorAtom(atom2))
			}
		default:
			return Error(TypeErrorAtom(atom1))
		}
	case Atom:
		switch env.Resolve(atom1).(type) {
		case Variable, Atom:
			break
		default:
			return Error(TypeErrorAtom(atom1))
		}

		switch env.Resolve(atom2).(type) {
		case Variable, Atom:
			break
		default:
			return Error(TypeErrorAtom(atom2))
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
		return Error(TypeErrorAtom(atom3))
	}
}

// SubAtom unifies subAtom with a sub atom of atom of length which appears with before runes preceding it and after runes following it.
func SubAtom(atom, before, length, after, subAtom Term, k func(*Env) *Promise, env *Env) *Promise {
	switch whole := env.Resolve(atom).(type) {
	case Variable:
		return Error(ErrInstantiation)
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
			return Error(TypeErrorAtom(subAtom))
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
		return Error(TypeErrorAtom(atom))
	}
}

func checkPositiveInteger(n Term, env *Env) error {
	switch b := env.Resolve(n).(type) {
	case Variable:
		return nil
	case Integer:
		if b < 0 {
			return domainErrorNotLessThanZero(n)
		}
		return nil
	default:
		return TypeErrorInteger(n)
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
				return Error(ErrInstantiation)
			case Atom:
				if len([]rune(e)) != 1 {
					return Error(TypeErrorCharacter(e))
				}
				_, _ = sb.WriteString(string(e))
			default:
				return Error(TypeErrorCharacter(e))
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
		return Error(TypeErrorAtom(a))
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
				return Error(ErrInstantiation)
			case Integer:
				_, _ = sb.WriteRune(rune(e))
			default:
				return Error(representationError("character_code"))
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
		return Error(TypeErrorAtom(atom))
	}
}

// NumberChars breaks up an atom representation of a number num into a list of characters and unifies it with chars, or
// constructs a number from a list of characters chars and unifies it with num.
func NumberChars(num, chars Term, k func(*Env) *Promise, env *Env) *Promise {
	switch chars := env.Resolve(chars).(type) {
	case Variable:
		break
	default:
		switch n := env.Resolve(num).(type) {
		case Variable, Integer, Float:
			break
		default:
			return Error(TypeErrorNumber(n))
		}

		var sb strings.Builder
		iter := ListIterator{List: chars, Env: env}
		for iter.Next() {
			switch e := env.Resolve(iter.Current()).(type) {
			case Variable:
				return Error(ErrInstantiation)
			case Atom:
				if len([]rune(e)) != 1 {
					return Error(TypeErrorCharacter(e))
				}
				_, _ = sb.WriteString(string(e))
			default:
				return Error(TypeErrorCharacter(e))
			}
		}
		if err := iter.Err(); err != nil {
			return Error(err)
		}

		p := newParser(bufio.NewReader(strings.NewReader(sb.String())), nil)
		t, err := p.Number()
		switch err {
		case nil:
			break
		case errNotANumber:
			return Error(syntaxErrorNotANumber())
		default:
			return Error(SystemError(err))
		}
		return Delay(func(context.Context) *Promise {
			return Unify(num, t, k, env)
		})
	}

	switch n := env.Resolve(num).(type) {
	case Variable:
		return Error(ErrInstantiation)
	case Integer, Float:
		var buf bytes.Buffer
		if err := Write(&buf, n, nil); err != nil {
			return Error(err)
		}
		rs := []rune(buf.String())
		cs := make([]Term, len(rs))
		for i, r := range rs {
			cs[i] = Atom(r)
		}
		return Delay(func(context.Context) *Promise {
			return Unify(chars, List(cs...), k, env)
		})
	default:
		return Error(TypeErrorNumber(num))
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
			return Error(TypeErrorNumber(n))
		}

		var sb strings.Builder
		iter := ListIterator{List: codes, Env: env}
		for iter.Next() {
			switch e := env.Resolve(iter.Current()).(type) {
			case Variable:
				return Error(ErrInstantiation)
			case Integer:
				_, _ = sb.WriteRune(rune(e))
			default:
				return Error(representationError("character_code"))
			}
		}
		if err := iter.Err(); err != nil {
			return Error(err)
		}

		p := newParser(bufio.NewReader(strings.NewReader(sb.String())), nil)
		t, err := p.Number()
		switch err {
		case nil:
			break
		case errNotANumber:
			return Error(syntaxErrorNotANumber())
		default:
			return Error(SystemError(err))
		}

		return Delay(func(context.Context) *Promise {
			return Unify(num, t, k, env)
		})
	}

	switch n := env.Resolve(num).(type) {
	case Variable:
		return Error(ErrInstantiation)
	case Integer, Float:
		var buf bytes.Buffer
		if err := Write(&buf, n, nil); err != nil {
			return Error(err)
		}
		rs := []rune(buf.String())
		cs := make([]Term, len(rs))
		for i, r := range rs {
			cs[i] = Integer(r)
		}
		return Delay(func(context.Context) *Promise {
			return Unify(codes, List(cs...), k, env)
		})
	default:
		return Error(TypeErrorNumber(num))
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
			return Error(existenceErrorStream(streamOrAlias))
		}
		streams = append(streams, v)
	case *Stream:
		streams = append(streams, s)
	default:
		return Error(domainErrorStreamOrAlias(streamOrAlias))
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
			return domainErrorStreamProperty(property)
		}
	case *Compound:
		if len(p.Args) != 1 {
			return domainErrorStreamProperty(property)
		}
		arg := p.Args[0]
		switch p.Functor {
		case "file_name", "mode", "alias", "end_of_stream", "eof_action", "reposition":
			return checkAtom(arg, env)
		case "position":
			return checkInteger(arg, env)
		default:
			return domainErrorStreamProperty(property)
		}
	default:
		return domainErrorStreamProperty(property)
	}
}

func checkAtom(t Term, env *Env) error {
	switch env.Resolve(t).(type) {
	case Variable, Atom:
		return nil
	default:
		return TypeErrorAtom(t)
	}
}

func checkInteger(t Term, env *Env) error {
	switch env.Resolve(t).(type) {
	case Variable, Integer:
		return nil
	default:
		return TypeErrorAtom(t)
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
		return Error(PermissionError("reposition", "stream", streamOrAlias))
	}

	switch p := env.Resolve(position).(type) {
	case Variable:
		return Error(ErrInstantiation)
	case Integer:
		if f, ok := s.file.(io.Seeker); ok {
			if _, err := seek(f, int64(p), 0); err != nil {
				return Error(SystemError(err))
			}

			s.buf.Reset(s.file)
		}

		return k(env)
	default:
		return Error(TypeErrorInteger(position))
	}
}

// CharConversion registers a character conversion from inChar to outChar, or remove the conversion if inChar = outChar.
func (state *State) CharConversion(inChar, outChar Term, k func(*Env) *Promise, env *Env) *Promise {
	switch in := env.Resolve(inChar).(type) {
	case Variable:
		return Error(ErrInstantiation)
	case Atom:
		i := []rune(in)
		if len(i) != 1 {
			return Error(representationError("character"))
		}

		switch out := env.Resolve(outChar).(type) {
		case Variable:
			return Error(ErrInstantiation)
		case Atom:
			o := []rune(out)
			if len(o) != 1 {
				return Error(representationError("character"))
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
			return Error(representationError("character"))
		}
	default:
		return Error(representationError("character"))
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
			return Error(representationError("character"))
		}
	default:
		return Error(representationError("character"))
	}

	switch out := env.Resolve(outChar).(type) {
	case Variable:
		break
	case Atom:
		o := []rune(out)
		if len(o) != 1 {
			return Error(representationError("character"))
		}
	default:
		return Error(representationError("character"))
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
		return Error(ErrInstantiation)
	case Atom:
		var modify func(value Atom) error
		switch f {
		case "bounded", "max_integer", "min_integer", "integer_rounding_function", "max_arity":
			return Error(PermissionError("modify", "flag", f))
		case "char_conversion":
			modify = state.modifyCharConversion
		case "debug":
			modify = state.modifyDebug
		case "unknown":
			modify = state.modifyUnknown
		case "double_quotes":
			modify = state.modifyDoubleQuotes
		default:
			return Error(domainErrorPrologFlag(f))
		}

		switch v := env.Resolve(value).(type) {
		case Variable:
			return Error(ErrInstantiation)
		case Atom:
			if err := modify(v); err != nil {
				return Error(err)
			}
			return k(env)
		default:
			return Error(domainErrorFlagValue(&Compound{
				Functor: "+",
				Args:    []Term{flag, value},
			}))
		}
	default:
		return Error(TypeErrorAtom(f))
	}
}

func (state *State) modifyCharConversion(value Atom) error {
	switch value {
	case "on":
		state.charConvEnabled = true
	case "off":
		state.charConvEnabled = false
	default:
		return domainErrorFlagValue(&Compound{
			Functor: "+",
			Args:    []Term{Atom("char_conversion"), value},
		})
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
		return domainErrorFlagValue(&Compound{
			Functor: "+",
			Args:    []Term{Atom("debug"), value},
		})
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
		return domainErrorFlagValue(&Compound{
			Functor: "+",
			Args:    []Term{Atom("unknown"), value},
		})
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
		return domainErrorFlagValue(&Compound{
			Functor: "+",
			Args:    []Term{Atom("double_quotes"), value},
		})
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
			return Error(domainErrorPrologFlag(f))
		}
	default:
		return Error(TypeErrorAtom(f))
	}

	pattern := Compound{Args: []Term{flag, value}}
	flags := []Term{
		&Compound{Args: []Term{Atom("bounded"), Atom("true")}},
		&Compound{Args: []Term{Atom("max_integer"), Integer(math.MaxInt64)}},
		&Compound{Args: []Term{Atom("min_integer"), Integer(math.MinInt64)}},
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
		return nil, ErrInstantiation
	case Atom:
		v, ok := state.streams[s]
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

// Dynamic declares a procedure indicated by pi is user-defined dynamic.
func (state *State) Dynamic(pi Term, k func(*Env) *Promise, env *Env) *Promise {
	if err := Each(pi, func(elem Term) error {
		key, err := NewProcedureIndicator(elem, env)
		if err != nil {
			return err
		}
		if state.procedures == nil {
			state.procedures = map[ProcedureIndicator]procedure{}
		}
		p, ok := state.procedures[key]
		if !ok {
			state.procedures[key] = clauses{}
			return nil
		}
		if _, ok := p.(clauses); !ok {
			return permissionErrorModifyStaticProcedure(elem)
		}
		return nil
	}, env); err != nil {
		return Error(err)
	}
	return k(env)
}

// BuiltIn declares a procedure indicated by pi is built-in and static.
func (state *State) BuiltIn(pi Term, k func(*Env) *Promise, env *Env) *Promise {
	if err := Each(pi, func(elem Term) error {
		key, err := NewProcedureIndicator(elem, env)
		if err != nil {
			return err
		}
		if state.procedures == nil {
			state.procedures = map[ProcedureIndicator]procedure{}
		}
		p, ok := state.procedures[key]
		if !ok {
			state.procedures[key] = builtin{}
			return nil
		}
		if _, ok := p.(builtin); !ok {
			return permissionErrorModifyStaticProcedure(elem)
		}
		return nil
	}, env); err != nil {
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
