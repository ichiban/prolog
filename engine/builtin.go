package engine

import (
	"bufio"
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
	operators       Operators
	charConversions map[rune]rune
	charConvEnabled bool
	doubleQuotes    DoubleQuotes

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
	return NewParser(br, state.charConversions,
		WithOperators(&state.operators),
		WithDoubleQuotes(state.doubleQuotes),
		WithParsedVars(vars),
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
		return Error(InstantiationError(goal))
	default:
		fvs := env.FreeVariables(g)
		args := make([]Term, len(fvs))
		for i, fv := range fvs {
			args[i] = fv
		}
		const call = Atom("$call")
		cs, err := compile(&Compound{
			Functor: ":-",
			Args: []Term{
				call.Apply(args...),
				g,
			},
		}, env)
		if err != nil {
			return Error(err)
		}

		return cs.Call(&state.VM, args, k, env)
	}
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

// Functor extracts the name and arity of term, or unifies term with an atomic/compound term of name and arity with
// fresh variables as arguments.
func Functor(t, name, arity Term, k func(*Env) *Promise, env *Env) *Promise {
	switch t := env.Resolve(t).(type) {
	case Variable:
		switch arity := env.Resolve(arity).(type) {
		case Variable:
			return Error(InstantiationError(arity))
		case Integer:
			switch {
			case arity < 0:
				return Error(domainErrorNotLessThanZero(arity))
			case arity == 0:
				return Unify(t, name, k, env)
			}

			switch name := env.Resolve(name).(type) {
			case Variable:
				return Error(InstantiationError(name))
			case *Compound:
				return Error(typeErrorAtomic(name))
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
				return Error(typeErrorAtom(name))
			}
		default:
			return Error(typeErrorInteger(arity))
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
		return Error(InstantiationError(t))
	case *Compound:
		switch n := env.Resolve(nth).(type) {
		case Variable:
			return Error(InstantiationError(nth))
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
			return Error(typeErrorInteger(n))
		}
	default:
		return Error(typeErrorCompound(t))
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
			return Error(typeErrorList(list))
		}

		f, ok := env.Resolve(cons.Args[0]).(Atom)
		if !ok {
			return Error(typeErrorAtom(cons.Args[0]))
		}

		var args []Term
		if err := EachList(cons.Args[1], func(elem Term) error {
			args = append(args, elem)
			return nil
		}, env); err != nil {
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

// Op defines operator with priority and specifier, or removes when priority is 0.
func (state *State) Op(priority, specifier, operator Term, k func(*Env) *Promise, env *Env) *Promise {
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
	var spec OperatorSpecifier
	switch s {
	case "fx":
		spec = OperatorSpecifierFX
	case "fy":
		spec = OperatorSpecifierFY
	case "xf":
		spec = OperatorSpecifierXF
	case "yf":
		spec = OperatorSpecifierYF
	case "xfx":
		spec = OperatorSpecifierXFX
	case "xfy":
		spec = OperatorSpecifierXFY
	case "yfx":
		spec = OperatorSpecifierYFX
	default:
		return Error(domainErrorOperatorSpecifier(s))
	}

	o, ok := env.Resolve(operator).(Atom)
	if !ok {
		return Error(typeErrorAtom(operator))
	}

	// already defined?
	for i, op := range state.operators {
		if op.Specifier != spec || op.Name != o {
			continue
		}

		// remove it first so that we can insert it again in the right position
		copy(state.operators[i:], state.operators[i+1:])
		state.operators[len(state.operators)-1] = Operator{}
		state.operators = state.operators[:len(state.operators)-1]

		// or keep it removed.
		if p == 0 {
			return k(env)
		}
	}

	// insert
	i := sort.Search(len(state.operators), func(i int) bool {
		return state.operators[i].Priority >= p
	})
	state.operators = append(state.operators, Operator{})
	copy(state.operators[i+1:], state.operators[i:])
	state.operators[i] = Operator{
		Priority:  p,
		Specifier: spec,
		Name:      o,
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
	ks := make([]func(context.Context) *Promise, len(state.operators))
	for i := range state.operators {
		op := state.operators[i]
		ks[i] = func(context.Context) *Promise {
			return Unify(&pattern, &Compound{Args: []Term{op.Priority, op.Specifier.Term(), op.Name}}, k, env)
		}
	}
	return Delay(ks...)
}

// Assertz appends t to the database.
func (state *State) Assertz(t Term, k func(*Env) *Promise, env *Env) *Promise {
	return state.assert(t, false, func(existing clauses, new clauses) clauses {
		return append(existing, new...)
	}, k, env)
}

// Asserta prepends t to the database.
func (state *State) Asserta(t Term, k func(*Env) *Promise, env *Env) *Promise {
	return state.assert(t, false, func(existing clauses, new clauses) clauses {
		return append(new, existing...)
	}, k, env)
}

// AssertStatic prepends t to the database.
func (state *State) AssertStatic(t Term, k func(*Env) *Promise, env *Env) *Promise {
	return state.assert(t, true, func(existing clauses, new clauses) clauses {
		return append(existing, new...)
	}, k, env)
}

func (state *State) assert(t Term, force bool, merge func(clauses, clauses) clauses, k func(*Env) *Promise, env *Env) *Promise {
	pi, args, err := piArgs(t, env)
	if err != nil {
		return Error(err)
	}

	switch pi {
	case ProcedureIndicator{Name: ":-", Arity: 1}: // directive
		name, args, err := piArgs(args[0], env)
		if err != nil {
			return Error(err)
		}
		return Delay(func(context.Context) *Promise {
			return state.Arrive(name, args, k, env)
		})
	case ProcedureIndicator{Name: ":-", Arity: 2}:
		pi, _, err = piArgs(args[0], env)
		if err != nil {
			return Error(err)
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

	added, err := compile(t, env)
	if err != nil {
		return Error(err)
	}

	switch existing := p.(type) {
	case clauses:
		state.procedures[pi] = merge(existing, added)
		return k(env)
	case builtin:
		if !force {
			return Error(permissionErrorModifyStaticProcedure(pi.Term()))
		}
		state.procedures[pi] = builtin{merge(existing.clauses, added)}
		return k(env)
	case static:
		if !force {
			return Error(permissionErrorModifyStaticProcedure(pi.Term()))
		}
		state.procedures[pi] = static{merge(existing.clauses, added)}
		return k(env)
	default:
		return Error(permissionErrorModifyStaticProcedure(pi.Term()))
	}
}

// BagOf collects all the solutions of goal as instances, which unify with template. instances may contain duplications.
func (state *State) BagOf(template, goal, instances Term, k func(*Env) *Promise, env *Env) *Promise {
	return state.collectionOf(List, template, goal, instances, k, env)
}

// SetOf collects all the solutions of goal as instances, which unify with template. instances don't contain duplications.
func (state *State) SetOf(template, goal, instances Term, k func(*Env) *Promise, env *Env) *Promise {
	return state.collectionOf(Set, template, goal, instances, k, env)
}

func (state *State) collectionOf(agg func(...Term) Term, template, goal, instances Term, k func(*Env) *Promise, env *Env) *Promise {
	var qualifier, body Term
	switch goal := env.Resolve(goal).(type) {
	case Variable:
		return Error(InstantiationError(goal))
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
			if err := EachList(answers, func(elem Term) error {
				answer := elem.(*Compound)
				vars, instance := answer.Args[0], answer.Args[1]
				for i := range solutions {
					if compare(solutions[i].vars, vars, env) == 0 {
						solutions[i].instances = append(solutions[i].instances, instance)
						return nil
					}
				}
				solutions = append(solutions, solution{vars: vars, instances: []Term{instance}})
				return nil
			}, env); err != nil {
				return Error(err)
			}
			return Bool(true)
		}, env).Force(ctx); err != nil {
			return Error(err)
		}

		sort.Slice(solutions, func(i, j int) bool {
			return compare(solutions[i].vars, solutions[j].vars, env) < 0
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
		return Error(typeErrorAtom(order))
	}

	d := compare(env.Resolve(term1), env.Resolve(term2), env)
	switch {
	case d < 0:
		return Unify(Atom("<"), order, k, env)
	case d > 0:
		return Unify(Atom(">"), order, k, env)
	default: // d == 0:
		return Unify(Atom("="), order, k, env)
	}
}

// Throw throws ball as an exception.
func Throw(ball Term, _ func(*Env) *Promise, env *Env) *Promise {
	if _, ok := env.Resolve(ball).(Variable); ok {
		return Error(InstantiationError(ball))
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
			return Error(typeErrorPredicateIndicator(pi))
		}
		if _, ok := env.Resolve(pi.Args[0]).(Atom); !ok {
			return Error(typeErrorPredicateIndicator(pi))
		}
		if _, ok := env.Resolve(pi.Args[1]).(Integer); !ok {
			return Error(typeErrorPredicateIndicator(pi))
		}
	default:
		return Error(typeErrorPredicateIndicator(pi))
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
		return Error(InstantiationError(pi))
	case *Compound:
		if pi.Functor != "/" || len(pi.Args) != 2 {
			return Error(typeErrorPredicateIndicator(pi))
		}

		name, arity := pi.Args[0], pi.Args[1]

		switch name := env.Resolve(name).(type) {
		case Variable:
			return Error(InstantiationError(name))
		case Atom:
			switch arity := env.Resolve(arity).(type) {
			case Variable:
				return Error(InstantiationError(arity))
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
				return Error(typeErrorInteger(arity))
			}
		default:
			return Error(typeErrorAtom(name))
		}
	default:
		return Error(typeErrorPredicateIndicator(pi))
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
		return Error(InstantiationError(SourceSink))
	case Atom:
		n = s
	default:
		return Error(domainErrorSourceSink(SourceSink))
	}

	var streamMode StreamMode
	switch m := env.Resolve(mode).(type) {
	case Variable:
		return Error(InstantiationError(mode))
	case Atom:
		switch m {
		case "read":
			streamMode = StreamModeRead
		case "write":
			streamMode = StreamModeWrite
		case "append":
			streamMode = StreamModeAppend
		default:
			return Error(domainErrorIOMode(m))
		}
	default:
		return Error(typeErrorAtom(mode))
	}

	if _, ok := env.Resolve(stream).(Variable); !ok {
		return Error(typeErrorVariable(stream))
	}

	var opts []StreamOption
	if err := EachList(env.Resolve(options), func(option Term) error {
		switch o := env.Resolve(option).(type) {
		case Variable:
			return InstantiationError(option)
		case *Compound:
			if len(o.Args) != 1 {
				return domainErrorStreamOption(option)
			}
			arg := o.Args[0]
			switch o.Functor {
			case "type":
				switch t := env.Resolve(arg).(type) {
				case Variable:
					return InstantiationError(arg)
				case Atom:
					switch t {
					case "text":
						opts = append(opts, WithStreamType(StreamTypeText))
						return nil
					case "binary":
						opts = append(opts, WithStreamType(StreamTypeBinary))
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
					return InstantiationError(arg)
				case Atom:
					switch b {
					case "true":
						opts = append(opts, WithReposition(true))
						return nil
					case "false":
						opts = append(opts, WithReposition(false))
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
					return InstantiationError(arg)
				case Atom:
					if _, ok := state.streams[a]; ok {
						return PermissionError("open", "source_sink", option, "%s is already defined as an alias.", a)
					}
					opts = append(opts, WithAlias(state, a))
					return nil
				default:
					return domainErrorStreamOption(option)
				}
			case "eof_action":
				switch a := env.Resolve(arg).(type) {
				case Variable:
					return InstantiationError(arg)
				case Atom:
					switch a {
					case "error":
						opts = append(opts, WithEOFAction(EOFActionError))
						return nil
					case "eof_code":
						opts = append(opts, WithEOFAction(EOFActionEOFCode))
						return nil
					case "reset":
						opts = append(opts, WithEOFAction(EOFActionReset))
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

	s, err := Open(n, streamMode, opts...)
	if err != nil {
		return Error(err)
	}

	return Delay(func(context.Context) *Promise {
		return Unify(stream, s, k, env)
	})
}

// Close closes a stream specified by streamOrAlias.
func (state *State) Close(streamOrAlias, options Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := state.stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	var force bool
	if err := EachList(env.Resolve(options), func(option Term) error {
		switch option := env.Resolve(option).(type) {
		case Variable:
			return InstantiationError(option)
		case *Compound:
			switch option.Functor {
			case "force":
				if len(option.Args) != 1 {
					return domainErrorStreamOption(option)
				}

				switch v := env.Resolve(option.Args[0]).(type) {
				case Variable:
					return InstantiationError(option.Args[0])
				case Atom:
					switch v {
					case "false":
						force = false
					case "true":
						force = true
					default:
						return domainErrorStreamOption(option)
					}
				default:
					return domainErrorStreamOption(option)
				}
			}
			return nil
		default:
			return domainErrorStreamOption(option)
		}
	}, env); err != nil {
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

	opts := WriteTermOptions{
		Ops:      state.operators,
		Priority: 1200,
	}
	if err := EachList(env.Resolve(options), func(option Term) error {
		switch option := env.Resolve(option).(type) {
		case Variable:
			return InstantiationError(option)
		case *Compound:
			if len(option.Args) != 1 {
				return domainErrorWriteOption(option)
			}

			var b bool
			switch v := env.Resolve(option.Args[0]).(type) {
			case Variable:
				return InstantiationError(v)
			case Atom:
				switch v {
				case "false":
					b = false
				case "true":
					b = true
				default:
					return domainErrorWriteOption(option)
				}
			default:
				return domainErrorWriteOption(option)
			}

			switch option.Functor {
			case "quoted":
				opts.Quoted = b
			case "ignore_ops":
				if b {
					opts.Ops = nil
				} else {
					opts.Ops = state.operators
				}
			case "numbervars":
				opts.NumberVars = b
			default:
				return domainErrorWriteOption(option)
			}
			return nil
		default:
			return domainErrorWriteOption(option)
		}
	}, env); err != nil {
		return Error(err)
	}

	if err := Write(s.file, env.Resolve(t), opts, env); err != nil {
		return Error(err)
	}

	return k(env)
}

// CharCode converts a single-rune Atom char to an Integer code, or vice versa.
func CharCode(char, code Term, k func(*Env) *Promise, env *Env) *Promise {
	switch ch := env.Resolve(char).(type) {
	case Variable:
		switch cd := env.Resolve(code).(type) {
		case Variable:
			return Error(InstantiationError(&Compound{
				Functor: ",",
				Args:    []Term{char, code},
			}))
		case Integer:
			r := rune(cd)

			if !utf8.ValidRune(r) {
				return Error(representationError(Atom("character_code"), Atom(fmt.Sprintf("%d is not a valid unicode code point.", r))))
			}

			return Delay(func(context.Context) *Promise {
				return Unify(ch, Atom(r), k, env)
			})
		default:
			return Error(typeErrorInteger(code))
		}
	case Atom:
		switch code := env.Resolve(code).(type) {
		case Variable, Integer:
			break
		default:
			return Error(typeErrorInteger(code))
		}

		rs := []rune(ch)
		if len(rs) != 1 {
			return Error(typeErrorCharacter(ch))
		}

		return Delay(func(context.Context) *Promise {
			return Unify(code, Integer(rs[0]), k, env)
		})
	default:
		return Error(typeErrorCharacter(ch))
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
		return Error(InstantiationError(byt))
	case Integer:
		if 0 > b || 255 < b {
			return Error(typeErrorByte(byt))
		}

		if _, err := write(s.file, []byte{byte(b)}); err != nil {
			return Error(SystemError(err))
		}

		return k(env)
	default:
		return Error(typeErrorByte(byt))
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
		return Error(InstantiationError(code))
	case Integer:
		r := rune(c)

		if !utf8.ValidRune(r) {
			return Error(representationError(Atom("character_code"), Atom(fmt.Sprintf("%s is not a valid unicode code point.", c))))
		}

		if _, err := write(s.file, []byte(string(r))); err != nil {
			return Error(SystemError(err))
		}

		return k(env)
	default:
		return Error(typeErrorInteger(code))
	}
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

	var opts struct {
		singletons    Term
		variables     Term
		variableNames Term
	}
	if err := EachList(env.Resolve(options), func(option Term) error {
		switch option := env.Resolve(option).(type) {
		case Variable:
			return InstantiationError(option)
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
	}, env); err != nil {
		return Error(err)
	}

	var vars []ParsedVariable
	p := state.Parser(s.buf, &vars)

	t, err := p.Term()
	if err != nil {
		var (
			unexpectedRune  *UnexpectedRuneError
			unexpectedToken *unexpectedTokenError
		)
		switch {
		case errors.Is(err, io.EOF):
			switch s.eofAction {
			case EOFActionError:
				return Error(permissionErrorInputPastEndOfStream(streamOrAlias))
			case EOFActionEOFCode:
				return Delay(func(context.Context) *Promise {
					return Unify(out, Atom("end_of_file"), k, env)
				})
			case EOFActionReset:
				return Delay(func(context.Context) *Promise {
					return state.ReadTerm(streamOrAlias, out, options, k, env)
				})
			default:
				return Error(SystemError(fmt.Errorf("unknown EOF action: %d", s.eofAction)))
			}
		case errors.Is(err, ErrInsufficient):
			return Error(syntaxErrorInsufficient())
		case errors.As(err, &unexpectedRune):
			return Error(syntaxErrorUnexpectedChar(Atom(err.Error())))
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

	var ok bool
	if opts.singletons != nil {
		env, ok = opts.singletons.Unify(List(singletons...), false, env)
		if !ok {
			return Bool(false)
		}
	}
	if opts.variables != nil {
		env, ok = opts.variables.Unify(List(variables...), false, env)
		if !ok {
			return Bool(false)
		}
	}
	if opts.variableNames != nil {
		env, ok = opts.variableNames.Unify(List(variableNames...), false, env)
		if !ok {
			return Bool(false)
		}
	}

	return Delay(func(context.Context) *Promise {
		return Unify(out, t, k, env)
	})
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
			Error(typeErrorInByte(inByte))
		}
	default:
		return Error(typeErrorInByte(inByte))
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
			return Error(typeErrorInCharacter(char))
		}
	default:
		return Error(typeErrorInCharacter(char))
	}

	r, _, err := readRune(s.buf)
	switch err {
	case nil:
		if r == unicode.ReplacementChar {
			return Error(representationError(Atom("character"), Atom("invalid character.")))
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
			return Error(typeErrorInByte(inByte))
		}
	default:
		return Error(typeErrorInByte(inByte))
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
			return Error(typeErrorInCharacter(char))
		}
	default:
		return Error(typeErrorInCharacter(char))
	}

	r, _, err := readRune(s.buf)
	switch err {
	case nil:
		if err := unreadRune(s.buf); err != nil {
			return Error(SystemError(err))
		}

		if r == unicode.ReplacementChar {
			return Error(representationError(Atom("character"), Atom("invalid character.")))
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
		return Error(InstantiationError(n))
	case Integer:
		osExit(int(code))
		return k(env)
	default:
		return Error(typeErrorInteger(n))
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
		return Error(typeErrorCallable(body))
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
		return Error(InstantiationError(atom))
	case Atom:
		switch l := env.Resolve(length).(type) {
		case Variable:
			break
		case Integer:
			if l < 0 {
				return Error(domainErrorNotLessThanZero(length))
			}
		default:
			return Error(typeErrorInteger(length))
		}

		return Delay(func(context.Context) *Promise {
			return Unify(length, Integer(len([]rune(a))), k, env)
		})
	default:
		return Error(typeErrorAtom(atom))
	}
}

// AtomConcat concatenates atom1 and atom2 and unifies it with atom3.
func AtomConcat(atom1, atom2, atom3 Term, k func(*Env) *Promise, env *Env) *Promise {
	switch a3 := env.Resolve(atom3).(type) {
	case Variable:
		switch a1 := env.Resolve(atom1).(type) {
		case Variable:
			return Error(InstantiationError(&Compound{
				Functor: ",",
				Args:    []Term{atom1, atom3},
			}))
		case Atom:
			switch a2 := env.Resolve(atom2).(type) {
			case Variable:
				return Error(InstantiationError(&Compound{
					Functor: ",",
					Args:    []Term{atom2, atom3},
				}))
			case Atom:
				return Delay(func(context.Context) *Promise {
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
		return Error(typeErrorAtom(atom3))
	}
}

// SubAtom unifies subAtom with a sub atom of atom of length which appears with before runes preceding it and after runes following it.
func SubAtom(atom, before, length, after, subAtom Term, k func(*Env) *Promise, env *Env) *Promise {
	switch whole := env.Resolve(atom).(type) {
	case Variable:
		return Error(InstantiationError(atom))
	case Atom:
		rs := []rune(whole)

		switch b := env.Resolve(before).(type) {
		case Variable:
			break
		case Integer:
			if b < 0 {
				return Error(domainErrorNotLessThanZero(before))
			}
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
		default:
			return Error(typeErrorInteger(after))
		}

		switch env.Resolve(subAtom).(type) {
		case Variable, Atom:
			break
		default:
			return Error(typeErrorAtom(subAtom))
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
		return Error(typeErrorAtom(atom))
	}
}

// AtomChars breaks down atom into list of characters and unifies with chars, or constructs an atom from a list of
// characters chars and unifies it with atom.
func AtomChars(atom, chars Term, k func(*Env) *Promise, env *Env) *Promise {
	switch a := env.Resolve(atom).(type) {
	case Variable:
		var sb strings.Builder
		if err := EachList(env.Resolve(chars), func(elem Term) error {
			switch e := env.Resolve(elem).(type) {
			case Variable:
				return InstantiationError(elem)
			case Atom:
				if len([]rune(e)) != 1 {
					return typeErrorCharacter(e)
				}
				if _, err := sb.WriteString(string(e)); err != nil {
					return SystemError(err)
				}
				return nil
			default:
				return typeErrorCharacter(e)
			}
		}, env); err != nil {
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
		return Error(typeErrorAtom(a))
	}
}

// AtomCodes breaks up atom into a list of runes and unifies it with codes, or constructs an atom from the list of runes
// and unifies it with atom.
func AtomCodes(atom, codes Term, k func(*Env) *Promise, env *Env) *Promise {
	switch a := env.Resolve(atom).(type) {
	case Variable:
		var sb strings.Builder
		if err := EachList(env.Resolve(codes), func(elem Term) error {
			switch e := env.Resolve(elem).(type) {
			case Variable:
				return InstantiationError(elem)
			case Integer:
				if _, err := sb.WriteRune(rune(e)); err != nil {
					return SystemError(err)
				}
				return nil
			default:
				return representationError(Atom("character_code"), Atom("invalid character code."))
			}
		}, env); err != nil {
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
		return Error(typeErrorAtom(atom))
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
			return Error(typeErrorNumber(n))
		}

		var sb strings.Builder
		if err := EachList(env.Resolve(chars), func(elem Term) error {
			switch e := env.Resolve(elem).(type) {
			case Variable:
				return InstantiationError(elem)
			case Atom:
				if len([]rune(e)) != 1 {
					return typeErrorCharacter(elem)
				}
				if _, err := sb.WriteString(string(e)); err != nil {
					return SystemError(err)
				}
				return nil
			default:
				return typeErrorCharacter(elem)
			}
		}, env); err != nil {
			return Error(err)
		}

		p := NewParser(bufio.NewReader(strings.NewReader(sb.String())), nil)
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
		return Error(InstantiationError(num))
	case Integer, Float:
		rs := []rune(n.String())
		cs := make([]Term, len(rs))
		for i, r := range rs {
			cs[i] = Atom(r)
		}
		return Delay(func(context.Context) *Promise {
			return Unify(chars, List(cs...), k, env)
		})
	default:
		return Error(typeErrorNumber(num))
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
			return Error(typeErrorNumber(n))
		}

		var sb strings.Builder
		if err := EachList(env.Resolve(codes), func(elem Term) error {
			switch e := env.Resolve(elem).(type) {
			case Variable:
				return InstantiationError(elem)
			case Integer:
				if _, err := sb.WriteRune(rune(e)); err != nil {
					return SystemError(err)
				}
				return nil
			default:
				return representationError(Atom("character_code"), Atom(fmt.Sprintf("%s is not a valid character code.", elem)))
			}
		}, env); err != nil {
			return Error(err)
		}

		p := NewParser(bufio.NewReader(strings.NewReader(sb.String())), nil)
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
		return Error(InstantiationError(num))
	case Integer, Float:
		rs := []rune(n.String())
		cs := make([]Term, len(rs))
		for i, r := range rs {
			cs[i] = Integer(r)
		}
		return Delay(func(context.Context) *Promise {
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
func (fs FunctionSet) Is(result, expression Term, k func(*Env) *Promise, env *Env) *Promise {
	v, err := fs.eval(expression, env)
	if err != nil {
		return Error(err)
	}
	return Delay(func(context.Context) *Promise {
		return Unify(result, v, k, env)
	})
}

// Equal succeeds iff lhs equals to rhs.
func (fs FunctionSet) Equal(lhs, rhs Term, k func(*Env) *Promise, env *Env) *Promise {
	return fs.compare(lhs, rhs, k, func(i Integer, j Integer) bool {
		return i == j
	}, func(f Float, g Float) bool {
		return f == g
	}, env)
}

// NotEqual succeeds iff lhs doesn't equal to rhs.
func (fs FunctionSet) NotEqual(lhs, rhs Term, k func(*Env) *Promise, env *Env) *Promise {
	return fs.compare(lhs, rhs, k, func(i Integer, j Integer) bool {
		return i != j
	}, func(f Float, g Float) bool {
		return f != g
	}, env)
}

// LessThan succeeds iff lhs is less than rhs.
func (fs FunctionSet) LessThan(lhs, rhs Term, k func(*Env) *Promise, env *Env) *Promise {
	return fs.compare(lhs, rhs, k, func(i Integer, j Integer) bool {
		return i < j
	}, func(f Float, g Float) bool {
		return f < g
	}, env)
}

// GreaterThan succeeds iff lhs is greater than rhs.
func (fs FunctionSet) GreaterThan(lhs, rhs Term, k func(*Env) *Promise, env *Env) *Promise {
	return fs.compare(lhs, rhs, k, func(i Integer, j Integer) bool {
		return i > j
	}, func(f Float, g Float) bool {
		return f > g
	}, env)
}

// LessThanOrEqual succeeds iff lhs is less than or equal to rhs.
func (fs FunctionSet) LessThanOrEqual(lhs, rhs Term, k func(*Env) *Promise, env *Env) *Promise {
	return fs.compare(lhs, rhs, k, func(i Integer, j Integer) bool {
		return i <= j
	}, func(f Float, g Float) bool {
		return f <= g
	}, env)
}

// GreaterThanOrEqual succeeds iff lhs is greater than or equal to rhs.
func (fs FunctionSet) GreaterThanOrEqual(lhs, rhs Term, k func(*Env) *Promise, env *Env) *Promise {
	return fs.compare(lhs, rhs, k, func(i Integer, j Integer) bool {
		return i >= j
	}, func(f Float, g Float) bool {
		return f >= g
	}, env)
}

func (fs FunctionSet) compare(lhs, rhs Term, k func(*Env) *Promise, pi func(Integer, Integer) bool, pf func(Float, Float) bool, env *Env) *Promise {
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
		return nil, InstantiationError(expression)
	case Atom: // TODO: constants?
		return nil, typeErrorEvaluable(&Compound{
			Functor: "/",
			Args:    []Term{t, Integer(0)},
		})
	case Integer, Float:
		return t, nil
	case *Compound:
		switch len(t.Args) {
		case 1:
			f, ok := fs.Unary[t.Functor]
			if !ok {
				return nil, typeErrorEvaluable(&Compound{
					Functor: "/",
					Args: []Term{
						t.Functor,
						Integer(1),
					},
				})
			}
			x, err := fs.eval(t.Args[0], env)
			if err != nil {
				return nil, err
			}
			return f(x, env)
		case 2:
			f, ok := fs.Binary[t.Functor]
			if !ok {
				return nil, typeErrorEvaluable(&Compound{
					Functor: "/",
					Args: []Term{
						t.Functor,
						Integer(2),
					},
				})
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
			return nil, typeErrorEvaluable(t)
		}
	default:
		return nil, typeErrorEvaluable(t)
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

	var ks []func(context.Context) *Promise
	for _, s := range streams {
		var properties []Term

		switch s.mode {
		case StreamModeRead:
			properties = append(properties, &Compound{Functor: "mode", Args: []Term{Atom("read")}})
		case StreamModeWrite:
			properties = append(properties, &Compound{Functor: "mode", Args: []Term{Atom("write")}})
		case StreamModeAppend:
			properties = append(properties, &Compound{Functor: "mode", Args: []Term{Atom("append")}})
		}

		if s.alias != "" {
			properties = append(properties, &Compound{Functor: "alias", Args: []Term{s.alias}})
		}

		switch s.eofAction {
		case EOFActionError:
			properties = append(properties, &Compound{Functor: "eof_action", Args: []Term{Atom("error")}})
		case EOFActionEOFCode:
			properties = append(properties, &Compound{Functor: "eof_action", Args: []Term{Atom("eof_code")}})
		case EOFActionReset:
			properties = append(properties, &Compound{Functor: "eof_action", Args: []Term{Atom("reset")}})
		}

		if f, ok := s.file.(*os.File); ok {
			pos, err := f.Seek(0, 1)
			if err != nil {
				return Error(err)
			}
			pos -= int64(s.buf.Buffered())

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
		case StreamTypeText:
			properties = append(properties, &Compound{Functor: "type", Args: []Term{Atom("text")}})
		case StreamTypeBinary:
			properties = append(properties, &Compound{Functor: "type", Args: []Term{Atom("false")}})
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

var seek = io.Seeker.Seek

// SetStreamPosition sets the position property of the stream represented by streamOrAlias.
func (state *State) SetStreamPosition(streamOrAlias, position Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := state.stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	if !s.reposition {
		return Error(PermissionError("reposition", "stream", streamOrAlias, "%s is not repositionable.", streamOrAlias))
	}

	switch p := env.Resolve(position).(type) {
	case Variable:
		return Error(InstantiationError(position))
	case Integer:
		if f, ok := s.file.(io.Seeker); ok {
			if _, err := seek(f, int64(p), 0); err != nil {
				return Error(SystemError(err))
			}

			s.buf.Reset(s.file)
		}

		return k(env)
	default:
		return Error(typeErrorInteger(position))
	}
}

// CharConversion registers a character conversion from inChar to outChar, or remove the conversion if inChar = outChar.
func (state *State) CharConversion(inChar, outChar Term, k func(*Env) *Promise, env *Env) *Promise {
	switch in := env.Resolve(inChar).(type) {
	case Variable:
		return Error(InstantiationError(inChar))
	case Atom:
		i := []rune(in)
		if len(i) != 1 {
			return Error(representationError(Atom("character"), Atom(fmt.Sprintf("%s is not a character.", inChar))))
		}

		switch out := env.Resolve(outChar).(type) {
		case Variable:
			return Error(InstantiationError(outChar))
		case Atom:
			o := []rune(out)
			if len(o) != 1 {
				return Error(representationError(Atom("character"), Atom(fmt.Sprintf("%s is not a character.", outChar))))
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
			return Error(representationError(Atom("character"), Atom(fmt.Sprintf("%s is not a character.", outChar))))
		}
	default:
		return Error(representationError(Atom("character"), Atom(fmt.Sprintf("%s is not a character.", inChar))))
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
		return Error(InstantiationError(flag))
	case Atom:
		switch f {
		case "bounded", "max_integer", "min_integer", "integer_rounding_function", "max_arity":
			return Error(PermissionError("modify", "flag", f, "%s is not modifiable.", f))
		case "char_conversion":
			switch a := env.Resolve(value).(type) {
			case Variable:
				return Error(InstantiationError(value))
			case Atom:
				switch a {
				case "on":
					state.charConvEnabled = true
					return k(env)
				case "off":
					state.charConvEnabled = false
					return k(env)
				default:
					return Error(domainErrorFlagValue(&Compound{
						Functor: "+",
						Args:    []Term{f, a},
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
				return Error(InstantiationError(value))
			case Atom:
				switch a {
				case "on":
					state.debug = true
					return k(env)
				case "off":
					state.debug = false
					return k(env)
				default:
					return Error(domainErrorFlagValue(&Compound{
						Functor: "+",
						Args:    []Term{f, a},
					}))
				}
			default:
				return Error(domainErrorFlagValue(&Compound{
					Functor: "+",
					Args:    []Term{f, a},
				}))
			}
		case "unknown":
			switch a := env.Resolve(value).(type) {
			case Variable:
				return Error(InstantiationError(value))
			case Atom:
				switch a {
				case "error":
					state.unknown = unknownError
					return k(env)
				case "warning":
					state.unknown = unknownWarning
					return k(env)
				case "fail":
					state.unknown = unknownFail
					return k(env)
				default:
					return Error(domainErrorFlagValue(&Compound{
						Functor: "+",
						Args:    []Term{f, a},
					}))
				}
			default:
				return Error(domainErrorFlagValue(&Compound{
					Functor: "+",
					Args:    []Term{f, a},
				}))
			}
		case "double_quotes":
			switch a := env.Resolve(value).(type) {
			case Variable:
				return Error(InstantiationError(value))
			case Atom:
				switch a {
				case "codes":
					state.doubleQuotes = DoubleQuotesCodes
					return k(env)
				case "chars":
					state.doubleQuotes = DoubleQuotesChars
					return k(env)
				case "atom":
					state.doubleQuotes = DoubleQuotesAtom
					return k(env)
				default:
					return Error(domainErrorFlagValue(&Compound{
						Functor: "+",
						Args:    []Term{f, a},
					}))
				}
			default:
				return Error(domainErrorFlagValue(&Compound{
					Functor: "+",
					Args:    []Term{f, a},
				}))
			}
		default:
			return Error(domainErrorPrologFlag(f))
		}
	default:
		return Error(typeErrorAtom(f))
	}
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
		return Error(typeErrorAtom(f))
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
		return nil, InstantiationError(streamOrAlias)
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

// ExpandTerm transforms term1 according to term_expansion/2 and unifies with term2.
func (state *State) ExpandTerm(term1, term2 Term, k func(*Env) *Promise, env *Env) *Promise {
	const termExpansion = "term_expansion"
	return Delay(func(ctx context.Context) *Promise {
		if _, ok := state.procedures[ProcedureIndicator{Name: termExpansion, Arity: 2}]; !ok {
			return Bool(false)
		}

		return state.Call(&Compound{
			Functor: termExpansion,
			Args:    []Term{term1, term2},
		}, k, env)
	}, func(ctx context.Context) *Promise {
		return Unify(term1, term2, k, env)
	})
}
