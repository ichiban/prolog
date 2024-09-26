package prolog

import (
	"context"
	"errors"
	"github.com/ichiban/prolog/internal"
)

import (
	"bytes"
	"io"
	"io/fs"
	"os"
	"sort"
	"strings"
	"unicode"
	"unicode/utf8"
)

// Not calls goal and returns false if it succeeds. Otherwise, invokes the continuation.
func Not(ctx context.Context, goal Term) Promise {
	return Delay(func(yield func(thunk func() Promise)) {
		yield(func() Promise {
			vm := internal.ContextVM(ctx)
			ok, err := Call(ctx, goal).Force(vm.Stack)
			if err != nil {
				return internal.Error(err)
			}
			if ok {
				return Failure
			}
			return Continue(ctx)
		})
	})
}

// Call executes goal. it succeeds if goal followed by k succeeds. A cut inside goal doesn't affect outside of Call.
func Call(ctx context.Context, goal Term) Promise {
	vm := internal.ContextVM(ctx)
	k := internal.ContextCont(ctx)
	return vm.Call(ctx, goal, k)
}

// Call1 succeeds if closure with an additional argument succeeds.
func Call1(ctx context.Context, closure, arg1 Term) Promise {
	vm := internal.ContextVM(ctx)
	return vm.CallN(ctx, closure, []Term{arg1})
}

// Call2 succeeds if closure with 2 additional arguments succeeds.
func Call2(ctx context.Context, closure, arg1, arg2 Term) Promise {
	vm := internal.ContextVM(ctx)
	return vm.CallN(ctx, closure, []Term{arg1, arg2})
}

// Call3 succeeds if closure with 3 additional arguments succeeds.
func Call3(ctx context.Context, closure, arg1, arg2, arg3 Term) Promise {
	vm := internal.ContextVM(ctx)
	return vm.CallN(ctx, closure, []Term{arg1, arg2, arg3})
}

// Call4 succeeds if closure with 4 additional arguments succeeds.
func Call4(ctx context.Context, closure, arg1, arg2, arg3, arg4 Term) Promise {
	vm := internal.ContextVM(ctx)
	return vm.CallN(ctx, closure, []Term{arg1, arg2, arg3, arg4})
}

// Call5 succeeds if closure with 5 additional arguments succeeds.
func Call5(ctx context.Context, closure, arg1, arg2, arg3, arg4, arg5 Term) Promise {
	vm := internal.ContextVM(ctx)
	return vm.CallN(ctx, closure, []Term{arg1, arg2, arg3, arg4, arg5})
}

// Call6 succeeds if closure with 6 additional arguments succeeds.
func Call6(ctx context.Context, closure, arg1, arg2, arg3, arg4, arg5, arg6 Term) Promise {
	vm := internal.ContextVM(ctx)
	return vm.CallN(ctx, closure, []Term{arg1, arg2, arg3, arg4, arg5, arg6})
}

// Call7 succeeds if closure with 7 additional arguments succeeds.
func Call7(ctx context.Context, closure, arg1, arg2, arg3, arg4, arg5, arg6, arg7 Term) Promise {
	vm := internal.ContextVM(ctx)
	return vm.CallN(ctx, closure, []Term{arg1, arg2, arg3, arg4, arg5, arg6, arg7})
}

// Unify unifies x and y without occurs check (i.e., X = f(X) is allowed).
func Unify(ctx context.Context, x, y Term) Promise {
	vm := internal.ContextVM(ctx)
	ok, err := vm.Terms.Unify(x, y)
	if err != nil {
		return Error(err)
	}
	if !ok {
		return Failure
	}
	return Continue(ctx)
}

// UnifyWithOccursCheck unifies x and y with occurs check (i.e., X = f(X) is not allowed).
func UnifyWithOccursCheck(ctx context.Context, x, y Term) Promise {
	vm := internal.ContextVM(ctx)
	ok, err := vm.Terms.UnifyWithOccursCheck(x, y)
	if err != nil {
		return Error(err)
	}
	if !ok {
		return Failure
	}
	return Continue(ctx)
}

// SubsumesTerm succeeds if general and specific are unifiable without binding variables in specific.
func SubsumesTerm(ctx context.Context, general, specific Term) Promise {
	vm := internal.ContextVM(ctx)
	snapshot := vm.Terms
	defer func() {
		vm.Terms = snapshot
	}()

	ok, err := vm.Terms.UnifyWithOccursCheck(general, specific)
	if err != nil {
		return Error(err)
	}
	if !ok {
		return Failure
	}

	g, err := vm.Terms.RenamedCopy(general)
	if err != nil {
		return Error(err)
	}

	d, err := vm.Terms.Compare(g, specific)
	if err != nil {
		return Error(err)
	}
	if d != 0 {
		return Failure
	}

	return Continue(ctx)
}

// TypeVar checks if t is a variable.
func TypeVar(ctx context.Context, t Term) Promise {
	vm := internal.ContextVM(ctx)
	if _, ok := vm.Terms.Variable(t); !ok {
		return Failure
	}
	return Continue(ctx)
}

// TypeFloat checks if t is a floating-point number.
func TypeFloat(ctx context.Context, t Term) Promise {
	vm := internal.ContextVM(ctx)
	if _, ok := vm.Terms.Float(t); !ok {
		return Failure
	}
	return Continue(ctx)
}

// TypeInteger checks if t is an integer.
func TypeInteger(ctx context.Context, t Term) Promise {
	vm := internal.ContextVM(ctx)
	if _, ok := vm.Terms.Integer(t); !ok {
		return Failure
	}
	return Continue(ctx)
}

// TypeAtom checks if t is an atom.
func TypeAtom(ctx context.Context, t Term) Promise {
	vm := internal.ContextVM(ctx)
	if _, ok := vm.Terms.Atom(t); !ok {
		return Failure
	}
	return Continue(ctx)
}

// TypeCompound checks if t is a compound term.
func TypeCompound(ctx context.Context, t Term) Promise {
	vm := internal.ContextVM(ctx)
	if _, _, ok := vm.Terms.Compound(t); !ok {
		return Failure
	}
	return Continue(ctx)
}

// AcyclicTerm checks if t is acyclic.
func AcyclicTerm(ctx context.Context, t Term) Promise {
	vm := internal.ContextVM(ctx)
	ok, err := vm.Terms.CyclicTerm(t)
	if err != nil {
		return Error(err)
	}
	if !ok {
		return Failure
	}
	return Continue(ctx)
}

/*
// Functor extracts the name and arity of term, or unifies term with an atomic/compound term of name and arity with
// fresh variables as arguments.
func Functor(vm *VM, t, name, arity Term, k Cont, env *Env) *Promise {
	switch t := env.Resolve(t).(type) {
	case Variable:
		switch arity := env.Resolve(arity).(type) {
		case Variable:
			return Error(InstantiationError(env))
		case Integer:
			if arity < 0 {
				return Error(domainError(validDomainNotLessThanZero, arity, env))
			}

			name := env.Resolve(name)

			switch name := name.(type) {
			case Variable:
				return Error(InstantiationError(env))
			case Compound:
				return Error(typeError(validTypeAtomic, name, env))
			}

			if arity == 0 {
				return Unify(vm, t, name, k, env)
			}

			n, ok := name.(Atom)
			if !ok {
				return Error(typeError(validTypeAtom, name, env))
			}

			vs, err := makeSlice(int(arity))
			if err != nil {
				return Error(resourceError(resourceMemory, env))
			}
			for i := range vs {
				vs[i] = NewVariable()
			}
			return Unify(vm, t, n.Apply(vs...), k, env)
		default:
			return Error(typeError(validTypeInteger, arity, env))
		}
	case Compound:
		return Unify(vm, tuple(name, arity), tuple(t.Functor(), Integer(t.Arity())), k, env)
	default: // atomic
		return Unify(vm, tuple(name, arity), tuple(t, Integer(0)), k, env)
	}
}

*/

// Arg extracts nth argument of term as arg, or finds the argument position of arg in term as nth.
func Arg(ctx context.Context, nth, t, arg Term) Promise {
	vm := internal.ContextVM(ctx)

	if _, ok := vm.Terms.Variable(t); ok {
		return Error(internal.ErrInstantiation)
	}

	f, args, ok := vm.Terms.Compound(t)
	if !ok {
		return Error(&internal.TypeError{Type: NewAtom("compound"), Culprit: t})
	}

	if _, ok := vm.Terms.Variable(nth); ok {
		return Error(internal.ErrInstantiation)
	}

	n, ok := vm.Terms.Integer(nth)
	if !ok {
		return Error(&internal.TypeError{Type: NewAtom("integer"), Culprit: nth})
	}

	if n == 0 || int(n) > f.Arity {
		return Failure
	}
	if n < 0 {
		return Error(&internal.DomainError{Domain: NewAtom("not_less_than_zero"), Culprit: nth})
	}

	a, err := args(int(n))
	if err != nil {
		return Error(err)
	}

	return Unify(ctx, arg, a)
}

// Univ constructs list as a list which first element is the functor of term and the rest is the arguments of term, or construct a compound from list as term.
func Univ(ctx context.Context, t, list Term) Promise {
	vm := internal.ContextVM(ctx)

	if _, ok := vm.Terms.Variable(t); ok {
		var elems []Term
		for e, err := range vm.Terms.MustBeList(list) {
			if err != nil {
				return Error(err)
			}
			elems = append(elems, vm.Terms.Resolve(e))
		}

		switch len(elems) {
		case 0:
			return Error(&internal.DomainError{Domain: NewAtom("non_empty_list"), Culprit: list})
		case 1:
			e := elems[0]
			if _, ok := vm.Terms.Variable(e); ok {
				return Error(internal.ErrInstantiation)
			}
			if !vm.Terms.Atomic(e) {
				return Error(&internal.TypeError{Type: NewAtom("atomic"), Culprit: e})
			}
			return Unify(ctx, t, e)
		default:
			e := elems[0]
			if _, ok := vm.Terms.Variable(e); ok {
				return Error(internal.ErrInstantiation)
			}
			a, ok := vm.Terms.Atom(e)
			if !ok {
				return Error(&internal.TypeError{Type: NewAtom("atom"), Culprit: e})
			}
			c, err := vm.Terms.PutCompound(a, elems[1:]...)
			if err != nil {
				return Error(err)
			}
			return Unify(ctx, t, c)
		}
	}

	for _, err := range vm.Terms.CanBeList(list) {
		if err != nil {
			return Error(err)
		}
	}

	var elems []Term
	if f, arg, ok := vm.Terms.Compound(t); ok {
		elems = make([]Term, 0, f.Arity+1)
		functor, err := vm.Terms.PutAtom(f.Name)
		if err != nil {
			return Error(err)
		}
		elems = append(elems, functor)
		for i := 0; i < f.Arity; i++ {
			a, err := arg(i)
			if err != nil {
				return Error(err)
			}
			elems = append(elems, a)
		}
	} else {
		elems = []Term{t}
	}

	l, err := vm.Terms.PutList(elems...)
	if err != nil {
		return Error(err)
	}

	return Unify(ctx, list, l)
}

// CopyTerm clones in as out.
func CopyTerm(ctx context.Context, in, out Term) Promise {
	vm := internal.ContextVM(ctx)
	c, err := vm.Terms.RenamedCopy(in)
	if err != nil {
		return internal.Error(err)
	}
	return Unify(ctx, c, out)
}

// TermVariables succeeds if vars unifies with a list of variables in term.
func TermVariables(ctx context.Context, term, vars Term) Promise {
	var (
		vm       = internal.ContextVM(ctx)
		witness  = map[internal.Variable]struct{}{}
		ret      []Term
		t        Term
		traverse = []Term{term}
	)
	for len(traverse) > 0 {
		t, traverse = vm.Terms.Resolve(traverse[0]), traverse[1:]

		if v, ok := vm.Terms.Variable(t); ok {
			if _, ok := witness[v]; !ok {
				ret = append(ret, t)
			}
			witness[v] = struct{}{}
			continue
		}

		if f, arg, ok := vm.Terms.Compound(t); ok {
			args := make([]Term, f.Arity)
			for i := 0; i < f.Arity; i++ {
				a, err := arg(i)
				if err != nil {
					return Error(err)
				}
				args[i] = a
			}
			traverse = append(args, traverse...)
			continue
		}
	}

	for _, err := range vm.Terms.CanBeList(vars) {
		if err != nil {
			return Error(err)
		}
	}

	r, err := vm.Terms.PutList(ret...)
	if err != nil {
		return Error(err)
	}

	return Unify(ctx, vars, r)
}

var (
	atomFX  = NewAtom("fx")
	atomFY  = NewAtom("fy")
	atomXF  = NewAtom("xf")
	atomYF  = NewAtom("yf")
	atomXFX = NewAtom("xfx")
	atomXFY = NewAtom("xfy")
	atomYFX = NewAtom("yfx")
)

var operatorSpecifiers = map[Atom]internal.OperatorSpecifier{
	atomFX:  internal.OperatorSpecifierFX,
	atomFY:  internal.OperatorSpecifierFY,
	atomXF:  internal.OperatorSpecifierXF,
	atomYF:  internal.OperatorSpecifierYF,
	atomXFX: internal.OperatorSpecifierXFX,
	atomXFY: internal.OperatorSpecifierXFY,
	atomYFX: internal.OperatorSpecifierYFX,
}

// Op defines operator with priority and specifier, or removes when priority is 0.
func Op(ctx context.Context, priority, specifier, op Term) Promise {
	vm := internal.ContextVM(ctx)
	priority = vm.Terms.Resolve(priority)
	specifier = vm.Terms.Resolve(specifier)
	op = vm.Terms.Resolve(op)

	if _, ok := vm.Terms.Variable(op); ok {
		return Error(internal.ErrInstantiation)
	}

	p, err := vm.Terms.MustBeOperatorPriority(priority)
	if err != nil {
		return Error(err)
	}

	spec, err := vm.Terms.MustBeOperatorSpecifier(specifier)
	if err != nil {
		return Error(err)
	}

	var names []Atom
	if a, ok := vm.Terms.Atom(op); ok {
		names = []Atom{a}
	} else {
		for e, err := range vm.Terms.MustBeList(op) {
			if err != nil {
				return Error(err)
			}

			a, err := vm.Terms.MustBeAtom(e)
			if err != nil {
				return Error(err)
			}

			names = internal.AppendUnique(names, a)
		}
	}

	m := vm.TypeInModule()

	for _, name := range names {
		switch err := m.SetOperator(p, spec, name.String()); {
		case err == nil:
			break
		case errors.Is(err, internal.ErrOpCreation):
			n, err := vm.Terms.PutAtom(name)
			if err != nil {
				return Error(err)
			}
			return Error(&internal.PermissionError{Operation: NewAtom("create"), PermissionType: NewAtom("operator"), Culprit: n})
		case errors.Is(err, internal.ErrOpModification):
			n, err := vm.Terms.PutAtom(name)
			if err != nil {
				return Error(err)
			}
			return Error(&internal.PermissionError{Operation: NewAtom("modify"), PermissionType: NewAtom("operator"), Culprit: n})
		}
	}

	return Continue(ctx)
}

// CurrentOp succeeds if operator is defined with priority and specifier.
func CurrentOp(ctx context.Context, priority, specifier, op Term) Promise {
	vm := internal.ContextVM(ctx)

	if err := vm.Terms.CanBeOperatorPriority(priority); err != nil {
		return Error(err)
	}

	if err := vm.Terms.CanBeOperatorSpecifier(specifier); err != nil {
		return Error(err)
	}

	if err := vm.Terms.CanBeAtom(op); err != nil {
		return Error(err)
	}

	m := vm.TypeInModule()

	pattern, err := vm.Terms.PutCompound(Atom('$'), priority, specifier, op)
	if err != nil {
		return Error(err)
	}

	return Delay(func(yield func(thunk func() Promise)) {
		for op := range m.Operators() {
			yield(func() Promise {
				p, err := vm.Terms.PutInteger(int64(op.Priority))
				if err != nil {
					return Error(err)
				}

				s, err := vm.Terms.PutOperatorSpecifier(op.Specifier)
				if err != nil {
					return Error(err)
				}

				n, err := vm.Terms.PutAtom(op.Name)
				if err != nil {
					return Error(err)
				}

				o, err := vm.Terms.PutCompound(Atom('$'), p, s, n)
				if err != nil {
					return Error(err)
				}

				return Unify(ctx, pattern, o)
			})
		}
	})
}

// Asserta prepends t to the database.
func Asserta(ctx context.Context, t Term) Promise {
	vm := internal.ContextVM(ctx)
	if err := vm.AssertA(t); err != nil {
		return Error(err)
	}
	return Continue(ctx)
}

// Assertz appends t to the database.
func Assertz(ctx context.Context, t Term) Promise {
	vm := internal.ContextVM(ctx)
	if err := vm.AssertZ(t); err != nil {
		return Error(err)
	}
	return Continue(ctx)
}

type (
	bag       []Term
	ctxKeyBag struct{}
)

var errNoBag = errors.New("no bag")

// CreateBag creates an empty bag for findall/3.
func CreateBag(ctx context.Context) Promise {
	ctx = context.WithValue(ctx, ctxKeyBag{}, &bag{})
	return Continue(ctx)
}

// AppendBag appends a term to the bag.
func AppendBag(ctx context.Context, t Term) Promise {
	b, ok := ctx.Value(ctxKeyBag{}).(*bag)
	if !ok {
		return Error(errNoBag)
	}

	*b = append(*b, t)

	return Continue(ctx)
}

// UnifyBag unifies the bag with a term.
func UnifyBag(ctx context.Context, out Term) Promise {
	vm := internal.ContextVM(ctx)

	b, ok := ctx.Value(ctxKeyBag{}).(*bag)
	if !ok {
		return Error(errNoBag)
	}

	bag, err := vm.Terms.PutList(*b...)
	if err != nil {
		return Error(err)
	}

	return Unify(ctx, out, bag)
}

// Compare compares term1 and term2 and unifies order with <, =, or >.
func Compare(ctx context.Context, order, term1, term2 Term) Promise {
	vm := internal.ContextVM(ctx)

	if err := vm.Terms.CanBeOrder(order); err != nil {
		return Error(err)
	}

	o, err := vm.Terms.Compare(term1, term2)
	if err != nil {
		return Error(err)
	}

	var ret Atom
	switch o {
	case 1:
		ret = Atom('<')
	case -1:
		ret = Atom('>')
	default:
		ret = Atom('=')
	}

	or, err := vm.Terms.PutAtom(ret)
	if err != nil {
		return Error(err)
	}

	return Unify(ctx, order, or)
}

// Between succeeds when lower, upper, and value are all integers, and lower <= value <= upper.
// If value is a variable, it is unified with successive integers from lower to upper.
func Between(ctx context.Context, lower, upper, value Term) Promise {
	vm := internal.ContextVM(ctx)

	low, err := vm.Terms.MustBeInteger(lower)
	if err != nil {
		return Error(err)
	}

	high, err := vm.Terms.MustBeInteger(upper)
	if err != nil {
		return Error(err)
	}

	if err := vm.Terms.CanBeInteger(value); err != nil {
		return Error(err)
	}

	if low > high {
		return Failure
	}

	if v, ok := vm.Terms.Integer(value); ok {
		if v < low || v > high {
			return Failure
		}
		return Continue(ctx)
	}

	return Delay(func(yield func(thunk func() Promise)) {
		yield(func() Promise {
			return Unify(ctx, value, lower)
		})
		if low < high {
			yield(func() Promise {
				l, err := vm.Terms.PutInteger(low + 1)
				if err != nil {
					return Error(err)
				}
				return Between(ctx, l, upper, value)
			})
		}
	})
}

// Sort succeeds if sorted list of elements of list unifies with sorted.
func Sort(ctx context.Context, list, sorted Term) Promise {
	vm := internal.ContextVM(ctx)

	var elems []Term
	for e, err := range vm.Terms.MustBeList(list) {
		if err != nil {
			return Error(err)
		}
		elems = append(elems, vm.Terms.Resolve(e))
	}

	for _, err := range vm.Terms.CanBeList(sorted) {
		if err != nil {
			return Error(err)
		}
	}

	s, err := vm.Terms.PutSet(elems...)
	if err != nil {
		return Error(err)
	}

	return Unify(ctx, sorted, s)
}

// KeySort succeeds if sorted is a sorted list of pairs based on their keys.
func KeySort(ctx context.Context, pairs, sorted Term) Promise {
	vm := internal.ContextVM(ctx)

	type pair struct {
		key Term
		val Term
	}
	var elems []pair
	for e, err := range vm.Terms.MustBeList(pairs) {
		if err != nil {
			return Error(err)
		}

		k, v, err := vm.Terms.MustBePair(e)
		if err != nil {
			return Error(err)
		}

		elems = append(elems, pair{
			key: k,
			val: v,
		})
	}

	for e, err := range vm.Terms.CanBeList(sorted) {
		if err != nil {
			return Error(err)
		}

		if err := vm.Terms.CanBePair(e); err != nil {
			return Error(err)
		}
	}

	sort.SliceStable(elems, func(i, j int) bool {
		return vm.Terms.Compare(elems[i].key, elems[j].key) == -1
	})

	return Unify(vm, sorted, List(elems...), k, env)
}

// Throw throws ball as an exception.
func Throw(ctx context.Context, ball Term, _ internal.Cont) Promise {
	switch b := env.Resolve(ball).(type) {
	case internal.Variable:
		return internal.Error(InstantiationError(env))
	default:
		return internal.Error(NewException(b, env))
	}
}

// Catch calls goal. If an exception is thrown and unifies with catcher, it calls recover.
func Catch(ctx context.Context, goal, catcher, recover Term) Promise {
	return catch(func(err error) Promise {
		e, ok := err.(internal.Exception)
		if !ok {
			e = internal.Exception{term: atomError.Apply(NewAtom("system_error"), NewAtom(err.Error()))}
		}

		env, ok := env.Unify(catcher, e.term)
		if !ok {
			return nil
		}

		return Call(vm, recover, k, env)
	}, func(ctx context.Context) Promise {
		return Call(vm, goal, k, env)
	})
}

// CurrentPredicate matches pi with a predicate indicator of the user-defined procedures in the database.
func CurrentPredicate(ctx context.Context, pi Term) Promise {
	switch pi := env.Resolve(pi).(type) {
	case internal.Variable:
		break
	case Compound:
		if pi.Functor() != atomSlash || pi.Arity() != 2 {
			return internal.Error(typeError(validTypePredicateIndicator, pi, env))
		}
		if _, ok := env.Resolve(pi.Arg(0)).(Atom); !ok {
			return internal.Error(typeError(validTypePredicateIndicator, pi, env))
		}
		if _, ok := env.Resolve(pi.Arg(1)).(Integer); !ok {
			return internal.Error(typeError(validTypePredicateIndicator, pi, env))
		}
	default:
		return internal.Error(typeError(validTypePredicateIndicator, pi, env))
	}

	m := vm.TypeInModule()
	ks := make([]func(context.Context) *internal.Promise, 0, len(m.procedures))
	for key, e := range m.procedures {
		switch e.procedure.(type) {
		case internal.clauses:
			c := key.Term()
			ks = append(ks, func(context.Context) Promise {
				return Unify(vm, pi, c, k, env)
			})
		default:
			continue
		}
	}
	return Delay(ks...)
}

// Retract removes the first clause that matches with t.
func Retract(ctx context.Context, t Term) Promise {
	m := vm.TypeInModule()
	t = rulify(t, env)

	h := t.(Compound).Arg(0)
	pi, _, err := piArg(h, env)
	if err != nil {
		return internal.Error(err)
	}

	e, ok := m.procedures[pi]
	if !ok {
		return Failure
	}

	cs, ok := e.procedure.(internal.clauses)
	if !ok || !e.dynamic {
		return internal.Error(permissionError(operationModify, permissionTypeStaticProcedure, pi.Term(), env))
	}

	deleted := 0
	ks := make([]func(context.Context) *internal.Promise, len(cs))
	for i, c := range cs {
		i := i
		raw := rulify(c.raw, env)
		ks[i] = func(_ context.Context) Promise {
			return Unify(vm, t, raw, func(env *internal.Env) Promise {
				j := i - deleted
				cs, cs[len(cs)-1] = append(cs[:j], cs[j+1:]...), internal.clause{}
				e.procedure = cs
				m.procedures[pi] = e
				deleted++
				return Continue(ctx)
			}, env)
		}
	}
	return Delay(ks...)
}

// Abolish removes the procedure indicated by pi from the database.
func Abolish(ctx context.Context, pi Term) Promise {
	switch pi := env.Resolve(pi).(type) {
	case internal.Variable:
		return internal.Error(InstantiationError(env))
	case Compound:
		if pi.Functor() != atomSlash || pi.Arity() != 2 {
			return internal.Error(typeError(validTypePredicateIndicator, pi, env))
		}

		name, arity := pi.Arg(0), pi.Arg(1)

		switch name := env.Resolve(name).(type) {
		case internal.Variable:
			return internal.Error(InstantiationError(env))
		case Atom:
			switch arity := env.Resolve(arity).(type) {
			case internal.Variable:
				return internal.Error(InstantiationError(env))
			case Integer:
				if arity < 0 {
					return internal.Error(domainError(validDomainNotLessThanZero, arity, env))
				}
				m := vm.TypeInModule()
				key := predicateIndicator{name: name, arity: arity}
				if e, ok := m.procedures[key]; !ok || !e.dynamic {
					return internal.Error(permissionError(operationModify, permissionTypeStaticProcedure, key.Term(), env))
				}
				delete(m.procedures, key)
				return Continue(ctx)
			default:
				return internal.Error(typeError(validTypeInteger, arity, env))
			}
		default:
			return internal.Error(typeError(validTypeAtom, name, env))
		}
	default:
		return internal.Error(typeError(validTypePredicateIndicator, pi, env))
	}
}

// CurrentInput unifies stream with the current input stream.
func CurrentInput(ctx context.Context, stream Term) Promise {
	switch env.Resolve(stream).(type) {
	case internal.Variable, *internal.Stream:
		return Unify(vm, stream, vm.input, k, env)
	default:
		return internal.Error(domainError(validDomainStream, stream, env))
	}
}

// CurrentOutput unifies stream with the current output stream.
func CurrentOutput(ctx context.Context, stream Term) Promise {
	switch env.Resolve(stream).(type) {
	case internal.Variable, *internal.Stream:
		return Unify(vm, stream, vm.output, k, env)
	default:
		return internal.Error(domainError(validDomainStream, stream, env))
	}
}

// SetInput sets streamOrAlias as the current input stream.
func SetInput(ctx context.Context, streamOrAlias Term) Promise {
	s, err := stream(vm, streamOrAlias, env)
	if err != nil {
		return internal.Error(err)
	}

	if s.mode != internal.ioModeRead {
		return internal.Error(permissionError(operationInput, permissionTypeStream, streamOrAlias, env))
	}

	vm.input = s
	return Continue(ctx)
}

// SetOutput sets streamOrAlias as the current output stream.
func SetOutput(ctx context.Context, streamOrAlias Term) Promise {
	s, err := stream(vm, streamOrAlias, env)
	if err != nil {
		return internal.Error(err)
	}

	if s.mode != internal.ioModeWrite && s.mode != internal.ioModeAppend {
		return internal.Error(permissionError(operationOutput, permissionTypeStream, streamOrAlias, env))
	}

	vm.output = s
	return Continue(ctx)
}

func stream(ctx context.Context, streamOrAlias Term) (*internal.Stream, error) {
	switch s := env.Resolve(streamOrAlias).(type) {
	case internal.Variable:
		return nil, InstantiationError(env)
	case Atom:
		v, ok := vm.streams.lookup(s)
		if !ok {
			return nil, existenceError(objectTypeStream, streamOrAlias, env)
		}
		return v, nil
	case *internal.Stream:
		return s, nil
	default:
		return nil, domainError(validDomainStreamOrAlias, streamOrAlias, env)
	}
}

var openFile = os.OpenFile

// Open opens SourceSink in mode and unifies with stream.
func Open(ctx context.Context, sourceSink, mode, stream, options Term) Promise {
	var name string
	switch s := env.Resolve(sourceSink).(type) {
	case internal.Variable:
		return internal.Error(InstantiationError(env))
	case Atom:
		name = s.String()
	default:
		return internal.Error(domainError(validDomainSourceSink, sourceSink, env))
	}

	var streamMode internal.ioMode
	switch m := env.Resolve(mode).(type) {
	case internal.Variable:
		return internal.Error(InstantiationError(env))
	case Atom:
		var ok bool
		streamMode, ok = map[Atom]internal.ioMode{
			atomRead:   internal.ioModeRead,
			atomWrite:  internal.ioModeWrite,
			atomAppend: internal.ioModeAppend,
		}[m]
		if !ok {
			return internal.Error(domainError(validDomainIOMode, m, env))
		}
	default:
		return internal.Error(typeError(validTypeAtom, mode, env))
	}

	if _, ok := env.Resolve(stream).(internal.Variable); !ok {
		return internal.Error(InstantiationError(env))
	}

	s := internal.Stream{vm: vm, mode: streamMode}
	switch f, err := openFile(name, int(s.mode), 0644); {
	case err == nil:
		if s.mode == internal.ioModeRead {
			s.source = f
			s.initRead()
		} else {
			s.sink = f
		}
		if fi, err := f.Stat(); err == nil {
			s.reposition = fi.Mode()&fs.ModeType == 0
		}
	case os.IsNotExist(err):
		return internal.Error(existenceError(objectTypeSourceSink, sourceSink, env))
	case os.IsPermission(err):
		return internal.Error(permissionError(operationOpen, permissionTypeSourceSink, sourceSink, env))
	default:
		return internal.Error(err)
	}

	iter := internal.ListIterator{List: options, Env: env}
	for iter.Next() {
		if err := handleStreamOption(vm, &s, iter.Current(), env); err != nil {
			return internal.Error(err)
		}
	}
	if err := iter.Err(); err != nil {
		return internal.Error(err)
	}

	return Unify(vm, stream, &s, k, env)
}

func handleStreamOption(ctx context.Context, s *internal.Stream, option Term) error {
	switch o := env.Resolve(option).(type) {
	case internal.Variable:
		return InstantiationError(env)
	case Compound:
		if o.Arity() != 1 {
			break
		}

		switch o.Functor() {
		case atomAlias:
			return handleStreamOptionAlias(vm, s, o, env)
		case atomType:
			return handleStreamOptionType(vm, s, o, env)
		case atomReposition:
			return handleStreamOptionReposition(vm, s, o, env)
		case atomEOFAction:
			return handleStreamOptionEOFAction(vm, s, o, env)
		}
	}
	return domainError(validDomainStreamOption, option, env)
}

func handleStreamOptionAlias(ctx context.Context, s *internal.Stream, o Compound) error {
	switch a := env.Resolve(o.Arg(0)).(type) {
	case internal.Variable:
		return InstantiationError(env)
	case Atom:
		if _, ok := vm.streams.lookup(a); ok {
			return permissionError(operationOpen, permissionTypeSourceSink, o, env)
		}
		s.alias = a
		vm.streams.add(s)
		return nil
	default:
		return domainError(validDomainStreamOption, o, env)
	}
}

func handleStreamOptionType(ctx context.Context, s *internal.Stream, o Compound) error {
	switch t := env.Resolve(o.Arg(0)).(type) {
	case internal.Variable:
		return InstantiationError(env)
	case Atom:
		switch t {
		case atomText:
			s.streamType = internal.streamTypeText
			return nil
		case atomBinary:
			s.streamType = internal.streamTypeBinary
			return nil
		}
	}
	return domainError(validDomainStreamOption, o, env)
}

func handleStreamOptionReposition(ctx context.Context, s *internal.Stream, o Compound) error {
	switch r := env.Resolve(o.Arg(0)).(type) {
	case internal.Variable:
		return InstantiationError(env)
	case Atom:
		switch r {
		case atomTrue:
			s.reposition = true
			return nil
		case atomFalse:
			s.reposition = false
			return nil
		}
	}
	return domainError(validDomainStreamOption, o, env)
}

func handleStreamOptionEOFAction(ctx context.Context, s *internal.Stream, o Compound) error {
	switch e := env.Resolve(o.Arg(0)).(type) {
	case internal.Variable:
		return InstantiationError(env)
	case Atom:
		switch e {
		case atomError:
			s.eofAction = internal.eofActionError
			return nil
		case atomEOFCode:
			s.eofAction = internal.eofActionEOFCode
			return nil
		case atomReset:
			s.eofAction = internal.eofActionReset
			return nil
		}
	}
	return domainError(validDomainStreamOption, o, env)
}

// Close closes a stream specified by streamOrAlias.
func Close(ctx context.Context, streamOrAlias, options Term) Promise {
	s, err := stream(vm, streamOrAlias, env)
	if err != nil {
		return internal.Error(err)
	}

	var force bool
	iter := internal.ListIterator{List: options, Env: env}
	for iter.Next() {
		switch option := env.Resolve(iter.Current()).(type) {
		case internal.Variable:
			return internal.Error(InstantiationError(env))
		case Compound:
			switch option.Functor() {
			case atomForce:
				if option.Arity() != 1 {
					return internal.Error(domainError(validDomainStreamOption, option, env))
				}

				switch v := env.Resolve(option.Arg(0)).(type) {
				case Atom:
					switch v {
					case atomFalse:
						force = false
					case atomTrue:
						force = true
					default:
						return internal.Error(domainError(validDomainStreamOption, option, env))
					}
				default:
					return internal.Error(domainError(validDomainStreamOption, option, env))
				}
			}
		default:
			return internal.Error(domainError(validDomainStreamOption, option, env))
		}
	}
	if err := iter.Err(); err != nil {
		return internal.Error(err)
	}

	if err := s.Close(); err != nil && !force {
		return internal.Error(err)
	}

	return Continue(ctx)
}

// FlushOutput sends any buffered output to the stream.
func FlushOutput(ctx context.Context, streamOrAlias Term) Promise {
	s, err := stream(vm, streamOrAlias, env)
	if err != nil {
		return internal.Error(err)
	}

	switch err := s.Flush(); err {
	case nil:
		return Continue(ctx)
	case internal.errWrongIOMode:
		return internal.Error(permissionError(operationOutput, permissionTypeStream, streamOrAlias, env))
	default:
		return internal.Error(err)
	}
}

// WriteTerm outputs term to stream with options.
func WriteTerm(ctx context.Context, streamOrAlias, t, options Term) Promise {
	s, err := stream(vm, streamOrAlias, env)
	if err != nil {
		return internal.Error(err)
	}

	m := vm.TypeInModule()
	opts := internal.WriteOptions{
		ops:      m.operators,
		priority: 1200,
	}
	iter := internal.ListIterator{List: options, Env: env}
	for iter.Next() {
		if err := writeTermOption(&opts, iter.Current(), env); err != nil {
			return internal.Error(err)
		}
	}
	if err := iter.Err(); err != nil {
		return internal.Error(err)
	}

	w, err := s.textWriter()
	switch {
	case errors.Is(err, internal.errWrongIOMode):
		return internal.Error(permissionError(operationOutput, permissionTypeStream, streamOrAlias, env))
	case errors.Is(err, internal.errWrongStreamType):
		return internal.Error(permissionError(operationOutput, permissionTypeBinaryStream, streamOrAlias, env))
	case err != nil:
		return internal.Error(err)
	}

	if err := env.Resolve(t).WriteTerm(w, &opts, env); err != nil {
		return internal.Error(err)
	}

	return Continue(ctx)
}

func writeTermOption(opts *internal.WriteOptions, option Term) error {
	switch o := env.Resolve(option).(type) {
	case internal.Variable:
		return InstantiationError(env)
	case Compound:
		if o.Arity() != 1 {
			return domainError(validDomainWriteOption, o, env)
		}

		switch o.Functor() {
		case atomQuoted:
			b, err := writeTermOptionBool(o, env)
			opts.quoted = b
			return err
		case atomIgnoreOps:
			b, err := writeTermOptionBool(o, env)
			opts.ignoreOps = b
			return err
		case atomNumberVars:
			b, err := writeTermOptionBool(o, env)
			opts.numberVars = b
			return err
		case atomVariableNames:
			vns, err := writeTermOptionVariableNames(o, env)
			opts.variableNames = vns
			return err
		case atomMaxDepth:
			n, err := writeTermOptionInteger(o, env)
			opts.maxDepth = n
			return err
		}
	}
	return domainError(validDomainWriteOption, option, env)
}

func writeTermOptionBool(o Compound) (bool, error) {
	switch v := env.Resolve(o.Arg(0)).(type) {
	case internal.Variable:
		return false, InstantiationError(env)
	case Atom:
		switch v {
		case atomTrue:
			return true, nil
		case atomFalse:
			return false, nil
		}
	}
	return false, domainError(validDomainWriteOption, o, env)
}

func writeTermOptionVariableNames(option Compound) (map[internal.Variable]Atom, error) {
	vns := map[internal.Variable]Atom{}
	iter := internal.ListIterator{List: option.Arg(0), Env: env}
	for iter.Next() {
		var vn Compound
		switch elem := env.Resolve(iter.Current()).(type) {
		case internal.Variable:
			return nil, InstantiationError(env)
		case Compound:
			if elem.Functor() != atomEqual || elem.Arity() != 2 {
				return nil, domainError(validDomainWriteOption, option, env)
			}
			vn = elem
		default:
			return nil, domainError(validDomainWriteOption, option, env)
		}

		var n Atom
		switch arg := env.Resolve(vn.Arg(0)).(type) {
		case internal.Variable:
			return nil, InstantiationError(env)
		case Atom:
			n = arg
		default:
			return nil, domainError(validDomainWriteOption, option, env)
		}

		var v internal.Variable
		switch arg := env.Resolve(vn.Arg(1)).(type) {
		case internal.Variable:
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
	case internal.Variable:
		return nil, InstantiationError(env)
	case Atom:
		if s != AtomEmptyList {
			return nil, domainError(validDomainWriteOption, option, env)
		}
		return vns, nil
	default:
		return nil, domainError(validDomainWriteOption, option, env)
	}
}

func writeTermOptionInteger(o Compound) (Integer, error) {
	switch v := env.Resolve(o.Arg(0)).(type) {
	case internal.Variable:
		return 0, InstantiationError(env)
	case Integer:
		return v, nil
	}
	return 0, domainError(validDomainWriteOption, o, env)
}

// CharCode converts a single-rune Atom char to an Integer code, or vice versa.
func CharCode(ctx context.Context, char, code Term) Promise {
	switch ch := env.Resolve(char).(type) {
	case internal.Variable:
		switch cd := env.Resolve(code).(type) {
		case internal.Variable:
			return internal.Error(InstantiationError(env))
		case Integer:
			r := rune(cd)

			if !utf8.ValidRune(r) {
				return internal.Error(representationError(flagCharacterCode, env))
			}

			return Unify(vm, ch, Atom(r), k, env)
		default:
			return internal.Error(typeError(validTypeInteger, code, env))
		}
	case Atom:
		switch code := env.Resolve(code).(type) {
		case internal.Variable, Integer:
			break
		default:
			return internal.Error(typeError(validTypeInteger, code, env))
		}

		rs := []rune(ch.String())
		if len(rs) != 1 {
			return internal.Error(typeError(validTypeCharacter, ch, env))
		}

		return Unify(vm, code, Integer(rs[0]), k, env)
	default:
		return internal.Error(typeError(validTypeCharacter, ch, env))
	}
}

// PutByte outputs an integer byte to a stream represented by streamOrAlias.
func PutByte(ctx context.Context, streamOrAlias, byt Term) Promise {
	s, err := stream(vm, streamOrAlias, env)
	if err != nil {
		return internal.Error(err)
	}

	switch b := env.Resolve(byt).(type) {
	case internal.Variable:
		return internal.Error(InstantiationError(env))
	case Integer:
		if 0 > b || 255 < b {
			return internal.Error(typeError(validTypeByte, byt, env))
		}

		switch err := s.WriteByte(byte(b)); {
		case errors.Is(err, internal.errWrongIOMode):
			return internal.Error(permissionError(operationOutput, permissionTypeStream, streamOrAlias, env))
		case errors.Is(err, internal.errWrongStreamType):
			return internal.Error(permissionError(operationOutput, permissionTypeTextStream, streamOrAlias, env))
		case err != nil:
			return internal.Error(err)
		}

		return Continue(ctx)
	default:
		return internal.Error(typeError(validTypeByte, byt, env))
	}
}

// PutChar outputs char to the stream represented by streamOrAlias.
func PutChar(ctx context.Context, streamOrAlias, char Term) Promise {
	s, err := stream(vm, streamOrAlias, env)
	if err != nil {
		return internal.Error(err)
	}

	switch c := env.Resolve(char).(type) {
	case internal.Variable:
		return internal.Error(InstantiationError(env))
	case Atom:
		if c > utf8.MaxRune {
			return internal.Error(typeError(validTypeCharacter, c, env))
		}

		r := rune(c)

		switch _, err := s.WriteRune(r); {
		case errors.Is(err, internal.errWrongIOMode):
			return internal.Error(permissionError(operationOutput, permissionTypeStream, streamOrAlias, env))
		case errors.Is(err, internal.errWrongStreamType):
			return internal.Error(permissionError(operationOutput, permissionTypeBinaryStream, streamOrAlias, env))
		case err != nil:
			return internal.Error(err)
		}

		return Continue(ctx)
	default:
		return internal.Error(typeError(validTypeCharacter, char, env))
	}
}

type readTermOptions struct {
	singletons    Term
	variables     Term
	variableNames Term
}

// ReadTerm reads from the stream represented by streamOrAlias and unifies with stream.
func ReadTerm(ctx context.Context, streamOrAlias, out, options Term) Promise {
	s, err := stream(vm, streamOrAlias, env)
	if err != nil {
		return internal.Error(err)
	}

	opts := readTermOptions{
		singletons:    internal.NewVariable(),
		variables:     internal.NewVariable(),
		variableNames: internal.NewVariable(),
	}
	iter := internal.ListIterator{List: options, Env: env}
	for iter.Next() {
		if err := readTermOption(&opts, iter.Current(), env); err != nil {
			return internal.Error(err)
		}
	}
	if err := iter.Err(); err != nil {
		return internal.Error(err)
	}

	p := internal.NewParser(vm.TypeInModule, s)
	defer func() {
		_ = s.UnreadRune()
	}()

	t, err := p.Term()
	switch {
	case err == nil:
		break
	case errors.Is(err, io.EOF):
		return Unify(vm, out, atomEndOfFile, k, env)
	case errors.Is(err, internal.errWrongIOMode):
		return internal.Error(permissionError(operationInput, permissionTypeStream, streamOrAlias, env))
	case errors.Is(err, internal.errWrongStreamType):
		return internal.Error(permissionError(operationInput, permissionTypeBinaryStream, streamOrAlias, env))
	case errors.Is(err, internal.errPastEndOfStream):
		return internal.Error(permissionError(operationInput, permissionTypePastEndOfStream, streamOrAlias, env))
	default:
		return internal.Error(syntaxError(err, env))
	}

	var singletons, variables, variableNames []Term
	for _, v := range p.Vars {
		if v.Count == 1 {
			singletons = append(singletons, v.Variable)
		}
		variables = append(variables, v.Variable)
		variableNames = append(variableNames, atomEqual.Apply(v.Name, v.Variable))
	}

	return Unify(vm, tuple(
		out,
		opts.singletons,
		opts.variables,
		opts.variableNames,
	), tuple(
		t,
		List(singletons...),
		List(variables...),
		List(variableNames...),
	), k, env)
}

func readTermOption(opts *readTermOptions, option Term) error {
	switch option := env.Resolve(option).(type) {
	case internal.Variable:
		return InstantiationError(env)
	case Compound:
		if option.Arity() != 1 {
			return domainError(validDomainReadOption, option, env)
		}

		v := env.Resolve(option.Arg(0))
		switch option.Functor() {
		case atomSingletons:
			opts.singletons = v
		case atomVariables:
			opts.variables = v
		case atomVariableNames:
			opts.variableNames = v
		default:
			return domainError(validDomainReadOption, option, env)
		}
		return nil
	default:
		return domainError(validDomainReadOption, option, env)
	}
}

// GetByte reads a byte from the stream represented by streamOrAlias and unifies it with inByte.
func GetByte(ctx context.Context, streamOrAlias, inByte Term) Promise {
	s, err := stream(vm, streamOrAlias, env)
	if err != nil {
		return internal.Error(err)
	}

	switch b := env.Resolve(inByte).(type) {
	case internal.Variable:
		break
	case Integer:
		if b < 0 || b > 255 {
			return internal.Error(typeError(validTypeInByte, inByte, env))
		}
	default:
		return internal.Error(typeError(validTypeInByte, inByte, env))
	}

	switch b, err := s.ReadByte(); err {
	case nil:
		return Unify(vm, inByte, Integer(b), k, env)
	case io.EOF:
		return Unify(vm, inByte, Integer(-1), k, env)
	case internal.errWrongIOMode:
		return internal.Error(permissionError(operationInput, permissionTypeStream, streamOrAlias, env))
	case internal.errWrongStreamType:
		return internal.Error(permissionError(operationInput, permissionTypeTextStream, streamOrAlias, env))
	case internal.errPastEndOfStream:
		return internal.Error(permissionError(operationInput, permissionTypePastEndOfStream, streamOrAlias, env))
	default:
		return internal.Error(err)
	}
}

// GetChar reads a character from the stream represented by streamOrAlias and unifies it with char.
func GetChar(ctx context.Context, streamOrAlias, char Term) Promise {
	s, err := stream(vm, streamOrAlias, env)
	if err != nil {
		return internal.Error(err)
	}

	switch c := env.Resolve(char).(type) {
	case internal.Variable:
		break
	case Atom:
		if len([]rune(c.String())) != 1 {
			return internal.Error(typeError(validTypeInCharacter, char, env))
		}
	default:
		return internal.Error(typeError(validTypeInCharacter, char, env))
	}

	switch r, _, err := s.ReadRune(); err {
	case nil:
		if r == utf8.RuneError {
			return internal.Error(representationError(flagCharacter, env))
		}

		return Unify(vm, char, Atom(r), k, env)
	case io.EOF:
		return Unify(vm, char, atomEndOfFile, k, env)
	case internal.errWrongIOMode:
		return internal.Error(permissionError(operationInput, permissionTypeStream, streamOrAlias, env))
	case internal.errWrongStreamType:
		return internal.Error(permissionError(operationInput, permissionTypeBinaryStream, streamOrAlias, env))
	case internal.errPastEndOfStream:
		return internal.Error(permissionError(operationInput, permissionTypePastEndOfStream, streamOrAlias, env))
	default:
		return internal.Error(err)
	}
}

// PeekByte peeks a byte from the stream represented by streamOrAlias and unifies it with inByte.
func PeekByte(ctx context.Context, streamOrAlias, inByte Term) Promise {
	s, err := stream(vm, streamOrAlias, env)
	if err != nil {
		return internal.Error(err)
	}

	switch b := env.Resolve(inByte).(type) {
	case internal.Variable:
		break
	case Integer:
		if b < 0 || b > 255 {
			return internal.Error(typeError(validTypeInByte, inByte, env))
		}
	default:
		return internal.Error(typeError(validTypeInByte, inByte, env))
	}

	b, err := s.ReadByte()
	defer func() {
		_ = s.UnreadByte()
	}()
	switch err {
	case nil:
		return Unify(vm, inByte, Integer(b), k, env)
	case io.EOF:
		return Unify(vm, inByte, Integer(-1), k, env)
	case internal.errWrongIOMode:
		return internal.Error(permissionError(operationInput, permissionTypeStream, streamOrAlias, env))
	case internal.errWrongStreamType:
		return internal.Error(permissionError(operationInput, permissionTypeTextStream, streamOrAlias, env))
	case internal.errPastEndOfStream:
		return internal.Error(permissionError(operationInput, permissionTypePastEndOfStream, streamOrAlias, env))
	default:
		return internal.Error(err)
	}
}

// PeekChar peeks a rune from the stream represented by streamOrAlias and unifies it with char.
func PeekChar(ctx context.Context, streamOrAlias, char Term) Promise {
	s, err := stream(vm, streamOrAlias, env)
	if err != nil {
		return internal.Error(err)
	}

	switch c := env.Resolve(char).(type) {
	case internal.Variable:
		break
	case Atom:
		if len([]rune(c.String())) != 1 {
			return internal.Error(typeError(validTypeInCharacter, char, env))
		}
	default:
		return internal.Error(typeError(validTypeInCharacter, char, env))
	}

	r, _, err := s.ReadRune()
	defer func() {
		_ = s.UnreadRune()
	}()
	switch err {
	case nil:
		if r == unicode.ReplacementChar {
			return internal.Error(representationError(flagCharacter, env))
		}

		return Unify(vm, char, Atom(r), k, env)
	case io.EOF:
		return Unify(vm, char, atomEndOfFile, k, env)
	case internal.errWrongIOMode:
		return internal.Error(permissionError(operationInput, permissionTypeStream, streamOrAlias, env))
	case internal.errWrongStreamType:
		return internal.Error(permissionError(operationInput, permissionTypeBinaryStream, streamOrAlias, env))
	case internal.errPastEndOfStream:
		return internal.Error(permissionError(operationInput, permissionTypePastEndOfStream, streamOrAlias, env))
	default:
		return internal.Error(err)
	}
}

var osExit = os.Exit

// Halt exits the process with exit code of n.
func Halt(ctx context.Context, n Term) Promise {
	switch code := env.Resolve(n).(type) {
	case internal.Variable:
		return internal.Error(InstantiationError(env))
	case Integer:
		osExit(int(code))
		return Continue(ctx)
	default:
		return internal.Error(typeError(validTypeInteger, n, env))
	}
}

// Clause unifies head and body with H and B respectively where H :- B is in the database.
func Clause(ctx context.Context, head, body Term) Promise {
	pi, _, err := piArg(head, env)
	if err != nil {
		return internal.Error(err)
	}

	switch env.Resolve(body).(type) {
	case internal.Variable, Atom, Compound:
		break
	default:
		return internal.Error(typeError(validTypeCallable, body, env))
	}

	m := vm.TypeInModule()
	e, ok := m.procedures[pi]
	if !ok {
		return Failure
	}

	cs, ok := e.procedure.(internal.clauses)
	if !ok || !e.public {
		return internal.Error(permissionError(operationAccess, permissionTypePrivateProcedure, pi.Term(), env))
	}

	ks := make([]func(context.Context) *internal.Promise, len(cs))
	for i, c := range cs {
		cp, err := renamedCopy(c.raw, nil, env)
		if err != nil {
			return internal.Error(err)
		}
		r := rulify(cp, env)
		ks[i] = func(context.Context) Promise {
			return Unify(vm, atomColonMinus.Apply(head, body), r, k, env)
		}
	}
	return Delay(ks...)
}

func rulify(t Term) Term {
	t = env.Resolve(t)
	if c, ok := t.(Compound); ok && c.Functor() == atomColonMinus && c.Arity() == 2 {
		return t
	}
	return atomColonMinus.Apply(t, atomTrue)
}

// AtomLength counts the runes in atom and unifies the result with length.
func AtomLength(ctx context.Context, atom, length Term) Promise {
	var a Atom
	switch atom := env.Resolve(atom).(type) {
	case internal.Variable:
		return internal.Error(InstantiationError(env))
	case Atom:
		a = atom
	default:
		return internal.Error(typeError(validTypeAtom, atom, env))
	}

	switch l := env.Resolve(length).(type) {
	case internal.Variable:
		break
	case Integer:
		if l < 0 {
			return internal.Error(domainError(validDomainNotLessThanZero, length, env))
		}
	default:
		return internal.Error(typeError(validTypeInteger, length, env))
	}

	return Unify(vm, length, Integer(len([]rune(a.String()))), k, env)
}

// AtomConcat concatenates atom1 and atom2 and unifies it with atom3.
func AtomConcat(ctx context.Context, atom1, atom2, atom3 Term) Promise {
	switch a3 := env.Resolve(atom3).(type) {
	case internal.Variable:
		switch a1 := env.Resolve(atom1).(type) {
		case internal.Variable:
			return internal.Error(InstantiationError(env))
		case Atom:
			switch a2 := env.Resolve(atom2).(type) {
			case internal.Variable:
				return internal.Error(InstantiationError(env))
			case Atom:
				return Delay(func(context.Context) Promise {
					return Unify(vm, a3, NewAtom(a1.String()+a2.String()), k, env)
				})
			default:
				return internal.Error(typeError(validTypeAtom, atom2, env))
			}
		default:
			return internal.Error(typeError(validTypeAtom, atom1, env))
		}
	case Atom:
		switch env.Resolve(atom1).(type) {
		case internal.Variable, Atom:
			break
		default:
			return internal.Error(typeError(validTypeAtom, atom1, env))
		}

		switch env.Resolve(atom2).(type) {
		case internal.Variable, Atom:
			break
		default:
			return internal.Error(typeError(validTypeAtom, atom2, env))
		}

		pattern := tuple(atom1, atom2)
		s := a3.String()
		ks := make([]func(context.Context) *internal.Promise, 0, len(s)+1)
		for i := range s {
			a1, a2 := s[:i], s[i:]
			ks = append(ks, func(context.Context) Promise {
				return Unify(vm, pattern, tuple(NewAtom(a1), NewAtom(a2)), k, env)
			})
		}
		ks = append(ks, func(context.Context) Promise {
			return Unify(vm, pattern, tuple(a3, atomEmpty), k, env)
		})
		return Delay(ks...)
	default:
		return internal.Error(typeError(validTypeAtom, atom3, env))
	}
}

// SubAtom unifies subAtom with a sub atom of length which appears with before runes preceding it and after runes following it.
func SubAtom(ctx context.Context, atom, before, length, after, subAtom Term) Promise {
	switch whole := env.Resolve(atom).(type) {
	case internal.Variable:
		return internal.Error(InstantiationError(env))
	case Atom:
		rs := []rune(whole.String())

		if err := checkPositiveInteger(before, env); err != nil {
			return internal.Error(err)
		}

		if err := checkPositiveInteger(length, env); err != nil {
			return internal.Error(err)
		}

		if err := checkPositiveInteger(after, env); err != nil {
			return internal.Error(err)
		}

		switch env.Resolve(subAtom).(type) {
		case internal.Variable, Atom:
			break
		default:
			return internal.Error(typeError(validTypeAtom, subAtom, env))
		}

		pattern := tuple(before, length, after, subAtom)
		var ks []func(context.Context) *internal.Promise
		for i := 0; i <= len(rs); i++ {
			for j := i; j <= len(rs); j++ {
				before, length, after, subAtom := Integer(i), Integer(j-i), Integer(len(rs)-j), NewAtom(string(rs[i:j]))
				ks = append(ks, func(context.Context) Promise {
					return Unify(vm, pattern, tuple(before, length, after, subAtom), k, env)
				})
			}
		}
		return Delay(ks...)
	default:
		return internal.Error(typeError(validTypeAtom, atom, env))
	}
}

func checkPositiveInteger(n Term) error {
	switch b := env.Resolve(n).(type) {
	case internal.Variable:
		return nil
	case Integer:
		if b < 0 {
			return domainError(validDomainNotLessThanZero, n, env)
		}
		return nil
	default:
		return typeError(validTypeInteger, n, env)
	}
}

// AtomChars breaks down atom into list of characters and unifies with chars, or constructs an atom from a list of
// characters chars and unifies it with atom.
func AtomChars(ctx context.Context, atom, chars Term) Promise {
	switch a := env.Resolve(atom).(type) {
	case internal.Variable:
		var sb strings.Builder
		iter := internal.ListIterator{List: chars, Env: env}
		for iter.Next() {
			switch e := env.Resolve(iter.Current()).(type) {
			case internal.Variable:
				return internal.Error(InstantiationError(env))
			case Atom:
				if len([]rune(e.String())) != 1 {
					return internal.Error(typeError(validTypeCharacter, e, env))
				}
				_, _ = sb.WriteString(e.String())
			default:
				return internal.Error(typeError(validTypeCharacter, e, env))
			}
		}
		if err := iter.Err(); err != nil {
			return internal.Error(err)
		}
		return Unify(vm, atom, NewAtom(sb.String()), k, env)
	case Atom:
		iter := internal.ListIterator{List: chars, Env: env, AllowPartial: true}
		for iter.Next() {
			switch e := env.Resolve(iter.Current()).(type) {
			case internal.Variable:
				break
			case Atom:
				if len([]rune(e.String())) != 1 {
					return internal.Error(typeError(validTypeCharacter, e, env))
				}
			default:
				return internal.Error(typeError(validTypeCharacter, e, env))
			}
		}
		if err := iter.Err(); err != nil {
			return internal.Error(err)
		}

		s := a.String()
		if s == "" {
			return Unify(vm, chars, AtomEmptyList, k, env)
		}
		return Unify(vm, chars, charList(s), k, env)
	default:
		return internal.Error(typeError(validTypeAtom, a, env))
	}
}

// AtomCodes breaks up atom into a list of runes and unifies it with codes, or constructs an atom from the list of runes
// and unifies it with atom.
func AtomCodes(ctx context.Context, atom, codes Term) Promise {
	switch a := env.Resolve(atom).(type) {
	case internal.Variable:
		var sb strings.Builder
		iter := internal.ListIterator{List: codes, Env: env}
		for iter.Next() {
			switch e := env.Resolve(iter.Current()).(type) {
			case internal.Variable:
				return internal.Error(InstantiationError(env))
			case Integer:
				if e < 0 || e > unicode.MaxRune {
					return internal.Error(representationError(flagCharacterCode, env))
				}
				_, _ = sb.WriteRune(rune(e))
			default:
				return internal.Error(typeError(validTypeInteger, e, env))
			}
		}
		if err := iter.Err(); err != nil {
			return internal.Error(err)
		}
		return Unify(vm, atom, NewAtom(sb.String()), k, env)
	case Atom:
		iter := internal.ListIterator{List: codes, Env: env, AllowPartial: true}
		for iter.Next() {
			switch e := env.Resolve(iter.Current()).(type) {
			case internal.Variable:
				break
			case Integer:
				if e < 0 || e > unicode.MaxRune {
					return internal.Error(representationError(flagCharacterCode, env))
				}
			default:
				return internal.Error(typeError(validTypeInteger, e, env))
			}
		}
		if err := iter.Err(); err != nil {
			return internal.Error(err)
		}

		s := a.String()
		if s == "" {
			return Unify(vm, codes, AtomEmptyList, k, env)
		}
		return Unify(vm, codes, codeList(s), k, env)
	default:
		return internal.Error(typeError(validTypeAtom, atom, env))
	}
}

// NumberChars breaks up an atom representation of a number num into a list of characters and unifies it with chars, or
// constructs a number from a list of characters chars and unifies it with num.
func NumberChars(ctx context.Context, num, chars Term) Promise {
	var sb strings.Builder
	iter := internal.ListIterator{List: chars, Env: env, AllowPartial: true}
	for iter.Next() {
		switch e := env.Resolve(iter.Current()).(type) {
		case internal.Variable:
			return numberCharsWrite(vm, num, chars, k, env)
		case Atom:
			s := e.String()
			if len([]rune(s)) != 1 {
				return internal.Error(typeError(validTypeCharacter, e, env))
			}
			_, _ = sb.WriteString(s)
		default:
			return internal.Error(typeError(validTypeCharacter, e, env))
		}
	}
	if err := iter.Err(); err != nil {
		return internal.Error(err)
	}
	if _, ok := iter.Suffix().(internal.Variable); ok {
		return numberCharsWrite(vm, num, chars, k, env)
	}

	p := internal.Parser{
		Lexer: internal.Lexer{
			input: internal.newRuneRingBuffer(strings.NewReader(sb.String())),
		},
	}
	t, err := p.number()
	if err != nil {
		return internal.Error(syntaxError(err, env))
	}

	switch n := env.Resolve(num).(type) {
	case internal.Variable, Number:
		return Unify(vm, n, t, k, env)
	default:
		return internal.Error(typeError(validTypeNumber, n, env))
	}
}

func numberCharsWrite(ctx context.Context, num, chars Term) Promise {
	var n Number
	switch num := env.Resolve(num).(type) {
	case internal.Variable:
		return internal.Error(InstantiationError(env))
	case Number:
		n = num
	default:
		return internal.Error(typeError(validTypeNumber, num, env))
	}

	iter := internal.ListIterator{List: chars, Env: env, AllowPartial: true}
	for iter.Next() {
		switch e := env.Resolve(iter.Current()).(type) {
		case internal.Variable:
			break
		case Atom:
			if len(e.String()) != 1 {
				return internal.Error(typeError(validTypeCharacter, e, env))
			}
		default:
			return internal.Error(typeError(validTypeCharacter, e, env))
		}
	}
	if err := iter.Err(); err != nil {
		return internal.Error(err)
	}

	var buf bytes.Buffer
	_ = n.WriteTerm(&buf, &internal.defaultWriteOptions, nil)
	rs := []rune(buf.String())

	cs := make([]Term, len(rs))
	for i, r := range rs {
		cs[i] = Atom(r)
	}
	return Unify(vm, chars, List(cs...), k, env)
}

// NumberCodes breaks up an atom representation of a number num into a list of runes and unifies it with codes, or
// constructs a number from a list of runes codes and unifies it with num.
func NumberCodes(ctx context.Context, num, codes Term) Promise {
	var sb strings.Builder
	iter := internal.ListIterator{List: codes, Env: env, AllowPartial: true}
	for iter.Next() {
		switch e := env.Resolve(iter.Current()).(type) {
		case internal.Variable:
			return numberCodesWrite(vm, num, codes, k, env)
		case Integer:
			if !utf8.ValidRune(rune(e)) {
				return internal.Error(representationError(flagCharacterCode, env))
			}
			_, _ = sb.WriteRune(rune(e))
		default:
			return internal.Error(typeError(validTypeInteger, e, env))
		}
	}
	if err := iter.Err(); err != nil {
		return internal.Error(err)
	}
	if _, ok := iter.Suffix().(internal.Variable); ok {
		return numberCodesWrite(vm, num, codes, k, env)
	}

	p := internal.Parser{
		Lexer: internal.Lexer{
			input: internal.newRuneRingBuffer(strings.NewReader(sb.String())),
		},
	}
	t, err := p.number()
	if err != nil {
		return internal.Error(syntaxError(err, env))
	}

	switch n := env.Resolve(num).(type) {
	case internal.Variable, Number:
		return Unify(vm, n, t, k, env)
	default:
		return internal.Error(typeError(validTypeNumber, n, env))
	}
}

func numberCodesWrite(ctx context.Context, num, codes Term) Promise {
	var n Number
	switch num := env.Resolve(num).(type) {
	case internal.Variable:
		return internal.Error(InstantiationError(env))
	case Number:
		n = num
	default:
		return internal.Error(typeError(validTypeNumber, num, env))
	}

	iter := internal.ListIterator{List: codes, Env: env, AllowPartial: true}
	for iter.Next() {
		switch e := env.Resolve(iter.Current()).(type) {
		case internal.Variable:
			break
		case Integer:
			if !utf8.ValidRune(rune(e)) {
				return internal.Error(representationError(flagCharacterCode, env))
			}
		default:
			return internal.Error(typeError(validTypeInteger, e, env))
		}
	}
	if err := iter.Err(); err != nil {
		return internal.Error(err)
	}

	var buf bytes.Buffer
	_ = n.WriteTerm(&buf, &internal.defaultWriteOptions, nil)
	rs := []rune(buf.String())

	cs := make([]Term, len(rs))
	for i, r := range rs {
		cs[i] = Integer(r)
	}
	return Unify(vm, codes, List(cs...), k, env)
}

// StreamProperty succeeds iff the stream represented by stream has the stream property.
func StreamProperty(ctx context.Context, stream, property Term) Promise {
	streams := make([]*internal.Stream, 0, len(vm.streams.elems))
	switch s := env.Resolve(stream).(type) {
	case internal.Variable:
		for _, v := range vm.streams.elems {
			streams = append(streams, v)
		}
	case *internal.Stream:
		streams = append(streams, s)
	default:
		return internal.Error(domainError(validDomainStream, stream, env))
	}

	if !isStreamProperty(property, env) {
		return internal.Error(domainError(validDomainStreamProperty, property, env))
	}

	var ks []func(context.Context) *internal.Promise
	for _, s := range streams {
		s := s
		for _, p := range s.properties() {
			p := p
			ks = append(ks, func(context.Context) Promise {
				return Unify(vm, atomEmpty.Apply(stream, property), atomEmpty.Apply(s, p), k, env)
			})
		}
	}
	return Delay(ks...)
}

func isStreamProperty(property Term) bool {
	switch p := env.Resolve(property).(type) {
	case internal.Variable:
		return true
	case Atom:
		return p == atomInput || p == atomOutput
	case Compound:
		if p.Arity() != 1 {
			return false
		}
		arg := p.Arg(0)
		switch p.Functor() {
		case atomFileName, atomMode, atomAlias, atomEndOfStream, atomEOFAction, atomReposition:
			return isAtom(arg, env)
		case atomPosition:
			return isInteger(arg, env)
		}
		return false
	default:
		return false
	}
}

func isAtom(t Term) bool {
	switch env.Resolve(t).(type) {
	case internal.Variable, Atom:
		return true
	default:
		return false
	}
}

func isInteger(t Term) bool {
	switch env.Resolve(t).(type) {
	case internal.Variable, Integer:
		return true
	default:
		return false
	}
}

// SetStreamPosition sets the position property of the stream represented by streamOrAlias.
func SetStreamPosition(ctx context.Context, streamOrAlias, position Term) Promise {
	s, err := stream(vm, streamOrAlias, env)
	if err != nil {
		return internal.Error(err)
	}

	switch p := env.Resolve(position).(type) {
	case internal.Variable:
		return internal.Error(InstantiationError(env))
	case Integer:
		switch _, err := s.Seek(int64(p), 0); err {
		case nil:
			return Continue(ctx)
		case internal.errReposition:
			return internal.Error(permissionError(operationReposition, permissionTypeStream, streamOrAlias, env))
		default:
			return internal.Error(err)
		}
	default:
		return internal.Error(typeError(validTypeInteger, position, env))
	}
}

// CharConversion registers a character conversion from inChar to outChar, or remove the conversion if inChar = outChar.
func CharConversion(ctx context.Context, inChar, outChar Term) Promise {
	switch in := env.Resolve(inChar).(type) {
	case internal.Variable:
		return internal.Error(InstantiationError(env))
	case Atom:
		i := []rune(in.String())
		if len(i) != 1 {
			return internal.Error(representationError(flagCharacter, env))
		}

		switch out := env.Resolve(outChar).(type) {
		case internal.Variable:
			return internal.Error(InstantiationError(env))
		case Atom:
			o := []rune(out.String())
			if len(o) != 1 {
				return internal.Error(representationError(flagCharacter, env))
			}

			m := vm.TypeInModule()
			if m.charConversions == nil {
				m.charConversions = map[rune]rune{}
			}
			if i[0] == o[0] {
				delete(m.charConversions, i[0])
				return Continue(ctx)
			}
			m.charConversions[i[0]] = o[0]
			return Continue(ctx)
		default:
			return internal.Error(representationError(flagCharacter, env))
		}
	default:
		return internal.Error(representationError(flagCharacter, env))
	}
}

// CurrentCharConversion succeeds iff a conversion from inChar to outChar is defined.
func CurrentCharConversion(ctx context.Context, inChar, outChar Term) Promise {
	switch in := env.Resolve(inChar).(type) {
	case internal.Variable:
		break
	case Atom:
		i := []rune(in.String())
		if len(i) != 1 {
			return internal.Error(representationError(flagCharacter, env))
		}
	default:
		return internal.Error(representationError(flagCharacter, env))
	}

	switch out := env.Resolve(outChar).(type) {
	case internal.Variable:
		break
	case Atom:
		o := []rune(out.String())
		if len(o) != 1 {
			return internal.Error(representationError(flagCharacter, env))
		}
	default:
		return internal.Error(representationError(flagCharacter, env))
	}

	m := vm.TypeInModule()
	if c1, ok := env.Resolve(inChar).(Atom); ok {
		r := []rune(c1.String())
		if r, ok := m.charConversions[r[0]]; ok {
			return Unify(vm, outChar, Atom(r), k, env)
		}
		return Unify(vm, outChar, c1, k, env)
	}

	pattern := tuple(inChar, outChar)
	ks := make([]func(context.Context) *internal.Promise, 256)
	for i := 0; i < 256; i++ {
		r := rune(i)
		cr, ok := m.charConversions[r]
		if !ok {
			cr = r
		}

		ks[i] = func(context.Context) Promise {
			return Unify(vm, pattern, tuple(Atom(r), Atom(cr)), k, env)
		}
	}
	return Delay(ks...)
}

// SetPrologFlag sets flag to value.
func SetPrologFlag(ctx context.Context, flag, value Term) Promise {
	switch f := env.Resolve(flag).(type) {
	case internal.Variable:
		return internal.Error(InstantiationError(env))
	case Atom:
		m := vm.TypeInModule()
		var modify func(m *internal.module, value Atom) error
		switch f {
		case atomBounded, atomMaxInteger, atomMinInteger, atomIntegerRoundingFunction, atomMaxArity:
			return internal.Error(permissionError(operationModify, permissionTypeFlag, f, env))
		case atomCharConversion:
			modify = modifyCharConversion
		case atomDebug:
			modify = modifyDebug
		case atomUnknown:
			modify = modifyUnknown
		case atomDoubleQuotes:
			modify = modifyDoubleQuotes
		default:
			return internal.Error(domainError(validDomainPrologFlag, f, env))
		}

		switch v := env.Resolve(value).(type) {
		case internal.Variable:
			return internal.Error(InstantiationError(env))
		case Atom:
			if err := modify(m, v); err != nil {
				return internal.Error(err)
			}
			return Continue(ctx)
		default:
			return internal.Error(domainError(validDomainFlagValue, atomPlus.Apply(flag, value), env))
		}
	default:
		return internal.Error(typeError(validTypeAtom, f, env))
	}
}

func modifyCharConversion(m *internal.module, value Atom) error {
	switch value {
	case atomOn:
		m.charConvEnabled = true
	case atomOff:
		m.charConvEnabled = false
	default:
		return domainError(validDomainFlagValue, atomPlus.Apply(atomCharConversion, value), nil)
	}
	return nil
}

func modifyDebug(m *internal.module, value Atom) error {
	switch value {
	case atomOn:
		m.debug = true
	case atomOff:
		m.debug = false
	default:
		return domainError(validDomainFlagValue, atomPlus.Apply(atomDebug, value), nil)
	}
	return nil
}

func modifyUnknown(m *internal.module, value Atom) error {
	switch value {
	case atomError:
		m.unknown = internal.unknownError
	case atomWarning:
		m.unknown = internal.unknownWarning
	case atomFail:
		m.unknown = internal.unknownFail
	default:
		return domainError(validDomainFlagValue, atomPlus.Apply(atomUnknown, value), nil)
	}
	return nil
}

func modifyDoubleQuotes(m *internal.module, value Atom) error {
	switch value {
	case atomCodes:
		m.doubleQuotes = internal.doubleQuotesCodes
	case atomChars:
		m.doubleQuotes = internal.doubleQuotesChars
	case atomAtom:
		m.doubleQuotes = internal.doubleQuotesAtom
	default:
		return domainError(validDomainFlagValue, atomPlus.Apply(atomDoubleQuotes, value), nil)
	}
	return nil
}

// CurrentPrologFlag succeeds iff flag is set to value.
func CurrentPrologFlag(ctx context.Context, flag, value Term) Promise {
	switch f := env.Resolve(flag).(type) {
	case internal.Variable:
		break
	case Atom:
		switch f {
		case atomBounded, atomMaxInteger, atomMinInteger, atomIntegerRoundingFunction, atomCharConversion, atomDebug, atomMaxArity, atomUnknown, atomDoubleQuotes:
			break
		default:
			return internal.Error(domainError(validDomainPrologFlag, f, env))
		}
	default:
		return internal.Error(typeError(validTypeAtom, f, env))
	}

	m := vm.TypeInModule()
	pattern := tuple(flag, value)
	flags := []Term{
		tuple(atomBounded, atomTrue),
		tuple(atomMaxInteger, internal.maxInt),
		tuple(atomMinInteger, internal.minInt),
		tuple(atomIntegerRoundingFunction, atomTowardZero),
		tuple(atomCharConversion, onOff(m.charConvEnabled)),
		tuple(atomDebug, onOff(m.debug)),
		tuple(atomMaxArity, atomUnbounded),
		tuple(atomUnknown, NewAtom(m.unknown.String())),
		tuple(atomDoubleQuotes, NewAtom(m.doubleQuotes.String())),
	}
	ks := make([]func(context.Context) *internal.Promise, len(flags))
	for i := range flags {
		f := flags[i]
		ks[i] = func(context.Context) Promise {
			return Unify(vm, pattern, f, k, env)
		}
	}
	return Delay(ks...)
}

func onOff(b bool) Atom {
	if b {
		return atomOn
	}
	return atomOff
}

// ExpandTerm transforms term1 according to term_expansion/2 and DCG rules then unifies with term2.
func ExpandTerm(ctx context.Context, term1, term2 Term) Promise {
	t, err := expand(vm, term1, env)
	if err != nil {
		return internal.Error(err)
	}

	return Unify(vm, t, term2, k, env)
}

// Nth0 succeeds if elem is the n-th element of list, counting from 0.
func Nth0(ctx context.Context, n, list, elem Term) Promise {
	return nth(vm, 0, n, list, elem, k, env)
}

// Nth1 succeeds if elem is the n-th element of list, counting from 1.
func Nth1(ctx context.Context, n, list, elem Term) Promise {
	return nth(vm, 1, n, list, elem, k, env)
}

func nth(ctx context.Context, base Integer, n, list, elem Term) Promise {
	switch n := env.Resolve(n).(type) {
	case internal.Variable:
		var ks []func(context.Context) *internal.Promise
		iter := internal.ListIterator{List: list, Env: env}
		for i := base; iter.Next(); i++ {
			i, e := i, iter.Current()
			ks = append(ks, func(context.Context) Promise {
				return Unify(vm, tuple(n, elem), tuple(i, e), k, env)
			})
		}
		if err := iter.Err(); err != nil {
			return internal.Error(err)
		}
		return Delay(ks...)
	case Integer:
		if n < base {
			return Failure
		}
		iter := internal.ListIterator{List: list, Env: env, AllowCycle: true}
		for i := base; iter.Next(); i++ {
			if i == n {
				return Unify(vm, elem, iter.Current(), k, env)
			}
		}
		if err := iter.Err(); err != nil {
			return internal.Error(err)
		}
		return Failure
	default:
		return internal.Error(typeError(validTypeInteger, n, env))
	}
}

// Succ succeeds if s is the successor of non-negative integer x.
func Succ(ctx context.Context, x, s Term) Promise {
	switch x := env.Resolve(x).(type) {
	case internal.Variable:
		switch s := env.Resolve(s).(type) {
		case internal.Variable:
			return internal.Error(InstantiationError(env))
		case Integer:
			switch {
			case s < Integer(0):
				return internal.Error(domainError(validDomainNotLessThanZero, s, env))
			case s == Integer(0):
				return Failure
			default:
				return Unify(vm, x, s-Integer(1), k, env)
			}
		default:
			return internal.Error(typeError(validTypeInteger, s, env))
		}
	case Integer:
		if x < Integer(0) {
			return internal.Error(domainError(validDomainNotLessThanZero, x, env))
		}

		r, err := internal.add(x, Integer(1))
		if err != nil {
			var ev exceptionalValue
			if errors.As(err, &ev) {
				return internal.Error(evaluationError(ev, env))
			}
			return internal.Error(err)
		}

		switch s := env.Resolve(s).(type) {
		case internal.Variable:
			return Unify(vm, s, r, k, env)
		case Integer:
			if s < Integer(0) {
				return internal.Error(domainError(validDomainNotLessThanZero, s, env))
			}
			return Unify(vm, s, r, k, env)
		default:
			return internal.Error(typeError(validTypeInteger, s, env))
		}
	default:
		return internal.Error(typeError(validTypeInteger, x, env))
	}
}

// Length succeeds iff list is a list of length.
func Length(ctx context.Context, list, length Term) Promise {
	// https://github.com/mthom/scryer-prolog/issues/1325#issue-1160713156
	// Note that it's a bit simpler since we don't have attributed variables (yet).

	n := env.Resolve(length)
	switch n := n.(type) {
	case internal.Variable:
		break
	case Integer:
		if n < 0 {
			return internal.Error(domainError(validDomainNotLessThanZero, n, env))
		}
	default:
		return internal.Error(typeError(validTypeInteger, n, env))
	}

	var (
		skipped = internal.NewVariable()
		suffix  = internal.NewVariable()
	)
	return SkipMaxList(vm, skipped, n, list, suffix, func(env *internal.Env) Promise {
		skipped := env.Resolve(skipped).(Integer)

		switch suffix := env.Resolve(suffix).(type) {
		case internal.Variable: // partial list
			if n, ok := n.(Integer); ok {
				return lengthRundown(vm, suffix, n-skipped, k, env)
			}

			n := n.(internal.Variable)

			if n == suffix {
				return internal.Error(resourceError(resourceFiniteMemory, env))
			}

			return lengthAddendum(vm, AtomEmptyList, skipped, suffix, n, k, env)
		case Atom: // list or non-list terminated by an atom
			if suffix != AtomEmptyList {
				return Failure
			}

			return Unify(vm, n, skipped, k, env)
		case Compound: // non-list including infinite list
			if suffix.Functor() != atomDot || suffix.Arity() != 2 {
				return Failure
			}

			if _, ok := n.(internal.Variable); !ok {
				return Failure
			}

			return internal.Error(resourceError(resourceFiniteMemory, env))
		default: // non-list terminated by a term that is neither an atom nor a compound
			return Failure
		}
	}, env)
}

func lengthRundown(ctx context.Context, list internal.Variable, n Integer) Promise {
	elems, err := makeSlice(int(n))
	if err != nil {
		return internal.Error(resourceError(resourceMemory, env))
	}
	for i := range elems {
		elems[i] = internal.NewVariable()
	}
	return Unify(vm, list, List(elems...), k, env)
}

func lengthAddendum(ctx context.Context, suffix Term, offset Integer, list, length internal.Variable) Promise {
	return Delay(func(context.Context) Promise {
		return Unify(vm, tuple(list, length), tuple(suffix, offset), k, env)
	}, func(context.Context) Promise {
		suffix := atomDot.Apply(internal.NewVariable(), suffix)
		offset, err := internal.addI(offset, 1)
		if err != nil {
			return internal.Error(representationError(flagMaxInteger, env))
		}
		return lengthAddendum(vm, suffix, offset, list, length, k, env)
	})
}

// SkipMaxList iterates over list up to max elements and unifies the number of skipped elements with skip and the rest with suffix.
func SkipMaxList(ctx context.Context, skip, max, list, suffix Term) Promise {
	m := internal.maxInt
	switch max := env.Resolve(max).(type) {
	case internal.Variable:
		break
	case Integer:
		if max < 0 {
			return internal.Error(domainError(validDomainNotLessThanZero, max, env))
		}
		m = max
	default:
		return internal.Error(typeError(validTypeInteger, max, env))
	}

	var (
		iter = internal.ListIterator{List: list, Env: env}
		n    = Integer(0)
	)
	for n < m && iter.Next() {
		n++
	}

	return Unify(vm, tuple(skip, suffix), tuple(n, iter.Suffix()), k, env)
}

// Append succeeds iff zs is the concatenation of lists xs and ys.
func Append(ctx context.Context, xs, ys, zs Term) Promise {
	// A special case for non-empty lists without a variable in the spine.
	if xs, ok := env.Resolve(xs).(Compound); ok {
		iter := internal.ListIterator{List: xs, Env: nil} // No variables allowed.
		for iter.Next() {
		}
		if err := iter.Err(); err == nil {
			return Unify(vm, zs, &partial{
				Compound: xs,
				tail:     &ys,
			}, k, env)
		}
	}

	return appendLists(vm, xs, ys, zs, k, env)
}

func appendLists(ctx context.Context, xs, ys, zs Term) Promise {
	/*
		append([], L, L).
		append([X|L1], L2, [X|L3]) :- append(L1, L2, L3).
	*/
	return Delay(func(context.Context) Promise {
		return Unify(vm, tuple(xs, ys), tuple(List(), zs), k, env)
	}, func(context.Context) Promise {
		x := internal.NewVariable()
		l1, l3 := internal.NewVariable(), internal.NewVariable()
		return Unify(vm, tuple(xs, zs), tuple(Cons(x, l1), Cons(x, l3)), func(env *internal.Env) Promise {
			return appendLists(vm, l1, ys, l3, k, env)
		}, env)
	})
}

func Dynamic(ctx context.Context, pi Term) Promise {
	m := vm.TypeInModule()
	iter := internal.anyIterator{Any: pi, Env: env}
	for iter.Next() {
		pi, err := mustBePI(iter.Current(), env)
		if err != nil {
			return internal.Error(err)
		}
		e, _ := m.procedures[pi]
		e.dynamic = true
		e.public = true
		if e.procedure == nil {
			e.procedure = internal.clauses{}
		}
		m.procedures[pi] = e
	}
	if err := iter.Err(); err != nil {
		return internal.Error(err)
	}
	return Continue(ctx)
}

func Multifile(ctx context.Context, pi Term) Promise {
	m := vm.TypeInModule()
	iter := internal.anyIterator{Any: pi, Env: env}
	for iter.Next() {
		pi, err := mustBePI(iter.Current(), env)
		if err != nil {
			return internal.Error(err)
		}
		e, _ := m.procedures[pi]
		e.multifile = true
		m.procedures[pi] = e
	}
	if err := iter.Err(); err != nil {
		return internal.Error(err)
	}
	return Continue(ctx)
}

func Discontiguous(ctx context.Context, pi Term) Promise {
	m := vm.TypeInModule()
	iter := internal.anyIterator{Any: pi, Env: env}
	for iter.Next() {
		pi, err := mustBePI(iter.Current(), env)
		if err != nil {
			return internal.Error(err)
		}
		e, _ := m.procedures[pi]
		e.discontiguous = true
		m.procedures[pi] = e
	}
	if err := iter.Err(); err != nil {
		return internal.Error(err)
	}
	return Continue(ctx)
}

func Initialization(ctx context.Context, goal Term) Promise {
	m := vm.TypeInModule()
	m.initGoals = append(m.initGoals, goal)
	return Continue(ctx)
}

func Include(ctx context.Context, file Term) Promise {
	f, err := mustBeAtom(file, env)
	if err != nil {
		return internal.Error(err)
	}
	return Delay(func(ctx context.Context) Promise {
		if err := vm.LoadFile(ctx, f.String()); err != nil {
			return internal.Error(err)
		}
		return Continue(ctx)
	})
}

// LoadFile loads a Prolog text from a file.
func LoadFile(ctx context.Context, path, options Term) Promise {
	filename, err := mustBeAtom(path, env)
	if err != nil {
		return internal.Error(err)
	}
	fn := filename.String()

	var (
		condition  internal.LoadFileCondition
		importList []internal.ImportSpec
	)
	iter := internal.ListIterator{List: options, Env: env}
	for iter.Next() {
		opt := iter.Current()

		if _, ok := env.Unify(opt, atomIf.Apply(atomTrue)); ok {
			condition = internal.LoadFileConditionTrue
			break
		}

		if _, ok := env.Unify(opt, atomIf.Apply(atomChanged)); ok {
			condition = internal.LoadFileConditionChanged
			break
		}

		if _, ok := env.Unify(opt, atomImports.Apply(atomAll)); ok {
			importList = nil
			break
		}

		imports := internal.NewVariable()
		if env, ok := env.Unify(opt, atomImports.Apply(imports)); ok {
			importList = []internal.ImportSpec{}
			iter := internal.ListIterator{List: imports, Env: env}
			for iter.Next() {
				i := iter.Current()

				pi, err := mustBePI(i, env)
				if err != nil {
					return internal.Error(err)
				}
				importList = append(importList, internal.ImportSpec{
					Name:  pi.name.String(),
					Arity: int(pi.arity),
				})
			}
			if err := iter.Err(); err != nil {
				return internal.Error(err)
			}
		}
	}

	switch err := vm.LoadFile(context.Background(), fn,
		internal.LoadFileOptionIf(condition),
		internal.LoadFileOptionImports(importList),
	); {
	case err == nil:
		break
	case errors.Is(err, fs.ErrInvalid):
		fallthrough
	case errors.Is(err, fs.ErrNotExist):
		return internal.Error(existenceError(objectTypeSourceSink, path, env))
	default:
		return internal.Error(err)
	}

	return Continue(ctx)
}

func DefineModule(ctx context.Context, moduleName, exportList Term) Promise {
	var name Atom
	switch n := env.Resolve(moduleName).(type) {
	case internal.Variable:
		return internal.Error(InstantiationError(env))
	case Atom:
		name = n
	default:
		return internal.Error(typeError(validTypeAtom, moduleName, env))
	}

	var pis []predicateIndicator
	iter := internal.ListIterator{List: exportList, Env: env}
	for iter.Next() {
		pi, err := mustBePI(iter.Current(), env)
		if err != nil {
			return internal.Error(err)
		}
		pis = append(pis, pi)
	}
	if err := iter.Err(); err != nil {
		return internal.Error(err)
	}

	m := vm.module(name)
	m.reset()

	vm.importPredicates(name, vm.system, nil)
	vm.SetModule(name)

	return Continue(ctx)
}

func MetaPredicate(ctx context.Context, mi Term) Promise {
	m := vm.TypeInModule()
	iter := internal.anyIterator{Any: mi, Env: env}
	for iter.Next() {
		pi, arg, err := piArg(mi, env)
		if err != nil {
			return internal.Error(err)
		}
		e, _ := m.procedures[pi]
		e.metaPredicate = make([]internal.metaArgumentSpecifier, pi.arity)
		for i := 0; i < int(pi.arity); i++ {
			switch t := env.Resolve(arg(i)).(type) {
			case internal.Variable:
				return internal.Error(InstantiationError(env))
			case Atom:
				e.metaPredicate[i] = internal.metaArgumentSpecifier{atom: t}
			case Integer:
				e.metaPredicate[i] = internal.metaArgumentSpecifier{integer: t}
			default:
				return internal.Error(domainError(validDomainMetaArgumentSpecifier, t, env))
			}
		}
		m.procedures[pi] = e
	}
	if err := iter.Err(); err != nil {
		return internal.Error(err)
	}
	return Continue(ctx)
}
