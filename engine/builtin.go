package engine

import (
	"bytes"
	"context"
	"errors"
	"io"
	"io/fs"
	"os"
	"sort"
	"strings"
	"unicode"
	"unicode/utf8"
)

// Repeat repeats the continuation until it succeeds.
func Repeat(ctx context.Context) *Promise {
	return repeat(func() *Promise {
		return Continue(ctx)
	})
}

// Negate calls goal and returns false if it succeeds. Otherwise, invokes the continuation.
func Negate(ctx context.Context, goal Term) *Promise {
	ok, err := Call(ctx, goal).Force()
	if err != nil {
		return Error(err)
	}
	if ok {
		return Bool(false)
	}
	return Continue(ctx)
}

// Call executes goal. it succeeds if goal followed by k succeeds. A cut inside goal doesn't affect outside of Call.
func Call(ctx context.Context, goal Term) *Promise {
	switch g := Resolve(ctx, goal).(type) {
	case Variable:
		return Error(InstantiationError(ctx))
	default:
		env := env(ctx)
		fvs := env.freeVariables(g)
		args, err := makeSlice[Term](len(fvs))
		if err != nil {
			return Error(resourceError(ctx, resourceMemory))
		}
		for i, fv := range fvs {
			args[i] = fv
		}
		cs, err := compile(ctx, atomIf.Apply(tuple(args...), g))
		if err != nil {
			return Error(err)
		}

		u := userDefined{clauses: cs}
		return u.call(ctx, args)
	}
}

// Call1 succeeds if closure with an additional argument succeeds.
func Call1(ctx context.Context, closure, arg1 Term) *Promise {
	return callN(ctx, closure, []Term{arg1})
}

// Call2 succeeds if closure with 2 additional arguments succeeds.
func Call2(ctx context.Context, closure, arg1, arg2 Term) *Promise {
	return callN(ctx, closure, []Term{arg1, arg2})
}

// Call3 succeeds if closure with 3 additional arguments succeeds.
func Call3(ctx context.Context, closure, arg1, arg2, arg3 Term) *Promise {
	return callN(ctx, closure, []Term{arg1, arg2, arg3})
}

// Call4 succeeds if closure with 4 additional arguments succeeds.
func Call4(ctx context.Context, closure, arg1, arg2, arg3, arg4 Term) *Promise {
	return callN(ctx, closure, []Term{arg1, arg2, arg3, arg4})
}

// Call5 succeeds if closure with 5 additional arguments succeeds.
func Call5(ctx context.Context, closure, arg1, arg2, arg3, arg4, arg5 Term) *Promise {
	return callN(ctx, closure, []Term{arg1, arg2, arg3, arg4, arg5})
}

// Call6 succeeds if closure with 6 additional arguments succeeds.
func Call6(ctx context.Context, closure, arg1, arg2, arg3, arg4, arg5, arg6 Term) *Promise {
	return callN(ctx, closure, []Term{arg1, arg2, arg3, arg4, arg5, arg6})
}

// Call7 succeeds if closure with 7 additional arguments succeeds.
func Call7(ctx context.Context, closure, arg1, arg2, arg3, arg4, arg5, arg6, arg7 Term) *Promise {
	return callN(ctx, closure, []Term{arg1, arg2, arg3, arg4, arg5, arg6, arg7})
}

func callN(ctx context.Context, closure Term, additional []Term) *Promise {
	pi, arg, err := piArg(ctx, closure)
	if err != nil {
		return Error(err)
	}
	args, err := makeSlice[Term](int(pi.arity) + len(additional))
	if err != nil {
		return Error(resourceError(ctx, resourceMemory))
	}
	args = args[:pi.arity]
	for i := 0; i < int(pi.arity); i++ {
		args[i] = arg(i)
	}
	args = append(args, additional...)
	return Call(ctx, pi.name.Apply(args...))
}

// CallNth succeeds iff goal succeeds and nth unifies with the number of re-execution.
// See http://www.complang.tuwien.ac.at/ulrich/iso-prolog/call_nth
func CallNth(ctx context.Context, goal, nth Term) *Promise {
	nth = Resolve(ctx, nth)
	switch nth := nth.(type) {
	case Variable:
		break
	case Integer:
		switch {
		case nth < 0:
			return Error(domainError(ctx, validDomainNotLessThanZero, nth))
		case nth == 0:
			return Bool(false)
		}
	default:
		return Error(typeError(ctx, validTypeInteger, nth))
	}

	var (
		p         *Promise
		n         Integer
		err       error
		parentCtx = ctx
	)
	ctx = WithCont(ctx, func(ctx context.Context) *Promise {
		n, err = addI(n, Integer(1))
		if err != nil {
			return Error(representationError(parentCtx, flagMaxInteger))
		}

		u := Unify(ctx, n, nth)
		if nth, ok := nth.(Integer); ok && nth <= n {
			return cut(p, func() *Promise {
				return u
			})
		}
		return u
	})
	p = Call(ctx, goal)
	return p
}

// Unify unifies x and y without occurs check (i.e., X = f(X) is allowed).
func Unify(ctx context.Context, x, y Term) *Promise {
	ctx, ok := withUnification(ctx, x, y)
	if !ok {
		return Bool(false)
	}
	return Continue(ctx)
}

// UnifyWithOccursCheck unifies x and y with occurs check (i.e., X = f(X) is not allowed).
func UnifyWithOccursCheck(ctx context.Context, x, y Term) *Promise {
	env := env(ctx)
	k := cont(ctx)

	env, ok := env.unifyWithOccursCheck(x, y)
	if !ok {
		return Bool(false)
	}
	return k(withEnv(ctx, env))
}

// SubsumesTerm succeeds if general and specific are unifiable without binding variables in specific.
func SubsumesTerm(ctx context.Context, general, specific Term) *Promise {
	env := env(ctx)

	theta, ok := env.unifyWithOccursCheck(general, specific)
	if !ok {
		return Bool(false)
	}

	if d := env.compare(theta.simplify(general), specific); d != 0 {
		return Bool(false)
	}

	return Continue(ctx)
}

// TypeVar checks if t is a variable.
func TypeVar(ctx context.Context, t Term) *Promise {
	if _, ok := Resolve(ctx, t).(Variable); !ok {
		return Bool(false)
	}
	return Continue(ctx)
}

// TypeFloat checks if t is a floating-point number.
func TypeFloat(ctx context.Context, t Term) *Promise {
	if _, ok := Resolve(ctx, t).(Float); !ok {
		return Bool(false)
	}
	return Continue(ctx)
}

// TypeInteger checks if t is an integer.
func TypeInteger(ctx context.Context, t Term) *Promise {
	if _, ok := Resolve(ctx, t).(Integer); !ok {
		return Bool(false)
	}
	return Continue(ctx)
}

// TypeAtom checks if t is an atom.
func TypeAtom(ctx context.Context, t Term) *Promise {
	if _, ok := Resolve(ctx, t).(Atom); !ok {
		return Bool(false)
	}
	return Continue(ctx)
}

// TypeCompound checks if t is a compound term.
func TypeCompound(ctx context.Context, t Term) *Promise {
	if _, ok := Resolve(ctx, t).(Compound); !ok {
		return Bool(false)
	}
	return Continue(ctx)
}

// AcyclicTerm checks if t is acyclic.
func AcyclicTerm(ctx context.Context, t Term) *Promise {
	if cyclicTerm(ctx, t, nil) {
		return Bool(false)
	}
	return Continue(ctx)
}

func cyclicTerm(ctx context.Context, t Term, visited []Term) bool {
	t = Resolve(ctx, t)

	for _, v := range visited {
		if t == v {
			return true
		}
	}
	visited = append(visited, t)

	if c, ok := t.(Compound); ok {
		for i := 0; i < c.Arity(); i++ {
			if cyclicTerm(ctx, c.Arg(i), visited) {
				return true
			}
		}
	}

	return false
}

// Functor extracts the name and arity of term, or unifies term with an atomic/compound term of name and arity with
// fresh variables as arguments.
func Functor(ctx context.Context, t, name, arity Term) *Promise {
	switch t := Resolve(ctx, t).(type) {
	case Variable:
		switch arity := Resolve(ctx, arity).(type) {
		case Variable:
			return Error(InstantiationError(ctx))
		case Integer:
			if arity < 0 {
				return Error(domainError(ctx, validDomainNotLessThanZero, arity))
			}

			name := Resolve(ctx, name)

			switch name := name.(type) {
			case Variable:
				return Error(InstantiationError(ctx))
			case Compound:
				return Error(typeError(ctx, validTypeAtomic, name))
			}

			if arity == 0 {
				return Unify(ctx, t, name)
			}

			n, ok := name.(Atom)
			if !ok {
				return Error(typeError(ctx, validTypeAtom, name))
			}

			vs, err := makeSlice[Term](int(arity))
			if err != nil {
				return Error(resourceError(ctx, resourceMemory))
			}
			for i := range vs {
				vs[i] = NewVariable()
			}
			return Unify(ctx, t, n.Apply(vs...))
		default:
			return Error(typeError(ctx, validTypeInteger, arity))
		}
	case Compound:
		return Unify(ctx, tuple(name, arity), tuple(t.Functor(), Integer(t.Arity())))
	default: // atomic
		return Unify(ctx, tuple(name, arity), tuple(t, Integer(0)))
	}
}

// Arg extracts nth argument of term as arg, or finds the argument position of arg in term as nth.
func Arg(ctx context.Context, nth, t, arg Term) *Promise {
	switch c := Resolve(ctx, t).(type) {
	case Variable:
		return Error(InstantiationError(ctx))
	case Compound:
		switch n := Resolve(ctx, nth).(type) {
		case Variable:
			return Error(InstantiationError(ctx))
		case Integer:
			if n == 0 || int(n) > c.Arity() {
				return Bool(false)
			}
			if n < 0 {
				return Error(domainError(ctx, validDomainNotLessThanZero, n))
			}
			return Unify(ctx, arg, c.Arg(int(n)-1))
		default:
			return Error(typeError(ctx, validTypeInteger, n))
		}
	default:
		return Error(typeError(ctx, validTypeCompound, t))
	}
}

// Univ constructs list as a list which first element is the functor of term and the rest is the arguments of term, or construct a compound from list as term.
func Univ(ctx context.Context, t, list Term) *Promise {
	switch t := Resolve(ctx, t).(type) {
	case Variable:
		elems, err := slice(ctx, list)
		if err != nil {
			return Error(err)
		}
		switch len(elems) {
		case 0:
			return Error(domainError(ctx, validDomainNonEmptyList, list))
		case 1:
			switch e := Resolve(ctx, elems[0]).(type) {
			case Variable:
				return Error(InstantiationError(ctx))
			case Compound:
				return Error(typeError(ctx, validTypeAtomic, e))
			default:
				return Unify(ctx, t, e)
			}
		default:
			switch e := Resolve(ctx, elems[0]).(type) {
			case Variable:
				return Error(InstantiationError(ctx))
			case Atom:
				return Unify(ctx, t, e.Apply(elems[1:]...))
			default:
				return Error(typeError(ctx, validTypeAtom, e))
			}
		}
	case Compound:
		iter := ListIterator{List: list, AllowPartial: true}
		for iter.Next(ctx) {
		}
		if err := iter.Err(); err != nil {
			return Error(err)
		}
		elems := []Term{t.Functor()}
		for i := 0; i < t.Arity(); i++ {
			elems = append(elems, t.Arg(i))
		}
		return Unify(ctx, list, List(elems...))
	default:
		iter := ListIterator{List: list, AllowPartial: true}
		for iter.Next(ctx) {
		}
		if err := iter.Err(); err != nil {
			return Error(err)
		}
		return Unify(ctx, list, List(t))
	}
}

// CopyTerm clones in as out.
func CopyTerm(ctx context.Context, in, out Term) *Promise {
	return Unify(ctx, renamedCopy(ctx, in, nil), out)
}

func renamedCopy(ctx context.Context, t Term, copied map[termID]Term) Term {
	if copied == nil {
		copied = map[termID]Term{}
	}
	t = Resolve(ctx, t)
	if c, ok := copied[id(t)]; ok {
		return c
	}
	switch t := t.(type) {
	case Variable:
		v := NewVariable()
		copied[id(t)] = v
		return v
	case charList, codeList:
		return t
	case list:
		l := make(list, len(t))
		copied[id(t)] = l
		for i := range t {
			l[i] = renamedCopy(ctx, t[i], copied)
		}
		return l
	case *partial:
		var p partial
		copied[id(t)] = &p
		p.Compound = renamedCopy(ctx, t.Compound, copied).(Compound)
		tail := renamedCopy(ctx, *t.tail, copied)
		p.tail = &tail
		return &p
	case Compound:
		args, err := makeSlice[Term](t.Arity())
		if err != nil {
			return resourceError(ctx, resourceMemory)
		}
		c := compound{
			functor: t.Functor(),
			args:    args,
		}
		copied[id(t)] = &c
		for i := 0; i < t.Arity(); i++ {
			c.args[i] = renamedCopy(ctx, t.Arg(i), copied)
		}
		return &c
	default:
		return t
	}
}

// TermVariables succeeds if vars unifies with a list of variables in term.
func TermVariables(ctx context.Context, term, vars Term) *Promise {
	var (
		witness  = map[Variable]struct{}{}
		ret      []Term
		t        Term
		traverse = []Term{term}
	)
	for len(traverse) > 0 {
		t, traverse = traverse[0], traverse[1:]
		switch t := Resolve(ctx, t).(type) {
		case Variable:
			if _, ok := witness[t]; !ok {
				ret = append(ret, t)
			}
			witness[t] = struct{}{}
		case Compound:
			args, err := makeSlice[Term](t.Arity())
			if err != nil {
				return Error(resourceError(ctx, resourceMemory))
			}
			for i := 0; i < t.Arity(); i++ {
				args[i] = t.Arg(i)
			}
			traverse = append(args, traverse...)
		}
	}

	iter := ListIterator{List: vars, AllowPartial: true}
	for iter.Next(ctx) {
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}

	return Unify(ctx, vars, List(ret...))
}

var operatorSpecifiers = map[Atom]operatorSpecifier{
	atomFX:  operatorSpecifierFX,
	atomFY:  operatorSpecifierFY,
	atomXF:  operatorSpecifierXF,
	atomYF:  operatorSpecifierYF,
	atomXFX: operatorSpecifierXFX,
	atomXFY: operatorSpecifierXFY,
	atomYFX: operatorSpecifierYFX,
}

// Op defines operator with priority and specifier, or removes when priority is 0.
func Op(ctx context.Context, priority, specifier, op Term) *Promise {
	var p Integer
	switch priority := Resolve(ctx, priority).(type) {
	case Variable:
		return Error(InstantiationError(ctx))
	case Integer:
		if priority < 0 || priority > 1200 {
			return Error(domainError(ctx, validDomainOperatorPriority, priority))
		}
		p = priority
	default:
		return Error(typeError(ctx, validTypeInteger, priority))
	}

	var spec operatorSpecifier
	switch specifier := Resolve(ctx, specifier).(type) {
	case Variable:
		return Error(InstantiationError(ctx))
	case Atom:
		var ok bool
		spec, ok = operatorSpecifiers[specifier]
		if !ok {
			return Error(domainError(ctx, validDomainOperatorSpecifier, specifier))
		}
	default:
		return Error(typeError(ctx, validTypeAtom, specifier))
	}

	var names []Atom
	switch op := Resolve(ctx, op).(type) {
	case Atom:
		names = []Atom{op}
	default:
		iter := ListIterator{List: op}
		for iter.Next(ctx) {
			switch op := Resolve(ctx, iter.Current()).(type) {
			case Atom:
				names = appendUniqNewAtom(names, op)
			default:
				return Error(typeError(ctx, validTypeAtom, op))
			}
		}
		if err := iter.Err(); err != nil {
			return Error(err)
		}
	}

	for _, name := range names {
		if p := validateOp(ctx, p, spec, name); p != nil {
			return p
		}
	}

	vm, err := vm(ctx)
	if err != nil {
		return Error(err)
	}

	for _, name := range names {
		if class := spec.class(); vm.operators.definedInClass(name, spec.class()) {
			vm.operators.remove(name, class)
		}

		vm.operators.define(p, spec, name)
	}

	return Continue(ctx)
}

func validateOp(ctx context.Context, p Integer, spec operatorSpecifier, name Atom) *Promise {
	vm, err := vm(ctx)
	if err != nil {
		return Error(err)
	}

	switch name {
	case atomComma:
		if vm.operators.definedInClass(name, operatorClassInfix) {
			return Error(permissionError(ctx, operationModify, permissionTypeOperator, name))
		}
	case atomBar:
		if spec.class() != operatorClassInfix || (p > 0 && p < 1001) {
			op := operationCreate
			if vm.operators.definedInClass(name, operatorClassInfix) {
				op = operationModify
			}
			return Error(permissionError(ctx, op, permissionTypeOperator, name))
		}
	case atomEmptyBlock, atomEmptyList:
		return Error(permissionError(ctx, operationCreate, permissionTypeOperator, name))
	}

	// 6.3.4.3 There shall not be an infix and a postfix Operator with the same name.
	switch spec.class() {
	case operatorClassInfix:
		if vm.operators.definedInClass(name, operatorClassPostfix) {
			return Error(permissionError(ctx, operationCreate, permissionTypeOperator, name))
		}
	case operatorClassPostfix:
		if vm.operators.definedInClass(name, operatorClassInfix) {
			return Error(permissionError(ctx, operationCreate, permissionTypeOperator, name))
		}
	}

	return nil
}

func appendUniqNewAtom(slice []Atom, elem Atom) []Atom {
	for _, e := range slice {
		if e == elem {
			return slice
		}
	}
	return append(slice, elem)
}

// CurrentOp succeeds if operator is defined with priority and specifier.
func CurrentOp(ctx context.Context, priority, specifier, op Term) *Promise {
	switch p := Resolve(ctx, priority).(type) {
	case Variable:
		break
	case Integer:
		if p < 0 || p > 1200 {
			return Error(domainError(ctx, validDomainOperatorPriority, priority))
		}
	default:
		return Error(domainError(ctx, validDomainOperatorPriority, priority))
	}

	switch s := Resolve(ctx, specifier).(type) {
	case Variable:
		break
	case Atom:
		if _, ok := map[Atom]struct{}{
			atomXF:  {},
			atomYF:  {},
			atomXFX: {},
			atomXFY: {},
			atomYFX: {},
			atomFX:  {},
			atomFY:  {},
		}[s]; !ok {
			return Error(domainError(ctx, validDomainOperatorSpecifier, s))
		}
	default:
		return Error(domainError(ctx, validDomainOperatorSpecifier, s))
	}

	switch Resolve(ctx, op).(type) {
	case Variable, Atom:
		break
	default:
		return Error(typeError(ctx, validTypeAtom, op))
	}

	vm, err := vm(ctx)
	if err != nil {
		return Error(err)
	}

	pattern := tuple(priority, specifier, op)
	ks := make([]func() *Promise, 0, len(vm.operators)*int(_operatorClassLen))
	for _, ops := range vm.operators {
		for _, op := range ops {
			op := op
			if op == (operator{}) {
				continue
			}
			ks = append(ks, func() *Promise {
				return Unify(ctx, pattern, tuple(op.priority, op.specifier.term(), op.name))
			})
		}
	}
	return Delay(ks...)
}

// Assertz appends t to the database.
func Assertz(ctx context.Context, t Term) *Promise {
	if err := assertMerge(ctx, t, func(existing, new []clause) []clause {
		return append(existing, new...)
	}); err != nil {
		return Error(err)
	}
	return Continue(ctx)
}

// Asserta prepends t to the database.
func Asserta(ctx context.Context, t Term) *Promise {
	if err := assertMerge(ctx, t, func(existing, new []clause) []clause {
		return append(new, existing...)
	}); err != nil {
		return Error(err)
	}
	return Continue(ctx)
}

func assertMerge(ctx context.Context, t Term, merge func([]clause, []clause) []clause) error {
	pi, arg, err := piArg(ctx, t)
	if err != nil {
		return err
	}

	if pi == (procedureIndicator{name: atomIf, arity: 2}) {
		pi, _, err = piArg(ctx, arg(0))
		if err != nil {
			return err
		}
	}

	vm, err := vm(ctx)
	if err != nil {
		return err
	}

	if vm.procedures == nil {
		vm.procedures = map[procedureIndicator]procedure{}
	}
	p, ok := vm.procedures[pi]
	if !ok {
		p = &userDefined{dynamic: true}
		vm.procedures[pi] = p
	}

	added, err := compile(ctx, t)
	if err != nil {
		return err
	}

	u, ok := p.(*userDefined)
	if !ok || !u.dynamic {
		return permissionError(ctx, operationModify, permissionTypeStaticProcedure, pi.Term())
	}

	u.clauses = merge(u.clauses, added)
	return nil
}

// BagOf collects all the solutions of goal as instances, which unify with template. instances may contain duplications.
func BagOf(ctx context.Context, template, goal, instances Term) *Promise {
	return collectionOf(ctx, func(ctx context.Context, tList []Term) Term {
		return List(tList...)
	}, template, goal, instances)
}

// SetOf collects all the solutions of goal as instances, which unify with template. instances don't contain duplications.
func SetOf(ctx context.Context, template, goal, instances Term) *Promise {
	return collectionOf(ctx, func(ctx context.Context, tList []Term) Term {
		env := env(ctx)
		return env.set(tList...)
	}, template, goal, instances)
}

func collectionOf(ctx context.Context, agg func(context.Context, []Term) Term, template, goal, instances Term) *Promise {
	fvs := newFreeVariablesSet(ctx, goal, template)
	w, err := makeSlice[Term](len(fvs))
	if err != nil {
		return Error(resourceError(ctx, resourceMemory))
	}
	w = w[:0]
	for v := range fvs {
		w = append(w, v)
	}
	sort.Slice(w, func(i, j int) bool {
		return w[i].(Variable) < w[j].(Variable)
	})
	witness := tuple(w...)
	g := iteratedGoalTerm(ctx, goal)
	s := Term(NewVariable())

	iter := ListIterator{List: instances, AllowPartial: true}
	for iter.Next(ctx) {
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}

	ctx = WithCont(ctx, func(ctx context.Context) *Promise {
		s, _ := slice(ctx, s)
		ks := make([]func() *Promise, 0, len(s))
		for len(s) > 0 {
			var wt Compound
			wt, s = s[0].(Compound), s[1:]
			w, t := wt.Arg(0), wt.Arg(1) // W+T
			wList, tList := []Term{w}, []Term{t}
			n := 0 // https://github.com/golang/go/wiki/SliceTricks#filter-in-place
			for _, e := range s {
				e := e.(Compound)
				ww, tt := e.Arg(0), e.Arg(1) // WW+TT
				if variant(ctx, ww, w) {
					wList = append(wList, ww)
					tList = append(tList, tt)
				} else { // keep
					s[n] = e
					n++
				}
			}
			s = s[:n]
			ks = append(ks, func() *Promise {
				env := env(ctx)
				for _, w = range wList {
					env, _ = env.Unify(witness, w)
				}
				return Unify(withEnv(ctx, env), agg(ctx, tList), instances)
			})
		}
		return Delay(ks...)
	})

	return FindAll(ctx, atomPlus.Apply(witness, template), g, s)
}

// FindAll collects all the solutions of goal as instances, which unify with template. instances may contain duplications.
func FindAll(ctx context.Context, template, goal, instances Term) *Promise {
	iter := ListIterator{List: instances, AllowPartial: true}
	for iter.Next(ctx) {
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}

	var answers []Term
	callCtx := WithCont(ctx, func(ctx context.Context) *Promise {
		answers = append(answers, renamedCopy(ctx, template, nil))
		return Bool(false) // ask for more solutions
	})
	if _, err := Call(callCtx, goal).Force(); err != nil {
		return Error(err)
	}
	return Unify(ctx, instances, List(answers...))
}

// Compare compares term1 and term2 and unifies order with <, =, or >.
func Compare(ctx context.Context, order, term1, term2 Term) *Promise {
	switch o := Resolve(ctx, order).(type) {
	case Variable:
		break
	case Atom:
		switch o {
		case atomLessThan, atomEqual, atomGreaterThan:
			break
		default:
			return Error(domainError(ctx, validDomainOrder, order))
		}
	default:
		return Error(typeError(ctx, validTypeAtom, order))
	}

	env := env(ctx)
	switch o := env.compare(term1, term2); o {
	case 1:
		return Unify(ctx, atomGreaterThan, order)
	case -1:
		return Unify(ctx, atomLessThan, order)
	default:
		return Unify(ctx, atomEqual, order)
	}
}

// Between succeeds when lower, upper, and value are all integers, and lower <= value <= upper.
// If value is a variable, it is unified with successive integers from lower to upper.
func Between(ctx context.Context, lower, upper, value Term) *Promise {
	var low, high Integer

	switch lower := Resolve(ctx, lower).(type) {
	case Integer:
		low = lower
	case Variable:
		return Error(InstantiationError(ctx))
	default:
		return Error(typeError(ctx, validTypeInteger, lower))
	}

	switch upper := Resolve(ctx, upper).(type) {
	case Integer:
		high = upper
	case Variable:
		return Error(InstantiationError(ctx))
	default:
		return Error(typeError(ctx, validTypeInteger, upper))
	}

	if low > high {
		return Bool(false)
	}

	switch value := Resolve(ctx, value).(type) {
	case Integer:
		if value < low || value > high {
			return Bool(false)
		}
		return Continue(ctx)
	case Variable:
		ks := make([]func() *Promise, 0, 2)
		ks = append(ks, func() *Promise {
			return Unify(ctx, value, low)
		})
		if low < high {
			ks = append(ks, func() *Promise {
				return Between(ctx, low+1, upper, value)
			})
		}
		return Delay(ks...)
	default:
		return Error(typeError(ctx, validTypeInteger, value))
	}
}

// Sort succeeds if sorted list of elements of list unifies with sorted.
func Sort(ctx context.Context, list, sorted Term) *Promise {
	var elems []Term
	iter := ListIterator{List: list}
	for iter.Next(ctx) {
		elems = append(elems, Resolve(ctx, iter.Current()))
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}

	iter = ListIterator{List: sorted, AllowPartial: true}
	for iter.Next(ctx) {
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}

	env := env(ctx)
	return Unify(ctx, sorted, env.set(elems...))
}

// KeySort succeeds if sorted is a sorted list of pairs based on their keys.
func KeySort(ctx context.Context, pairs, sorted Term) *Promise {
	var elems []Term
	iter := ListIterator{List: pairs}
	for iter.Next(ctx) {
		switch e := Resolve(ctx, iter.Current()).(type) {
		case Variable:
			return Error(InstantiationError(ctx))
		case Compound:
			if e.Functor() != atomMinus || e.Arity() != 2 {
				return Error(typeError(ctx, validTypePair, e))
			}
			elems = append(elems, e)
		default:
			return Error(typeError(ctx, validTypePair, e))
		}
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}

	switch s := Resolve(ctx, sorted).(type) {
	case Variable:
		break
	default:
		iter := ListIterator{List: s}
		for iter.Next(ctx) {
			switch e := Resolve(ctx, iter.Current()).(type) {
			case Variable:
				continue
			case Compound:
				if e.Functor() != atomMinus || e.Arity() != 2 {
					return Error(typeError(ctx, validTypePair, e))
				}
			default:
				return Error(typeError(ctx, validTypePair, e))
			}
		}
		if err := iter.Err(); err != nil {
			return Error(err)
		}
	}

	env := env(ctx)
	sort.SliceStable(elems, func(i, j int) bool {
		return env.compare(elems[i].(Compound).Arg(0), elems[j].(Compound).Arg(0)) == -1
	})

	return Unify(ctx, sorted, List(elems...))
}

// Throw throws ball as an exception.
func Throw(ctx context.Context, ball Term) *Promise {
	switch b := Resolve(ctx, ball).(type) {
	case Variable:
		return Error(InstantiationError(ctx))
	default:
		return Error(NewException(ctx, b))
	}
}

// Catch calls goal. If an exception is thrown and unifies with catcher, it calls recover.
func Catch(ctx context.Context, goal, catcher, recover Term) *Promise {
	return catch(func(err error) *Promise {
		e, ok := Resolve(ctx, err).(Exception)
		if !ok {
			e = Exception{term: atomError.Apply(NewAtom("system_error"), NewAtom(err.Error()))}
		}

		env := env(ctx)
		env, ok = env.Unify(catcher, e.term)
		if !ok {
			return nil
		}

		return Call(ctx, recover)
	}, func() *Promise {
		return Call(ctx, goal)
	})
}

// CurrentPredicate matches pi with a predicate indicator of the user-defined procedures in the database.
func CurrentPredicate(ctx context.Context, pi Term) *Promise {
	switch pi := Resolve(ctx, pi).(type) {
	case Variable:
		break
	case Compound:
		if pi.Functor() != atomSlash || pi.Arity() != 2 {
			return Error(typeError(ctx, validTypePredicateIndicator, pi))
		}
		if _, ok := Resolve(ctx, pi.Arg(0)).(Atom); !ok {
			return Error(typeError(ctx, validTypePredicateIndicator, pi))
		}
		if _, ok := Resolve(ctx, pi.Arg(1)).(Integer); !ok {
			return Error(typeError(ctx, validTypePredicateIndicator, pi))
		}
	default:
		return Error(typeError(ctx, validTypePredicateIndicator, pi))
	}

	vm, err := vm(ctx)
	if err != nil {
		return Error(err)
	}

	ks := make([]func() *Promise, 0, len(vm.procedures))
	for key, p := range vm.procedures {
		switch p.(type) {
		case *userDefined:
			c := key.Term()
			ks = append(ks, func() *Promise {
				return Unify(ctx, pi, c)
			})
		default:
			continue
		}
	}
	return Delay(ks...)
}

// Retract removes the first clause that matches with t.
func Retract(ctx context.Context, t Term) *Promise {
	t = rulify(ctx, t)

	h := t.(Compound).Arg(0)
	pi, _, err := piArg(ctx, h)
	if err != nil {
		return Error(err)
	}

	vm, err := vm(ctx)
	if err != nil {
		return Error(err)
	}

	p, ok := vm.procedures[pi]
	if !ok {
		return Bool(false)
	}

	u, ok := p.(*userDefined)
	if !ok || !u.dynamic {
		return Error(permissionError(ctx, operationModify, permissionTypeStaticProcedure, pi.Term()))
	}

	deleted := 0
	ks := make([]func() *Promise, len(u.clauses))
	for i, c := range u.clauses {
		i := i
		raw := rulify(ctx, c.raw)
		ks[i] = func() *Promise {
			ctx := WithCont(ctx, func(ctx context.Context) *Promise {
				j := i - deleted
				u.clauses, u.clauses[len(u.clauses)-1] = append(u.clauses[:j], u.clauses[j+1:]...), clause{}
				deleted++
				return Continue(ctx)
			})
			return Unify(ctx, t, raw)
		}
	}
	return Delay(ks...)
}

// Abolish removes the procedure indicated by pi from the database.
func Abolish(ctx context.Context, pi Term) *Promise {
	switch pi := Resolve(ctx, pi).(type) {
	case Variable:
		return Error(InstantiationError(ctx))
	case Compound:
		if pi.Functor() != atomSlash || pi.Arity() != 2 {
			return Error(typeError(ctx, validTypePredicateIndicator, pi))
		}

		name, arity := pi.Arg(0), pi.Arg(1)

		vm, err := vm(ctx)
		if err != nil {
			return Error(err)
		}

		switch name := Resolve(ctx, name).(type) {
		case Variable:
			return Error(InstantiationError(ctx))
		case Atom:
			switch arity := Resolve(ctx, arity).(type) {
			case Variable:
				return Error(InstantiationError(ctx))
			case Integer:
				if arity < 0 {
					return Error(domainError(ctx, validDomainNotLessThanZero, arity))
				}
				key := procedureIndicator{name: name, arity: arity}
				if u, ok := vm.procedures[key].(*userDefined); !ok || !u.dynamic {
					return Error(permissionError(ctx, operationModify, permissionTypeStaticProcedure, key.Term()))
				}
				delete(vm.procedures, key)
				return Continue(ctx)
			default:
				return Error(typeError(ctx, validTypeInteger, arity))
			}
		default:
			return Error(typeError(ctx, validTypeAtom, name))
		}
	default:
		return Error(typeError(ctx, validTypePredicateIndicator, pi))
	}
}

// CurrentInput unifies stream with the current input stream.
func CurrentInput(ctx context.Context, stream Term) *Promise {
	vm, err := vm(ctx)
	if err != nil {
		return Error(err)
	}

	switch Resolve(ctx, stream).(type) {
	case Variable, *Stream:
		return Unify(ctx, stream, vm.input)
	default:
		return Error(domainError(ctx, validDomainStream, stream))
	}
}

// CurrentOutput unifies stream with the current output stream.
func CurrentOutput(ctx context.Context, stream Term) *Promise {
	vm, err := vm(ctx)
	if err != nil {
		return Error(err)
	}

	switch Resolve(ctx, stream).(type) {
	case Variable, *Stream:
		return Unify(ctx, stream, vm.output)
	default:
		return Error(domainError(ctx, validDomainStream, stream))
	}
}

// SetInput sets streamOrAlias as the current input stream.
func SetInput(ctx context.Context, streamOrAlias Term) *Promise {
	s, err := stream(ctx, streamOrAlias)
	if err != nil {
		return Error(err)
	}

	if s.mode != ioModeRead {
		return Error(permissionError(ctx, operationInput, permissionTypeStream, streamOrAlias))
	}

	vm, err := vm(ctx)
	if err != nil {
		return Error(err)
	}

	vm.input = s
	return Continue(ctx)
}

// SetOutput sets streamOrAlias as the current output stream.
func SetOutput(ctx context.Context, streamOrAlias Term) *Promise {
	s, err := stream(ctx, streamOrAlias)
	if err != nil {
		return Error(err)
	}

	if s.mode != ioModeWrite && s.mode != ioModeAppend {
		return Error(permissionError(ctx, operationOutput, permissionTypeStream, streamOrAlias))
	}

	vm, err := vm(ctx)
	if err != nil {
		return Error(err)
	}

	vm.output = s
	return Continue(ctx)
}

func stream(ctx context.Context, streamOrAlias Term) (*Stream, error) {
	vm, err := vm(ctx)
	if err != nil {
		return nil, err
	}
	switch s := Resolve(ctx, streamOrAlias).(type) {
	case Variable:
		return nil, InstantiationError(ctx)
	case Atom:
		v, ok := vm.streams.lookup(s)
		if !ok {
			return nil, existenceError(ctx, objectTypeStream, streamOrAlias)
		}
		return v, nil
	case *Stream:
		return s, nil
	default:
		return nil, domainError(ctx, validDomainStreamOrAlias, streamOrAlias)
	}
}

var openFile = os.OpenFile

// Open opens SourceSink in mode and unifies with stream.
func Open(ctx context.Context, sourceSink, mode, stream, options Term) *Promise {
	var name string
	switch s := Resolve(ctx, sourceSink).(type) {
	case Variable:
		return Error(InstantiationError(ctx))
	case Atom:
		name = s.String()
	default:
		return Error(domainError(ctx, validDomainSourceSink, sourceSink))
	}

	var streamMode ioMode
	switch m := Resolve(ctx, mode).(type) {
	case Variable:
		return Error(InstantiationError(ctx))
	case Atom:
		var ok bool
		streamMode, ok = map[Atom]ioMode{
			atomRead:   ioModeRead,
			atomWrite:  ioModeWrite,
			atomAppend: ioModeAppend,
		}[m]
		if !ok {
			return Error(domainError(ctx, validDomainIOMode, m))
		}
	default:
		return Error(typeError(ctx, validTypeAtom, mode))
	}

	if _, ok := Resolve(ctx, stream).(Variable); !ok {
		return Error(InstantiationError(ctx))
	}

	vm, err := vm(ctx)
	if err != nil {
		return Error(err)
	}

	s := Stream{vm: vm, mode: streamMode}
	switch f, err := openFile(name, int(s.mode), 0644); {
	case err == nil:
		s.sourceSink = f
		if fi, err := f.Stat(); err == nil {
			s.reposition = fi.Mode()&fs.ModeType == 0
		}
	case os.IsNotExist(err):
		return Error(existenceError(ctx, objectTypeSourceSink, sourceSink))
	case os.IsPermission(err):
		return Error(permissionError(ctx, operationOpen, permissionTypeSourceSink, sourceSink))
	default:
		return Error(err)
	}

	iter := ListIterator{List: options}
	for iter.Next(ctx) {
		if err := handleStreamOption(ctx, &s, iter.Current()); err != nil {
			return Error(err)
		}
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}

	if err := s.initRead(); err == nil {
		s.checkEOS()
	}

	return Unify(ctx, stream, &s)
}

func handleStreamOption(ctx context.Context, s *Stream, option Term) error {
	switch o := Resolve(ctx, option).(type) {
	case Variable:
		return InstantiationError(ctx)
	case Compound:
		if o.Arity() != 1 {
			break
		}

		switch o.Functor() {
		case atomAlias:
			return handleStreamOptionAlias(ctx, s, o)
		case atomType:
			return handleStreamOptionType(ctx, s, o)
		case atomReposition:
			return handleStreamOptionReposition(ctx, s, o)
		case atomEOFAction:
			return handleStreamOptionEOFAction(ctx, s, o)
		}
	}
	return domainError(ctx, validDomainStreamOption, option)
}

func handleStreamOptionAlias(ctx context.Context, s *Stream, o Compound) error {
	vm, err := vm(ctx)
	if err != nil {
		return err
	}

	switch a := Resolve(ctx, o.Arg(0)).(type) {
	case Variable:
		return InstantiationError(ctx)
	case Atom:
		if _, ok := vm.streams.lookup(a); ok {
			return permissionError(ctx, operationOpen, permissionTypeSourceSink, o)
		}
		s.alias = a
		vm.streams.add(s)
		return nil
	default:
		return domainError(ctx, validDomainStreamOption, o)
	}
}

func handleStreamOptionType(ctx context.Context, s *Stream, o Compound) error {
	switch t := Resolve(ctx, o.Arg(0)).(type) {
	case Variable:
		return InstantiationError(ctx)
	case Atom:
		switch t {
		case atomText:
			s.streamType = streamTypeText
			return nil
		case atomBinary:
			s.streamType = streamTypeBinary
			return nil
		}
	}
	return domainError(ctx, validDomainStreamOption, o)
}

func handleStreamOptionReposition(ctx context.Context, s *Stream, o Compound) error {
	switch r := Resolve(ctx, o.Arg(0)).(type) {
	case Variable:
		return InstantiationError(ctx)
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
	return domainError(ctx, validDomainStreamOption, o)
}

func handleStreamOptionEOFAction(ctx context.Context, s *Stream, o Compound) error {
	switch e := Resolve(ctx, o.Arg(0)).(type) {
	case Variable:
		return InstantiationError(ctx)
	case Atom:
		switch e {
		case atomError:
			s.eofAction = eofActionError
			return nil
		case atomEOFCode:
			s.eofAction = eofActionEOFCode
			return nil
		case atomReset:
			s.eofAction = eofActionReset
			return nil
		}
	}
	return domainError(ctx, validDomainStreamOption, o)
}

// Close closes a stream specified by streamOrAlias.
func Close(ctx context.Context, streamOrAlias, options Term) *Promise {
	s, err := stream(ctx, streamOrAlias)
	if err != nil {
		return Error(err)
	}

	var force bool
	iter := ListIterator{List: options}
	for iter.Next(ctx) {
		switch option := Resolve(ctx, iter.Current()).(type) {
		case Variable:
			return Error(InstantiationError(ctx))
		case Compound:
			switch option.Functor() {
			case atomForce:
				if option.Arity() != 1 {
					return Error(domainError(ctx, validDomainStreamOption, option))
				}

				switch v := Resolve(ctx, option.Arg(0)).(type) {
				case Atom:
					switch v {
					case atomFalse:
						force = false
					case atomTrue:
						force = true
					default:
						return Error(domainError(ctx, validDomainStreamOption, option))
					}
				default:
					return Error(domainError(ctx, validDomainStreamOption, option))
				}
			}
		default:
			return Error(domainError(ctx, validDomainStreamOption, option))
		}
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}

	if err := s.Close(); err != nil && !force {
		return Error(err)
	}

	return Continue(ctx)
}

// FlushOutput sends any buffered output to the stream.
func FlushOutput(ctx context.Context, streamOrAlias Term) *Promise {
	s, err := stream(ctx, streamOrAlias)
	if err != nil {
		return Error(err)
	}

	switch err := s.Flush(); err {
	case nil:
		return Continue(ctx)
	case errWrongIOMode:
		return Error(permissionError(ctx, operationOutput, permissionTypeStream, streamOrAlias))
	default:
		return Error(err)
	}
}

// WriteTerm outputs term to stream with options.
func WriteTerm(ctx context.Context, streamOrAlias, t, options Term) *Promise {
	s, err := stream(ctx, streamOrAlias)
	if err != nil {
		return Error(err)
	}

	vm, err := vm(ctx)
	if err != nil {
		return Error(err)
	}

	opts := writeOptions{
		ops:      vm.operators,
		priority: 1200,
	}
	iter := ListIterator{List: options}
	for iter.Next(ctx) {
		if err := writeTermOption(ctx, &opts, iter.Current()); err != nil {
			return Error(err)
		}
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}

	switch err := writeTerm(ctx, s, Resolve(ctx, t), &opts); err {
	case nil:
		return Continue(ctx)
	case errWrongIOMode:
		return Error(permissionError(ctx, operationOutput, permissionTypeStream, streamOrAlias))
	case errWrongStreamType:
		return Error(permissionError(ctx, operationOutput, permissionTypeBinaryStream, streamOrAlias))
	default:
		return Error(err)
	}
}

func writeTermOption(ctx context.Context, opts *writeOptions, option Term) error {
	switch o := Resolve(ctx, option).(type) {
	case Variable:
		return InstantiationError(ctx)
	case Compound:
		if o.Arity() != 1 {
			return domainError(ctx, validDomainWriteOption, o)
		}

		if o.Functor() == atomVariableNames {
			vns, err := variableNames(ctx, o)
			if err != nil {
				return err
			}
			opts.variableNames = vns
			return nil
		}

		var b bool
		switch v := Resolve(ctx, o.Arg(0)).(type) {
		case Variable:
			return InstantiationError(ctx)
		case Atom:
			switch v {
			case atomTrue:
				b = true
			case atomFalse:
				b = false
			default:
				return domainError(ctx, validDomainWriteOption, o)
			}
		default:
			return domainError(ctx, validDomainWriteOption, o)
		}

		switch o.Functor() {
		case atomQuoted:
			opts.quoted = b
			return nil
		case atomIgnoreOps:
			opts.ignoreOps = b
			return nil
		case atomNumberVars:
			opts.numberVars = b
			return nil
		default:
			return domainError(ctx, validDomainWriteOption, o)
		}
	default:
		return domainError(ctx, validDomainWriteOption, o)
	}
}

func variableNames(ctx context.Context, option Compound) (map[Variable]Atom, error) {
	vns := map[Variable]Atom{}
	iter := ListIterator{List: option.Arg(0)}
	for iter.Next(ctx) {
		var vn Compound
		switch elem := Resolve(ctx, iter.Current()).(type) {
		case Variable:
			return nil, InstantiationError(ctx)
		case Compound:
			if elem.Functor() != atomEqual || elem.Arity() != 2 {
				return nil, domainError(ctx, validDomainWriteOption, option)
			}
			vn = elem
		default:
			return nil, domainError(ctx, validDomainWriteOption, option)
		}

		var n Atom
		switch arg := Resolve(ctx, vn.Arg(0)).(type) {
		case Variable:
			return nil, InstantiationError(ctx)
		case Atom:
			n = arg
		default:
			return nil, domainError(ctx, validDomainWriteOption, option)
		}

		var v Variable
		switch arg := Resolve(ctx, vn.Arg(1)).(type) {
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
		return nil, InstantiationError(ctx)
	case Atom:
		if s != atomEmptyList {
			return nil, domainError(ctx, validDomainWriteOption, option)
		}
		return vns, nil
	default:
		return nil, domainError(ctx, validDomainWriteOption, option)
	}
}

// CharCode converts a single-rune Atom char to an Integer code, or vice versa.
func CharCode(ctx context.Context, char, code Term) *Promise {
	switch ch := Resolve(ctx, char).(type) {
	case Variable:
		switch cd := Resolve(ctx, code).(type) {
		case Variable:
			return Error(InstantiationError(ctx))
		case Integer:
			r := rune(cd)

			if !utf8.ValidRune(r) {
				return Error(representationError(ctx, flagCharacterCode))
			}

			return Unify(ctx, ch, Atom(r))
		default:
			return Error(typeError(ctx, validTypeInteger, code))
		}
	case Atom:
		switch code := Resolve(ctx, code).(type) {
		case Variable, Integer:
			break
		default:
			return Error(typeError(ctx, validTypeInteger, code))
		}

		rs := []rune(ch.String())
		if len(rs) != 1 {
			return Error(typeError(ctx, validTypeCharacter, ch))
		}

		return Unify(ctx, code, Integer(rs[0]))
	default:
		return Error(typeError(ctx, validTypeCharacter, ch))
	}
}

// PutByte outputs an integer byte to a stream represented by streamOrAlias.
func PutByte(ctx context.Context, streamOrAlias, byt Term) *Promise {
	s, err := stream(ctx, streamOrAlias)
	if err != nil {
		return Error(err)
	}

	switch b := Resolve(ctx, byt).(type) {
	case Variable:
		return Error(InstantiationError(ctx))
	case Integer:
		if 0 > b || 255 < b {
			return Error(typeError(ctx, validTypeByte, byt))
		}

		switch err := s.WriteByte(byte(b)); err {
		case nil:
			return Continue(ctx)
		case errWrongIOMode:
			return Error(permissionError(ctx, operationOutput, permissionTypeStream, streamOrAlias))
		case errWrongStreamType:
			return Error(permissionError(ctx, operationOutput, permissionTypeTextStream, streamOrAlias))
		default:
			return Error(err)
		}
	default:
		return Error(typeError(ctx, validTypeByte, byt))
	}
}

// PutChar outputs char to the stream represented by streamOrAlias.
func PutChar(ctx context.Context, streamOrAlias, char Term) *Promise {
	s, err := stream(ctx, streamOrAlias)
	if err != nil {
		return Error(err)
	}

	switch c := Resolve(ctx, char).(type) {
	case Variable:
		return Error(InstantiationError(ctx))
	case Atom:
		if c > utf8.MaxRune {
			return Error(typeError(ctx, validTypeCharacter, c))
		}

		r := rune(c)

		switch _, err := s.WriteRune(r); err {
		case nil:
			return Continue(ctx)
		case errWrongIOMode:
			return Error(permissionError(ctx, operationOutput, permissionTypeStream, streamOrAlias))
		case errWrongStreamType:
			return Error(permissionError(ctx, operationOutput, permissionTypeBinaryStream, streamOrAlias))
		default:
			return Error(err)
		}
	default:
		return Error(typeError(ctx, validTypeCharacter, char))
	}
}

type readTermOptions struct {
	singletons    Term
	variables     Term
	variableNames Term
}

// ReadTerm reads from the stream represented by streamOrAlias and unifies with stream.
func ReadTerm(ctx context.Context, streamOrAlias, out, options Term) *Promise {
	s, err := stream(ctx, streamOrAlias)
	if err != nil {
		return Error(err)
	}

	opts := readTermOptions{
		singletons:    NewVariable(),
		variables:     NewVariable(),
		variableNames: NewVariable(),
	}
	iter := ListIterator{List: options}
	for iter.Next(ctx) {
		if err := readTermOption(ctx, &opts, iter.Current()); err != nil {
			return Error(err)
		}
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}

	vm, err := vm(ctx)
	if err != nil {
		return Error(err)
	}

	p := NewParser(vm, s)
	defer func() {
		_ = s.UnreadRune()
	}()

	t, err := p.Term()
	switch err {
	case nil:
		break
	case io.EOF:
		return Unify(ctx, out, atomEndOfFile)
	case errWrongIOMode:
		return Error(permissionError(ctx, operationInput, permissionTypeStream, streamOrAlias))
	case errWrongStreamType:
		return Error(permissionError(ctx, operationInput, permissionTypeBinaryStream, streamOrAlias))
	case errPastEndOfStream:
		return Error(permissionError(ctx, operationInput, permissionTypePastEndOfStream, streamOrAlias))
	default:
		return Error(syntaxError(ctx, err))
	}

	var singletons, variables, variableNames []Term
	for _, v := range p.Vars {
		if v.Count == 1 {
			singletons = append(singletons, v.Variable)
		}
		variables = append(variables, v.Variable)
		variableNames = append(variableNames, atomEqual.Apply(v.Name, v.Variable))
	}

	return Unify(ctx, tuple(
		out,
		opts.singletons,
		opts.variables,
		opts.variableNames,
	), tuple(
		t,
		List(singletons...),
		List(variables...),
		List(variableNames...),
	))
}

func readTermOption(ctx context.Context, opts *readTermOptions, option Term) error {
	switch option := Resolve(ctx, option).(type) {
	case Variable:
		return InstantiationError(ctx)
	case Compound:
		if option.Arity() != 1 {
			return domainError(ctx, validDomainReadOption, option)
		}

		v := Resolve(ctx, option.Arg(0))
		switch option.Functor() {
		case atomSingletons:
			opts.singletons = v
		case atomVariables:
			opts.variables = v
		case atomVariableNames:
			opts.variableNames = v
		default:
			return domainError(ctx, validDomainReadOption, option)
		}
		return nil
	default:
		return domainError(ctx, validDomainReadOption, option)
	}
}

// GetByte reads a byte from the stream represented by streamOrAlias and unifies it with inByte.
func GetByte(ctx context.Context, streamOrAlias, inByte Term) *Promise {
	s, err := stream(ctx, streamOrAlias)
	if err != nil {
		return Error(err)
	}

	switch b := Resolve(ctx, inByte).(type) {
	case Variable:
		break
	case Integer:
		if b < 0 || b > 255 {
			return Error(typeError(ctx, validTypeInByte, inByte))
		}
	default:
		return Error(typeError(ctx, validTypeInByte, inByte))
	}

	switch b, err := s.ReadByte(); err {
	case nil:
		return Unify(ctx, inByte, Integer(b))
	case io.EOF:
		return Unify(ctx, inByte, Integer(-1))
	case errWrongIOMode:
		return Error(permissionError(ctx, operationInput, permissionTypeStream, streamOrAlias))
	case errWrongStreamType:
		return Error(permissionError(ctx, operationInput, permissionTypeTextStream, streamOrAlias))
	case errPastEndOfStream:
		return Error(permissionError(ctx, operationInput, permissionTypePastEndOfStream, streamOrAlias))
	default:
		return Error(err)
	}
}

// GetChar reads a character from the stream represented by streamOrAlias and unifies it with char.
func GetChar(ctx context.Context, streamOrAlias, char Term) *Promise {
	s, err := stream(ctx, streamOrAlias)
	if err != nil {
		return Error(err)
	}

	switch c := Resolve(ctx, char).(type) {
	case Variable:
		break
	case Atom:
		if len([]rune(c.String())) != 1 {
			return Error(typeError(ctx, validTypeInCharacter, char))
		}
	default:
		return Error(typeError(ctx, validTypeInCharacter, char))
	}

	switch r, _, err := s.ReadRune(); err {
	case nil:
		if r == utf8.RuneError {
			return Error(representationError(ctx, flagCharacter))
		}

		return Unify(ctx, char, Atom(r))
	case io.EOF:
		return Unify(ctx, char, atomEndOfFile)
	case errWrongIOMode:
		return Error(permissionError(ctx, operationInput, permissionTypeStream, streamOrAlias))
	case errWrongStreamType:
		return Error(permissionError(ctx, operationInput, permissionTypeBinaryStream, streamOrAlias))
	case errPastEndOfStream:
		return Error(permissionError(ctx, operationInput, permissionTypePastEndOfStream, streamOrAlias))
	default:
		return Error(err)
	}
}

// PeekByte peeks a byte from the stream represented by streamOrAlias and unifies it with inByte.
func PeekByte(ctx context.Context, streamOrAlias, inByte Term) *Promise {
	s, err := stream(ctx, streamOrAlias)
	if err != nil {
		return Error(err)
	}

	switch b := Resolve(ctx, inByte).(type) {
	case Variable:
		break
	case Integer:
		if b < 0 || b > 255 {
			return Error(typeError(ctx, validTypeInByte, inByte))
		}
	default:
		return Error(typeError(ctx, validTypeInByte, inByte))
	}

	b, err := s.ReadByte()
	defer func() {
		_ = s.UnreadByte()
	}()
	switch err {
	case nil:
		return Unify(ctx, inByte, Integer(b))
	case io.EOF:
		return Unify(ctx, inByte, Integer(-1))
	case errWrongIOMode:
		return Error(permissionError(ctx, operationInput, permissionTypeStream, streamOrAlias))
	case errWrongStreamType:
		return Error(permissionError(ctx, operationInput, permissionTypeTextStream, streamOrAlias))
	case errPastEndOfStream:
		return Error(permissionError(ctx, operationInput, permissionTypePastEndOfStream, streamOrAlias))
	default:
		return Error(err)
	}
}

// PeekChar peeks a rune from the stream represented by streamOrAlias and unifies it with char.
func PeekChar(ctx context.Context, streamOrAlias, char Term) *Promise {
	s, err := stream(ctx, streamOrAlias)
	if err != nil {
		return Error(err)
	}

	switch c := Resolve(ctx, char).(type) {
	case Variable:
		break
	case Atom:
		if len([]rune(c.String())) != 1 {
			return Error(typeError(ctx, validTypeInCharacter, char))
		}
	default:
		return Error(typeError(ctx, validTypeInCharacter, char))
	}

	r, _, err := s.ReadRune()
	defer func() {
		_ = s.UnreadRune()
	}()
	switch err {
	case nil:
		if r == unicode.ReplacementChar {
			return Error(representationError(ctx, flagCharacter))
		}

		return Unify(ctx, char, Atom(r))
	case io.EOF:
		return Unify(ctx, char, atomEndOfFile)
	case errWrongIOMode:
		return Error(permissionError(ctx, operationInput, permissionTypeStream, streamOrAlias))
	case errWrongStreamType:
		return Error(permissionError(ctx, operationInput, permissionTypeBinaryStream, streamOrAlias))
	case errPastEndOfStream:
		return Error(permissionError(ctx, operationInput, permissionTypePastEndOfStream, streamOrAlias))
	default:
		return Error(err)
	}
}

var osExit = os.Exit

// Halt exits the process with exit code of n.
func Halt(ctx context.Context, n Term) *Promise {
	switch code := Resolve(ctx, n).(type) {
	case Variable:
		return Error(InstantiationError(ctx))
	case Integer:
		osExit(int(code))
		return Continue(ctx)
	default:
		return Error(typeError(ctx, validTypeInteger, n))
	}
}

// Clause unifies head and body with H and B respectively where H :- B is in the database.
func Clause(ctx context.Context, head, body Term) *Promise {
	pi, _, err := piArg(ctx, head)
	if err != nil {
		return Error(err)
	}

	switch Resolve(ctx, body).(type) {
	case Variable, Atom, Compound:
		break
	default:
		return Error(typeError(ctx, validTypeCallable, body))
	}

	vm, err := vm(ctx)
	if err != nil {
		return Error(err)
	}

	p, ok := vm.procedures[pi]
	if !ok {
		return Bool(false)
	}

	u, ok := p.(*userDefined)
	if !ok || !u.public {
		return Error(permissionError(ctx, operationAccess, permissionTypePrivateProcedure, pi.Term()))
	}

	ks := make([]func() *Promise, len(u.clauses))
	for i, c := range u.clauses {
		r := rulify(ctx, renamedCopy(ctx, c.raw, nil))
		ks[i] = func() *Promise {
			return Unify(ctx, atomIf.Apply(head, body), r)
		}
	}
	return Delay(ks...)
}

func rulify(ctx context.Context, t Term) Term {
	t = Resolve(ctx, t)
	if c, ok := t.(Compound); ok && c.Functor() == atomIf && c.Arity() == 2 {
		return t
	}
	return atomIf.Apply(t, atomTrue)
}

// AtomLength counts the runes in atom and unifies the result with length.
func AtomLength(ctx context.Context, atom, length Term) *Promise {
	var a Atom
	switch atom := Resolve(ctx, atom).(type) {
	case Variable:
		return Error(InstantiationError(ctx))
	case Atom:
		a = atom
	default:
		return Error(typeError(ctx, validTypeAtom, atom))
	}

	switch l := Resolve(ctx, length).(type) {
	case Variable:
		break
	case Integer:
		if l < 0 {
			return Error(domainError(ctx, validDomainNotLessThanZero, length))
		}
	default:
		return Error(typeError(ctx, validTypeInteger, length))
	}

	return Unify(ctx, length, Integer(len([]rune(a.String()))))
}

// AtomConcat concatenates atom1 and atom2 and unifies it with atom3.
func AtomConcat(ctx context.Context, atom1, atom2, atom3 Term) *Promise {
	switch a3 := Resolve(ctx, atom3).(type) {
	case Variable:
		switch a1 := Resolve(ctx, atom1).(type) {
		case Variable:
			return Error(InstantiationError(ctx))
		case Atom:
			switch a2 := Resolve(ctx, atom2).(type) {
			case Variable:
				return Error(InstantiationError(ctx))
			case Atom:
				return Unify(ctx, a3, NewAtom(a1.String()+a2.String()))
			default:
				return Error(typeError(ctx, validTypeAtom, atom2))
			}
		default:
			return Error(typeError(ctx, validTypeAtom, atom1))
		}
	case Atom:
		switch Resolve(ctx, atom1).(type) {
		case Variable, Atom:
			break
		default:
			return Error(typeError(ctx, validTypeAtom, atom1))
		}

		switch Resolve(ctx, atom2).(type) {
		case Variable, Atom:
			break
		default:
			return Error(typeError(ctx, validTypeAtom, atom2))
		}

		pattern := tuple(atom1, atom2)
		s := a3.String()
		ks := make([]func() *Promise, 0, len(s)+1)
		for i := range s {
			a1, a2 := s[:i], s[i:]
			ks = append(ks, func() *Promise {
				return Unify(ctx, &pattern, tuple(NewAtom(a1), NewAtom(a2)))
			})
		}
		ks = append(ks, func() *Promise {
			return Unify(ctx, &pattern, tuple(a3, atomEmpty))
		})
		return Delay(ks...)
	default:
		return Error(typeError(ctx, validTypeAtom, atom3))
	}
}

// SubAtom unifies subAtom with a sub atom of length which appears with before runes preceding it and after runes following it.
func SubAtom(ctx context.Context, atom, before, length, after, subAtom Term) *Promise {
	switch whole := Resolve(ctx, atom).(type) {
	case Variable:
		return Error(InstantiationError(ctx))
	case Atom:
		rs := []rune(whole.String())

		if err := checkPositiveInteger(ctx, before); err != nil {
			return Error(err)
		}

		if err := checkPositiveInteger(ctx, length); err != nil {
			return Error(err)
		}

		if err := checkPositiveInteger(ctx, after); err != nil {
			return Error(err)
		}

		switch Resolve(ctx, subAtom).(type) {
		case Variable, Atom:
			break
		default:
			return Error(typeError(ctx, validTypeAtom, subAtom))
		}

		pattern := tuple(before, length, after, subAtom)
		var ks []func() *Promise
		for i := 0; i <= len(rs); i++ {
			for j := i; j <= len(rs); j++ {
				before, length, after, subAtom := Integer(i), Integer(j-i), Integer(len(rs)-j), NewAtom(string(rs[i:j]))
				ks = append(ks, func() *Promise {
					return Unify(ctx, pattern, tuple(before, length, after, subAtom))
				})
			}
		}
		return Delay(ks...)
	default:
		return Error(typeError(ctx, validTypeAtom, atom))
	}
}

func checkPositiveInteger(ctx context.Context, n Term) error {
	switch b := Resolve(ctx, n).(type) {
	case Variable:
		return nil
	case Integer:
		if b < 0 {
			return domainError(ctx, validDomainNotLessThanZero, n)
		}
		return nil
	default:
		return typeError(ctx, validTypeInteger, n)
	}
}

// AtomChars breaks down atom into list of characters and unifies with chars, or constructs an atom from a list of
// characters chars and unifies it with atom.
func AtomChars(ctx context.Context, atom, chars Term) *Promise {
	switch a := Resolve(ctx, atom).(type) {
	case Variable:
		var sb strings.Builder
		iter := ListIterator{List: chars}
		for iter.Next(ctx) {
			switch e := Resolve(ctx, iter.Current()).(type) {
			case Variable:
				return Error(InstantiationError(ctx))
			case Atom:
				if len([]rune(e.String())) != 1 {
					return Error(typeError(ctx, validTypeCharacter, e))
				}
				_, _ = sb.WriteString(e.String())
			default:
				return Error(typeError(ctx, validTypeCharacter, e))
			}
		}
		if err := iter.Err(); err != nil {
			return Error(err)
		}
		return Unify(ctx, atom, NewAtom(sb.String()))
	case Atom:
		iter := ListIterator{List: chars, AllowPartial: true}
		for iter.Next(ctx) {
			switch e := Resolve(ctx, iter.Current()).(type) {
			case Variable:
				break
			case Atom:
				if len([]rune(e.String())) != 1 {
					return Error(typeError(ctx, validTypeCharacter, e))
				}
			default:
				return Error(typeError(ctx, validTypeCharacter, e))
			}
		}
		if err := iter.Err(); err != nil {
			return Error(err)
		}

		s := a.String()
		if s == "" {
			return Unify(ctx, chars, atomEmptyList)
		}
		return Unify(ctx, chars, charList(s))
	default:
		return Error(typeError(ctx, validTypeAtom, a))
	}
}

// AtomCodes breaks up atom into a list of runes and unifies it with codes, or constructs an atom from the list of runes
// and unifies it with atom.
func AtomCodes(ctx context.Context, atom, codes Term) *Promise {
	switch a := Resolve(ctx, atom).(type) {
	case Variable:
		var sb strings.Builder
		iter := ListIterator{List: codes}
		for iter.Next(ctx) {
			switch e := Resolve(ctx, iter.Current()).(type) {
			case Variable:
				return Error(InstantiationError(ctx))
			case Integer:
				if e < 0 || e > unicode.MaxRune {
					return Error(representationError(ctx, flagCharacterCode))
				}
				_, _ = sb.WriteRune(rune(e))
			default:
				return Error(typeError(ctx, validTypeInteger, e))
			}
		}
		if err := iter.Err(); err != nil {
			return Error(err)
		}
		return Unify(ctx, atom, NewAtom(sb.String()))
	case Atom:
		iter := ListIterator{List: codes, AllowPartial: true}
		for iter.Next(ctx) {
			switch e := Resolve(ctx, iter.Current()).(type) {
			case Variable:
				break
			case Integer:
				if e < 0 || e > unicode.MaxRune {
					return Error(representationError(ctx, flagCharacterCode))
				}
			default:
				return Error(typeError(ctx, validTypeInteger, e))
			}
		}
		if err := iter.Err(); err != nil {
			return Error(err)
		}

		s := a.String()
		if s == "" {
			return Unify(ctx, codes, atomEmptyList)
		}
		return Unify(ctx, codes, codeList(s))
	default:
		return Error(typeError(ctx, validTypeAtom, atom))
	}
}

// NumberChars breaks up an atom representation of a number num into a list of characters and unifies it with chars, or
// constructs a number from a list of characters chars and unifies it with num.
func NumberChars(ctx context.Context, num, chars Term) *Promise {
	var sb strings.Builder
	iter := ListIterator{List: chars, AllowPartial: true}
	for iter.Next(ctx) {
		switch e := Resolve(ctx, iter.Current()).(type) {
		case Variable:
			return numberCharsWrite(ctx, num, chars)
		case Atom:
			s := e.String()
			if len([]rune(s)) != 1 {
				return Error(typeError(ctx, validTypeCharacter, e))
			}
			_, _ = sb.WriteString(s)
		default:
			return Error(typeError(ctx, validTypeCharacter, e))
		}
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}
	if _, ok := iter.Suffix().(Variable); ok {
		return numberCharsWrite(ctx, num, chars)
	}

	p := Parser{
		lexer: Lexer{
			input: newRuneRingBuffer(strings.NewReader(sb.String())),
		},
	}
	t, err := p.number()
	if err != nil {
		return Error(syntaxError(ctx, err))
	}

	switch n := Resolve(ctx, num).(type) {
	case Variable, Number:
		return Unify(ctx, n, t)
	default:
		return Error(typeError(ctx, validTypeNumber, n))
	}
}

func numberCharsWrite(ctx context.Context, num, chars Term) *Promise {
	var n Number
	switch num := Resolve(ctx, num).(type) {
	case Variable:
		return Error(InstantiationError(ctx))
	case Number:
		n = num
	default:
		return Error(typeError(ctx, validTypeNumber, num))
	}

	iter := ListIterator{List: chars, AllowPartial: true}
	for iter.Next(ctx) {
		switch e := Resolve(ctx, iter.Current()).(type) {
		case Variable:
			break
		case Atom:
			if len(e.String()) != 1 {
				return Error(typeError(ctx, validTypeCharacter, e))
			}
		default:
			return Error(typeError(ctx, validTypeCharacter, e))
		}
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}

	var buf bytes.Buffer
	_ = writeTerm(ctx, &buf, n, &defaultWriteOptions)
	rs := []rune(buf.String())

	cs := make([]Term, len(rs))
	for i, r := range rs {
		cs[i] = Atom(r)
	}
	return Unify(ctx, chars, List(cs...))
}

// NumberCodes breaks up an atom representation of a number num into a list of runes and unifies it with codes, or
// constructs a number from a list of runes codes and unifies it with num.
func NumberCodes(ctx context.Context, num, codes Term) *Promise {
	var sb strings.Builder
	iter := ListIterator{List: codes, AllowPartial: true}
	for iter.Next(ctx) {
		switch e := Resolve(ctx, iter.Current()).(type) {
		case Variable:
			return numberCodesWrite(ctx, num, codes)
		case Integer:
			if !utf8.ValidRune(rune(e)) {
				return Error(representationError(ctx, flagCharacterCode))
			}
			_, _ = sb.WriteRune(rune(e))
		default:
			return Error(typeError(ctx, validTypeInteger, e))
		}
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}
	if _, ok := iter.Suffix().(Variable); ok {
		return numberCodesWrite(ctx, num, codes)
	}

	p := Parser{
		lexer: Lexer{
			input: newRuneRingBuffer(strings.NewReader(sb.String())),
		},
	}
	t, err := p.number()
	if err != nil {
		return Error(syntaxError(ctx, err))
	}

	switch n := Resolve(ctx, num).(type) {
	case Variable, Number:
		return Unify(ctx, n, t)
	default:
		return Error(typeError(ctx, validTypeNumber, n))
	}
}

func numberCodesWrite(ctx context.Context, num, codes Term) *Promise {
	var n Number
	switch num := Resolve(ctx, num).(type) {
	case Variable:
		return Error(InstantiationError(ctx))
	case Number:
		n = num
	default:
		return Error(typeError(ctx, validTypeNumber, num))
	}

	iter := ListIterator{List: codes, AllowPartial: true}
	for iter.Next(ctx) {
		switch e := Resolve(ctx, iter.Current()).(type) {
		case Variable:
			break
		case Integer:
			if !utf8.ValidRune(rune(e)) {
				return Error(representationError(ctx, flagCharacterCode))
			}
		default:
			return Error(typeError(ctx, validTypeInteger, e))
		}
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}

	var buf bytes.Buffer
	_ = writeTerm(ctx, &buf, n, &defaultWriteOptions)
	rs := []rune(buf.String())

	cs := make([]Term, len(rs))
	for i, r := range rs {
		cs[i] = Integer(r)
	}
	return Unify(ctx, codes, List(cs...))
}

// StreamProperty succeeds iff the stream represented by stream has the stream property.
func StreamProperty(ctx context.Context, stream, property Term) *Promise {
	vm, err := vm(ctx)
	if err != nil {
		return Error(err)
	}

	streams := make([]*Stream, 0, len(vm.streams.elems))
	switch s := Resolve(ctx, stream).(type) {
	case Variable:
		for _, v := range vm.streams.elems {
			streams = append(streams, v)
		}
	case *Stream:
		streams = append(streams, s)
	default:
		return Error(domainError(ctx, validDomainStream, stream))
	}

	if !isStreamProperty(ctx, property) {
		return Error(domainError(ctx, validDomainStreamProperty, property))
	}

	var ks []func() *Promise
	for _, s := range streams {
		s := s
		for _, p := range s.properties() {
			p := p
			ks = append(ks, func() *Promise {
				return Unify(ctx, atomEmpty.Apply(stream, property), atomEmpty.Apply(s, p))
			})
		}
	}
	return Delay(ks...)
}

func isStreamProperty(ctx context.Context, property Term) bool {
	switch p := Resolve(ctx, property).(type) {
	case Variable:
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
			return isAtom(ctx, arg)
		case atomPosition:
			return isInteger(ctx, arg)
		}
		return false
	default:
		return false
	}
}

func isAtom(ctx context.Context, t Term) bool {
	switch Resolve(ctx, t).(type) {
	case Variable, Atom:
		return true
	default:
		return false
	}
}

func isInteger(ctx context.Context, t Term) bool {
	switch Resolve(ctx, t).(type) {
	case Variable, Integer:
		return true
	default:
		return false
	}
}

// SetStreamPosition sets the position property of the stream represented by streamOrAlias.
func SetStreamPosition(ctx context.Context, streamOrAlias, position Term) *Promise {
	s, err := stream(ctx, streamOrAlias)
	if err != nil {
		return Error(err)
	}

	switch p := Resolve(ctx, position).(type) {
	case Variable:
		return Error(InstantiationError(ctx))
	case Integer:
		switch _, err := s.Seek(int64(p), 0); err {
		case nil:
			return Continue(ctx)
		case errReposition:
			return Error(permissionError(ctx, operationReposition, permissionTypeStream, streamOrAlias))
		default:
			return Error(err)
		}
	default:
		return Error(typeError(ctx, validTypeInteger, position))
	}
}

// CharConversion registers a character conversion from inChar to outChar, or remove the conversion if inChar = outChar.
func CharConversion(ctx context.Context, inChar, outChar Term) *Promise {
	switch in := Resolve(ctx, inChar).(type) {
	case Variable:
		return Error(InstantiationError(ctx))
	case Atom:
		i := []rune(in.String())
		if len(i) != 1 {
			return Error(representationError(ctx, flagCharacter))
		}

		switch out := Resolve(ctx, outChar).(type) {
		case Variable:
			return Error(InstantiationError(ctx))
		case Atom:
			o := []rune(out.String())
			if len(o) != 1 {
				return Error(representationError(ctx, flagCharacter))
			}

			vm, err := vm(ctx)
			if err != nil {
				return Error(err)
			}

			if vm.charConversions == nil {
				vm.charConversions = map[rune]rune{}
			}
			if i[0] == o[0] {
				delete(vm.charConversions, i[0])
				return Continue(ctx)
			}
			vm.charConversions[i[0]] = o[0]
			return Continue(ctx)
		default:
			return Error(representationError(ctx, flagCharacter))
		}
	default:
		return Error(representationError(ctx, flagCharacter))
	}
}

// CurrentCharConversion succeeds iff a conversion from inChar to outChar is defined.
func CurrentCharConversion(ctx context.Context, inChar, outChar Term) *Promise {
	switch in := Resolve(ctx, inChar).(type) {
	case Variable:
		break
	case Atom:
		i := []rune(in.String())
		if len(i) != 1 {
			return Error(representationError(ctx, flagCharacter))
		}
	default:
		return Error(representationError(ctx, flagCharacter))
	}

	switch out := Resolve(ctx, outChar).(type) {
	case Variable:
		break
	case Atom:
		o := []rune(out.String())
		if len(o) != 1 {
			return Error(representationError(ctx, flagCharacter))
		}
	default:
		return Error(representationError(ctx, flagCharacter))
	}

	vm, err := vm(ctx)
	if err != nil {
		return Error(err)
	}

	if c1, ok := Resolve(ctx, inChar).(Atom); ok {
		r := []rune(c1.String())
		if r, ok := vm.charConversions[r[0]]; ok {
			return Unify(ctx, outChar, Atom(r))
		}
		return Unify(ctx, outChar, c1)
	}

	pattern := tuple(inChar, outChar)
	ks := make([]func() *Promise, 256)
	for i := 0; i < 256; i++ {
		r := rune(i)
		cr, ok := vm.charConversions[r]
		if !ok {
			cr = r
		}

		ks[i] = func() *Promise {
			return Unify(ctx, pattern, tuple(Atom(r), Atom(cr)))
		}
	}
	return Delay(ks...)
}

// SetPrologFlag sets flag to value.
func SetPrologFlag(ctx context.Context, flag, value Term) *Promise {
	switch f := Resolve(ctx, flag).(type) {
	case Variable:
		return Error(InstantiationError(ctx))
	case Atom:
		var modify func(ctx context.Context, value Atom) error
		switch f {
		case atomBounded, atomMaxInteger, atomMinInteger, atomIntegerRoundingFunction, atomMaxArity:
			return Error(permissionError(ctx, operationModify, permissionTypeFlag, f))
		case atomCharConversion:
			modify = modifyCharConversion
		case atomDebug:
			modify = modifyDebug
		case atomUnknown:
			modify = modifyUnknown
		case atomDoubleQuotes:
			modify = modifyDoubleQuotes
		default:
			return Error(domainError(ctx, validDomainPrologFlag, f))
		}

		switch v := Resolve(ctx, value).(type) {
		case Variable:
			return Error(InstantiationError(ctx))
		case Atom:
			if err := modify(ctx, v); err != nil {
				return Error(err)
			}
			return Continue(ctx)
		default:
			return Error(domainError(ctx, validDomainFlagValue, atomPlus.Apply(flag, value)))
		}
	default:
		return Error(typeError(ctx, validTypeAtom, f))
	}
}

func modifyCharConversion(ctx context.Context, value Atom) error {
	vm, err := vm(ctx)
	if err != nil {
		return err
	}
	switch value {
	case atomOn:
		vm.charConvEnabled = true
	case atomOff:
		vm.charConvEnabled = false
	default:
		return domainError(ctx, validDomainFlagValue, atomPlus.Apply(atomCharConversion, value))
	}
	return nil
}

func modifyDebug(ctx context.Context, value Atom) error {
	vm, err := vm(ctx)
	if err != nil {
		return err
	}
	switch value {
	case atomOn:
		vm.debug = true
	case atomOff:
		vm.debug = false
	default:
		return domainError(ctx, validDomainFlagValue, atomPlus.Apply(atomDebug, value))
	}
	return nil
}

func modifyUnknown(ctx context.Context, value Atom) error {
	vm, err := vm(ctx)
	if err != nil {
		return err
	}
	switch value {
	case atomError:
		vm.unknown = unknownError
	case atomWarning:
		vm.unknown = unknownWarning
	case atomFail:
		vm.unknown = unknownFail
	default:
		return domainError(ctx, validDomainFlagValue, atomPlus.Apply(atomUnknown, value))
	}
	return nil
}

func modifyDoubleQuotes(ctx context.Context, value Atom) error {
	vm, err := vm(ctx)
	if err != nil {
		return err
	}
	switch value {
	case atomCodes:
		vm.doubleQuotes = doubleQuotesCodes
	case atomChars:
		vm.doubleQuotes = doubleQuotesChars
	case atomAtom:
		vm.doubleQuotes = doubleQuotesAtom
	default:
		return domainError(ctx, validDomainFlagValue, atomPlus.Apply(atomDoubleQuotes, value))
	}
	return nil
}

// CurrentPrologFlag succeeds iff flag is set to value.
func CurrentPrologFlag(ctx context.Context, flag, value Term) *Promise {
	switch f := Resolve(ctx, flag).(type) {
	case Variable:
		break
	case Atom:
		switch f {
		case atomBounded, atomMaxInteger, atomMinInteger, atomIntegerRoundingFunction, atomCharConversion, atomDebug, atomMaxArity, atomUnknown, atomDoubleQuotes:
			break
		default:
			return Error(domainError(ctx, validDomainPrologFlag, f))
		}
	default:
		return Error(typeError(ctx, validTypeAtom, f))
	}

	vm, err := vm(ctx)
	if err != nil {
		return Error(err)
	}

	pattern := tuple(flag, value)
	flags := []Term{
		tuple(atomBounded, atomTrue),
		tuple(atomMaxInteger, maxInt),
		tuple(atomMinInteger, minInt),
		tuple(atomIntegerRoundingFunction, atomTowardZero),
		tuple(atomCharConversion, onOff(vm.charConvEnabled)),
		tuple(atomDebug, onOff(vm.debug)),
		tuple(atomMaxArity, atomUnbounded),
		tuple(atomUnknown, NewAtom(vm.unknown.String())),
		tuple(atomDoubleQuotes, NewAtom(vm.doubleQuotes.String())),
	}
	ks := make([]func() *Promise, len(flags))
	for i := range flags {
		f := flags[i]
		ks[i] = func() *Promise {
			return Unify(ctx, pattern, f)
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
func ExpandTerm(ctx context.Context, term1, term2 Term) *Promise {
	t, err := expand(ctx, term1)
	if err != nil {
		return Error(err)
	}

	return Unify(ctx, t, term2)
}

func expand(ctx context.Context, term Term) (Term, error) {
	vm, err := vm(ctx)
	if err != nil {
		return nil, err
	}
	if _, ok := vm.procedures[procedureIndicator{name: atomTermExpansion, arity: 2}]; ok {
		var ret Term
		v := NewVariable()
		ctx := WithCont(ctx, func(ctx context.Context) *Promise {
			env := env(ctx)
			ret = env.simplify(v)
			return Bool(true)
		})
		ok, err := Call(ctx, atomTermExpansion.Apply(term, v)).Force()
		if err != nil {
			return nil, err
		}
		if ok {
			return ret, nil
		}
	}

	t, err := expandDCG(ctx, term)
	if err != nil {
		return term, nil
	}
	return t, err
}

// Nth0 succeeds if elem is the n-th element of list, counting from 0.
func Nth0(ctx context.Context, n, list, elem Term) *Promise {
	return nth(ctx, 0, n, list, elem)
}

// Nth1 succeeds if elem is the n-th element of list, counting from 1.
func Nth1(ctx context.Context, n, list, elem Term) *Promise {
	return nth(ctx, 1, n, list, elem)
}

func nth(ctx context.Context, base Integer, n, list, elem Term) *Promise {
	switch n := Resolve(ctx, n).(type) {
	case Variable:
		var ks []func() *Promise
		iter := ListIterator{List: list}
		for i := base; iter.Next(ctx); i++ {
			i, e := i, iter.Current()
			ks = append(ks, func() *Promise {
				return Unify(ctx, tuple(n, elem), tuple(i, e))
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
		iter := ListIterator{List: list, AllowCycle: true}
		for i := base; iter.Next(ctx); i++ {
			if i == n {
				return Unify(ctx, elem, iter.Current())
			}
		}
		if err := iter.Err(); err != nil {
			return Error(err)
		}
		return Bool(false)
	default:
		return Error(typeError(ctx, validTypeInteger, n))
	}
}

// Succ succeeds if s is the successor of non-negative integer x.
func Succ(ctx context.Context, x, s Term) *Promise {
	switch x := x.(type) {
	case Variable:
		switch s := s.(type) {
		case Variable:
			return Error(InstantiationError(ctx))
		case Integer:
			switch {
			case s < Integer(0):
				return Error(domainError(ctx, validDomainNotLessThanZero, s))
			case s == Integer(0):
				return Bool(false)
			default:
				return Unify(ctx, x, s-Integer(1))
			}
		default:
			return Error(typeError(ctx, validTypeInteger, s))
		}
	case Integer:
		if x < Integer(0) {
			return Error(domainError(ctx, validDomainNotLessThanZero, x))
		}

		r, err := add(x, Integer(1))
		if err != nil {
			var ev exceptionalValue
			if errors.As(err, &ev) {
				return Error(evaluationError(ctx, ev))
			}
			return Error(err)
		}

		switch s := s.(type) {
		case Variable:
			return Unify(ctx, s, r)
		case Integer:
			if s < Integer(0) {
				return Error(domainError(ctx, validDomainNotLessThanZero, s))
			}
			return Unify(ctx, s, r)
		default:
			return Error(typeError(ctx, validTypeInteger, s))
		}
	default:
		return Error(typeError(ctx, validTypeInteger, x))
	}
}

// Length succeeds iff list is a list of length.
func Length(ctx context.Context, list, length Term) *Promise {
	// https://github.com/mthom/scryer-prolog/issues/1325#issue-1160713156
	// Note that it's a bit simpler since we don't have attributed variables (yet).

	n := Resolve(ctx, length)
	switch n := n.(type) {
	case Variable:
		break
	case Integer:
		if n < 0 {
			return Error(domainError(ctx, validDomainNotLessThanZero, n))
		}
	default:
		return Error(typeError(ctx, validTypeInteger, n))
	}

	var (
		skipped = NewVariable()
		suffix  = NewVariable()
	)
	ctx = WithCont(ctx, func(ctx context.Context) *Promise {
		skipped := Resolve(ctx, skipped).(Integer)

		switch suffix := Resolve(ctx, suffix).(type) {
		case Variable: // partial list
			if n, ok := n.(Integer); ok {
				return lengthRundown(ctx, suffix, n-skipped)
			}

			n := n.(Variable)

			if n == suffix {
				return Error(resourceError(ctx, resourceFiniteMemory))
			}

			return lengthAddendum(ctx, atomEmptyList, skipped, suffix, n)
		case Atom: // list or non-list terminated by an atom
			if suffix != atomEmptyList {
				return Bool(false)
			}

			return Unify(ctx, n, skipped)
		case Compound: // non-list including infinite list
			if suffix.Functor() != atomDot || suffix.Arity() != 2 {
				return Bool(false)
			}

			if _, ok := n.(Variable); !ok {
				return Bool(false)
			}

			return Error(resourceError(ctx, resourceFiniteMemory))
		default: // non-list terminated by a term that is neither an atom nor a compound
			return Bool(false)
		}
	})
	return SkipMaxList(ctx, skipped, n, list, suffix)
}

func lengthRundown(ctx context.Context, list Variable, n Integer) *Promise {
	elems, err := makeSlice[Term](int(n))
	if err != nil {
		return Error(resourceError(ctx, resourceMemory))
	}
	for i := range elems {
		elems[i] = NewVariable()
	}
	return Unify(ctx, list, List(elems...))
}

func lengthAddendum(ctx context.Context, suffix Term, offset Integer, list, length Variable) *Promise {
	return Delay(func() *Promise {
		return Unify(ctx, tuple(list, length), tuple(suffix, offset))
	}, func() *Promise {
		suffix := atomDot.Apply(NewVariable(), suffix)
		offset, err := addI(offset, 1)
		if err != nil {
			return Error(representationError(ctx, flagMaxInteger))
		}
		return lengthAddendum(ctx, suffix, offset, list, length)
	})
}

// SkipMaxList iterates over list up to max elements and unifies the number of skipped elements with skip and the rest with suffix.
func SkipMaxList(ctx context.Context, skip, max, list, suffix Term) *Promise {
	m := maxInt
	switch max := Resolve(ctx, max).(type) {
	case Variable:
		break
	case Integer:
		if max < 0 {
			return Error(domainError(ctx, validDomainNotLessThanZero, max))
		}
		m = max
	default:
		return Error(typeError(ctx, validTypeInteger, max))
	}

	var (
		iter = ListIterator{List: list}
		n    = Integer(0)
	)
	for n < m && iter.Next(ctx) {
		n++
	}

	return Unify(ctx, tuple(skip, suffix), tuple(n, iter.Suffix()))
}

// Append succeeds iff zs is the concatenation of lists xs and ys.
func Append(ctx context.Context, xs, ys, zs Term) *Promise {
	// A special case for non-empty lists without a variable in the spine.
	if xs, ok := Resolve(ctx, xs).(Compound); ok {
		iter := ListIterator{List: xs}
		for iter.Next(context.Background()) { // No variables allowed.
		}
		if err := iter.Err(); err == nil {
			return Unify(ctx, zs, &partial{
				Compound: xs,
				tail:     &ys,
			})
		}
	}

	return appendLists(ctx, xs, ys, zs)
}

func appendLists(ctx context.Context, xs, ys, zs Term) *Promise {
	/*
		append([], L, L).
		append([X|L1], L2, [X|L3]) :- append(L1, L2, L3).
	*/
	return Delay(func() *Promise {
		return Unify(ctx, tuple(xs, ys), tuple(List(), zs))
	}, func() *Promise {
		x := NewVariable()
		l1, l3 := NewVariable(), NewVariable()
		ctx := WithCont(ctx, func(ctx context.Context) *Promise {
			return appendLists(ctx, l1, ys, l3)
		})
		return Unify(ctx, tuple(xs, zs), tuple(Cons(x, l1), Cons(x, l3)))
	})
}
