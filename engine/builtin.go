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
func Repeat(_ *VM, k func(*Env) *Promise, env *Env) *Promise {
	return repeat(func(ctx context.Context) *Promise {
		return k(env)
	})
}

// Negation calls goal and returns false if it succeeds. Otherwise, invokes the continuation.
func Negation(vm *VM, goal Term, k func(*Env) *Promise, env *Env) *Promise {
	return Delay(func(ctx context.Context) *Promise {
		ok, err := Call(vm, goal, Success, env).Force(ctx)
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
func Call(vm *VM, goal Term, k func(*Env) *Promise, env *Env) *Promise {
	switch g := env.Resolve(goal).(type) {
	case Variable:
		return Error(instantiationError(env))
	default:
		fvs := env.FreeVariables(g)
		args := make([]Term, len(fvs))
		for i, fv := range fvs {
			args[i] = fv
		}
		cs, err := compile(atomIf.Apply(tuple(args...), g), env)
		if err != nil {
			return Error(err)
		}

		u := userDefined{clauses: cs}
		return u.call(vm, args, k, env)
	}
}

// Call1 succeeds if closure with an additional argument succeeds.
func Call1(vm *VM, closure, arg1 Term, k func(*Env) *Promise, env *Env) *Promise {
	return callN(vm, closure, []Term{arg1}, k, env)
}

// Call2 succeeds if closure with 2 additional arguments succeeds.
func Call2(vm *VM, closure, arg1, arg2 Term, k func(*Env) *Promise, env *Env) *Promise {
	return callN(vm, closure, []Term{arg1, arg2}, k, env)
}

// Call3 succeeds if closure with 3 additional arguments succeeds.
func Call3(vm *VM, closure, arg1, arg2, arg3 Term, k func(*Env) *Promise, env *Env) *Promise {
	return callN(vm, closure, []Term{arg1, arg2, arg3}, k, env)
}

// Call4 succeeds if closure with 4 additional arguments succeeds.
func Call4(vm *VM, closure, arg1, arg2, arg3, arg4 Term, k func(*Env) *Promise, env *Env) *Promise {
	return callN(vm, closure, []Term{arg1, arg2, arg3, arg4}, k, env)
}

// Call5 succeeds if closure with 5 additional arguments succeeds.
func Call5(vm *VM, closure, arg1, arg2, arg3, arg4, arg5 Term, k func(*Env) *Promise, env *Env) *Promise {
	return callN(vm, closure, []Term{arg1, arg2, arg3, arg4, arg5}, k, env)
}

// Call6 succeeds if closure with 6 additional arguments succeeds.
func Call6(vm *VM, closure, arg1, arg2, arg3, arg4, arg5, arg6 Term, k func(*Env) *Promise, env *Env) *Promise {
	return callN(vm, closure, []Term{arg1, arg2, arg3, arg4, arg5, arg6}, k, env)
}

// Call7 succeeds if closure with 7 additional arguments succeeds.
func Call7(vm *VM, closure, arg1, arg2, arg3, arg4, arg5, arg6, arg7 Term, k func(*Env) *Promise, env *Env) *Promise {
	return callN(vm, closure, []Term{arg1, arg2, arg3, arg4, arg5, arg6, arg7}, k, env)
}

func callN(vm *VM, closure Term, additional []Term, k func(*Env) *Promise, env *Env) *Promise {
	pi, arg, err := piArg(closure, env)
	if err != nil {
		return Error(err)
	}
	args := make([]Term, pi.arity, int(pi.arity)+len(additional))
	for i := 0; i < int(pi.arity); i++ {
		args[i] = arg(i)
	}
	args = append(args, additional...)
	return Call(vm, pi.name.Apply(args...), k, env)
}

// CallNth succeeds iff goal succeeds and nth unifies with the number of re-execution.
// See http://www.complang.tuwien.ac.at/ulrich/iso-prolog/call_nth
func CallNth(vm *VM, goal, nth Term, k func(*Env) *Promise, env *Env) *Promise {
	nth = env.Resolve(nth)
	switch nth := nth.(type) {
	case Variable:
		break
	case Integer:
		switch {
		case nth < 0:
			return Error(domainError(validDomainNotLessThanZero, nth, env))
		case nth == 0:
			return Bool(false)
		}
	default:
		return Error(typeError(validTypeInteger, nth, env))
	}

	var (
		p         *Promise
		n         Integer
		err       error
		parentEnv = env
	)
	p = Call(vm, goal, func(env *Env) *Promise {
		n, err = addI(n, Integer(1))
		if err != nil {
			return Error(representationError(flagMaxInteger, parentEnv))
		}

		u := Unify(vm, n, nth, k, env)
		if nth, ok := nth.(Integer); ok && nth <= n {
			return cut(p, func(context.Context) *Promise {
				return u
			})
		}
		return u
	}, env)
	return p
}

// Unify unifies x and y without occurs check (i.e., X = f(X) is allowed).
func Unify(_ *VM, x, y Term, k func(*Env) *Promise, env *Env) *Promise {
	env, ok := env.Unify(x, y, false)
	if !ok {
		return Bool(false)
	}
	return k(env)
}

// UnifyWithOccursCheck unifies x and y with occurs check (i.e., X = f(X) is not allowed).
func UnifyWithOccursCheck(_ *VM, x, y Term, k func(*Env) *Promise, env *Env) *Promise {
	env, ok := env.Unify(x, y, true)
	if !ok {
		return Bool(false)
	}
	return k(env)
}

// SubsumesTerm succeeds if general and specific are unifiable without binding variables in specific.
func SubsumesTerm(_ *VM, general, specific Term, k func(*Env) *Promise, env *Env) *Promise {
	theta, ok := env.Unify(general, specific, true)
	if !ok {
		return Bool(false)
	}

	if d := env.Compare(theta.Simplify(general), specific); d != 0 {
		return Bool(false)
	}

	return k(env)
}

// TypeVar checks if t is a variable.
func TypeVar(_ *VM, t Term, k func(*Env) *Promise, env *Env) *Promise {
	if _, ok := env.Resolve(t).(Variable); !ok {
		return Bool(false)
	}
	return k(env)
}

// TypeFloat checks if t is a floating-point number.
func TypeFloat(_ *VM, t Term, k func(*Env) *Promise, env *Env) *Promise {
	if _, ok := env.Resolve(t).(Float); !ok {
		return Bool(false)
	}
	return k(env)
}

// TypeInteger checks if t is an integer.
func TypeInteger(_ *VM, t Term, k func(*Env) *Promise, env *Env) *Promise {
	if _, ok := env.Resolve(t).(Integer); !ok {
		return Bool(false)
	}
	return k(env)
}

// TypeAtom checks if t is an atom.
func TypeAtom(_ *VM, t Term, k func(*Env) *Promise, env *Env) *Promise {
	if _, ok := env.Resolve(t).(Atom); !ok {
		return Bool(false)
	}
	return k(env)
}

// TypeCompound checks if t is a compound term.
func TypeCompound(_ *VM, t Term, k func(*Env) *Promise, env *Env) *Promise {
	if _, ok := env.Resolve(t).(Compound); !ok {
		return Bool(false)
	}
	return k(env)
}

// AcyclicTerm checks if t is acyclic.
func AcyclicTerm(_ *VM, t Term, k func(*Env) *Promise, env *Env) *Promise {
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

	if c, ok := t.(Compound); ok {
		for i := 0; i < c.Arity(); i++ {
			if cyclicTerm(c.Arg(i), visited, env) {
				return true
			}
		}
	}

	return false
}

// Functor extracts the name and arity of term, or unifies term with an atomic/compound term of name and arity with
// fresh variables as arguments.
func Functor(vm *VM, t, name, arity Term, k func(*Env) *Promise, env *Env) *Promise {
	switch t := env.Resolve(t).(type) {
	case Variable:
		switch arity := env.Resolve(arity).(type) {
		case Variable:
			return Error(instantiationError(env))
		case Integer:
			if arity < 0 {
				return Error(domainError(validDomainNotLessThanZero, arity, env))
			}

			name := env.Resolve(name)

			switch name := name.(type) {
			case Variable:
				return Error(instantiationError(env))
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

			vs := make([]Term, arity)
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

// Arg extracts nth argument of term as arg, or finds the argument position of arg in term as nth.
func Arg(vm *VM, nth, t, arg Term, k func(*Env) *Promise, env *Env) *Promise {
	switch c := env.Resolve(t).(type) {
	case Variable:
		return Error(instantiationError(env))
	case Compound:
		switch n := env.Resolve(nth).(type) {
		case Variable:
			return Error(instantiationError(env))
		case Integer:
			if n == 0 || int(n) > c.Arity() {
				return Bool(false)
			}
			if n < 0 {
				return Error(domainError(validDomainNotLessThanZero, n, env))
			}
			return Unify(vm, arg, c.Arg(int(n)-1), k, env)
		default:
			return Error(typeError(validTypeInteger, n, env))
		}
	default:
		return Error(typeError(validTypeCompound, t, env))
	}
}

// Univ constructs list as a list which first element is the functor of term and the rest is the arguments of term, or construct a compound from list as term.
func Univ(vm *VM, t, list Term, k func(*Env) *Promise, env *Env) *Promise {
	switch t := env.Resolve(t).(type) {
	case Variable:
		elems, err := slice(list, env)
		if err != nil {
			return Error(err)
		}
		switch len(elems) {
		case 0:
			return Error(domainError(validDomainNonEmptyList, list, env))
		case 1:
			switch e := env.Resolve(elems[0]).(type) {
			case Variable:
				return Error(instantiationError(env))
			case Compound:
				return Error(typeError(validTypeAtomic, e, env))
			default:
				return k(env.Bind(t, e))
			}
		default:
			switch e := env.Resolve(elems[0]).(type) {
			case Variable:
				return Error(instantiationError(env))
			case Atom:
				return k(env.Bind(t, e.Apply(elems[1:]...)))
			default:
				return Error(typeError(validTypeAtom, e, env))
			}
		}
	case Compound:
		iter := ListIterator{List: list, Env: env, AllowPartial: true}
		for iter.Next() {
		}
		if err := iter.Err(); err != nil {
			return Error(err)
		}
		elems := []Term{t.Functor()}
		for i := 0; i < t.Arity(); i++ {
			elems = append(elems, t.Arg(i))
		}
		return Unify(vm, list, List(elems...), k, env)
	default:
		iter := ListIterator{List: list, Env: env, AllowPartial: true}
		for iter.Next() {
		}
		if err := iter.Err(); err != nil {
			return Error(err)
		}
		return Unify(vm, list, List(t), k, env)
	}
}

// CopyTerm clones in as out.
func CopyTerm(vm *VM, in, out Term, k func(*Env) *Promise, env *Env) *Promise {
	return Unify(vm, renamedCopy(in, nil, env), out, k, env)
}

func renamedCopy(t Term, copied map[termID]Term, env *Env) Term {
	if copied == nil {
		copied = map[termID]Term{}
	}
	t = env.Resolve(t)
	if c, ok := copied[id(t)]; ok {
		return c
	}
	switch t := t.(type) {
	case Variable:
		v := NewVariable()
		copied[t] = v
		return v
	case charList, codeList:
		return t
	case list:
		l := make(list, len(t))
		for i := range t {
			l[i] = renamedCopy(t[i], copied, env)
		}
		return l
	case Compound:
		c := compound{
			functor: t.Functor(),
			args:    make([]Term, t.Arity()),
		}
		copied[id(t)] = &c
		for i := 0; i < t.Arity(); i++ {
			c.args[i] = renamedCopy(t.Arg(i), copied, env)
		}
		return &c
	default:
		return t
	}
}

// TermVariables succeeds if vars unifies with a list of variables in term.
func TermVariables(vm *VM, term, vars Term, k func(*Env) *Promise, env *Env) *Promise {
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
		case Compound:
			args := make([]Term, t.Arity())
			for i := 0; i < t.Arity(); i++ {
				args[i] = t.Arg(i)
			}
			traverse = append(args, traverse...)
		}
	}

	iter := ListIterator{List: vars, Env: env, AllowPartial: true}
	for iter.Next() {
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}

	return Unify(vm, vars, List(ret...), k, env)
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
func Op(vm *VM, priority, specifier, op Term, k func(*Env) *Promise, env *Env) *Promise {
	var p Integer
	switch priority := env.Resolve(priority).(type) {
	case Variable:
		return Error(instantiationError(env))
	case Integer:
		if priority < 0 || priority > 1200 {
			return Error(domainError(validDomainOperatorPriority, priority, env))
		}
		p = priority
	default:
		return Error(typeError(validTypeInteger, priority, env))
	}

	var spec operatorSpecifier
	switch specifier := env.Resolve(specifier).(type) {
	case Variable:
		return Error(instantiationError(env))
	case Atom:
		var ok bool
		spec, ok = operatorSpecifiers[specifier]
		if !ok {
			return Error(domainError(validDomainOperatorSpecifier, specifier, env))
		}
	default:
		return Error(typeError(validTypeAtom, specifier, env))
	}

	var names []Atom
	switch op := env.Resolve(op).(type) {
	case Atom:
		names = []Atom{op}
	default:
		iter := ListIterator{List: op, Env: env}
		for iter.Next() {
			switch op := env.Resolve(iter.Current()).(type) {
			case Atom:
				names = appendUniqNewAtom(names, op)
			default:
				return Error(typeError(validTypeAtom, op, env))
			}
		}
		if err := iter.Err(); err != nil {
			return Error(err)
		}
	}

	for _, name := range names {
		if p := validateOp(vm, p, spec, name, env); p != nil {
			return p
		}
	}

	for _, name := range names {
		if class := spec.class(); vm.operators.definedInClass(name, spec.class()) {
			vm.operators.remove(name, class)
		}

		vm.operators.define(p, spec, name)
	}

	return k(env)
}

func validateOp(vm *VM, p Integer, spec operatorSpecifier, name Atom, env *Env) *Promise {
	switch name {
	case atomComma:
		if vm.operators.definedInClass(name, operatorClassInfix) {
			return Error(permissionError(operationModify, permissionTypeOperator, name, env))
		}
	case atomBar:
		if spec.class() != operatorClassInfix || (p > 0 && p < 1001) {
			op := operationCreate
			if vm.operators.definedInClass(name, operatorClassInfix) {
				op = operationModify
			}
			return Error(permissionError(op, permissionTypeOperator, name, env))
		}
	case atomEmptyBlock, atomEmptyList:
		return Error(permissionError(operationCreate, permissionTypeOperator, name, env))
	}

	// 6.3.4.3 There shall not be an infix and a postfix Operator with the same name.
	switch spec.class() {
	case operatorClassInfix:
		if vm.operators.definedInClass(name, operatorClassPostfix) {
			return Error(permissionError(operationCreate, permissionTypeOperator, name, env))
		}
	case operatorClassPostfix:
		if vm.operators.definedInClass(name, operatorClassInfix) {
			return Error(permissionError(operationCreate, permissionTypeOperator, name, env))
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
func CurrentOp(vm *VM, priority, specifier, op Term, k func(*Env) *Promise, env *Env) *Promise {
	switch p := env.Resolve(priority).(type) {
	case Variable:
		break
	case Integer:
		if p < 0 || p > 1200 {
			return Error(domainError(validDomainOperatorPriority, priority, env))
		}
	default:
		return Error(domainError(validDomainOperatorPriority, priority, env))
	}

	switch s := env.Resolve(specifier).(type) {
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
			return Error(domainError(validDomainOperatorSpecifier, s, env))
		}
	default:
		return Error(domainError(validDomainOperatorSpecifier, s, env))
	}

	switch env.Resolve(op).(type) {
	case Variable, Atom:
		break
	default:
		return Error(typeError(validTypeAtom, op, env))
	}

	pattern := tuple(priority, specifier, op)
	ks := make([]func(context.Context) *Promise, 0, len(vm.operators)*int(_operatorClassLen))
	for _, ops := range vm.operators {
		for _, op := range ops {
			op := op
			if op == (operator{}) {
				continue
			}
			ks = append(ks, func(context.Context) *Promise {
				return Unify(vm, pattern, tuple(op.priority, op.specifier.term(), op.name), k, env)
			})
		}
	}
	return Delay(ks...)
}

// Assertz appends t to the database.
func Assertz(vm *VM, t Term, k func(*Env) *Promise, env *Env) *Promise {
	if err := assertMerge(vm, t, func(existing, new []clause) []clause {
		return append(existing, new...)
	}, env); err != nil {
		return Error(err)
	}
	return k(env)
}

// Asserta prepends t to the database.
func Asserta(vm *VM, t Term, k func(*Env) *Promise, env *Env) *Promise {
	if err := assertMerge(vm, t, func(existing, new []clause) []clause {
		return append(new, existing...)
	}, env); err != nil {
		return Error(err)
	}
	return k(env)
}

func assertMerge(vm *VM, t Term, merge func([]clause, []clause) []clause, env *Env) error {
	pi, arg, err := piArg(t, env)
	if err != nil {
		return err
	}

	if pi == (procedureIndicator{name: atomIf, arity: 2}) {
		pi, _, err = piArg(arg(0), env)
		if err != nil {
			return err
		}
	}

	if vm.procedures == nil {
		vm.procedures = map[procedureIndicator]procedure{}
	}
	p, ok := vm.procedures[pi]
	if !ok {
		p = &userDefined{dynamic: true}
		vm.procedures[pi] = p
	}

	added, err := compile(t, env)
	if err != nil {
		return err
	}

	u, ok := p.(*userDefined)
	if !ok || !u.dynamic {
		return permissionError(operationModify, permissionTypeStaticProcedure, pi.Term(), env)
	}

	u.clauses = merge(u.clauses, added)
	return nil
}

// BagOf collects all the solutions of goal as instances, which unify with template. instances may contain duplications.
func BagOf(vm *VM, template, goal, instances Term, k func(*Env) *Promise, env *Env) *Promise {
	return collectionOf(vm, func(tList []Term, env *Env) Term {
		return List(tList...)
	}, template, goal, instances, k, env)
}

// SetOf collects all the solutions of goal as instances, which unify with template. instances don't contain duplications.
func SetOf(vm *VM, template, goal, instances Term, k func(*Env) *Promise, env *Env) *Promise {
	return collectionOf(vm, func(tList []Term, env *Env) Term {
		return env.Set(tList...)
	}, template, goal, instances, k, env)
}

func collectionOf(vm *VM, agg func([]Term, *Env) Term, template, goal, instances Term, k func(*Env) *Promise, env *Env) *Promise {
	fvs := newFreeVariablesSet(goal, template, env)
	w := make([]Term, 0, len(fvs))
	for v := range fvs {
		w = append(w, v)
	}
	sort.Slice(w, func(i, j int) bool {
		return w[i].(Variable) < w[j].(Variable)
	})
	witness := tuple(w...)
	g := iteratedGoalTerm(goal, env)
	s := Term(NewVariable())

	iter := ListIterator{List: instances, Env: env, AllowPartial: true}
	for iter.Next() {
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}

	return FindAll(vm, atomPlus.Apply(witness, template), g, s, func(env *Env) *Promise {
		s, _ := slice(s, env)
		ks := make([]func(context.Context) *Promise, 0, len(s))
		for len(s) > 0 {
			var wt Compound
			wt, s = s[0].(Compound), s[1:]
			w, t := wt.Arg(0), wt.Arg(1) // W+T
			wList, tList := []Term{w}, []Term{t}
			n := 0 // https://github.com/golang/go/wiki/SliceTricks#filter-in-place
			for _, e := range s {
				e := e.(Compound)
				ww, tt := e.Arg(0), e.Arg(1) // WW+TT
				if variant(ww, w, env) {
					wList = append(wList, ww)
					tList = append(tList, tt)
				} else { // keep
					s[n] = e
					n++
				}
			}
			s = s[:n]
			ks = append(ks, func(context.Context) *Promise {
				env := env
				for _, w = range wList {
					env, _ = env.Unify(witness, w, false)
				}
				return Unify(vm, agg(tList, env), instances, k, env)
			})
		}
		return Delay(ks...)
	}, env)
}

// FindAll collects all the solutions of goal as instances, which unify with template. instances may contain duplications.
func FindAll(vm *VM, template, goal, instances Term, k func(*Env) *Promise, env *Env) *Promise {
	iter := ListIterator{List: instances, Env: env, AllowPartial: true}
	for iter.Next() {
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}
	return Delay(func(ctx context.Context) *Promise {
		var answers []Term
		if _, err := Call(vm, goal, func(env *Env) *Promise {
			answers = append(answers, renamedCopy(template, nil, env))
			return Bool(false) // ask for more solutions
		}, env).Force(ctx); err != nil {
			return Error(err)
		}
		return Unify(vm, instances, List(answers...), k, env)
	})
}

// Compare compares term1 and term2 and unifies order with <, =, or >.
func Compare(vm *VM, order, term1, term2 Term, k func(*Env) *Promise, env *Env) *Promise {
	switch o := env.Resolve(order).(type) {
	case Variable:
		break
	case Atom:
		switch o {
		case atomLessThan, atomEqual, atomGreaterThan:
			break
		default:
			return Error(domainError(validDomainOrder, order, env))
		}
	default:
		return Error(typeError(validTypeAtom, order, env))
	}

	switch o := env.Compare(term1, term2); o {
	case 1:
		return Unify(vm, atomGreaterThan, order, k, env)
	case -1:
		return Unify(vm, atomLessThan, order, k, env)
	default:
		return Unify(vm, atomEqual, order, k, env)
	}
}

// Between succeeds when lower, upper, and value are all integers, and lower <= value <= upper.
// If value is a variable, it is unified with successive integers from lower to upper.
func Between(vm *VM, lower, upper, value Term, k func(*Env) *Promise, env *Env) *Promise {
	var low, high Integer

	switch lower := env.Resolve(lower).(type) {
	case Integer:
		low = lower
	case Variable:
		return Error(instantiationError(env))
	default:
		return Error(typeError(validTypeInteger, lower, env))
	}

	switch upper := env.Resolve(upper).(type) {
	case Integer:
		high = upper
	case Variable:
		return Error(instantiationError(env))
	default:
		return Error(typeError(validTypeInteger, upper, env))
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
			return Unify(vm, value, low, k, env)
		})
		if low < high {
			ks = append(ks, func(context.Context) *Promise {
				return Between(vm, low+1, upper, value, k, env)
			})
		}
		return Delay(ks...)
	default:
		return Error(typeError(validTypeInteger, value, env))
	}
}

// Sort succeeds if sorted list of elements of list unifies with sorted.
func Sort(vm *VM, list, sorted Term, k func(*Env) *Promise, env *Env) *Promise {
	var elems []Term
	iter := ListIterator{List: list, Env: env}
	for iter.Next() {
		elems = append(elems, env.Resolve(iter.Current()))
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}

	iter = ListIterator{List: sorted, Env: env, AllowPartial: true}
	for iter.Next() {
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}

	return Unify(vm, sorted, env.Set(elems...), k, env)
}

// KeySort succeeds if sorted is a sorted list of pairs based on their keys.
func KeySort(vm *VM, pairs, sorted Term, k func(*Env) *Promise, env *Env) *Promise {
	var elems []Term
	iter := ListIterator{List: pairs, Env: env}
	for iter.Next() {
		switch e := env.Resolve(iter.Current()).(type) {
		case Variable:
			return Error(instantiationError(env))
		case Compound:
			if e.Functor() != atomMinus || e.Arity() != 2 {
				return Error(typeError(validTypePair, e, env))
			}
			elems = append(elems, e)
		default:
			return Error(typeError(validTypePair, e, env))
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
			case Compound:
				if e.Functor() != atomMinus || e.Arity() != 2 {
					return Error(typeError(validTypePair, e, env))
				}
			default:
				return Error(typeError(validTypePair, e, env))
			}
		}
		if err := iter.Err(); err != nil {
			return Error(err)
		}
	}

	sort.SliceStable(elems, func(i, j int) bool {
		return env.Compare(elems[i].(Compound).Arg(0), elems[j].(Compound).Arg(0)) == -1
	})

	return Unify(vm, sorted, List(elems...), k, env)
}

// Throw throws ball as an exception.
func Throw(_ *VM, ball Term, _ func(*Env) *Promise, env *Env) *Promise {
	switch b := env.Resolve(ball).(type) {
	case Variable:
		return Error(instantiationError(env))
	default:
		return Error(NewException(b, env))
	}
}

// Catch calls goal. If an exception is thrown and unifies with catcher, it calls recover.
func Catch(vm *VM, goal, catcher, recover Term, k func(*Env) *Promise, env *Env) *Promise {
	return catch(func(err error) *Promise {
		e, ok := env.Resolve(err).(Exception)
		if !ok {
			e = Exception{term: atomError.Apply(NewAtom("system_error"), NewAtom(err.Error()))}
		}

		env, ok := env.Unify(catcher, e.term, false)
		if !ok {
			return nil
		}

		return Call(vm, recover, k, env)
	}, func(ctx context.Context) *Promise {
		return Call(vm, goal, k, env)
	})
}

// CurrentPredicate matches pi with a predicate indicator of the user-defined procedures in the database.
func CurrentPredicate(vm *VM, pi Term, k func(*Env) *Promise, env *Env) *Promise {
	switch pi := env.Resolve(pi).(type) {
	case Variable:
		break
	case Compound:
		if pi.Functor() != atomSlash || pi.Arity() != 2 {
			return Error(typeError(validTypePredicateIndicator, pi, env))
		}
		if _, ok := env.Resolve(pi.Arg(0)).(Atom); !ok {
			return Error(typeError(validTypePredicateIndicator, pi, env))
		}
		if _, ok := env.Resolve(pi.Arg(1)).(Integer); !ok {
			return Error(typeError(validTypePredicateIndicator, pi, env))
		}
	default:
		return Error(typeError(validTypePredicateIndicator, pi, env))
	}

	ks := make([]func(context.Context) *Promise, 0, len(vm.procedures))
	for key, p := range vm.procedures {
		switch p.(type) {
		case *userDefined:
			c := key.Term()
			ks = append(ks, func(context.Context) *Promise {
				return Unify(vm, pi, c, k, env)
			})
		default:
			continue
		}
	}
	return Delay(ks...)
}

// Retract removes the first clause that matches with t.
func Retract(vm *VM, t Term, k func(*Env) *Promise, env *Env) *Promise {
	t = rulify(t, env)

	h := t.(Compound).Arg(0)
	pi, _, err := piArg(h, env)
	if err != nil {
		return Error(err)
	}

	p, ok := vm.procedures[pi]
	if !ok {
		return Bool(false)
	}

	u, ok := p.(*userDefined)
	if !ok || !u.dynamic {
		return Error(permissionError(operationModify, permissionTypeStaticProcedure, pi.Term(), env))
	}

	deleted := 0
	ks := make([]func(context.Context) *Promise, len(u.clauses))
	for i, c := range u.clauses {
		i := i
		raw := rulify(c.raw, env)
		ks[i] = func(_ context.Context) *Promise {
			return Unify(vm, t, raw, func(env *Env) *Promise {
				j := i - deleted
				u.clauses, u.clauses[len(u.clauses)-1] = append(u.clauses[:j], u.clauses[j+1:]...), clause{}
				deleted++
				return k(env)
			}, env)
		}
	}
	return Delay(ks...)
}

// Abolish removes the procedure indicated by pi from the database.
func Abolish(vm *VM, pi Term, k func(*Env) *Promise, env *Env) *Promise {
	switch pi := env.Resolve(pi).(type) {
	case Variable:
		return Error(instantiationError(env))
	case Compound:
		if pi.Functor() != atomSlash || pi.Arity() != 2 {
			return Error(typeError(validTypePredicateIndicator, pi, env))
		}

		name, arity := pi.Arg(0), pi.Arg(1)

		switch name := env.Resolve(name).(type) {
		case Variable:
			return Error(instantiationError(env))
		case Atom:
			switch arity := env.Resolve(arity).(type) {
			case Variable:
				return Error(instantiationError(env))
			case Integer:
				if arity < 0 {
					return Error(domainError(validDomainNotLessThanZero, arity, env))
				}
				key := procedureIndicator{name: name, arity: arity}
				if u, ok := vm.procedures[key].(*userDefined); !ok || !u.dynamic {
					return Error(permissionError(operationModify, permissionTypeStaticProcedure, key.Term(), env))
				}
				delete(vm.procedures, key)
				return k(env)
			default:
				return Error(typeError(validTypeInteger, arity, env))
			}
		default:
			return Error(typeError(validTypeAtom, name, env))
		}
	default:
		return Error(typeError(validTypePredicateIndicator, pi, env))
	}
}

// CurrentInput unifies stream with the current input stream.
func CurrentInput(vm *VM, stream Term, k func(*Env) *Promise, env *Env) *Promise {
	switch env.Resolve(stream).(type) {
	case Variable, *Stream:
		return Unify(vm, stream, vm.input, k, env)
	default:
		return Error(domainError(validDomainStream, stream, env))
	}
}

// CurrentOutput unifies stream with the current output stream.
func CurrentOutput(vm *VM, stream Term, k func(*Env) *Promise, env *Env) *Promise {
	switch env.Resolve(stream).(type) {
	case Variable, *Stream:
		return Unify(vm, stream, vm.output, k, env)
	default:
		return Error(domainError(validDomainStream, stream, env))
	}
}

// SetInput sets streamOrAlias as the current input stream.
func SetInput(vm *VM, streamOrAlias Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := stream(vm, streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	if s.mode != ioModeRead {
		return Error(permissionError(operationInput, permissionTypeStream, streamOrAlias, env))
	}

	vm.input = s
	return k(env)
}

// SetOutput sets streamOrAlias as the current output stream.
func SetOutput(vm *VM, streamOrAlias Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := stream(vm, streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	if s.mode != ioModeWrite && s.mode != ioModeAppend {
		return Error(permissionError(operationOutput, permissionTypeStream, streamOrAlias, env))
	}

	vm.output = s
	return k(env)
}

func stream(vm *VM, streamOrAlias Term, env *Env) (*Stream, error) {
	switch s := env.Resolve(streamOrAlias).(type) {
	case Variable:
		return nil, instantiationError(env)
	case Atom:
		v, ok := vm.streams.lookup(s)
		if !ok {
			return nil, existenceError(objectTypeStream, streamOrAlias, env)
		}
		return v, nil
	case *Stream:
		return s, nil
	default:
		return nil, domainError(validDomainStreamOrAlias, streamOrAlias, env)
	}
}

var openFile = os.OpenFile

// Open opens SourceSink in mode and unifies with stream.
func Open(vm *VM, sourceSink, mode, stream, options Term, k func(*Env) *Promise, env *Env) *Promise {
	var name string
	switch s := env.Resolve(sourceSink).(type) {
	case Variable:
		return Error(instantiationError(env))
	case Atom:
		name = s.String()
	default:
		return Error(domainError(validDomainSourceSink, sourceSink, env))
	}

	var streamMode ioMode
	switch m := env.Resolve(mode).(type) {
	case Variable:
		return Error(instantiationError(env))
	case Atom:
		var ok bool
		streamMode, ok = map[Atom]ioMode{
			atomRead:   ioModeRead,
			atomWrite:  ioModeWrite,
			atomAppend: ioModeAppend,
		}[m]
		if !ok {
			return Error(domainError(validDomainIOMode, m, env))
		}
	default:
		return Error(typeError(validTypeAtom, mode, env))
	}

	if _, ok := env.Resolve(stream).(Variable); !ok {
		return Error(instantiationError(env))
	}

	s := Stream{vm: vm, mode: streamMode}
	switch f, err := openFile(name, int(s.mode), 0644); {
	case err == nil:
		s.sourceSink = f
		if fi, err := f.Stat(); err == nil {
			s.reposition = fi.Mode()&fs.ModeType == 0
		}
	case os.IsNotExist(err):
		return Error(existenceError(objectTypeSourceSink, sourceSink, env))
	case os.IsPermission(err):
		return Error(permissionError(operationOpen, permissionTypeSourceSink, sourceSink, env))
	default:
		return Error(err)
	}

	iter := ListIterator{List: options, Env: env}
	for iter.Next() {
		if err := handleStreamOption(vm, &s, iter.Current(), env); err != nil {
			return Error(err)
		}
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}

	if err := s.initRead(); err == nil {
		s.checkEOS()
	}

	return Unify(vm, stream, &s, k, env)
}

func handleStreamOption(vm *VM, s *Stream, option Term, env *Env) error {
	switch o := env.Resolve(option).(type) {
	case Variable:
		return instantiationError(env)
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

func handleStreamOptionAlias(vm *VM, s *Stream, o Compound, env *Env) error {
	switch a := env.Resolve(o.Arg(0)).(type) {
	case Variable:
		return instantiationError(env)
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

func handleStreamOptionType(_ *VM, s *Stream, o Compound, env *Env) error {
	switch t := env.Resolve(o.Arg(0)).(type) {
	case Variable:
		return instantiationError(env)
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
	return domainError(validDomainStreamOption, o, env)
}

func handleStreamOptionReposition(_ *VM, s *Stream, o Compound, env *Env) error {
	switch r := env.Resolve(o.Arg(0)).(type) {
	case Variable:
		return instantiationError(env)
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

func handleStreamOptionEOFAction(_ *VM, s *Stream, o Compound, env *Env) error {
	switch e := env.Resolve(o.Arg(0)).(type) {
	case Variable:
		return instantiationError(env)
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
	return domainError(validDomainStreamOption, o, env)
}

// Close closes a stream specified by streamOrAlias.
func Close(vm *VM, streamOrAlias, options Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := stream(vm, streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	var force bool
	iter := ListIterator{List: options, Env: env}
	for iter.Next() {
		switch option := env.Resolve(iter.Current()).(type) {
		case Variable:
			return Error(instantiationError(env))
		case Compound:
			switch option.Functor() {
			case atomForce:
				if option.Arity() != 1 {
					return Error(domainError(validDomainStreamOption, option, env))
				}

				switch v := env.Resolve(option.Arg(0)).(type) {
				case Atom:
					switch v {
					case atomFalse:
						force = false
					case atomTrue:
						force = true
					default:
						return Error(domainError(validDomainStreamOption, option, env))
					}
				default:
					return Error(domainError(validDomainStreamOption, option, env))
				}
			}
		default:
			return Error(domainError(validDomainStreamOption, option, env))
		}
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}

	if err := s.Close(); err != nil && !force {
		return Error(err)
	}

	return k(env)
}

// FlushOutput sends any buffered output to the stream.
func FlushOutput(vm *VM, streamOrAlias Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := stream(vm, streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	switch err := s.Flush(); err {
	case nil:
		return k(env)
	case errWrongIOMode:
		return Error(permissionError(operationOutput, permissionTypeStream, streamOrAlias, env))
	default:
		return Error(err)
	}
}

// WriteTerm outputs term to stream with options.
func WriteTerm(vm *VM, streamOrAlias, t, options Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := stream(vm, streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	opts := writeOptions{
		ops:      vm.operators,
		priority: 1200,
	}
	iter := ListIterator{List: options, Env: env}
	for iter.Next() {
		if err := writeTermOption(&opts, iter.Current(), env); err != nil {
			return Error(err)
		}
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}

	switch err := writeTerm(s, env.Resolve(t), &opts, env); err {
	case nil:
		return k(env)
	case errWrongIOMode:
		return Error(permissionError(operationOutput, permissionTypeStream, streamOrAlias, env))
	case errWrongStreamType:
		return Error(permissionError(operationOutput, permissionTypeBinaryStream, streamOrAlias, env))
	default:
		return Error(err)
	}
}

func writeTermOption(opts *writeOptions, option Term, env *Env) error {
	switch o := env.Resolve(option).(type) {
	case Variable:
		return instantiationError(env)
	case Compound:
		if o.Arity() != 1 {
			return domainError(validDomainWriteOption, o, env)
		}

		if o.Functor() == atomVariableNames {
			vns, err := variableNames(o, env)
			if err != nil {
				return err
			}
			opts.variableNames = vns
			return nil
		}

		var b bool
		switch v := env.Resolve(o.Arg(0)).(type) {
		case Variable:
			return instantiationError(env)
		case Atom:
			switch v {
			case atomTrue:
				b = true
			case atomFalse:
				b = false
			default:
				return domainError(validDomainWriteOption, o, env)
			}
		default:
			return domainError(validDomainWriteOption, o, env)
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
			return domainError(validDomainWriteOption, o, env)
		}
	default:
		return domainError(validDomainWriteOption, o, env)
	}
}

func variableNames(option Compound, env *Env) (map[Variable]Atom, error) {
	vns := map[Variable]Atom{}
	iter := ListIterator{List: option.Arg(0), Env: env}
	for iter.Next() {
		var vn Compound
		switch elem := env.Resolve(iter.Current()).(type) {
		case Variable:
			return nil, instantiationError(env)
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
		case Variable:
			return nil, instantiationError(env)
		case Atom:
			n = arg
		default:
			return nil, domainError(validDomainWriteOption, option, env)
		}

		var v Variable
		switch arg := env.Resolve(vn.Arg(1)).(type) {
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
		return nil, instantiationError(env)
	case Atom:
		if s != atomEmptyList {
			return nil, domainError(validDomainWriteOption, option, env)
		}
		return vns, nil
	default:
		return nil, domainError(validDomainWriteOption, option, env)
	}
}

// CharCode converts a single-rune Atom char to an Integer code, or vice versa.
func CharCode(vm *VM, char, code Term, k func(*Env) *Promise, env *Env) *Promise {
	switch ch := env.Resolve(char).(type) {
	case Variable:
		switch cd := env.Resolve(code).(type) {
		case Variable:
			return Error(instantiationError(env))
		case Integer:
			r := rune(cd)

			if !utf8.ValidRune(r) {
				return Error(representationError(flagCharacterCode, env))
			}

			return Delay(func(context.Context) *Promise {
				return Unify(vm, ch, Atom(r), k, env)
			})
		default:
			return Error(typeError(validTypeInteger, code, env))
		}
	case Atom:
		switch code := env.Resolve(code).(type) {
		case Variable, Integer:
			break
		default:
			return Error(typeError(validTypeInteger, code, env))
		}

		rs := []rune(ch.String())
		if len(rs) != 1 {
			return Error(typeError(validTypeCharacter, ch, env))
		}

		return Delay(func(context.Context) *Promise {
			return Unify(vm, code, Integer(rs[0]), k, env)
		})
	default:
		return Error(typeError(validTypeCharacter, ch, env))
	}
}

// PutByte outputs an integer byte to a stream represented by streamOrAlias.
func PutByte(vm *VM, streamOrAlias, byt Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := stream(vm, streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	switch b := env.Resolve(byt).(type) {
	case Variable:
		return Error(instantiationError(env))
	case Integer:
		if 0 > b || 255 < b {
			return Error(typeError(validTypeByte, byt, env))
		}

		switch err := s.WriteByte(byte(b)); err {
		case nil:
			return k(env)
		case errWrongIOMode:
			return Error(permissionError(operationOutput, permissionTypeStream, streamOrAlias, env))
		case errWrongStreamType:
			return Error(permissionError(operationOutput, permissionTypeTextStream, streamOrAlias, env))
		default:
			return Error(err)
		}
	default:
		return Error(typeError(validTypeByte, byt, env))
	}
}

// PutCode outputs code to the stream represented by streamOrAlias.
func PutCode(vm *VM, streamOrAlias, code Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := stream(vm, streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	switch c := env.Resolve(code).(type) {
	case Variable:
		return Error(instantiationError(env))
	case Integer:
		r := rune(c)

		if !utf8.ValidRune(r) {
			return Error(representationError(flagCharacterCode, env))
		}

		switch _, err := s.WriteRune(r); err {
		case nil:
			return k(env)
		case errWrongIOMode:
			return Error(permissionError(operationOutput, permissionTypeStream, streamOrAlias, env))
		case errWrongStreamType:
			return Error(permissionError(operationOutput, permissionTypeBinaryStream, streamOrAlias, env))
		default:
			return Error(err)
		}
	default:
		return Error(typeError(validTypeInteger, code, env))
	}
}

type readTermOptions struct {
	singletons    Term
	variables     Term
	variableNames Term
}

// ReadTerm reads from the stream represented by streamOrAlias and unifies with stream.
func ReadTerm(vm *VM, streamOrAlias, out, options Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := stream(vm, streamOrAlias, env)
	if err != nil {
		return Error(err)
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

	p := NewParser(vm, s)
	defer func() {
		_ = s.UnreadRune()
	}()

	t, err := p.Term()
	switch err {
	case nil:
		break
	case io.EOF:
		return Unify(vm, out, atomEndOfFile, k, env)
	case errWrongIOMode:
		return Error(permissionError(operationInput, permissionTypeStream, streamOrAlias, env))
	case errWrongStreamType:
		return Error(permissionError(operationInput, permissionTypeBinaryStream, streamOrAlias, env))
	case errPastEndOfStream:
		return Error(permissionError(operationInput, permissionTypePastEndOfStream, streamOrAlias, env))
	default:
		return Error(syntaxError(err, env))
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

func readTermOption(opts *readTermOptions, option Term, env *Env) error {
	switch option := env.Resolve(option).(type) {
	case Variable:
		return instantiationError(env)
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
func GetByte(vm *VM, streamOrAlias, inByte Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := stream(vm, streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	switch b := env.Resolve(inByte).(type) {
	case Variable:
		break
	case Integer:
		if b < 0 || b > 255 {
			return Error(typeError(validTypeInByte, inByte, env))
		}
	default:
		return Error(typeError(validTypeInByte, inByte, env))
	}

	switch b, err := s.ReadByte(); err {
	case nil:
		return Unify(vm, inByte, Integer(b), k, env)
	case io.EOF:
		return Unify(vm, inByte, Integer(-1), k, env)
	case errWrongIOMode:
		return Error(permissionError(operationInput, permissionTypeStream, streamOrAlias, env))
	case errWrongStreamType:
		return Error(permissionError(operationInput, permissionTypeTextStream, streamOrAlias, env))
	case errPastEndOfStream:
		return Error(permissionError(operationInput, permissionTypePastEndOfStream, streamOrAlias, env))
	default:
		return Error(err)
	}
}

// GetChar reads a character from the stream represented by streamOrAlias and unifies it with char.
func GetChar(vm *VM, streamOrAlias, char Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := stream(vm, streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	switch c := env.Resolve(char).(type) {
	case Variable:
		break
	case Atom:
		if len([]rune(c.String())) != 1 {
			return Error(typeError(validTypeInCharacter, char, env))
		}
	default:
		return Error(typeError(validTypeInCharacter, char, env))
	}

	switch r, _, err := s.ReadRune(); err {
	case nil:
		if r == utf8.RuneError {
			return Error(representationError(flagCharacter, env))
		}

		return Unify(vm, char, Atom(r), k, env)
	case io.EOF:
		return Unify(vm, char, atomEndOfFile, k, env)
	case errWrongIOMode:
		return Error(permissionError(operationInput, permissionTypeStream, streamOrAlias, env))
	case errWrongStreamType:
		return Error(permissionError(operationInput, permissionTypeBinaryStream, streamOrAlias, env))
	case errPastEndOfStream:
		return Error(permissionError(operationInput, permissionTypePastEndOfStream, streamOrAlias, env))
	default:
		return Error(err)
	}
}

// PeekByte peeks a byte from the stream represented by streamOrAlias and unifies it with inByte.
func PeekByte(vm *VM, streamOrAlias, inByte Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := stream(vm, streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	switch b := env.Resolve(inByte).(type) {
	case Variable:
		break
	case Integer:
		if b < 0 || b > 255 {
			return Error(typeError(validTypeInByte, inByte, env))
		}
	default:
		return Error(typeError(validTypeInByte, inByte, env))
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
	case errWrongIOMode:
		return Error(permissionError(operationInput, permissionTypeStream, streamOrAlias, env))
	case errWrongStreamType:
		return Error(permissionError(operationInput, permissionTypeTextStream, streamOrAlias, env))
	case errPastEndOfStream:
		return Error(permissionError(operationInput, permissionTypePastEndOfStream, streamOrAlias, env))
	default:
		return Error(err)
	}
}

// PeekChar peeks a rune from the stream represented by streamOrAlias and unifies it with char.
func PeekChar(vm *VM, streamOrAlias, char Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := stream(vm, streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	switch c := env.Resolve(char).(type) {
	case Variable:
		break
	case Atom:
		if len([]rune(c.String())) != 1 {
			return Error(typeError(validTypeInCharacter, char, env))
		}
	default:
		return Error(typeError(validTypeInCharacter, char, env))
	}

	r, _, err := s.ReadRune()
	defer func() {
		_ = s.UnreadRune()
	}()
	switch err {
	case nil:
		if r == unicode.ReplacementChar {
			return Error(representationError(flagCharacter, env))
		}

		return Unify(vm, char, Atom(r), k, env)
	case io.EOF:
		return Unify(vm, char, atomEndOfFile, k, env)
	case errWrongIOMode:
		return Error(permissionError(operationInput, permissionTypeStream, streamOrAlias, env))
	case errWrongStreamType:
		return Error(permissionError(operationInput, permissionTypeBinaryStream, streamOrAlias, env))
	case errPastEndOfStream:
		return Error(permissionError(operationInput, permissionTypePastEndOfStream, streamOrAlias, env))
	default:
		return Error(err)
	}
}

var osExit = os.Exit

// Halt exits the process with exit code of n.
func Halt(_ *VM, n Term, k func(*Env) *Promise, env *Env) *Promise {
	switch code := env.Resolve(n).(type) {
	case Variable:
		return Error(instantiationError(env))
	case Integer:
		osExit(int(code))
		return k(env)
	default:
		return Error(typeError(validTypeInteger, n, env))
	}
}

// Clause unifies head and body with H and B respectively where H :- B is in the database.
func Clause(vm *VM, head, body Term, k func(*Env) *Promise, env *Env) *Promise {
	pi, _, err := piArg(head, env)
	if err != nil {
		return Error(err)
	}

	switch env.Resolve(body).(type) {
	case Variable, Atom, Compound:
		break
	default:
		return Error(typeError(validTypeCallable, body, env))
	}

	p, ok := vm.procedures[pi]
	if !ok {
		return Bool(false)
	}

	u, ok := p.(*userDefined)
	if !ok || !u.public {
		return Error(permissionError(operationAccess, permissionTypePrivateProcedure, pi.Term(), env))
	}

	ks := make([]func(context.Context) *Promise, len(u.clauses))
	for i, c := range u.clauses {
		r := rulify(renamedCopy(c.raw, nil, env), env)
		ks[i] = func(context.Context) *Promise {
			return Unify(vm, atomIf.Apply(head, body), r, k, env)
		}
	}
	return Delay(ks...)
}

func rulify(t Term, env *Env) Term {
	t = env.Resolve(t)
	if c, ok := t.(Compound); ok && c.Functor() == atomIf && c.Arity() == 2 {
		return t
	}
	return atomIf.Apply(t, atomTrue)
}

// AtomLength counts the runes in atom and unifies the result with length.
func AtomLength(vm *VM, atom, length Term, k func(*Env) *Promise, env *Env) *Promise {
	switch a := env.Resolve(atom).(type) {
	case Variable:
		return Error(instantiationError(env))
	case Atom:
		switch l := env.Resolve(length).(type) {
		case Variable:
			break
		case Integer:
			if l < 0 {
				return Error(domainError(validDomainNotLessThanZero, length, env))
			}
		default:
			return Error(typeError(validTypeInteger, length, env))
		}

		return Delay(func(context.Context) *Promise {
			return Unify(vm, length, Integer(len([]rune(a.String()))), k, env)
		})
	default:
		return Error(typeError(validTypeAtom, atom, env))
	}
}

// AtomConcat concatenates atom1 and atom2 and unifies it with atom3.
func AtomConcat(vm *VM, atom1, atom2, atom3 Term, k func(*Env) *Promise, env *Env) *Promise {
	switch a3 := env.Resolve(atom3).(type) {
	case Variable:
		switch a1 := env.Resolve(atom1).(type) {
		case Variable:
			return Error(instantiationError(env))
		case Atom:
			switch a2 := env.Resolve(atom2).(type) {
			case Variable:
				return Error(instantiationError(env))
			case Atom:
				return Delay(func(context.Context) *Promise {
					return Unify(vm, a3, NewAtom(a1.String()+a2.String()), k, env)
				})
			default:
				return Error(typeError(validTypeAtom, atom2, env))
			}
		default:
			return Error(typeError(validTypeAtom, atom1, env))
		}
	case Atom:
		switch env.Resolve(atom1).(type) {
		case Variable, Atom:
			break
		default:
			return Error(typeError(validTypeAtom, atom1, env))
		}

		switch env.Resolve(atom2).(type) {
		case Variable, Atom:
			break
		default:
			return Error(typeError(validTypeAtom, atom2, env))
		}

		pattern := tuple(atom1, atom2)
		s := a3.String()
		ks := make([]func(context.Context) *Promise, 0, len(s)+1)
		for i := range s {
			a1, a2 := s[:i], s[i:]
			ks = append(ks, func(context.Context) *Promise {
				return Unify(vm, &pattern, tuple(NewAtom(a1), NewAtom(a2)), k, env)
			})
		}
		ks = append(ks, func(context.Context) *Promise {
			return Unify(vm, &pattern, tuple(a3, atomEmpty), k, env)
		})
		return Delay(ks...)
	default:
		return Error(typeError(validTypeAtom, atom3, env))
	}
}

// SubAtom unifies subAtom with a sub atom of length which appears with before runes preceding it and after runes following it.
func SubAtom(vm *VM, atom, before, length, after, subAtom Term, k func(*Env) *Promise, env *Env) *Promise {
	switch whole := env.Resolve(atom).(type) {
	case Variable:
		return Error(instantiationError(env))
	case Atom:
		rs := []rune(whole.String())

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
			return Error(typeError(validTypeAtom, subAtom, env))
		}

		pattern := tuple(before, length, after, subAtom)
		var ks []func(context.Context) *Promise
		for i := 0; i <= len(rs); i++ {
			for j := i; j <= len(rs); j++ {
				before, length, after, subAtom := Integer(i), Integer(j-i), Integer(len(rs)-j), NewAtom(string(rs[i:j]))
				ks = append(ks, func(context.Context) *Promise {
					return Unify(vm, pattern, tuple(before, length, after, subAtom), k, env)
				})
			}
		}
		return Delay(ks...)
	default:
		return Error(typeError(validTypeAtom, atom, env))
	}
}

func checkPositiveInteger(n Term, env *Env) error {
	switch b := env.Resolve(n).(type) {
	case Variable:
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
func AtomChars(vm *VM, atom, chars Term, k func(*Env) *Promise, env *Env) *Promise {
	switch a := env.Resolve(atom).(type) {
	case Variable:
		var sb strings.Builder
		iter := ListIterator{List: chars, Env: env}
		for iter.Next() {
			switch e := env.Resolve(iter.Current()).(type) {
			case Variable:
				return Error(instantiationError(env))
			case Atom:
				if len([]rune(e.String())) != 1 {
					return Error(typeError(validTypeCharacter, e, env))
				}
				_, _ = sb.WriteString(e.String())
			default:
				return Error(typeError(validTypeCharacter, e, env))
			}
		}
		if err := iter.Err(); err != nil {
			return Error(err)
		}
		return Unify(vm, atom, NewAtom(sb.String()), k, env)
	case Atom:
		iter := ListIterator{List: chars, Env: env, AllowPartial: true}
		for iter.Next() {
			switch e := env.Resolve(iter.Current()).(type) {
			case Variable:
				break
			case Atom:
				if len([]rune(e.String())) != 1 {
					return Error(typeError(validTypeCharacter, e, env))
				}
			default:
				return Error(typeError(validTypeCharacter, e, env))
			}
		}
		if err := iter.Err(); err != nil {
			return Error(err)
		}

		s := a.String()
		if s == "" {
			return Unify(vm, chars, atomEmptyList, k, env)
		}
		return Unify(vm, chars, charList(s), k, env)
	default:
		return Error(typeError(validTypeAtom, a, env))
	}
}

// AtomCodes breaks up atom into a list of runes and unifies it with codes, or constructs an atom from the list of runes
// and unifies it with atom.
func AtomCodes(vm *VM, atom, codes Term, k func(*Env) *Promise, env *Env) *Promise {
	switch a := env.Resolve(atom).(type) {
	case Variable:
		var sb strings.Builder
		iter := ListIterator{List: codes, Env: env}
		for iter.Next() {
			switch e := env.Resolve(iter.Current()).(type) {
			case Variable:
				return Error(instantiationError(env))
			case Integer:
				if e < 0 || e > unicode.MaxRune {
					return Error(representationError(flagCharacterCode, env))
				}
				_, _ = sb.WriteRune(rune(e))
			default:
				return Error(typeError(validTypeInteger, e, env))
			}
		}
		if err := iter.Err(); err != nil {
			return Error(err)
		}
		return Unify(vm, atom, NewAtom(sb.String()), k, env)
	case Atom:
		iter := ListIterator{List: codes, Env: env, AllowPartial: true}
		for iter.Next() {
			switch e := env.Resolve(iter.Current()).(type) {
			case Variable:
				break
			case Integer:
				if e < 0 || e > unicode.MaxRune {
					return Error(representationError(flagCharacterCode, env))
				}
			default:
				return Error(typeError(validTypeInteger, e, env))
			}
		}
		if err := iter.Err(); err != nil {
			return Error(err)
		}

		s := a.String()
		if s == "" {
			return Unify(vm, codes, atomEmptyList, k, env)
		}
		return Unify(vm, codes, codeList(s), k, env)
	default:
		return Error(typeError(validTypeAtom, atom, env))
	}
}

// NumberChars breaks up an atom representation of a number num into a list of characters and unifies it with chars, or
// constructs a number from a list of characters chars and unifies it with num.
func NumberChars(vm *VM, num, chars Term, k func(*Env) *Promise, env *Env) *Promise {
	var (
		sb   strings.Builder
		iter = ListIterator{List: chars, Env: env}
	)
	for iter.Next() {
		switch e := env.Resolve(iter.Current()).(type) {
		case Variable:
			return numberCharsWrite(vm, num, chars, k, env)
		case Atom:
			s := e.String()
			if len([]rune(s)) != 1 {
				return Error(typeError(validTypeCharacter, e, env))
			}
			_, _ = sb.WriteString(s)
		default:
			return Error(typeError(validTypeCharacter, e, env))
		}
	}
	if err := iter.Err(); err != nil {
		if _, ok := iter.Suffix().(Variable); ok {
			return numberCharsWrite(vm, num, chars, k, env)
		}
		return Error(err)
	}

	p := Parser{
		lexer: Lexer{
			input: newRuneRingBuffer(strings.NewReader(sb.String())),
		},
	}
	t, err := p.number()
	if err != nil {
		return Error(syntaxError(err, env))
	}

	switch n := env.Resolve(num).(type) {
	case Variable, Number:
		return Unify(vm, n, t, k, env)
	default:
		return Error(typeError(validTypeNumber, n, env))
	}
}

func numberCharsWrite(vm *VM, num, chars Term, k func(*Env) *Promise, env *Env) *Promise {
	switch n := env.Resolve(num).(type) {
	case Variable:
		return Error(instantiationError(env))
	case Number:
		iter := ListIterator{List: chars, Env: env}
		for iter.Next() {
			switch e := env.Resolve(iter.Current()).(type) {
			case Variable:
				break
			case Atom:
				if len(e.String()) != 1 {
					return Error(typeError(validTypeCharacter, e, env))
				}
			default:
				return Error(typeError(validTypeCharacter, e, env))
			}
		}
		if err := iter.Err(); err != nil {
			if _, ok := iter.Suffix().(Variable); !ok {
				return Error(err)
			}
		}

		var buf bytes.Buffer
		_ = writeTerm(&buf, n, &defaultWriteOptions, nil)
		rs := []rune(buf.String())

		cs := make([]Term, len(rs))
		for i, r := range rs {
			cs[i] = Atom(r)
		}
		return Unify(vm, chars, List(cs...), k, env)
	default:
		return Error(typeError(validTypeNumber, n, env))
	}
}

// NumberCodes breaks up an atom representation of a number num into a list of runes and unifies it with codes, or
// constructs a number from a list of runes codes and unifies it with num.
func NumberCodes(vm *VM, num, codes Term, k func(*Env) *Promise, env *Env) *Promise {
	switch codes := env.Resolve(codes).(type) {
	case Variable:
		break
	default:
		switch n := env.Resolve(num).(type) {
		case Variable, Integer, Float:
			break
		default:
			return Error(typeError(validTypeNumber, n, env))
		}

		var sb strings.Builder
		iter := ListIterator{List: codes, Env: env}
		for iter.Next() {
			switch e := env.Resolve(iter.Current()).(type) {
			case Variable:
				return Error(instantiationError(env))
			case Integer:
				_, _ = sb.WriteRune(rune(e))
			default:
				return Error(representationError(flagCharacterCode, env))
			}
		}
		if err := iter.Err(); err != nil {
			return Error(err)
		}

		p := Parser{
			lexer: Lexer{
				input: newRuneRingBuffer(strings.NewReader(sb.String())),
			},
		}
		t, err := p.number()
		if err != nil {
			return Error(syntaxError(err, env))
		}

		return Delay(func(context.Context) *Promise {
			return Unify(vm, num, t, k, env)
		})
	}

	switch n := env.Resolve(num).(type) {
	case Variable:
		return Error(instantiationError(env))
	case Integer, Float:
		var buf bytes.Buffer
		_ = writeTerm(&buf, n, &defaultWriteOptions, nil)
		rs := []rune(buf.String())
		cs := make([]Term, len(rs))
		for i, r := range rs {
			cs[i] = Integer(r)
		}
		return Delay(func(context.Context) *Promise {
			return Unify(vm, codes, List(cs...), k, env)
		})
	default:
		return Error(typeError(validTypeNumber, num, env))
	}
}

// StreamProperty succeeds iff the stream represented by stream has the stream property.
func StreamProperty(vm *VM, stream, property Term, k func(*Env) *Promise, env *Env) *Promise {
	streams := make([]*Stream, 0, len(vm.streams.elems))
	switch s := env.Resolve(stream).(type) {
	case Variable:
		for _, v := range vm.streams.elems {
			streams = append(streams, v)
		}
	case *Stream:
		streams = append(streams, s)
	default:
		return Error(domainError(validDomainStream, stream, env))
	}

	if !isStreamProperty(property, env) {
		return Error(domainError(validDomainStreamProperty, property, env))
	}

	var ks []func(context.Context) *Promise
	for _, s := range streams {
		s := s
		for _, p := range s.properties() {
			p := p
			ks = append(ks, func(context.Context) *Promise {
				return Unify(vm, atomEmpty.Apply(stream, property), atomEmpty.Apply(s, p), k, env)
			})
		}
	}
	return Delay(ks...)
}

func isStreamProperty(property Term, env *Env) bool {
	switch p := env.Resolve(property).(type) {
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
			return isAtom(arg, env)
		case atomPosition:
			return isInteger(arg, env)
		}
		return false
	default:
		return false
	}
}

func isAtom(t Term, env *Env) bool {
	switch env.Resolve(t).(type) {
	case Variable, Atom:
		return true
	default:
		return false
	}
}

func isInteger(t Term, env *Env) bool {
	switch env.Resolve(t).(type) {
	case Variable, Integer:
		return true
	default:
		return false
	}
}

// SetStreamPosition sets the position property of the stream represented by streamOrAlias.
func SetStreamPosition(vm *VM, streamOrAlias, position Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := stream(vm, streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	switch p := env.Resolve(position).(type) {
	case Variable:
		return Error(instantiationError(env))
	case Integer:
		switch _, err := s.Seek(int64(p), 0); err {
		case nil:
			return k(env)
		case errReposition:
			return Error(permissionError(operationReposition, permissionTypeStream, streamOrAlias, env))
		default:
			return Error(err)
		}
	default:
		return Error(typeError(validTypeInteger, position, env))
	}
}

// CharConversion registers a character conversion from inChar to outChar, or remove the conversion if inChar = outChar.
func CharConversion(vm *VM, inChar, outChar Term, k func(*Env) *Promise, env *Env) *Promise {
	switch in := env.Resolve(inChar).(type) {
	case Variable:
		return Error(instantiationError(env))
	case Atom:
		i := []rune(in.String())
		if len(i) != 1 {
			return Error(representationError(flagCharacter, env))
		}

		switch out := env.Resolve(outChar).(type) {
		case Variable:
			return Error(instantiationError(env))
		case Atom:
			o := []rune(out.String())
			if len(o) != 1 {
				return Error(representationError(flagCharacter, env))
			}

			if vm.charConversions == nil {
				vm.charConversions = map[rune]rune{}
			}
			if i[0] == o[0] {
				delete(vm.charConversions, i[0])
				return k(env)
			}
			vm.charConversions[i[0]] = o[0]
			return k(env)
		default:
			return Error(representationError(flagCharacter, env))
		}
	default:
		return Error(representationError(flagCharacter, env))
	}
}

// CurrentCharConversion succeeds iff a conversion from inChar to outChar is defined.
func CurrentCharConversion(vm *VM, inChar, outChar Term, k func(*Env) *Promise, env *Env) *Promise {
	switch in := env.Resolve(inChar).(type) {
	case Variable:
		break
	case Atom:
		i := []rune(in.String())
		if len(i) != 1 {
			return Error(representationError(flagCharacter, env))
		}
	default:
		return Error(representationError(flagCharacter, env))
	}

	switch out := env.Resolve(outChar).(type) {
	case Variable:
		break
	case Atom:
		o := []rune(out.String())
		if len(o) != 1 {
			return Error(representationError(flagCharacter, env))
		}
	default:
		return Error(representationError(flagCharacter, env))
	}

	if c1, ok := env.Resolve(inChar).(Atom); ok {
		r := []rune(c1.String())
		if r, ok := vm.charConversions[r[0]]; ok {
			return Unify(vm, outChar, Atom(r), k, env)
		}
		return Unify(vm, outChar, c1, k, env)
	}

	pattern := tuple(inChar, outChar)
	ks := make([]func(context.Context) *Promise, 256)
	for i := 0; i < 256; i++ {
		r := rune(i)
		cr, ok := vm.charConversions[r]
		if !ok {
			cr = r
		}

		ks[i] = func(context.Context) *Promise {
			return Unify(vm, pattern, tuple(Atom(r), Atom(cr)), k, env)
		}
	}
	return Delay(ks...)
}

// SetPrologFlag sets flag to value.
func SetPrologFlag(vm *VM, flag, value Term, k func(*Env) *Promise, env *Env) *Promise {
	switch f := env.Resolve(flag).(type) {
	case Variable:
		return Error(instantiationError(env))
	case Atom:
		var modify func(vm *VM, value Atom) error
		switch f {
		case atomBounded, atomMaxInteger, atomMinInteger, atomIntegerRoundingFunction, atomMaxArity:
			return Error(permissionError(operationModify, permissionTypeFlag, f, env))
		case atomCharConversion:
			modify = modifyCharConversion
		case atomDebug:
			modify = modifyDebug
		case atomUnknown:
			modify = modifyUnknown
		case atomDoubleQuotes:
			modify = modifyDoubleQuotes
		default:
			return Error(domainError(validDomainPrologFlag, f, env))
		}

		switch v := env.Resolve(value).(type) {
		case Variable:
			return Error(instantiationError(env))
		case Atom:
			if err := modify(vm, v); err != nil {
				return Error(err)
			}
			return k(env)
		default:
			return Error(domainError(validDomainFlagValue, atomPlus.Apply(flag, value), env))
		}
	default:
		return Error(typeError(validTypeAtom, f, env))
	}
}

func modifyCharConversion(vm *VM, value Atom) error {
	switch value {
	case atomOn:
		vm.charConvEnabled = true
	case atomOff:
		vm.charConvEnabled = false
	default:
		return domainError(validDomainFlagValue, atomPlus.Apply(atomCharConversion, value), nil)
	}
	return nil
}

func modifyDebug(vm *VM, value Atom) error {
	switch value {
	case atomOn:
		vm.debug = true
	case atomOff:
		vm.debug = false
	default:
		return domainError(validDomainFlagValue, atomPlus.Apply(atomDebug, value), nil)
	}
	return nil
}

func modifyUnknown(vm *VM, value Atom) error {
	switch value {
	case atomError:
		vm.unknown = unknownError
	case atomWarning:
		vm.unknown = unknownWarning
	case atomFail:
		vm.unknown = unknownFail
	default:
		return domainError(validDomainFlagValue, atomPlus.Apply(atomUnknown, value), nil)
	}
	return nil
}

func modifyDoubleQuotes(vm *VM, value Atom) error {
	switch value {
	case atomCodes:
		vm.doubleQuotes = doubleQuotesCodes
	case atomChars:
		vm.doubleQuotes = doubleQuotesChars
	case atomAtom:
		vm.doubleQuotes = doubleQuotesAtom
	default:
		return domainError(validDomainFlagValue, atomPlus.Apply(atomDoubleQuotes, value), nil)
	}
	return nil
}

// CurrentPrologFlag succeeds iff flag is set to value.
func CurrentPrologFlag(vm *VM, flag, value Term, k func(*Env) *Promise, env *Env) *Promise {
	switch f := env.Resolve(flag).(type) {
	case Variable:
		break
	case Atom:
		switch f {
		case atomBounded, atomMaxInteger, atomMinInteger, atomIntegerRoundingFunction, atomCharConversion, atomDebug, atomMaxArity, atomUnknown, atomDoubleQuotes:
			break
		default:
			return Error(domainError(validDomainPrologFlag, f, env))
		}
	default:
		return Error(typeError(validTypeAtom, f, env))
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
	ks := make([]func(context.Context) *Promise, len(flags))
	for i := range flags {
		f := flags[i]
		ks[i] = func(context.Context) *Promise {
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
func ExpandTerm(vm *VM, term1, term2 Term, k func(*Env) *Promise, env *Env) *Promise {
	t, err := expand(vm, term1, env)
	if err != nil {
		return Error(err)
	}

	return Unify(vm, t, term2, k, env)
}

func expand(vm *VM, term Term, env *Env) (Term, error) {
	if _, ok := vm.procedures[procedureIndicator{name: atomTermExpansion, arity: 2}]; ok {
		var ret Term
		v := NewVariable()
		ok, err := Call(vm, atomTermExpansion.Apply(term, v), func(env *Env) *Promise {
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
	if err != nil {
		return term, nil
	}
	return t, err
}

// Environ succeeds if an environment variable key has value.
func Environ(vm *VM, key, value Term, k func(*Env) *Promise, env *Env) *Promise {
	lines := os.Environ()
	ks := make([]func(ctx context.Context) *Promise, len(lines))
	for i, l := range lines {
		kv := strings.SplitN(l, "=", 2)
		ks[i] = func(ctx context.Context) *Promise {
			return Unify(vm, tuple(key, value), tuple(NewAtom(kv[0]), NewAtom(kv[1])), k, env)
		}
	}
	return Delay(ks...)
}

// Nth0 succeeds if elem is the n-th element of list, counting from 0.
func Nth0(vm *VM, n, list, elem Term, k func(*Env) *Promise, env *Env) *Promise {
	return nth(vm, 0, n, list, elem, k, env)
}

// Nth1 succeeds if elem is the n-th element of list, counting from 1.
func Nth1(vm *VM, n, list, elem Term, k func(*Env) *Promise, env *Env) *Promise {
	return nth(vm, 1, n, list, elem, k, env)
}

func nth(vm *VM, base Integer, n, list, elem Term, k func(*Env) *Promise, env *Env) *Promise {
	switch n := env.Resolve(n).(type) {
	case Variable:
		var ks []func(context.Context) *Promise
		iter := ListIterator{List: list, Env: env}
		for i := base; iter.Next(); i++ {
			i, e := i, iter.Current()
			ks = append(ks, func(context.Context) *Promise {
				return Unify(vm, tuple(n, elem), tuple(i, e), k, env)
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
		iter := ListIterator{List: list, Env: env, AllowCycle: true}
		for i := base; iter.Next(); i++ {
			if i == n {
				return Unify(vm, elem, iter.Current(), k, env)
			}
		}
		if err := iter.Err(); err != nil {
			return Error(err)
		}
		return Bool(false)
	default:
		return Error(typeError(validTypeInteger, n, env))
	}
}

// Succ succeeds if s is the successor of non-negative integer x.
func Succ(vm *VM, x, s Term, k func(*Env) *Promise, env *Env) *Promise {
	switch x := x.(type) {
	case Variable:
		switch s := s.(type) {
		case Variable:
			return Error(instantiationError(env))
		case Integer:
			switch {
			case s < Integer(0):
				return Error(domainError(validDomainNotLessThanZero, s, env))
			case s == Integer(0):
				return Bool(false)
			default:
				return Unify(vm, x, s-Integer(1), k, env)
			}
		default:
			return Error(typeError(validTypeInteger, s, env))
		}
	case Integer:
		if x < Integer(0) {
			return Error(domainError(validDomainNotLessThanZero, x, env))
		}

		r, err := add(x, Integer(1))
		if err != nil {
			var ev exceptionalValue
			if errors.As(err, &ev) {
				return Error(evaluationError(ev, env))
			}
			return Error(err)
		}

		switch s := s.(type) {
		case Variable:
			return Unify(vm, s, r, k, env)
		case Integer:
			if s < Integer(0) {
				return Error(domainError(validDomainNotLessThanZero, s, env))
			}
			return Unify(vm, s, r, k, env)
		default:
			return Error(typeError(validTypeInteger, s, env))
		}
	default:
		return Error(typeError(validTypeInteger, x, env))
	}
}

// Length succeeds iff list is a list of length.
func Length(vm *VM, list, length Term, k func(*Env) *Promise, env *Env) *Promise {
	// https://github.com/mthom/scryer-prolog/issues/1325#issue-1160713156
	// Note that it's a bit simpler since we don't have attributed variables (yet).

	n := env.Resolve(length)
	switch n := n.(type) {
	case Variable:
		break
	case Integer:
		if n < 0 {
			return Error(domainError(validDomainNotLessThanZero, n, env))
		}
	default:
		return Error(typeError(validTypeInteger, n, env))
	}

	var (
		skipped = NewVariable()
		suffix  = NewVariable()
	)
	return SkipMaxList(vm, skipped, n, list, suffix, func(env *Env) *Promise {
		skipped := env.Resolve(skipped).(Integer)

		switch suffix := env.Resolve(suffix).(type) {
		case Variable: // partial list
			if n, ok := n.(Integer); ok {
				return lengthRundown(vm, suffix, n-skipped, k, env)
			}

			n := n.(Variable)

			if n == suffix {
				return Error(resourceError(resourceFiniteMemory, env))
			}

			return lengthAddendum(vm, atomEmptyList, skipped, suffix, n, k, env)
		case Atom: // list or non-list terminated by an atom
			if suffix != atomEmptyList {
				return Bool(false)
			}

			return Unify(vm, n, skipped, k, env)
		case Compound: // non-list including infinite list
			if suffix.Functor() != atomDot || suffix.Arity() != 2 {
				return Bool(false)
			}

			if _, ok := n.(Variable); !ok {
				return Bool(false)
			}

			return Error(resourceError(resourceFiniteMemory, env))
		default: // non-list terminated by a term that is neither an atom nor a compound
			return Bool(false)
		}
	}, env)
}

func lengthRundown(vm *VM, list Variable, n Integer, k func(*Env) *Promise, env *Env) *Promise {
	elems := make([]Term, n)
	for i := range elems {
		elems[i] = NewVariable()
	}
	return Unify(vm, list, List(elems...), k, env)
}

func lengthAddendum(vm *VM, suffix Term, offset Integer, list, length Variable, k func(*Env) *Promise, env *Env) *Promise {
	return Delay(func(context.Context) *Promise {
		return Unify(vm, tuple(list, length), tuple(suffix, offset), k, env)
	}, func(context.Context) *Promise {
		suffix := atomDot.Apply(NewVariable(), suffix)
		offset, err := addI(offset, 1)
		if err != nil {
			return Error(representationError(flagMaxInteger, env))
		}
		return lengthAddendum(vm, suffix, offset, list, length, k, env)
	})
}

// SkipMaxList iterates over list up to max elements and unifies the number of skipped elements with skip and the rest with suffix.
func SkipMaxList(vm *VM, skip, max, list, suffix Term, k func(*Env) *Promise, env *Env) *Promise {
	m := maxInt
	switch max := env.Resolve(max).(type) {
	case Variable:
		break
	case Integer:
		if max < 0 {
			return Error(domainError(validDomainNotLessThanZero, max, env))
		}
		m = max
	default:
		return Error(typeError(validTypeInteger, max, env))
	}

	var (
		iter = ListIterator{List: list, Env: env}
		n    = Integer(0)
	)
	for n < m && iter.Next() {
		n++
	}

	return Unify(vm, tuple(skip, suffix), tuple(n, iter.Suffix()), k, env)
}

// Append succeeds iff zs is the concatenation of lists xs and ys.
func Append(vm *VM, xs, ys, zs Term, k func(*Env) *Promise, env *Env) *Promise {
	// A special case for non-empty lists without a variable in the spine.
	if xs, ok := env.Resolve(xs).(Compound); ok {
		iter := ListIterator{List: xs, Env: nil} // No variables allowed.
		for iter.Next() {
		}
		if err := iter.Err(); err == nil {
			return Unify(vm, zs, partial{
				Compound: xs,
				tail:     ys,
			}, k, env)
		}
	}

	return appendLists(vm, xs, ys, zs, k, env)
}

func appendLists(vm *VM, xs, ys, zs Term, k func(*Env) *Promise, env *Env) *Promise {
	/*
		append([], L, L).
		append([X|L1], L2, [X|L3]) :- append(L1, L2, L3).
	*/
	return Delay(func(context.Context) *Promise {
		return Unify(vm, tuple(xs, ys), tuple(List(), zs), k, env)
	}, func(context.Context) *Promise {
		x := NewVariable()
		l1, l3 := NewVariable(), NewVariable()
		return Unify(vm, tuple(xs, zs), tuple(Cons(x, l1), Cons(x, l3)), func(env *Env) *Promise {
			return appendLists(vm, l1, ys, l3, k, env)
		}, env)
	})
}
