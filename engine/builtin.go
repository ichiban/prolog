package engine

import (
	"bytes"
	"context"
	"errors"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"unicode"
	"unicode/utf8"
)

// Repeat repeats the continuation until it succeeds.
func Repeat(_ *VM, k Cont, env *Env) *Promise {
	return repeat(func(ctx context.Context) *Promise {
		return k(env)
	})
}

// Not calls goal and returns false if it succeeds. Otherwise, invokes the continuation.
func Not(vm *VM, goal Term, k Cont, env *Env) *Promise {
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
func Call(vm *VM, goal Term, k Cont, env *Env) (promise *Promise) {
	defer ensurePromise(&promise)
	switch g := env.Resolve(goal).(type) {
	case Variable:
		return Error(InstantiationError(env))
	default:
		fvs := env.freeVariables(g)
		args, err := makeSlice(len(fvs))
		if err != nil {
			return Error(resourceError(resourceMemory, env))
		}
		for i, fv := range fvs {
			args[i] = fv
		}
		cs, err := compileTerm(vm.typeIn, atomColonMinus.Apply(tuple(args...), g), env)
		if err != nil {
			return Error(err)
		}

		return cs.call(vm, args, k, env)
	}
}

// Call1 succeeds if closure with an additional argument succeeds.
func Call1(vm *VM, closure, arg1 Term, k Cont, env *Env) *Promise {
	return callN(vm, closure, []Term{arg1}, k, env)
}

// Call2 succeeds if closure with 2 additional arguments succeeds.
func Call2(vm *VM, closure, arg1, arg2 Term, k Cont, env *Env) *Promise {
	return callN(vm, closure, []Term{arg1, arg2}, k, env)
}

// Call3 succeeds if closure with 3 additional arguments succeeds.
func Call3(vm *VM, closure, arg1, arg2, arg3 Term, k Cont, env *Env) *Promise {
	return callN(vm, closure, []Term{arg1, arg2, arg3}, k, env)
}

// Call4 succeeds if closure with 4 additional arguments succeeds.
func Call4(vm *VM, closure, arg1, arg2, arg3, arg4 Term, k Cont, env *Env) *Promise {
	return callN(vm, closure, []Term{arg1, arg2, arg3, arg4}, k, env)
}

// Call5 succeeds if closure with 5 additional arguments succeeds.
func Call5(vm *VM, closure, arg1, arg2, arg3, arg4, arg5 Term, k Cont, env *Env) *Promise {
	return callN(vm, closure, []Term{arg1, arg2, arg3, arg4, arg5}, k, env)
}

// Call6 succeeds if closure with 6 additional arguments succeeds.
func Call6(vm *VM, closure, arg1, arg2, arg3, arg4, arg5, arg6 Term, k Cont, env *Env) *Promise {
	return callN(vm, closure, []Term{arg1, arg2, arg3, arg4, arg5, arg6}, k, env)
}

// Call7 succeeds if closure with 7 additional arguments succeeds.
func Call7(vm *VM, closure, arg1, arg2, arg3, arg4, arg5, arg6, arg7 Term, k Cont, env *Env) *Promise {
	return callN(vm, closure, []Term{arg1, arg2, arg3, arg4, arg5, arg6, arg7}, k, env)
}

func callN(vm *VM, closure Term, additional []Term, k Cont, env *Env) *Promise {
	pi, arg, err := piArg(closure, env)
	if err != nil {
		return Error(err)
	}
	args, err := makeSlice(int(pi.arity) + len(additional))
	if err != nil {
		return Error(resourceError(resourceMemory, env))
	}
	args = args[:pi.arity]
	for i := 0; i < int(pi.arity); i++ {
		args[i] = arg(i)
	}
	args = append(args, additional...)
	return Call(vm, pi.name.Apply(args...), k, env)
}

// CallNth succeeds iff goal succeeds and nth unifies with the number of re-execution.
// See http://www.complang.tuwien.ac.at/ulrich/iso-prolog/call_nth
func CallNth(vm *VM, goal, nth Term, k Cont, env *Env) *Promise {
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
func Unify(_ *VM, x, y Term, k Cont, env *Env) *Promise {
	env, ok := env.Unify(x, y)
	if !ok {
		return Bool(false)
	}
	return k(env)
}

// UnifyWithOccursCheck unifies x and y with occurs check (i.e., X = f(X) is not allowed).
func UnifyWithOccursCheck(_ *VM, x, y Term, k Cont, env *Env) *Promise {
	env, ok := env.unifyWithOccursCheck(x, y)
	if !ok {
		return Bool(false)
	}
	return k(env)
}

// SubsumesTerm succeeds if general and specific are unifiable without binding variables in specific.
func SubsumesTerm(_ *VM, general, specific Term, k Cont, env *Env) *Promise {
	theta, ok := env.unifyWithOccursCheck(general, specific)
	if !ok {
		return Bool(false)
	}

	if d := theta.simplify(general).Compare(specific, env); d != 0 {
		return Bool(false)
	}

	return k(env)
}

// TypeVar checks if t is a variable.
func TypeVar(_ *VM, t Term, k Cont, env *Env) *Promise {
	if _, ok := env.Resolve(t).(Variable); !ok {
		return Bool(false)
	}
	return k(env)
}

// TypeFloat checks if t is a floating-point number.
func TypeFloat(_ *VM, t Term, k Cont, env *Env) *Promise {
	if _, ok := env.Resolve(t).(Float); !ok {
		return Bool(false)
	}
	return k(env)
}

// TypeInteger checks if t is an integer.
func TypeInteger(_ *VM, t Term, k Cont, env *Env) *Promise {
	if _, ok := env.Resolve(t).(Integer); !ok {
		return Bool(false)
	}
	return k(env)
}

// TypeAtom checks if t is an atom.
func TypeAtom(_ *VM, t Term, k Cont, env *Env) *Promise {
	if _, ok := env.Resolve(t).(Atom); !ok {
		return Bool(false)
	}
	return k(env)
}

// TypeCompound checks if t is a compound term.
func TypeCompound(_ *VM, t Term, k Cont, env *Env) *Promise {
	if _, ok := env.Resolve(t).(Compound); !ok {
		return Bool(false)
	}
	return k(env)
}

// AcyclicTerm checks if t is acyclic.
func AcyclicTerm(_ *VM, t Term, k Cont, env *Env) *Promise {
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

// Arg extracts nth argument of term as arg, or finds the argument position of arg in term as nth.
func Arg(vm *VM, nth, t, arg Term, k Cont, env *Env) *Promise {
	switch c := env.Resolve(t).(type) {
	case Variable:
		return Error(InstantiationError(env))
	case Compound:
		switch n := env.Resolve(nth).(type) {
		case Variable:
			return Error(InstantiationError(env))
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
func Univ(vm *VM, t, list Term, k Cont, env *Env) *Promise {
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
				return Error(InstantiationError(env))
			case Compound:
				return Error(typeError(validTypeAtomic, e, env))
			default:
				return k(env.bind(t, e))
			}
		default:
			switch e := env.Resolve(elems[0]).(type) {
			case Variable:
				return Error(InstantiationError(env))
			case Atom:
				return k(env.bind(t, e.Apply(elems[1:]...)))
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
func CopyTerm(vm *VM, in, out Term, k Cont, env *Env) *Promise {
	c, err := renamedCopy(in, nil, env)
	if err != nil {
		return Error(err)
	}
	return Unify(vm, c, out, k, env)
}

func renamedCopy(t Term, copied map[termID]Term, env *Env) (Term, error) {
	if copied == nil {
		copied = map[termID]Term{}
	}
	t = env.Resolve(t)
	if c, ok := copied[id(t)]; ok {
		return c, nil
	}
	switch t := t.(type) {
	case Variable:
		v := NewVariable()
		copied[id(t)] = v
		return v, nil
	case charList, codeList:
		return t, nil
	case list:
		s, err := makeSlice(len(t))
		if err != nil {
			return nil, resourceError(resourceMemory, env)
		}
		l := list(s)
		copied[id(t)] = l
		for i := range t {
			c, err := renamedCopy(t[i], copied, env)
			if err != nil {
				return nil, err
			}
			l[i] = c
		}
		return l, nil
	case *partial:
		var p partial
		copied[id(t)] = &p
		cp, err := renamedCopy(t.Compound, copied, env)
		if err != nil {
			return nil, err
		}
		p.Compound = cp.(Compound)
		cp, err = renamedCopy(*t.tail, copied, env)
		if err != nil {
			return nil, err
		}
		tail := cp
		p.tail = &tail
		return &p, nil
	case Compound:
		args, err := makeSlice(t.Arity())
		if err != nil {
			return nil, resourceError(resourceMemory, env)
		}
		c := compound{
			functor: t.Functor(),
			args:    args,
		}
		copied[id(t)] = &c
		for i := 0; i < t.Arity(); i++ {
			cp, err := renamedCopy(t.Arg(i), copied, env)
			if err != nil {
				return nil, err
			}
			c.args[i] = cp
		}
		return &c, nil
	default:
		return t, nil
	}
}

// TermVariables succeeds if vars unifies with a list of variables in term.
func TermVariables(vm *VM, term, vars Term, k Cont, env *Env) *Promise {
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
			args, err := makeSlice(t.Arity())
			if err != nil {
				return Error(resourceError(resourceMemory, env))
			}
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
func Op(vm *VM, priority, specifier, op Term, k Cont, env *Env) *Promise {
	var p Integer
	switch priority := env.Resolve(priority).(type) {
	case Variable:
		return Error(InstantiationError(env))
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
		return Error(InstantiationError(env))
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

	m := vm.TypeInModule()

	for _, name := range names {
		if p := validateOp(m, p, spec, name, env); p != nil {
			return p
		}
	}

	for _, name := range names {
		if class := spec.class(); m.operators.definedInClass(name, spec.class()) {
			m.operators.remove(name, class)
		}

		m.operators.define(p, spec, name)
	}

	return k(env)
}

func validateOp(m *module, p Integer, spec operatorSpecifier, name Atom, env *Env) *Promise {
	switch name {
	case atomComma:
		if m.operators.definedInClass(name, operatorClassInfix) {
			return Error(permissionError(operationModify, permissionTypeOperator, name, env))
		}
	case atomBar:
		if spec.class() != operatorClassInfix || (p > 0 && p < 1001) {
			op := operationCreate
			if m.operators.definedInClass(name, operatorClassInfix) {
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
		if m.operators.definedInClass(name, operatorClassPostfix) {
			return Error(permissionError(operationCreate, permissionTypeOperator, name, env))
		}
	case operatorClassPostfix:
		if m.operators.definedInClass(name, operatorClassInfix) {
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
func CurrentOp(vm *VM, priority, specifier, op Term, k Cont, env *Env) *Promise {
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

	m := vm.TypeInModule()
	pattern := tuple(priority, specifier, op)
	ks := make([]func(context.Context) *Promise, 0, len(m.operators)*int(_operatorClassLen))
	for _, ops := range m.operators {
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
func Assertz(vm *VM, t Term, k Cont, env *Env) *Promise {
	if err := assertMerge(vm, t, func(existing, new []clause) clauses {
		return append(existing, new...)
	}, env); err != nil {
		return Error(err)
	}
	return k(env)
}

// Asserta prepends t to the database.
func Asserta(vm *VM, t Term, k Cont, env *Env) *Promise {
	if err := assertMerge(vm, t, func(existing, new []clause) clauses {
		return append(new, existing...)
	}, env); err != nil {
		return Error(err)
	}
	return k(env)
}

func assertMerge(vm *VM, t Term, merge func([]clause, []clause) clauses, env *Env) error {
	pi, arg, err := piArg(t, env)
	if err != nil {
		return err
	}

	if pi == (predicateIndicator{name: atomColonMinus, arity: 2}) {
		pi, _, err = piArg(arg(0), env)
		if err != nil {
			return err
		}
	}

	m := vm.TypeInModule()
	if m.procedures == nil {
		m.procedures = map[predicateIndicator]procedureEntry{}
	}
	e, ok := m.procedures[pi]
	if !ok {
		e.dynamic = true
		e.procedure = clauses{}
		m.procedures[pi] = e
	}

	added, err := compileTerm(vm.typeIn, t, env)
	if err != nil {
		return err
	}

	cs, ok := e.procedure.(clauses)
	if !ok || !e.dynamic {
		return permissionError(operationModify, permissionTypeStaticProcedure, pi.Term(), env)
	}

	e.procedure = merge(cs, added)
	m.procedures[pi] = e
	return nil
}

// BagOf collects all the solutions of goal as instances, which unify with template. instances may contain duplications.
func BagOf(vm *VM, template, goal, instances Term, k Cont, env *Env) *Promise {
	return collectionOf(vm, func(tList []Term, env *Env) Term {
		return List(tList...)
	}, template, goal, instances, k, env)
}

// SetOf collects all the solutions of goal as instances, which unify with template. instances don't contain duplications.
func SetOf(vm *VM, template, goal, instances Term, k Cont, env *Env) *Promise {
	return collectionOf(vm, func(tList []Term, env *Env) Term {
		return env.set(tList...)
	}, template, goal, instances, k, env)
}

func collectionOf(vm *VM, agg func([]Term, *Env) Term, template, goal, instances Term, k Cont, env *Env) *Promise {
	fvs := newFreeVariablesSet(goal, template, env)
	w, err := makeSlice(len(fvs))
	if err != nil {
		return Error(resourceError(resourceMemory, env))
	}
	w = w[:0]
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
					env, _ = env.Unify(witness, w)
				}
				return Unify(vm, agg(tList, env), instances, k, env)
			})
		}
		return Delay(ks...)
	}, env)
}

func variant(t1, t2 Term, env *Env) bool {
	s := map[Variable]Variable{}
	rest := [][2]Term{
		{t1, t2},
	}
	var xy [2]Term
	for len(rest) > 0 {
		rest, xy = rest[:len(rest)-1], rest[len(rest)-1]
		x, y := env.Resolve(xy[0]), env.Resolve(xy[1])
		switch x := x.(type) {
		case Variable:
			switch y := y.(type) {
			case Variable:
				if z, ok := s[x]; ok {
					if z != y {
						return false
					}
				} else {
					s[x] = y
				}
			default:
				return false
			}
		case Compound:
			switch y := y.(type) {
			case Compound:
				if x.Functor() != y.Functor() || x.Arity() != y.Arity() {
					return false
				}
				for i := 0; i < x.Arity(); i++ {
					rest = append(rest, [2]Term{x.Arg(i), y.Arg(i)})
				}
			default:
				return false
			}
		default:
			if x != y {
				return false
			}
		}
	}
	return true
}

func iteratedGoalTerm(t Term, env *Env) Term {
	for {
		c, ok := env.Resolve(t).(Compound)
		if !ok || c.Functor() != atomCaret || c.Arity() != 2 {
			return t
		}
		t = c.Arg(1)
	}
}

// FindAll collects all the solutions of goal as instances, which unify with template. instances may contain duplications.
func FindAll(vm *VM, template, goal, instances Term, k Cont, env *Env) *Promise {
	iter := ListIterator{List: instances, Env: env, AllowPartial: true}
	for iter.Next() {
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}
	return Delay(func(ctx context.Context) *Promise {
		var answers []Term
		if _, err := Call(vm, goal, func(env *Env) *Promise {
			c, err := renamedCopy(template, nil, env)
			if err != nil {
				return Error(err)
			}
			answers = append(answers, c)
			return Bool(false) // ask for more solutions
		}, env).Force(ctx); err != nil {
			return Error(err)
		}
		return Unify(vm, instances, List(answers...), k, env)
	})
}

// Compare compares term1 and term2 and unifies order with <, =, or >.
func Compare(vm *VM, order, term1, term2 Term, k Cont, env *Env) *Promise {
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

	switch o := term1.Compare(term2, env); o {
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
func Between(vm *VM, lower, upper, value Term, k Cont, env *Env) *Promise {
	var low, high Integer

	switch lower := env.Resolve(lower).(type) {
	case Integer:
		low = lower
	case Variable:
		return Error(InstantiationError(env))
	default:
		return Error(typeError(validTypeInteger, lower, env))
	}

	switch upper := env.Resolve(upper).(type) {
	case Integer:
		high = upper
	case Variable:
		return Error(InstantiationError(env))
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
func Sort(vm *VM, list, sorted Term, k Cont, env *Env) *Promise {
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

	return Unify(vm, sorted, env.set(elems...), k, env)
}

// KeySort succeeds if sorted is a sorted list of pairs based on their keys.
func KeySort(vm *VM, pairs, sorted Term, k Cont, env *Env) *Promise {
	var elems []Term
	iter := ListIterator{List: pairs, Env: env}
	for iter.Next() {
		switch e := env.Resolve(iter.Current()).(type) {
		case Variable:
			return Error(InstantiationError(env))
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
		return elems[i].(Compound).Arg(0).Compare(elems[j].(Compound).Arg(0), env) == -1
	})

	return Unify(vm, sorted, List(elems...), k, env)
}

// Throw throws ball as an exception.
func Throw(_ *VM, ball Term, _ Cont, env *Env) *Promise {
	switch b := env.Resolve(ball).(type) {
	case Variable:
		return Error(InstantiationError(env))
	default:
		return Error(NewException(b, env))
	}
}

// Catch calls goal. If an exception is thrown and unifies with catcher, it calls recover.
func Catch(vm *VM, goal, catcher, recover Term, k Cont, env *Env) *Promise {
	return catch(func(err error) *Promise {
		e, ok := err.(Exception)
		if !ok {
			e = Exception{term: atomError.Apply(NewAtom("system_error"), NewAtom(err.Error()))}
		}

		env, ok := env.Unify(catcher, e.term)
		if !ok {
			return nil
		}

		return Call(vm, recover, k, env)
	}, func(ctx context.Context) *Promise {
		return Call(vm, goal, k, env)
	})
}

// CurrentPredicate matches pi with a predicate indicator of the user-defined procedures in the database.
func CurrentPredicate(vm *VM, pi Term, k Cont, env *Env) *Promise {
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

	m := vm.TypeInModule()
	ks := make([]func(context.Context) *Promise, 0, len(m.procedures))
	for key, e := range m.procedures {
		switch e.procedure.(type) {
		case clauses:
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
func Retract(vm *VM, t Term, k Cont, env *Env) *Promise {
	m := vm.TypeInModule()
	t = rulify(t, env)

	h := t.(Compound).Arg(0)
	pi, _, err := piArg(h, env)
	if err != nil {
		return Error(err)
	}

	e, ok := m.procedures[pi]
	if !ok {
		return Bool(false)
	}

	cs, ok := e.procedure.(clauses)
	if !ok || !e.dynamic {
		return Error(permissionError(operationModify, permissionTypeStaticProcedure, pi.Term(), env))
	}

	deleted := 0
	ks := make([]func(context.Context) *Promise, len(cs))
	for i, c := range cs {
		i := i
		raw := rulify(c.raw, env)
		ks[i] = func(_ context.Context) *Promise {
			return Unify(vm, t, raw, func(env *Env) *Promise {
				j := i - deleted
				cs, cs[len(cs)-1] = append(cs[:j], cs[j+1:]...), clause{}
				e.procedure = cs
				m.procedures[pi] = e
				deleted++
				return k(env)
			}, env)
		}
	}
	return Delay(ks...)
}

// Abolish removes the procedure indicated by pi from the database.
func Abolish(vm *VM, pi Term, k Cont, env *Env) *Promise {
	switch pi := env.Resolve(pi).(type) {
	case Variable:
		return Error(InstantiationError(env))
	case Compound:
		if pi.Functor() != atomSlash || pi.Arity() != 2 {
			return Error(typeError(validTypePredicateIndicator, pi, env))
		}

		name, arity := pi.Arg(0), pi.Arg(1)

		switch name := env.Resolve(name).(type) {
		case Variable:
			return Error(InstantiationError(env))
		case Atom:
			switch arity := env.Resolve(arity).(type) {
			case Variable:
				return Error(InstantiationError(env))
			case Integer:
				if arity < 0 {
					return Error(domainError(validDomainNotLessThanZero, arity, env))
				}
				m := vm.TypeInModule()
				key := predicateIndicator{name: name, arity: arity}
				if e, ok := m.procedures[key]; !ok || !e.dynamic {
					return Error(permissionError(operationModify, permissionTypeStaticProcedure, key.Term(), env))
				}
				delete(m.procedures, key)
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
func CurrentInput(vm *VM, stream Term, k Cont, env *Env) *Promise {
	switch env.Resolve(stream).(type) {
	case Variable, *Stream:
		return Unify(vm, stream, vm.input, k, env)
	default:
		return Error(domainError(validDomainStream, stream, env))
	}
}

// CurrentOutput unifies stream with the current output stream.
func CurrentOutput(vm *VM, stream Term, k Cont, env *Env) *Promise {
	switch env.Resolve(stream).(type) {
	case Variable, *Stream:
		return Unify(vm, stream, vm.output, k, env)
	default:
		return Error(domainError(validDomainStream, stream, env))
	}
}

// SetInput sets streamOrAlias as the current input stream.
func SetInput(vm *VM, streamOrAlias Term, k Cont, env *Env) *Promise {
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
func SetOutput(vm *VM, streamOrAlias Term, k Cont, env *Env) *Promise {
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
		return nil, InstantiationError(env)
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
func Open(vm *VM, sourceSink, mode, stream, options Term, k Cont, env *Env) *Promise {
	var name string
	switch s := env.Resolve(sourceSink).(type) {
	case Variable:
		return Error(InstantiationError(env))
	case Atom:
		name = s.String()
	default:
		return Error(domainError(validDomainSourceSink, sourceSink, env))
	}

	var streamMode ioMode
	switch m := env.Resolve(mode).(type) {
	case Variable:
		return Error(InstantiationError(env))
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
		return Error(InstantiationError(env))
	}

	s := Stream{vm: vm, mode: streamMode}
	switch f, err := openFile(name, int(s.mode), 0644); {
	case err == nil:
		if s.mode == ioModeRead {
			s.source = f
			s.initRead()
		} else {
			s.sink = f
		}
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

	return Unify(vm, stream, &s, k, env)
}

func handleStreamOption(vm *VM, s *Stream, option Term, env *Env) error {
	switch o := env.Resolve(option).(type) {
	case Variable:
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

func handleStreamOptionAlias(vm *VM, s *Stream, o Compound, env *Env) error {
	switch a := env.Resolve(o.Arg(0)).(type) {
	case Variable:
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

func handleStreamOptionType(_ *VM, s *Stream, o Compound, env *Env) error {
	switch t := env.Resolve(o.Arg(0)).(type) {
	case Variable:
		return InstantiationError(env)
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

func handleStreamOptionEOFAction(_ *VM, s *Stream, o Compound, env *Env) error {
	switch e := env.Resolve(o.Arg(0)).(type) {
	case Variable:
		return InstantiationError(env)
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
func Close(vm *VM, streamOrAlias, options Term, k Cont, env *Env) *Promise {
	s, err := stream(vm, streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	var force bool
	iter := ListIterator{List: options, Env: env}
	for iter.Next() {
		switch option := env.Resolve(iter.Current()).(type) {
		case Variable:
			return Error(InstantiationError(env))
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
func FlushOutput(vm *VM, streamOrAlias Term, k Cont, env *Env) *Promise {
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
func WriteTerm(vm *VM, streamOrAlias, t, options Term, k Cont, env *Env) *Promise {
	s, err := stream(vm, streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	m := vm.TypeInModule()
	opts := WriteOptions{
		ops:      m.operators,
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

	w, err := s.textWriter()
	switch {
	case errors.Is(err, errWrongIOMode):
		return Error(permissionError(operationOutput, permissionTypeStream, streamOrAlias, env))
	case errors.Is(err, errWrongStreamType):
		return Error(permissionError(operationOutput, permissionTypeBinaryStream, streamOrAlias, env))
	case err != nil:
		return Error(err)
	}

	if err := env.Resolve(t).WriteTerm(w, &opts, env); err != nil {
		return Error(err)
	}

	return k(env)
}

func writeTermOption(opts *WriteOptions, option Term, env *Env) error {
	switch o := env.Resolve(option).(type) {
	case Variable:
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

func writeTermOptionBool(o Compound, env *Env) (bool, error) {
	switch v := env.Resolve(o.Arg(0)).(type) {
	case Variable:
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

func writeTermOptionVariableNames(option Compound, env *Env) (map[Variable]Atom, error) {
	vns := map[Variable]Atom{}
	iter := ListIterator{List: option.Arg(0), Env: env}
	for iter.Next() {
		var vn Compound
		switch elem := env.Resolve(iter.Current()).(type) {
		case Variable:
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
		case Variable:
			return nil, InstantiationError(env)
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
		return nil, InstantiationError(env)
	case Atom:
		if s != atomEmptyList {
			return nil, domainError(validDomainWriteOption, option, env)
		}
		return vns, nil
	default:
		return nil, domainError(validDomainWriteOption, option, env)
	}
}

func writeTermOptionInteger(o Compound, env *Env) (Integer, error) {
	switch v := env.Resolve(o.Arg(0)).(type) {
	case Variable:
		return 0, InstantiationError(env)
	case Integer:
		return v, nil
	}
	return 0, domainError(validDomainWriteOption, o, env)
}

// CharCode converts a single-rune Atom char to an Integer code, or vice versa.
func CharCode(vm *VM, char, code Term, k Cont, env *Env) *Promise {
	switch ch := env.Resolve(char).(type) {
	case Variable:
		switch cd := env.Resolve(code).(type) {
		case Variable:
			return Error(InstantiationError(env))
		case Integer:
			r := rune(cd)

			if !utf8.ValidRune(r) {
				return Error(representationError(flagCharacterCode, env))
			}

			return Unify(vm, ch, Atom(r), k, env)
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

		return Unify(vm, code, Integer(rs[0]), k, env)
	default:
		return Error(typeError(validTypeCharacter, ch, env))
	}
}

// PutByte outputs an integer byte to a stream represented by streamOrAlias.
func PutByte(vm *VM, streamOrAlias, byt Term, k Cont, env *Env) *Promise {
	s, err := stream(vm, streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	switch b := env.Resolve(byt).(type) {
	case Variable:
		return Error(InstantiationError(env))
	case Integer:
		if 0 > b || 255 < b {
			return Error(typeError(validTypeByte, byt, env))
		}

		switch err := s.WriteByte(byte(b)); {
		case errors.Is(err, errWrongIOMode):
			return Error(permissionError(operationOutput, permissionTypeStream, streamOrAlias, env))
		case errors.Is(err, errWrongStreamType):
			return Error(permissionError(operationOutput, permissionTypeTextStream, streamOrAlias, env))
		case err != nil:
			return Error(err)
		}

		return k(env)
	default:
		return Error(typeError(validTypeByte, byt, env))
	}
}

// PutChar outputs char to the stream represented by streamOrAlias.
func PutChar(vm *VM, streamOrAlias, char Term, k Cont, env *Env) *Promise {
	s, err := stream(vm, streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	switch c := env.Resolve(char).(type) {
	case Variable:
		return Error(InstantiationError(env))
	case Atom:
		if c > utf8.MaxRune {
			return Error(typeError(validTypeCharacter, c, env))
		}

		r := rune(c)

		switch _, err := s.WriteRune(r); {
		case errors.Is(err, errWrongIOMode):
			return Error(permissionError(operationOutput, permissionTypeStream, streamOrAlias, env))
		case errors.Is(err, errWrongStreamType):
			return Error(permissionError(operationOutput, permissionTypeBinaryStream, streamOrAlias, env))
		case err != nil:
			return Error(err)
		}

		return k(env)
	default:
		return Error(typeError(validTypeCharacter, char, env))
	}
}

type readTermOptions struct {
	singletons    Term
	variables     Term
	variableNames Term
}

// ReadTerm reads from the stream represented by streamOrAlias and unifies with stream.
func ReadTerm(vm *VM, streamOrAlias, out, options Term, k Cont, env *Env) *Promise {
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

	p := NewParser(vm.TypeInModule, s)
	defer func() {
		_ = s.UnreadRune()
	}()

	t, err := p.Term()
	switch {
	case err == nil:
		break
	case errors.Is(err, io.EOF):
		return Unify(vm, out, atomEndOfFile, k, env)
	case errors.Is(err, errWrongIOMode):
		return Error(permissionError(operationInput, permissionTypeStream, streamOrAlias, env))
	case errors.Is(err, errWrongStreamType):
		return Error(permissionError(operationInput, permissionTypeBinaryStream, streamOrAlias, env))
	case errors.Is(err, errPastEndOfStream):
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
func GetByte(vm *VM, streamOrAlias, inByte Term, k Cont, env *Env) *Promise {
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
func GetChar(vm *VM, streamOrAlias, char Term, k Cont, env *Env) *Promise {
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
func PeekByte(vm *VM, streamOrAlias, inByte Term, k Cont, env *Env) *Promise {
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
func PeekChar(vm *VM, streamOrAlias, char Term, k Cont, env *Env) *Promise {
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
func Halt(_ *VM, n Term, k Cont, env *Env) *Promise {
	switch code := env.Resolve(n).(type) {
	case Variable:
		return Error(InstantiationError(env))
	case Integer:
		osExit(int(code))
		return k(env)
	default:
		return Error(typeError(validTypeInteger, n, env))
	}
}

// Clause unifies head and body with H and B respectively where H :- B is in the database.
func Clause(vm *VM, head, body Term, k Cont, env *Env) *Promise {
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

	m := vm.TypeInModule()
	e, ok := m.procedures[pi]
	if !ok {
		return Bool(false)
	}

	cs, ok := e.procedure.(clauses)
	if !ok || !e.public {
		return Error(permissionError(operationAccess, permissionTypePrivateProcedure, pi.Term(), env))
	}

	ks := make([]func(context.Context) *Promise, len(cs))
	for i, c := range cs {
		cp, err := renamedCopy(c.raw, nil, env)
		if err != nil {
			return Error(err)
		}
		r := rulify(cp, env)
		ks[i] = func(context.Context) *Promise {
			return Unify(vm, atomColonMinus.Apply(head, body), r, k, env)
		}
	}
	return Delay(ks...)
}

func rulify(t Term, env *Env) Term {
	t = env.Resolve(t)
	if c, ok := t.(Compound); ok && c.Functor() == atomColonMinus && c.Arity() == 2 {
		return t
	}
	return atomColonMinus.Apply(t, atomTrue)
}

// AtomLength counts the runes in atom and unifies the result with length.
func AtomLength(vm *VM, atom, length Term, k Cont, env *Env) *Promise {
	var a Atom
	switch atom := env.Resolve(atom).(type) {
	case Variable:
		return Error(InstantiationError(env))
	case Atom:
		a = atom
	default:
		return Error(typeError(validTypeAtom, atom, env))
	}

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

	return Unify(vm, length, Integer(len([]rune(a.String()))), k, env)
}

// AtomConcat concatenates atom1 and atom2 and unifies it with atom3.
func AtomConcat(vm *VM, atom1, atom2, atom3 Term, k Cont, env *Env) *Promise {
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
				return Unify(vm, pattern, tuple(NewAtom(a1), NewAtom(a2)), k, env)
			})
		}
		ks = append(ks, func(context.Context) *Promise {
			return Unify(vm, pattern, tuple(a3, atomEmpty), k, env)
		})
		return Delay(ks...)
	default:
		return Error(typeError(validTypeAtom, atom3, env))
	}
}

// SubAtom unifies subAtom with a sub atom of length which appears with before runes preceding it and after runes following it.
func SubAtom(vm *VM, atom, before, length, after, subAtom Term, k Cont, env *Env) *Promise {
	switch whole := env.Resolve(atom).(type) {
	case Variable:
		return Error(InstantiationError(env))
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
func AtomChars(vm *VM, atom, chars Term, k Cont, env *Env) *Promise {
	switch a := env.Resolve(atom).(type) {
	case Variable:
		var sb strings.Builder
		iter := ListIterator{List: chars, Env: env}
		for iter.Next() {
			switch e := env.Resolve(iter.Current()).(type) {
			case Variable:
				return Error(InstantiationError(env))
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
func AtomCodes(vm *VM, atom, codes Term, k Cont, env *Env) *Promise {
	switch a := env.Resolve(atom).(type) {
	case Variable:
		var sb strings.Builder
		iter := ListIterator{List: codes, Env: env}
		for iter.Next() {
			switch e := env.Resolve(iter.Current()).(type) {
			case Variable:
				return Error(InstantiationError(env))
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
func NumberChars(vm *VM, num, chars Term, k Cont, env *Env) *Promise {
	var sb strings.Builder
	iter := ListIterator{List: chars, Env: env, AllowPartial: true}
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
		return Error(err)
	}
	if _, ok := iter.Suffix().(Variable); ok {
		return numberCharsWrite(vm, num, chars, k, env)
	}

	p := Parser{
		Lexer: Lexer{
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

func numberCharsWrite(vm *VM, num, chars Term, k Cont, env *Env) *Promise {
	var n Number
	switch num := env.Resolve(num).(type) {
	case Variable:
		return Error(InstantiationError(env))
	case Number:
		n = num
	default:
		return Error(typeError(validTypeNumber, num, env))
	}

	iter := ListIterator{List: chars, Env: env, AllowPartial: true}
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
		return Error(err)
	}

	var buf bytes.Buffer
	_ = n.WriteTerm(&buf, &defaultWriteOptions, nil)
	rs := []rune(buf.String())

	cs := make([]Term, len(rs))
	for i, r := range rs {
		cs[i] = Atom(r)
	}
	return Unify(vm, chars, List(cs...), k, env)
}

// NumberCodes breaks up an atom representation of a number num into a list of runes and unifies it with codes, or
// constructs a number from a list of runes codes and unifies it with num.
func NumberCodes(vm *VM, num, codes Term, k Cont, env *Env) *Promise {
	var sb strings.Builder
	iter := ListIterator{List: codes, Env: env, AllowPartial: true}
	for iter.Next() {
		switch e := env.Resolve(iter.Current()).(type) {
		case Variable:
			return numberCodesWrite(vm, num, codes, k, env)
		case Integer:
			if !utf8.ValidRune(rune(e)) {
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
	if _, ok := iter.Suffix().(Variable); ok {
		return numberCodesWrite(vm, num, codes, k, env)
	}

	p := Parser{
		Lexer: Lexer{
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

func numberCodesWrite(vm *VM, num, codes Term, k Cont, env *Env) *Promise {
	var n Number
	switch num := env.Resolve(num).(type) {
	case Variable:
		return Error(InstantiationError(env))
	case Number:
		n = num
	default:
		return Error(typeError(validTypeNumber, num, env))
	}

	iter := ListIterator{List: codes, Env: env, AllowPartial: true}
	for iter.Next() {
		switch e := env.Resolve(iter.Current()).(type) {
		case Variable:
			break
		case Integer:
			if !utf8.ValidRune(rune(e)) {
				return Error(representationError(flagCharacterCode, env))
			}
		default:
			return Error(typeError(validTypeInteger, e, env))
		}
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}

	var buf bytes.Buffer
	_ = n.WriteTerm(&buf, &defaultWriteOptions, nil)
	rs := []rune(buf.String())

	cs := make([]Term, len(rs))
	for i, r := range rs {
		cs[i] = Integer(r)
	}
	return Unify(vm, codes, List(cs...), k, env)
}

// StreamProperty succeeds iff the stream represented by stream has the stream property.
func StreamProperty(vm *VM, stream, property Term, k Cont, env *Env) *Promise {
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
func SetStreamPosition(vm *VM, streamOrAlias, position Term, k Cont, env *Env) *Promise {
	s, err := stream(vm, streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	switch p := env.Resolve(position).(type) {
	case Variable:
		return Error(InstantiationError(env))
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
func CharConversion(vm *VM, inChar, outChar Term, k Cont, env *Env) *Promise {
	switch in := env.Resolve(inChar).(type) {
	case Variable:
		return Error(InstantiationError(env))
	case Atom:
		i := []rune(in.String())
		if len(i) != 1 {
			return Error(representationError(flagCharacter, env))
		}

		switch out := env.Resolve(outChar).(type) {
		case Variable:
			return Error(InstantiationError(env))
		case Atom:
			o := []rune(out.String())
			if len(o) != 1 {
				return Error(representationError(flagCharacter, env))
			}

			m := vm.TypeInModule()
			if m.charConversions == nil {
				m.charConversions = map[rune]rune{}
			}
			if i[0] == o[0] {
				delete(m.charConversions, i[0])
				return k(env)
			}
			m.charConversions[i[0]] = o[0]
			return k(env)
		default:
			return Error(representationError(flagCharacter, env))
		}
	default:
		return Error(representationError(flagCharacter, env))
	}
}

// CurrentCharConversion succeeds iff a conversion from inChar to outChar is defined.
func CurrentCharConversion(vm *VM, inChar, outChar Term, k Cont, env *Env) *Promise {
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

	m := vm.TypeInModule()
	if c1, ok := env.Resolve(inChar).(Atom); ok {
		r := []rune(c1.String())
		if r, ok := m.charConversions[r[0]]; ok {
			return Unify(vm, outChar, Atom(r), k, env)
		}
		return Unify(vm, outChar, c1, k, env)
	}

	pattern := tuple(inChar, outChar)
	ks := make([]func(context.Context) *Promise, 256)
	for i := 0; i < 256; i++ {
		r := rune(i)
		cr, ok := m.charConversions[r]
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
func SetPrologFlag(vm *VM, flag, value Term, k Cont, env *Env) *Promise {
	switch f := env.Resolve(flag).(type) {
	case Variable:
		return Error(InstantiationError(env))
	case Atom:
		m := vm.TypeInModule()
		var modify func(m *module, value Atom) error
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
			return Error(InstantiationError(env))
		case Atom:
			if err := modify(m, v); err != nil {
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

func modifyCharConversion(m *module, value Atom) error {
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

func modifyDebug(m *module, value Atom) error {
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

func modifyUnknown(m *module, value Atom) error {
	switch value {
	case atomError:
		m.unknown = unknownError
	case atomWarning:
		m.unknown = unknownWarning
	case atomFail:
		m.unknown = unknownFail
	default:
		return domainError(validDomainFlagValue, atomPlus.Apply(atomUnknown, value), nil)
	}
	return nil
}

func modifyDoubleQuotes(m *module, value Atom) error {
	switch value {
	case atomCodes:
		m.doubleQuotes = doubleQuotesCodes
	case atomChars:
		m.doubleQuotes = doubleQuotesChars
	case atomAtom:
		m.doubleQuotes = doubleQuotesAtom
	default:
		return domainError(validDomainFlagValue, atomPlus.Apply(atomDoubleQuotes, value), nil)
	}
	return nil
}

// CurrentPrologFlag succeeds iff flag is set to value.
func CurrentPrologFlag(vm *VM, flag, value Term, k Cont, env *Env) *Promise {
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

	m := vm.TypeInModule()
	pattern := tuple(flag, value)
	flags := []Term{
		tuple(atomBounded, atomTrue),
		tuple(atomMaxInteger, maxInt),
		tuple(atomMinInteger, minInt),
		tuple(atomIntegerRoundingFunction, atomTowardZero),
		tuple(atomCharConversion, onOff(m.charConvEnabled)),
		tuple(atomDebug, onOff(m.debug)),
		tuple(atomMaxArity, atomUnbounded),
		tuple(atomUnknown, NewAtom(m.unknown.String())),
		tuple(atomDoubleQuotes, NewAtom(m.doubleQuotes.String())),
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
func ExpandTerm(vm *VM, term1, term2 Term, k Cont, env *Env) *Promise {
	t, err := expand(vm, term1, env)
	if err != nil {
		return Error(err)
	}

	return Unify(vm, t, term2, k, env)
}

func expand(vm *VM, term Term, env *Env) (Term, error) {
	m := vm.TypeInModule()
	if _, ok := m.procedures[predicateIndicator{name: atomTermExpansion, arity: 2}]; ok {
		var ret Term
		v := NewVariable()
		ok, err := Call(vm, atomTermExpansion.Apply(term, v), func(env *Env) *Promise {
			ret = env.simplify(v)
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

// Nth0 succeeds if elem is the n-th element of list, counting from 0.
func Nth0(vm *VM, n, list, elem Term, k Cont, env *Env) *Promise {
	return nth(vm, 0, n, list, elem, k, env)
}

// Nth1 succeeds if elem is the n-th element of list, counting from 1.
func Nth1(vm *VM, n, list, elem Term, k Cont, env *Env) *Promise {
	return nth(vm, 1, n, list, elem, k, env)
}

func nth(vm *VM, base Integer, n, list, elem Term, k Cont, env *Env) *Promise {
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
func Succ(vm *VM, x, s Term, k Cont, env *Env) *Promise {
	switch x := x.(type) {
	case Variable:
		switch s := s.(type) {
		case Variable:
			return Error(InstantiationError(env))
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
func Length(vm *VM, list, length Term, k Cont, env *Env) *Promise {
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

func lengthRundown(vm *VM, list Variable, n Integer, k Cont, env *Env) *Promise {
	elems, err := makeSlice(int(n))
	if err != nil {
		return Error(resourceError(resourceMemory, env))
	}
	for i := range elems {
		elems[i] = NewVariable()
	}
	return Unify(vm, list, List(elems...), k, env)
}

func lengthAddendum(vm *VM, suffix Term, offset Integer, list, length Variable, k Cont, env *Env) *Promise {
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
func SkipMaxList(vm *VM, skip, max, list, suffix Term, k Cont, env *Env) *Promise {
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
func Append(vm *VM, xs, ys, zs Term, k Cont, env *Env) *Promise {
	// A special case for non-empty lists without a variable in the spine.
	if xs, ok := env.Resolve(xs).(Compound); ok {
		iter := ListIterator{List: xs, Env: nil} // No variables allowed.
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

func appendLists(vm *VM, xs, ys, zs Term, k Cont, env *Env) *Promise {
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

func Dynamic(vm *VM, pi Term, k Cont, env *Env) *Promise {
	m := vm.TypeInModule()
	iter := anyIterator{Any: pi, Env: env}
	for iter.Next() {
		pi, err := mustBePI(iter.Current(), env)
		if err != nil {
			return Error(err)
		}
		e, _ := m.procedures[pi]
		e.dynamic = true
		e.public = true
		if e.procedure == nil {
			e.procedure = clauses{}
		}
		m.procedures[pi] = e
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}
	return k(env)
}

func Multifile(vm *VM, pi Term, k Cont, env *Env) *Promise {
	m := vm.TypeInModule()
	iter := anyIterator{Any: pi, Env: env}
	for iter.Next() {
		pi, err := mustBePI(iter.Current(), env)
		if err != nil {
			return Error(err)
		}
		e, _ := m.procedures[pi]
		e.multifile = true
		m.procedures[pi] = e
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}
	return k(env)
}

func Discontiguous(vm *VM, pi Term, k Cont, env *Env) *Promise {
	m := vm.TypeInModule()
	iter := anyIterator{Any: pi, Env: env}
	for iter.Next() {
		pi, err := mustBePI(iter.Current(), env)
		if err != nil {
			return Error(err)
		}
		e, _ := m.procedures[pi]
		e.discontiguous = true
		m.procedures[pi] = e
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}
	return k(env)
}

func Initialization(vm *VM, goal Term, k Cont, env *Env) *Promise {
	m := vm.TypeInModule()
	m.initGoals = append(m.initGoals, goal)
	return k(env)
}

func Include(vm *VM, file Term, k Cont, env *Env) *Promise {
	f, err := mustBeAtom(file, env)
	if err != nil {
		return Error(err)
	}
	return Delay(func(ctx context.Context) *Promise {
		if _, err := vm.LoadFile(ctx, f.String()); err != nil {
			return Error(err)
		}
		return k(env)
	})
}

// LoadFile loads a Prolog text from a file.
func LoadFile(vm *VM, file, options Term, k Cont, env *Env) *Promise {
	filename, err := mustBeAtom(file, env)
	if err != nil {
		return Error(err)
	}
	fn := filename.String()

	// TODO: implement absolute_file_name/[2, 3]?
	if ext := filepath.Ext(fn); ext == "" {
		fn += ".pl"
	}

	var (
		onlyIfChanged bool
		importList    []predicateIndicator
	)
	iter := ListIterator{List: options, Env: env}
	for iter.Next() {
		opt := iter.Current()

		if _, ok := env.Unify(opt, atomIf.Apply(atomTrue)); ok {
			onlyIfChanged = false
			break
		}

		if _, ok := env.Unify(opt, atomIf.Apply(atomChanged)); ok {
			onlyIfChanged = true
			break
		}

		imports := NewVariable()
		if env, ok := env.Unify(opt, atomImports.Apply(imports)); ok {
			iter := ListIterator{List: imports, Env: env}
			for iter.Next() {
				i := iter.Current()

				pi, err := mustBePI(i, env)
				if err != nil {
					return Error(err)
				}
				importList = append(importList, pi)
			}
			if err := iter.Err(); err != nil {
				return Error(err)
			}
		}
	}

	var opts []LoadFileOption
	if onlyIfChanged {
		opts = append(opts, LoadFileOptionIfChanged)
	}
	module, err := vm.LoadFile(context.Background(), fn, opts...)
	switch {
	case err == nil:
		break
	case errors.Is(err, fs.ErrInvalid):
		fallthrough
	case errors.Is(err, fs.ErrNotExist):
		return Error(existenceError(objectTypeSourceSink, file, env))
	default:
		return Error(err)
	}

	vm.importPredicates(vm.typeIn, module, importList)

	return k(env)
}

func DefineModule(vm *VM, moduleName, exportList Term, k Cont, env *Env) *Promise {
	var name Atom
	switch n := env.Resolve(moduleName).(type) {
	case Variable:
		return Error(InstantiationError(env))
	case Atom:
		name = n
	default:
		return Error(typeError(validTypeAtom, moduleName, env))
	}

	var pis []predicateIndicator
	iter := ListIterator{List: exportList, Env: env}
	for iter.Next() {
		pi, err := mustBePI(iter.Current(), env)
		if err != nil {
			return Error(err)
		}
		pis = append(pis, pi)
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}

	m := vm.module(name)
	m.reset()

	vm.importPredicates(name, vm.system, nil)
	vm.SetModule(name)

	return k(env)
}

func MetaPredicate(vm *VM, mi Term, k Cont, env *Env) *Promise {
	m := vm.TypeInModule()
	iter := anyIterator{Any: mi, Env: env}
	for iter.Next() {
		pi, arg, err := piArg(mi, env)
		if err != nil {
			return Error(err)
		}
		e, _ := m.procedures[pi]
		e.metaPredicate = make([]metaArgumentSpecifier, pi.arity)
		for i := 0; i < int(pi.arity); i++ {
			switch t := env.Resolve(arg(i)).(type) {
			case Variable:
				return Error(InstantiationError(env))
			case Atom:
				e.metaPredicate[i] = metaArgumentSpecifier{atom: t}
			case Integer:
				e.metaPredicate[i] = metaArgumentSpecifier{integer: t}
			default:
				return Error(domainError(validDomainMetaArgumentSpecifier, t, env))
			}
		}
		m.procedures[pi] = e
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}
	return k(env)
}
