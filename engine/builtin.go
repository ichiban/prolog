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

// State represents the internal state of an interpreter.
type State struct {
	VM

	FS     fs.FS
	loaded map[string]struct{}

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

// SetUserInput sets the given reader as a stream with an alias of user_input.
func (state *State) SetUserInput(r io.Reader) {
	s := Stream{
		sourceSink: r,
		mode:       ioModeRead,
		alias:      atomUserInput,
		eofAction:  eofActionReset,
		reposition: false,
		streamType: streamTypeText,
	}
	if state.streams == nil {
		state.streams = map[Term]*Stream{}
	}
	state.streams[s.alias] = &s
	state.input = &s
}

// SetUserOutput sets the given writer as a stream with an alias of user_output.
func (state *State) SetUserOutput(w io.Writer) {
	s := Stream{
		sourceSink: w,
		mode:       ioModeAppend,
		alias:      atomUserOutput,
		eofAction:  eofActionReset,
		reposition: false,
		streamType: streamTypeText,
	}

	if state.streams == nil {
		state.streams = map[Term]*Stream{}
	}
	state.streams[s.alias] = &s
	state.output = &s
}

// Parser creates a new parser from the current State and io.Reader.
// If non-nil, vars will hold the information on variables it parses.
func (state *State) Parser(r io.RuneReader, vars *[]ParsedVariable) *Parser {
	if state.operators == nil {
		state.operators = operators{}
	}
	return newParser(r,
		withCharConversions(state.charConversions),
		withOperators(state.operators),
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
		call := NewAtom("$call")
		cs, err := compile(&compound{
			functor: atomIf,
			args: []Term{
				call.Apply(args...),
				g,
			},
		}, env)
		if err != nil {
			return Error(err)
		}

		u := userDefined{clauses: cs}
		return u.Call(&state.VM, args, k, env)
	}
}

// Call1 succeeds if closure with an additional argument succeeds.
func (state *State) Call1(closure, arg1 Term, k func(*Env) *Promise, env *Env) *Promise {
	return state.callN(closure, []Term{arg1}, k, env)
}

// Call2 succeeds if closure with 2 additional arguments succeeds.
func (state *State) Call2(closure, arg1, arg2 Term, k func(*Env) *Promise, env *Env) *Promise {
	return state.callN(closure, []Term{arg1, arg2}, k, env)
}

// Call3 succeeds if closure with 3 additional arguments succeeds.
func (state *State) Call3(closure, arg1, arg2, arg3 Term, k func(*Env) *Promise, env *Env) *Promise {
	return state.callN(closure, []Term{arg1, arg2, arg3}, k, env)
}

// Call4 succeeds if closure with 4 additional arguments succeeds.
func (state *State) Call4(closure, arg1, arg2, arg3, arg4 Term, k func(*Env) *Promise, env *Env) *Promise {
	return state.callN(closure, []Term{arg1, arg2, arg3, arg4}, k, env)
}

// Call5 succeeds if closure with 5 additional arguments succeeds.
func (state *State) Call5(closure, arg1, arg2, arg3, arg4, arg5 Term, k func(*Env) *Promise, env *Env) *Promise {
	return state.callN(closure, []Term{arg1, arg2, arg3, arg4, arg5}, k, env)
}

// Call6 succeeds if closure with 6 additional arguments succeeds.
func (state *State) Call6(closure, arg1, arg2, arg3, arg4, arg5, arg6 Term, k func(*Env) *Promise, env *Env) *Promise {
	return state.callN(closure, []Term{arg1, arg2, arg3, arg4, arg5, arg6}, k, env)
}

// Call7 succeeds if closure with 7 additional arguments succeeds.
func (state *State) Call7(closure, arg1, arg2, arg3, arg4, arg5, arg6, arg7 Term, k func(*Env) *Promise, env *Env) *Promise {
	return state.callN(closure, []Term{arg1, arg2, arg3, arg4, arg5, arg6, arg7}, k, env)
}

func (state *State) callN(closure Term, additional []Term, k func(*Env) *Promise, env *Env) *Promise {
	pi, arg, err := PI(closure, env)
	if err != nil {
		return Error(err)
	}
	args := make([]Term, pi.Arity, int(pi.Arity)+len(additional))
	for i := 0; i < int(pi.Arity); i++ {
		args[i] = arg(i)
	}
	args = append(args, additional...)
	return state.Call(pi.Name.Apply(args...), k, env)
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

// Unify unifies x and y without occurs check (i.e., X = f(X) is allowed).
func Unify(x, y Term, k func(*Env) *Promise, env *Env) *Promise {
	env, ok := env.Unify(x, y, false)
	if !ok {
		return Bool(false)
	}
	return k(env)
}

// UnifyWithOccursCheck unifies x and y with occurs check (i.e., X = f(X) is not allowed).
func UnifyWithOccursCheck(x, y Term, k func(*Env) *Promise, env *Env) *Promise {
	env, ok := env.Unify(x, y, true)
	if !ok {
		return Bool(false)
	}
	return k(env)
}

// SubsumesTerm succeeds if general and specific are unifiable without binding variables in specific.
func SubsumesTerm(general, specific Term, k func(*Env) *Promise, env *Env) *Promise {
	theta, ok := env.Unify(general, specific, true)
	if !ok {
		return Bool(false)
	}

	if d := env.Compare(theta.Simplify(general), specific); d != OrderEqual {
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
	if _, ok := env.Resolve(t).(Compound); !ok {
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
func Functor(t, name, arity Term, k func(*Env) *Promise, env *Env) *Promise {
	switch t := env.Resolve(t).(type) {
	case Variable:
		switch arity := env.Resolve(arity).(type) {
		case Variable:
			return Error(InstantiationError(env))
		case Integer:
			if arity < 0 {
				return Error(DomainError(ValidDomainNotLessThanZero, arity, env))
			}

			name := env.Resolve(name)

			switch name := name.(type) {
			case Variable:
				return Error(InstantiationError(env))
			case Compound:
				return Error(TypeError(ValidTypeAtomic, name, env))
			}

			if arity == 0 {
				return Unify(t, name, k, env)
			}

			n, ok := name.(Atom)
			if !ok {
				return Error(TypeError(ValidTypeAtom, name, env))
			}

			vs := make([]Term, arity)
			for i := range vs {
				vs[i] = NewVariable()
			}
			return Unify(t, &compound{
				functor: n,
				args:    vs,
			}, k, env)
		default:
			return Error(TypeError(ValidTypeInteger, arity, env))
		}
	case Compound:
		pattern := compound{args: []Term{name, arity}}
		return Unify(&pattern, &compound{args: []Term{t.Functor(), Integer(t.Arity())}}, k, env)
	default: // atomic
		pattern := compound{args: []Term{name, arity}}
		return Unify(&pattern, &compound{args: []Term{t, Integer(0)}}, k, env)
	}
}

// Arg extracts nth argument of term as arg, or finds the argument position of arg in term as nth.
func Arg(nth, t, arg Term, k func(*Env) *Promise, env *Env) *Promise {
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
				return Error(DomainError(ValidDomainNotLessThanZero, n, env))
			}
			return Unify(arg, c.Arg(int(n)-1), k, env)
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
		elems, err := Slice(list, env)
		if err != nil {
			return Error(err)
		}
		switch len(elems) {
		case 0:
			return Error(DomainError(ValidDomainNonEmptyList, list, env))
		case 1:
			switch e := env.Resolve(elems[0]).(type) {
			case Variable:
				return Error(InstantiationError(env))
			case Compound:
				return Error(TypeError(ValidTypeAtomic, e, env))
			default:
				return k(env.Bind(t, e))
			}
		default:
			switch e := env.Resolve(elems[0]).(type) {
			case Variable:
				return Error(InstantiationError(env))
			case Atom:
				return k(env.Bind(t, e.Apply(elems[1:]...)))
			default:
				return Error(TypeError(ValidTypeAtom, e, env))
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
		return Unify(list, List(elems...), k, env)
	default:
		iter := ListIterator{List: list, Env: env, AllowPartial: true}
		for iter.Next() {
		}
		if err := iter.Err(); err != nil {
			return Error(err)
		}
		return Unify(list, List(t), k, env)
	}
}

// CopyTerm clones in as out.
func CopyTerm(in, out Term, k func(*Env) *Promise, env *Env) *Promise {
	return Unify(renamedCopy(in, nil, env), out, k, env)
}

func renamedCopy(t Term, copied map[TermID]Term, env *Env) Term {
	if copied == nil {
		copied = map[TermID]Term{}
	}
	t = env.Resolve(t)
	if c, ok := copied[ID(t)]; ok {
		return c
	}
	switch t := t.(type) {
	case Variable:
		v := NewVariable()
		copied[t] = v
		return v
	case Compound:
		c := compound{
			functor: t.Functor(),
			args:    make([]Term, t.Arity()),
		}
		copied[ID(t)] = &c
		for i := 0; i < t.Arity(); i++ {
			c.args[i] = renamedCopy(t.Arg(i), copied, env)
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

	return Unify(vars, List(ret...), k, env)
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
func (state *State) Op(priority, specifier, op Term, k func(*Env) *Promise, env *Env) *Promise {
	var p Integer
	switch priority := env.Resolve(priority).(type) {
	case Variable:
		return Error(InstantiationError(env))
	case Integer:
		if priority < 0 || priority > 1200 {
			return Error(DomainError(ValidDomainOperatorPriority, priority, env))
		}
		p = priority
	default:
		return Error(TypeError(ValidTypeInteger, priority, env))
	}

	var spec operatorSpecifier
	switch specifier := env.Resolve(specifier).(type) {
	case Variable:
		return Error(InstantiationError(env))
	case Atom:
		var ok bool
		spec, ok = operatorSpecifiers[specifier]
		if !ok {
			return Error(DomainError(ValidDomainOperatorSpecifier, specifier, env))
		}
	default:
		return Error(TypeError(ValidTypeAtom, specifier, env))
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
				return Error(TypeError(ValidTypeAtom, op, env))
			}
		}
		if err := iter.Err(); err != nil {
			return Error(err)
		}
	}

	for _, name := range names {
		if p := state.validateOp(p, spec, name, env); p != nil {
			return p
		}
	}

	for _, name := range names {
		if class := spec.class(); state.operators.definedInClass(name, spec.class()) {
			state.operators.remove(name, class)
		}

		state.operators.define(p, spec, name)
	}

	return k(env)
}

func (state *State) validateOp(p Integer, spec operatorSpecifier, name Atom, env *Env) *Promise {
	switch name {
	case atomComma:
		if state.operators.definedInClass(name, operatorClassInfix) {
			return Error(PermissionError(OperationModify, PermissionTypeOperator, name, env))
		}
	case atomBar:
		if spec.class() != operatorClassInfix || (p > 0 && p < 1001) {
			op := OperationCreate
			if state.operators.definedInClass(name, operatorClassInfix) {
				op = OperationModify
			}
			return Error(PermissionError(op, PermissionTypeOperator, name, env))
		}
	case atomEmptyBlock, atomEmptyList:
		return Error(PermissionError(OperationCreate, PermissionTypeOperator, name, env))
	}

	// 6.3.4.3 There shall not be an infix and a postfix Operator with the same name.
	switch spec.class() {
	case operatorClassInfix:
		if state.operators.definedInClass(name, operatorClassPostfix) {
			return Error(PermissionError(OperationCreate, PermissionTypeOperator, name, env))
		}
	case operatorClassPostfix:
		if state.operators.definedInClass(name, operatorClassInfix) {
			return Error(PermissionError(OperationCreate, PermissionTypeOperator, name, env))
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
func (state *State) CurrentOp(priority, specifier, op Term, k func(*Env) *Promise, env *Env) *Promise {
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
			atomXF:  {},
			atomYF:  {},
			atomXFX: {},
			atomXFY: {},
			atomYFX: {},
			atomFX:  {},
			atomFY:  {},
		}[s]; !ok {
			return Error(DomainError(ValidDomainOperatorSpecifier, s, env))
		}
	default:
		return Error(DomainError(ValidDomainOperatorSpecifier, s, env))
	}

	switch env.Resolve(op).(type) {
	case Variable, Atom:
		break
	default:
		return Error(TypeError(ValidTypeAtom, op, env))
	}

	pattern := compound{args: []Term{priority, specifier, op}}
	ks := make([]func(context.Context) *Promise, 0, len(state.operators)*int(_operatorClassLen))
	for _, ops := range state.operators {
		for _, op := range ops {
			op := op
			if op == (operator{}) {
				continue
			}
			ks = append(ks, func(context.Context) *Promise {
				return Unify(&pattern, &compound{args: []Term{op.priority, op.specifier.term(), op.name}}, k, env)
			})
		}
	}
	return Delay(ks...)
}

// Assertz appends t to the database.
func (state *State) Assertz(t Term, k func(*Env) *Promise, env *Env) *Promise {
	if err := state.assert(t, func(existing, new []clause) []clause {
		return append(existing, new...)
	}, env); err != nil {
		return Error(err)
	}
	return k(env)
}

// Asserta prepends t to the database.
func (state *State) Asserta(t Term, k func(*Env) *Promise, env *Env) *Promise {
	if err := state.assert(t, func(existing, new []clause) []clause {
		return append(new, existing...)
	}, env); err != nil {
		return Error(err)
	}
	return k(env)
}

func (state *State) assert(t Term, merge func([]clause, []clause) []clause, env *Env) error {
	pi, arg, err := PI(t, env)
	if err != nil {
		return err
	}

	if pi == (ProcedureIndicator{Name: atomIf, Arity: 2}) {
		pi, _, err = PI(arg(0), env)
		if err != nil {
			return err
		}
	}

	if state.procedures == nil {
		state.procedures = map[ProcedureIndicator]procedure{}
	}
	p, ok := state.procedures[pi]
	if !ok {
		p = &userDefined{dynamic: true}
		state.procedures[pi] = p
	}

	added, err := compile(t, env)
	if err != nil {
		return err
	}

	u, ok := p.(*userDefined)
	if !ok || !u.dynamic {
		return PermissionError(OperationModify, PermissionTypeStaticProcedure, pi.Term(), env)
	}

	u.clauses = merge(u.clauses, added)
	return nil
}

// BagOf collects all the solutions of goal as instances, which unify with template. instances may contain duplications.
func (state *State) BagOf(template, goal, instances Term, k func(*Env) *Promise, env *Env) *Promise {
	return state.collectionOf(func(tList []Term, env *Env) Term {
		return List(tList...)
	}, template, goal, instances, k, env)
}

// SetOf collects all the solutions of goal as instances, which unify with template. instances don't contain duplications.
func (state *State) SetOf(template, goal, instances Term, k func(*Env) *Promise, env *Env) *Promise {
	return state.collectionOf(func(tList []Term, env *Env) Term {
		return env.Set(tList...)
	}, template, goal, instances, k, env)
}

func (state *State) collectionOf(agg func([]Term, *Env) Term, template, goal, instances Term, k func(*Env) *Promise, env *Env) *Promise {
	fvs := newFreeVariablesSet(goal, template, env)
	w := make([]Term, 0, len(fvs))
	for v := range fvs {
		w = append(w, v)
	}
	sort.Slice(w, func(i, j int) bool {
		return w[i].(Variable) < w[j].(Variable)
	})
	witness := NewAtom("$witness").Apply(w...)
	g := iteratedGoalTerm(goal, env)
	s := Term(NewVariable())

	iter := ListIterator{List: instances, Env: env, AllowPartial: true}
	for iter.Next() {
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}

	return state.FindAll(atomPlus.Apply(witness, template), g, s, func(env *Env) *Promise {
		s, _ := Slice(s, env)
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
				return Unify(agg(tList, env), instances, k, env)
			})
		}
		return Delay(ks...)
	}, env)
}

// FindAll collects all the solutions of goal as instances, which unify with template. instances may contain duplications.
func (state *State) FindAll(template, goal, instances Term, k func(*Env) *Promise, env *Env) *Promise {
	iter := ListIterator{List: instances, Env: env, AllowPartial: true}
	for iter.Next() {
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}
	return Delay(func(ctx context.Context) *Promise {
		var answers []Term
		if _, err := state.Call(goal, func(env *Env) *Promise {
			answers = append(answers, renamedCopy(template, nil, env))
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
		case atomLessThan, atomEqual, atomGreaterThan:
			break
		default:
			return Error(DomainError(ValidDomainOrder, order, env))
		}
	default:
		return Error(TypeError(ValidTypeAtom, order, env))
	}

	o := env.Compare(term1, term2)
	return Unify(o.Term(), order, k, env)
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

	iter = ListIterator{List: sorted, Env: env, AllowPartial: true}
	for iter.Next() {
	}
	if err := iter.Err(); err != nil {
		return Error(err)
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
		case Compound:
			if e.Functor() != atomMinus || e.Arity() != 2 {
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
			case Compound:
				if e.Functor() != atomMinus || e.Arity() != 2 {
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
		return env.Compare(elems[i].(Compound).Arg(0), elems[j].(Compound).Arg(0)) == OrderLess
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

		env, ok := env.Unify(catcher, e.term, false)
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
	case Compound:
		if pi.Functor() != atomSlash || pi.Arity() != 2 {
			return Error(TypeError(ValidTypePredicateIndicator, pi, env))
		}
		if _, ok := env.Resolve(pi.Arg(0)).(Atom); !ok {
			return Error(TypeError(ValidTypePredicateIndicator, pi, env))
		}
		if _, ok := env.Resolve(pi.Arg(1)).(Integer); !ok {
			return Error(TypeError(ValidTypePredicateIndicator, pi, env))
		}
	default:
		return Error(TypeError(ValidTypePredicateIndicator, pi, env))
	}

	ks := make([]func(context.Context) *Promise, 0, len(state.procedures))
	for key, p := range state.procedures {
		switch p.(type) {
		case *userDefined:
			c := key.Term()
			ks = append(ks, func(context.Context) *Promise {
				return Unify(pi, c, k, env)
			})
		default:
			continue
		}
	}
	return Delay(ks...)
}

// Retract removes the first clause that matches with t.
func (state *State) Retract(t Term, k func(*Env) *Promise, env *Env) *Promise {
	t = Rulify(t, env)

	h := t.(Compound).Arg(0)
	pi, _, err := PI(h, env)
	if err != nil {
		return Error(err)
	}

	p, ok := state.procedures[pi]
	if !ok {
		return Bool(false)
	}

	u, ok := p.(*userDefined)
	if !ok || !u.dynamic {
		return Error(PermissionError(OperationModify, PermissionTypeStaticProcedure, pi.Term(), env))
	}

	deleted := 0
	ks := make([]func(context.Context) *Promise, len(u.clauses))
	for i, c := range u.clauses {
		i := i
		raw := Rulify(c.raw, env)
		ks[i] = func(_ context.Context) *Promise {
			return Unify(t, raw, func(env *Env) *Promise {
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
func (state *State) Abolish(pi Term, k func(*Env) *Promise, env *Env) *Promise {
	switch pi := env.Resolve(pi).(type) {
	case Variable:
		return Error(InstantiationError(env))
	case Compound:
		if pi.Functor() != atomSlash || pi.Arity() != 2 {
			return Error(TypeError(ValidTypePredicateIndicator, pi, env))
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
					return Error(DomainError(ValidDomainNotLessThanZero, arity, env))
				}
				key := ProcedureIndicator{Name: name, Arity: arity}
				if u, ok := state.procedures[key].(*userDefined); !ok || !u.dynamic {
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
	s, err := state.Stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	if s.mode != ioModeRead {
		return Error(PermissionError(OperationInput, PermissionTypeStream, streamOrAlias, env))
	}

	state.input = s
	return k(env)
}

// SetOutput sets streamOrAlias as the current output stream.
func (state *State) SetOutput(streamOrAlias Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := state.Stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	if s.mode != ioModeWrite && s.mode != ioModeAppend {
		return Error(PermissionError(OperationOutput, PermissionTypeStream, streamOrAlias, env))
	}

	state.output = s
	return k(env)
}

var openFile = os.OpenFile

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
			return Error(DomainError(ValidDomainIOMode, m, env))
		}
	default:
		return Error(TypeError(ValidTypeAtom, mode, env))
	}

	if _, ok := env.Resolve(stream).(Variable); !ok {
		return Error(InstantiationError(env))
	}

	s := Stream{mode: streamMode}
	switch f, err := openFile(n.String(), int(s.mode), 0644); {
	case err == nil:
		s.sourceSink = f
		if fi, err := f.Stat(); err == nil {
			s.reposition = fi.Mode()&fs.ModeType == 0
		}
	case os.IsNotExist(err):
		return Error(ExistenceError(ObjectTypeSourceSink, n, env))
	case os.IsPermission(err):
		return Error(PermissionError(OperationOpen, PermissionTypeSourceSink, n, env))
	default:
		return Error(SystemError(err))
	}

	iter := ListIterator{List: options, Env: env}
	for iter.Next() {
		if err := state.handleStreamOption(&s, iter.Current(), env); err != nil {
			return Error(err)
		}
	}
	if err := iter.Err(); err != nil {
		return Error(err)
	}

	return Unify(stream, &s, k, env)
}

func (state *State) handleStreamOption(s *Stream, option Term, env *Env) error {
	switch o := env.Resolve(option).(type) {
	case Variable:
		return InstantiationError(env)
	case Compound:
		if o.Arity() != 1 {
			break
		}

		switch o.Functor() {
		case atomAlias:
			if err := state.handleStreamOptionAlias(s, o, env); err != nil {
				return err
			}
			return nil
		case atomType:
			if err := state.handleStreamOptionType(s, o, env); err != nil {
				return err
			}
			return nil
		case atomReposition:
			if err := state.handleStreamOptionReposition(s, o, env); err != nil {
				return err
			}
			return nil
		case atomEOFAction:
			if err := state.handleStreamOptionEOFAction(s, o, env); err != nil {
				return err
			}
			return nil
		}
	}
	return DomainError(ValidDomainStreamOption, option, env)
}

func (state *State) handleStreamOptionAlias(s *Stream, o Compound, env *Env) error {
	switch a := env.Resolve(o.Arg(0)).(type) {
	case Variable:
		return InstantiationError(env)
	case Atom:
		if _, ok := state.streams[a]; ok {
			return PermissionError(OperationOpen, PermissionTypeSourceSink, o, env)
		}
		if state.streams == nil {
			state.streams = map[Term]*Stream{}
		}
		state.streams[a] = s
		return nil
	default:
		return DomainError(ValidDomainStreamOption, o, env)
	}
}

func (state *State) handleStreamOptionType(s *Stream, o Compound, env *Env) error {
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
	return DomainError(ValidDomainStreamOption, o, env)
}

func (state *State) handleStreamOptionReposition(s *Stream, o Compound, env *Env) error {
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
	return DomainError(ValidDomainStreamOption, o, env)
}

func (state *State) handleStreamOptionEOFAction(s *Stream, o Compound, env *Env) error {
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
	return DomainError(ValidDomainStreamOption, o, env)
}

// Close closes a stream specified by streamOrAlias.
func (state *State) Close(streamOrAlias, options Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := state.Stream(streamOrAlias, env)
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
					return Error(DomainError(ValidDomainStreamOption, option, env))
				}

				switch v := env.Resolve(option.Arg(0)).(type) {
				case Atom:
					switch v {
					case atomFalse:
						force = false
					case atomTrue:
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

	if s.alias == 0 {
		delete(state.streams, s)
	} else {
		delete(state.streams, s.alias)
	}

	return k(env)
}

// FlushOutput sends any buffered output to the stream.
func (state *State) FlushOutput(streamOrAlias Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := state.Stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	if s.mode != ioModeWrite && s.mode != ioModeAppend {
		return Error(PermissionError(OperationOutput, PermissionTypeStream, streamOrAlias, env))
	}

	if err := s.Flush(); err != nil {
		return Error(err)
	}

	return k(env)
}

// WriteTerm outputs term to stream with options.
func (state *State) WriteTerm(streamOrAlias, t, options Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := state.Stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	if s.mode != ioModeWrite && s.mode != ioModeAppend {
		return Error(PermissionError(OperationOutput, PermissionTypeStream, streamOrAlias, env))
	}

	if s.streamType == streamTypeBinary {
		return Error(PermissionError(OperationOutput, PermissionTypeBinaryStream, streamOrAlias, env))
	}

	// The character sequence for a variable begins with `_` (7.10.5.a).
	for v := range newVariableSet(t, env) {
		if !v.Anonymous() {
			env = env.Bind(v, NewVariable())
		}
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

	if err := state.Write(s, env.Resolve(t), &opts, env); err != nil {
		return Error(err)
	}

	return k(env)
}

// Write outputs term to the writer.
func (state *State) Write(w io.Writer, t Term, opts *WriteOptions, env *Env) error {
	opts.ops = state.operators
	opts.priority = 1200
	return WriteTerm(w, t, opts, env)
}

func writeTermOption(opts *WriteOptions, option Term, env *Env) error {
	switch o := env.Resolve(option).(type) {
	case Variable:
		return InstantiationError(env)
	case Compound:
		if o.Arity() != 1 {
			return DomainError(ValidDomainWriteOption, o, env)
		}

		if o.Functor() == atomVariableNames {
			vns, err := variableNames(o, env)
			if err != nil {
				return err
			}
			opts.VariableNames = vns
			return nil
		}

		var b bool
		switch v := env.Resolve(o.Arg(0)).(type) {
		case Variable:
			return InstantiationError(env)
		case Atom:
			switch v {
			case atomTrue:
				b = true
			case atomFalse:
				b = false
			default:
				return DomainError(ValidDomainWriteOption, o, env)
			}
		default:
			return DomainError(ValidDomainWriteOption, o, env)
		}

		switch o.Functor() {
		case atomQuoted:
			opts.Quoted = b
			return nil
		case atomIgnoreOps:
			opts.IgnoreOps = b
			return nil
		case atomNumberVars:
			opts.NumberVars = b
			return nil
		default:
			return DomainError(ValidDomainWriteOption, o, env)
		}
	default:
		return DomainError(ValidDomainWriteOption, o, env)
	}
}

func variableNames(option Compound, env *Env) (map[Variable]Atom, error) {
	vns := map[Variable]Atom{}
	iter := ListIterator{List: option.Arg(0), Env: env}
	for iter.Next() {
		var vn Compound
		switch elem := env.Resolve(iter.Current()).(type) {
		case Variable:
			return nil, InstantiationError(env)
		case Compound:
			if elem.Functor() != atomEqual || elem.Arity() != 2 {
				return nil, DomainError(ValidDomainWriteOption, option, env)
			}
			vn = elem
		default:
			return nil, DomainError(ValidDomainWriteOption, option, env)
		}

		var n Atom
		switch arg := env.Resolve(vn.Arg(0)).(type) {
		case Variable:
			return nil, InstantiationError(env)
		case Atom:
			n = arg
		default:
			return nil, DomainError(ValidDomainWriteOption, option, env)
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
				return Unify(ch, NewAtom(string(r)), k, env)
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

		rs := []rune(ch.String())
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

// PutByte outputs an integer byte to a stream represented by streamOrAlias.
func (state *State) PutByte(streamOrAlias, byt Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := state.Stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	if s.mode != ioModeWrite && s.mode != ioModeAppend {
		return Error(PermissionError(OperationOutput, PermissionTypeStream, streamOrAlias, env))
	}

	if s.streamType == streamTypeText {
		return Error(PermissionError(OperationOutput, PermissionTypeTextStream, streamOrAlias, env))
	}

	switch b := env.Resolve(byt).(type) {
	case Variable:
		return Error(InstantiationError(env))
	case Integer:
		if 0 > b || 255 < b {
			return Error(TypeError(ValidTypeByte, byt, env))
		}

		if _, err := s.Write([]byte{byte(b)}); err != nil {
			return Error(SystemError(err))
		}

		return k(env)
	default:
		return Error(TypeError(ValidTypeByte, byt, env))
	}
}

// PutCode outputs code to the stream represented by streamOrAlias.
func (state *State) PutCode(streamOrAlias, code Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := state.Stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	if s.mode != ioModeWrite && s.mode != ioModeAppend {
		return Error(PermissionError(OperationOutput, PermissionTypeStream, streamOrAlias, env))
	}

	if s.streamType == streamTypeBinary {
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

		if _, err := s.Write([]byte(string(r))); err != nil {
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
	s, err := state.Stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	if s.mode != ioModeRead {
		return Error(PermissionError(OperationInput, PermissionTypeStream, streamOrAlias, env))
	}

	if s.streamType == streamTypeBinary {
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
	p := state.Parser(s, &vars)
	defer func() {
		_ = s.UnreadRune()
	}()

	t, err := p.Term()
	if err != nil {
		switch {
		case errors.Is(err, io.EOF):
			switch s.eofAction {
			case eofActionError:
				return Error(PermissionError(OperationInput, PermissionTypePastEndOfStream, streamOrAlias, env))
			case eofActionReset:
				return Delay(func(context.Context) *Promise {
					return state.ReadTerm(streamOrAlias, out, options, k, env)
				})
			default:
				return Unify(out, atomEndOfFile, k, env)
			}
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
		variableNames = append(variableNames, &compound{
			functor: atomEqual,
			args:    []Term{v.Name, v.Variable},
		})
	}

	env, ok := env.Unify(&compound{args: []Term{
		opts.singletons,
		opts.variables,
		opts.variableNames,
	}}, &compound{args: []Term{
		List(singletons...),
		List(variables...),
		List(variableNames...),
	}}, false)
	if !ok {
		return Bool(false)
	}

	return Unify(out, t, k, env)
}

func readTermOption(opts *readTermOptions, option Term, env *Env) error {
	switch option := env.Resolve(option).(type) {
	case Variable:
		return InstantiationError(env)
	case Compound:
		if option.Arity() != 1 {
			return DomainError(ValidDomainReadOption, option, env)
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
			return DomainError(ValidDomainReadOption, option, env)
		}
		return nil
	default:
		return DomainError(ValidDomainReadOption, option, env)
	}
}

// GetByte reads a byte from the stream represented by streamOrAlias and unifies it with inByte.
func (state *State) GetByte(streamOrAlias, inByte Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := state.Stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	if s.mode != ioModeRead {
		return Error(PermissionError(OperationInput, PermissionTypeStream, streamOrAlias, env))
	}

	if s.streamType == streamTypeText {
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

	switch b, err := s.ReadByte(); err {
	case nil:
		return Delay(func(context.Context) *Promise {
			return Unify(inByte, Integer(b), k, env)
		})
	case io.EOF:
		switch s.eofAction {
		case eofActionError:
			return Error(PermissionError(OperationInput, PermissionTypePastEndOfStream, streamOrAlias, env))
		case eofActionReset:
			return Delay(func(context.Context) *Promise {
				return state.GetByte(streamOrAlias, inByte, k, env)
			})
		default:
			return Unify(inByte, Integer(-1), k, env)
		}
	default:
		return Error(err)
	}
}

// GetChar reads a character from the stream represented by streamOrAlias and unifies it with char.
func (state *State) GetChar(streamOrAlias, char Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := state.Stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	if s.mode != ioModeRead {
		return Error(PermissionError(OperationInput, PermissionTypeStream, streamOrAlias, env))
	}

	if s.streamType == streamTypeBinary {
		return Error(PermissionError(OperationInput, PermissionTypeBinaryStream, streamOrAlias, env))
	}

	switch c := env.Resolve(char).(type) {
	case Variable:
		break
	case Atom:
		if len([]rune(c.String())) != 1 {
			return Error(TypeError(ValidTypeInCharacter, char, env))
		}
	default:
		return Error(TypeError(ValidTypeInCharacter, char, env))
	}

	switch r, _, err := s.ReadRune(); err {
	case nil:
		if r == unicode.ReplacementChar {
			return Error(RepresentationError(FlagCharacter, env))
		}

		return Delay(func(context.Context) *Promise {
			return Unify(char, NewAtom(string(r)), k, env)
		})
	case io.EOF:
		switch s.eofAction {
		case eofActionError:
			return Error(PermissionError(OperationInput, PermissionTypePastEndOfStream, streamOrAlias, env))
		case eofActionReset:
			return Delay(func(context.Context) *Promise {
				return state.GetChar(streamOrAlias, char, k, env)
			})
		default:
			return Unify(char, atomEndOfFile, k, env)
		}
	default:
		return Error(SystemError(err))
	}
}

// PeekByte peeks a byte from the stream represented by streamOrAlias and unifies it with inByte.
func (state *State) PeekByte(streamOrAlias, inByte Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := state.Stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	if s.mode != ioModeRead {
		return Error(PermissionError(OperationInput, PermissionTypeStream, streamOrAlias, env))
	}

	if s.streamType == streamTypeText {
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

	switch b, err := s.Peek(1); err {
	case nil:
		return Delay(func(context.Context) *Promise {
			return Unify(inByte, Integer(b[0]), k, env)
		})
	case io.EOF:
		switch s.eofAction {
		case eofActionError:
			return Error(PermissionError(OperationInput, PermissionTypePastEndOfStream, streamOrAlias, env))
		case eofActionReset:
			return Delay(func(context.Context) *Promise {
				return state.PeekByte(streamOrAlias, inByte, k, env)
			})
		default:
			return Unify(inByte, Integer(-1), k, env)
		}
	default:
		return Error(SystemError(err))
	}
}

// PeekChar peeks a rune from the stream represented by streamOrAlias and unifies it with char.
func (state *State) PeekChar(streamOrAlias, char Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := state.Stream(streamOrAlias, env)
	if err != nil {
		return Error(err)
	}

	if s.mode != ioModeRead {
		return Error(PermissionError(OperationInput, PermissionTypeStream, streamOrAlias, env))
	}

	if s.streamType == streamTypeBinary {
		return Error(PermissionError(OperationInput, PermissionTypeBinaryStream, streamOrAlias, env))
	}

	switch c := env.Resolve(char).(type) {
	case Variable:
		break
	case Atom:
		if len([]rune(c.String())) != 1 {
			return Error(TypeError(ValidTypeInCharacter, char, env))
		}
	default:
		return Error(TypeError(ValidTypeInCharacter, char, env))
	}

	switch r, _, err := s.ReadRune(); err {
	case nil:
		_ = s.UnreadRune()

		if r == unicode.ReplacementChar {
			return Error(RepresentationError(FlagCharacter, env))
		}

		return Delay(func(context.Context) *Promise {
			return Unify(char, NewAtom(string(r)), k, env)
		})
	case io.EOF:
		switch s.eofAction {
		case eofActionError:
			return Error(PermissionError(OperationInput, PermissionTypePastEndOfStream, streamOrAlias, env))
		case eofActionReset:
			return Delay(func(context.Context) *Promise {
				return state.PeekChar(streamOrAlias, char, k, env)
			})
		default:
			return Unify(char, atomEndOfFile, k, env)
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
	pi, _, err := PI(head, env)
	if err != nil {
		return Error(err)
	}

	switch env.Resolve(body).(type) {
	case Variable, Atom, Compound:
		break
	default:
		return Error(TypeError(ValidTypeCallable, body, env))
	}

	p, ok := state.procedures[pi]
	if !ok {
		return Bool(false)
	}

	u, ok := p.(*userDefined)
	if !ok || !u.public {
		return Error(PermissionError(OperationAccess, PermissionTypePrivateProcedure, pi.Term(), env))
	}

	ks := make([]func(context.Context) *Promise, len(u.clauses))
	for i, c := range u.clauses {
		r := Rulify(renamedCopy(c.raw, nil, env), env)
		ks[i] = func(context.Context) *Promise {
			return Unify(&compound{
				functor: atomIf,
				args:    []Term{head, body},
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
			return Unify(length, Integer(len([]rune(a.String()))), k, env)
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
					return Unify(a3, NewAtom(a1.String()+a2.String()), k, env)
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

		pattern := compound{args: []Term{atom1, atom2}}
		s := a3.String()
		ks := make([]func(context.Context) *Promise, 0, len(s)+1)
		for i := range s {
			a1, a2 := s[:i], s[i:]
			ks = append(ks, func(context.Context) *Promise {
				return Unify(&pattern, &compound{args: []Term{NewAtom(a1), NewAtom(a2)}}, k, env)
			})
		}
		ks = append(ks, func(context.Context) *Promise {
			return Unify(&pattern, &compound{args: []Term{a3, atomEmpty}}, k, env)
		})
		return Delay(ks...)
	default:
		return Error(TypeError(ValidTypeAtom, atom3, env))
	}
}

// SubAtom unifies subAtom with a sub atom of length which appears with before runes preceding it and after runes following it.
func SubAtom(atom, before, length, after, subAtom Term, k func(*Env) *Promise, env *Env) *Promise {
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
			return Error(TypeError(ValidTypeAtom, subAtom, env))
		}

		subAtomPattern := NewAtom("$sub_atom_pattern")
		pattern := subAtomPattern.Apply(before, length, after, subAtom)
		var ks []func(context.Context) *Promise
		for i := 0; i <= len(rs); i++ {
			for j := i; j <= len(rs); j++ {
				before, length, after, subAtom := Integer(i), Integer(j-i), Integer(len(rs)-j), NewAtom(string(rs[i:j]))
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
				if len([]rune(e.String())) != 1 {
					return Error(TypeError(ValidTypeCharacter, e, env))
				}
				_, _ = sb.WriteString(e.String())
			default:
				return Error(TypeError(ValidTypeCharacter, e, env))
			}
		}
		if err := iter.Err(); err != nil {
			return Error(err)
		}
		return Unify(atom, NewAtom(sb.String()), k, env)
	case Atom:
		iter := ListIterator{List: chars, Env: env, AllowPartial: true}
		for iter.Next() {
			switch e := env.Resolve(iter.Current()).(type) {
			case Variable:
				break
			case Atom:
				if len([]rune(e.String())) != 1 {
					return Error(TypeError(ValidTypeCharacter, e, env))
				}
			default:
				return Error(TypeError(ValidTypeCharacter, e, env))
			}
		}
		if err := iter.Err(); err != nil {
			return Error(err)
		}

		s := a.String()
		if s == "" {
			return Unify(chars, atomEmptyList, k, env)
		}
		return Unify(chars, charList(s), k, env)
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
				if e < 0 || e > unicode.MaxRune {
					return Error(RepresentationError(FlagCharacterCode, env))
				}
				_, _ = sb.WriteRune(rune(e))
			default:
				return Error(TypeError(ValidTypeInteger, e, env))
			}
		}
		if err := iter.Err(); err != nil {
			return Error(err)
		}
		return Unify(atom, NewAtom(sb.String()), k, env)
	case Atom:
		iter := ListIterator{List: codes, Env: env, AllowPartial: true}
		for iter.Next() {
			switch e := env.Resolve(iter.Current()).(type) {
			case Variable:
				break
			case Integer:
				if e < 0 || e > unicode.MaxRune {
					return Error(RepresentationError(FlagCharacterCode, env))
				}
			default:
				return Error(TypeError(ValidTypeInteger, e, env))
			}
		}
		if err := iter.Err(); err != nil {
			return Error(err)
		}

		s := a.String()
		if s == "" {
			return Unify(codes, atomEmptyList, k, env)
		}
		return Unify(codes, codeList(s), k, env)
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
			s := e.String()
			if len([]rune(s)) != 1 {
				return Error(TypeError(ValidTypeCharacter, e, env))
			}
			_, _ = sb.WriteString(s)
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

	p := newParser(strings.NewReader(sb.String()))
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
				if len(e.String()) != 1 {
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
		_ = WriteTerm(&buf, n, &defaultWriteOptions, nil)
		rs := []rune(buf.String())

		cs := make([]Term, len(rs))
		for i, r := range rs {
			cs[i] = NewAtom(string(r))
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

		p := newParser(strings.NewReader(sb.String()))
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
		_ = WriteTerm(&buf, n, &defaultWriteOptions, nil)
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

// StreamProperty succeeds iff the stream represented by streamOrAlias has the stream property.
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
		for _, p := range s.properties() {
			p := p
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
		case atomInput, atomOutput:
			return nil
		default:
			return DomainError(ValidDomainStreamProperty, property, env)
		}
	case Compound:
		if p.Arity() != 1 {
			return DomainError(ValidDomainStreamProperty, property, env)
		}
		arg := p.Arg(0)
		switch p.Functor() {
		case atomFileName, atomMode, atomAlias, atomEndOfStream, atomEOFAction, atomReposition:
			return checkAtom(arg, env)
		case atomPosition:
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

// SetStreamPosition sets the position property of the stream represented by streamOrAlias.
func (state *State) SetStreamPosition(streamOrAlias, position Term, k func(*Env) *Promise, env *Env) *Promise {
	s, err := state.Stream(streamOrAlias, env)
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
		if _, err := s.Seek(int64(p), 0); err != nil {
			return Error(SystemError(err))
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
		i := []rune(in.String())
		if len(i) != 1 {
			return Error(RepresentationError(FlagCharacter, env))
		}

		switch out := env.Resolve(outChar).(type) {
		case Variable:
			return Error(InstantiationError(env))
		case Atom:
			o := []rune(out.String())
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
		i := []rune(in.String())
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
		o := []rune(out.String())
		if len(o) != 1 {
			return Error(RepresentationError(FlagCharacter, env))
		}
	default:
		return Error(RepresentationError(FlagCharacter, env))
	}

	if c1, ok := env.Resolve(inChar).(Atom); ok {
		r := []rune(c1.String())
		if r, ok := state.charConversions[r[0]]; ok {
			return Delay(func(context.Context) *Promise {
				return Unify(outChar, NewAtom(string(r)), k, env)
			})
		}
		return Delay(func(context.Context) *Promise {
			return Unify(outChar, c1, k, env)
		})
	}

	pattern := compound{args: []Term{inChar, outChar}}
	ks := make([]func(context.Context) *Promise, 256)
	for i := 0; i < 256; i++ {
		r := rune(i)
		cr, ok := state.charConversions[r]
		if !ok {
			cr = r
		}

		ks[i] = func(context.Context) *Promise {
			return Unify(&pattern, &compound{args: []Term{NewAtom(string(r)), NewAtom(string(cr))}}, k, env)
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
		case atomBounded, atomMaxInteger, atomMinInteger, atomIntegerRoundingFunction, atomMaxArity:
			return Error(PermissionError(OperationModify, PermissionTypeFlag, f, env))
		case atomCharConversion:
			modify = state.modifyCharConversion
		case atomDebug:
			modify = state.modifyDebug
		case atomUnknown:
			modify = state.modifyUnknown
		case atomDoubleQuotes:
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
			return Error(DomainError(ValidDomainFlagValue, &compound{
				functor: atomPlus,
				args:    []Term{flag, value},
			}, env))
		}
	default:
		return Error(TypeError(ValidTypeAtom, f, env))
	}
}

func (state *State) modifyCharConversion(value Atom) error {
	switch value {
	case atomOn:
		state.charConvEnabled = true
	case atomOff:
		state.charConvEnabled = false
	default:
		return DomainError(ValidDomainFlagValue, &compound{
			functor: atomPlus,
			args:    []Term{atomCharConversion, value},
		}, nil)
	}
	return nil
}

func (state *State) modifyDebug(value Atom) error {
	switch value {
	case atomOn:
		state.debug = true
	case atomOff:
		state.debug = false
	default:
		return DomainError(ValidDomainFlagValue, &compound{
			functor: atomPlus,
			args:    []Term{atomDebug, value},
		}, nil)
	}
	return nil
}

func (state *State) modifyUnknown(value Atom) error {
	switch value {
	case atomError:
		state.unknown = unknownError
	case atomWarning:
		state.unknown = unknownWarning
	case atomFail:
		state.unknown = unknownFail
	default:
		return DomainError(ValidDomainFlagValue, &compound{
			functor: atomPlus,
			args:    []Term{atomUnknown, value},
		}, nil)
	}
	return nil
}

func (state *State) modifyDoubleQuotes(value Atom) error {
	switch value {
	case atomCodes:
		state.doubleQuotes = doubleQuotesCodes
	case atomChars:
		state.doubleQuotes = doubleQuotesChars
	case atomAtom:
		state.doubleQuotes = doubleQuotesAtom
	default:
		return DomainError(ValidDomainFlagValue, &compound{
			functor: atomPlus,
			args:    []Term{atomDoubleQuotes, value},
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
		case atomBounded, atomMaxInteger, atomMinInteger, atomIntegerRoundingFunction, atomCharConversion, atomDebug, atomMaxArity, atomUnknown, atomDoubleQuotes:
			break
		default:
			return Error(DomainError(ValidDomainPrologFlag, f, env))
		}
	default:
		return Error(TypeError(ValidTypeAtom, f, env))
	}

	pattern := compound{args: []Term{flag, value}}
	flags := []Term{
		&compound{args: []Term{atomBounded, atomTrue}},
		&compound{args: []Term{atomMaxInteger, maxInt}},
		&compound{args: []Term{atomMinInteger, minInt}},
		&compound{args: []Term{atomIntegerRoundingFunction, atomTowardZero}},
		&compound{args: []Term{atomCharConversion, onOff(state.charConvEnabled)}},
		&compound{args: []Term{atomDebug, onOff(state.debug)}},
		&compound{args: []Term{atomMaxArity, atomUnbounded}},
		&compound{args: []Term{atomUnknown, NewAtom(state.unknown.String())}},
		&compound{args: []Term{atomDoubleQuotes, NewAtom(state.doubleQuotes.String())}},
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
		return atomOn
	}
	return atomOff
}

// Stream returns a stream represented by streamOrAlias.
func (state *State) Stream(streamOrAlias Term, env *Env) (*Stream, error) {
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

// ExpandTerm transforms term1 according to term_expansion/2 and DCG rules then unifies with term2.
func (state *State) ExpandTerm(term1, term2 Term, k func(*Env) *Promise, env *Env) *Promise {
	t, err := state.expand(term1, env)
	if err != nil {
		return Error(err)
	}

	return Unify(t, term2, k, env)
}

func (state *State) expand(term Term, env *Env) (Term, error) {
	termExpansion := atomTermExpansion

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
			return Unify(&compound{
				args: []Term{key, value},
			}, &compound{
				args: []Term{NewAtom(kv[0]), NewAtom(kv[1])},
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
		idx := NewAtom("$idx")
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

			return lengthAddendum(atomEmptyList, skipped, suffix, n, k, env)
		case Atom: // list or non-list terminated by an atom
			if suffix != atomEmptyList {
				return Bool(false)
			}

			return Unify(n, skipped, k, env)
		case Compound: // non-list including infinite list
			if suffix.Functor() != atomDot || suffix.Arity() != 2 {
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
	elems := make([]Term, n)
	for i := range elems {
		elems[i] = NewVariable()
	}
	return Unify(list, List(elems...), k, env)
}

func lengthAddendum(suffix Term, offset Integer, list, length Variable, k func(*Env) *Promise, env *Env) *Promise {
	return Delay(func(context.Context) *Promise {
		a := NewAtom("$addendum")
		return Unify(a.Apply(list, length), a.Apply(suffix, offset), k, env)
	}, func(context.Context) *Promise {
		suffix := atomDot.Apply(NewVariable(), suffix)
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

	s := NewAtom("$skipped")
	return Unify(s.Apply(skip, suffix), s.Apply(n, iter.Suffix()), k, env)
}

// Append succeeds iff zs is the concatenation of lists xs and ys.
func Append(xs, ys, zs Term, k func(*Env) *Promise, env *Env) *Promise {
	// A special case for non-empty lists without a variable in the spine.
	if xs, ok := env.Resolve(xs).(Compound); ok {
		iter := ListIterator{List: xs, Env: nil} // No variables allowed.
		for iter.Next() {
		}
		if err := iter.Err(); err == nil {
			return Unify(zs, partial{
				Compound: xs,
				tail:     ys,
			}, k, env)
		}
	}

	return appendLists(xs, ys, zs, k, env)
}

func appendLists(xs, ys, zs Term, k func(*Env) *Promise, env *Env) *Promise {
	/*
		append([], L, L).
		append([X|L1], L2, [X|L3]) :- append(L1, L2, L3).
	*/
	f := NewAtom("$append")
	return Delay(func(context.Context) *Promise {
		return Unify(f.Apply(xs, ys), f.Apply(List(), zs), k, env)
	}, func(context.Context) *Promise {
		x := NewVariable()
		l1, l3 := NewVariable(), NewVariable()
		return Unify(f.Apply(xs, zs), f.Apply(Cons(x, l1), Cons(x, l3)), func(env *Env) *Promise {
			return appendLists(l1, ys, l3, k, env)
		}, env)
	})
}
