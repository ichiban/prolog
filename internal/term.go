package internal

import (
	"errors"
	"fmt"
	"github.com/ichiban/prolog/internal/rbtree"
	"io"
	"iter"
	"maps"
	"strconv"
	"strings"
	"unicode/utf8"
	"unsafe"
)

var (
	errTooManyTerms    = errors.New("too many Terms")
	errTooManyAtoms    = errors.New("too many atoms")
	errTooManyIntegers = errors.New("too many integers")
	errTooManyFloats   = errors.New("too many floats")
	errTooManyFunctors = errors.New("too many functors")
	errTooManyStrings  = errors.New("too many strings")
)

type termTag int8

const (
	termTagVariable = iota + 1
	termTagCharacter
	termTagAtom
	termTagInteger
	termTagFloat
	termTagFunctor
	termTagList
	termTagString
	termTagReference
)

// Functor is a Name with an arity.
type Functor struct {
	Module Atom
	Name   Atom
	Arity  int
}

func (f Functor) String() string {
	if f.Module != 0 {
		return fmt.Sprintf("%s:%s/%d", f.Module, f.Name, f.Arity)
	}
	return fmt.Sprintf("%s/%d", f.Name, f.Arity)
}

// Term is a Prolog datum.
// It may refer to another Term in TermPool.
type Term struct {
	tag     termTag
	payload int32
}

// TermPool is a memory region to store Terms.
type TermPool struct {
	terms []Term

	integers []int64
	floats   []float64
	functors []Functor
	strings  []string

	env   Env
	atoms SymbolTable
}

func NewTermPool(bytes int) TermPool {
	var (
		termBytes    = int(unsafe.Sizeof(Term{}))
		atomBytes    = int(unsafe.Sizeof(rbtree.Map[atomID, string]{}) + unsafe.Sizeof(rbtree.Map[string, atomID]{}))
		integerBytes = int(unsafe.Sizeof(int64(0)))
		floatBytes   = int(unsafe.Sizeof(float64(0)))
		functorBytes = int(unsafe.Sizeof(Functor{}))
		stringBytes  = int(unsafe.Sizeof(""))
		bindingBytes = int(unsafe.Sizeof(rbtree.Node[Variable, Term]{}))
	)

	const (
		termRatio    = 5
		atomRatio    = 2
		integerRatio = 1
		floatRatio   = 1
		functorRatio = 2
		stringRatio  = 1
		bindingRatio = 5
	)

	unit := bytes / (termRatio + atomRatio + integerRatio + floatRatio + functorRatio + stringRatio + bindingRatio)

	var (
		MaxTerms    = (termRatio * unit) / termBytes
		MaxAtoms    = (atomRatio * unit) / atomBytes
		MaxIntegers = (integerRatio * unit) / integerBytes
		MaxFloats   = (floatRatio * unit) / floatBytes
		MaxFunctors = (functorRatio * unit) / functorBytes
		MaxStrings  = (stringRatio * unit) / stringBytes
		MaxBindings = (bindingRatio * unit) / bindingBytes
	)

	p := TermPool{
		terms: make([]Term, 0, MaxTerms),

		integers: make([]int64, 0, MaxIntegers),
		floats:   make([]float64, 0, MaxFloats),
		functors: make([]Functor, 0, MaxFunctors),
		strings:  make([]string, 0, MaxStrings),
	}

	p.env.bindings.Grow(MaxBindings)
	p.atoms.Grow(MaxAtoms)

	return p
}

func (t *TermPool) PutVariable() (Term, error) {
	v := t.env.lastVariable + 1
	return Term{tag: termTagVariable, payload: int32(v)}, nil
}

func (t *TermPool) Variable(term Term) bool {
	return term.tag == termTagVariable
}

func (t *TermPool) Resolve(term Term) Term {
	for {
		switch term.tag {
		case termTagVariable:
			v := Variable(term.payload)
			val, ok := t.env.Lookup(v)
			if !ok {
				return term
			}
			term = val
		case termTagReference:
			term = t.terms[term.payload]
		default:
			return term
		}
	}
}

func (t *TermPool) PutCharacter(char rune) (Term, error) {
	return Term{tag: termTagCharacter, payload: char}, nil
}

func (t *TermPool) Character(term Term) (rune, bool) {
	if term.tag != termTagCharacter {
		return utf8.RuneError, false
	}
	return term.payload, true
}

func (t *TermPool) PutAtom(name string) (Term, error) {
	atom, ok := t.atoms.Put(name)
	if !ok {
		return Term{}, errTooManyAtoms
	}
	return Term{tag: termTagAtom, payload: int32(atom)}, nil
}

func (t *TermPool) Atom(term Term) (string, bool) {
	if term.tag != termTagAtom {
		return "", false
	}
	return t.atoms.names.Get(atomID(term.payload))
}

func (t *TermPool) PutInteger(n int64) (Term, error) {
	var ok bool
	t.integers, ok = cappend(t.integers, n)
	if !ok {
		return Term{}, errTooManyIntegers
	}

	return Term{tag: termTagInteger, payload: int32(len(t.integers) - 1)}, nil
}

func (t *TermPool) Integer(term Term) (int64, bool) {
	if term.tag != termTagInteger {
		return 0, false
	}
	return t.integers[term.payload], true
}

func (t *TermPool) PutFloat(f float64) (Term, error) {
	var ok bool
	t.floats, ok = cappend(t.floats, f)
	if !ok {
		return Term{}, errTooManyFloats
	}

	return Term{tag: termTagFloat, payload: int32(len(t.floats) - 1)}, nil
}

func (t *TermPool) Float(term Term) (float64, bool) {
	if term.tag != termTagFloat {
		return 0, false
	}
	return t.floats[term.payload], true
}

func (t *TermPool) Atomic(term Term) bool {
	tag := term.tag
	return termTagAtom <= tag && tag <= termTagFloat
}

func (t *TermPool) PutCompound(name string, args ...Term) (Term, error) {
	if len(args) == 0 {
		return t.PutAtom(name)
	}

	var ok bool
	t.functors, ok = cappend(t.functors, Functor{Name: name, Arity: len(args)})
	if !ok {
		return 0, errTooManyFunctors
	}

	id, err := t.putTagPayload(termTagFunctor, int32(len(t.functors)-1))
	if err != nil {
		return 0, err
	}

	for _, arg := range args {
		if _, err := t.putTagPayload(termTagArgument, int32(arg)); err != nil {
			return 0, err
		}
	}

	return id, nil
}

func (t *TermPool) PutCompoundWithVarArgs(f Functor) (Term, error) {
	if f.Arity == 0 {
		return t.PutAtom(f.Name)
	}

	var ok bool
	t.functors, ok = cappend(t.functors, f)
	if !ok {
		return 0, errTooManyFunctors
	}

	id, err := t.putTagPayload(termTagFunctor, int32(len(t.functors)-1))
	if err != nil {
		return 0, err
	}

	for i := 0; i < f.Arity; i++ {
		if _, err := t.PutVariable(NewVariable(t)); err != nil {
			return 0, err
		}
	}

	return id, nil
}

func (t *TermPool) PutFunctor(f Functor) (Term, error) {
	n, err := t.PutAtom(f.Name)
	if err != nil {
		return 0, err
	}

	a, err := t.PutInteger(int64(f.Arity))
	if err != nil {
		return 0, err
	}

	return t.PutCompound(Atom('/'), n, a)
}

func (t *TermPool) PutList(args ...Term) (Term, error) {
	return t.putListLike(func() (Term, error) {
		return t.PutAtom(atomEmptyList)
	}, args...)
}

func (t *TermPool) PutPartialList(tail Term, args ...Term) (Term, error) {
	return t.putListLike(func() (Term, error) {
		id, err := t.PutVariable(NewVariable(t))
		if err != nil {
			return 0, err
		}

		if _, err := t.Unify(id, tail); err != nil {
			return 0, err
		}

		return id, nil
	}, args...)
}

func (t *TermPool) putListLike(tail func() (Term, error), args ...Term) (Term, error) {
	if len(args) == 0 {
		return tail()
	}

	id, err := t.putTagPayload(termTagList, int32(args[0]))
	if err != nil {
		return 0, err
	}

	for _, arg := range args[1:] {
		if _, err := t.putTagPayload(termTagList, int32(arg)); err != nil {
			return 0, err
		}
	}

	if _, err := tail(); err != nil {
		return 0, err
	}

	return id, nil
}

func (t *TermPool) PutSet(elems ...Term) (Term, error) {
	return t.PutList(elems...) // TODO:
}

func (t *TermPool) PutString(str string) (Term, error) {
	return t.putStringLike(func() (Term, error) {
		return t.PutAtom(atomEmptyList)
	}, str)
}

func (t *TermPool) PutPartialString(tail Term, str string) (Term, error) {
	return t.putStringLike(func() (Term, error) {
		id, err := t.PutVariable(NewVariable(t))
		if err != nil {
			return 0, err
		}

		if _, err := t.Unify(id, tail); err != nil {
			return 0, err
		}

		return id, nil
	}, str)
}

func (t *TermPool) putStringLike(tail func() (Term, error), str string) (Term, error) {
	if str == "" {
		return tail()
	}

	var ok bool
	t.strings, ok = cappend(t.strings, str)
	if !ok {
		return 0, errTooManyStrings
	}

	id, err := t.putTagPayload(termTagString, int32(len(t.strings)-1))
	if err != nil {
		return 0, err
	}

	if _, err := tail(); err != nil {
		return 0, err
	}

	return id, nil
}

func (t *TermPool) PutSequence(sep Atom, args ...Term) (Term, error) {
	ret, args := args[len(args)-1], args[:len(args)-1]
	for i := len(args) - 1; i >= 0; i-- {
		var err error
		ret, err = t.PutCompound(sep, args[i], ret)
		if err != nil {
			return 0, err
		}
	}
	return ret, nil
}

func (t *TermPool) Compound(id Term) (Functor, func(int) (Term, error), bool) {
	switch tag := t.tags[id]; tag {
	case termTagFunctor:
		f := t.functors[t.terms[id]]
		return f, func(n int) (Term, error) {
			return t.terms[int(id)+1+n], nil
		}, true
	case termTagList:
		return Functor{Name: Atom('.'), Arity: 2}, func(n int) (Term, error) {
			if n == 0 {
				return t.terms[id], nil
			}
			return id + 1, nil
		}, true
	case termTagString:
		str := t.strings[t.terms[id]]
		r, i := utf8.DecodeRuneInString(str)
		return Functor{Name: Atom('.'), Arity: 2}, func(n int) (Term, error) {
			if n == 0 {
				return t.PutAtom(Atom(r))
			}
			tail := str[i:]
			if tail == "" {
				return id + 1, nil
			}
			return t.PutString(tail)
		}, true
	default:
		return Functor{}, nil, false
	}
}

func (t *TermPool) Callable(id Term) (Functor, func(int) (Term, error), bool) {
	if a, ok := t.Atom(id); ok {
		return Functor{Name: a, Arity: 0}, nil, true
	}

	return t.Compound(id)
}

// Unify unifies 2 Terms.
func (t *TermPool) Unify(x, y Term) (bool, error) {
	return t.unify(x, y, false)
}

func (t *TermPool) UnifyWithOccursCheck(x, y Term) (bool, error) {
	return t.unify(x, y, true)
}

func (t *TermPool) unify(x, y Term, occursCheck bool) (bool, error) {
	if x == y {
		return true, nil
	}

	if vx, ok := t.Variable(x); ok {
		if vy, ok := t.Variable(y); ok {
			if vx == vy {
				return true, nil
			}
		}
		if _, _, ok := t.Compound(y); ok && occursCheck {
			ok, err := t.contains(y, x)
			if err != nil {
				return false, err
			}
			if ok {
				return false, nil
			}
		}
		if err := t.env.Bind(vx, y); err != nil {
			return false, err
		}
		return true, nil
	}

	if ax, ok := t.Atom(x); ok {
		if ay, ok := t.Atom(y); ok {
			return ay == ax, nil
		}
	}

	if ix, ok := t.Integer(x); ok {
		if iy, ok := t.Integer(y); ok {
			return ix == iy, nil
		}
	}

	if fx, ok := t.Float(x); ok {
		if fy, ok := t.Float(y); ok {
			return fx == fy, nil
		}
	}

	if fx, ax, ok := t.Compound(x); ok {
		if fy, ay, ok := t.Compound(y); ok {
			if fx != fy {
				return false, nil
			}

			for i := 0; i < fx.Arity; i++ {
				x, err := ax(i)
				if err != nil {
					return false, err
				}
				y, err := ay(i)
				if err != nil {
					return false, err
				}

				ok, err := t.unify(x, y, occursCheck)
				if err != nil {
					return false, err
				}
				if !ok {
					return false, nil
				}
			}
		}
	}

	if _, ok := t.Variable(y); ok {
		return t.unify(y, x, occursCheck)
	}

	return false, nil
}

func (t *TermPool) contains(x, y Term) (bool, error) {
	if x == y {
		return true, nil
	}

	f, arg, ok := t.Compound(x)
	if !ok {
		return false, nil
	}

	for i := 0; i < f.Arity; i++ {
		a, err := arg(i)
		if err != nil {
			return false, err
		}

		ok, err := t.contains(a, y)
		if err != nil {
			return false, err
		}
		if ok {
			return true, nil
		}
	}

	return false, nil
}

func (t *TermPool) RenamedCopy(id Term) (Term, error) {
	return t.renamedCopy(id, map[Term]Term{})
}

func (t *TermPool) renamedCopy(id Term, copied map[Term]Term) (Term, error) {
	id = t.Resolve(id)
	if cid, ok := copied[id]; ok {
		return cid, nil
	}

	if _, ok := t.Variable(id); ok {
		cid, err := t.PutVariable(NewVariable(t))
		if err != nil {
			return 0, err
		}
		copied[id] = cid
		return cid, nil
	}

	if f, arg, ok := t.Compound(id); ok {
		if f == (Functor{Name: Atom('.'), Arity: 2}) {
			if t.tags[id] == termTagString {
				tail, err := t.renamedCopy(id+1, copied)
				if err != nil {
					return 0, err
				}
				cid, err := t.PutPartialString(tail, t.strings[t.terms[id]])
				if err != nil {
					return 0, err
				}
				copied[id] = cid
				return cid, nil
			}

			var ids []Term
			iter := ListIterator{TermPool: t, List: id, AllowPartial: true, AllowCycle: true}
			for iter.Next() {
				id, err := t.renamedCopy(iter.Current(), copied)
				if err != nil {
					return 0, err
				}
				ids = append(ids, id)
			}
			if err := iter.Err(); err != nil {
				return 0, err
			}
			tail, err := t.renamedCopy(iter.Suffix(), copied)
			if err != nil {
				return 0, err
			}
			cid, err := t.PutPartialList(tail, ids...)
			if err != nil {
				return 0, err
			}
			copied[id] = cid
			return cid, nil
		}

		ids := make([]Term, 0, f.Arity)
		for i := 0; i < f.Arity; i++ {
			a, err := arg(i)
			if err != nil {
				return 0, err
			}
			id, err := t.renamedCopy(a, copied)
			ids = append(ids, id)
		}
		cid, err := t.PutCompound(f.Name, ids...)
		if err != nil {
			return 0, err
		}
		copied[id] = cid
		return cid, nil
	}

	copied[id] = id
	return id, nil
}

func (t *TermPool) CyclicTerm(id Term) (bool, error) {
	return t.cyclicTerm(id, map[Term]struct{}{})
}

func (t *TermPool) cyclicTerm(id Term, visited map[Term]struct{}) (bool, error) {
	id = t.Resolve(id)

	if _, ok := visited[id]; ok {
		return true, nil
	}
	visited[id] = struct{}{}

	f, arg, ok := t.Compound(id)
	if !ok {
		return false, nil
	}

	for i := 0; i < f.Arity; i++ {
		a, err := arg(i)
		if err != nil {
			return false, err
		}
		ok, err := t.cyclicTerm(a, visited)
		if err != nil {
			return false, err
		}
		if ok {
			return true, nil
		}
	}

	return false, nil
}

// Unqualify returns qualifying module and unqualified term.
func (t *TermPool) Unqualify(m Atom, id Term) (Atom, Term) {
	f, arg, ok := t.Compound(id)
	if !ok {
		return m, id
	}

	if f != (Functor{Name: Atom(':'), Arity: 2}) {
		return m, id
	}

	aid, _ := arg(0)
	mm, ok := t.Atom(aid)
	if !ok {
		return m, id
	}

	aid, _ = arg(1)
	return t.Unqualify(mm, aid)
}

// WriteOptions specify how the Term writes itself.
type WriteOptions struct {
	ignoreOps     bool
	quoted        bool
	variableNames map[Variable]Atom
	numberVars    bool

	ops         operators
	priority    int16
	visited     map[Term]struct{}
	prefixMinus bool
	left, right Operator
	maxDepth    int
}

func (o WriteOptions) withQuoted(quoted bool) *WriteOptions {
	o.quoted = quoted
	return &o
}

func (o WriteOptions) withVisited(id Term) *WriteOptions {
	visited := make(map[Term]struct{}, len(o.visited))
	maps.Copy(visited, o.visited)
	visited[id] = struct{}{}
	o.visited = visited
	return &o
}

func (o WriteOptions) withPriority(priority int16) *WriteOptions {
	o.priority = priority
	return &o
}

func (o WriteOptions) withLeft(op Operator) *WriteOptions {
	o.left = op
	return &o
}

func (o WriteOptions) withRight(op Operator) *WriteOptions {
	o.right = op
	return &o
}

var defaultWriteOptions = WriteOptions{
	ops: operators{
		'+': [_operatorClassLen]Operator{
			operatorClassInfix: {Priority: 500, Specifier: OperatorSpecifierYFX, Name: Atom('+')}, // for flag+value
		},
		'/': [_operatorClassLen]Operator{
			operatorClassInfix: {Priority: 400, Specifier: OperatorSpecifierYFX, Name: Atom('/')}, // for principal functors
		},
	},
	variableNames: map[Variable]Atom{},
	priority:      1200,
}

func (t *TermPool) Write(w io.Writer, id Term, opts *WriteOptions) error {
	if opts == nil {
		opts = &defaultWriteOptions
	}

	id = t.Resolve(id)

	if v, ok := t.Variable(id); ok {
		return t.writeVariable(w, v, opts)
	}

	if a, ok := t.Atom(id); ok {
		return t.writeAtom(w, a, opts)
	}

	if i, ok := t.Integer(id); ok {
		return t.writeInteger(w, i, opts)
	}

	if f, ok := t.Float(id); ok {
		return t.writeFloat(w, f, opts)
	}

	if ok, err := t.writeCompoundVisit(w, id, opts); err != nil || ok {
		return err
	}

	opts = opts.withVisited(id)

	f, arg, _ := t.Compound(id)

	if opts.numberVars && f == (Functor{Name: NewAtom("$VAR"), Arity: 1}) {
		a, _ := arg(0)
		if n, ok := t.Integer(a); ok && n >= 0 {
			return t.writeCompoundNumberVars(w, n)
		}
	}

	if !opts.ignoreOps {
		switch f {
		case Functor{Name: Atom('.'), Arity: 2}:
			return t.writeCompoundList(w, id, opts)
		case Functor{Name: atomEmptyBlock, Arity: 1}:
			return t.writeCompoundCurlyBracketed(w, id, opts)
		}

		for _, o := range opts.ops[f.Name] {
			if o.Specifier.arity() == f.Arity {
				return t.writeCompoundOp(w, id, opts, &o)
			}
		}
	}

	return t.writeCompoundFunctionalNotation(w, id, opts)
}

func (t *TermPool) writeVariable(w io.Writer, v Variable, opts *WriteOptions) error {
	if id, ok := t.env.Lookup(v); ok {
		return t.Write(w, id, opts)
	}

	if a, ok := opts.variableNames[v]; ok {
		return t.writeAtom(w, a, opts.withQuoted(false).withLeft(Operator{}).withRight(Operator{}))
	}

	_, err := fmt.Fprint(w, v)
	return err
}

func (t *TermPool) writeAtom(w io.Writer, a Atom, opts *WriteOptions) error {
	ew := errWriter{w: w}
	openClose := (opts.left != (Operator{}) || opts.right != (Operator{})) && opts.ops.defined(a)

	if openClose {
		if opts.left.Name != 0 && opts.left.Specifier.class() == operatorClassPrefix {
			_, _ = fmt.Fprint(&ew, " ")
		}
		_, _ = fmt.Fprint(&ew, "(")
		opts = opts.withLeft(Operator{}).withRight(Operator{})
	}

	if opts.quoted && needQuoted(a) {
		if opts.left != (Operator{}) && needQuoted(opts.left.Name) { // Avoid 'FOO''BAR'.
			_, _ = fmt.Fprint(&ew, " ")
		}
		_, _ = ew.Write([]byte(quote(a.String())))
		if opts.right != (Operator{}) && needQuoted(opts.right.Name) { // Avoid 'FOO''BAR'.
			_, _ = fmt.Fprint(&ew, " ")
		}
	} else {
		if (letterDigit(opts.left.Name) && letterDigit(a)) || (graphic(opts.left.Name) && graphic(a)) {
			_, _ = fmt.Fprint(&ew, " ")
		}
		_, _ = fmt.Fprint(&ew, a)
		if (letterDigit(opts.right.Name) && letterDigit(a)) || (graphic(opts.right.Name) && graphic(a)) {
			_, _ = fmt.Fprint(&ew, " ")
		}
	}

	if openClose {
		_, _ = fmt.Fprint(&ew, ")")
	}

	return ew.err
}

func (t *TermPool) writeInteger(w io.Writer, i int64, opts *WriteOptions) error {
	ew := errWriter{w: w}
	openClose := opts.left.Name == Atom('-') && opts.left.Specifier.class() == operatorClassPrefix && i > 0

	if openClose {
		_, _ = fmt.Fprint(&ew, " (")
		opts = opts.withLeft(Operator{}).withRight(Operator{})
	} else {
		if opts.left != (Operator{}) && (letterDigit(opts.left.Name) || (i < 0 && graphic(opts.left.Name))) {
			_, _ = fmt.Fprint(&ew, " ")
		}
	}

	s := strconv.FormatInt(i, 10)
	_, _ = fmt.Fprint(&ew, s)

	if openClose {
		_, _ = fmt.Fprint(&ew, ")")
	}

	// Avoid ambiguous 0b, 0o, 0x or 0'.
	if !openClose && opts.right != (Operator{}) && (letterDigit(opts.right.Name) || (needQuoted(opts.right.Name) && opts.right.Name != Atom(',') && opts.right.Name != Atom('|'))) {
		_, _ = fmt.Fprint(&ew, " ")
	}

	return ew.err
}

func (t *TermPool) writeFloat(w io.Writer, f float64, opts *WriteOptions) error {
	ew := errWriter{w: w}
	openClose := opts.left.Name == Atom('-') && opts.left.Specifier.class() == operatorClassPrefix && f > 0

	if openClose || (f < 0 && opts.left != Operator{}) {
		_, _ = fmt.Fprint(&ew, " ")
	}

	if openClose {
		_, _ = fmt.Fprint(&ew, "(")
	}

	s := strconv.FormatFloat(f, 'g', -1, 64)
	if !strings.ContainsRune(s, '.') {
		if strings.ContainsRune(s, 'e') {
			s = strings.Replace(s, "e", ".0e", 1)
		} else {
			s += ".0"
		}
	}
	_, _ = fmt.Fprint(&ew, s)

	if openClose {
		_, _ = fmt.Fprint(&ew, ")")
	}

	if !openClose && opts.right != (Operator{}) && (opts.right.Name == Atom('e') || opts.right.Name == Atom('E')) {
		_, _ = fmt.Fprint(&ew, " ")
	}

	return ew.err
}

func (t *TermPool) writeCompoundVisit(w io.Writer, id Term, opts *WriteOptions) (bool, error) {
	if _, ok := opts.visited[id]; ok {
		err := t.writeAtom(w, atomEllipsis, opts)
		return true, err
	}
	return false, nil
}

func (t *TermPool) writeCompoundNumberVars(w io.Writer, n int64) error {
	const letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	ew := errWriter{w: w}
	i, j := n%int64(len(letters)), n/int64(len(letters))
	_, _ = fmt.Fprint(&ew, string(letters[i]))
	if j != 0 {
		_, _ = fmt.Fprint(&ew, strconv.FormatInt(j, 10))
	}
	return ew.err
}

func (t *TermPool) writeCompoundList(w io.Writer, id Term, opts *WriteOptions) error {
	ew := errWriter{w: w}
	opts = opts.withPriority(999).withLeft(Operator{}).withRight(Operator{})
	_, _ = fmt.Fprint(&ew, "[")
	_, arg, _ := t.Compound(id)
	car, err := arg(0)
	if err != nil {
		return err
	}
	if err := t.Write(&ew, car, opts); err != nil {
		return err
	}
	cdr, err := arg(1)
	if err != nil {
		return err
	}
	iter := ListIterator{TermPool: t, List: cdr, AllowCycle: opts.maxDepth > 0}
	for iter.Next() {
		opts.maxDepth--
		if opts.maxDepth == 0 {
			_, _ = fmt.Fprint(&ew, "|")
			if err := t.writeAtom(&ew, atomEllipsis, opts); err != nil {
				return err
			}
			break
		}
		_, _ = fmt.Fprint(&ew, ",")
		if err := t.Write(&ew, iter.Current(), opts); err != nil {
			return err
		}
	}
	if err := iter.Err(); err != nil {
		_, _ = fmt.Fprint(&ew, "|")
		s := iter.Suffix()
		if f, _, ok := t.Compound(s); ok && f == (Functor{Name: Atom('.'), Arity: 2}) {
			if err := t.writeAtom(&ew, atomEllipsis, opts); err != nil {
				return err
			}
		} else {
			if err := t.Write(&ew, s, opts); err != nil {
				return err
			}
		}
	}
	_, _ = fmt.Fprint(&ew, "]")
	return ew.err
}

func (t *TermPool) writeCompoundCurlyBracketed(w io.Writer, id Term, opts *WriteOptions) error {
	ew := errWriter{w: w}
	_, _ = fmt.Fprint(&ew, "{")
	_, arg, _ := t.Compound(id)
	b, _ := arg(0)
	if err := t.Write(&ew, b, opts.withLeft(Operator{})); err != nil {
		return err
	}
	_, _ = fmt.Fprint(&ew, "}")
	return ew.err
}

func (t *TermPool) writeCompoundOp(w io.Writer, id Term, opts *WriteOptions, op *Operator) error {
	switch op.Specifier {
	case OperatorSpecifierFX, OperatorSpecifierFY:
		return t.writeCompoundOpPrefix(w, id, opts, op)
	case OperatorSpecifierXF, OperatorSpecifierYF:
		return t.writeCompoundOpPostfix(w, id, opts, op)
	default:
		return t.writeCompoundOpInfix(w, id, opts, op)
	}
}

func (t *TermPool) writeCompoundOpPrefix(w io.Writer, id Term, opts *WriteOptions, op *Operator) error {
	ew := errWriter{w: w}
	_, r := op.bindingPriorities()
	openClose := opts.priority < op.Priority || (opts.right != Operator{} && r >= opts.right.Priority)

	if opts.left != (Operator{}) {
		_, _ = fmt.Fprint(&ew, " ")
	}
	if openClose {
		_, _ = fmt.Fprint(&ew, "(")
		opts = opts.withLeft(Operator{}).withRight(Operator{})
	}
	f, arg, _ := t.Compound(id)
	_ = t.writeAtom(&ew, f.Name, opts.withLeft(Operator{}).withRight(Operator{}))
	{
		opts := opts.withPriority(r).withLeft(*op)
		opts.maxDepth--
		if opts.maxDepth == 0 {
			_ = t.writeAtom(&ew, atomEllipsis, opts)
		} else {
			a, _ := arg(0)
			if err := t.Write(&ew, a, opts); err != nil {
				return err
			}
		}
	}
	if openClose {
		_, _ = fmt.Fprint(&ew, ")")
	}
	return ew.err
}

func (t *TermPool) writeCompoundOpPostfix(w io.Writer, id Term, opts *WriteOptions, op *Operator) error {
	ew := errWriter{w: w}
	l, _ := op.bindingPriorities()
	openClose := opts.priority < op.Priority || (opts.left.Name == Atom('-') && opts.left.Specifier.class() == operatorClassPrefix)

	if openClose {
		if opts.left != (Operator{}) {
			_, _ = fmt.Fprint(&ew, " ")
		}
		_, _ = fmt.Fprint(&ew, "(")
		opts = opts.withLeft(Operator{}).withRight(Operator{})
	}
	f, arg, _ := t.Compound(id)
	{
		opts := opts.withPriority(l).withRight(*op)
		opts.maxDepth--
		if opts.maxDepth == 0 {
			_ = t.writeAtom(&ew, atomEllipsis, opts)
		} else {
			a, _ := arg(0)
			if err := t.Write(&ew, a, opts); err != nil {
				return err
			}
		}
	}
	_ = t.writeAtom(&ew, f.Name, opts.withLeft(Operator{}).withRight(Operator{}))
	if openClose {
		_, _ = fmt.Fprint(&ew, ")")
	} else if opts.right != (Operator{}) {
		_, _ = fmt.Fprint(&ew, " ")
	}
	return ew.err
}

func (t *TermPool) writeCompoundOpInfix(w io.Writer, id Term, opts *WriteOptions, op *Operator) error {
	ew := errWriter{w: w}
	l, r := op.bindingPriorities()
	openClose := opts.priority < op.Priority ||
		(opts.left.Name == Atom('-') && opts.left.Specifier.class() == operatorClassPrefix) ||
		(opts.right != Operator{} && r >= opts.right.Priority)

	if openClose {
		if opts.left.Name != 0 && opts.left.Specifier.class() == operatorClassPrefix {
			_, _ = fmt.Fprint(&ew, " ")
		}
		_, _ = fmt.Fprint(&ew, "(")
		opts = opts.withLeft(Operator{}).withRight(Operator{})
	}
	f, arg, _ := t.Compound(id)
	{
		opts := opts.withPriority(l).withRight(*op)
		opts.maxDepth--
		if opts.maxDepth == 0 {
			_ = t.writeAtom(&ew, atomEllipsis, opts)
		} else {
			l, _ := arg(0)
			if err := t.Write(&ew, l, opts); err != nil {
				return err
			}
		}
	}
	switch f.Name {
	case Atom('.'), Atom('|'):
		_, _ = fmt.Fprint(&ew, f.Name.String())
	default:
		_ = t.writeAtom(&ew, f.Name, opts.withLeft(Operator{}).withRight(Operator{}))
	}
	{
		opts := opts.withPriority(r).withLeft(*op)
		opts.maxDepth--
		if opts.maxDepth == 0 {
			_ = t.writeAtom(&ew, atomEllipsis, opts)
		} else {
			r, _ := arg(1)
			if err := t.Write(&ew, r, opts); err != nil {
				return err
			}
		}
	}
	if openClose {
		_, _ = fmt.Fprint(&ew, ")")
	}
	return ew.err
}

func (t *TermPool) writeCompoundFunctionalNotation(w io.Writer, id Term, opts *WriteOptions) error {
	ew := errWriter{w: w}
	opts = opts.withRight(Operator{})
	f, arg, _ := t.Compound(id)
	_ = t.writeAtom(&ew, f.Name, opts)
	_, _ = fmt.Fprint(&ew, "(")
	opts = opts.withLeft(Operator{}).withPriority(999)
	opts.maxDepth--
	for i := 0; i < f.Arity; i++ {
		if i != 0 {
			_, _ = fmt.Fprint(&ew, ",")
		}
		if opts.maxDepth == 0 {
			_ = t.writeAtom(&ew, atomEllipsis, opts)
			continue
		}
		a, _ := arg(i)
		if err := t.Write(&ew, a, opts); err != nil {
			return err
		}
	}
	_, _ = fmt.Fprint(&ew, ")")
	return ew.err
}

// Compare compares two Terms.
func (t *TermPool) Compare(x, y Term) (int, error) {
	x, y = t.Resolve(x), t.Resolve(y)

	if x, ok := t.Variable(x); ok {
		if y, ok := t.Variable(y); ok {
			return int(x) - int(y), nil
		}

		return -1, nil
	}

	if x, ok := t.Float(x); ok {
		if _, ok := t.Variable(y); ok {
			return 1, nil
		}

		if y, ok := t.Float(y); ok {
			switch {
			case x > y:
				return 1, nil
			case x < y:
				return -1, nil
			default:
				return 0, nil
			}
		}

		return -1, nil
	}

	if x, ok := t.Integer(x); ok {
		if _, ok := t.Variable(y); ok {
			return 1, nil
		}

		if _, ok := t.Float(y); ok {
			return 1, nil
		}

		if y, ok := t.Integer(y); ok {
			switch {
			case x > y:
				return 1, nil
			case x < y:
				return -1, nil
			default:
				return 0, nil
			}
		}

		return -1, nil
	}

	if x, ok := t.Atom(x); ok {
		if _, ok := t.Variable(y); ok {
			return 1, nil
		}

		if _, ok := t.Float(y); ok {
			return 1, nil
		}

		if _, ok := t.Integer(y); ok {
			return 1, nil
		}

		if y, ok := t.Atom(y); ok {
			return strings.Compare(x.String(), y.String()), nil
		}

		return -1, nil
	}

	fx, argX, _ := t.Compound(x)
	fy, argY, ok := t.Compound(y)
	if !ok {
		return 1, nil
	}

	if o := fx.Arity - fy.Arity; o != 0 {
		return o, nil
	}

	if o := strings.Compare(fx.Name.String(), fy.Name.String()); o != 0 {
		return o, nil
	}

	for i := 0; i < fx.Arity; i++ {
		ax, err := argX(i)
		if err != nil {
			return 0, err
		}

		ay, err := argY(i)
		if err != nil {
			return 0, err
		}

		o, err := t.Compare(ax, ay)
		if err != nil {
			return 0, err
		}
		if o != 0 {
			return o, nil
		}
	}

	return 0, nil
}

// https://go.dev/blog/errors-are-values
type errWriter struct {
	w   io.Writer
	err error
}

func (ew *errWriter) Write(p []byte) (int, error) {
	if ew.err != nil {
		return 0, nil
	}
	var n int
	n, ew.err = ew.w.Write(p)
	return n, nil
}

func (t *TermPool) MustBeAtom(id Term) (Atom, error) {
	id = t.Resolve(id)

	if _, ok := t.Variable(id); ok {
		return 0, ErrInstantiation
	}

	a, ok := t.Atom(id)
	if !ok {
		return 0, &TypeError{Type: NewAtom("atom"), Culprit: id}
	}

	return a, nil
}

func (t *TermPool) CanBeAtom(id Term) error {
	id = t.Resolve(id)

	if _, ok := t.Variable(id); ok {
		return nil
	}

	if _, ok := t.Atom(id); !ok {
		return &TypeError{Type: NewAtom("atom"), Culprit: id}
	}

	return nil
}

func (t *TermPool) MustBeInteger(id Term) (int64, error) {
	id = t.Resolve(id)

	if _, ok := t.Variable(id); ok {
		return 0, ErrInstantiation
	}

	i, ok := t.Integer(id)
	if !ok {
		return 0, &TypeError{Type: NewAtom("integer"), Culprit: id}
	}

	return i, nil
}

func (t *TermPool) CanBeInteger(id Term) error {
	id = t.Resolve(id)

	if _, ok := t.Variable(id); ok {
		return nil
	}

	if _, ok := t.Integer(id); !ok {
		return &TypeError{Type: NewAtom("integer"), Culprit: id}
	}

	return nil
}

func (t *TermPool) MustBeOperatorPriority(id Term) (int16, error) {
	id = t.Resolve(id)

	if _, ok := t.Variable(id); ok {
		return 0, ErrInstantiation
	}

	p, ok := t.Integer(id)
	if !ok {
		return 0, &TypeError{Type: NewAtom("integer"), Culprit: id}
	}
	if p < 0 || p > 1200 {
		return 0, &DomainError{Domain: NewAtom("operator_priority"), Culprit: id}
	}

	return int16(p), nil
}

func (t *TermPool) CanBeOperatorPriority(id Term) error {
	id = t.Resolve(id)

	if _, ok := t.Variable(id); ok {
		return nil
	}

	p, ok := t.Integer(id)
	if !ok || p < 0 || p > 1200 {
		return &DomainError{Domain: NewAtom("operator_priority"), Culprit: id}
	}

	return nil
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

var operatorSpecifiers = map[Atom]OperatorSpecifier{
	atomFX:  OperatorSpecifierFX,
	atomFY:  OperatorSpecifierFY,
	atomXF:  OperatorSpecifierXF,
	atomYF:  OperatorSpecifierYF,
	atomXFX: OperatorSpecifierXFX,
	atomXFY: OperatorSpecifierXFY,
	atomYFX: OperatorSpecifierYFX,
}

var revOperatorSpecifiers = reverseMap(operatorSpecifiers)

func (t *TermPool) PutOperatorSpecifier(spec OperatorSpecifier) (Term, error) {
	return t.PutAtom(revOperatorSpecifiers[spec])
}

func (t *TermPool) MustBeOperatorSpecifier(id Term) (OperatorSpecifier, error) {
	id = t.Resolve(id)

	if _, ok := t.Variable(id); ok {
		return 0, ErrInstantiation
	}
	s, ok := t.Atom(id)
	if !ok {
		return 0, &TypeError{Type: NewAtom("atom"), Culprit: id}
	}
	spec, ok := operatorSpecifiers[s]
	if !ok {
		return 0, &DomainError{Domain: NewAtom("operator_specifier"), Culprit: id}
	}
	return spec, nil
}

func (t *TermPool) CanBeOperatorSpecifier(id Term) error {
	id = t.Resolve(id)

	if _, ok := t.Variable(id); ok {
		return nil
	}

	a, ok := t.Atom(id)
	_, valid := operatorSpecifiers[a]
	if !ok || !valid {
		return &DomainError{Domain: NewAtom("operator_specifier"), Culprit: id}
	}

	return nil
}

const (
	atomLessThan    = Atom('<')
	atomEqual       = Atom('=')
	atomGreaterThan = Atom('>')
)

func (t *TermPool) MustBeOrder(id Term) (Atom, error) {
	id = t.Resolve(id)

	if _, ok := t.Variable(id); ok {
		return 0, ErrInstantiation
	}

	a, ok := t.Atom(id)
	if !ok {
		return 0, &TypeError{Type: NewAtom("atom"), Culprit: id}
	}

	switch a {
	case atomLessThan, atomEqual, atomGreaterThan:
		break
	default:
		return 0, &DomainError{Domain: NewAtom("order"), Culprit: id}
	}

	return a, nil
}

func (t *TermPool) CanBeOrder(id Term) error {
	id = t.Resolve(id)

	if _, ok := t.Variable(id); ok {
		return nil
	}

	a, ok := t.Atom(id)
	if !ok {
		return &TypeError{Type: NewAtom("atom"), Culprit: id}
	}

	switch a {
	case atomLessThan, atomEqual, atomGreaterThan:
		break
	default:
		return &DomainError{Domain: NewAtom("order"), Culprit: id}
	}

	return nil
}

func (t *TermPool) MustBeCallable(id Term) (Functor, func(int) (Term, error), error) {
	id = t.Resolve(id)

	if _, ok := t.Variable(id); ok {
		return Functor{}, nil, ErrInstantiation
	}

	f, arg, ok := t.Callable(id)
	if !ok {
		return Functor{}, nil, &DomainError{Domain: NewAtom("callable"), Culprit: id}
	}

	return f, arg, nil
}

func (t *TermPool) CanBeCallable(id Term) error {
	id = t.Resolve(id)

	if _, ok := t.Variable(id); ok {
		return nil
	}

	if _, _, ok := t.Callable(id); !ok {
		return &DomainError{Domain: NewAtom("callable"), Culprit: id}
	}

	return nil
}

func (t *TermPool) MustBeList(id Term) iter.Seq2[Term, error] {
	return func(yield func(Term, error) bool) {
		iter := ListIterator{TermPool: t, List: id}
		for iter.Next() {
			yield(iter.Current(), nil)
		}
		yield(0, iter.Err())
	}
}

func (t *TermPool) CanBeList(id Term) iter.Seq2[Term, error] {
	return func(yield func(Term, error) bool) {
		iter := ListIterator{TermPool: t, List: id, AllowPartial: true}
		for iter.Next() {
			yield(iter.Current(), nil)
		}
		yield(0, iter.Err())
	}
}

func (t *TermPool) MustBePair(id Term) (Term, Term, error) {
	id = t.Resolve(id)

	if _, ok := t.Variable(id); ok {
		return 0, 0, ErrInstantiation
	}

	f, arg, ok := t.Compound(id)
	if !ok || f != (Functor{Name: Atom('-'), Arity: 2}) {
		return 0, 0, &TypeError{Type: NewAtom("pair"), Culprit: id}
	}

	k, err := arg(0)
	if err != nil {
		return 0, 0, err
	}

	v, err := arg(1)
	if err != nil {
		return 0, 0, err
	}

	return k, v, nil
}

func (t *TermPool) CanBePair(id Term) error {
	id = t.Resolve(id)

	if _, ok := t.Variable(id); ok {
		return nil
	}

	f, _, ok := t.Compound(id)
	if !ok || f != (Functor{Name: Atom('-'), Arity: 2}) {
		return &TypeError{Type: NewAtom("pair"), Culprit: id}
	}

	return nil
}
