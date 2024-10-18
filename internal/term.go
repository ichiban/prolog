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
	Module string
	Name   string
	Arity  int
}

func (f Functor) String() string {
	if f.Module != "" {
		return fmt.Sprintf("%s:%s/%d", f.Module, f.Name, f.Arity)
	}
	return fmt.Sprintf("%s/%d", f.Name, f.Arity)
}

// Term is a Prolog datum.
// It may refer to another Term in Heap.
type Term struct {
	tag     termTag
	payload int32
}

// Heap is a memory region to store Terms.
type Heap struct {
	terms []Term

	env      Env
	atoms    AtomTable
	integers []int64
	floats   []float64
	functors []Functor
	strings  []string
}

func NewHeap(bytes int) Heap {
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

	p := Heap{
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

func (h *Heap) PutVariable() (Term, error) {
	v := h.env.lastVariable + 1
	return Term{tag: termTagVariable, payload: int32(v)}, nil
}

func (h *Heap) Variable(term Term) bool {
	return term.tag == termTagVariable
}

func (h *Heap) Resolve(term Term) Term {
	for {
		switch term.tag {
		case termTagVariable:
			v := Variable(term.payload)
			val, ok := h.env.Lookup(v)
			if !ok {
				return term
			}
			term = val
		case termTagReference:
			term = h.terms[term.payload]
		default:
			return term
		}
	}
}

func (h *Heap) PutAtom(name string) (Term, error) {
	// A one-char atom is just a rune.
	if r, n := utf8.DecodeLastRuneInString(name); r != utf8.RuneError && n == len(name) {
		return Term{tag: termTagCharacter, payload: r}, nil
	}

	atom, ok := h.atoms.Put(name)
	if !ok {
		return Term{}, errTooManyAtoms
	}
	return Term{tag: termTagAtom, payload: int32(atom)}, nil
}

func (h *Heap) Atom(term Term) (string, bool) {
	if term.tag == termTagCharacter {
		return string(term.payload), true
	}

	if term.tag != termTagAtom {
		return "", false
	}
	return h.atoms.names.Get(atomID(term.payload))
}

func (h *Heap) PutInteger(n int64) (Term, error) {
	// TODO: optimize for smaller/bigger integers.
	var ok bool
	h.integers, ok = cappend(h.integers, n)
	if !ok {
		return Term{}, errTooManyIntegers
	}

	return Term{tag: termTagInteger, payload: int32(len(h.integers) - 1)}, nil
}

func (h *Heap) Integer(term Term) (int64, bool) {
	if term.tag != termTagInteger {
		return 0, false
	}
	return h.integers[term.payload], true
}

func (h *Heap) PutFloat(f float64) (Term, error) {
	var ok bool
	h.floats, ok = cappend(h.floats, f)
	if !ok {
		return Term{}, errTooManyFloats
	}

	return Term{tag: termTagFloat, payload: int32(len(h.floats) - 1)}, nil
}

func (h *Heap) Float(term Term) (float64, bool) {
	if term.tag != termTagFloat {
		return 0, false
	}
	return h.floats[term.payload], true
}

func (h *Heap) Atomic(term Term) bool {
	tag := term.tag
	return termTagAtom <= tag && tag <= termTagFloat
}

func (h *Heap) PutCompound(name string, args ...Term) (Term, error) {
	if len(args) == 0 {
		return h.PutAtom(name)
	}

	var ok bool
	h.functors, ok = cappend(h.functors, Functor{Name: name, Arity: len(args)})
	if !ok {
		return Term{}, errTooManyFunctors
	}

	id, err := h.putTagPayload(termTagFunctor, int32(len(h.functors)-1))
	if err != nil {
		return 0, err
	}

	for _, arg := range args {
		if _, err := h.putTagPayload(termTagArgument, int32(arg)); err != nil {
			return 0, err
		}
	}

	return id, nil
}

func (h *Heap) PutCompoundWithVarArgs(f Functor) (Term, error) {
	if f.Arity == 0 {
		return h.PutAtom(f.Name)
	}

	var ok bool
	h.functors, ok = cappend(h.functors, f)
	if !ok {
		return 0, errTooManyFunctors
	}

	id, err := h.putTagPayload(termTagFunctor, int32(len(h.functors)-1))
	if err != nil {
		return 0, err
	}

	for i := 0; i < f.Arity; i++ {
		if _, err := h.PutVariable(NewVariable(h)); err != nil {
			return 0, err
		}
	}

	return id, nil
}

func (h *Heap) PutFunctor(f Functor) (Term, error) {
	n, err := h.PutAtom(f.Name)
	if err != nil {
		return 0, err
	}

	a, err := h.PutInteger(int64(f.Arity))
	if err != nil {
		return 0, err
	}

	return h.PutCompound(Atom('/'), n, a)
}

func (h *Heap) PutList(args ...Term) (Term, error) {
	return h.putListLike(func() (Term, error) {
		return h.PutAtom(atomEmptyList)
	}, args...)
}

func (h *Heap) PutPartialList(tail Term, args ...Term) (Term, error) {
	return h.putListLike(func() (Term, error) {
		id, err := h.PutVariable(NewVariable(h))
		if err != nil {
			return 0, err
		}

		if _, err := h.Unify(id, tail); err != nil {
			return 0, err
		}

		return id, nil
	}, args...)
}

func (h *Heap) putListLike(tail func() (Term, error), args ...Term) (Term, error) {
	if len(args) == 0 {
		return tail()
	}

	id, err := h.putTagPayload(termTagList, int32(args[0]))
	if err != nil {
		return 0, err
	}

	for _, arg := range args[1:] {
		if _, err := h.putTagPayload(termTagList, int32(arg)); err != nil {
			return 0, err
		}
	}

	if _, err := tail(); err != nil {
		return 0, err
	}

	return id, nil
}

func (h *Heap) PutSet(elems ...Term) (Term, error) {
	return h.PutList(elems...) // TODO:
}

func (h *Heap) PutString(str string) (Term, error) {
	return h.putStringLike(func() (Term, error) {
		return h.PutAtom(atomEmptyList)
	}, str)
}

func (h *Heap) PutPartialString(tail Term, str string) (Term, error) {
	return h.putStringLike(func() (Term, error) {
		id, err := h.PutVariable(NewVariable(h))
		if err != nil {
			return 0, err
		}

		if _, err := h.Unify(id, tail); err != nil {
			return 0, err
		}

		return id, nil
	}, str)
}

func (h *Heap) putStringLike(tail func() (Term, error), str string) (Term, error) {
	if str == "" {
		return tail()
	}

	var ok bool
	h.strings, ok = cappend(h.strings, str)
	if !ok {
		return 0, errTooManyStrings
	}

	id, err := h.putTagPayload(termTagString, int32(len(h.strings)-1))
	if err != nil {
		return 0, err
	}

	if _, err := tail(); err != nil {
		return 0, err
	}

	return id, nil
}

func (h *Heap) PutSequence(sep Atom, args ...Term) (Term, error) {
	ret, args := args[len(args)-1], args[:len(args)-1]
	for i := len(args) - 1; i >= 0; i-- {
		var err error
		ret, err = h.PutCompound(sep, args[i], ret)
		if err != nil {
			return 0, err
		}
	}
	return ret, nil
}

func (h *Heap) Compound(id Term) (Functor, func(int) (Term, error), bool) {
	switch tag := h.tags[id]; tag {
	case termTagFunctor:
		f := h.functors[h.terms[id]]
		return f, func(n int) (Term, error) {
			return h.terms[int(id)+1+n], nil
		}, true
	case termTagList:
		return Functor{Name: Atom('.'), Arity: 2}, func(n int) (Term, error) {
			if n == 0 {
				return h.terms[id], nil
			}
			return id + 1, nil
		}, true
	case termTagString:
		str := h.strings[h.terms[id]]
		r, i := utf8.DecodeRuneInString(str)
		return Functor{Name: Atom('.'), Arity: 2}, func(n int) (Term, error) {
			if n == 0 {
				return h.PutAtom(Atom(r))
			}
			tail := str[i:]
			if tail == "" {
				return id + 1, nil
			}
			return h.PutString(tail)
		}, true
	default:
		return Functor{}, nil, false
	}
}

func (h *Heap) Callable(id Term) (Functor, func(int) (Term, error), bool) {
	if a, ok := h.Atom(id); ok {
		return Functor{Name: a, Arity: 0}, nil, true
	}

	return h.Compound(id)
}

// Unify unifies 2 Terms.
func (h *Heap) Unify(x, y Term) (bool, error) {
	return h.unify(x, y, false)
}

func (h *Heap) UnifyWithOccursCheck(x, y Term) (bool, error) {
	return h.unify(x, y, true)
}

func (h *Heap) unify(x, y Term, occursCheck bool) (bool, error) {
	if x == y {
		return true, nil
	}

	if vx, ok := h.Variable(x); ok {
		if vy, ok := h.Variable(y); ok {
			if vx == vy {
				return true, nil
			}
		}
		if _, _, ok := h.Compound(y); ok && occursCheck {
			ok, err := h.contains(y, x)
			if err != nil {
				return false, err
			}
			if ok {
				return false, nil
			}
		}
		if err := h.env.Bind(vx, y); err != nil {
			return false, err
		}
		return true, nil
	}

	if ax, ok := h.Atom(x); ok {
		if ay, ok := h.Atom(y); ok {
			return ay == ax, nil
		}
	}

	if ix, ok := h.Integer(x); ok {
		if iy, ok := h.Integer(y); ok {
			return ix == iy, nil
		}
	}

	if fx, ok := h.Float(x); ok {
		if fy, ok := h.Float(y); ok {
			return fx == fy, nil
		}
	}

	if fx, ax, ok := h.Compound(x); ok {
		if fy, ay, ok := h.Compound(y); ok {
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

				ok, err := h.unify(x, y, occursCheck)
				if err != nil {
					return false, err
				}
				if !ok {
					return false, nil
				}
			}
		}
	}

	if _, ok := h.Variable(y); ok {
		return h.unify(y, x, occursCheck)
	}

	return false, nil
}

func (h *Heap) contains(x, y Term) (bool, error) {
	if x == y {
		return true, nil
	}

	f, arg, ok := h.Compound(x)
	if !ok {
		return false, nil
	}

	for i := 0; i < f.Arity; i++ {
		a, err := arg(i)
		if err != nil {
			return false, err
		}

		ok, err := h.contains(a, y)
		if err != nil {
			return false, err
		}
		if ok {
			return true, nil
		}
	}

	return false, nil
}

func (h *Heap) RenamedCopy(id Term) (Term, error) {
	return h.renamedCopy(id, map[Term]Term{})
}

func (h *Heap) renamedCopy(id Term, copied map[Term]Term) (Term, error) {
	id = h.Resolve(id)
	if cid, ok := copied[id]; ok {
		return cid, nil
	}

	if _, ok := h.Variable(id); ok {
		cid, err := h.PutVariable(NewVariable(h))
		if err != nil {
			return 0, err
		}
		copied[id] = cid
		return cid, nil
	}

	if f, arg, ok := h.Compound(id); ok {
		if f == (Functor{Name: Atom('.'), Arity: 2}) {
			if h.tags[id] == termTagString {
				tail, err := h.renamedCopy(id+1, copied)
				if err != nil {
					return 0, err
				}
				cid, err := h.PutPartialString(tail, h.strings[h.terms[id]])
				if err != nil {
					return 0, err
				}
				copied[id] = cid
				return cid, nil
			}

			var ids []Term
			iter := ListIterator{TermPool: h, List: id, AllowPartial: true, AllowCycle: true}
			for iter.Next() {
				id, err := h.renamedCopy(iter.Current(), copied)
				if err != nil {
					return 0, err
				}
				ids = append(ids, id)
			}
			if err := iter.Err(); err != nil {
				return 0, err
			}
			tail, err := h.renamedCopy(iter.Suffix(), copied)
			if err != nil {
				return 0, err
			}
			cid, err := h.PutPartialList(tail, ids...)
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
			id, err := h.renamedCopy(a, copied)
			ids = append(ids, id)
		}
		cid, err := h.PutCompound(f.Name, ids...)
		if err != nil {
			return 0, err
		}
		copied[id] = cid
		return cid, nil
	}

	copied[id] = id
	return id, nil
}

func (h *Heap) CyclicTerm(id Term) (bool, error) {
	return h.cyclicTerm(id, map[Term]struct{}{})
}

func (h *Heap) cyclicTerm(id Term, visited map[Term]struct{}) (bool, error) {
	id = h.Resolve(id)

	if _, ok := visited[id]; ok {
		return true, nil
	}
	visited[id] = struct{}{}

	f, arg, ok := h.Compound(id)
	if !ok {
		return false, nil
	}

	for i := 0; i < f.Arity; i++ {
		a, err := arg(i)
		if err != nil {
			return false, err
		}
		ok, err := h.cyclicTerm(a, visited)
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
func (h *Heap) Unqualify(m Atom, id Term) (Atom, Term) {
	f, arg, ok := h.Compound(id)
	if !ok {
		return m, id
	}

	if f != (Functor{Name: Atom(':'), Arity: 2}) {
		return m, id
	}

	aid, _ := arg(0)
	mm, ok := h.Atom(aid)
	if !ok {
		return m, id
	}

	aid, _ = arg(1)
	return h.Unqualify(mm, aid)
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

func (h *Heap) Write(w io.Writer, id Term, opts *WriteOptions) error {
	if opts == nil {
		opts = &defaultWriteOptions
	}

	id = h.Resolve(id)

	if v, ok := h.Variable(id); ok {
		return h.writeVariable(w, v, opts)
	}

	if a, ok := h.Atom(id); ok {
		return h.writeAtom(w, a, opts)
	}

	if i, ok := h.Integer(id); ok {
		return h.writeInteger(w, i, opts)
	}

	if f, ok := h.Float(id); ok {
		return h.writeFloat(w, f, opts)
	}

	if ok, err := h.writeCompoundVisit(w, id, opts); err != nil || ok {
		return err
	}

	opts = opts.withVisited(id)

	f, arg, _ := h.Compound(id)

	if opts.numberVars && f == (Functor{Name: NewAtom("$VAR"), Arity: 1}) {
		a, _ := arg(0)
		if n, ok := h.Integer(a); ok && n >= 0 {
			return h.writeCompoundNumberVars(w, n)
		}
	}

	if !opts.ignoreOps {
		switch f {
		case Functor{Name: Atom('.'), Arity: 2}:
			return h.writeCompoundList(w, id, opts)
		case Functor{Name: atomEmptyBlock, Arity: 1}:
			return h.writeCompoundCurlyBracketed(w, id, opts)
		}

		for _, o := range opts.ops[f.Name] {
			if o.Specifier.arity() == f.Arity {
				return h.writeCompoundOp(w, id, opts, &o)
			}
		}
	}

	return h.writeCompoundFunctionalNotation(w, id, opts)
}

func (h *Heap) writeVariable(w io.Writer, v Variable, opts *WriteOptions) error {
	if id, ok := h.env.Lookup(v); ok {
		return h.Write(w, id, opts)
	}

	if a, ok := opts.variableNames[v]; ok {
		return h.writeAtom(w, a, opts.withQuoted(false).withLeft(Operator{}).withRight(Operator{}))
	}

	_, err := fmt.Fprint(w, v)
	return err
}

func (h *Heap) writeAtom(w io.Writer, a Atom, opts *WriteOptions) error {
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

func (h *Heap) writeInteger(w io.Writer, i int64, opts *WriteOptions) error {
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

func (h *Heap) writeFloat(w io.Writer, f float64, opts *WriteOptions) error {
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

func (h *Heap) writeCompoundVisit(w io.Writer, id Term, opts *WriteOptions) (bool, error) {
	if _, ok := opts.visited[id]; ok {
		err := h.writeAtom(w, atomEllipsis, opts)
		return true, err
	}
	return false, nil
}

func (h *Heap) writeCompoundNumberVars(w io.Writer, n int64) error {
	const letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	ew := errWriter{w: w}
	i, j := n%int64(len(letters)), n/int64(len(letters))
	_, _ = fmt.Fprint(&ew, string(letters[i]))
	if j != 0 {
		_, _ = fmt.Fprint(&ew, strconv.FormatInt(j, 10))
	}
	return ew.err
}

func (h *Heap) writeCompoundList(w io.Writer, id Term, opts *WriteOptions) error {
	ew := errWriter{w: w}
	opts = opts.withPriority(999).withLeft(Operator{}).withRight(Operator{})
	_, _ = fmt.Fprint(&ew, "[")
	_, arg, _ := h.Compound(id)
	car, err := arg(0)
	if err != nil {
		return err
	}
	if err := h.Write(&ew, car, opts); err != nil {
		return err
	}
	cdr, err := arg(1)
	if err != nil {
		return err
	}
	iter := ListIterator{TermPool: h, List: cdr, AllowCycle: opts.maxDepth > 0}
	for iter.Next() {
		opts.maxDepth--
		if opts.maxDepth == 0 {
			_, _ = fmt.Fprint(&ew, "|")
			if err := h.writeAtom(&ew, atomEllipsis, opts); err != nil {
				return err
			}
			break
		}
		_, _ = fmt.Fprint(&ew, ",")
		if err := h.Write(&ew, iter.Current(), opts); err != nil {
			return err
		}
	}
	if err := iter.Err(); err != nil {
		_, _ = fmt.Fprint(&ew, "|")
		s := iter.Suffix()
		if f, _, ok := h.Compound(s); ok && f == (Functor{Name: Atom('.'), Arity: 2}) {
			if err := h.writeAtom(&ew, atomEllipsis, opts); err != nil {
				return err
			}
		} else {
			if err := h.Write(&ew, s, opts); err != nil {
				return err
			}
		}
	}
	_, _ = fmt.Fprint(&ew, "]")
	return ew.err
}

func (h *Heap) writeCompoundCurlyBracketed(w io.Writer, id Term, opts *WriteOptions) error {
	ew := errWriter{w: w}
	_, _ = fmt.Fprint(&ew, "{")
	_, arg, _ := h.Compound(id)
	b, _ := arg(0)
	if err := h.Write(&ew, b, opts.withLeft(Operator{})); err != nil {
		return err
	}
	_, _ = fmt.Fprint(&ew, "}")
	return ew.err
}

func (h *Heap) writeCompoundOp(w io.Writer, id Term, opts *WriteOptions, op *Operator) error {
	switch op.Specifier {
	case OperatorSpecifierFX, OperatorSpecifierFY:
		return h.writeCompoundOpPrefix(w, id, opts, op)
	case OperatorSpecifierXF, OperatorSpecifierYF:
		return h.writeCompoundOpPostfix(w, id, opts, op)
	default:
		return h.writeCompoundOpInfix(w, id, opts, op)
	}
}

func (h *Heap) writeCompoundOpPrefix(w io.Writer, id Term, opts *WriteOptions, op *Operator) error {
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
	f, arg, _ := h.Compound(id)
	_ = h.writeAtom(&ew, f.Name, opts.withLeft(Operator{}).withRight(Operator{}))
	{
		opts := opts.withPriority(r).withLeft(*op)
		opts.maxDepth--
		if opts.maxDepth == 0 {
			_ = h.writeAtom(&ew, atomEllipsis, opts)
		} else {
			a, _ := arg(0)
			if err := h.Write(&ew, a, opts); err != nil {
				return err
			}
		}
	}
	if openClose {
		_, _ = fmt.Fprint(&ew, ")")
	}
	return ew.err
}

func (h *Heap) writeCompoundOpPostfix(w io.Writer, id Term, opts *WriteOptions, op *Operator) error {
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
	f, arg, _ := h.Compound(id)
	{
		opts := opts.withPriority(l).withRight(*op)
		opts.maxDepth--
		if opts.maxDepth == 0 {
			_ = h.writeAtom(&ew, atomEllipsis, opts)
		} else {
			a, _ := arg(0)
			if err := h.Write(&ew, a, opts); err != nil {
				return err
			}
		}
	}
	_ = h.writeAtom(&ew, f.Name, opts.withLeft(Operator{}).withRight(Operator{}))
	if openClose {
		_, _ = fmt.Fprint(&ew, ")")
	} else if opts.right != (Operator{}) {
		_, _ = fmt.Fprint(&ew, " ")
	}
	return ew.err
}

func (h *Heap) writeCompoundOpInfix(w io.Writer, id Term, opts *WriteOptions, op *Operator) error {
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
	f, arg, _ := h.Compound(id)
	{
		opts := opts.withPriority(l).withRight(*op)
		opts.maxDepth--
		if opts.maxDepth == 0 {
			_ = h.writeAtom(&ew, atomEllipsis, opts)
		} else {
			l, _ := arg(0)
			if err := h.Write(&ew, l, opts); err != nil {
				return err
			}
		}
	}
	switch f.Name {
	case Atom('.'), Atom('|'):
		_, _ = fmt.Fprint(&ew, f.Name.String())
	default:
		_ = h.writeAtom(&ew, f.Name, opts.withLeft(Operator{}).withRight(Operator{}))
	}
	{
		opts := opts.withPriority(r).withLeft(*op)
		opts.maxDepth--
		if opts.maxDepth == 0 {
			_ = h.writeAtom(&ew, atomEllipsis, opts)
		} else {
			r, _ := arg(1)
			if err := h.Write(&ew, r, opts); err != nil {
				return err
			}
		}
	}
	if openClose {
		_, _ = fmt.Fprint(&ew, ")")
	}
	return ew.err
}

func (h *Heap) writeCompoundFunctionalNotation(w io.Writer, id Term, opts *WriteOptions) error {
	ew := errWriter{w: w}
	opts = opts.withRight(Operator{})
	f, arg, _ := h.Compound(id)
	_ = h.writeAtom(&ew, f.Name, opts)
	_, _ = fmt.Fprint(&ew, "(")
	opts = opts.withLeft(Operator{}).withPriority(999)
	opts.maxDepth--
	for i := 0; i < f.Arity; i++ {
		if i != 0 {
			_, _ = fmt.Fprint(&ew, ",")
		}
		if opts.maxDepth == 0 {
			_ = h.writeAtom(&ew, atomEllipsis, opts)
			continue
		}
		a, _ := arg(i)
		if err := h.Write(&ew, a, opts); err != nil {
			return err
		}
	}
	_, _ = fmt.Fprint(&ew, ")")
	return ew.err
}

// Compare compares two Terms.
func (h *Heap) Compare(x, y Term) (int, error) {
	x, y = h.Resolve(x), h.Resolve(y)

	if x, ok := h.Variable(x); ok {
		if y, ok := h.Variable(y); ok {
			return int(x) - int(y), nil
		}

		return -1, nil
	}

	if x, ok := h.Float(x); ok {
		if _, ok := h.Variable(y); ok {
			return 1, nil
		}

		if y, ok := h.Float(y); ok {
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

	if x, ok := h.Integer(x); ok {
		if _, ok := h.Variable(y); ok {
			return 1, nil
		}

		if _, ok := h.Float(y); ok {
			return 1, nil
		}

		if y, ok := h.Integer(y); ok {
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

	if x, ok := h.Atom(x); ok {
		if _, ok := h.Variable(y); ok {
			return 1, nil
		}

		if _, ok := h.Float(y); ok {
			return 1, nil
		}

		if _, ok := h.Integer(y); ok {
			return 1, nil
		}

		if y, ok := h.Atom(y); ok {
			return strings.Compare(x.String(), y.String()), nil
		}

		return -1, nil
	}

	fx, argX, _ := h.Compound(x)
	fy, argY, ok := h.Compound(y)
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

		o, err := h.Compare(ax, ay)
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

func (h *Heap) MustBeAtom(id Term) (Atom, error) {
	id = h.Resolve(id)

	if _, ok := h.Variable(id); ok {
		return 0, ErrInstantiation
	}

	a, ok := h.Atom(id)
	if !ok {
		return 0, &TypeError{Type: NewAtom("atom"), Culprit: id}
	}

	return a, nil
}

func (h *Heap) CanBeAtom(id Term) error {
	id = h.Resolve(id)

	if _, ok := h.Variable(id); ok {
		return nil
	}

	if _, ok := h.Atom(id); !ok {
		return &TypeError{Type: NewAtom("atom"), Culprit: id}
	}

	return nil
}

func (h *Heap) MustBeInteger(id Term) (int64, error) {
	id = h.Resolve(id)

	if _, ok := h.Variable(id); ok {
		return 0, ErrInstantiation
	}

	i, ok := h.Integer(id)
	if !ok {
		return 0, &TypeError{Type: NewAtom("integer"), Culprit: id}
	}

	return i, nil
}

func (h *Heap) CanBeInteger(id Term) error {
	id = h.Resolve(id)

	if _, ok := h.Variable(id); ok {
		return nil
	}

	if _, ok := h.Integer(id); !ok {
		return &TypeError{Type: NewAtom("integer"), Culprit: id}
	}

	return nil
}

func (h *Heap) MustBeOperatorPriority(id Term) (int16, error) {
	id = h.Resolve(id)

	if _, ok := h.Variable(id); ok {
		return 0, ErrInstantiation
	}

	p, ok := h.Integer(id)
	if !ok {
		return 0, &TypeError{Type: NewAtom("integer"), Culprit: id}
	}
	if p < 0 || p > 1200 {
		return 0, &DomainError{Domain: NewAtom("operator_priority"), Culprit: id}
	}

	return int16(p), nil
}

func (h *Heap) CanBeOperatorPriority(id Term) error {
	id = h.Resolve(id)

	if _, ok := h.Variable(id); ok {
		return nil
	}

	p, ok := h.Integer(id)
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

func (h *Heap) PutOperatorSpecifier(spec OperatorSpecifier) (Term, error) {
	return h.PutAtom(revOperatorSpecifiers[spec])
}

func (h *Heap) MustBeOperatorSpecifier(id Term) (OperatorSpecifier, error) {
	id = h.Resolve(id)

	if _, ok := h.Variable(id); ok {
		return 0, ErrInstantiation
	}
	s, ok := h.Atom(id)
	if !ok {
		return 0, &TypeError{Type: NewAtom("atom"), Culprit: id}
	}
	spec, ok := operatorSpecifiers[s]
	if !ok {
		return 0, &DomainError{Domain: NewAtom("operator_specifier"), Culprit: id}
	}
	return spec, nil
}

func (h *Heap) CanBeOperatorSpecifier(id Term) error {
	id = h.Resolve(id)

	if _, ok := h.Variable(id); ok {
		return nil
	}

	a, ok := h.Atom(id)
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

func (h *Heap) MustBeOrder(id Term) (Atom, error) {
	id = h.Resolve(id)

	if _, ok := h.Variable(id); ok {
		return 0, ErrInstantiation
	}

	a, ok := h.Atom(id)
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

func (h *Heap) CanBeOrder(id Term) error {
	id = h.Resolve(id)

	if _, ok := h.Variable(id); ok {
		return nil
	}

	a, ok := h.Atom(id)
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

func (h *Heap) MustBeCallable(id Term) (Functor, func(int) (Term, error), error) {
	id = h.Resolve(id)

	if _, ok := h.Variable(id); ok {
		return Functor{}, nil, ErrInstantiation
	}

	f, arg, ok := h.Callable(id)
	if !ok {
		return Functor{}, nil, &DomainError{Domain: NewAtom("callable"), Culprit: id}
	}

	return f, arg, nil
}

func (h *Heap) CanBeCallable(id Term) error {
	id = h.Resolve(id)

	if _, ok := h.Variable(id); ok {
		return nil
	}

	if _, _, ok := h.Callable(id); !ok {
		return &DomainError{Domain: NewAtom("callable"), Culprit: id}
	}

	return nil
}

func (h *Heap) MustBeList(id Term) iter.Seq2[Term, error] {
	return func(yield func(Term, error) bool) {
		iter := ListIterator{TermPool: h, List: id}
		for iter.Next() {
			yield(iter.Current(), nil)
		}
		yield(0, iter.Err())
	}
}

func (h *Heap) CanBeList(id Term) iter.Seq2[Term, error] {
	return func(yield func(Term, error) bool) {
		iter := ListIterator{TermPool: h, List: id, AllowPartial: true}
		for iter.Next() {
			yield(iter.Current(), nil)
		}
		yield(0, iter.Err())
	}
}

func (h *Heap) MustBePair(id Term) (Term, Term, error) {
	id = h.Resolve(id)

	if _, ok := h.Variable(id); ok {
		return 0, 0, ErrInstantiation
	}

	f, arg, ok := h.Compound(id)
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

func (h *Heap) CanBePair(id Term) error {
	id = h.Resolve(id)

	if _, ok := h.Variable(id); ok {
		return nil
	}

	f, _, ok := h.Compound(id)
	if !ok || f != (Functor{Name: Atom('-'), Arity: 2}) {
		return &TypeError{Type: NewAtom("pair"), Culprit: id}
	}

	return nil
}
