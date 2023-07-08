package engine

import (
	"fmt"
	"io"
	"sort"
	"strconv"
	"strings"
	"unicode/utf8"
)

// Compound is a Prolog compound.
type Compound interface {
	Term
	Functor() Atom
	Arity() int
	Arg(n int) Term
}

// WriteCompound outputs the Compound to an io.Writer.
func WriteCompound(w io.Writer, c Compound, opts *WriteOptions, env *Env) error {
	ok, err := writeCompoundVisit(w, c, opts)
	if err != nil || ok {
		return err
	}

	opts = opts.withVisited(c)

	a := env.Resolve(c.Arg(0))
	if n, ok := a.(Integer); ok && opts.numberVars && c.Functor() == atomVar && c.Arity() == 1 && n >= 0 {
		return writeCompoundNumberVars(w, n)
	}

	if !opts.ignoreOps {
		if c.Functor() == atomDot && c.Arity() == 2 {
			return writeCompoundList(w, c, opts, env)
		}

		if c.Functor() == atomEmptyBlock && c.Arity() == 1 {
			return writeCompoundCurlyBracketed(w, c, opts, env)
		}
	}

	if opts.ignoreOps {
		return writeCompoundFunctionalNotation(w, c, opts, env)
	}

	for _, o := range opts.ops[c.Functor()] {
		if o.specifier.arity() == c.Arity() {
			return writeCompoundOp(w, c, opts, env, &o)
		}
	}

	return writeCompoundFunctionalNotation(w, c, opts, env)
}

func writeCompoundVisit(w io.Writer, c Compound, opts *WriteOptions) (bool, error) {
	if _, ok := opts.visited[id(c)]; ok {
		err := atomElipsis.WriteTerm(w, opts, nil)
		return true, err
	}
	return false, nil
}

func writeCompoundNumberVars(w io.Writer, n Integer) error {
	const letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	ew := errWriter{w: w}
	i, j := int(n)%len(letters), int(n)/len(letters)
	_, _ = fmt.Fprint(&ew, string(letters[i]))
	if j != 0 {
		_, _ = fmt.Fprint(&ew, strconv.Itoa(j))
	}
	return ew.err
}

func writeCompoundList(w io.Writer, c Compound, opts *WriteOptions, env *Env) error {
	ew := errWriter{w: w}
	opts = opts.withPriority(999).withLeft(operator{}).withRight(operator{})
	_, _ = fmt.Fprint(&ew, "[")
	_ = c.Arg(0).WriteTerm(&ew, opts, env)
	iter := ListIterator{List: c.Arg(1), Env: env}
	for iter.Next() {
		_, _ = fmt.Fprint(&ew, ",")
		_ = iter.Current().WriteTerm(&ew, opts, env)
	}
	if err := iter.Err(); err != nil {
		_, _ = fmt.Fprint(&ew, "|")
		s := iter.Suffix()
		if l, ok := iter.Suffix().(Compound); ok && l.Functor() == atomDot && l.Arity() == 2 {
			_ = atomElipsis.WriteTerm(&ew, opts, nil)
		} else {
			_ = s.WriteTerm(&ew, opts, env)
		}
	}
	_, _ = fmt.Fprint(&ew, "]")
	return ew.err
}

func writeCompoundCurlyBracketed(w io.Writer, c Compound, opts *WriteOptions, env *Env) error {
	ew := errWriter{w: w}
	_, _ = fmt.Fprint(&ew, "{")
	_ = c.Arg(0).WriteTerm(&ew, opts.withLeft(operator{}), env)
	_, _ = fmt.Fprint(&ew, "}")
	return ew.err
}

var writeCompoundOps = [...]func(w io.Writer, c Compound, opts *WriteOptions, env *Env, op *operator) error{
	operatorSpecifierFX:  nil,
	operatorSpecifierFY:  nil,
	operatorSpecifierXF:  nil,
	operatorSpecifierYF:  nil,
	operatorSpecifierXFX: nil,
	operatorSpecifierXFY: nil,
	operatorSpecifierYFX: nil,
}

func init() {
	writeCompoundOps = [len(writeCompoundOps)]func(w io.Writer, c Compound, opts *WriteOptions, env *Env, op *operator) error{
		operatorSpecifierFX:  writeCompoundOpPrefix,
		operatorSpecifierFY:  writeCompoundOpPrefix,
		operatorSpecifierXF:  writeCompoundOpPostfix,
		operatorSpecifierYF:  writeCompoundOpPostfix,
		operatorSpecifierXFX: writeCompoundOpInfix,
		operatorSpecifierXFY: writeCompoundOpInfix,
		operatorSpecifierYFX: writeCompoundOpInfix,
	}
}

func writeCompoundOp(w io.Writer, c Compound, opts *WriteOptions, env *Env, op *operator) error {
	return writeCompoundOps[op.specifier](w, c, opts, env, op)
}

func writeCompoundOpPrefix(w io.Writer, c Compound, opts *WriteOptions, env *Env, op *operator) error {
	ew := errWriter{w: w}
	_, r := op.bindingPriorities()
	openClose := opts.priority < op.priority || (opts.right != operator{} && r >= opts.right.priority)

	if opts.left != (operator{}) {
		_, _ = fmt.Fprint(&ew, " ")
	}
	if openClose {
		_, _ = fmt.Fprint(&ew, "(")
		opts = opts.withLeft(operator{}).withRight(operator{})
	}
	_ = c.Functor().WriteTerm(&ew, opts.withLeft(operator{}).withRight(operator{}), env)
	_ = c.Arg(0).WriteTerm(&ew, opts.withPriority(r).withLeft(*op), env)
	if openClose {
		_, _ = fmt.Fprint(&ew, ")")
	}
	return ew.err
}

func writeCompoundOpPostfix(w io.Writer, c Compound, opts *WriteOptions, env *Env, op *operator) error {
	ew := errWriter{w: w}
	l, _ := op.bindingPriorities()
	openClose := opts.priority < op.priority || (opts.left.name == atomMinus && opts.left.specifier.class() == operatorClassPrefix)

	if openClose {
		if opts.left != (operator{}) {
			_, _ = fmt.Fprint(&ew, " ")
		}
		_, _ = fmt.Fprint(&ew, "(")
		opts = opts.withLeft(operator{}).withRight(operator{})
	}
	_ = c.Arg(0).WriteTerm(&ew, opts.withPriority(l).withRight(*op), env)
	_ = c.Functor().WriteTerm(&ew, opts.withLeft(operator{}).withRight(operator{}), env)
	if openClose {
		_, _ = fmt.Fprint(&ew, ")")
	} else if opts.right != (operator{}) {
		_, _ = fmt.Fprint(&ew, " ")
	}
	return ew.err
}

func writeCompoundOpInfix(w io.Writer, c Compound, opts *WriteOptions, env *Env, op *operator) error {
	ew := errWriter{w: w}
	l, r := op.bindingPriorities()
	openClose := opts.priority < op.priority ||
		(opts.left.name == atomMinus && opts.left.specifier.class() == operatorClassPrefix) ||
		(opts.right != operator{} && r >= opts.right.priority)

	if openClose {
		if opts.left.name != 0 && opts.left.specifier.class() == operatorClassPrefix {
			_, _ = fmt.Fprint(&ew, " ")
		}
		_, _ = fmt.Fprint(&ew, "(")
		opts = opts.withLeft(operator{}).withRight(operator{})
	}
	_ = c.Arg(0).WriteTerm(&ew, opts.withPriority(l).withRight(*op), env)
	switch c.Functor() {
	case atomComma, atomBar:
		_, _ = fmt.Fprint(&ew, c.Functor().String())
	default:
		_ = c.Functor().WriteTerm(&ew, opts.withLeft(operator{}).withRight(operator{}), env)
	}
	_ = c.Arg(1).WriteTerm(&ew, opts.withPriority(r).withLeft(*op), env)
	if openClose {
		_, _ = fmt.Fprint(&ew, ")")
	}
	return ew.err
}

func writeCompoundFunctionalNotation(w io.Writer, c Compound, opts *WriteOptions, env *Env) error {
	ew := errWriter{w: w}
	opts = opts.withRight(operator{})
	_ = c.Functor().WriteTerm(&ew, opts, env)
	_, _ = fmt.Fprint(&ew, "(")
	opts = opts.withLeft(operator{}).withPriority(999)
	for i := 0; i < c.Arity(); i++ {
		if i != 0 {
			_, _ = fmt.Fprint(&ew, ",")
		}
		_ = c.Arg(i).WriteTerm(&ew, opts, env)
	}
	_, _ = fmt.Fprint(&ew, ")")
	return ew.err
}

// CompareCompound compares the Compound with a Term.
func CompareCompound(c Compound, t Term, env *Env) int {
	switch t := env.Resolve(t).(type) {
	case Compound:
		switch x, y := c.Arity(), t.Arity(); {
		case x > y:
			return 1
		case x < y:
			return -1
		}

		if o := c.Functor().Compare(t.Functor(), env); o != 0 {
			return o
		}

		for i := 0; i < c.Arity(); i++ {
			if o := c.Arg(i).Compare(t.Arg(i), env); o != 0 {
				return o
			}
		}
		return 0
	default:
		return 1
	}
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

type compound struct {
	functor Atom
	args    []Term
}

func (c *compound) WriteTerm(w io.Writer, opts *WriteOptions, env *Env) error {
	return WriteCompound(w, c, opts, env)
}

func (c *compound) Compare(t Term, env *Env) int {
	return CompareCompound(c, t, env)
}

func (c *compound) Functor() Atom {
	return c.functor
}

func (c *compound) Arity() int {
	return len(c.args)
}

func (c *compound) Arg(n int) Term {
	return c.args[n]
}

func (c *compound) GoString() string {
	return fmt.Sprintf(`&engine.compound{functor:%#v, args:%#v}`, c.functor, c.args)
}

// Cons returns a list consists of a first element car and the rest cdr.
func Cons(car, cdr Term) Term {
	return atomDot.Apply(car, cdr)
}

type list []Term

func (l list) WriteTerm(w io.Writer, opts *WriteOptions, env *Env) error {
	return WriteCompound(w, l, opts, env)
}

func (l list) Compare(t Term, env *Env) int {
	return CompareCompound(l, t, env)
}

func (l list) termID() termID { // Slices are not comparable.
	type listID struct {
		len  int
		head *Term
	}
	id := listID{
		len: len(l),
	}
	if len(l) > 0 {
		id.head = &l[0]
	}
	return id
}

func (l list) Functor() Atom {
	return atomDot
}

func (l list) Arity() int {
	return 2
}

func (l list) Arg(n int) Term {
	var t Term
	switch n {
	case 0:
		t = l[0]
	case 1:
		if len(l) == 1 {
			t = atomEmptyList
			break
		}
		t = l[1:]
	}
	return t
}

func (l list) GoString() string {
	var sb strings.Builder
	_, _ = sb.WriteString(`engine.list{`)
	for i, e := range l {
		if i != 0 {
			_, _ = sb.WriteString(`, `)
		}
		_, _ = fmt.Fprintf(&sb, "%#v", e)
	}
	_, _ = sb.WriteString(`}`)
	return sb.String()
}

// List returns a list of ts.
func List(ts ...Term) Term {
	if len(ts) == 0 {
		return atomEmptyList
	}
	return list(ts)
}

type partial struct {
	Compound
	tail *Term
}

func (p *partial) WriteTerm(w io.Writer, opts *WriteOptions, env *Env) error {
	return WriteCompound(w, p, opts, env)
}

func (p *partial) Compare(t Term, env *Env) int {
	return CompareCompound(p, t, env)
}

func (p *partial) termID() termID { // The underlying compound might not be comparable.
	type partialID struct {
		prefixID, tailID termID
	}
	return partialID{
		prefixID: id(p.Compound),
		tailID:   p.tail,
	}
}

func (p *partial) Arg(n int) Term {
	t := p.Compound.Arg(n)
	if c := p.Compound; c.Functor() == atomDot && c.Arity() == 2 && n == 1 {
		if t == atomEmptyList {
			t = *p.tail
		} else {
			t = &partial{Compound: t.(Compound), tail: p.tail}
		}
	}
	return t
}

func (p *partial) GoString() string {
	return fmt.Sprintf(`engine.partial{Compound:%#v, tail:%#v}`, p.Compound, *p.tail)
}

// PartialList returns a list of ts followed by tail.
func PartialList(tail Term, ts ...Term) Term {
	if len(ts) == 0 {
		return tail
	}
	return &partial{
		Compound: list(ts),
		tail:     &tail,
	}
}

// set returns a list of ts which elements are unique.
func (e *Env) set(ts ...Term) Term {
	sort.Slice(ts, func(i, j int) bool {
		return ts[i].Compare(ts[j], e) == -1
	})
	us := make([]Term, 0, len(ts))
	for _, t := range ts {
		if len(us) > 0 && us[len(us)-1].Compare(t, e) == 0 {
			continue
		}
		us = append(us, t)
	}
	return List(us...)
}

// slice returns a Term slice containing the elements of list.
// It errors if the given Term is not a list.
func slice(list Term, env *Env) ([]Term, error) {
	var ret []Term
	iter := ListIterator{List: list, Env: env}
	for iter.Next() {
		ret = append(ret, env.Resolve(iter.Current()))
	}
	return ret, iter.Err()
}

// seq returns a sequence of ts separated by sep.
func seq(sep Atom, ts ...Term) Term {
	s, ts := ts[len(ts)-1], ts[:len(ts)-1]
	for i := len(ts) - 1; i >= 0; i-- {
		s = sep.Apply(ts[i], s)
	}
	return s
}

func pair(k, v Term) Term {
	return atomMinus.Apply(k, v)
}

func tuple(args ...Term) Term {
	return Atom(0).Apply(args...)
}

type charList string

func (c charList) String() string {
	return string(c)
}

func (c charList) WriteTerm(w io.Writer, opts *WriteOptions, env *Env) error {
	return WriteCompound(w, c, opts, env)
}

func (c charList) Compare(t Term, env *Env) int {
	return CompareCompound(c, t, env)
}

func (c charList) Functor() Atom {
	return atomDot
}

func (c charList) Arity() int {
	return 2
}

func (c charList) Arg(n int) Term {
	r, i := utf8.DecodeRuneInString(string(c))
	var t Term
	switch n {
	case 0:
		t = Atom(r)
	case 1:
		if i == len(c) {
			t = atomEmptyList
		} else {
			t = c[i:]
		}
	}
	return t
}

// CharList returns a character list.
func CharList(s string) Term {
	if s == "" {
		return atomEmptyList
	}
	return charList(s)
}

type codeList string

func (c codeList) String() string {
	return string(c)
}

func (c codeList) WriteTerm(w io.Writer, opts *WriteOptions, env *Env) error {
	return WriteCompound(w, c, opts, env)
}

func (c codeList) Compare(t Term, env *Env) int {
	return CompareCompound(c, t, env)
}

func (c codeList) Functor() Atom {
	return atomDot
}

func (c codeList) Arity() int {
	return 2
}

func (c codeList) Arg(n int) Term {
	r, i := utf8.DecodeRuneInString(string(c))
	var t Term
	switch n {
	case 0:
		t = Integer(r)
	case 1:
		if i == len(c) {
			t = atomEmptyList
		} else {
			t = c[i:]
		}
	}
	return t
}

// CodeList returns a character code list.
func CodeList(s string) Term {
	if s == "" {
		return atomEmptyList
	}
	return codeList(s)
}
