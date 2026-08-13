package syntax

import (
	"errors"
	"fmt"
	"io"
	"iter"
	"regexp"
	"slices"
	"strconv"
	"strings"
	"unicode/utf8"

	"github.com/ichiban/prolog/v2/internal/term"
)

var (
	atomBar       = term.NewAtomRune('|')
	atomComma     = term.NewAtomRune(',')
	atomEllipses  = term.NewAtom("...")
	atomSmallE    = term.NewAtomRune('e')
	atomLargeE    = term.NewAtomRune('E')
	atomPeriod    = term.NewAtomRune('.')
	atomNumberVar = term.NewAtom("$VAR")
)

var (
	functorCons      = term.NewFunctor(atomPeriod, 2)
	functorNumberVar = term.NewFunctor(atomNumberVar, 1)
	functorBlock     = term.NewFunctor(atomEmptyBlock, 1)
)

type Formatter struct {
	Arena *term.Arena
	Term  term.Handle

	IgnoreOps     bool
	Quoted        bool
	VariableNames []term.VariableName
	NumberVars    bool

	Ops       *OperatorSet
	MaxDepth  int
	Precision int
}

func (f *Formatter) Format(s fmt.State, verb rune) {
	c := *f
	c.Quoted = c.Quoted || verb == 'q'
	c.IgnoreOps = c.IgnoreOps || s.Flag('-')
	c.NumberVars = c.NumberVars || s.Flag('#')

	if w, ok := s.Width(); ok {
		c.MaxDepth = w
	} else {
		c.MaxDepth = 10
	}

	if p, ok := s.Precision(); ok {
		c.Precision = p
	} else {
		c.Precision = -1
	}

	_, _ = c.WriteTo(s)
}

func (f *Formatter) WriteTo(w io.Writer) (int64, error) {
	if f.Ops == nil {
		f.Ops = NewOperatorSet()
	}
	fm := formatter{
		Formatter: *f,
		priority:  1201,
	}
	return fm.writeTerm(w, f.Term)
}

type formatter struct {
	Formatter
	priority    int
	visited     map[term.Handle]struct{}
	prefixMinus bool
	left, right operator
	depth       int
}

func (f formatter) writeTerm(w io.Writer, t term.Handle) (int64, error) {
	arena := f.Arena
	t = arena.Deref(t)

	if t == (term.Handle{}) {
		return 0, errors.New("invalid term")
	}

	if _, ok := f.visited[t]; ok || (f.MaxDepth > 0 && f.depth > f.MaxDepth) {
		return f.writeAtom(w, atomEllipses)
	}

	if _, ok := arena.Variable(t); ok {
		return f.writeVariable(w, t)
	}

	if name, ok := arena.Atom(t); ok {
		return f.writeAtom(w, name)
	}

	if i, ok := arena.Integer(t); ok {
		return f.writeInteger(w, i)
	}

	if fl, ok := arena.Float(t); ok {
		return f.writeFloat(w, fl)
	}

	if s, ok := arena.Stream(t); ok {
		id, err := arena.PutInteger(int64(slices.Index(arena.Streams, *s)))
		if err != nil {
			return 0, err
		}
		return f.writeCompoundFunctionalNotation(w, term.NewAtom("$stream"), singleton(id))
	}

	if f.visited == nil {
		f.visited = map[term.Handle]struct{}{}
	}
	f.visited[t] = struct{}{}

	return f.writeCompound(w, t)
}

func (f formatter) writeVariable(w io.Writer, v term.Handle) (int64, error) {
	arena := f.Arena
	ew := errWriter{w: w}
	if letterDigit(f.left.name) {
		_, _ = fmt.Fprint(&ew, " ")
	}
	if i := slices.IndexFunc(f.VariableNames, func(vn term.VariableName) bool {
		return vn.Variable == v
	}); i >= 0 {
		vn := f.VariableNames[i]
		f.Quoted = false
		_, _ = f.writeAtom(&ew, term.NewAtom(vn.Name))
	} else {
		addr, _ := arena.Variable(v)
		_, _ = fmt.Fprintf(&ew, "_%d", addr)
	}
	if letterDigit(f.right.name) {
		_, _ = fmt.Fprint(&ew, " ")
	}
	return ew.Result()
}

func (f formatter) writeAtom(w io.Writer, name term.Atom) (int64, error) {
	ew := errWriter{w: w}
	openClose := (f.left != (operator{}) || f.right != (operator{})) && f.Ops.defined(name)

	if openClose {
		if f.left.name != (term.Atom{}) && f.left.specifier.class() == operatorClassPrefix {
			_, _ = fmt.Fprint(&ew, " ")
		}
		_, _ = fmt.Fprint(&ew, "(")
		f.left, f.right = operator{}, operator{}
	}

	if f.Quoted && needQuoted(name) {
		if f.left != (operator{}) && needQuoted(f.left.name) { // Avoid 'FOO''BAR'.
			_, _ = fmt.Fprint(&ew, " ")
		}
		_, _ = ew.Write([]byte(quote(name)))
		if f.right != (operator{}) && needQuoted(f.right.name) { // Avoid 'FOO''BAR'.
			_, _ = fmt.Fprint(&ew, " ")
		}
	} else {
		if (letterDigit(f.left.name) && letterDigit(name)) || (graphic(f.left.name) && graphic(name)) {
			_, _ = fmt.Fprint(&ew, " ")
		}
		_, _ = fmt.Fprint(&ew, name)
		if (letterDigit(f.right.name) && letterDigit(name)) || (graphic(f.right.name) && graphic(name)) {
			_, _ = fmt.Fprint(&ew, " ")
		}
	}

	if openClose {
		_, _ = fmt.Fprint(&ew, ")")
	}

	return ew.Result()
}

func needQuoted(name term.Atom) bool {
	p := newParser(name.String())
	parsed, ok, err := p.atom()
	return err != nil || !ok || parsed != name
}

var (
	quotedAtomEscapePattern = regexp.MustCompile(`[[:cntrl:]]|\\|'`)
)

func quotedIdentEscape(s string) string {
	switch s {
	case "\a":
		return `\a`
	case "\b":
		return `\b`
	case "\f":
		return `\f`
	case "\n":
		return `\n`
	case "\r":
		return `\r`
	case "\t":
		return `\t`
	case "\v":
		return `\v`
	case `\`:
		return `\\`
	case `'`:
		return `\'`
	default:
		var ret []string
		for _, r := range s {
			ret = append(ret, fmt.Sprintf(`\x%x\`, r))
		}
		return strings.Join(ret, "")
	}
}

func quote(s term.Atom) string {
	return fmt.Sprintf("'%s'", quotedAtomEscapePattern.ReplaceAllStringFunc(s.String(), quotedIdentEscape))
}

func letterDigit(a term.Atom) bool {
	r, _ := utf8.DecodeRuneInString(a.String())
	return r != utf8.RuneError && isSmallLetterChar(r)
}

func graphic(s term.Atom) bool {
	r, _ := utf8.DecodeRuneInString(s.String())
	return r != utf8.RuneError && (isGraphicChar(r) || r == '\\')
}

func (f formatter) writeInteger(w io.Writer, i int64) (int64, error) {
	ew := errWriter{w: w}
	openClose := f.left.name == atomMinus && f.left.specifier.class() == operatorClassPrefix && i > 0

	if openClose {
		_, _ = ew.Write([]byte(" ("))
		f.left = operator{}
		f.right = operator{}
	} else {
		if f.left != (operator{}) && (letterDigit(f.left.name) || (i < 0 && graphic(f.left.name))) {
			_, _ = ew.Write([]byte(" "))
		}
	}

	s := strconv.FormatInt(i, 10)
	_, _ = ew.Write([]byte(s))

	if openClose {
		_, _ = ew.Write([]byte(")"))
	}

	// Avoid ambiguous 0b, 0o, 0x or 0'.
	if !openClose && f.right != (operator{}) && (letterDigit(f.right.name) || (needQuoted(f.right.name) && f.right.name != atomComma && f.right.name != atomBar)) {
		_, _ = ew.Write([]byte(" "))
	}

	return ew.Result()
}

func (f formatter) writeFloat(w io.Writer, fl float64) (int64, error) {
	ew := errWriter{w: w}
	openClose := f.left.name == atomMinus && f.left.specifier.class() == operatorClassPrefix && fl > 0

	if openClose || (fl < 0 && f.left != operator{}) {
		_, _ = ew.Write([]byte(" "))
	}

	if openClose {
		_, _ = ew.Write([]byte("("))
	}

	s := strconv.FormatFloat(fl, 'g', f.Precision, 64)
	if !strings.ContainsRune(s, '.') {
		if strings.ContainsRune(s, 'e') {
			s = strings.Replace(s, "e", ".0e", 1)
		} else {
			s += ".0"
		}
	}
	_, _ = ew.Write([]byte(s))

	if openClose {
		_, _ = ew.Write([]byte(")"))
	}

	if !openClose && f.right != (operator{}) && (f.right.name == atomSmallE || f.right.name == atomLargeE) {
		_, _ = ew.Write([]byte(" "))
	}

	return ew.Result()
}

func (f formatter) writeCompound(w io.Writer, t term.Handle) (int64, error) {
	arena := f.Arena
	fn, _ := arena.Functor(t)
	if fn == functorNumberVar && f.NumberVars {
		a := arena.Arg(t, 0)
		if n, ok := arena.Integer(a); ok {
			return f.writeCompoundNumberVars(w, n)
		}
	}

	if !f.IgnoreOps {
		switch fn {
		case functorCons:
			return f.writeCompoundList(w, t)
		case functorBlock:
			return f.writeCompoundCurlyBracketed(w, t)
		}

		ops := f.Ops.ops
		switch fn.Arity() {
		case 1:
			if op, ok := ops[opKey{name: fn.Name(), opClass: operatorClassPrefix}]; ok {
				return f.writeCompoundOpPrefix(w, fn.Name(), arena.Arg(t, 0), &op)
			}
			if op, ok := ops[opKey{name: fn.Name(), opClass: operatorClassPostfix}]; ok {
				return f.writeCompoundOpPostfix(w, fn.Name(), arena.Arg(t, 0), &op)
			}
		case 2:
			if op, ok := ops[opKey{name: fn.Name(), opClass: operatorClassInfix}]; ok {
				return f.writeCompoundOpInfix(w, arena.Arg(t, 0), fn.Name(), arena.Arg(t, 1), &op)
			}
		}
	}

	return f.writeCompoundFunctionalNotation(w, fn.Name(), arena.Args(t))
}

func (f formatter) writeCompoundNumberVars(w io.Writer, n int64) (int64, error) {
	const letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	ew := errWriter{w: w}
	i, j := int(n)%len(letters), int(n)/len(letters)
	_, _ = fmt.Fprint(&ew, string(letters[i]))
	if j != 0 {
		_, _ = fmt.Fprint(&ew, strconv.Itoa(j))
	}
	return ew.Result()
}

func (f formatter) writeCompoundList(w io.Writer, t term.Handle) (int64, error) {
	arena := f.Arena
	ew := errWriter{w: w}
	f.priority = 999
	f.left = operator{}
	f.right = operator{}
	_, _ = fmt.Fprint(&ew, "[")
	var (
		car = arena.Arg(t, 0)
		cdr = arena.Arg(t, 1)
	)
	_, _ = f.writeTerm(&ew, car)
	for elem, ok := range arena.List(cdr, term.AllowCycle(f.MaxDepth > f.depth)) {
		if !ok {
			_, _ = fmt.Fprint(&ew, "|")
			if fn, ok := arena.Functor(elem); ok && fn == functorCons {
				_, _ = f.writeAtom(&ew, atomEllipses)
			} else {
				_, _ = f.writeTerm(&ew, elem)
			}
			break
		}

		f.depth++
		_, _ = fmt.Fprint(&ew, ",")
		_, _ = f.writeTerm(&ew, elem)
	}
	_, _ = fmt.Fprint(&ew, "]")
	return ew.Result()
}

func (f formatter) writeCompoundCurlyBracketed(w io.Writer, t term.Handle) (int64, error) {
	arena := f.Arena
	ew := errWriter{w: w}
	f.left = operator{}
	_, _ = fmt.Fprint(&ew, "{")
	_, _ = f.writeTerm(&ew, arena.Arg(t, 0))
	_, _ = fmt.Fprint(&ew, "}")
	return ew.Result()
}

func (f formatter) writeCompoundOpPrefix(w io.Writer, name term.Atom, arg term.Handle, op *operator) (int64, error) {
	ew := errWriter{w: w}
	_, r := op.bindingPriorities()
	openClose := f.priority < op.priority || (f.right != operator{} && r >= f.right.priority)

	if f.left != (operator{}) {
		_, _ = fmt.Fprint(&ew, " ")
	}
	if openClose {
		_, _ = fmt.Fprint(&ew, "(")
		f.left = operator{}
		f.right = operator{}
	}
	{
		f := f
		f.left = operator{}
		f.right = operator{}
		_, _ = f.writeAtom(&ew, name)
	}
	{
		f := f
		f.priority = r
		f.left = *op
		f.depth++
		_, _ = f.writeTerm(&ew, arg)
	}
	if openClose {
		_, _ = fmt.Fprint(&ew, ")")
	}
	return ew.Result()
}

func (f formatter) writeCompoundOpPostfix(w io.Writer, name term.Atom, arg term.Handle, op *operator) (int64, error) {
	ew := errWriter{w: w}
	l, _ := op.bindingPriorities()
	openClose := f.priority < op.priority || (f.left.name == atomMinus && f.left.specifier.class() == operatorClassPrefix)

	if openClose {
		if f.left != (operator{}) {
			_, _ = fmt.Fprint(&ew, " ")
		}
		_, _ = fmt.Fprint(&ew, "(")
		f.left = operator{}
		f.right = operator{}
	}
	{
		f := f
		f.priority = l
		f.right = *op
		f.depth++
		_, _ = f.writeTerm(&ew, arg)
	}
	{
		f := f
		f.left = operator{}
		f.right = operator{}
		_, _ = f.writeAtom(&ew, name)
	}
	if openClose {
		_, _ = fmt.Fprint(&ew, ")")
	} else if f.right != (operator{}) {
		_, _ = fmt.Fprint(&ew, " ")
	}
	return ew.Result()
}

func (f formatter) writeCompoundOpInfix(w io.Writer, left term.Handle, name term.Atom, right term.Handle, op *operator) (int64, error) {
	ew := errWriter{w: w}
	l, r := op.bindingPriorities()
	openClose := f.priority < op.priority ||
		(f.left.name == atomMinus && f.left.specifier.class() == operatorClassPrefix) ||
		(f.right != operator{} && r >= f.right.priority)

	if openClose {
		if f.left != (operator{}) && f.left.specifier.class() == operatorClassPrefix {
			_, _ = fmt.Fprint(&ew, " ")
		}
		_, _ = fmt.Fprint(&ew, "(")
		f.left = operator{}
		f.right = operator{}
	}
	{
		f := f
		f.priority = l
		f.right = *op
		f.depth++
		_, _ = f.writeTerm(&ew, left)
	}
	switch name {
	case atomComma, atomBar:
		_, _ = fmt.Fprint(&ew, name)
	default:
		f := f
		f.left = operator{}
		f.right = operator{}
		_, _ = f.writeAtom(&ew, name)
	}
	{
		f := f
		f.priority = r
		f.left = *op
		f.depth++
		_, _ = f.writeTerm(&ew, right)
	}
	if openClose {
		_, _ = fmt.Fprint(&ew, ")")
	}
	return ew.Result()
}

func (f formatter) writeCompoundFunctionalNotation(w io.Writer, name term.Atom, args iter.Seq[term.Handle]) (int64, error) {
	ew := errWriter{w: w}
	f.right = operator{}
	_, _ = f.writeAtom(&ew, name)
	_, _ = fmt.Fprint(&ew, "(")
	f.left = operator{}
	f.priority = 999
	f.depth++
	for i, a := range withIndex(args) {
		if i != 0 {
			_, _ = fmt.Fprint(&ew, ",")
		}
		_, _ = f.writeTerm(&ew, a)
	}
	_, _ = fmt.Fprint(&ew, ")")
	return ew.Result()
}

func withIndex[T any](seq iter.Seq[T]) iter.Seq2[int, T] {
	return func(yield func(int, T) bool) {
		var i int
		for v := range seq {
			if !yield(i, v) {
				return
			}
			i++
		}
	}
}

// https://go.dev/blog/errors-are-values
type errWriter struct {
	w   io.Writer
	n   int64
	err error
}

func (ew *errWriter) Write(p []byte) (int, error) {
	if ew.err != nil {
		return 0, nil
	}
	var n int
	n, ew.err = ew.w.Write(p)
	ew.n += int64(n)
	return n, nil
}

func (ew *errWriter) Result() (int64, error) {
	return ew.n, ew.err
}

func singleton[T any](e T) iter.Seq[T] {
	return func(yield func(T) bool) {
		_ = yield(e)
	}
}
