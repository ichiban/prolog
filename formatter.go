package prolog

import (
	"fmt"
	"io"
	"iter"
	"regexp"
	"strconv"
	"strings"
	"unicode/utf8"
)

type formatState struct {
	priority    int
	visited     map[Term]struct{}
	prefixMinus bool
	left, right operator
	depth       int
}

type Formatter struct {
	Engine *Engine
	Term   Term

	IgnoreOps    bool
	Quoted       bool
	VariableName map[Variable]Atom
	NumberVars   bool

	Ops       Operators
	MaxDepth  int
	Precision int
}

func (f *Formatter) Format(s fmt.State, verb rune) {
	c := *f
	c.Quoted = verb == 'q'
	c.IgnoreOps = s.Flag('-')
	c.NumberVars = s.Flag('#')

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
	state := formatState{
		priority: 1201,
	}
	return writeTerm(w, f.Engine, f.Term, f, state)
}

func writeTerm(w io.Writer, e *Engine, t Term, opts *Formatter, state formatState) (int64, error) {
	t = e.Deref(t)

	if _, ok := state.visited[t]; ok || (opts.MaxDepth > 0 && state.depth > opts.MaxDepth) {
		return writeAtom(w, "...", opts, state)
	}

	if v, err := e.Variable(t); err == nil {
		return writeVariable(w, v, opts, state)
	}

	if name, err := e.Atom(t); err == nil {
		return writeAtom(w, name, opts, state)
	}

	if i, err := e.Integer(t); err == nil {
		return writeInteger(w, i, opts, state)
	}

	if f, err := e.Float(t); err == nil {
		return writeFloat(w, f, opts, state)
	}

	if state.visited == nil {
		state.visited = map[Term]struct{}{}
	}
	state.visited[t] = struct{}{}

	return writeCompound(w, e, t, opts, state)
}

func writeVariable(w io.Writer, v Variable, opts *Formatter, state formatState) (int64, error) {
	ew := errWriter{w: w}
	if letterDigit(state.left.name) {
		_, _ = fmt.Fprint(&ew, " ")
	}
	if name, ok := opts.VariableName[v]; ok {
		f := *opts
		f.Quoted = false
		_, _ = writeAtom(&ew, name, opts, state)
	} else {
		_, _ = fmt.Fprintf(&ew, "_%d", v)
	}
	if letterDigit(state.right.name) {
		_, _ = fmt.Fprint(&ew, " ")
	}
	return ew.Result()
}

func writeAtom(w io.Writer, name Atom, opts *Formatter, state formatState) (int64, error) {
	ew := errWriter{w: w}
	openClose := (state.left != (operator{}) || state.right != (operator{})) && opts.Ops.defined(name)

	if openClose {
		if state.left.name != "" && state.left.specifier.class() == operatorClassPrefix {
			_, _ = fmt.Fprint(&ew, " ")
		}
		_, _ = fmt.Fprint(&ew, "(")
		state.left, state.right = operator{}, operator{}
	}

	if opts.Quoted && needQuoted(name) {
		if state.left != (operator{}) && needQuoted(state.left.name) { // Avoid 'FOO''BAR'.
			_, _ = fmt.Fprint(&ew, " ")
		}
		_, _ = ew.Write([]byte(quote(name)))
		if state.right != (operator{}) && needQuoted(state.right.name) { // Avoid 'FOO''BAR'.
			_, _ = fmt.Fprint(&ew, " ")
		}
	} else {
		if (letterDigit(state.left.name) && letterDigit(name)) || (graphic(state.left.name) && graphic(name)) {
			_, _ = fmt.Fprint(&ew, " ")
		}
		_, _ = fmt.Fprint(&ew, name)
		if (letterDigit(state.right.name) && letterDigit(name)) || (graphic(state.right.name) && graphic(name)) {
			_, _ = fmt.Fprint(&ew, " ")
		}
	}

	if openClose {
		_, _ = fmt.Fprint(&ew, ")")
	}

	return ew.Result()
}

func needQuoted(name Atom) bool {
	p := NewParser(strings.NewReader(string(name)), nil)
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

func quote(s Atom) string {
	return fmt.Sprintf("'%s'", quotedAtomEscapePattern.ReplaceAllStringFunc(string(s), quotedIdentEscape))
}

func letterDigit(s Atom) bool {
	return len(s) > 0 && isSmallLetterChar([]rune(s)[0])
}

func graphic(s Atom) bool {
	r, _ := utf8.DecodeRuneInString(string(s))
	return r != utf8.RuneError && (isGraphicChar(r) || r == '\\')
}

func writeInteger(w io.Writer, i int64, _ *Formatter, state formatState) (int64, error) {
	ew := errWriter{w: w}
	openClose := state.left.name == "-" && state.left.specifier.class() == operatorClassPrefix && i > 0

	if openClose {
		_, _ = ew.Write([]byte(" ("))
		state.left = operator{}
		state.right = operator{}
	} else {
		if state.left != (operator{}) && (letterDigit(state.left.name) || (i < 0 && graphic(state.left.name))) {
			_, _ = ew.Write([]byte(" "))
		}
	}

	s := strconv.FormatInt(i, 10)
	_, _ = ew.Write([]byte(s))

	if openClose {
		_, _ = ew.Write([]byte(")"))
	}

	// Avoid ambiguous 0b, 0o, 0x or 0'.
	if !openClose && state.right != (operator{}) && (letterDigit(state.right.name) || (needQuoted(state.right.name) && state.right.name != "," && state.right.name != "|")) {
		_, _ = ew.Write([]byte(" "))
	}

	return ew.Result()
}

func writeFloat(w io.Writer, f float64, opts *Formatter, state formatState) (int64, error) {
	ew := errWriter{w: w}
	openClose := state.left.name == "-" && state.left.specifier.class() == operatorClassPrefix && f > 0

	if openClose || (f < 0 && state.left != operator{}) {
		_, _ = ew.Write([]byte(" "))
	}

	if openClose {
		_, _ = ew.Write([]byte("("))
	}

	s := strconv.FormatFloat(f, 'g', opts.Precision, 64)
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

	if !openClose && state.right != (operator{}) && (state.right.name == "e" || state.right.name == "E") {
		_, _ = ew.Write([]byte(" "))
	}

	return ew.Result()
}

func writeCompound(w io.Writer, e *Engine, t Term, opts *Formatter, state formatState) (int64, error) {
	f, err := e.Functor(t)
	if err != nil {
		return 0, err
	}
	if f == (Functor{Name: "$VAR", Arity: 1}) && opts.NumberVars {
		a := e.Arg(t, 0)
		if n, err := e.Integer(a); err == nil {
			return writeCompoundNumberVars(w, n)
		}
	}

	if !opts.IgnoreOps {
		switch f {
		case Functor{Name: ".", Arity: 2}:
			return writeCompoundList(w, e, t, opts, state)
		case Functor{Name: "{}", Arity: 1}:
			return writeCompoundCurlyBracketed(w, e, t, opts, state)
		}

		ops := opts.Ops.ops
		switch f.Arity {
		case 1:
			if op, ok := ops[opKey{name: f.Name, opClass: operatorClassPrefix}]; ok {
				return writeCompoundOpPrefix(w, e, f.Name, e.Arg(t, 0), &op, opts, state)
			}
			if op, ok := ops[opKey{name: f.Name, opClass: operatorClassPostfix}]; ok {
				return writeCompoundOpPostfix(w, e, f.Name, e.Arg(t, 0), &op, opts, state)
			}
		case 2:
			if op, ok := ops[opKey{name: f.Name, opClass: operatorClassInfix}]; ok {
				return writeCompoundOpInfix(w, e, e.Arg(t, 0), f.Name, e.Arg(t, 1), &op, opts, state)
			}
		}
	}

	return writeCompoundFunctionalNotation(w, e, f.Name, e.Args(t), opts, state)
}

func writeCompoundNumberVars(w io.Writer, n int64) (int64, error) {
	const letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	ew := errWriter{w: w}
	i, j := int(n)%len(letters), int(n)/len(letters)
	_, _ = fmt.Fprint(&ew, string(letters[i]))
	if j != 0 {
		_, _ = fmt.Fprint(&ew, strconv.Itoa(j))
	}
	return ew.Result()
}

func writeCompoundList(w io.Writer, e *Engine, t Term, opts *Formatter, state formatState) (int64, error) {
	ew := errWriter{w: w}
	state.priority = 999
	state.left = operator{}
	state.right = operator{}
	_, _ = fmt.Fprint(&ew, "[")
	_, _ = writeTerm(&ew, e, e.Arg(t, 0), opts, state)
	for elem, err := range e.List(e.Arg(t, 1), AllowCycle(opts.MaxDepth > state.depth)) {
		if err != nil {
			_, _ = fmt.Fprint(&ew, "|")
			if f, err := e.Functor(elem); err == nil && f == (Functor{Name: ".", Arity: 2}) {
				_, _ = writeAtom(&ew, "...", opts, state)
			} else {
				_, _ = writeTerm(&ew, e, elem, opts, state)
			}
			break
		}

		state.depth++
		_, _ = fmt.Fprint(&ew, ",")
		_, _ = writeTerm(&ew, e, elem, opts, state)
	}
	_, _ = fmt.Fprint(&ew, "]")
	return ew.Result()
}

func writeCompoundCurlyBracketed(w io.Writer, e *Engine, t Term, opts *Formatter, state formatState) (int64, error) {
	ew := errWriter{w: w}
	state.left = operator{}
	_, _ = fmt.Fprint(&ew, "{")
	_, _ = writeTerm(&ew, e, e.Arg(t, 0), opts, state)
	_, _ = fmt.Fprint(&ew, "}")
	return ew.Result()
}

func writeCompoundOpPrefix(w io.Writer, e *Engine, name Atom, arg Term, op *operator, opts *Formatter, state formatState) (int64, error) {
	ew := errWriter{w: w}
	_, r := op.bindingPriorities()
	openClose := state.priority < op.priority || (state.right != operator{} && r >= state.right.priority)

	if state.left != (operator{}) {
		_, _ = fmt.Fprint(&ew, " ")
	}
	if openClose {
		_, _ = fmt.Fprint(&ew, "(")
		state.left = operator{}
		state.right = operator{}
	}
	{
		state := state
		state.left = operator{}
		state.right = operator{}
		_, _ = writeAtom(&ew, name, opts, state)
	}
	{
		state := state
		state.priority = r
		state.left = *op
		state.depth++
		_, _ = writeTerm(&ew, e, arg, opts, state)
	}
	if openClose {
		_, _ = fmt.Fprint(&ew, ")")
	}
	return ew.Result()
}

func writeCompoundOpPostfix(w io.Writer, e *Engine, name Atom, arg Term, op *operator, opts *Formatter, state formatState) (int64, error) {
	ew := errWriter{w: w}
	l, _ := op.bindingPriorities()
	openClose := state.priority < op.priority || (state.left.name == "-" && state.left.specifier.class() == operatorClassPrefix)

	if openClose {
		if state.left != (operator{}) {
			_, _ = fmt.Fprint(&ew, " ")
		}
		_, _ = fmt.Fprint(&ew, "(")
		state.left = operator{}
		state.right = operator{}
	}
	{
		state := state
		state.priority = l
		state.right = *op
		state.depth++
		_, _ = writeTerm(&ew, e, arg, opts, state)
	}
	{
		state := state
		state.left = operator{}
		state.right = operator{}
		_, _ = writeAtom(&ew, name, opts, state)
	}
	if openClose {
		_, _ = fmt.Fprint(&ew, ")")
	} else if state.right != (operator{}) {
		_, _ = fmt.Fprint(&ew, " ")
	}
	return ew.Result()
}

func writeCompoundOpInfix(w io.Writer, e *Engine, left Term, name Atom, right Term, op *operator, opts *Formatter, state formatState) (int64, error) {
	ew := errWriter{w: w}
	l, r := op.bindingPriorities()
	openClose := state.priority < op.priority ||
		(state.left.name == "-" && state.left.specifier.class() == operatorClassPrefix) ||
		(state.right != operator{} && r >= state.right.priority)

	if openClose {
		if state.left != (operator{}) && state.left.specifier.class() == operatorClassPrefix {
			_, _ = fmt.Fprint(&ew, " ")
		}
		_, _ = fmt.Fprint(&ew, "(")
		state.left = operator{}
		state.right = operator{}
	}
	{
		state := state
		state.priority = l
		state.right = *op
		state.depth++
		_, _ = writeTerm(&ew, e, left, opts, state)
	}
	switch name {
	case ",", "|":
		_, _ = fmt.Fprint(&ew, name)
	default:
		state := state
		state.left = operator{}
		state.right = operator{}
		_, _ = writeAtom(&ew, name, opts, state)
	}
	{
		state := state
		state.priority = r
		state.left = *op
		state.depth++
		_, _ = writeTerm(&ew, e, right, opts, state)
	}
	if openClose {
		_, _ = fmt.Fprint(&ew, ")")
	}
	return ew.Result()
}

func writeCompoundFunctionalNotation(w io.Writer, e *Engine, name Atom, args iter.Seq[Term], opts *Formatter, state formatState) (int64, error) {
	ew := errWriter{w: w}
	state.right = operator{}
	_, _ = writeAtom(&ew, name, opts, state)
	_, _ = fmt.Fprint(&ew, "(")
	state.left = operator{}
	state.priority = 999
	state.depth++
	for i, a := range withIndex(args) {
		if i != 0 {
			_, _ = fmt.Fprint(&ew, ",")
		}
		_, _ = writeTerm(&ew, e, a, opts, state)
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
