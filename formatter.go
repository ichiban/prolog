package prolog

import (
	"fmt"
	"io"
	"regexp"
	"strconv"
	"strings"
)

type formatState struct {
	priority    int
	visited     map[Term]struct{}
	prefixMinus bool
	left, right operator
	depth       int
}

type Formatter struct {
	Term Term
	Heap *Heap

	IgnoreOps    bool
	Quoted       bool
	VariableName map[Variable]string
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
	return writeTerm(w, f.Heap, f.Term, f, state)
}

func writeTerm(w io.Writer, h *Heap, t Term, opts *Formatter, state formatState) (int64, error) {
	t = t.resolve(h)

	if _, ok := state.visited[t]; ok || (opts.MaxDepth > 0 && state.depth > opts.MaxDepth) {
		return writeAtom(w, "...", opts, state)
	}

	if v, err := t.Variable(h); err == nil {
		return writeVariable(w, v, opts, state)
	}

	if name, err := t.Atom(h); err == nil {
		return writeAtom(w, name, opts, state)
	}

	if i, err := t.Integer(h); err == nil {
		return writeInteger(w, i, opts, state)
	}

	if f, err := t.Float(h); err == nil {
		return writeFloat(w, f, opts, state)
	}

	if state.visited == nil {
		state.visited = map[Term]struct{}{}
	}
	state.visited[t] = struct{}{}

	c, err := t.Compound(h)
	if err != nil {
		return 0, err
	}

	return writeCompound(w, h, c, opts, state)
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

func writeAtom(w io.Writer, name string, opts *Formatter, state formatState) (int64, error) {
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

func needQuoted(name string) bool {
	p := NewParser(strings.NewReader(name), Operators{}, doubleQuotesChars)
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

func quote(s string) string {
	return fmt.Sprintf("'%s'", quotedAtomEscapePattern.ReplaceAllStringFunc(s, quotedIdentEscape))
}

func letterDigit(s string) bool {
	return len(s) > 0 && isSmallLetterChar([]rune(s)[0])
}

func graphic(s string) bool {
	return len(s) > 0 && (isGraphicChar([]rune(s)[0]) || []rune(s)[0] == '\\')
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

func writeCompound(w io.Writer, h *Heap, c *Compound, opts *Formatter, state formatState) (int64, error) {
	if c.Functor == (Functor{Name: "$VAR", Arity: 1}) && opts.NumberVars {
		a := c.Arg(h, 0)
		if n, err := a.Integer(h); err == nil {
			return writeCompoundNumberVars(w, n)
		}
	}

	if !opts.IgnoreOps {
		switch c.Functor {
		case Functor{Name: ".", Arity: 2}:
			return writeCompoundList(w, h, c, opts, state)
		case Functor{Name: "{}", Arity: 1}:
			return writeCompoundCurlyBracketed(w, h, c, opts, state)
		}

		ops := opts.Ops.ops
		switch c.Arity {
		case 1:
			if op, ok := ops[opKey{name: c.Name, opClass: operatorClassPrefix}]; ok {
				return writeCompoundOpPrefix(w, h, c, &op, opts, state)
			}
			if op, ok := ops[opKey{name: c.Name, opClass: operatorClassPostfix}]; ok {
				return writeCompoundOpPostfix(w, h, c, &op, opts, state)
			}
		case 2:
			if op, ok := ops[opKey{name: c.Name, opClass: operatorClassInfix}]; ok {
				return writeCompoundOpInfix(w, h, c, &op, opts, state)
			}
		}
	}

	return writeCompoundFunctionalNotation(w, h, c, opts, state)
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

func writeCompoundList(w io.Writer, h *Heap, c *Compound, opts *Formatter, state formatState) (int64, error) {
	ew := errWriter{w: w}
	state.priority = 999
	state.left = operator{}
	state.right = operator{}
	_, _ = fmt.Fprint(&ew, "[")
	_, _ = writeTerm(&ew, h, c.Arg(h, 0), opts, state)
	for elem, err := range c.Arg(h, 1).List(h, AllowCycle(opts.MaxDepth > state.depth)) {
		if err != nil {
			_, _ = fmt.Fprint(&ew, "|")
			if c, err := elem.Compound(h); err == nil && c.Functor == (Functor{Name: ".", Arity: 2}) {
				_, _ = writeAtom(&ew, "...", opts, state)
			} else {
				_, _ = writeTerm(&ew, h, elem, opts, state)
			}
			break
		}

		state.depth++
		_, _ = fmt.Fprint(&ew, ",")
		_, _ = writeTerm(&ew, h, elem, opts, state)
	}
	_, _ = fmt.Fprint(&ew, "]")
	return ew.Result()
}

func writeCompoundCurlyBracketed(w io.Writer, h *Heap, c *Compound, opts *Formatter, state formatState) (int64, error) {
	ew := errWriter{w: w}
	state.left = operator{}
	_, _ = fmt.Fprint(&ew, "{")
	_, _ = writeTerm(&ew, h, c.Arg(h, 0), opts, state)
	_, _ = fmt.Fprint(&ew, "}")
	return ew.Result()
}

func writeCompoundOpPrefix(w io.Writer, h *Heap, c *Compound, op *operator, opts *Formatter, state formatState) (int64, error) {
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
		_, _ = writeAtom(&ew, c.Name, opts, state)
	}
	{
		state := state
		state.priority = r
		state.left = *op
		state.depth++
		_, _ = writeTerm(&ew, h, c.Arg(h, 0), opts, state)
	}
	if openClose {
		_, _ = fmt.Fprint(&ew, ")")
	}
	return ew.Result()
}

func writeCompoundOpPostfix(w io.Writer, h *Heap, c *Compound, op *operator, opts *Formatter, state formatState) (int64, error) {
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
		_, _ = writeTerm(&ew, h, c.Arg(h, 0), opts, state)
	}
	{
		state := state
		state.left = operator{}
		state.right = operator{}
		_, _ = writeAtom(&ew, c.Name, opts, state)
	}
	if openClose {
		_, _ = fmt.Fprint(&ew, ")")
	} else if state.right != (operator{}) {
		_, _ = fmt.Fprint(&ew, " ")
	}
	return ew.Result()
}

func writeCompoundOpInfix(w io.Writer, h *Heap, c *Compound, op *operator, opts *Formatter, state formatState) (int64, error) {
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
		_, _ = writeTerm(&ew, h, c.Arg(h, 0), opts, state)
	}
	switch name := c.Name; name {
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
		_, _ = writeTerm(&ew, h, c.Arg(h, 1), opts, state)
	}
	if openClose {
		_, _ = fmt.Fprint(&ew, ")")
	}
	return ew.Result()
}

func writeCompoundFunctionalNotation(w io.Writer, h *Heap, c *Compound, opts *Formatter, state formatState) (int64, error) {
	ew := errWriter{w: w}
	state.right = operator{}
	_, _ = writeAtom(&ew, c.Name, opts, state)
	_, _ = fmt.Fprint(&ew, "(")
	state.left = operator{}
	state.priority = 999
	state.depth++
	for i, a := range c.Args(h) {
		if i != 0 {
			_, _ = fmt.Fprint(&ew, ",")
		}
		_, _ = writeTerm(&ew, h, a, opts, state)
	}
	_, _ = fmt.Fprint(&ew, ")")
	return ew.Result()
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
