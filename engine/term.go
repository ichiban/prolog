package engine

import (
	"io"
	"reflect"
	"strconv"
	"strings"
)

// Term is a prolog term.
type Term interface {
}

// writeOptions specify how the Term writes itself.
type writeOptions struct {
	ignoreOps     bool
	quoted        bool
	variableNames map[Variable]Atom
	numberVars    bool

	ops         operators
	priority    Integer
	visited     map[termID]struct{}
	prefixMinus bool
	left, right operator
}

func (o writeOptions) withQuoted(quoted bool) *writeOptions {
	o.quoted = quoted
	return &o
}

func (o writeOptions) withFreshVisited() *writeOptions {
	visited := make(map[termID]struct{}, len(o.visited))
	for k, v := range o.visited {
		visited[k] = v
	}
	o.visited = visited
	return &o
}

func (o writeOptions) withPriority(priority Integer) *writeOptions {
	o.priority = priority
	return &o
}

func (o writeOptions) withLeft(op operator) *writeOptions {
	o.left = op
	return &o
}

func (o writeOptions) withRight(op operator) *writeOptions {
	o.right = op
	return &o
}

var defaultWriteOptions = writeOptions{
	ops: operators{
		atomPlus: [_operatorClassLen]operator{
			operatorClassInfix: {priority: 500, specifier: operatorSpecifierYFX, name: atomPlus}, // for flag+value
		},
		atomSlash: [_operatorClassLen]operator{
			operatorClassInfix: {priority: 400, specifier: operatorSpecifierYFX, name: atomSlash}, // for principal functors
		},
	},
	variableNames: map[Variable]Atom{},
	priority:      1200,
}

func writeTerm(w io.StringWriter, t Term, opts *writeOptions, env *Env) error {
	switch t := env.Resolve(t).(type) {
	case Variable:
		return writeVariable(w, t, opts, env)
	case Atom:
		return writeAtom(w, t, opts)
	case Integer:
		return writeInteger(w, t, opts)
	case Float:
		return writeFloat(w, t, opts)
	case Compound:
		return writeCompound(w, t, opts, env)
	default:
		return writeAny(w, t)
	}
}

func writeVariable(w io.StringWriter, v Variable, opts *writeOptions, env *Env) error {
	if a, ok := opts.variableNames[v]; ok {
		return writeTerm(w, a, opts.withQuoted(false).withLeft(operator{}).withRight(operator{}), env)
	}
	_, err := w.WriteString(v.String())
	return err
}

func writeAtom(w io.StringWriter, a Atom, opts *writeOptions) error {
	ew := errStringWriter{w: w}
	openClose := (opts.left != (operator{}) || opts.right != (operator{})) && opts.ops.defined(a)

	if openClose {
		if opts.left.name != 0 && opts.left.specifier.class() == operatorClassPrefix {
			_, _ = ew.WriteString(" ")
		}
		_, _ = ew.WriteString("(")
		opts = opts.withLeft(operator{}).withRight(operator{})
	}

	if opts.quoted && needQuoted(a) {
		if opts.left != (operator{}) && needQuoted(opts.left.name) { // Avoid 'FOO''BAR'.
			_, _ = ew.WriteString(" ")
		}
		_, _ = ew.WriteString(quote(a.String()))
		if opts.right != (operator{}) && needQuoted(opts.right.name) { // Avoid 'FOO''BAR'.
			_, _ = ew.WriteString(" ")
		}
	} else {
		if (letterDigit(opts.left.name) && letterDigit(a)) || (graphic(opts.left.name) && graphic(a)) {
			_, _ = ew.WriteString(" ")
		}
		_, _ = ew.WriteString(a.String())
		if (letterDigit(opts.right.name) && letterDigit(a)) || (graphic(opts.right.name) && graphic(a)) {
			_, _ = ew.WriteString(" ")
		}
	}

	if openClose {
		_, _ = ew.WriteString(")")
	}

	return ew.err
}

func writeInteger(w io.StringWriter, i Integer, opts *writeOptions) error {
	ew := errStringWriter{w: w}
	openClose := opts.left.name == atomMinus && opts.left.specifier.class() == operatorClassPrefix && i > 0

	if openClose {
		_, _ = ew.WriteString(" (")
		opts = opts.withLeft(operator{}).withRight(operator{})
	} else {
		if opts.left != (operator{}) && (letterDigit(opts.left.name) || (i < 0 && graphic(opts.left.name))) {
			_, _ = ew.WriteString(" ")
		}
	}

	s := strconv.FormatInt(int64(i), 10)
	_, _ = ew.WriteString(s)

	if openClose {
		_, _ = ew.WriteString(")")
	}

	// Avoid ambiguous 0b, 0o, 0x or 0'.
	if !openClose && opts.right != (operator{}) && (letterDigit(opts.right.name) || (needQuoted(opts.right.name) && opts.right.name != atomComma && opts.right.name != atomBar)) {
		_, _ = ew.WriteString(" ")
	}

	return ew.err
}

func writeFloat(w io.StringWriter, f Float, opts *writeOptions) error {
	ew := errStringWriter{w: w}
	openClose := opts.left.name == atomMinus && opts.left.specifier.class() == operatorClassPrefix && f > 0

	if openClose || (f < 0 && opts.left != operator{}) {
		_, _ = ew.WriteString(" ")
	}

	if openClose {
		_, _ = ew.WriteString("(")
	}

	s := strconv.FormatFloat(float64(f), 'g', -1, 64)
	if !strings.ContainsRune(s, '.') {
		if strings.ContainsRune(s, 'e') {
			s = strings.Replace(s, "e", ".0e", 1)
		} else {
			s += ".0"
		}
	}
	_, _ = ew.WriteString(s)

	if openClose {
		_, _ = ew.WriteString(")")
	}

	if !openClose && opts.right != (operator{}) && (opts.right.name == atomSmallE || opts.right.name == atomE) {
		_, _ = ew.WriteString(" ")
	}

	return ew.err
}

func writeCompound(w io.StringWriter, c Compound, opts *writeOptions, env *Env) error {
	ok, err := writeCompoundVisit(w, c, opts)
	if err != nil || ok {
		return err
	}

	if n, ok := env.Resolve(c.Arg(0)).(Integer); ok && opts.numberVars && c.Functor() == atomVar && c.Arity() == 1 && n >= 0 {
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

func writeCompoundVisit(w io.StringWriter, c Compound, opts *writeOptions) (bool, error) {
	if opts.visited == nil {
		opts.visited = map[termID]struct{}{}
	}

	if _, ok := opts.visited[id(c)]; ok {
		_, err := w.WriteString("...")
		return true, err
	}
	opts.visited[id(c)] = struct{}{}
	return false, nil
}

func writeCompoundNumberVars(w io.StringWriter, n Integer) error {
	const letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	ew := errStringWriter{w: w}
	i, j := int(n)%len(letters), int(n)/len(letters)
	_, _ = ew.WriteString(string(letters[i]))
	if j != 0 {
		_, _ = ew.WriteString(strconv.Itoa(j))
	}
	return ew.err
}

func writeCompoundList(w io.StringWriter, c Compound, opts *writeOptions, env *Env) error {
	ew := errStringWriter{w: w}
	opts = opts.withPriority(999).withLeft(operator{}).withRight(operator{})
	_, _ = ew.WriteString("[")
	_ = writeTerm(&ew, c.Arg(0), opts, env)
	iter := ListIterator{List: c.Arg(1), Env: env}
	for iter.Next() {
		_, _ = ew.WriteString(",")
		_ = writeTerm(&ew, iter.Current(), opts, env)
	}
	if err := iter.Err(); err != nil {
		_, _ = ew.WriteString("|")
		s := iter.Suffix()
		if l, ok := iter.Suffix().(Compound); ok && l.Functor() == atomDot && l.Arity() == 2 {
			_, _ = ew.WriteString("...")
		} else {
			_ = writeTerm(&ew, s, opts, env)
		}
	}
	_, _ = ew.WriteString("]")
	return ew.err
}

func writeCompoundCurlyBracketed(w io.StringWriter, c Compound, opts *writeOptions, env *Env) error {
	ew := errStringWriter{w: w}
	_, _ = ew.WriteString("{")
	_ = writeTerm(&ew, c.Arg(0), opts.withLeft(operator{}), env)
	_, _ = ew.WriteString("}")
	return ew.err
}

var writeCompoundOps = [...]func(w io.StringWriter, c Compound, opts *writeOptions, env *Env, op *operator) error{
	operatorSpecifierFX:  nil,
	operatorSpecifierFY:  nil,
	operatorSpecifierXF:  nil,
	operatorSpecifierYF:  nil,
	operatorSpecifierXFX: nil,
	operatorSpecifierXFY: nil,
	operatorSpecifierYFX: nil,
}

func init() {
	writeCompoundOps = [len(writeCompoundOps)]func(w io.StringWriter, c Compound, opts *writeOptions, env *Env, op *operator) error{
		operatorSpecifierFX:  writeCompoundOpPrefix,
		operatorSpecifierFY:  writeCompoundOpPrefix,
		operatorSpecifierXF:  writeCompoundOpPostfix,
		operatorSpecifierYF:  writeCompoundOpPostfix,
		operatorSpecifierXFX: writeCompoundOpInfix,
		operatorSpecifierXFY: writeCompoundOpInfix,
		operatorSpecifierYFX: writeCompoundOpInfix,
	}
}

func writeCompoundOp(w io.StringWriter, c Compound, opts *writeOptions, env *Env, op *operator) error {
	return writeCompoundOps[op.specifier](w, c, opts, env, op)
}

func writeCompoundOpPrefix(w io.StringWriter, c Compound, opts *writeOptions, env *Env, op *operator) error {
	ew := errStringWriter{w: w}
	opts = opts.withFreshVisited()
	_, r := op.bindingPriorities()
	openClose := opts.priority < op.priority || (opts.right != operator{} && r >= opts.right.priority)

	if opts.left != (operator{}) {
		_, _ = ew.WriteString(" ")
	}
	if openClose {
		_, _ = ew.WriteString("(")
		opts = opts.withLeft(operator{}).withRight(operator{})
	}
	_ = writeAtom(&ew, c.Functor(), opts.withLeft(operator{}).withRight(operator{}))
	_ = writeTerm(&ew, c.Arg(0), opts.withPriority(r).withLeft(*op), env)
	if openClose {
		_, _ = ew.WriteString(")")
	}
	return ew.err
}

func writeCompoundOpPostfix(w io.StringWriter, c Compound, opts *writeOptions, env *Env, op *operator) error {
	ew := errStringWriter{w: w}
	opts = opts.withFreshVisited()
	l, _ := op.bindingPriorities()
	openClose := opts.priority < op.priority || (opts.left.name == atomMinus && opts.left.specifier.class() == operatorClassPrefix)

	if openClose {
		if opts.left != (operator{}) {
			_, _ = ew.WriteString(" ")
		}
		_, _ = ew.WriteString("(")
		opts = opts.withLeft(operator{}).withRight(operator{})
	}
	_ = writeTerm(&ew, c.Arg(0), opts.withPriority(l).withRight(*op), env)
	_ = writeAtom(&ew, c.Functor(), opts.withLeft(operator{}).withRight(operator{}))
	if openClose {
		_, _ = ew.WriteString(")")
	} else if opts.right != (operator{}) {
		_, _ = ew.WriteString(" ")
	}
	return ew.err
}

func writeCompoundOpInfix(w io.StringWriter, c Compound, opts *writeOptions, env *Env, op *operator) error {
	ew := errStringWriter{w: w}
	opts = opts.withFreshVisited()
	l, r := op.bindingPriorities()
	openClose := opts.priority < op.priority ||
		(opts.left.name == atomMinus && opts.left.specifier.class() == operatorClassPrefix) ||
		(opts.right != operator{} && r >= opts.right.priority)

	if openClose {
		if opts.left.name != 0 && opts.left.specifier.class() == operatorClassPrefix {
			_, _ = ew.WriteString(" ")
		}
		_, _ = ew.WriteString("(")
		opts = opts.withLeft(operator{}).withRight(operator{})
	}
	_ = writeTerm(&ew, c.Arg(0), opts.withPriority(l).withRight(*op), env)
	switch c.Functor() {
	case atomComma, atomBar:
		_, _ = ew.WriteString(c.Functor().String())
	default:
		_ = writeAtom(&ew, c.Functor(), opts.withLeft(operator{}).withRight(operator{}))
	}
	_ = writeTerm(&ew, c.Arg(1), opts.withPriority(r).withLeft(*op), env)
	if openClose {
		_, _ = ew.WriteString(")")
	}
	return ew.err
}

func writeCompoundFunctionalNotation(w io.StringWriter, c Compound, opts *writeOptions, env *Env) error {
	ew := errStringWriter{w: w}
	opts = opts.withRight(operator{})
	_ = writeAtom(&ew, c.Functor(), opts)
	_, _ = ew.WriteString("(")
	opts = opts.withLeft(operator{}).withPriority(999)
	for i := 0; i < c.Arity(); i++ {
		if i != 0 {
			_, _ = ew.WriteString(",")
		}
		_ = writeTerm(&ew, c.Arg(i), opts, env)
	}
	_, _ = ew.WriteString(")")
	return ew.err
}

func writeAny(w io.StringWriter, i interface{}) error {
	v := reflect.ValueOf(i)
	_, err := w.WriteString(v.String())
	return err
}

// https://go.dev/blog/errors-are-values
type errStringWriter struct {
	w   io.StringWriter
	err error
}

func (ew *errStringWriter) WriteString(s string) (int, error) {
	if ew.err != nil {
		return 0, nil
	}
	var n int
	n, ew.err = ew.w.WriteString(s)
	return n, nil
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

// termIDer lets a Term which is not comparable per se return its termID for comparison.
type termIDer interface {
	termID() termID
}

// termID is an identifier for a Term.
type termID interface{}

// id returns a termID for the Term.
func id(t Term) termID {
	switch t := t.(type) {
	case termIDer:
		return t.termID()
	default:
		return t // Assuming it's comparable.
	}
}
