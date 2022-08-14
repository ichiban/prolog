package engine

import (
	"fmt"
	"io"
	"reflect"
	"strconv"
	"strings"
)

// Term is a prolog term.
type Term interface {
	Compare(Term, *Env) int64
}

// Contains checks if t contains s.
func Contains(t, s Term, env *Env) bool {
	switch t := t.(type) {
	case Variable:
		if t == s {
			return true
		}
		ref, ok := env.Lookup(t)
		if !ok {
			return false
		}
		return Contains(ref, s, env)
	case *Compound:
		if s, ok := s.(Atom); ok && t.Functor == s {
			return true
		}
		for _, a := range t.Args {
			if Contains(a, s, env) {
				return true
			}
		}
		return false
	default:
		return t == s
	}
}

// Rulify returns t if t is in a form of P:-Q, t:-true otherwise.
func Rulify(t Term, env *Env) Term {
	t = env.Resolve(t)
	if c, ok := t.(*Compound); ok && c.Functor == ":-" && len(c.Args) == 2 {
		return t
	}
	return &Compound{Functor: ":-", Args: []Term{t, Atom("true")}}
}

// WriteOptions specify how the Term writes itself.
type WriteOptions struct {
	IgnoreOps     bool
	Quoted        bool
	VariableNames map[Variable]Atom
	NumberVars    bool

	ops         operators
	priority    Integer
	visited     map[Term]struct{}
	prefixMinus bool
	left, right operator
}

func (o WriteOptions) withQuoted(quoted bool) *WriteOptions {
	o.Quoted = quoted
	return &o
}

func (o WriteOptions) withFreshVisited() *WriteOptions {
	visited := make(map[Term]struct{}, len(o.visited))
	for k, v := range o.visited {
		visited[k] = v
	}
	o.visited = visited
	return &o
}

func (o WriteOptions) withPriority(priority Integer) *WriteOptions {
	o.priority = priority
	return &o
}

func (o WriteOptions) withLeft(op operator) *WriteOptions {
	o.left = op
	return &o
}

func (o WriteOptions) withRight(op operator) *WriteOptions {
	o.right = op
	return &o
}

var defaultWriteOptions = WriteOptions{
	ops: operators{
		"+": [_operatorClassLen]operator{
			operatorClassInfix: {priority: 500, specifier: operatorSpecifierYFX, name: "+"}, // for flag+value
		},
		"/": [_operatorClassLen]operator{
			operatorClassInfix: {priority: 400, specifier: operatorSpecifierYFX, name: "/"}, // for principal functors
		},
	},
	VariableNames: map[Variable]Atom{},
	priority:      1200,
}

func WriteTerm(w io.Writer, t Term, opts *WriteOptions, env *Env) error {
	switch t := env.Resolve(t).(type) {
	case Variable:
		return writeVariable(w, t, opts, env)
	case Atom:
		return writeAtom(w, t, opts)
	case Integer:
		return writeInteger(w, t, opts)
	case Float:
		return writeFloat(w, t, opts)
	case *Compound:
		return writeCompound(w, t, opts, env)
	default:
		return writeAny(w, t)
	}
}

func writeVariable(w io.Writer, v Variable, opts *WriteOptions, env *Env) error {
	if a, ok := opts.VariableNames[v]; ok {
		return WriteTerm(w, a, opts.withQuoted(false).withLeft(operator{}).withRight(operator{}), env)
	}
	_, err := fmt.Fprint(w, string(v))
	return err
}

func writeAtom(w io.Writer, a Atom, opts *WriteOptions) error {
	ew := errWriter{w: w}
	openClose := (opts.left != (operator{}) || opts.right != (operator{})) && opts.ops.defined(a)

	if openClose {
		if opts.left.name != "" && opts.left.specifier.class() == operatorClassPrefix {
			_, _ = fmt.Fprint(&ew, " ")
		}
		_, _ = fmt.Fprint(&ew, "(")
		opts = opts.withLeft(operator{}).withRight(operator{})
	}

	if opts.Quoted && needQuoted(a) {
		if opts.left != (operator{}) && needQuoted(opts.left.name) { // Avoid 'FOO''BAR'.
			_, _ = fmt.Fprint(&ew, " ")
		}
		_, _ = fmt.Fprint(&ew, quote(string(a)))
		if opts.right != (operator{}) && needQuoted(opts.right.name) { // Avoid 'FOO''BAR'.
			_, _ = fmt.Fprint(&ew, " ")
		}
	} else {
		if (letterDigit(opts.left.name) && letterDigit(a)) || (graphic(opts.left.name) && graphic(a)) {
			_, _ = fmt.Fprint(&ew, " ")
		}
		_, _ = fmt.Fprint(&ew, string(a))
		if (letterDigit(opts.right.name) && letterDigit(a)) || (graphic(opts.right.name) && graphic(a)) {
			_, _ = fmt.Fprint(&ew, " ")
		}
	}

	if openClose {
		_, _ = fmt.Fprint(&ew, ")")
	}

	return ew.err
}

func writeInteger(w io.Writer, i Integer, opts *WriteOptions) error {
	ew := errWriter{w: w}
	openClose := opts.left.name == "-" && opts.left.specifier.class() == operatorClassPrefix && i > 0

	if openClose {
		_, _ = fmt.Fprint(&ew, " (")
		opts = opts.withLeft(operator{}).withRight(operator{})
	} else {
		if opts.left != (operator{}) && (letterDigit(opts.left.name) || (i < 0 && graphic(opts.left.name))) {
			_, _ = fmt.Fprint(&ew, " ")
		}
	}

	s := strconv.FormatInt(int64(i), 10)
	_, _ = fmt.Fprint(&ew, s)

	if openClose {
		_, _ = fmt.Fprint(&ew, ")")
	}

	// Avoid ambiguous 0b, 0o, 0x or 0'.
	if !openClose && opts.right != (operator{}) && (letterDigit(opts.right.name) || (needQuoted(opts.right.name) && opts.right.name != "," && opts.right.name != "|")) {
		_, _ = fmt.Fprint(&ew, " ")
	}

	return ew.err
}

func writeFloat(w io.Writer, f Float, opts *WriteOptions) error {
	ew := errWriter{w: w}
	openClose := opts.left.name == "-" && opts.left.specifier.class() == operatorClassPrefix && f > 0

	if openClose || (f < 0 && opts.left != operator{}) {
		_, _ = fmt.Fprint(&ew, " ")
	}

	if openClose {
		_, _ = fmt.Fprint(&ew, "(")
	}

	s := strconv.FormatFloat(float64(f), 'g', -1, 64)
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

	if !openClose && opts.right != (operator{}) && (opts.right.name == "e" || opts.right.name == "E") {
		_, _ = fmt.Fprint(&ew, " ")
	}

	return ew.err
}

func writeCompound(w io.Writer, c *Compound, opts *WriteOptions, env *Env) error {
	ok, err := writeCompoundVisit(w, c, opts)
	if err != nil || ok {
		return err
	}

	if n, ok := env.Resolve(c.Args[0]).(Integer); ok && opts.NumberVars && c.Functor == "$VAR" && len(c.Args) == 1 && n >= 0 {
		return writeCompoundNumberVars(w, n)
	}

	if !opts.IgnoreOps {
		if c.Functor == "." && len(c.Args) == 2 {
			return writeCompoundList(w, c, opts, env)
		}

		if c.Functor == "{}" && len(c.Args) == 1 {
			return writeCompoundCurlyBracketed(w, c, opts, env)
		}
	}

	if opts.IgnoreOps {
		return writeCompoundFunctionalNotation(w, c, opts, env)
	}

	for _, o := range opts.ops[c.Functor] {
		if o.specifier.arity() == len(c.Args) {
			return writeCompoundOp(w, c, opts, env, &o)
		}
	}

	return writeCompoundFunctionalNotation(w, c, opts, env)
}

func writeCompoundVisit(w io.Writer, c *Compound, opts *WriteOptions) (bool, error) {
	if opts.visited == nil {
		opts.visited = map[Term]struct{}{}
	}

	if _, ok := opts.visited[c]; ok {
		_, err := fmt.Fprint(w, "...")
		return true, err
	}
	opts.visited[c] = struct{}{}
	return false, nil
}

func writeCompoundNumberVars(w io.Writer, n Integer) error {
	const letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	ew := errWriter{w: w}
	i, j := int(n)%len(letters), int(n)/len(letters)
	_, _ = fmt.Fprint(&ew, string(letters[i]))
	if j != 0 {
		_, _ = fmt.Fprint(&ew, j)
	}
	return ew.err
}

func writeCompoundList(w io.Writer, c *Compound, opts *WriteOptions, env *Env) error {
	ew := errWriter{w: w}
	opts = opts.withPriority(999).withLeft(operator{}).withRight(operator{})
	_, _ = fmt.Fprint(&ew, "[")
	_ = WriteTerm(&ew, c.Args[0], opts, env)
	iter := ListIterator{List: c.Args[1], Env: env}
	for iter.Next() {
		_, _ = fmt.Fprint(&ew, ",")
		_ = WriteTerm(&ew, iter.Current(), opts, env)
	}
	if err := iter.Err(); err != nil {
		_, _ = fmt.Fprint(&ew, "|")
		s := iter.Suffix()
		if l, ok := iter.Suffix().(*Compound); ok && l.Functor == "." && len(l.Args) == 2 {
			_, _ = fmt.Fprint(&ew, "...")
		} else {
			_ = WriteTerm(&ew, s, opts, env)
		}
	}
	_, _ = fmt.Fprint(&ew, "]")
	return ew.err
}

func writeCompoundCurlyBracketed(w io.Writer, c *Compound, opts *WriteOptions, env *Env) error {
	ew := errWriter{w: w}
	_, _ = fmt.Fprint(&ew, "{")
	_ = WriteTerm(&ew, c.Args[0], opts.withLeft(operator{}), env)
	_, _ = fmt.Fprint(&ew, "}")
	return ew.err
}

var writeCompoundOps = [...]func(w io.Writer, c *Compound, opts *WriteOptions, env *Env, op *operator) error{
	operatorSpecifierFX:  nil,
	operatorSpecifierFY:  nil,
	operatorSpecifierXF:  nil,
	operatorSpecifierYF:  nil,
	operatorSpecifierXFX: nil,
	operatorSpecifierXFY: nil,
	operatorSpecifierYFX: nil,
}

func init() {
	writeCompoundOps = [len(writeCompoundOps)]func(w io.Writer, c *Compound, opts *WriteOptions, env *Env, op *operator) error{
		operatorSpecifierFX:  writeCompoundOpPrefix,
		operatorSpecifierFY:  writeCompoundOpPrefix,
		operatorSpecifierXF:  writeCompoundOpPostfix,
		operatorSpecifierYF:  writeCompoundOpPostfix,
		operatorSpecifierXFX: writeCompoundOpInfix,
		operatorSpecifierXFY: writeCompoundOpInfix,
		operatorSpecifierYFX: writeCompoundOpInfix,
	}
}

func writeCompoundOp(w io.Writer, c *Compound, opts *WriteOptions, env *Env, op *operator) error {
	return writeCompoundOps[op.specifier](w, c, opts, env, op)
}

func writeCompoundOpPrefix(w io.Writer, c *Compound, opts *WriteOptions, env *Env, op *operator) error {
	ew := errWriter{w: w}
	opts = opts.withFreshVisited()
	_, r := op.bindingPriorities()
	openClose := opts.priority < op.priority || (opts.right != operator{} && r >= opts.right.priority)

	if opts.left != (operator{}) {
		_, _ = fmt.Fprint(&ew, " ")
	}
	if openClose {
		_, _ = fmt.Fprint(&ew, "(")
		opts = opts.withLeft(operator{}).withRight(operator{})
	}
	_ = writeAtom(&ew, c.Functor, opts.withLeft(operator{}).withRight(operator{}))
	_ = WriteTerm(&ew, c.Args[0], opts.withPriority(r).withLeft(*op), env)
	if openClose {
		_, _ = fmt.Fprint(&ew, ")")
	}
	return ew.err
}

func writeCompoundOpPostfix(w io.Writer, c *Compound, opts *WriteOptions, env *Env, op *operator) error {
	ew := errWriter{w: w}
	opts = opts.withFreshVisited()
	l, _ := op.bindingPriorities()
	openClose := opts.priority < op.priority || (opts.left.name == "-" && opts.left.specifier.class() == operatorClassPrefix)

	if openClose {
		if opts.left != (operator{}) {
			_, _ = fmt.Fprint(&ew, " ")
		}
		_, _ = fmt.Fprint(&ew, "(")
		opts = opts.withLeft(operator{}).withRight(operator{})
	}
	_ = WriteTerm(&ew, c.Args[0], opts.withPriority(l).withRight(*op), env)
	_ = writeAtom(&ew, c.Functor, opts.withLeft(operator{}).withRight(operator{}))
	if openClose {
		_, _ = fmt.Fprint(&ew, ")")
	} else if opts.right != (operator{}) {
		_, _ = fmt.Fprint(&ew, " ")
	}
	return ew.err
}

func writeCompoundOpInfix(w io.Writer, c *Compound, opts *WriteOptions, env *Env, op *operator) error {
	ew := errWriter{w: w}
	opts = opts.withFreshVisited()
	l, r := op.bindingPriorities()
	openClose := opts.priority < op.priority ||
		(opts.left.name == "-" && opts.left.specifier.class() == operatorClassPrefix) ||
		(opts.right != operator{} && r >= opts.right.priority)

	if openClose {
		if opts.left.name != "" && opts.left.specifier.class() == operatorClassPrefix {
			_, _ = fmt.Fprint(&ew, " ")
		}
		_, _ = fmt.Fprint(&ew, "(")
		opts = opts.withLeft(operator{}).withRight(operator{})
	}
	_ = WriteTerm(&ew, c.Args[0], opts.withPriority(l).withRight(*op), env)
	switch c.Functor {
	case ",", "|":
		_, _ = fmt.Fprint(&ew, c.Functor)
	default:
		_ = writeAtom(&ew, c.Functor, opts.withLeft(operator{}).withRight(operator{}))
	}
	_ = WriteTerm(&ew, c.Args[1], opts.withPriority(r).withLeft(*op), env)
	if openClose {
		_, _ = fmt.Fprint(&ew, ")")
	}
	return ew.err
}

func writeCompoundFunctionalNotation(w io.Writer, c *Compound, opts *WriteOptions, env *Env) error {
	ew := errWriter{w: w}
	opts = opts.withRight(operator{})
	_ = writeAtom(&ew, c.Functor, opts)
	_, _ = fmt.Fprint(&ew, "(")
	opts = opts.withLeft(operator{}).withPriority(999)
	for i, a := range c.Args {
		if i != 0 {
			_, _ = fmt.Fprint(&ew, ",")
		}
		_ = WriteTerm(&ew, a, opts, env)
	}
	_, _ = fmt.Fprint(&ew, ")")
	return ew.err
}

func writeAny(w io.Writer, i interface{}) error {
	v := reflect.ValueOf(i)
	_, err := fmt.Fprint(w, v.String())
	return err
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

func iteratedGoalTerm(t Term, env *Env) Term {
	for {
		c, ok := env.Resolve(t).(*Compound)
		if !ok || c.Functor != "^" || len(c.Args) != 2 {
			return t
		}
		t = c.Args[1]
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
		case *Compound:
			switch y := y.(type) {
			case *Compound:
				if x.Functor != y.Functor || len(x.Args) != len(y.Args) {
					return false
				}
				for i := range x.Args {
					rest = append(rest, [2]Term{x.Args[i], y.Args[i]})
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
