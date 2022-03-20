package engine

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"reflect"
	"regexp"
	"strconv"
	"strings"
)

// Parser turns bytes into Term.
type Parser struct {
	lexer        *Lexer
	current      *Token
	history      []Token
	operators    *operators
	placeholder  Atom
	args         []Term
	doubleQuotes doubleQuotes
	vars         *[]ParsedVariable
}

// ParsedVariable is a set of information regarding a variable in a parsed term.
type ParsedVariable struct {
	Name     Atom
	Variable Variable
	Count    int
}

func newParser(input *bufio.Reader, charConversions map[rune]rune, opts ...parserOption) *Parser {
	p := Parser{
		lexer: NewLexer(input, charConversions),
	}
	for _, o := range opts {
		o(&p)
	}
	return &p
}

type parserOption func(p *Parser)

func withOperators(operators *operators) parserOption {
	return func(p *Parser) {
		p.operators = operators
	}
}

func withDoubleQuotes(quotes doubleQuotes) parserOption {
	return func(p *Parser) {
		p.doubleQuotes = quotes
	}
}

func withParsedVars(vars *[]ParsedVariable) parserOption {
	return func(p *Parser) {
		p.vars = vars
	}
}

// Replace registers placeholder and its arguments. Every occurrence of placeholder will be replaced by arguments.
// Mismatch of the number of occurrences of placeholder and the number of arguments raises an error.
func (p *Parser) Replace(placeholder Atom, args ...interface{}) error {
	p.placeholder = placeholder
	p.args = make([]Term, len(args))
	for i, a := range args {
		var err error
		p.args[i], err = termOf(reflect.ValueOf(a))
		if err != nil {
			return err
		}
	}
	return nil
}

func termOf(o reflect.Value) (Term, error) {
	if t, ok := o.Interface().(Term); ok {
		return t, nil
	}

	switch o.Kind() {
	case reflect.Float32, reflect.Float64:
		return Float(o.Float()), nil
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		return Integer(o.Int()), nil
	case reflect.String:
		return Atom(o.String()), nil
	case reflect.Array, reflect.Slice:
		l := o.Len()
		es := make([]Term, l)
		for i := 0; i < l; i++ {
			var err error
			es[i], err = termOf(o.Index(i))
			if err != nil {
				return nil, err
			}
		}
		return List(es...), nil
	default:
		return nil, fmt.Errorf("can't convert to term: %v", o)
	}
}

func (p *Parser) accept(k TokenKind, vals ...string) (string, error) {
	v, err := p.expect(k, vals...)
	if err != nil {
		return "", err
	}
	p.history = append(p.history, *p.current)
	if len(p.history) > 4 {
		p.history = p.history[1:]
	}
	p.current = nil
	return v, nil
}

func (p *Parser) acceptAtom(allowComma, allowBar bool, vals ...string) (Atom, error) {
	if v, err := p.accept(TokenIdent, vals...); err == nil {
		return Atom(v), nil
	}
	if v, err := p.accept(TokenQuotedIdent, quoteSlice(vals)...); err == nil {
		return Atom(unquote(v)), nil
	}
	if v, err := p.accept(TokenGraphic, vals...); err == nil {
		return Atom(v), nil
	}
	if allowComma {
		if v, err := p.accept(TokenComma, vals...); err == nil {
			return Atom(v), nil
		}
	}
	if allowBar {
		if v, err := p.accept(TokenBar, vals...); err == nil {
			return Atom(v), nil
		}
	}
	if v, err := p.accept(TokenSign, vals...); err == nil {
		return Atom(v), nil
	}
	return "", errors.New("not an atom")
}

func (p *Parser) acceptOp(min int, allowComma, allowBar bool) (*operator, error) {
	if p.operators == nil {
		return nil, errors.New("no op")
	}
	for _, op := range *p.operators {
		l, _ := op.bindingPowers()
		if l < min {
			continue
		}

		if _, err := p.acceptAtom(allowComma, allowBar, string(op.name)); err != nil {
			continue
		}

		return &op, nil
	}
	return nil, errors.New("no op")
}

func (p *Parser) acceptPrefix(allowComma, allowBar bool) (*operator, error) {
	if p.operators == nil {
		return nil, errors.New("no op")
	}
	for _, op := range *p.operators {
		l, _ := op.bindingPowers()
		if l != 0 {
			continue
		}

		if _, err := p.acceptAtom(allowComma, allowBar, string(op.name)); err != nil {
			continue
		}

		return &op, nil
	}
	return nil, errors.New("no op")
}

func (p *Parser) expect(k TokenKind, vals ...string) (string, error) {
	if p.current == nil {
		t, err := p.lexer.Token()
		if err != nil {
			return "", err
		}
		p.current = &t
	}

	if p.current.Kind != k {
		return "", p.expectationError(k, vals)
	}

	if len(vals) > 0 {
		for _, v := range vals {
			if v == p.current.Val {
				return v, nil
			}
		}
		return "", p.expectationError(k, vals)
	}

	return p.current.Val, nil
}

func (p *Parser) expectationError(k TokenKind, vals []string) error {
	if p.current == nil || p.current.Kind == TokenEOF {
		return ErrInsufficient
	}
	return &unexpectedTokenError{
		ExpectedKind: k,
		ExpectedVals: vals,
		Actual:       *p.current,
		History:      p.history,
	}
}

// Term parses a term followed by a full stop.
func (p *Parser) Term() (Term, error) {
	switch _, err := p.accept(TokenEOF); {
	case err == nil:
		return nil, io.EOF
	case p.current == nil:
		// When accepting EOF failed, there must be a valid token ready in p.current.
		// If not the case, we can't miss the error.
		return nil, err
	}

	if p.vars != nil {
		// reset vars
		for i := range *p.vars {
			(*p.vars)[i] = ParsedVariable{}
		}
		*p.vars = (*p.vars)[:0]
	}

	t, err := p.expr(1, true, true)
	if err != nil {
		return nil, err
	}

	if _, err := p.accept(TokenPeriod); err != nil {
		return nil, err
	}

	if len(p.args) != 0 {
		return nil, fmt.Errorf("too many arguments for placeholders: %s", p.args)
	}

	return t, nil
}

var errNotANumber = errors.New("not a number")

// Number parses a number term.
func (p *Parser) Number() (Term, error) {
	n, err := p.number()
	if err != nil {
		return nil, err
	}

	_, err = p.accept(TokenEOF)
	return n, err
}

func (p *Parser) number() (Term, error) {
	sign, _ := p.accept(TokenSign)

	if f, err := p.accept(TokenFloat); err == nil {
		f = sign + f
		n, _ := strconv.ParseFloat(f, 64)
		return Float(n), nil
	}

	if i, err := p.accept(TokenInteger); err == nil {
		i = sign + i
		switch {
		case strings.HasPrefix(i, "0'"):
			return Integer([]rune(i)[2]), nil
		case strings.HasPrefix(i, "+0'"):
			return Integer([]rune(i)[3]), nil
		case strings.HasPrefix(i, "-0'"):
			return Integer(-1 * int64([]rune(i)[3])), nil
		default:
			n, _ := strconv.ParseInt(i, 0, 64)
			return Integer(n), nil
		}
	}

	return nil, errNotANumber
}

// based on Pratt parser explained in this article: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
func (p *Parser) expr(min int, allowComma, allowBar bool) (Term, error) {
	lhs, err := p.lhs(allowComma, allowBar)
	if err != nil {
		return nil, err
	}

	for {
		op, err := p.acceptOp(min, allowComma, allowBar)
		if err != nil {
			break
		}

		_, r := op.bindingPowers()
		rhs, err := p.expr(r, allowComma, allowBar)
		if err != nil {
			return nil, err
		}

		lhs = &Compound{
			Functor: op.name,
			Args:    []Term{lhs, rhs},
		}
	}

	return lhs, nil
}

func (p *Parser) lhs(allowComma, allowBar bool) (Term, error) {
	if _, err := p.accept(TokenEOF); err == nil {
		return nil, ErrInsufficient
	}

	if p, err := p.paren(); err == nil {
		return p, nil
	}

	if b, err := p.block(); err == nil {
		return b, nil
	}

	if t, err := p.number(); err == nil {
		return t, nil
	}

	if v, err := p.variable(); err == nil {
		return v, nil
	}

	if d, err := p.acceptDoubleQuoted(); err == nil {
		return d, nil
	}

	if l, err := p.list(); err == nil {
		return l, nil
	}

	if p, err := p.prefix(allowComma, allowBar); err == nil {
		return p, nil
	}

	if t, err := p.atomOrCompound(allowComma, allowBar); err == nil {
		return t, nil
	}

	return nil, p.expectationError(0, nil)
}

func (p *Parser) atomOrCompound(allowComma bool, allowBar bool) (Term, error) {
	a, err := p.acceptAtom(allowComma, allowBar)
	if err != nil {
		return nil, err
	}

	if _, err := p.accept(TokenParenL); err != nil {
		if p.placeholder != "" && p.placeholder == a {
			if len(p.args) == 0 {
				return nil, errors.New("not enough arguments for placeholders")
			}
			var t Term
			t, p.args = p.args[0], p.args[1:]
			return t, nil
		}
		return a, nil
	}

	var args []Term
	for {
		t, err := p.expr(1, false, true)
		if err != nil {
			return nil, err
		}
		args = append(args, t)

		if _, err := p.accept(TokenParenR); err == nil {
			break
		}

		if _, err := p.accept(TokenComma); err != nil {
			return nil, fmt.Errorf("lhs: %w", err)
		}
	}

	return &Compound{Functor: a, Args: args}, nil
}

func (p *Parser) prefix(allowComma bool, allowBar bool) (Term, error) {
	op, err := p.acceptPrefix(allowComma, allowBar)
	if err != nil {
		return nil, err
	}
	_, r := op.bindingPowers()
	rhs, err := p.expr(r, allowComma, allowBar)
	if err != nil {
		return op.name, nil
	}
	return &Compound{
		Functor: op.name,
		Args:    []Term{rhs},
	}, nil
}

func (p *Parser) paren() (Term, error) {
	if _, err := p.accept(TokenParenL); err != nil {
		return nil, err
	}

	lhs, err := p.expr(1, true, true)
	if err != nil {
		return nil, err
	}

	if _, err := p.accept(TokenParenR); err != nil {
		return nil, err
	}

	return lhs, nil
}

func (p *Parser) variable() (Term, error) {
	v, err := p.accept(TokenVariable)
	if err != nil {
		return nil, err
	}

	if v == "_" {
		return NewVariable(), nil
	}

	if p.vars == nil {
		n := Variable(v)
		return n, nil
	}

	n := Atom(v)
	for i, v := range *p.vars {
		if v.Name == n {
			(*p.vars)[i].Count++
			return v.Variable, nil
		}
	}
	w := NewVariable()
	*p.vars = append(*p.vars, ParsedVariable{Name: n, Variable: w, Count: 1})
	return w, nil
}

func (p *Parser) block() (Term, error) {
	if _, err := p.accept(TokenBraceL); err != nil {
		return nil, err
	}
	lhs, err := p.expr(1, true, true)
	if err != nil {
		return nil, err
	}

	if _, err := p.accept(TokenBraceR); err != nil {
		return nil, err
	}

	return &Compound{
		Functor: "{}",
		Args:    []Term{lhs},
	}, nil
}

func (p *Parser) list() (Term, error) {
	if _, err := p.accept(TokenBracketL); err != nil {
		return nil, err
	}

	var es []Term
	for {
		e, err := p.expr(1, false, false)
		if err != nil {
			return nil, err
		}
		es = append(es, e)

		if _, err := p.accept(TokenBar); err == nil {
			rest, err := p.expr(1, true, true)
			if err != nil {
				return nil, err
			}

			if _, err := p.accept(TokenBracketR); err != nil {
				return nil, err
			}

			return ListRest(rest, es...), nil
		}

		if _, err := p.accept(TokenBracketR); err == nil {
			return List(es...), nil
		}

		if _, err := p.accept(TokenComma); err != nil {
			return nil, err
		}
	}
}

func (p *Parser) acceptDoubleQuoted() (Term, error) {
	v, err := p.accept(TokenDoubleQuoted)
	if err != nil {
		return nil, err
	}
	v = unDoubleQuote(v)
	switch p.doubleQuotes {
	case doubleQuotesCodes:
		var codes []Term
		for _, r := range v {
			codes = append(codes, Integer(r))
		}
		return List(codes...), nil
	case doubleQuotesChars:
		var chars []Term
		for _, r := range v {
			chars = append(chars, Atom(r))
		}
		return List(chars...), nil
	case doubleQuotesAtom:
		return Atom(v), nil
	default:
		return nil, fmt.Errorf("unknown double quote(%d)", p.doubleQuotes)
	}
}

// More checks if the parser has more tokens to read.
func (p *Parser) More() bool {
	_, err := p.accept(TokenEOF)
	return err != nil
}

type operatorSpecifier uint8

const (
	operatorSpecifierFX operatorSpecifier = iota
	operatorSpecifierFY
	operatorSpecifierXF
	operatorSpecifierYF
	operatorSpecifierXFX
	operatorSpecifierXFY
	operatorSpecifierYFX
)

func (s operatorSpecifier) term() Term {
	return [...]Term{
		operatorSpecifierFX:  Atom("fx"),
		operatorSpecifierFY:  Atom("fy"),
		operatorSpecifierXF:  Atom("xf"),
		operatorSpecifierYF:  Atom("yf"),
		operatorSpecifierXFX: Atom("xfx"),
		operatorSpecifierXFY: Atom("xfy"),
		operatorSpecifierYFX: Atom("yfx"),
	}[s]
}

type operators []operator

func (ops operators) find(name Atom, arity int) *operator {
	switch arity {
	case 1:
		for _, op := range ops {
			if op.name != name {
				continue
			}
			switch op.specifier {
			case operatorSpecifierFX, operatorSpecifierFY, operatorSpecifierXF, operatorSpecifierYF:
				return &op
			}
		}
	case 2:
		for _, op := range ops {
			if op.name != name {
				continue
			}
			switch op.specifier {
			case operatorSpecifierXFX, operatorSpecifierXFY, operatorSpecifierYFX:
				return &op
			}
		}
	default:
		return nil
	}
	return nil
}

type operator struct {
	priority  Integer // 1 ~ 1200
	specifier operatorSpecifier
	name      Atom
}

func (o *operator) bindingPowers() (int, int) {
	bp := 1201 - int(o.priority) // 1 ~ 1200
	switch o.specifier {
	case operatorSpecifierFX:
		return 0, bp + 1
	case operatorSpecifierFY:
		return 0, bp
	case operatorSpecifierXF:
		return bp + 1, 0
	case operatorSpecifierYF:
		return bp, -1
	case operatorSpecifierXFX:
		return bp + 1, bp + 1
	case operatorSpecifierXFY:
		return bp + 1, bp
	case operatorSpecifierYFX:
		return bp, bp + 1
	default:
		return 0, 0
	}
}

type doubleQuotes int

const (
	doubleQuotesCodes doubleQuotes = iota
	doubleQuotesChars
	doubleQuotesAtom
)

func (d doubleQuotes) String() string {
	return [...]string{
		doubleQuotesCodes: "codes",
		doubleQuotesChars: "chars",
		doubleQuotesAtom:  "atom",
	}[d]
}

type unexpectedTokenError struct {
	ExpectedKind TokenKind
	ExpectedVals []string
	Actual       Token
	History      []Token
}

func (e unexpectedTokenError) Error() string {
	return fmt.Sprintf("unexpected token: %s", e.Actual)
}

var doubleQuotedEscapePattern = regexp.MustCompile("\"\"|\\\\(?:[\\nabfnrtv\\\\'\"`]|(?:x[\\da-fA-F]+|[0-8]+)\\\\)")

func unDoubleQuote(s string) string {
	return doubleQuotedEscapePattern.ReplaceAllStringFunc(s[1:len(s)-1], doubleQuotedUnescape)
}

func doubleQuotedUnescape(s string) string {
	switch s {
	case `""`:
		return `"`
	case "\\\n":
		return ""
	case `\a`:
		return "\a"
	case `\b`:
		return "\b"
	case `\f`:
		return "\f"
	case `\n`:
		return "\n"
	case `\r`:
		return "\r"
	case `\t`:
		return "\t"
	case `\v`:
		return "\v"
	case `\\`:
		return `\`
	case `\'`:
		return `'`
	case `\"`:
		return `"`
	case "\\`":
		return "`"
	default: // `\x23\` or `\23\`
		s = s[1 : len(s)-1] // `x23` or `23`
		base := 8

		if s[0] == 'x' {
			s = s[1:]
			base = 16
		}

		r, _ := strconv.ParseInt(s, base, 4*8) // rune is up to 4 bytes
		return string(rune(r))
	}
}
