package term

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"reflect"
	"strconv"
	"strings"

	"github.com/ichiban/prolog/syntax"
)

// Parser turns bytes into Interface.
type Parser struct {
	lexer        *syntax.Lexer
	current      *syntax.Token
	history      []syntax.Token
	operators    *Operators
	placeholder  Atom
	args         []Interface
	doubleQuotes DoubleQuotes
	vars         *[]ParsedVariable
}

// ParsedVariable is a set of information regarding a variable in a parsed term.
type ParsedVariable struct {
	Name     Atom
	Variable Variable
	Count    int
}

// NewParser creates a Parser.
func NewParser(input *bufio.Reader, charConversions map[rune]rune, opts ...ParserOption) *Parser {
	p := Parser{
		lexer: syntax.NewLexer(input, charConversions),
	}
	for _, o := range opts {
		o(&p)
	}
	return &p
}

// ParserOption is option for NewParser.
type ParserOption func(p *Parser)

// WithOperators sets operators for Parser.
func WithOperators(operators *Operators) ParserOption {
	return func(p *Parser) {
		p.operators = operators
	}
}

// WithDoubleQuotes sets how Parser handles double quotes.
func WithDoubleQuotes(quotes DoubleQuotes) ParserOption {
	return func(p *Parser) {
		p.doubleQuotes = quotes
	}
}

// WithParsedVars sets where Parser to store information regarding parsed variables.
func WithParsedVars(vars *[]ParsedVariable) ParserOption {
	return func(p *Parser) {
		p.vars = vars
	}
}

// Replace registers placeholder and its arguments. Every occurrence of placeholder will be replaced by arguments.
// Mismatch of the number of occurrences of placeholder and the number of arguments raises an error.
func (p *Parser) Replace(placeholder Atom, args ...interface{}) error {
	p.placeholder = placeholder
	p.args = make([]Interface, len(args))
	for i, a := range args {
		var err error
		p.args[i], err = termOf(reflect.ValueOf(a))
		if err != nil {
			return err
		}
	}
	return nil
}

func termOf(o reflect.Value) (Interface, error) {
	if t, ok := o.Interface().(Interface); ok {
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
		es := make([]Interface, l)
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

func (p *Parser) accept(k syntax.TokenKind, vals ...string) (string, error) {
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

func (p *Parser) acceptAtom(allowComma bool, vals ...string) (Atom, error) {
	if v, err := p.accept(syntax.TokenIdent, vals...); err == nil {
		return Atom(v), nil
	}
	if v, err := p.accept(syntax.TokenQuotedIdent, quoteSlice(vals)...); err == nil {
		return Atom(unquote(v)), nil
	}
	if v, err := p.accept(syntax.TokenGraphic, vals...); err == nil {
		return Atom(v), nil
	}
	if allowComma {
		if v, err := p.accept(syntax.TokenComma, vals...); err == nil {
			return Atom(v), nil
		}
	}
	if v, err := p.accept(syntax.TokenSign, vals...); err == nil {
		return Atom(v), nil
	}
	if v, err := p.accept(syntax.TokenBar, vals...); err == nil {
		return Atom(v), nil
	}
	return "", errors.New("not an atom")
}

func quotedIdentUnescape(s string) string {
	switch s {
	case "''":
		return "'"
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

func (p *Parser) acceptOp(min int, allowComma bool) (*Operator, error) {
	if p.operators == nil {
		return nil, errors.New("no op")
	}
	for _, op := range *p.operators {
		l, _ := op.bindingPowers()
		if l < min {
			continue
		}

		if _, err := p.acceptAtom(allowComma, string(op.Name)); err != nil {
			continue
		}

		return &op, nil
	}
	return nil, errors.New("no op")
}

func (p *Parser) acceptPrefix(allowComma bool) (*Operator, error) {
	if p.operators == nil {
		return nil, errors.New("no op")
	}
	for _, op := range *p.operators {
		l, _ := op.bindingPowers()
		if l != 0 {
			continue
		}

		if _, err := p.acceptAtom(allowComma, string(op.Name)); err != nil {
			continue
		}

		return &op, nil
	}
	return nil, errors.New("no op")
}

func (p *Parser) expect(k syntax.TokenKind, vals ...string) (string, error) {
	if p.current == nil {
		t, err := p.lexer.Next()
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

func (p *Parser) expectationError(k syntax.TokenKind, vals []string) error {
	if p.current.Kind == syntax.TokenEOS {
		return syntax.ErrInsufficient
	}
	return &UnexpectedTokenError{
		ExpectedKind: k,
		ExpectedVals: vals,
		Actual:       *p.current,
		History:      p.history,
	}
}

// Term parses a term followed by a full stop.
func (p *Parser) Term() (Interface, error) {
	if _, err := p.accept(syntax.TokenEOS); err == nil {
		return nil, io.EOF
	}

	if p.vars != nil {
		// reset vars
		for i := range *p.vars {
			(*p.vars)[i] = ParsedVariable{}
		}
		*p.vars = (*p.vars)[:0]
	}

	t, err := p.expr(1, true)
	if err != nil {
		return nil, err
	}

	if _, err := p.accept(syntax.TokenPeriod); err != nil {
		return nil, err
	}

	if len(p.args) != 0 {
		return nil, fmt.Errorf("too many arguments for placeholders: %s", p.args)
	}

	return t, nil
}

var ErrNotANumber = errors.New("not a number")

// Number parses a number term.
func (p *Parser) Number() (Interface, error) {
	n, err := p.number()
	if err != nil {
		return nil, err
	}

	_, err = p.accept(syntax.TokenEOS)
	return n, err
}

func (p *Parser) number() (Interface, error) {
	sign, _ := p.accept(syntax.TokenSign)

	if f, err := p.accept(syntax.TokenFloat); err == nil {
		f = sign + f
		n, _ := strconv.ParseFloat(f, 64)
		return Float(n), nil
	}

	if i, err := p.accept(syntax.TokenInteger); err == nil {
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

	return nil, ErrNotANumber
}

// based on Pratt parser explained in this article: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
func (p *Parser) expr(min int, allowComma bool) (Interface, error) {
	lhs, err := p.lhs(allowComma)
	if err != nil {
		return nil, err
	}

	for {
		op, err := p.acceptOp(min, allowComma)
		if err != nil {
			break
		}

		_, r := op.bindingPowers()
		rhs, err := p.expr(r, allowComma)
		if err != nil {
			return nil, err
		}

		lhs = &Compound{
			Functor: op.Name,
			Args:    []Interface{lhs, rhs},
		}
	}

	return lhs, nil
}

func (p *Parser) lhs(allowComma bool) (Interface, error) {
	if _, err := p.accept(syntax.TokenEOS); err == nil {
		return nil, syntax.ErrInsufficient
	}

	if _, err := p.accept(syntax.TokenParenL); err == nil {
		lhs, err := p.expr(1, true)
		if err != nil {
			return nil, err
		}

		if _, err := p.accept(syntax.TokenParenR); err != nil {
			return nil, err
		}

		return lhs, nil
	}

	if _, err := p.accept(syntax.TokenBraceL); err == nil {
		lhs, err := p.expr(1, true)
		if err != nil {
			return nil, err
		}

		if _, err := p.accept(syntax.TokenBraceR); err != nil {
			return nil, err
		}

		return &Compound{
			Functor: "{}",
			Args:    []Interface{lhs},
		}, nil
	}

	if t, err := p.number(); err == nil {
		return t, nil
	}

	if op, err := p.acceptPrefix(allowComma); err == nil {
		_, r := op.bindingPowers()
		rhs, err := p.expr(r, allowComma)
		if err != nil {
			return op.Name, nil
		}
		return &Compound{
			Functor: op.Name,
			Args:    []Interface{rhs},
		}, nil
	}

	if v, err := p.accept(syntax.TokenVariable); err == nil {
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
		v := NewVariable()
		*p.vars = append(*p.vars, ParsedVariable{Name: n, Variable: v, Count: 1})
		return v, nil
	}

	if v, err := p.accept(syntax.TokenDoubleQuoted); err == nil {
		switch p.doubleQuotes {
		case DoubleQuotesCodes:
			var codes []Interface
			for _, r := range v {
				codes = append(codes, Integer(r))
			}
			return List(codes...), nil
		case DoubleQuotesChars:
			var chars []Interface
			for _, r := range v {
				chars = append(chars, Atom(r))
			}
			return List(chars...), nil
		case DoubleQuotesAtom:
			return Atom(v), nil
		default:
			return nil, fmt.Errorf("unknown double quote(%d)", p.doubleQuotes)
		}
	}

	if a, err := p.acceptAtom(allowComma); err == nil {
		if _, err := p.accept(syntax.TokenParenL); err != nil {
			if p.placeholder != "" && p.placeholder == a {
				if len(p.args) == 0 {
					return nil, errors.New("not enough arguments for placeholders")
				}
				var t Interface
				t, p.args = p.args[0], p.args[1:]
				return t, nil
			}
			return a, nil
		}

		var args []Interface
		for {
			t, err := p.expr(1, false)
			if err != nil {
				return nil, err
			}
			args = append(args, t)

			if _, err := p.accept(syntax.TokenParenR); err == nil {
				break
			}

			if _, err := p.accept(syntax.TokenComma); err != nil {
				return nil, fmt.Errorf("lhs: %w", err)
			}
		}

		return &Compound{Functor: a, Args: args}, nil
	}

	if _, err := p.accept(syntax.TokenBracketL); err == nil {
		var es []Interface
		for {
			e, err := p.expr(1, false)
			if err != nil {
				return nil, err
			}
			es = append(es, e)

			if _, err := p.accept(syntax.TokenBar); err == nil {
				rest, err := p.expr(1, true)
				if err != nil {
					return nil, err
				}

				if _, err := p.accept(syntax.TokenBracketR); err != nil {
					return nil, err
				}

				return ListRest(rest, es...), nil
			}

			if _, err := p.accept(syntax.TokenBracketR); err == nil {
				return List(es...), nil
			}

			if _, err := p.accept(syntax.TokenComma); err != nil {
				return nil, err
			}
		}
	}

	return nil, fmt.Errorf("failed to parse: %v, history=%#v", p.current, p.history)
}

// More checks if the parser has more tokens to read.
func (p *Parser) More() bool {
	_, err := p.accept(syntax.TokenEOS)
	return err != nil
}

type OperatorSpecifier uint8

const (
	OperatorSpecifierNone OperatorSpecifier = iota
	OperatorSpecifierFX
	OperatorSpecifierFY
	OperatorSpecifierXF
	OperatorSpecifierYF
	OperatorSpecifierXFX
	OperatorSpecifierXFY
	OperatorSpecifierYFX

	operatorSpecifierLen
)

func (s OperatorSpecifier) Term() Interface {
	return [operatorSpecifierLen]Interface{
		OperatorSpecifierFX:  Atom("fx"),
		OperatorSpecifierFY:  Atom("fy"),
		OperatorSpecifierXF:  Atom("xf"),
		OperatorSpecifierYF:  Atom("yf"),
		OperatorSpecifierXFX: Atom("xfx"),
		OperatorSpecifierXFY: Atom("xfy"),
		OperatorSpecifierYFX: Atom("yfx"),
	}[s]
}

// Operators are a list of operators sorted in a descending order of precedence.
type Operators []Operator

// Operator is an operator definition.
type Operator struct {
	Priority  Integer // 1 ~ 1200
	Specifier OperatorSpecifier
	Name      Atom
}

func (o *Operator) bindingPowers() (int, int) {
	bp := 1201 - int(o.Priority) // 1 ~ 1200
	switch o.Specifier {
	case OperatorSpecifierFX:
		return 0, bp + 1
	case OperatorSpecifierFY:
		return 0, bp
	case OperatorSpecifierXF:
		return bp + 1, 0
	case OperatorSpecifierYF:
		return bp, -1
	case OperatorSpecifierXFX:
		return bp + 1, bp + 1
	case OperatorSpecifierXFY:
		return bp + 1, bp
	case OperatorSpecifierYFX:
		return bp, bp + 1
	default:
		return 0, 0
	}
}

type DoubleQuotes int

const (
	DoubleQuotesCodes DoubleQuotes = iota
	DoubleQuotesChars
	DoubleQuotesAtom
	doubleQuotesLen
)

func (d DoubleQuotes) String() string {
	return [doubleQuotesLen]string{
		DoubleQuotesCodes: "codes",
		DoubleQuotesChars: "chars",
		DoubleQuotesAtom:  "atom",
	}[d]
}

type UnexpectedTokenError struct {
	ExpectedKind syntax.TokenKind
	ExpectedVals []string
	Actual       syntax.Token
	History      []syntax.Token
}

func (e UnexpectedTokenError) Error() string {
	return fmt.Sprintf("unexpected token: %s", e.Actual)
}
