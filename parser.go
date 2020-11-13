package prolog

import (
	"errors"
	"fmt"
	"sort"
)

var DefaultOperators = Operators{
	{Precedence: 1200, Type: XFX, Name: `:-`},
	{Precedence: 1200, Type: FX, Name: `:-`},
	{Precedence: 1200, Type: FX, Name: `?-`},
	{Precedence: 1100, Type: XFY, Name: `;`},
	{Precedence: 1000, Type: XFY, Name: `,`},
	{Precedence: 700, Type: XFX, Name: `<`},
	{Precedence: 700, Type: XFX, Name: `=`},
	{Precedence: 700, Type: XFX, Name: `=..`},
	{Precedence: 700, Type: XFX, Name: `=<`},
	{Precedence: 700, Type: XFX, Name: `=\=`},
	{Precedence: 700, Type: XFX, Name: `>`},
	{Precedence: 700, Type: XFX, Name: `>=`},
	{Precedence: 500, Type: YFX, Name: `+`},
	{Precedence: 500, Type: YFX, Name: `-`},
	{Precedence: 400, Type: YFX, Name: `*`},
	{Precedence: 400, Type: YFX, Name: `/`},
	{Precedence: 200, Type: FY, Name: `+`},
	{Precedence: 200, Type: FY, Name: `-`},
	{Precedence: 100, Type: YFX, Name: `.`},
}

func init() {
	sort.Sort(DefaultOperators)
}

type Parser struct {
	lexer     *Lexer
	current   Token
	operators Operators
}

func NewParser(input string, operators Operators) *Parser {
	p := Parser{
		lexer:     NewLexer(input),
		operators: operators,
	}
	p.current = p.lexer.Next()
	return &p
}

func (p *Parser) accept(k TokenKind, vals ...string) (string, error) {
	v, err := p.expect(k, vals...)
	if err != nil {
		return "", err
	}
	p.current = p.lexer.Next()
	return v, nil
}

func (p *Parser) expect(k TokenKind, vals ...string) (string, error) {
	if p.current.Kind != k {
		return "", &UnexpectedToken{
			ExpectedKind: k,
			ExpectedVals: vals,
			Actual:       p.current,
		}
	}

	if len(vals) > 0 {
		for _, v := range vals {
			if v == p.current.Val {
				return v, nil
			}
		}
		return "", &UnexpectedToken{
			ExpectedKind: k,
			ExpectedVals: vals,
			Actual:       p.current,
		}
	}

	return p.current.Val, nil
}

func (p *Parser) Program() ([]Term, error) {
	var ret []Term
	for {
		if _, err := p.accept(TokenEOS); err == nil {
			return ret, nil
		}

		c, err := p.clause()
		if err != nil {
			return nil, err
		}
		ret = append(ret, c)
	}
}

func (p *Parser) clause() (Term, error) {
	t, err := p.Term()
	if err != nil {
		return nil, err
	}

	if _, err := p.accept(TokenSeparator, "."); err != nil {
		return nil, err
	}

	return t, nil
}

func (p *Parser) Term() (Term, error) {
	if _, err := p.accept(TokenSeparator, "("); err == nil {
		t, err := p.Term()
		if err != nil {
			return nil, err
		}
		if _, err := p.accept(TokenSeparator, ")"); err != nil {
			return nil, err
		}
		return t, nil
	}

	return p.expr(1200)
}

// based on Pratt parser explained in this article: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
func (p *Parser) expr(max int) (Term, error) {
	if t, err := p.prefixUnary(max); err == nil {
		return t, nil
	}

	lhs, err := p.expr0()
	if err != nil {
		return nil, err
	}

loop:
	for {
		for _, op := range p.operators.AtMost(max) {
			if _, err := p.accept(TokenAtom, op.Name); err != nil {
				continue
			}

			l, r := op.leftRight()
			if l > max {
				break loop
			}

			rhs, err := p.expr(r)
			if err != nil {
				return nil, err
			}

			lhs = &Compound{
				Functor: Atom(op.Name),
				Args:    []Term{lhs, rhs},
			}
		}
		break
	}

	return lhs, nil
}

func (p *Parser) prefixUnary(max int) (Term, error) {
	for _, op := range p.operators.AtMost(max) {
		l, r := op.leftRight()
		if l >= 0 {
			continue
		}

		if _, err := p.accept(TokenAtom, op.Name); err != nil {
			continue
		}

		x, err := p.expr(r)
		if err != nil {
			return nil, err
		}

		return &Compound{
			Functor: Atom(op.Name),
			Args:    []Term{x},
		}, nil
	}

	return nil, errors.New("not unary")
}

func (p *Parser) expr0() (Term, error) {
	a, err := p.accept(TokenAtom)
	if err != nil {
		v, err := p.accept(TokenVariable)
		if err != nil {
			return nil, err
		}
		return &Variable{
			Name: v,
		}, nil
	}

	if _, err := p.accept(TokenSeparator, "("); err != nil {
		return Atom(a), nil
	}

	var args []Term
	for {
		t, err := p.Term()
		if err != nil {
			return nil, err
		}
		args = append(args, t)

		sep, err := p.accept(TokenSeparator, ",", ")")
		if err != nil {
			return nil, err
		}
		if sep == ")" {
			break
		}
	}

	return &Compound{Functor: Atom(a), Args: args}, nil
}

type Operators []Operator

func (os Operators) Len() int {
	return len(os)
}

func (os Operators) Less(i, j int) bool {
	return os[i].Precedence > os[j].Precedence
}

func (os Operators) Swap(i, j int) {
	os[i], os[j] = os[j], os[i]
}

func (os Operators) AtMost(p int) Operators {
	i := sort.Search(len(os), func(i int) bool { return os[i].Precedence <= p })
	if i == len(os) {
		return nil // not found
	}
	return os[i:]
}

type Operator struct {
	Precedence int // 1 ~ 1200
	Type       OperatorType
	Name       string
}

func (o *Operator) leftRight() (int, int) {
	switch o.Type {
	case XF:
		return o.Precedence - 1, -1
	case YF:
		return o.Precedence, -1
	case XFX:
		return o.Precedence - 1, o.Precedence - 1
	case XFY:
		return o.Precedence - 1, o.Precedence
	case YFX:
		return o.Precedence, o.Precedence - 1
	case FX:
		return -1, o.Precedence - 1
	case FY:
		return -1, o.Precedence
	default:
		return -1, -1
	}
}

type OperatorType byte

const (
	NAO OperatorType = iota // not an operator
	XF
	YF
	XFX
	XFY
	YFX
	FX
	FY
)

type UnexpectedToken struct {
	ExpectedKind TokenKind
	ExpectedVals []string
	Actual       Token
}

func (e *UnexpectedToken) Error() string {
	return fmt.Sprintf("expected: <%s %s>, actual: %s", e.ExpectedKind, e.ExpectedVals, e.Actual)
}
