package prolog

import (
	"errors"
	"fmt"
	"sort"
)

var defaultOperators = operators{
	{Precedence: 1200, Type: xfx, Name: `:-`},
	{Precedence: 1200, Type: fx, Name: `:-`},
	{Precedence: 1200, Type: fx, Name: `?-`},
	{Precedence: 1100, Type: xfy, Name: `;`},
	{Precedence: 1000, Type: xfy, Name: `,`},
	{Precedence: 700, Type: xfx, Name: `<`},
	{Precedence: 700, Type: xfx, Name: `=`},
	{Precedence: 700, Type: xfx, Name: `=..`},
	{Precedence: 700, Type: xfx, Name: `=<`},
	{Precedence: 700, Type: xfx, Name: `=\=`},
	{Precedence: 700, Type: xfx, Name: `>`},
	{Precedence: 700, Type: xfx, Name: `>=`},
	{Precedence: 500, Type: yfx, Name: `+`},
	{Precedence: 500, Type: yfx, Name: `-`},
	{Precedence: 400, Type: yfx, Name: `*`},
	{Precedence: 400, Type: yfx, Name: `/`},
	{Precedence: 200, Type: fy, Name: `+`},
	{Precedence: 200, Type: fy, Name: `-`},
	{Precedence: 100, Type: yfx, Name: `.`},
}

type Parser struct {
	lexer     *Lexer
	current   Token
	operators operators
}

func NewParser(input string, operators operators) *Parser {
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
		return "", &unexpectedToken{
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
		return "", &unexpectedToken{
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

		c, err := p.Clause()
		if err != nil {
			return nil, err
		}
		ret = append(ret, c)
	}
}

func (p *Parser) Clause() (Term, error) {
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
			if l < 0 {
				continue
			}
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

type operators []operator

func (os operators) Len() int {
	return len(os)
}

func (os operators) Less(i, j int) bool {
	return os[i].Precedence > os[j].Precedence
}

func (os operators) Swap(i, j int) {
	os[i], os[j] = os[j], os[i]
}

func (os operators) AtMost(p int) operators {
	i := sort.Search(len(os), func(i int) bool { return os[i].Precedence <= p })
	if i == len(os) {
		return nil // not found
	}
	return os[i:]
}

type operator struct {
	Precedence int // 1 ~ 1200
	Type       operatorType
	Name       string
}

func (o *operator) leftRight() (int, int) {
	switch o.Type {
	case xf:
		return o.Precedence - 1, -1
	case yf:
		return o.Precedence, -1
	case xfx:
		return o.Precedence - 1, o.Precedence - 1
	case xfy:
		return o.Precedence - 1, o.Precedence
	case yfx:
		return o.Precedence, o.Precedence - 1
	case fx:
		return -1, o.Precedence - 1
	case fy:
		return -1, o.Precedence
	default:
		return -1, -1
	}
}

type operatorType byte

const (
	nao operatorType = iota // not an operator
	xf
	yf
	xfx
	xfy
	yfx
	fx
	fy
)

type unexpectedToken struct {
	ExpectedKind TokenKind
	ExpectedVals []string
	Actual       Token
}

func (e *unexpectedToken) Error() string {
	return fmt.Sprintf("expected: <%s %s>, actual: %s", e.ExpectedKind, e.ExpectedVals, e.Actual)
}
