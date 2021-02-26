package prolog

import (
	"bufio"
	"errors"
	"fmt"
	"strconv"
)

type Parser struct {
	lexer     *Lexer
	current   *Token
	operators *Operators
	vars      []variableWithCount
}

type variableWithCount struct {
	variable *Variable
	count    int
}

func NewParser(input *bufio.Reader, operators *Operators, charConversions map[rune]rune) *Parser {
	p := Parser{
		lexer:     NewLexer(input, charConversions),
		operators: operators,
	}
	return &p
}

func (p *Parser) accept(k TokenKind, vals ...string) (string, error) {
	v, err := p.expect(k, vals...)
	if err != nil {
		return "", err
	}
	p.current = nil
	return v, nil
}

func (p *Parser) acceptOp(min int) (*Operator, error) {
	for _, op := range *p.operators {
		l, _ := op.bindingPowers()
		if l < min {
			continue
		}

		if _, err := p.accept(TokenAtom, string(op.Name)); err != nil {
			continue
		}

		return &op, nil
	}
	return nil, errors.New("no op")
}

func (p *Parser) acceptPrefix() (*Operator, error) {
	for _, op := range *p.operators {
		l, _ := op.bindingPowers()
		if l != 0 {
			continue
		}

		if _, err := p.accept(TokenAtom, string(op.Name)); err != nil {
			continue
		}

		return &op, nil
	}
	return nil, errors.New("no op")
}

func (p *Parser) expect(k TokenKind, vals ...string) (string, error) {
	if p.current == nil {
		t := p.lexer.Next()
		p.current = &t
	}

	if p.current.Kind != k {
		return "", &unexpectedToken{
			ExpectedKind: k,
			ExpectedVals: vals,
			Actual:       *p.current,
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
			Actual:       *p.current,
		}
	}

	return p.current.Val, nil
}

func (p *Parser) Clause() (Term, error) {
	// reset vars
	for i := range p.vars {
		p.vars[i] = variableWithCount{}
	}
	p.vars = p.vars[:0]

	t, err := p.Term()
	if err != nil {
		return nil, err
	}

	if _, err := p.accept(TokenSeparator, "."); err != nil {
		return nil, fmt.Errorf("clause: %w", err)
	}

	return t, nil
}

func (p *Parser) Term() (Term, error) {
	return p.expr(1)
}

// based on Pratt parser explained in this article: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
func (p *Parser) expr(min int) (Term, error) {
	lhs, err := p.lhs()
	if err != nil {
		return nil, err
	}

	for {
		op, err := p.acceptOp(min)
		if err != nil {
			break
		}

		_, r := op.bindingPowers()
		rhs, err := p.expr(r)
		if err != nil {
			return nil, err
		}

		lhs = &Compound{
			Functor: op.Name,
			Args:    []Term{lhs, rhs},
		}
	}

	return lhs, nil
}

func (p *Parser) lhs() (Term, error) {
	if _, err := p.accept(TokenSeparator, "("); err == nil {
		lhs, err := p.expr(1)
		if err != nil {
			return nil, err
		}

		if _, err := p.accept(TokenSeparator, ")"); err != nil {
			return nil, err
		}

		return lhs, nil
	}

	if _, err := p.accept(TokenSeparator, "["); err == nil {
		var es []Term
		for {
			e, err := p.Term()
			if err != nil {
				return nil, err
			}
			es = append(es, e)

			s, err := p.accept(TokenSeparator, ",", "|", "]")
			if err != nil {
				return nil, err
			}
			switch s {
			case "|":
				rest, err := p.Term()
				if err != nil {
					return nil, err
				}

				if _, err := p.accept(TokenSeparator, "]"); err != nil {
					return nil, err
				}

				return ListRest(rest, es...), nil
			case "]":
				return List(es...), nil
			}
		}
	}

	if op, err := p.acceptPrefix(); err == nil {
		_, r := op.bindingPowers()
		rhs, err := p.expr(r)
		if err != nil {
			return nil, err
		}
		return &Compound{
			Functor: op.Name,
			Args:    []Term{rhs},
		}, nil
	}

	if f, err := p.accept(TokenFloat); err == nil {
		n, _ := strconv.ParseFloat(f, 64)
		return Float(n), nil
	}

	if i, err := p.accept(TokenInteger); err == nil {
		n, _ := strconv.Atoi(i)
		return Integer(n), nil
	}

	if v, err := p.accept(TokenVariable); err == nil {
		if v == "_" {
			return &Variable{}, nil
		}
		for i, e := range p.vars {
			if e.variable.Name == v {
				p.vars[i].count++
				return e.variable, nil
			}
		}
		n := &Variable{Name: v}
		p.vars = append(p.vars, variableWithCount{variable: n, count: 1})
		return n, nil
	}

	a, err := p.accept(TokenAtom)
	if err != nil {
		return nil, err
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
			return nil, fmt.Errorf("lhs: %w", err)
		}
		if sep == ")" {
			break
		}
	}

	return &Compound{Functor: Atom(a), Args: args}, nil
}

type Operators []Operator

type Operator struct {
	Precedence Integer // 1 ~ 1200
	Type       Atom
	Name       Atom
}

func (o *Operator) bindingPowers() (int, int) {
	bp := 1201 - int(o.Precedence) // 1 ~ 1200
	switch o.Type {
	case "xf":
		return bp + 1, 0
	case "yf":
		return bp, -1
	case "xfx":
		return bp + 1, bp + 1
	case "xfy":
		return bp + 1, bp
	case "yfx":
		return bp, bp + 1
	case "fx":
		return 0, bp + 1
	case "fy":
		return 0, bp
	default:
		return 0, 0
	}
}

type unexpectedToken struct {
	ExpectedKind TokenKind
	ExpectedVals []string
	Actual       Token
}

func (e *unexpectedToken) Error() string {
	return fmt.Sprintf("expected: <%s %s>, actual: %s", e.ExpectedKind, e.ExpectedVals, e.Actual)
}
