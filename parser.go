package prolog

import (
	"bufio"
	"errors"
	"fmt"
	"strconv"

	"github.com/ichiban/prolog/internal"
)

// Parser turns bytes into Term.
type Parser struct {
	lexer     *internal.Lexer
	current   *internal.Token
	operators *Operators
	vars      []variableWithCount
}

type variableWithCount struct {
	variable *Variable
	Count    int
}

// NewParser creates a Parser.
func NewParser(input *bufio.Reader, operators *Operators, charConversions map[rune]rune) *Parser {
	p := Parser{
		lexer:     internal.NewLexer(input, charConversions),
		operators: operators,
	}
	return &p
}

func (p *Parser) accept(k internal.TokenKind, vals ...string) (string, error) {
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

		if _, err := p.accept(internal.TokenAtom, string(op.Name)); err != nil {
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

		if _, err := p.accept(internal.TokenAtom, string(op.Name)); err != nil {
			continue
		}

		return &op, nil
	}
	return nil, errors.New("no op")
}

func (p *Parser) expect(k internal.TokenKind, vals ...string) (string, error) {
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

// Clause parses a clause, term followed by a full stop.
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

	if _, err := p.accept(internal.TokenSeparator, "."); err != nil {
		return nil, fmt.Errorf("clause: %w", err)
	}

	return t, nil
}

// Term parses a term.
func (p *Parser) Term() (Term, error) {
	return p.expr(1)
}

// Number parses a number.
func (p *Parser) Number() (Term, error) {
	if f, err := p.accept(internal.TokenFloat); err == nil {
		n, _ := strconv.ParseFloat(f, 64)
		return Float(n), nil
	}

	if i, err := p.accept(internal.TokenInteger); err == nil {
		n, _ := strconv.Atoi(i)
		return Integer(n), nil
	}

	return nil, SyntaxError(Atom("not_a_number"), Atom("not a number."))
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
	if _, err := p.accept(internal.TokenSeparator, "("); err == nil {
		lhs, err := p.expr(1)
		if err != nil {
			return nil, err
		}

		if _, err := p.accept(internal.TokenSeparator, ")"); err != nil {
			return nil, err
		}

		return lhs, nil
	}

	if _, err := p.accept(internal.TokenSeparator, "["); err == nil {
		var es []Term
		for {
			e, err := p.Term()
			if err != nil {
				return nil, err
			}
			es = append(es, e)

			s, err := p.accept(internal.TokenSeparator, ",", "|", "]")
			if err != nil {
				return nil, err
			}
			switch s {
			case "|":
				rest, err := p.Term()
				if err != nil {
					return nil, err
				}

				if _, err := p.accept(internal.TokenSeparator, "]"); err != nil {
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

	if f, err := p.accept(internal.TokenFloat); err == nil {
		n, _ := strconv.ParseFloat(f, 64)
		return Float(n), nil
	}

	if i, err := p.accept(internal.TokenInteger); err == nil {
		n, _ := strconv.Atoi(i)
		return Integer(n), nil
	}

	if v, err := p.accept(internal.TokenVariable); err == nil {
		if v == "_" {
			return &Variable{}, nil
		}
		for i, e := range p.vars {
			if e.variable.Name == v {
				p.vars[i].Count++
				return e.variable, nil
			}
		}
		n := &Variable{Name: v}
		p.vars = append(p.vars, variableWithCount{variable: n, Count: 1})
		return n, nil
	}

	a, err := p.accept(internal.TokenAtom)
	if err != nil {
		return nil, err
	}

	if _, err := p.accept(internal.TokenSeparator, "("); err != nil {
		return Atom(a), nil
	}

	var args []Term
	for {
		t, err := p.Term()
		if err != nil {
			return nil, err
		}
		args = append(args, t)

		sep, err := p.accept(internal.TokenSeparator, ",", ")")
		if err != nil {
			return nil, fmt.Errorf("lhs: %w", err)
		}
		if sep == ")" {
			break
		}
	}

	return &Compound{Functor: Atom(a), Args: args}, nil
}

// Operators are a list of operators sorted in a descending order of precedence.
type Operators []Operator

// Operator is an operator definition.
type Operator struct {
	Priority  Integer // 1 ~ 1200
	Specifier Atom    // xf, yf, xfx, xfy, yfx, fx, or fy
	Name      Atom
}

func (o *Operator) bindingPowers() (int, int) {
	bp := 1201 - int(o.Priority) // 1 ~ 1200
	switch o.Specifier {
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
	ExpectedKind internal.TokenKind
	ExpectedVals []string
	Actual       internal.Token
}

func (e *unexpectedToken) Error() string {
	return fmt.Sprintf("expected: <%s %s>, actual: %s", e.ExpectedKind, e.ExpectedVals, e.Actual)
}
