package prolog

import (
	"fmt"
)

type Parser struct {
	lexer   *Lexer
	current Token
}

func NewParser(input string) *Parser {
	p := Parser{
		lexer: NewLexer(input),
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

func (p *Parser) Clauses() ([]Term, error) {
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
	h, err := p.Pred()
	if err != nil {
		return nil, err
	}

	if _, err := p.accept(TokenAtom, ":-"); err != nil {
		if _, err := p.accept(TokenSeparator, "."); err != nil {
			return nil, err
		}

		return h, nil
	}

	args := []Term{h}
	for {
		b, err := p.Pred()
		if err != nil {
			return nil, err
		}
		args = append(args, b)

		sep, err := p.accept(TokenSeparator, ",", ".")
		if err != nil {
			return nil, err
		}
		if sep == "." {
			break
		}
	}

	return &Compound{Functor: ":-", Args: args}, nil
}

func (p *Parser) Pred() (Term, error) {
	a, err := p.accept(TokenAtom)
	if err != nil {
		return nil, err
	}

	if _, err := p.accept(TokenSeparator, "("); err != nil {
		return nil, err
	}

	var args []Term
	for {
		t, err := p.Arg()
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

	return &Compound{
		Functor: Atom(a),
		Args:    args,
	}, nil
}

func (p *Parser) Arg() (Term, error) {
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
		t, err := p.Arg()
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

type UnexpectedToken struct {
	ExpectedKind TokenKind
	ExpectedVals []string
	Actual       Token
}

func (e *UnexpectedToken) Error() string {
	return fmt.Sprintf("expected: <%s %s>, actual: %s", e.ExpectedKind, e.ExpectedVals, e.Actual)
}
