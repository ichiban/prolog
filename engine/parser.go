package engine

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"reflect"
	"strconv"

	"github.com/ichiban/prolog/internal"
)

// Parser turns bytes into Term.
type Parser struct {
	lexer       *internal.Lexer
	current     *internal.Token
	operators   *Operators
	vars        []variableWithCount
	placeholder Atom
	args        []Term
}

type variableWithCount struct {
	variable *Variable
	Count    int
}

// NewParser creates a Parser.
func NewParser(vm *VM, input *bufio.Reader) *Parser {
	p := Parser{
		lexer:     internal.NewLexer(input, vm.charConversions),
		operators: &vm.operators,
	}
	return &p
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
		t, err := p.lexer.Next()
		if err != nil {
			return "", err
		}
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

// Term parses a term followed by a full stop.
func (p *Parser) Term() (Term, error) {
	if _, err := p.expect(internal.TokenEOS); err == nil {
		return nil, io.EOF
	}

	// reset vars
	for i := range p.vars {
		p.vars[i] = variableWithCount{}
	}
	p.vars = p.vars[:0]

	t, err := p.expr(1)
	if err != nil {
		return nil, err
	}

	_, err = p.accept(internal.TokenSeparator, ".")
	switch e := err.(type) {
	case nil:
		if len(p.args) != 0 {
			return nil, systemError(fmt.Errorf("too many arguments for placeholders: %s", p.args))
		}
		return t, nil
	case *unexpectedToken:
		if e.Actual.Kind == internal.TokenEOS {
			return nil, syntaxErrorInsufficient()
		}
		return nil, syntaxErrorInvalidToken(Atom(e.Error()))
	default:
		return nil, systemError(err)
	}
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
			e, err := p.expr(1)
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
				rest, err := p.expr(1)
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
		if p.placeholder != "" && p.placeholder == Atom(a) {
			if len(p.args) == 0 {
				return nil, errors.New("not enough arguments for placeholders")
			}
			var t Term
			t, p.args = p.args[0], p.args[1:]
			return t, nil
		}
		return Atom(a), nil
	}

	var args []Term
	for {
		t, err := p.expr(1)
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

// More checks if the parser has more tokens to read.
func (p *Parser) More() bool {
	_, err := p.accept(internal.TokenEOS)
	return err != nil
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
