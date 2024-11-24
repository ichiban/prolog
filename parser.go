package prolog

import (
	"fmt"
	"io"
	"math/big"
	"regexp"
	"strconv"
	"strings"

	"github.com/ichiban/prolog/v2/internal/ring"
)

// Parser turns bytes into Term.
type Parser struct {
	lexer        lexer
	operators    Operators
	doubleQuotes doubleQuotes

	buf *ring.Buffer[token]
}

// ParsedVariable is a set of information regarding a variable in a parsed term.
type ParsedVariable struct {
	Name     string
	Variable Variable
	Count    int
}

// NewParser creates a new parser from the current VM and io.RuneReader.
func NewParser(r io.RuneReader, ops Operators, doubleQuotes doubleQuotes) *Parser {
	return &Parser{
		lexer: lexer{
			input: ring.NewRuneReader(r, 4),
		},
		operators:    ops,
		doubleQuotes: doubleQuotes,
		buf:          ring.NewBuffer[token](4),
	}
}

func (p *Parser) next() (token, error) {
	if p.buf.Empty() {
		t, err := p.lexer.Token()
		if err != nil {
			return token{}, err
		}
		p.buf.Put(t)
	}
	return p.buf.Get(), nil
}

func (p *Parser) backup() {
	p.buf.Backup()
}

func (p *Parser) current() token {
	return p.buf.Current()
}

// Term parses a term followed by a full stop.
func (p *Parser) Term(h *Heap) (_ Term, _ []ParsedVariable, err error) {
	snapshot := *h
	defer func() {
		if err != nil {
			*h = snapshot
		}
	}()

	var pvs []ParsedVariable
	t, ok, err := p.term(h, &pvs, 1201)
	if err != nil {
		return Term{}, nil, err
	}
	if !ok {
		return Term{}, nil, &SyntaxError{impDepAtom: fmt.Sprintf("unexpected token: %s", p.current())}
	}

	switch t, _ := p.next(); t.kind {
	case tokenEnd:
		break
	default:
		p.backup()
		return Term{}, nil, &SyntaxError{impDepAtom: fmt.Sprintf("unexpected token: %s", p.current())}
	}

	return t, pvs, nil
}

// Number parses a number term.
func (p *Parser) Number(h *Heap) (_ Term, err error) {
	snapshot := *h
	defer func() {
		if err != nil {
			*h = snapshot
		}
	}()

	var n Term
	t, err := p.next()
	if err != nil {
		return Term{}, err
	}
	switch t.kind {
	case tokenInteger:
		n, err = integer(h, 1, t.val)
	case tokenFloatNumber:
		n, err = float(h, 1, t.val)
	default:
		p.backup()
		var (
			a  string
			ok bool
		)
		a, ok, err = p.name()
		if err != nil {
			return Term{}, err
		}
		if !ok {
			return Term{}, &SyntaxError{impDepAtom: "not_a_number"}
		}

		if a != "-" {
			p.backup()
			return Term{}, &SyntaxError{impDepAtom: "not_a_number"}
		}

		t, err = p.next()
		if err != nil {
			return Term{}, &SyntaxError{impDepAtom: "not_a_number"}
		}
		switch t.kind {
		case tokenInteger:
			n, err = integer(h, -1, t.val)
		case tokenFloatNumber:
			n, err = float(h, -1, t.val)
		default:
			p.backup()
			p.backup()
			return Term{}, &SyntaxError{impDepAtom: "not_a_number"}
		}
	}
	if err != nil {
		return Term{}, err
	}

	// No more runes after a number.
	switch _, err := p.lexer.rawNext(); err {
	case io.EOF:
		return n, nil
	default:
		return Term{}, &SyntaxError{impDepAtom: "not_a_number"}
	}
}

// More checks if the parser has more tokens to read.
func (p *Parser) More() bool {
	if _, err := p.next(); err != nil {
		return false
	}
	p.backup()
	return true
}

type doubleQuotes int

const (
	doubleQuotesChars doubleQuotes = iota
	doubleQuotesCodes
	doubleQuotesAtom
)

var doubleQuoteNames = [...]string{
	doubleQuotesCodes: "codes",
	doubleQuotesChars: "chars",
	doubleQuotesAtom:  "atom",
}

func (d doubleQuotes) String() string {
	return doubleQuoteNames[d]
}

// Loosely based on Pratt parser explained in this article: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
func (p *Parser) term(h *Heap, pvs *[]ParsedVariable, maxPriority int) (Term, bool, error) {
	var lhs Term
	switch op, ok, err := p.prefix(maxPriority); {
	case err != nil:
		return Term{}, false, err
	case !ok:
		lhs, ok, err = p.term0(h, pvs, maxPriority)
		if err != nil || !ok {
			return Term{}, ok, err
		}
	default:
		_, rbp := op.bindingPriorities()
		t, ok, err := p.term(h, pvs, rbp)
		if err != nil {
			return Term{}, false, err
		}
		if !ok {
			p.backup()
			return p.term0(h, pvs, maxPriority)
		}
		lhs, err = NewCompound(h, op.name, t)
		if err != nil {
			return Term{}, false, err
		}
	}

	for {
		op, ok, err := p.infix(maxPriority)
		if err != nil {
			return Term{}, false, err
		}
		if !ok {
			break
		}

		switch _, rbp := op.bindingPriorities(); {
		case rbp > 1200:
			var err error
			lhs, err = NewCompound(h, op.name, lhs)
			if err != nil {
				return Term{}, false, err
			}
		default:
			rhs, ok, err := p.term(h, pvs, rbp)
			if err != nil || !ok {
				return Term{}, ok, err
			}
			lhs, err = NewCompound(h, op.name, lhs, rhs)
			if err != nil {
				return Term{}, false, err
			}
		}
	}

	return lhs, true, nil
}

func (p *Parser) prefix(maxPriority int) (operator, bool, error) {
	a, ok, err := p.op(maxPriority)
	if err != nil || !ok {
		return operator{}, ok, err
	}

	if a == "-" {
		t, err := p.next()
		if err != nil {
			return operator{}, false, err
		}
		switch t.kind {
		case tokenInteger, tokenFloatNumber:
			p.backup()
			p.backup()
			return operator{}, false, nil
		default:
			p.backup()
		}
	}

	t, err := p.next()
	if err != nil {
		return operator{}, false, err
	}
	switch t.kind {
	case tokenOpenCT:
		p.backup()
		p.backup()
		return operator{}, false, nil
	default:
		p.backup()
	}

	op, ok := p.operators.ops[opKey{name: a, opClass: operatorClassPrefix}]
	if !ok || op.priority > maxPriority {
		p.backup()
		return operator{}, false, nil
	}
	return op, true, nil
}

func (p *Parser) infix(maxPriority int) (operator, bool, error) {
	a, ok, err := p.op(maxPriority)
	if err != nil || !ok {
		return operator{}, ok, err
	}

	if op := p.operators.ops[opKey{name: a, opClass: operatorClassInfix}]; op != (operator{}) {
		l, _ := op.bindingPriorities()
		if l <= maxPriority {
			return op, true, nil
		}
	}
	if op := p.operators.ops[opKey{name: a, opClass: operatorClassPostfix}]; op != (operator{}) {
		l, _ := op.bindingPriorities()
		if l <= maxPriority {
			return op, true, nil
		}
	}

	p.backup()
	return operator{}, false, nil
}

func (p *Parser) op(maxPriority int) (string, bool, error) {
	a, ok, err := p.atom()
	if err != nil {
		return "", false, err
	}
	if ok {
		switch a {
		case "[]":
			p.backup()
			if p.current().kind == tokenCloseList {
				p.backup()
			}
			return "", false, nil
		case "{}":
			p.backup()
			if p.current().kind == tokenCloseCurly {
				p.backup()
			}
			return "", false, nil
		default:
			return a, true, nil
		}
	}

	t, err := p.next()
	if err != nil {
		return "", false, err
	}
	switch t.kind {
	case tokenComma:
		if maxPriority >= 1000 {
			return t.val, true, nil
		}
	case tokenBar:
		return t.val, true, nil
	default:
		break
	}

	p.backup()
	return "", false, nil
}

func (p *Parser) term0(h *Heap, pvs *[]ParsedVariable, maxPriority int) (Term, bool, error) {
	t, err := p.next()
	if err != nil {
		return Term{}, false, err
	}
	switch t.kind {
	case tokenOpen, tokenOpenCT:
		return p.openClose(h, pvs)
	case tokenInteger:
		i, err := integer(h, 1, t.val)
		if err != nil {
			return Term{}, false, err
		}
		return i, true, nil
	case tokenFloatNumber:
		f, err := float(h, 1, t.val)
		if err != nil {
			return Term{}, false, err
		}
		return f, true, nil
	case tokenVariable:
		v, err := p.variable(h, pvs, t.val)
		if err != nil {
			return Term{}, false, err
		}
		return v, true, nil
	case tokenOpenList:
		if t, _ := p.next(); t.kind == tokenCloseList {
			p.backup()
			p.backup()
			break
		}
		p.backup()
		return p.list(h, pvs)
	case tokenOpenCurly:
		if t, _ := p.next(); t.kind == tokenCloseCurly {
			p.backup()
			p.backup()
			break
		}
		p.backup()
		return p.curlyBracketedTerm(h, pvs)
	case tokenDoubleQuotedList:
		switch p.doubleQuotes {
		case doubleQuotesChars:
			cl, err := NewCharList(h, unDoubleQuote(t.val))
			if err != nil {
				return Term{}, false, err
			}
			return cl, true, nil
		case doubleQuotesCodes:
			cl, err := NewCodeList(h, unDoubleQuote(t.val))
			if err != nil {
				return Term{}, false, err
			}
			return cl, true, nil
		default:
			p.backup()
			break
		}
	default:
		p.backup()
	}

	return p.term0Atom(h, pvs, maxPriority)
}

func (p *Parser) term0Atom(h *Heap, pvs *[]ParsedVariable, maxPriority int) (Term, bool, error) {
	a, ok, err := p.atom()
	if err != nil || !ok {
		return Term{}, ok, err
	}

	if a == "-" {
		t, err := p.next()
		if err != nil {
			return Term{}, false, err
		}
		switch t.kind {
		case tokenInteger:
			i, err := integer(h, -1, t.val)
			if err != nil {
				return Term{}, false, err
			}
			return i, true, nil
		case tokenFloatNumber:
			f, err := float(h, -1, t.val)
			if err != nil {
				return Term{}, false, err
			}
			return f, true, nil
		default:
			p.backup()
		}
	}

	t, ok, err := p.functionalNotation(h, pvs, a)
	if err != nil || !ok {
		return Term{}, ok, err
	}

	// 6.3.1.3 An atom which is an operator shall not be the immediate operand (3.120) of an operator.
	if a, err := t.Atom(h); err == nil && maxPriority < 1201 && p.operators.defined(a) {
		p.backup()
		return Term{}, false, nil
	}

	return t, true, nil
}

func (p *Parser) variable(h *Heap, pvs *[]ParsedVariable, s string) (Term, error) {
	if s == "_" {
		v, err := NewVariable(h)
		return v, err
	}
	for i, pv := range *pvs {
		if pv.Name == s {
			(*pvs)[i].Count++
			return Term{tag: termTagVariable, payload: int32(pv.Variable)}, nil
		}
	}
	v, err := NewVariable(h)
	if err != nil {
		return Term{}, err
	}
	*pvs = append(*pvs, ParsedVariable{Name: s, Variable: Variable(v.payload), Count: 1})
	return v, nil
}

func (p *Parser) openClose(h *Heap, pvs *[]ParsedVariable) (Term, bool, error) {
	t, ok, err := p.term(h, pvs, 1201)
	if err != nil || !ok {
		return Term{}, ok, err
	}
	if t, _ := p.next(); t.kind != tokenClose {
		p.backup()
		return Term{}, false, nil
	}
	return t, true, nil
}

func (p *Parser) atom() (string, bool, error) {
	if a, ok, err := p.name(); err != nil || ok {
		return a, ok, err
	}

	t, err := p.next()
	if err != nil {
		return "", false, err
	}
	switch t.kind {
	case tokenOpenList:
		t, err := p.next()
		if err != nil {
			return "", false, err
		}
		switch t.kind {
		case tokenCloseList:
			return "[]", true, nil
		default:
			p.backup()
			p.backup()
			return "", false, nil
		}
	case tokenOpenCurly:
		t, err := p.next()
		if err != nil {
			return "", false, err
		}
		switch t.kind {
		case tokenCloseCurly:
			return "{}", true, nil
		default:
			p.backup()
			p.backup()
			return "", false, nil
		}
	case tokenDoubleQuotedList:
		switch p.doubleQuotes {
		case doubleQuotesAtom:
			return unDoubleQuote(t.val), true, nil
		default:
			p.backup()
			return "", false, nil
		}
	default:
		p.backup()
		return "", false, nil
	}
}

func (p *Parser) name() (string, bool, error) {
	t, err := p.next()
	if err != nil {
		return "", false, err
	}
	switch t.kind {
	case tokenLetterDigit, tokenGraphic, tokenSemicolon, tokenCut:
		return t.val, true, nil
	case tokenQuoted:
		return unquote(t.val), true, nil
	default:
		p.backup()
		return "", false, nil
	}
}

func (p *Parser) list(h *Heap, pvs *[]ParsedVariable) (Term, bool, error) {
	var elems []Term
	arg, err := p.arg(h, pvs)
	if err != nil {
		return Term{}, false, err
	}
	elems = append(elems, arg)
	for {
		switch t, _ := p.next(); t.kind {
		case tokenComma:
			arg, err := p.arg(h, pvs)
			if err != nil {
				return Term{}, false, err
			}
			elems = append(elems, arg)
		case tokenBar:
			tail, err := p.arg(h, pvs)
			if err != nil {
				return Term{}, false, err
			}

			switch t, _ := p.next(); t.kind {
			case tokenCloseList:
				pl, err := NewPartialList(h, tail, elems...)
				if err != nil {
					return Term{}, false, err
				}

				return pl, true, nil
			default:
				p.backup()
				return Term{}, false, nil
			}
		case tokenCloseList:
			l, err := NewList(h, elems...)
			if err != nil {
				return Term{}, false, err
			}

			return l, true, nil
		default:
			p.backup()
			return Term{}, false, nil
		}
	}
}

func (p *Parser) curlyBracketedTerm(h *Heap, pvs *[]ParsedVariable) (Term, bool, error) {
	t, ok, err := p.term(h, pvs, 1201)
	if err != nil || !ok {
		return Term{}, ok, err
	}

	if t, _ := p.next(); t.kind != tokenCloseCurly {
		p.backup()
		return Term{}, false, nil
	}

	c, err := NewCompound(h, "{}", t)
	if err != nil {
		return Term{}, false, err
	}

	return c, true, nil
}

func (p *Parser) functionalNotation(h *Heap, pvs *[]ParsedVariable, functor string) (Term, bool, error) {
	switch t, _ := p.next(); t.kind {
	case tokenOpenCT:
		arg, err := p.arg(h, pvs)
		if err != nil {
			return Term{}, false, err
		}
		args := []Term{arg}
		for {
			switch t, _ := p.next(); t.kind {
			case tokenComma:
				arg, err := p.arg(h, pvs)
				if err != nil {
					return Term{}, false, err
				}
				args = append(args, arg)
			case tokenClose:
				c, err := NewCompound(h, functor, args...)
				if err != nil {
					return Term{}, false, err
				}

				return c, true, nil
			default:
				p.backup()
				return Term{}, false, nil
			}
		}
	default:
		p.backup()
		a, err := NewAtom(h, functor)
		if err != nil {
			return Term{}, false, err
		}
		return a, true, nil
	}
}

func (p *Parser) arg(h *Heap, pvs *[]ParsedVariable) (Term, error) {
	arg, ok, err := p.atom()
	if err != nil {
		return Term{}, err
	}
	if ok {
		if p.operators.defined(arg) {
			// Check if this atom is not followed by its own arguments.
			switch t, _ := p.next(); t.kind {
			case tokenComma, tokenClose, tokenBar, tokenCloseList:
				p.backup()
				a, err := NewAtom(h, arg)
				if err != nil {
					return Term{}, err
				}
				return a, nil
			default:
				p.backup()
			}
		}
		p.backup()
		if p.current().kind == tokenCloseList || p.current().kind == tokenCloseCurly {
			p.backup() // Unquoted [] or {} consist of 2 tokens.
		}
	}

	t, ok, err := p.term(h, pvs, 999)
	if err != nil {
		return Term{}, err
	}
	if !ok {
		return Term{}, &SyntaxError{impDepAtom: fmt.Sprintf("unexpected token: %s", p.current())}
	}
	return t, nil
}

func integer(h *Heap, sign int64, s string) (Term, error) {
	base := 10
	switch {
	case strings.HasPrefix(s, "0'"):
		s = s[2:]
		s = quotedIdentEscapePattern.ReplaceAllStringFunc(s, quotedIdentUnescape)
		return NewInteger(h, sign*int64([]rune(s)[0]))
	case strings.HasPrefix(s, "0b"):
		base = 2
		s = s[2:]
	case strings.HasPrefix(s, "0o"):
		base = 8
		s = s[2:]
	case strings.HasPrefix(s, "0x"):
		base = 16
		s = s[2:]
	}

	f, _, _ := big.ParseFloat(s, base, 0, big.ToZero)
	f.Mul(big.NewFloat(float64(sign)), f)

	switch i, a := f.Int64(); a {
	case big.Above:
		return Term{}, &RepresentationError{flag: "min_integer"}
	case big.Below:
		return Term{}, &RepresentationError{flag: "max_integer"}
	default:
		return NewInteger(h, i)
	}
}

func float(h *Heap, sign float64, s string) (Term, error) {
	bf, _, _ := big.ParseFloat(s, 10, 0, big.ToZero)
	bf.Mul(big.NewFloat(sign), bf)

	f, _ := bf.Float64()
	return NewFloat(h, f)
}

var (
	quotedIdentEscapePattern  = regexp.MustCompile("''|\\\\(?:[\\nabfnrtv\\\\'\"`]|(?:x[\\da-fA-F]+|[0-8]+)\\\\)")
	doubleQuotedEscapePattern = regexp.MustCompile("\"\"|\\\\(?:[\\nabfnrtv\\\\'\"`]|(?:x[\\da-fA-F]+|[0-8]+)\\\\)")
)

func unquote(s string) string {
	return quotedIdentEscapePattern.ReplaceAllStringFunc(s[1:len(s)-1], quotedIdentUnescape)
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
