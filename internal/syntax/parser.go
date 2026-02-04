package syntax

import (
	"errors"
	"fmt"
	"io"
	"math/big"
	"regexp"
	"strconv"
	"strings"

	"github.com/ichiban/prolog/v2/internal/ring"
	"github.com/ichiban/prolog/v2/internal/term"
)

var (
	atomMinus      = term.NewAtomRune('-')
	atomEmptyList  = term.NewAtom("[]")
	atomEmptyBlock = term.NewAtom("{}")
)

var (
	ErrNotANumber = errors.New("not a number")
	ErrIntAbove   = errors.New("int above")
	ErrIntBelow   = errors.New("int below")
)

type UnexpectedTokenError struct {
	token token
}

func (e *UnexpectedTokenError) Error() string {
	return fmt.Sprintf("unexpected token %q", e.token)
}

// Parser turns bytes into Term.
type Parser struct {
	heap         *term.Heap
	ops          *OperatorSet
	doubleQuotes *doubleQuotes
	lexer        lexer
	buf          *ring.Buffer[token]
	makeVariable func() (term.Handle, error)
}

// ParsedVariable is a set of information regarding a variable in a parsed term.
type ParsedVariable struct {
	Name     string
	Variable term.Handle
	Count    int
}

// NewParser creates a new parser from the current VM and io.RuneReader.
func NewParser(r io.RuneReader, heap *term.Heap, ops *OperatorSet, doubleQuotes *doubleQuotes) *Parser {
	return &Parser{
		heap:         heap,
		ops:          ops,
		doubleQuotes: doubleQuotes,
		lexer: lexer{
			input: ring.NewRuneReader(r, 4),
		},
		buf: ring.NewBuffer[token](4),
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
func (p *Parser) Term() (_ term.Handle, _ []ParsedVariable, err error) {
	var pvs []ParsedVariable
	t, ok, err := p.term(&pvs, 1201)
	if err != nil {
		return term.Handle{}, nil, err
	}
	if !ok {
		return term.Handle{}, nil, &UnexpectedTokenError{token: p.current()}
	}

	switch t, _ := p.next(); t.kind {
	case tokenEnd:
		break
	default:
		p.backup()
		return term.Handle{}, nil, &UnexpectedTokenError{token: p.current()}
	}

	return t, pvs, nil
}

// Number parses a number term.
func (p *Parser) Number() (_ term.Handle, err error) {
	var n term.Handle
	t, err := p.next()
	if err != nil {
		return term.Handle{}, err
	}
	switch t.kind {
	case tokenInteger:
		n, err = p.integer(1, t.val)
	case tokenFloatNumber:
		n, err = p.float(1, t.val)
	default:
		p.backup()
		var (
			a  term.Atom
			ok bool
		)
		a, ok, err = p.name()
		if err != nil {
			return term.Handle{}, err
		}
		if !ok {
			return term.Handle{}, ErrNotANumber
		}

		if a != atomMinus {
			p.backup()
			return term.Handle{}, ErrNotANumber
		}

		t, err = p.next()
		if err != nil {
			return term.Handle{}, ErrNotANumber
		}
		switch t.kind {
		case tokenInteger:
			n, err = p.integer(-1, t.val)
		case tokenFloatNumber:
			n, err = p.float(-1, t.val)
		default:
			p.backup()
			p.backup()
			return term.Handle{}, ErrNotANumber
		}
	}
	if err != nil {
		return term.Handle{}, err
	}

	// No more runes after a number.
	switch _, err := p.lexer.rawNext(); err {
	case io.EOF:
		return n, nil
	default:
		return term.Handle{}, ErrNotANumber
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
func (p *Parser) term(pvs *[]ParsedVariable, maxPriority int) (term.Handle, bool, error) {
	var lhs term.Handle
	switch op, ok, err := p.prefix(maxPriority); {
	case err != nil:
		return term.Handle{}, false, err
	case !ok:
		lhs, ok, err = p.term0(pvs, maxPriority)
		if err != nil || !ok {
			return term.Handle{}, ok, err
		}
	default:
		_, rbp := op.bindingPriorities()
		t, ok, err := p.term(pvs, rbp)
		if err != nil {
			return term.Handle{}, false, err
		}
		if !ok {
			p.backup()
			return p.term0(pvs, maxPriority)
		}
		lhs, err = p.heap.PutCompound(op.name, t)
		if err != nil {
			return term.Handle{}, false, err
		}
	}

	for {
		op, ok, err := p.infix(maxPriority)
		if err != nil {
			return term.Handle{}, false, err
		}
		if !ok {
			break
		}

		switch _, rbp := op.bindingPriorities(); {
		case rbp > 1200:
			var err error
			lhs, err = p.heap.PutCompound(op.name, lhs)
			if err != nil {
				return term.Handle{}, false, err
			}
		default:
			rhs, ok, err := p.term(pvs, rbp)
			if err != nil || !ok {
				return term.Handle{}, ok, err
			}
			lhs, err = p.heap.PutCompound(op.name, lhs, rhs)
			if err != nil {
				return term.Handle{}, false, err
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

	if a == atomMinus {
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

	op, ok := p.ops.ops[opKey{name: a, opClass: operatorClassPrefix}]
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

	if op := p.ops.ops[opKey{name: a, opClass: operatorClassInfix}]; op != (operator{}) {
		l, _ := op.bindingPriorities()
		if l <= maxPriority {
			return op, true, nil
		}
	}
	if op := p.ops.ops[opKey{name: a, opClass: operatorClassPostfix}]; op != (operator{}) {
		l, _ := op.bindingPriorities()
		if l <= maxPriority {
			return op, true, nil
		}
	}

	p.backup()
	return operator{}, false, nil
}

func (p *Parser) op(maxPriority int) (term.Atom, bool, error) {
	a, ok, err := p.atom()
	if err != nil {
		return term.Atom{}, false, err
	}
	if ok {
		switch a {
		case atomEmptyList:
			p.backup()
			if p.current().kind == tokenCloseList {
				p.backup()
			}
			return term.Atom{}, false, nil
		case atomEmptyBlock:
			p.backup()
			if p.current().kind == tokenCloseCurly {
				p.backup()
			}
			return term.Atom{}, false, nil
		default:
			return a, true, nil
		}
	}

	t, err := p.next()
	if err != nil {
		return term.Atom{}, false, err
	}
	switch t.kind {
	case tokenComma:
		if maxPriority >= 1000 {
			return term.NewAtom(t.val), true, nil
		}
	case tokenBar:
		return term.NewAtom(t.val), true, nil
	default:
		break
	}

	p.backup()
	return term.Atom{}, false, nil
}

func (p *Parser) term0(pvs *[]ParsedVariable, maxPriority int) (term.Handle, bool, error) {
	t, err := p.next()
	if err != nil {
		return term.Handle{}, false, err
	}
	switch t.kind {
	case tokenOpen, tokenOpenCT:
		return p.openClose(pvs)
	case tokenInteger:
		i, err := p.integer(1, t.val)
		if err != nil {
			return term.Handle{}, false, err
		}
		return i, true, nil
	case tokenFloatNumber:
		f, err := p.float(1, t.val)
		if err != nil {
			return term.Handle{}, false, err
		}
		return f, true, nil
	case tokenVariable:
		v, err := p.variable(pvs, t.val)
		if err != nil {
			return term.Handle{}, false, err
		}
		return v, true, nil
	case tokenOpenList:
		if t, _ := p.next(); t.kind == tokenCloseList {
			p.backup()
			p.backup()
			break
		}
		p.backup()
		return p.list(pvs)
	case tokenOpenCurly:
		if t, _ := p.next(); t.kind == tokenCloseCurly {
			p.backup()
			p.backup()
			break
		}
		p.backup()
		return p.curlyBracketedTerm(pvs)
	case tokenDoubleQuotedList:
		switch *p.doubleQuotes {
		case doubleQuotesChars:
			cl, err := p.heap.PutCharList(unDoubleQuote(t.val))
			if err != nil {
				return term.Handle{}, false, err
			}
			return cl, true, nil
		case doubleQuotesCodes:
			cl, err := p.heap.PutCodeList(unDoubleQuote(t.val))
			if err != nil {
				return term.Handle{}, false, err
			}
			return cl, true, nil
		default:
			p.backup()
			break
		}
	default:
		p.backup()
	}

	return p.term0Atom(pvs, maxPriority)
}

func (p *Parser) term0Atom(pvs *[]ParsedVariable, maxPriority int) (term.Handle, bool, error) {
	a, ok, err := p.atom()
	if err != nil || !ok {
		return term.Handle{}, ok, err
	}

	if a == atomMinus {
		t, err := p.next()
		if err != nil {
			return term.Handle{}, false, err
		}
		switch t.kind {
		case tokenInteger:
			i, err := p.integer(-1, t.val)
			if err != nil {
				return term.Handle{}, false, err
			}
			return i, true, nil
		case tokenFloatNumber:
			f, err := p.float(-1, t.val)
			if err != nil {
				return term.Handle{}, false, err
			}
			return f, true, nil
		default:
			p.backup()
		}
	}

	t, ok, err := p.functionalNotation(pvs, a)
	if err != nil || !ok {
		return term.Handle{}, ok, err
	}

	// 6.3.1.3 An atom which is an operator shall not be the immediate operand (3.120) of an operator.
	if a, ok := t.Atom(); ok && maxPriority < 1201 && p.ops.defined(a) {
		p.backup()
		return term.Handle{}, false, nil
	}

	return t, true, nil
}

func (p *Parser) variable(pvs *[]ParsedVariable, s string) (term.Handle, error) {
	if p.makeVariable == nil {
		p.makeVariable = p.heap.PutVariable
	}
	if s == "_" {
		v, err := p.makeVariable()
		return v, err
	}
	for i, pv := range *pvs {
		if pv.Name == s {
			(*pvs)[i].Count++
			return pv.Variable, nil
		}
	}
	v, err := p.makeVariable()
	if err != nil {
		return term.Handle{}, err
	}
	*pvs = append(*pvs, ParsedVariable{Name: s, Variable: v, Count: 1})
	return v, nil
}

func (p *Parser) openClose(pvs *[]ParsedVariable) (term.Handle, bool, error) {
	t, ok, err := p.term(pvs, 1201)
	if err != nil || !ok {
		return term.Handle{}, ok, err
	}
	if t, _ := p.next(); t.kind != tokenClose {
		p.backup()
		return term.Handle{}, false, nil
	}
	return t, true, nil
}

func (p *Parser) atom() (term.Atom, bool, error) {
	if a, ok, err := p.name(); err != nil || ok {
		return a, ok, err
	}

	t, err := p.next()
	if err != nil {
		return term.Atom{}, false, err
	}
	switch t.kind {
	case tokenOpenList:
		t, err := p.next()
		if err != nil {
			return term.Atom{}, false, err
		}
		switch t.kind {
		case tokenCloseList:
			return atomEmptyList, true, nil
		default:
			p.backup()
			p.backup()
			return term.Atom{}, false, nil
		}
	case tokenOpenCurly:
		t, err := p.next()
		if err != nil {
			return term.Atom{}, false, err
		}
		switch t.kind {
		case tokenCloseCurly:
			return atomEmptyBlock, true, nil
		default:
			p.backup()
			p.backup()
			return term.Atom{}, false, nil
		}
	case tokenDoubleQuotedList:
		switch *p.doubleQuotes {
		case doubleQuotesAtom:
			return term.NewAtom(unDoubleQuote(t.val)), true, nil
		default:
			p.backup()
			return term.Atom{}, false, nil
		}
	default:
		p.backup()
		return term.Atom{}, false, nil
	}
}

func (p *Parser) name() (term.Atom, bool, error) {
	t, err := p.next()
	if err != nil {
		return term.Atom{}, false, err
	}
	switch t.kind {
	case tokenLetterDigit, tokenGraphic, tokenSemicolon, tokenCut:
		return term.NewAtom(t.val), true, nil
	case tokenQuoted:
		return term.NewAtom(unquote(t.val)), true, nil
	default:
		p.backup()
		return term.Atom{}, false, nil
	}
}

func (p *Parser) list(pvs *[]ParsedVariable) (term.Handle, bool, error) {
	var elems []term.Handle
	arg, err := p.arg(pvs)
	if err != nil {
		return term.Handle{}, false, err
	}
	elems = append(elems, arg)
	for {
		switch t, _ := p.next(); t.kind {
		case tokenComma:
			arg, err := p.arg(pvs)
			if err != nil {
				return term.Handle{}, false, err
			}
			elems = append(elems, arg)
		case tokenBar:
			tail, err := p.arg(pvs)
			if err != nil {
				return term.Handle{}, false, err
			}

			switch t, _ := p.next(); t.kind {
			case tokenCloseList:
				pl, err := p.heap.PutPartialList(tail, elems...)
				if err != nil {
					return term.Handle{}, false, err
				}

				return pl, true, nil
			default:
				p.backup()
				return term.Handle{}, false, nil
			}
		case tokenCloseList:
			l, err := p.heap.PutList(elems...)
			if err != nil {
				return term.Handle{}, false, err
			}

			return l, true, nil
		default:
			p.backup()
			return term.Handle{}, false, nil
		}
	}
}

func (p *Parser) curlyBracketedTerm(pvs *[]ParsedVariable) (term.Handle, bool, error) {
	t, ok, err := p.term(pvs, 1201)
	if err != nil || !ok {
		return term.Handle{}, ok, err
	}

	if t, _ := p.next(); t.kind != tokenCloseCurly {
		p.backup()
		return term.Handle{}, false, nil
	}

	c, err := p.heap.PutCompound(atomEmptyBlock, t)
	if err != nil {
		return term.Handle{}, false, err
	}

	return c, true, nil
}

func (p *Parser) functionalNotation(pvs *[]ParsedVariable, functor term.Atom) (term.Handle, bool, error) {
	switch t, _ := p.next(); t.kind {
	case tokenOpenCT:
		arg, err := p.arg(pvs)
		if err != nil {
			return term.Handle{}, false, err
		}
		args := []term.Handle{arg}
		for {
			switch t, _ := p.next(); t.kind {
			case tokenComma:
				arg, err := p.arg(pvs)
				if err != nil {
					return term.Handle{}, false, err
				}
				args = append(args, arg)
			case tokenClose:
				c, err := p.heap.PutCompound(functor, args...)
				if err != nil {
					return term.Handle{}, false, err
				}

				return c, true, nil
			default:
				p.backup()
				return term.Handle{}, false, nil
			}
		}
	default:
		p.backup()
		a, err := p.heap.PutAtom(functor)
		if err != nil {
			return term.Handle{}, false, err
		}
		return a, true, nil
	}
}

func (p *Parser) arg(pvs *[]ParsedVariable) (term.Handle, error) {
	arg, ok, err := p.atom()
	if err != nil {
		return term.Handle{}, err
	}
	if ok {
		if p.ops.defined(arg) {
			// Check if this atom is not followed by its own arguments.
			switch t, _ := p.next(); t.kind {
			case tokenComma, tokenClose, tokenBar, tokenCloseList:
				p.backup()
				a, err := p.heap.PutAtom(arg)
				if err != nil {
					return term.Handle{}, err
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

	t, ok, err := p.term(pvs, 999)
	if err != nil {
		return term.Handle{}, err
	}
	if !ok {
		return term.Handle{}, &UnexpectedTokenError{token: p.current()}
	}
	return t, nil
}

func (p *Parser) integer(sign int64, s string) (term.Handle, error) {
	base := 10
	switch {
	case strings.HasPrefix(s, "0'"):
		s = s[2:]
		s = quotedIdentEscapePattern.ReplaceAllStringFunc(s, quotedIdentUnescape)
		return p.heap.PutInteger(sign * int64([]rune(s)[0]))
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
		return term.Handle{}, ErrIntAbove
	case big.Below:
		return term.Handle{}, ErrIntBelow
	default:
		return p.heap.PutInteger(i)
	}
}

func (p *Parser) float(sign float64, s string) (term.Handle, error) {
	bf, _, _ := big.ParseFloat(s, 10, 0, big.ToZero)
	bf.Mul(big.NewFloat(sign), bf)

	f, _ := bf.Float64()
	return p.heap.PutFloat(f)
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
