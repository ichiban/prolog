package syntax

import (
	"errors"
	"fmt"
	"io"
	"iter"
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

type ParseOptions struct {
	arena         *term.Arena
	operatorSet   *OperatorSet
	doubleQuotes  *DoubleQuotes
	variableNames *[]term.VariableName
	makeVariable  func() (term.Handle, error)
}

type ParseOption func(*ParseOptions)

func Arena(arena *term.Arena) ParseOption {
	return func(o *ParseOptions) {
		o.arena = arena
	}
}

func DoubleQuote(doubleQuotes *DoubleQuotes) ParseOption {
	return func(o *ParseOptions) {
		o.doubleQuotes = doubleQuotes
	}
}

func VariableNames(variables *[]term.VariableName) ParseOption {
	return func(o *ParseOptions) {
		o.variableNames = variables
	}
}

func Operators(ops *OperatorSet) ParseOption {
	return func(o *ParseOptions) {
		o.operatorSet = ops
	}
}

func MakeVariable(makeVariable func() (term.Handle, error)) ParseOption {
	return func(o *ParseOptions) {
		o.makeVariable = makeVariable
	}
}

func Parse(text string, opts ...ParseOption) iter.Seq2[term.Handle, error] {
	p := newParser(text, opts...)
	return func(yield func(term.Handle, error) bool) {
		for p.More() {
			if !yield(p.Term()) {
				return
			}
		}
	}
}

func ParseTerm(text string, opts ...ParseOption) (term.Handle, error) {
	p := newParser(text, opts...)
	t, err := p.Term()
	if err != nil {
		return term.Handle{}, fmt.Errorf("term(): %w", err)
	}
	return t, nil
}

func ParseNumber(text string, opts ...ParseOption) (term.Handle, error) {
	p := newParser(text, opts...)
	return p.Number()
}

func ParseVariable(text string, opts ...ParseOption) (term.Handle, error) {
	p := newParser(text, opts...)
	return p.Variable()
}

// parser turns bytes into Term.
type parser struct {
	ParseOptions
	lexer lexer
	buf   *ring.Buffer[token]
}

func newParser(text string, opts ...ParseOption) parser {
	p := parser{
		lexer: lexer{
			input: ring.NewRuneReader(strings.NewReader(text), 4),
		},
		buf: ring.NewBuffer[token](4),
	}
	for _, o := range opts {
		o(&p.ParseOptions)
	}
	if p.operatorSet == nil {
		p.operatorSet = NewOperatorSet()
	}
	if p.doubleQuotes == nil {
		var dq DoubleQuotes
		p.doubleQuotes = &dq
	}
	return p
}

func (p *parser) next() (token, error) {
	if p.buf.Empty() {
		t, err := p.lexer.Token()
		if err != nil {
			return token{}, err
		}
		p.buf.Put(t)
	}
	return p.buf.Get(), nil
}

func (p *parser) backup() {
	p.buf.Backup()
}

func (p *parser) current() token {
	return p.buf.Current()
}

// Term parses a term followed by a full stop.
func (p *parser) Term() (_ term.Handle, err error) {
	t, ok, err := p.term(1201)
	if err != nil {
		return term.Handle{}, fmt.Errorf("term(1201): %w", err)
	}
	if !ok {
		return term.Handle{}, fmt.Errorf("term(1201): %w", &UnexpectedTokenError{token: p.current()})
	}

	switch t, err := p.next(); {
	case errors.Is(err, io.EOF), t.kind == tokenEnd:
		break
	default:
		p.backup()
		return term.Handle{}, fmt.Errorf("next(): %w", &UnexpectedTokenError{token: p.current()})
	}

	return t, nil
}

// Number parses a number term.
func (p *parser) Number() (_ term.Handle, err error) {
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

// Variable parses a variable term.
func (p *parser) Variable() (_ term.Handle, err error) {
	var v term.Handle
	t, err := p.next()
	if err != nil {
		return term.Handle{}, err
	}
	switch t.kind {
	case tokenVariable:
		v, err = p.variable(t.val)
	default:
	}
	if err != nil {
		return term.Handle{}, err
	}

	// No more runes after a variable.
	switch _, err := p.lexer.rawNext(); err {
	case io.EOF:
		return v, nil
	default:
		return term.Handle{}, errors.New("unexpected rune")
	}
}

// More checks if the parser has more tokens to read.
func (p *parser) More() bool {
	if _, err := p.next(); err != nil {
		return false
	}
	p.backup()
	return true
}

type DoubleQuotes int

const (
	DoubleQuotesChars DoubleQuotes = iota
	DoubleQuotesCodes
	DoubleQuotesAtom
)

var doubleQuoteNames = [...]string{
	DoubleQuotesCodes: "codes",
	DoubleQuotesChars: "chars",
	DoubleQuotesAtom:  "atom",
}

func (d DoubleQuotes) String() string {
	return doubleQuoteNames[d]
}

// Loosely based on Pratt parser explained in this article: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
func (p *parser) term(maxPriority int) (term.Handle, bool, error) {
	var lhs term.Handle
	switch op, ok, err := p.prefix(maxPriority); {
	case err != nil:
		return term.Handle{}, false, fmt.Errorf("prefix(%d): %w", maxPriority, err)
	case !ok:
		lhs, ok, err = p.term0(maxPriority)
		if err != nil {
			return term.Handle{}, false, fmt.Errorf("term0(%d): %w", maxPriority, err)
		}
		if !ok {
			return term.Handle{}, false, nil
		}
	default:
		_, rbp := op.bindingPriorities()
		t, ok, err := p.term(rbp)
		if err != nil {
			return term.Handle{}, false, fmt.Errorf("term(%d): %w", rbp, err)
		}
		if !ok {
			p.backup()
			return p.term0(maxPriority)
		}
		lhs, err = p.arena.PutCompound(op.name, t)
		if err != nil {
			return term.Handle{}, false, fmt.Errorf("PutCompound(%s, %s): %w", op.name, &Formatter{Arena: p.arena, Term: t}, err)
		}
	}

	for {
		op, ok, err := p.infix(maxPriority)
		if err != nil {
			return term.Handle{}, false, fmt.Errorf("infix(%d): %w", maxPriority, err)
		}
		if !ok {
			break
		}

		switch _, rbp := op.bindingPriorities(); {
		case rbp > 1200:
			var err error
			lhs, err = p.arena.PutCompound(op.name, lhs)
			if err != nil {
				return term.Handle{}, false, err
			}
		default:
			rhs, ok, err := p.term(rbp)
			if err != nil {
				return term.Handle{}, false, fmt.Errorf("term(%d): %w", rbp, err)
			}
			if !ok {
				return term.Handle{}, false, nil
			}
			lhs, err = p.arena.PutCompound(op.name, lhs, rhs)
			if err != nil {
				return term.Handle{}, false, err
			}
		}
	}

	return lhs, true, nil
}

func (p *parser) prefix(maxPriority int) (operator, bool, error) {
	a, ok, err := p.op(maxPriority)
	if err != nil {
		return operator{}, false, fmt.Errorf("op(%d): %w", maxPriority, err)
	}
	if !ok {
		return operator{}, false, nil
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

	op, ok := p.operatorSet.ops[opKey{name: a, opClass: operatorClassPrefix}]
	if !ok || op.priority > maxPriority {
		p.backup()
		return operator{}, false, nil
	}
	return op, true, nil
}

func (p *parser) infix(maxPriority int) (operator, bool, error) {
	a, ok, err := p.op(maxPriority)
	if err != nil {
		return operator{}, ok, fmt.Errorf("op(%d): %w", maxPriority, err)
	}
	if !ok {
		return operator{}, false, nil
	}

	if op := p.operatorSet.ops[opKey{name: a, opClass: operatorClassInfix}]; op != (operator{}) {
		l, _ := op.bindingPriorities()
		if l <= maxPriority {
			return op, true, nil
		}
	}
	if op := p.operatorSet.ops[opKey{name: a, opClass: operatorClassPostfix}]; op != (operator{}) {
		l, _ := op.bindingPriorities()
		if l <= maxPriority {
			return op, true, nil
		}
	}

	p.backup()
	return operator{}, false, nil
}

func (p *parser) op(maxPriority int) (term.Atom, bool, error) {
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

func (p *parser) term0(maxPriority int) (term.Handle, bool, error) {
	t, err := p.next()
	if err != nil {
		return term.Handle{}, false, fmt.Errorf("next(): %w", err)
	}
	switch t.kind {
	case tokenOpen, tokenOpenCT:
		t, ok, err := p.openClose()
		if err != nil {
			return term.Handle{}, false, fmt.Errorf("openClose(): %w", err)
		}
		return t, ok, nil
	case tokenInteger:
		i, err := p.integer(1, t.val)
		if err != nil {
			return term.Handle{}, false, fmt.Errorf("integer(1, %s): %w", t.val, err)
		}
		return i, true, nil
	case tokenFloatNumber:
		f, err := p.float(1, t.val)
		if err != nil {
			return term.Handle{}, false, fmt.Errorf("float(1, %s): %w", t.val, err)
		}
		return f, true, nil
	case tokenVariable:
		v, err := p.variable(t.val)
		if err != nil {
			return term.Handle{}, false, fmt.Errorf("variable(%s): %w", t.val, err)
		}
		return v, true, nil
	case tokenOpenList:
		if t, _ := p.next(); t.kind == tokenCloseList {
			p.backup()
			p.backup()
			break
		}
		p.backup()
		t, ok, err := p.list()
		if err != nil {
			return term.Handle{}, false, fmt.Errorf("list(): %w", err)
		}
		return t, ok, nil
	case tokenOpenCurly:
		if t, _ := p.next(); t.kind == tokenCloseCurly {
			p.backup()
			p.backup()
			break
		}
		p.backup()
		t, ok, err := p.curlyBracketedTerm()
		if err != nil {
			return term.Handle{}, false, fmt.Errorf("curlyBracketedTerm(): %w", err)
		}
		return t, ok, nil
	case tokenDoubleQuotedList:
		switch *p.doubleQuotes {
		case DoubleQuotesChars:
			cl, err := p.arena.PutCharList(unDoubleQuote(t.val))
			if err != nil {
				return term.Handle{}, false, err
			}
			return cl, true, nil
		case DoubleQuotesCodes:
			cl, err := p.arena.PutCodeList(unDoubleQuote(t.val))
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

	a, ok, err := p.term0Atom(maxPriority)
	if err != nil {
		return term.Handle{}, false, fmt.Errorf("term0Atom(%d): %w", maxPriority, err)
	}
	return a, ok, nil
}

func (p *parser) term0Atom(maxPriority int) (term.Handle, bool, error) {
	a, ok, err := p.atom()
	if err != nil {
		return term.Handle{}, false, fmt.Errorf("atom(): %w", err)
	}
	if !ok {
		return term.Handle{}, false, nil
	}

	if a == atomMinus {
		t, err := p.next()
		if err != nil {
			return term.Handle{}, false, fmt.Errorf("next(): %w", err)
		}
		switch t.kind {
		case tokenInteger:
			i, err := p.integer(-1, t.val)
			if err != nil {
				return term.Handle{}, false, fmt.Errorf("integer(-1, %s): %w", t.val, err)
			}
			return i, true, nil
		case tokenFloatNumber:
			f, err := p.float(-1, t.val)
			if err != nil {
				return term.Handle{}, false, fmt.Errorf("float(-1, %s): %w", t.val, err)
			}
			return f, true, nil
		default:
			p.backup()
		}
	}

	t, ok, err := p.functionalNotation(a)
	if err != nil {
		return term.Handle{}, false, fmt.Errorf("functionalNotation(%s): %w", a, err)
	}
	if !ok {
		return term.Handle{}, false, nil
	}

	// 6.3.1.3 An atom which is an operator shall not be the immediate operand (3.120) of an operator.
	if a, ok := p.arena.Atom(t); ok && maxPriority < 1201 && p.operatorSet.defined(a) {
		p.backup()
		return term.Handle{}, false, nil
	}

	return t, true, nil
}

func (p *parser) variable(s string) (term.Handle, error) {
	if p.makeVariable == nil {
		p.makeVariable = p.arena.PutVariable
	}
	if s == "_" {
		v, err := p.makeVariable()
		return v, err
	}
	if p.variableNames == nil {
		var vns []term.VariableName
		p.variableNames = &vns
	}
	for i, pv := range *p.variableNames {
		if pv.Name == s {
			(*p.variableNames)[i].Count++
			return pv.Variable, nil
		}
	}
	v, err := p.makeVariable()
	if err != nil {
		return term.Handle{}, err
	}
	if p.variableNames != nil {
		*p.variableNames = append(*p.variableNames, term.VariableName{Name: s, Variable: v, Count: 1})
	}
	return v, nil
}

func (p *parser) openClose() (term.Handle, bool, error) {
	t, ok, err := p.term(1201)
	if err != nil {
		return term.Handle{}, false, fmt.Errorf("term(1201): %w", err)
	}
	if !ok {
		return term.Handle{}, false, nil
	}
	if t, _ := p.next(); t.kind != tokenClose {
		p.backup()
		return term.Handle{}, false, nil
	}
	return t, true, nil
}

func (p *parser) atom() (term.Atom, bool, error) {
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
		case DoubleQuotesAtom:
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

func (p *parser) name() (term.Atom, bool, error) {
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

func (p *parser) list() (term.Handle, bool, error) {
	var elems []term.Handle
	arg, err := p.arg()
	if err != nil {
		return term.Handle{}, false, err
	}
	elems = append(elems, arg)
	for {
		switch t, _ := p.next(); t.kind {
		case tokenComma:
			arg, err := p.arg()
			if err != nil {
				return term.Handle{}, false, err
			}
			elems = append(elems, arg)
		case tokenBar:
			tail, err := p.arg()
			if err != nil {
				return term.Handle{}, false, err
			}

			switch t, _ := p.next(); t.kind {
			case tokenCloseList:
				pl, err := p.arena.PutPartialList(tail, elems...)
				if err != nil {
					return term.Handle{}, false, err
				}

				return pl, true, nil
			default:
				p.backup()
				return term.Handle{}, false, nil
			}
		case tokenCloseList:
			l, err := p.arena.PutList(elems...)
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

func (p *parser) curlyBracketedTerm() (term.Handle, bool, error) {
	t, ok, err := p.term(1201)
	if err != nil {
		return term.Handle{}, false, fmt.Errorf("term(1201): %w", err)
	}
	if !ok {
		return term.Handle{}, false, nil
	}

	if t, _ := p.next(); t.kind != tokenCloseCurly {
		p.backup()
		return term.Handle{}, false, nil
	}

	c, err := p.arena.PutCompound(atomEmptyBlock, t)
	if err != nil {
		return term.Handle{}, false, err
	}

	return c, true, nil
}

func (p *parser) functionalNotation(functor term.Atom) (term.Handle, bool, error) {
	switch t, _ := p.next(); t.kind {
	case tokenOpenCT:
		arg, err := p.arg()
		if err != nil {
			return term.Handle{}, false, fmt.Errorf("arg(): %w", err)
		}
		args := []term.Handle{arg}
		for {
			switch t, _ := p.next(); t.kind {
			case tokenComma:
				arg, err := p.arg()
				if err != nil {
					return term.Handle{}, false, fmt.Errorf("arg(): %w", err)
				}
				args = append(args, arg)
			case tokenClose:
				c, err := p.arena.PutCompound(functor, args...)
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
		a, err := p.arena.PutAtom(functor)
		if err != nil {
			return term.Handle{}, false, err
		}
		return a, true, nil
	}
}

func (p *parser) arg() (term.Handle, error) {
	arg, ok, err := p.atom()
	if err != nil {
		return term.Handle{}, fmt.Errorf("atom(): %w", err)
	}
	if ok {
		if p.operatorSet.defined(arg) {
			// Check if this atom is not followed by its own arguments.
			switch t, _ := p.next(); t.kind {
			case tokenComma, tokenClose, tokenBar, tokenCloseList:
				p.backup()
				a, err := p.arena.PutAtom(arg)
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

	t, ok, err := p.term(999)
	if err != nil {
		return term.Handle{}, fmt.Errorf("term(999): %w", err)
	}
	if !ok {
		return term.Handle{}, fmt.Errorf("term(999): %w", &UnexpectedTokenError{token: p.current()})
	}
	return t, nil
}

func (p *parser) integer(sign int64, s string) (term.Handle, error) {
	base := 10
	switch {
	case strings.HasPrefix(s, "0'"):
		s = s[2:]
		s = quotedIdentEscapePattern.ReplaceAllStringFunc(s, quotedIdentUnescape)
		return p.arena.PutInteger(sign * int64([]rune(s)[0]))
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
		return p.arena.PutInteger(i)
	}
}

func (p *parser) float(sign float64, s string) (term.Handle, error) {
	bf, _, _ := big.ParseFloat(s, 10, 0, big.ToZero)
	bf.Mul(big.NewFloat(sign), bf)

	f, _ := bf.Float64()
	return p.arena.PutFloat(f)
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
