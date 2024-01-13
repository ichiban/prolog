package engine

import (
	"errors"
	"fmt"
	"io"
	"math/big"
	"reflect"
	"regexp"
	"strconv"
	"strings"
)

var (
	errNoOp        = errors.New("no op")
	errNotANumber  = errors.New("not a number")
	errPlaceholder = errors.New("wrong number of arguments for placeholders")
)

// Parser turns bytes into Term.
type Parser struct {
	Lexer

	Vars []ParsedVariable

	placeholder Atom
	args        []Term

	buf tokenRingBuffer
}

// ParsedVariable is a set of information regarding a variable in a parsed term.
type ParsedVariable struct {
	Name     Atom
	Variable Variable
	Count    int
}

// NewParser creates a new parser from the moduler and io.RuneReader.
func NewParser(m func() *module, r io.RuneReader) *Parser {
	return &Parser{
		Lexer: Lexer{
			module: m,
			input:  newRuneRingBuffer(r),
		},
	}
}

// SetPlaceholder registers placeholder and its arguments. Every occurrence of placeholder will be replaced by arguments.
// Mismatch of the number of occurrences of placeholder and the number of arguments raises an error.
func (p *Parser) SetPlaceholder(placeholder string, args ...interface{}) error {
	p.placeholder = NewAtom(placeholder)
	p.args = make([]Term, len(args))
	for i, a := range args {
		var err error
		p.args[i], err = p.termOf(reflect.ValueOf(a))
		if err != nil {
			return err
		}
	}
	return nil
}

func (p *Parser) termOf(o reflect.Value) (Term, error) {
	switch o.Kind() {
	case reflect.Float32, reflect.Float64:
		return Float(o.Float()), nil
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		return Integer(o.Int()), nil
	case reflect.String:
		m := p.module()
		switch m.doubleQuotes {
		case doubleQuotesCodes:
			return CodeList(o.String()), nil
		case doubleQuotesAtom:
			return NewAtom(o.String()), nil
		default:
			return CharList(o.String()), nil
		}
	case reflect.Array, reflect.Slice:
		l := o.Len()
		es := make([]Term, l)
		for i := 0; i < l; i++ {
			var err error
			es[i], err = p.termOf(o.Index(i))
			if err != nil {
				return nil, err
			}
		}
		return List(es...), nil
	default:
		return nil, fmt.Errorf("can't convert to term: %v", o)
	}
}

func (p *Parser) next() (Token, error) {
	if p.buf.empty() {
		t, err := p.Token()
		if err != nil {
			return Token{}, err
		}
		p.buf.put(t)
	}
	return p.buf.get(), nil
}

func (p *Parser) backup() {
	p.buf.backup()
}

func (p *Parser) current() Token {
	return p.buf.current()
}

func (p *Parser) unexpected() error {
	return unexpectedTokenError{actual: p.current()}
}

// Term parses a term followed by a full stop.
func (p *Parser) Term() (Term, error) {
	t, err := p.term(1201)
	switch {
	case err == nil:
		break
	case errors.As(err, &unexpectedTokenError{}):
		return nil, err
	default:
		return nil, err
	}

	switch n, _ := p.next(); n.kind {
	case tokenEnd:
		break
	default:
		p.backup()
		return nil, unexpectedTokenError{actual: p.current()}
	}

	if len(p.args) != 0 {
		return nil, errPlaceholder
	}

	return t, nil
}

// Number parses a number term.
func (p *Parser) number() (Number, error) {
	var (
		n   Number
		err error
	)
	t, err := p.next()
	if err != nil {
		return nil, err
	}
	switch t.kind {
	case tokenInteger:
		n, err = integer(1, t.val)
	case tokenFloatNumber:
		n, err = float(1, t.val)
	default:
		p.backup()
		var a Atom
		a, err = p.name()
		if err != nil {
			return nil, errNotANumber
		}

		if a != atomMinus {
			p.backup()
			return nil, errNotANumber
		}

		var t Token
		t, err = p.next()
		if err != nil {
			return nil, errNotANumber
		}
		switch t.kind {
		case tokenInteger:
			n, err = integer(-1, t.val)
		case tokenFloatNumber:
			n, err = float(-1, t.val)
		default:
			p.backup()
			p.backup()
			return nil, errNotANumber
		}
	}
	if err != nil {
		return nil, err
	}

	// No more runes after a number.
	switch _, err := p.rawNext(); {
	case errors.Is(err, io.EOF):
		return n, nil
	default:
		return nil, errNotANumber
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

type operatorClass uint8

const (
	operatorClassPrefix operatorClass = iota
	operatorClassPostfix
	operatorClassInfix
	_operatorClassLen
)

type operatorSpecifier uint8

const (
	operatorSpecifierFX  = operatorSpecifier(operatorClassPrefix<<2 + 1)
	operatorSpecifierFY  = operatorSpecifier(operatorClassPrefix<<2 + 2)
	operatorSpecifierXF  = operatorSpecifier(operatorClassPostfix<<2 + 1)
	operatorSpecifierYF  = operatorSpecifier(operatorClassPostfix<<2 + 2)
	operatorSpecifierXFX = operatorSpecifier(operatorClassInfix<<2 + 1)
	operatorSpecifierXFY = operatorSpecifier(operatorClassInfix<<2 + 2)
	operatorSpecifierYFX = operatorSpecifier(operatorClassInfix<<2 + 3)
)

func (s operatorSpecifier) class() operatorClass {
	return operatorClass((s & (0b11 << 2)) >> 2)
}

func (s operatorSpecifier) term() Term {
	return [...]Term{
		operatorSpecifierFX:  atomFX,
		operatorSpecifierFY:  atomFY,
		operatorSpecifierXF:  atomXF,
		operatorSpecifierYF:  atomYF,
		operatorSpecifierXFX: atomXFX,
		operatorSpecifierXFY: atomXFY,
		operatorSpecifierYFX: atomYFX,
	}[s]
}

func (s operatorSpecifier) arity() int {
	return [...]int{
		operatorSpecifierFX:  1,
		operatorSpecifierFY:  1,
		operatorSpecifierXF:  1,
		operatorSpecifierYF:  1,
		operatorSpecifierXFX: 2,
		operatorSpecifierXFY: 2,
		operatorSpecifierYFX: 2,
	}[s]
}

type operators map[Atom][_operatorClassLen]operator

func (ops *operators) defined(name Atom) bool {
	ops.init()
	_, ok := (*ops)[name]
	return ok
}

func (ops *operators) definedInClass(name Atom, class operatorClass) bool {
	ops.init()
	return (*ops)[name][class] != operator{}
}

func (ops *operators) define(p Integer, spec operatorSpecifier, op Atom, exported bool) {
	if p == 0 {
		return
	}
	ops.init()
	os := (*ops)[op]
	os[spec.class()] = operator{
		priority:  p,
		specifier: spec,
		name:      op,
		exported:  exported,
	}
	(*ops)[op] = os
}

func (ops *operators) init() {
	if *ops != nil {
		return
	}
	*ops = map[Atom][3]operator{}
}

func (ops *operators) reset() {
	for name := range *ops {
		(*ops)[name] = [3]operator{}
	}
}

func (ops *operators) remove(name Atom, class operatorClass) {
	os := (*ops)[name]
	os[class] = operator{}
	if os == ([_operatorClassLen]operator{}) {
		delete(*ops, name)
		return
	}
	(*ops)[name] = os
}

type operator struct {
	priority  Integer // 1 ~ 1200
	specifier operatorSpecifier
	name      Atom
	exported  bool
}

// Pratt parser's binding powers but in Prolog priority.
func (o *operator) bindingPriorities() (Integer, Integer) {
	const max = Integer(1202)
	type lr struct {
		left, right Integer
	}
	p := [...]lr{
		operatorSpecifierFX:  {max, o.priority - 1},
		operatorSpecifierFY:  {max, o.priority},
		operatorSpecifierXF:  {o.priority - 1, max},
		operatorSpecifierYF:  {o.priority, max},
		operatorSpecifierXFX: {o.priority - 1, o.priority - 1},
		operatorSpecifierXFY: {o.priority - 1, o.priority},
		operatorSpecifierYFX: {o.priority, o.priority - 1},
	}[o.specifier]
	return p.left, p.right
}

type doubleQuotes int

const (
	doubleQuotesChars doubleQuotes = iota
	doubleQuotesCodes
	doubleQuotesAtom
)

func (d doubleQuotes) String() string {
	return [...]string{
		doubleQuotesCodes: "codes",
		doubleQuotesChars: "chars",
		doubleQuotesAtom:  "atom",
	}[d]
}

// Loosely based on Pratt parser explained in this article: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
func (p *Parser) term(maxPriority Integer) (Term, error) {
	var lhs Term
	switch op, err := p.prefix(maxPriority); err {
	case nil:
		_, rbp := op.bindingPriorities()
		t, err := p.term(rbp)
		if err != nil {
			p.backup()
			return p.term0(maxPriority)
		}
		lhs = op.name.Apply(t)
	case errNoOp:
		lhs, err = p.term0(maxPriority)
		if err != nil {
			return nil, err
		}
	default:
		return nil, err
	}

	for {
		op, err := p.infix(maxPriority)
		if err != nil {
			break
		}
		switch _, rbp := op.bindingPriorities(); {
		case rbp > 1200:
			lhs = op.name.Apply(lhs)
		default:
			rhs, err := p.term(rbp)
			if err != nil {
				return nil, err
			}
			lhs = op.name.Apply(lhs, rhs)
		}
	}

	return lhs, nil
}

func (p *Parser) prefix(maxPriority Integer) (operator, error) {
	a, err := p.op(maxPriority)
	if err != nil {
		return operator{}, errNoOp
	}

	if a == atomMinus {
		t, err := p.next()
		if err != nil {
			return operator{}, err
		}
		switch t.kind {
		case tokenInteger, tokenFloatNumber:
			p.backup()
			p.backup()
			return operator{}, errNoOp
		default:
			p.backup()
		}
	}

	t, err := p.next()
	if err != nil {
		return operator{}, err
	}
	switch t.kind {
	case tokenOpenCT:
		p.backup()
		p.backup()
		return operator{}, errNoOp
	default:
		p.backup()
	}

	m := p.module()
	if op := m.operators[a][operatorClassPrefix]; op != (operator{}) && op.priority <= maxPriority {
		return op, nil
	}

	p.backup()
	return operator{}, errNoOp
}

func (p *Parser) infix(maxPriority Integer) (operator, error) {
	a, err := p.op(maxPriority)
	if err != nil {
		return operator{}, errNoOp
	}

	m := p.module()
	if op := m.operators[a][operatorClassInfix]; op != (operator{}) {
		l, _ := op.bindingPriorities()
		if l <= maxPriority {
			return op, nil
		}
	}
	if op := m.operators[a][operatorClassPostfix]; op != (operator{}) {
		l, _ := op.bindingPriorities()
		if l <= maxPriority {
			return op, nil
		}
	}

	p.backup()
	return operator{}, errNoOp
}

func (p *Parser) op(maxPriority Integer) (Atom, error) {
	if a, err := p.atom(); err == nil {
		switch a {
		case atomEmptyList:
			p.backup()
			if p.current().kind == tokenCloseList {
				p.backup()
			}
			return 0, errNoOp
		case atomEmptyBlock:
			p.backup()
			if p.current().kind == tokenCloseCurly {
				p.backup()
			}
			return 0, errNoOp
		default:
			return a, nil
		}
	}

	t, err := p.next()
	if err != nil {
		return 0, err
	}
	switch t.kind {
	case tokenComma:
		if maxPriority >= 1000 {
			return NewAtom(t.val), nil
		}
	case tokenBar:
		return NewAtom(t.val), nil
	}

	p.backup()
	return 0, p.unexpected()
}

func (p *Parser) term0(maxPriority Integer) (Term, error) {
	t, err := p.next()
	if err != nil {
		return nil, err
	}
	switch t.kind {
	case tokenOpen, tokenOpenCT:
		return p.openClose()
	case tokenInteger:
		return integer(1, t.val)
	case tokenFloatNumber:
		return float(1, t.val)
	case tokenVariable:
		return p.variable(t.val)
	case tokenOpenList:
		if t, _ := p.next(); t.kind == tokenCloseList {
			p.backup()
			p.backup()
			break
		}
		p.backup()
		return p.list()
	case tokenOpenCurly:
		if t, _ := p.next(); t.kind == tokenCloseCurly {
			p.backup()
			p.backup()
			break
		}
		p.backup()
		return p.curlyBracketedTerm()
	case tokenDoubleQuotedList:
		m := p.module()
		switch m.doubleQuotes {
		case doubleQuotesChars:
			return CharList(unDoubleQuote(t.val)), nil
		case doubleQuotesCodes:
			return CodeList(unDoubleQuote(t.val)), nil
		default:
			p.backup()
			break
		}
	default:
		p.backup()
	}

	return p.term0Atom(maxPriority)
}

func (p *Parser) term0Atom(maxPriority Integer) (Term, error) {
	a, err := p.atom()
	if err != nil {
		return nil, err
	}

	if a == atomMinus {
		t, err := p.next()
		if err != nil {
			return nil, err
		}
		switch t.kind {
		case tokenInteger:
			return integer(-1, t.val)
		case tokenFloatNumber:
			return float(-1, t.val)
		default:
			p.backup()
		}
	}

	t, err := p.functionalNotation(a)
	if err != nil {
		return nil, err
	}

	// 6.3.1.3 An atom which is an operator shall not be the immediate operand (3.120) of an operator.
	m := p.module()
	if t, ok := t.(Atom); ok && maxPriority < 1201 && m.operators.defined(t) {
		p.backup()
		return nil, p.unexpected()
	}

	if p.placeholder != 0 && t == p.placeholder {
		if len(p.args) == 0 {
			return nil, errPlaceholder
		}
		t, p.args = p.args[0], p.args[1:]
	}

	return t, nil
}

func (p *Parser) variable(s string) (Term, error) {
	if s == "_" {
		return NewVariable(), nil
	}
	n := NewAtom(s)
	for i, pv := range p.Vars {
		if pv.Name == n {
			p.Vars[i].Count++
			return pv.Variable, nil
		}
	}
	v := NewVariable()
	p.Vars = append(p.Vars, ParsedVariable{Name: n, Variable: v, Count: 1})
	return v, nil
}

func (p *Parser) openClose() (Term, error) {
	t, err := p.term(1201)
	if err != nil {
		return nil, err
	}
	if t, _ := p.next(); t.kind != tokenClose {
		p.backup()
		return nil, p.unexpected()
	}
	return t, nil
}

func (p *Parser) atom() (Atom, error) {
	if a, err := p.name(); err == nil {
		return a, nil
	}

	t, err := p.next()
	if err != nil {
		return 0, err
	}
	switch t.kind {
	case tokenOpenList:
		t, err := p.next()
		if err != nil {
			return 0, err
		}
		switch t.kind {
		case tokenCloseList:
			return atomEmptyList, nil
		default:
			p.backup()
			p.backup()
			return 0, p.unexpected()
		}
	case tokenOpenCurly:
		t, err := p.next()
		if err != nil {
			return 0, err
		}
		switch t.kind {
		case tokenCloseCurly:
			return atomEmptyBlock, nil
		default:
			p.backup()
			p.backup()
			return 0, p.unexpected()
		}
	case tokenDoubleQuotedList:
		m := p.module()
		switch m.doubleQuotes {
		case doubleQuotesAtom:
			return NewAtom(unDoubleQuote(t.val)), nil
		default:
			p.backup()
			return 0, p.unexpected()
		}
	default:
		p.backup()
		return 0, p.unexpected()
	}
}

func (p *Parser) name() (Atom, error) {
	t, err := p.next()
	if err != nil {
		return 0, err
	}
	switch t.kind {
	case tokenLetterDigit, tokenGraphic, tokenSemicolon, tokenCut:
		return NewAtom(t.val), nil
	case tokenQuoted:
		return NewAtom(unquote(t.val)), nil
	default:
		p.backup()
		return 0, p.unexpected()
	}
}

func (p *Parser) list() (Term, error) {
	arg, err := p.arg()
	if err != nil {
		return nil, err
	}
	args := []Term{arg}
	for {
		switch t, _ := p.next(); t.kind {
		case tokenComma:
			arg, err := p.arg()
			if err != nil {
				return nil, err
			}
			args = append(args, arg)
		case tokenBar:
			rest, err := p.arg()
			if err != nil {
				return nil, err
			}

			switch t, _ := p.next(); t.kind {
			case tokenCloseList:
				if len(args) == 1 {
					return Cons(args[0], rest), nil
				}
				return PartialList(rest, args...), nil
			default:
				p.backup()
				return nil, p.unexpected()
			}
		case tokenCloseList:
			return List(args...), nil
		default:
			p.backup()
			return nil, p.unexpected()
		}
	}
}

func (p *Parser) curlyBracketedTerm() (Term, error) {
	t, err := p.term(1201)
	if err != nil {
		return nil, err
	}

	if t, _ := p.next(); t.kind != tokenCloseCurly {
		p.backup()
		return nil, p.unexpected()
	}

	return atomEmptyBlock.Apply(t), nil
}

func (p *Parser) functionalNotation(functor Atom) (Term, error) {
	switch t, _ := p.next(); t.kind {
	case tokenOpenCT:
		arg, err := p.arg()
		if err != nil {
			return nil, err
		}
		args := []Term{arg}
		for {
			switch t, _ := p.next(); t.kind {
			case tokenComma:
				arg, err := p.arg()
				if err != nil {
					return nil, err
				}
				args = append(args, arg)
			case tokenClose:
				return functor.Apply(args...), nil
			default:
				p.backup()
				return nil, p.unexpected()
			}
		}
	default:
		p.backup()
		return functor, nil
	}
}

func (p *Parser) arg() (Term, error) {
	if arg, err := p.atom(); err == nil {
		m := p.module()
		if m.operators.defined(arg) {
			// Check if this atom is not followed by its own arguments.
			switch t, _ := p.next(); t.kind {
			case tokenComma, tokenClose, tokenBar, tokenCloseList:
				p.backup()
				return arg, nil
			default:
				p.backup()
			}
		}
		p.backup()
		if p.current().kind == tokenCloseList || p.current().kind == tokenCloseCurly {
			p.backup() // Unquoted [] or {} consist of 2 tokens.
		}
	}

	return p.term(999)
}

func integer(sign int64, s string) (Integer, error) {
	base := 10
	switch {
	case strings.HasPrefix(s, "0'"):
		s = s[2:]
		s = quotedIdentEscapePattern.ReplaceAllStringFunc(s, quotedIdentUnescape)
		return Integer(sign * int64([]rune(s)[0])), nil
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
		return 0, representationError(flagMinInteger, nil)
	case big.Below:
		return 0, representationError(flagMaxInteger, nil)
	default:
		return Integer(i), nil
	}
}

func float(sign float64, s string) (Float, error) {
	bf, _, _ := big.ParseFloat(s, 10, 0, big.ToZero)
	bf.Mul(big.NewFloat(sign), bf)

	f, _ := bf.Float64()
	return Float(f), nil
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

type tokenRingBuffer struct {
	buf        [4]Token
	start, end int
}

func (b *tokenRingBuffer) put(t Token) {
	b.buf[b.end] = t
	b.end++
	b.end %= len(b.buf)
}

func (b *tokenRingBuffer) get() Token {
	t := b.buf[b.start]
	b.start++
	b.start %= len(b.buf)
	return t
}

func (b *tokenRingBuffer) current() Token {
	return b.buf[b.start]
}

func (b *tokenRingBuffer) empty() bool {
	return b.start == b.end
}

func (b *tokenRingBuffer) backup() {
	b.start--
	b.start %= len(b.buf)
	if b.start < 0 {
		b.start += len(b.buf)
	}
}

type unexpectedTokenError struct {
	actual Token
}

func (e unexpectedTokenError) Error() string {
	return fmt.Sprintf("unexpected token: %s", e.actual)
}
