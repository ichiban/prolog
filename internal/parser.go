package internal

import (
	"errors"
	"fmt"
	"github.com/ichiban/prolog/internal/ring"
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

	errRepresentationMaxInteger = errors.New("representation max integer")
	errRepresentationMinInteger = errors.New("representation min integer")
)

// Parser turns bytes into Term.
type Parser struct {
	Lexer

	pool *TermPool

	Vars []ParsedVariable

	placeholder Atom
	args        []Term

	buf *ring.Buffer[Token]
}

// ParsedVariable is a set of information regarding a variable in a parsed term.
type ParsedVariable struct {
	Name     Atom
	Variable Variable
	Count    int
}

// NewParser creates a new parser from the Module and io.RuneReader.
func NewParser(pool *TermPool, m func() *Module, r io.RuneReader) *Parser {
	return &Parser{
		Lexer: Lexer{
			module: m,
			input:  newRuneRingBuffer(r),
		},
		pool: pool,
		buf:  ring.NewBuffer[Token](4),
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
		return p.pool.PutFloat(o.Float())
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		return p.pool.PutInteger(o.Int())
	case reflect.String:
		m := p.module()
		switch m.doubleQuotes {
		case doubleQuotesCodes:
			s := o.String()
			var ids []Term
			for _, r := range s {
				id, err := p.pool.PutInteger(int64(r))
				if err != nil {
					return 0, err
				}
				ids = append(ids, id)
			}
			return p.pool.PutList(ids...)
		case doubleQuotesAtom:
			return p.pool.PutAtom(NewAtom(o.String()))
		default:
			return p.pool.PutString(o.String())
		}
	case reflect.Array, reflect.Slice:
		l := o.Len()
		es := make([]Term, l)
		for i := 0; i < l; i++ {
			var err error
			es[i], err = p.termOf(o.Index(i))
			if err != nil {
				return 0, err
			}
		}
		return p.pool.PutList(es...)
	default:
		return 0, fmt.Errorf("can't convert to term: %v", o)
	}
}

func (p *Parser) next() (Token, error) {
	if p.buf.Empty() {
		t, err := p.Token()
		if err != nil {
			return Token{}, err
		}
		p.buf.Put(t)
	}
	return p.buf.Get(), nil
}

func (p *Parser) backup() {
	p.buf.Backup()
}

func (p *Parser) current() Token {
	return p.buf.Current()
}

func (p *Parser) unexpected() error {
	return unexpectedTokenError{actual: p.current()}
}

// Term parses a term followed by a full stop.
func (p *Parser) Term() (Term, error) {
	t, err := p.term(1201)
	if err != nil {
		return 0, err
	}

	switch n, _ := p.next(); n.kind {
	case tokenEnd:
		break
	default:
		p.backup()
		return 0, p.unexpected()
	}

	if len(p.args) != 0 {
		return 0, errPlaceholder
	}

	return t, nil
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

type OperatorSpecifier uint8

const (
	OperatorSpecifierFX  = OperatorSpecifier(operatorClassPrefix<<2 + 1)
	OperatorSpecifierFY  = OperatorSpecifier(operatorClassPrefix<<2 + 2)
	OperatorSpecifierXF  = OperatorSpecifier(operatorClassPostfix<<2 + 1)
	OperatorSpecifierYF  = OperatorSpecifier(operatorClassPostfix<<2 + 2)
	OperatorSpecifierXFX = OperatorSpecifier(operatorClassInfix<<2 + 1)
	OperatorSpecifierXFY = OperatorSpecifier(operatorClassInfix<<2 + 2)
	OperatorSpecifierYFX = OperatorSpecifier(operatorClassInfix<<2 + 3)
)

func (s OperatorSpecifier) class() operatorClass {
	return operatorClass((s & (0b11 << 2)) >> 2)
}

func (s OperatorSpecifier) arity() int {
	return [...]int{
		OperatorSpecifierFX:  1,
		OperatorSpecifierFY:  1,
		OperatorSpecifierXF:  1,
		OperatorSpecifierYF:  1,
		OperatorSpecifierXFX: 2,
		OperatorSpecifierXFY: 2,
		OperatorSpecifierYFX: 2,
	}[s]
}

type operators map[Atom][_operatorClassLen]Operator

func (ops *operators) defined(name Atom) bool {
	ops.init()
	_, ok := (*ops)[name]
	return ok
}

func (ops *operators) definedInClass(name Atom, class operatorClass) bool {
	ops.init()
	return (*ops)[name][class] != Operator{}
}

func (ops *operators) define(p int16, spec OperatorSpecifier, op Atom) {
	if p == 0 {
		return
	}
	ops.init()
	os := (*ops)[op]
	os[spec.class()] = Operator{
		Priority:  p,
		Specifier: spec,
		Name:      op,
	}
	(*ops)[op] = os
}

func (ops *operators) init() {
	if *ops != nil {
		return
	}
	*ops = map[Atom][3]Operator{}
}

func (ops *operators) reset() {
	for name := range *ops {
		(*ops)[name] = [3]Operator{}
	}
}

func (ops *operators) remove(name Atom, class operatorClass) {
	os := (*ops)[name]
	os[class] = Operator{}
	if os == ([_operatorClassLen]Operator{}) {
		delete(*ops, name)
		return
	}
	(*ops)[name] = os
}

type Operator struct {
	Priority  int16 // 1 ~ 1200
	Specifier OperatorSpecifier
	Name      Atom
}

// Pratt parser's binding powers but in Prolog Priority.
func (o *Operator) bindingPriorities() (int16, int16) {
	const maxPriority = int16(1202)
	type lr struct {
		left, right int16
	}
	p := [...]lr{
		OperatorSpecifierFX:  {maxPriority, o.Priority - 1},
		OperatorSpecifierFY:  {maxPriority, o.Priority},
		OperatorSpecifierXF:  {o.Priority - 1, maxPriority},
		OperatorSpecifierYF:  {o.Priority, maxPriority},
		OperatorSpecifierXFX: {o.Priority - 1, o.Priority - 1},
		OperatorSpecifierXFY: {o.Priority - 1, o.Priority},
		OperatorSpecifierYFX: {o.Priority, o.Priority - 1},
	}[o.Specifier]
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
func (p *Parser) term(maxPriority int16) (Term, error) {
	var lhs Term
	switch op, err := p.prefix(maxPriority); {
	case err == nil:
		_, rbp := op.bindingPriorities()
		t, err := p.term(rbp)
		if err != nil {
			p.backup()
			return p.term0(maxPriority)
		}
		lhs, err = p.pool.PutCompound(op.Name, t)
		if err != nil {
			return 0, err
		}
	case errors.Is(err, errNoOp):
		lhs, err = p.term0(maxPriority)
		if err != nil {
			return 0, err
		}
	default:
		return 0, err
	}

	for {
		op, err := p.infix(maxPriority)
		if err != nil {
			break
		}
		switch _, rbp := op.bindingPriorities(); {
		case rbp > 1200:
			lhs, err = p.pool.PutCompound(op.Name, lhs)
			if err != nil {
				return 0, err
			}
		default:
			rhs, err := p.term(rbp)
			if err != nil {
				return 0, err
			}
			lhs, err = p.pool.PutCompound(op.Name, lhs, rhs)
			if err != nil {
				return 0, err
			}
		}
	}

	return lhs, nil
}

func (p *Parser) prefix(maxPriority int16) (Operator, error) {
	a, err := p.op(maxPriority)
	if err != nil {
		return Operator{}, errNoOp
	}

	if a == Atom('-') {
		t, err := p.next()
		if err != nil {
			return Operator{}, err
		}
		switch t.kind {
		case tokenInteger, tokenFloatNumber:
			p.backup()
			p.backup()
			return Operator{}, errNoOp
		default:
			p.backup()
		}
	}

	t, err := p.next()
	if err != nil {
		return Operator{}, err
	}
	switch t.kind {
	case tokenOpenCT:
		p.backup()
		p.backup()
		return Operator{}, errNoOp
	default:
		p.backup()
	}

	m := p.module()
	if op := m.operators[a][operatorClassPrefix]; op != (Operator{}) && op.Priority <= maxPriority {
		return op, nil
	}

	p.backup()
	return Operator{}, errNoOp
}

func (p *Parser) infix(maxPriority int16) (Operator, error) {
	a, err := p.op(maxPriority)
	if err != nil {
		return Operator{}, errNoOp
	}

	m := p.module()
	if op := m.operators[a][operatorClassInfix]; op != (Operator{}) {
		l, _ := op.bindingPriorities()
		if l <= maxPriority {
			return op, nil
		}
	}
	if op := m.operators[a][operatorClassPostfix]; op != (Operator{}) {
		l, _ := op.bindingPriorities()
		if l <= maxPriority {
			return op, nil
		}
	}

	p.backup()
	return Operator{}, errNoOp
}

func (p *Parser) op(maxPriority int16) (Atom, error) {
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
	case tokenBar:
		return NewAtom(t.val), nil
	case tokenComma:
		if maxPriority >= 1000 {
			return NewAtom(t.val), nil
		}
		fallthrough
	default:
		p.backup()
		return 0, p.unexpected()
	}
}

func (p *Parser) term0(maxPriority int16) (Term, error) {
	t, err := p.next()
	if err != nil {
		return 0, err
	}
	switch t.kind {
	case tokenOpen, tokenOpenCT:
		return p.openClose()
	case tokenInteger:
		i, err := integer(1, t.val)
		if err != nil {
			return 0, err
		}
		return p.pool.PutInteger(i)
	case tokenFloatNumber:
		f, err := float(1, t.val)
		if err != nil {
			return 0, err
		}
		return p.pool.PutFloat(f)
	case tokenVariable:
		v := p.variable(t.val)
		return p.pool.PutVariable(v)
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
			return p.pool.PutString(unDoubleQuote(t.val))
		case doubleQuotesCodes:
			var ids []Term
			for _, r := range unDoubleQuote(t.val) {
				id, err := p.pool.PutInteger(int64(r))
				if err != nil {
					return 0, err
				}
				ids = append(ids, id)
			}
			return p.pool.PutList(ids...)
		default:
			p.backup()
			break
		}
	default:
		p.backup()
	}

	return p.term0Atom(maxPriority)
}

func (p *Parser) term0Atom(maxPriority int16) (Term, error) {
	a, err := p.atom()
	if err != nil {
		return 0, err
	}

	if a == Atom('-') {
		t, err := p.next()
		if err != nil {
			return 0, err
		}
		switch t.kind {
		case tokenInteger:
			i, err := integer(-1, t.val)
			if err != nil {
				return 0, err
			}
			return p.pool.PutInteger(i)
		case tokenFloatNumber:
			f, err := float(-1, t.val)
			if err != nil {
				return 0, err
			}
			return p.pool.PutFloat(f)
		default:
			p.backup()
		}
	}

	t, err := p.functionalNotation(a)
	if err != nil {
		return 0, err
	}

	// 6.3.1.3 An atom which is an Operator shall not be the immediate operand (3.120) of an Operator.
	m := p.module()
	if t, ok := p.pool.Atom(t); ok && maxPriority < 1201 && m.operators.defined(t) {
		p.backup()
		return 0, p.unexpected()
	}

	if a, ok := p.pool.Atom(t); ok && p.placeholder != 0 && a == p.placeholder {
		if len(p.args) == 0 {
			return 0, errPlaceholder
		}
		t, p.args = p.args[0], p.args[1:]
	}

	return t, nil
}

func (p *Parser) variable(s string) Variable {
	if s == "_" {
		return NewVariable(p.pool)
	}
	n := NewAtom(s)
	for i, pv := range p.Vars {
		if pv.Name == n {
			p.Vars[i].Count++
			return pv.Variable
		}
	}
	v := NewVariable(p.pool)
	p.Vars = append(p.Vars, ParsedVariable{Name: n, Variable: v, Count: 1})
	return v
}

func (p *Parser) openClose() (Term, error) {
	t, err := p.term(1201)
	if err != nil {
		return 0, err
	}
	if t, _ := p.next(); t.kind != tokenClose {
		p.backup()
		return 0, p.unexpected()
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
			return NewAtom(`[]`), nil
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
			return NewAtom(`{}`), nil
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
		return 0, err
	}
	args := []Term{arg}
	for {
		switch t, _ := p.next(); t.kind {
		case tokenComma:
			arg, err := p.arg()
			if err != nil {
				return 0, err
			}
			args = append(args, arg)
		case tokenBar:
			rest, err := p.arg()
			if err != nil {
				return 0, err
			}

			switch t, _ := p.next(); t.kind {
			case tokenCloseList:
				return p.pool.PutPartialList(rest, args...)
			default:
				p.backup()
				return 0, p.unexpected()
			}
		case tokenCloseList:
			return p.pool.PutList(args...)
		default:
			p.backup()
			return 0, p.unexpected()
		}
	}
}

func (p *Parser) curlyBracketedTerm() (Term, error) {
	t, err := p.term(1201)
	if err != nil {
		return 0, err
	}

	if t, _ := p.next(); t.kind != tokenCloseCurly {
		p.backup()
		return 0, p.unexpected()
	}

	return p.pool.PutCompound(atomEmptyBlock, t)
}

func (p *Parser) functionalNotation(functor Atom) (Term, error) {
	switch t, _ := p.next(); t.kind {
	case tokenOpenCT:
		arg, err := p.arg()
		if err != nil {
			return 0, err
		}
		args := []Term{arg}
		for {
			switch t, _ := p.next(); t.kind {
			case tokenComma:
				arg, err := p.arg()
				if err != nil {
					return 0, err
				}
				args = append(args, arg)
			case tokenClose:
				return p.pool.PutCompound(functor, args...)
			default:
				p.backup()
				return 0, p.unexpected()
			}
		}
	default:
		p.backup()
		return p.pool.PutAtom(functor)
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
				return p.pool.PutAtom(arg)
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

func integer(sign int64, s string) (int64, error) {
	base := 10
	switch {
	case strings.HasPrefix(s, "0'"):
		s = s[2:]
		s = quotedIdentEscapePattern.ReplaceAllStringFunc(s, quotedIdentUnescape)
		return sign * int64([]rune(s)[0]), nil
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
		return 0, errRepresentationMinInteger
	case big.Below:
		return 0, errRepresentationMaxInteger
	default:
		return i, nil
	}
}

func float(sign float64, s string) (float64, error) {
	bf, _, _ := big.ParseFloat(s, 10, 0, big.ToZero)
	bf.Mul(big.NewFloat(sign), bf)

	f, _ := bf.Float64()
	return f, nil
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

type unexpectedTokenError struct {
	actual Token
}

func (e unexpectedTokenError) Error() string {
	return fmt.Sprintf("unexpected token: %s", e.actual)
}
