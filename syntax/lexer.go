package syntax

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"strconv"
	"strings"
	"unicode"
)

// Lexer turns bytes into tokens.
type Lexer struct {
	input           *bufio.Reader
	charConversions map[rune]rune
	tokens          []Token
	pos             int
	width           int
}

// NewLexer create a lexer with an input and char conversions.
func NewLexer(input *bufio.Reader, charConversions map[rune]rune) *Lexer {
	l := Lexer{input: input, charConversions: charConversions}
	return &l
}

// Next returns the next token.
func (l *Lexer) Next(hint TokenKind) (Token, error) {
	state := [tokenLen]lexState{
		TokenEOS:      l.eos,
		TokenVariable: l.variable,
		TokenFloat:    l.number,
		TokenInteger:  l.number,
		TokenAtom:     l.atom,
		TokenComma:    l.comma,
		TokenPeriod:   l.period,
		TokenBar:      l.bar,
		TokenParenL:   l.parenL,
		TokenParenR:   l.parenR,
		TokenBracketL: l.bracketL,
		TokenBracketR: l.bracketR,
		TokenBraceL:   l.braceL,
		TokenBraceR:   l.braceR,
	}[hint]
	for state != nil && len(l.tokens) == 0 {
		r, err := l.next()
		if err != nil {
			return Token{}, err
		}
		state, err = state(r)
		if err != nil {
			return Token{}, err
		}
	}

	if len(l.tokens) > 0 {
		var t Token
		t, l.tokens = l.tokens[0], l.tokens[1:]
		return t, nil
	}

	return Token{}, errors.New("no match")
}

const etx = 0x2

func (l *Lexer) next() (rune, error) {
	r, w, err := l.input.ReadRune()
	switch err {
	case nil:
		break
	case io.EOF:
		r = etx
		w = 1
	default:
		return 0, err
	}
	l.width = w
	l.pos += l.width
	return r, nil
}

func (l *Lexer) backup() {
	_ = l.input.UnreadRune()
	l.pos -= l.width
}

func (l *Lexer) emit(t Token) {
	l.tokens = append(l.tokens, t)
}

// Token is a smallest meaningful unit of prolog program.
type Token struct {
	Kind TokenKind
	Val  string
}

func (t Token) String() string {
	return fmt.Sprintf("<%s %s>", t.Kind, t.Val)
}

// TokenKind is a type of Token.
type TokenKind byte

const (
	// TokenEOS represents an end of token stream.
	TokenEOS TokenKind = iota

	// TokenVariable represents a variable token.
	TokenVariable

	// TokenFloat represents a floating-point token.
	TokenFloat

	// TokenInteger represents an integer token.
	TokenInteger

	// TokenAtom represents an atom token.
	TokenAtom

	// TokenComma represents a comma.
	TokenComma

	// TokenPeriod represents a period.
	TokenPeriod

	// TokenBar represents a bar.
	TokenBar

	// TokenParenL represents an open parenthesis.
	TokenParenL

	// TokenParenR represents a close parenthesis.
	TokenParenR

	// TokenBracketL represents an open bracket.
	TokenBracketL

	// TokenBracketR represents a close bracket.
	TokenBracketR

	// TokenBraceL represents an open brace.
	TokenBraceL

	// TokenBraceR represents a close brace.
	TokenBraceR

	tokenLen
)

func (k TokenKind) String() string {
	return [tokenLen]string{
		TokenEOS:      "eos",
		TokenVariable: "variable",
		TokenFloat:    "float",
		TokenInteger:  "integer",
		TokenAtom:     "atom",
		TokenComma:    "comma",
		TokenPeriod:   "period",
		TokenBar:      "bar",
		TokenParenL:   "paren L",
		TokenParenR:   "paren R",
		TokenBracketL: "bracket L",
		TokenBracketR: "bracket R",
		TokenBraceL:   "brace L",
		TokenBraceR:   "brace R",
	}[k]
}

func (l *Lexer) conv(r rune) rune {
	if r, ok := l.charConversions[r]; ok {
		return r
	}
	return r
}

type lexState func(rune) (lexState, error)

func (l *Lexer) eos(r rune) (lexState, error) {
	r = l.conv(r)
	switch {
	case r == etx:
		l.emit(Token{Kind: TokenEOS})
		return nil, nil
	case unicode.IsSpace(r):
		return l.eos, nil
	case r == '%':
		return l.singleLineComment(l.eos)
	case r == '/':
		var b strings.Builder
		if _, err := b.WriteRune(r); err != nil {
			return nil, err
		}
		return l.multiLineCommentBegin(&b, l.eos)
	default:
		l.backup()
		return nil, UnexpectedRuneError{rune: r}
	}
}

func (l *Lexer) comma(r rune) (lexState, error) {
	r = l.conv(r)
	switch {
	case r == etx:
		return nil, ErrInsufficient
	case unicode.IsSpace(r):
		return l.comma, nil
	case r == ',':
		l.emit(Token{Kind: TokenComma, Val: string(r)})
		return nil, nil
	default:
		l.backup()
		return nil, UnexpectedRuneError{rune: r}
	}
}

func (l *Lexer) period(r rune) (lexState, error) {
	r = l.conv(r)
	switch {
	case r == etx:
		return nil, ErrInsufficient
	case unicode.IsSpace(r):
		return l.period, nil
	case r == '.':
		l.emit(Token{Kind: TokenPeriod, Val: string(r)})
		return nil, nil
	default:
		l.backup()
		return nil, UnexpectedRuneError{rune: r}
	}
}

func (l *Lexer) bar(r rune) (lexState, error) {
	r = l.conv(r)
	switch {
	case r == etx:
		return nil, ErrInsufficient
	case unicode.IsSpace(r):
		return l.bar, nil
	case r == '|':
		l.emit(Token{Kind: TokenBar, Val: string(r)})
		return nil, nil
	default:
		l.backup()
		return nil, UnexpectedRuneError{rune: r}
	}
}

func (l *Lexer) parenL(r rune) (lexState, error) {
	r = l.conv(r)
	switch {
	case r == etx:
		return nil, ErrInsufficient
	case unicode.IsSpace(r):
		return l.parenL, nil
	case r == '(':
		l.emit(Token{Kind: TokenParenL, Val: string(r)})
		return nil, nil
	default:
		l.backup()
		return nil, UnexpectedRuneError{rune: r}
	}
}

func (l *Lexer) parenR(r rune) (lexState, error) {
	r = l.conv(r)
	switch {
	case r == etx:
		return nil, ErrInsufficient
	case unicode.IsSpace(r):
		return l.parenR, nil
	case r == ')':
		l.emit(Token{Kind: TokenParenR, Val: string(r)})
		return nil, nil
	default:
		l.backup()
		return nil, UnexpectedRuneError{rune: r}
	}
}

func (l *Lexer) bracketL(r rune) (lexState, error) {
	r = l.conv(r)
	switch {
	case r == etx:
		return nil, ErrInsufficient
	case unicode.IsSpace(r):
		return l.bracketL, nil
	case r == '[':
		l.emit(Token{Kind: TokenBracketL, Val: string(r)})
		return nil, nil
	default:
		l.backup()
		return nil, UnexpectedRuneError{rune: r}
	}
}

func (l *Lexer) bracketR(r rune) (lexState, error) {
	r = l.conv(r)
	switch {
	case r == etx:
		return nil, ErrInsufficient
	case unicode.IsSpace(r):
		return l.bracketR, nil
	case r == ']':
		l.emit(Token{Kind: TokenBracketR, Val: string(r)})
		return nil, nil
	default:
		l.backup()
		return nil, UnexpectedRuneError{rune: r}
	}
}

func (l *Lexer) braceL(r rune) (lexState, error) {
	r = l.conv(r)
	switch {
	case r == etx:
		return nil, ErrInsufficient
	case unicode.IsSpace(r):
		return l.braceL, nil
	case r == '{':
		l.emit(Token{Kind: TokenBraceL, Val: string(r)})
		return nil, nil
	default:
		l.backup()
		return nil, UnexpectedRuneError{rune: r}
	}
}

func (l *Lexer) braceR(r rune) (lexState, error) {
	r = l.conv(r)
	switch {
	case r == etx:
		return nil, ErrInsufficient
	case unicode.IsSpace(r):
		return l.braceR, nil
	case r == '}':
		l.emit(Token{Kind: TokenBraceR, Val: string(r)})
		return nil, nil
	default:
		l.backup()
		return nil, UnexpectedRuneError{rune: r}
	}
}

func (l *Lexer) atom(r rune) (lexState, error) {
	r = l.conv(r)
	switch {
	case r == etx:
		return nil, ErrInsufficient
	case unicode.IsSpace(r):
		return l.atom, nil
	case r == '%':
		return l.singleLineComment(l.atom)
	case r == '/':
		var b strings.Builder
		if _, err := b.WriteRune(r); err != nil {
			return nil, err
		}
		return l.multiLineCommentBegin(&b, l.atom)
	case unicode.IsLower(r):
		var b strings.Builder
		if _, err := b.WriteRune(r); err != nil {
			return nil, err
		}
		return l.normalAtom(&b)
	case isGraphic(r):
		var b strings.Builder
		if _, err := b.WriteRune(r); err != nil {
			return nil, err
		}
		return l.graphic(&b)
	case r == ',', r == ';', r == '!':
		l.emit(Token{Kind: TokenAtom, Val: string(r)})
		return nil, nil
	case r == '[':
		return l.squareBracket, nil
	case r == '{':
		return l.curlyBracket, nil
	case r == '\'':
		var b strings.Builder
		return l.quotedAtom(&b)
	default:
		l.backup()
		return nil, UnexpectedRuneError{rune: r}
	}
}

func (l *Lexer) normalAtom(b *strings.Builder) (lexState, error) {
	return func(r rune) (lexState, error) {
		r = l.conv(r)
		switch {
		case unicode.IsLetter(r), unicode.IsNumber(r), r == '_':
			if _, err := b.WriteRune(r); err != nil {
				return nil, err
			}
			return l.normalAtom(b)
		default:
			l.backup()
			l.emit(Token{Kind: TokenAtom, Val: b.String()})
			return nil, nil
		}
	}, nil
}

func (l *Lexer) quotedAtom(b *strings.Builder) (lexState, error) {
	return func(r rune) (lexState, error) {
		switch r {
		case etx:
			return nil, ErrInsufficient
		case '\'':
			return l.quotedAtomQuote(b)
		case '\\':
			return l.quotedAtomSlash(b)
		default:
			if _, err := b.WriteRune(r); err != nil {
				return nil, err
			}
			return l.quotedAtom(b)
		}
	}, nil
}

func (l *Lexer) quotedAtomQuote(b *strings.Builder) (lexState, error) {
	return func(r rune) (lexState, error) {
		switch r {
		case '\'':
			if _, err := b.WriteRune(r); err != nil {
				return nil, err
			}
			return l.quotedAtom(b)
		default:
			l.backup()
			l.emit(Token{Kind: TokenAtom, Val: b.String()})
			return nil, nil
		}
	}, nil
}

func (l *Lexer) quotedAtomSlash(b *strings.Builder) (lexState, error) {
	return func(r rune) (lexState, error) {
		switch {
		case r == etx:
			return nil, ErrInsufficient
		case r == '\n':
			return l.quotedAtom(b)
		case r == 'x':
			var val strings.Builder
			return l.quotedAtomSlashCode(b, 16, &val)
		case unicode.IsNumber(r):
			var val strings.Builder
			if _, err := val.WriteRune(r); err != nil {
				return nil, err
			}
			return l.quotedAtomSlashCode(b, 8, &val)
		case r == 'a':
			if _, err := b.WriteRune('\a'); err != nil {
				return nil, err
			}
			return l.quotedAtom(b)
		case r == 'b':
			if _, err := b.WriteRune('\b'); err != nil {
				return nil, err
			}
			return l.quotedAtom(b)
		case r == 'f':
			if _, err := b.WriteRune('\f'); err != nil {
				return nil, err
			}
			return l.quotedAtom(b)
		case r == 'n':
			if _, err := b.WriteRune('\n'); err != nil {
				return nil, err
			}
			return l.quotedAtom(b)
		case r == 'r':
			if _, err := b.WriteRune('\r'); err != nil {
				return nil, err
			}
			return l.quotedAtom(b)
		case r == 't':
			if _, err := b.WriteRune('\t'); err != nil {
				return nil, err
			}
			return l.quotedAtom(b)
		case r == 'v':
			if _, err := b.WriteRune('\v'); err != nil {
				return nil, err
			}
			return l.quotedAtom(b)
		case r == '\\':
			if _, err := b.WriteRune('\\'); err != nil {
				return nil, err
			}
			return l.quotedAtom(b)
		case r == '\'':
			if _, err := b.WriteRune('\''); err != nil {
				return nil, err
			}
			return l.quotedAtom(b)
		case r == '"':
			if _, err := b.WriteRune('"'); err != nil {
				return nil, err
			}
			return l.quotedAtom(b)
		case r == '`':
			if _, err := b.WriteRune('`'); err != nil {
				return nil, err
			}
			return l.quotedAtom(b)
		default:
			return nil, UnexpectedRuneError{rune: r}
		}
	}, nil
}

func (l *Lexer) quotedAtomSlashCode(b *strings.Builder, base int, val *strings.Builder) (lexState, error) {
	return func(r rune) (lexState, error) {
		switch {
		case r == etx:
			return nil, ErrInsufficient
		case unicode.IsNumber(r):
			if _, err := val.WriteRune(r); err != nil {
				return nil, err
			}
			return l.quotedAtomSlashCode(b, base, val)
		case r == '\\':
			i, err := strconv.ParseInt(val.String(), base, 4*8) // rune is up to 4 bytes
			if err != nil {
				return nil, err
			}
			if _, err := b.WriteRune(rune(i)); err != nil {
				return nil, err
			}
			return l.quotedAtom(b)
		default:
			return nil, UnexpectedRuneError{rune: r}
		}
	}, nil
}

func (l *Lexer) squareBracket(r rune) (lexState, error) {
	r = l.conv(r)
	switch {
	case r == ']':
		l.emit(Token{Kind: TokenAtom, Val: "[]"})
		return nil, nil
	default:
		l.backup()
		l.emit(Token{Kind: TokenBracketL, Val: "["})
		return nil, nil
	}
}

func (l *Lexer) curlyBracket(r rune) (lexState, error) {
	r = l.conv(r)
	switch {
	case r == '}':
		l.emit(Token{Kind: TokenAtom, Val: "{}"})
		return nil, nil
	default:
		l.backup()
		l.emit(Token{Kind: TokenBraceL, Val: "{"})
		return nil, nil
	}
}

func (l *Lexer) floatMantissa(b *strings.Builder) (lexState, error) {
	return func(r rune) (lexState, error) {
		r = l.conv(r)
		switch {
		case unicode.IsNumber(r):
			if _, err := b.WriteRune(r); err != nil {
				return nil, err
			}
			return l.floatMantissa(b)
		case r == 'E' || r == 'e':
			if _, err := b.WriteRune(r); err != nil {
				return nil, err
			}
			return l.floatE(b)
		default:
			l.backup()
			l.emit(Token{Kind: TokenFloat, Val: b.String()})
			return nil, nil
		}
	}, nil
}

func (l *Lexer) floatE(b *strings.Builder) (lexState, error) {
	return func(r rune) (lexState, error) {
		r = l.conv(r)
		switch {
		case r == etx:
			return nil, ErrInsufficient
		case unicode.IsNumber(r), r == '+', r == '-':
			if _, err := b.WriteRune(r); err != nil {
				return nil, err
			}
			return l.floatExponent(b)
		default:
			return nil, UnexpectedRuneError{rune: r}
		}
	}, nil
}

func (l *Lexer) floatExponent(b *strings.Builder) (lexState, error) {
	return func(r rune) (lexState, error) {
		r = l.conv(r)
		switch {
		case unicode.IsNumber(r):
			if _, err := b.WriteRune(r); err != nil {
				return nil, err
			}
			return l.floatExponent(b)
		default:
			l.backup()
			l.emit(Token{Kind: TokenFloat, Val: b.String()})
			return nil, nil
		}
	}, nil
}

func (l *Lexer) integerZero(b *strings.Builder) (lexState, error) {
	return func(r rune) (lexState, error) {
		r = l.conv(r)
		switch {
		case r == 'o':
			if _, err := b.WriteRune(r); err != nil {
				return nil, err
			}
			return l.integerOctal(b)
		case r == 'x':
			if _, err := b.WriteRune(r); err != nil {
				return nil, err
			}
			return l.integerHex(b)
		case r == 'b':
			if _, err := b.WriteRune(r); err != nil {
				return nil, err
			}
			return l.integerBinary(b)
		case r == '\'':
			if _, err := b.WriteRune(r); err != nil {
				return nil, err
			}
			return l.integerChar(b)
		case unicode.IsDigit(r):
			if _, err := b.WriteRune(r); err != nil {
				return nil, err
			}
			return l.integerDecimal(b)
		default:
			l.backup()
			l.emit(Token{Kind: TokenInteger, Val: b.String()})
			return nil, nil
		}
	}, nil
}

func (l *Lexer) integerOctal(b *strings.Builder) (lexState, error) {
	return func(r rune) (lexState, error) {
		r = l.conv(r)
		switch r {
		case '0', '1', '2', '3', '4', '5', '6', '7':
			if _, err := b.WriteRune(r); err != nil {
				return nil, err
			}
			return l.integerOctal(b)
		default:
			l.backup()
			l.emit(Token{Kind: TokenInteger, Val: b.String()})
			return nil, nil
		}
	}, nil
}

func (l *Lexer) integerHex(b *strings.Builder) (lexState, error) {
	return func(r rune) (lexState, error) {
		r = l.conv(r)
		switch unicode.ToUpper(r) {
		case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F':
			if _, err := b.WriteRune(r); err != nil {
				return nil, err
			}
			return l.integerHex(b)
		default:
			l.backup()
			l.emit(Token{Kind: TokenInteger, Val: b.String()})
			return nil, nil
		}
	}, nil
}

func (l *Lexer) integerBinary(b *strings.Builder) (lexState, error) {
	return func(r rune) (lexState, error) {
		r = l.conv(r)
		switch r {
		case '0', '1':
			if _, err := b.WriteRune(r); err != nil {
				return nil, err
			}
			return l.integerBinary(b)
		default:
			l.backup()
			l.emit(Token{Kind: TokenInteger, Val: b.String()})
			return nil, nil
		}
	}, nil
}

func (l *Lexer) integerChar(b *strings.Builder) (lexState, error) {
	return func(r rune) (lexState, error) {
		r = l.conv(r)
		switch r {
		case etx:
			return nil, ErrInsufficient
		case '\\':
			return l.integerCharEscape(b)
		default:
			if _, err := b.WriteRune(r); err != nil {
				return nil, err
			}
			l.emit(Token{Kind: TokenInteger, Val: b.String()})
			return nil, nil
		}
	}, nil
}

func (l *Lexer) integerCharEscape(b *strings.Builder) (lexState, error) {
	return func(r rune) (lexState, error) {
		r = l.conv(r)
		switch r {
		case etx:
			return nil, ErrInsufficient
		case 'a':
			if _, err := b.WriteRune('\a'); err != nil {
				return nil, err
			}
		case 'b':
			if _, err := b.WriteRune('\b'); err != nil {
				return nil, err
			}
		case 'f':
			if _, err := b.WriteRune('\f'); err != nil {
				return nil, err
			}
		case 'n':
			if _, err := b.WriteRune('\n'); err != nil {
				return nil, err
			}
		case 'r':
			if _, err := b.WriteRune('\r'); err != nil {
				return nil, err
			}
		case 't':
			if _, err := b.WriteRune('\t'); err != nil {
				return nil, err
			}
		case 'v':
			if _, err := b.WriteRune('\v'); err != nil {
				return nil, err
			}
		case '\\':
			if _, err := b.WriteRune('\\'); err != nil {
				return nil, err
			}
		case '\'':
			if _, err := b.WriteRune('\''); err != nil {
				return nil, err
			}
		default:
			return nil, UnexpectedRuneError{rune: r}
		}
		l.emit(Token{Kind: TokenInteger, Val: b.String()})
		return nil, nil
	}, nil
}

func (l *Lexer) integerDecimal(b *strings.Builder) (lexState, error) {
	return func(r rune) (lexState, error) {
		r = l.conv(r)
		switch {
		case unicode.IsNumber(r):
			if _, err := b.WriteRune(r); err != nil {
				return nil, err
			}
			return l.integerDecimal(b)
		case r == '.':
			return l.integerDot(b)
		default:
			l.backup()
			l.emit(Token{Kind: TokenInteger, Val: b.String()})
			return nil, nil
		}
	}, nil
}

func (l *Lexer) integerDot(b *strings.Builder) (lexState, error) {
	return func(r rune) (lexState, error) {
		r = l.conv(r)
		switch {
		case unicode.IsDigit(r):
			if _, err := b.WriteRune('.'); err != nil {
				return nil, err
			}
			if _, err := b.WriteRune(r); err != nil {
				return nil, err
			}
			return l.floatMantissa(b)
		default:
			l.emit(Token{Kind: TokenInteger, Val: b.String()})
			l.emit(Token{Kind: TokenPeriod, Val: "."})
			l.backup()
			return nil, nil
		}
	}, nil
}

func (l *Lexer) number(r rune) (lexState, error) {
	r = l.conv(r)
	switch {
	case r == etx:
		return nil, ErrInsufficient
	case unicode.IsSpace(r):
		return l.number, nil
	case r == '%':
		return l.singleLineComment(l.number)
	case r == '/':
		var b strings.Builder
		if _, err := b.WriteRune(r); err != nil {
			return nil, err
		}
		return l.multiLineCommentBegin(&b, l.number)
	case r == '+' || r == '-':
		var b strings.Builder
		if _, err := b.WriteRune(r); err != nil {
			return nil, err
		}
		return l.sign(&b)
	case r == '0':
		var b strings.Builder
		if _, err := b.WriteRune(r); err != nil {
			return nil, err
		}
		return l.integerZero(&b)
	case unicode.IsNumber(r):
		var b strings.Builder
		if _, err := b.WriteRune(r); err != nil {
			return nil, err
		}
		return l.integerDecimal(&b)
	default:
		l.backup()
		return nil, UnexpectedRuneError{rune: r}
	}
}

func (l *Lexer) sign(b *strings.Builder) (lexState, error) {
	return func(r rune) (lexState, error) {
		r = l.conv(r)
		switch {
		case r == etx:
			return nil, ErrInsufficient
		case r == '0':
			if _, err := b.WriteRune(r); err != nil {
				return nil, err
			}
			return l.integerZero(b)
		case unicode.IsDigit(r):
			if _, err := b.WriteRune(r); err != nil {
				return nil, err
			}
			return l.integerDecimal(b)
		default:
			l.backup()
			return l.graphic(b)
		}
	}, nil
}

func (l *Lexer) variable(r rune) (lexState, error) {
	r = l.conv(r)
	switch {
	case r == etx:
		return nil, ErrInsufficient
	case unicode.IsSpace(r):
		return l.variable, nil
	case r == '%':
		return l.singleLineComment(l.variable)
	case r == '/':
		var b strings.Builder
		if _, err := b.WriteRune(r); err != nil {
			return nil, err
		}
		return l.multiLineCommentBegin(&b, l.variable)
	case unicode.IsUpper(r), r == '_':
		var b strings.Builder
		if _, err := b.WriteRune(r); err != nil {
			return nil, err
		}
		return l.variableName(&b)
	default:
		l.backup()
		return nil, UnexpectedRuneError{rune: r}
	}
}

func (l *Lexer) variableName(b *strings.Builder) (lexState, error) {
	return func(r rune) (lexState, error) {
		r = l.conv(r)
		switch {
		case unicode.IsLetter(r), unicode.IsNumber(r), r == '_':
			if _, err := b.WriteRune(r); err != nil {
				return nil, err
			}
			return l.variableName(b)
		default:
			l.backup()
			l.emit(Token{Kind: TokenVariable, Val: b.String()})
			return nil, nil
		}
	}, nil
}

func (l *Lexer) graphic(b *strings.Builder) (lexState, error) {
	return func(r rune) (lexState, error) {
		r = l.conv(r)
		switch {
		case isGraphic(r):
			if _, err := b.WriteRune(r); err != nil {
				return nil, err
			}
			return l.graphic(b)
		default:
			l.backup()
			l.emit(Token{Kind: TokenAtom, Val: b.String()})
			return nil, nil
		}
	}, nil
}

func (l *Lexer) singleLineComment(ctx lexState) (lexState, error) {
	return func(r rune) (lexState, error) {
		r = l.conv(r)
		switch r {
		case etx:
			return nil, ErrInsufficient
		case '\n':
			return ctx, nil
		default:
			return l.singleLineComment(ctx)
		}
	}, nil
}

func (l *Lexer) multiLineCommentBegin(b *strings.Builder, ctx lexState) (lexState, error) {
	return func(r rune) (lexState, error) {
		r = l.conv(r)
		switch {
		case r == '*':
			return l.multiLineCommentBody(ctx)
		case isGraphic(r):
			if _, err := b.WriteRune(r); err != nil {
				return nil, err
			}
			return l.graphic(b)
		default:
			l.backup()
			l.emit(Token{Kind: TokenAtom, Val: "/"})
			return nil, nil
		}
	}, nil
}

func (l *Lexer) multiLineCommentBody(ctx lexState) (lexState, error) {
	return func(r rune) (lexState, error) {
		r = l.conv(r)
		switch r {
		case etx:
			return nil, ErrInsufficient
		case '*':
			return l.multiLineCommentEnd(ctx)
		default:
			return l.multiLineCommentBody(ctx)
		}
	}, nil
}

func (l *Lexer) multiLineCommentEnd(ctx lexState) (lexState, error) {
	return func(r rune) (lexState, error) {
		r = l.conv(r)
		switch r {
		case etx:
			return nil, ErrInsufficient
		case '/':
			return ctx, nil
		default:
			return l.multiLineCommentBody(ctx)
		}
	}, nil
}

func isGraphic(r rune) bool {
	return strings.ContainsRune("#$&*+-./:<=>?@^~\\", r)
}

// IsExtendedGraphic checks if the rune is a graphic token, comma, or semicolon.
func IsExtendedGraphic(r rune) bool {
	return strings.ContainsRune(",;", r) || isGraphic(r)
}

// ErrInsufficient represents an error which is raised when the given input is insufficient for a term.
var ErrInsufficient = errors.New("insufficient input")

// UnexpectedRuneError represents an error which is raised when the given input contains an unexpected rune.
type UnexpectedRuneError struct {
	rune rune
}

func (e UnexpectedRuneError) Error() string {
	return fmt.Sprintf("unexpected char: %s", string(e.rune))
}
