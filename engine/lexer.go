package engine

import (
	"bufio"
	"errors"
	"fmt"
	"io"
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
func (l *Lexer) Next() (Token, error) {
	state := l.init
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

	// TokenIdent represents an identifier token.
	TokenIdent

	// TokenQuotedIdent represents a quoted identifier token.
	TokenQuotedIdent

	// TokenGraphic represents a graphical token.
	TokenGraphic

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

	// TokenSign represents a plus/minus.
	TokenSign

	// TokenDoubleQuoted represents a double-quoted string.
	TokenDoubleQuoted

	tokenKindLen
)

func (k TokenKind) String() string {
	return [tokenKindLen]string{
		TokenEOS:          "eos",
		TokenVariable:     "variable",
		TokenFloat:        "float",
		TokenInteger:      "integer",
		TokenIdent:        "ident",
		TokenQuotedIdent:  "quoted ident",
		TokenGraphic:      "graphical",
		TokenComma:        "comma",
		TokenPeriod:       "period",
		TokenBar:          "bar",
		TokenParenL:       "paren L",
		TokenParenR:       "paren R",
		TokenBracketL:     "bracket L",
		TokenBracketR:     "bracket R",
		TokenBraceL:       "brace L",
		TokenBraceR:       "brace R",
		TokenSign:         "sign",
		TokenDoubleQuoted: "double quoted",
	}[k]
}

func (l *Lexer) conv(r rune) rune {
	if r, ok := l.charConversions[r]; ok {
		return r
	}
	return r
}

type lexState func(rune) (lexState, error)

func (l *Lexer) init(r rune) (lexState, error) {
	r = l.conv(r)
	switch {
	case r == etx:
		l.emit(Token{Kind: TokenEOS})
		return nil, nil
	case unicode.IsSpace(r):
		return l.init, nil
	case r == '%':
		return l.singleLineComment(l.init)
	case r == '/':
		var b strings.Builder
		_, _ = b.WriteRune(r)
		return l.multiLineCommentBegin(&b, l.init)
	case r == '(':
		l.emit(Token{Kind: TokenParenL, Val: string(r)})
		return nil, nil
	case r == ')':
		l.emit(Token{Kind: TokenParenR, Val: string(r)})
		return nil, nil
	case r == ',':
		l.emit(Token{Kind: TokenComma, Val: string(r)})
		return nil, nil
	case r == '.':
		var b strings.Builder
		_, _ = b.WriteRune(r)
		return l.period(&b)
	case r == '|':
		l.emit(Token{Kind: TokenBar, Val: string(r)})
		return nil, nil
	case r == ']':
		l.emit(Token{Kind: TokenBracketR, Val: string(r)})
		return nil, nil
	case r == '}':
		l.emit(Token{Kind: TokenBraceR, Val: string(r)})
		return nil, nil
	case r == '+' || r == '-':
		var b strings.Builder
		_, _ = b.WriteRune(r)
		return l.sign(&b)
	case unicode.IsNumber(r):
		l.backup()
		return l.number, nil
	case unicode.IsUpper(r), r == '_':
		l.backup()
		return l.variable, nil
	case r == '"':
		var b strings.Builder
		_, _ = b.WriteRune(r)
		return l.doubleQuoted(&b)
	default:
		l.backup()
		return l.ident, nil
	}
}

func (l *Lexer) period(b *strings.Builder) (lexState, error) {
	return func(r rune) (lexState, error) {
		switch {
		case r == etx, unicode.IsSpace(r):
			l.emit(Token{Kind: TokenPeriod, Val: b.String()})
			return nil, nil
		default:
			l.backup()
			return l.graphic(b)
		}
	}, nil
}

func (l *Lexer) ident(r rune) (lexState, error) {
	r = l.conv(r)
	switch {
	case unicode.IsLower(r):
		var b strings.Builder
		_, _ = b.WriteRune(r)
		return l.normalAtom(&b)
	case isGraphic(r):
		var b strings.Builder
		_, _ = b.WriteRune(r)
		return l.graphic(&b)
	case r == ';', r == '!':
		l.emit(Token{Kind: TokenIdent, Val: string(r)})
		return nil, nil
	case r == '[':
		return l.squareBracket, nil
	case r == '{':
		return l.curlyBracket, nil
	case r == '\'':
		var b strings.Builder
		_, _ = b.WriteRune(r)
		return l.quotedIdent(&b)
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
			_, _ = b.WriteRune(r)
			return l.normalAtom(b)
		default:
			l.backup()
			l.emit(Token{Kind: TokenIdent, Val: b.String()})
			return nil, nil
		}
	}, nil
}

func (l *Lexer) quotedIdent(b *strings.Builder) (lexState, error) {
	return func(r rune) (lexState, error) {
		switch r {
		case etx:
			return nil, ErrInsufficient
		case '\'':
			_, _ = b.WriteRune(r)
			return l.quotedIdentQuote(b)
		case '\\':
			_, _ = b.WriteRune(r)
			return l.quotedIdentSlash(b)
		default:
			_, _ = b.WriteRune(r)
			return l.quotedIdent(b)
		}
	}, nil
}

func (l *Lexer) quotedIdentQuote(b *strings.Builder) (lexState, error) {
	return func(r rune) (lexState, error) {
		switch r {
		case '\'':
			_, _ = b.WriteRune(r)
			return l.quotedIdent(b)
		default:
			l.backup()
			l.emit(Token{Kind: TokenQuotedIdent, Val: b.String()})
			return nil, nil
		}
	}, nil
}

func (l *Lexer) quotedIdentSlash(b *strings.Builder) (lexState, error) {
	return func(r rune) (lexState, error) {
		switch {
		case r == etx:
			return nil, ErrInsufficient
		case r == 'x':
			_, _ = b.WriteRune(r)
			return l.quotedIdentSlashHex(b)
		case unicode.IsNumber(r):
			_, _ = b.WriteRune(r)
			return l.quotedIdentSlashOctal(b)
		default:
			_, _ = b.WriteRune(r)
			return l.quotedIdent(b)
		}
	}, nil
}

func (l *Lexer) quotedIdentSlashHex(b *strings.Builder) (lexState, error) {
	return func(r rune) (lexState, error) {
		switch {
		case r == etx:
			return nil, ErrInsufficient
		case isHex(r):
			_, _ = b.WriteRune(r)
			return l.quotedIdentSlashHex(b)
		case r == '\\':
			_, _ = b.WriteRune(r)
			return l.quotedIdent(b)
		default:
			return nil, UnexpectedRuneError{rune: r}
		}
	}, nil
}

func (l *Lexer) quotedIdentSlashOctal(b *strings.Builder) (lexState, error) {
	return func(r rune) (lexState, error) {
		switch {
		case r == etx:
			return nil, ErrInsufficient
		case isOctal(r):
			_, _ = b.WriteRune(r)
			return l.quotedIdentSlashOctal(b)
		case r == '\\':
			_, _ = b.WriteRune(r)
			return l.quotedIdent(b)
		default:
			return nil, UnexpectedRuneError{rune: r}
		}
	}, nil
}

func (l *Lexer) squareBracket(r rune) (lexState, error) {
	r = l.conv(r)
	switch {
	case r == ']':
		l.emit(Token{Kind: TokenIdent, Val: "[]"})
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
		l.emit(Token{Kind: TokenIdent, Val: "{}"})
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
			_, _ = b.WriteRune(r)
			return l.floatMantissa(b)
		case r == 'E' || r == 'e':
			_, _ = b.WriteRune(r)
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
			_, _ = b.WriteRune(r)
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
			_, _ = b.WriteRune(r)
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
			_, _ = b.WriteRune(r)
			return l.integerOctal(b)
		case r == 'x':
			_, _ = b.WriteRune(r)
			return l.integerHex(b)
		case r == 'b':
			_, _ = b.WriteRune(r)
			return l.integerBinary(b)
		case r == '\'':
			_, _ = b.WriteRune(r)
			return l.integerChar(b)
		case unicode.IsDigit(r):
			_, _ = b.WriteRune(r)
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

func (l *Lexer) integerOctal(b *strings.Builder) (lexState, error) {
	return func(r rune) (lexState, error) {
		r = l.conv(r)
		switch {
		case isOctal(r):
			_, _ = b.WriteRune(r)
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
		switch {
		case isHex(r):
			_, _ = b.WriteRune(r)
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
			_, _ = b.WriteRune(r)
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
			_, _ = b.WriteRune(r)
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
			_, _ = b.WriteRune(r)
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
			_, _ = b.WriteRune(r)
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
	case r == '0':
		var b strings.Builder
		_, _ = b.WriteRune(r)
		return l.integerZero(&b)
	case unicode.IsNumber(r):
		var b strings.Builder
		_, _ = b.WriteRune(r)
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
		case unicode.IsNumber(r):
			l.backup()
			l.emit(Token{Kind: TokenSign, Val: b.String()})
			return nil, nil
		case isGraphic(r):
			l.backup()
			return l.graphic(b)
		default:
			l.backup()
			l.emit(Token{Kind: TokenIdent, Val: b.String()})
			return nil, nil
		}
	}, nil
}

func (l *Lexer) variable(r rune) (lexState, error) {
	r = l.conv(r)
	switch {
	case unicode.IsUpper(r), r == '_':
		var b strings.Builder
		_, _ = b.WriteRune(r)
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
			_, _ = b.WriteRune(r)
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
			_, _ = b.WriteRune(r)
			return l.graphic(b)
		default:
			l.backup()
			l.emit(Token{Kind: TokenGraphic, Val: b.String()})
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
			_, _ = b.WriteRune(r)
			return l.graphic(b)
		default:
			l.backup()
			l.emit(Token{Kind: TokenIdent, Val: "/"})
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

func (l *Lexer) doubleQuoted(b *strings.Builder) (lexState, error) {
	return func(r rune) (lexState, error) {
		r = l.conv(r)
		switch r {
		case etx:
			return nil, ErrInsufficient
		case '"':
			_, _ = b.WriteRune(r)
			return l.doubleQuotedDoubleQuote(b)
		case '\\':
			_, _ = b.WriteRune(r)
			return l.doubleQuotedSlash(b)
		default:
			_, _ = b.WriteRune(r)
			return l.doubleQuoted(b)
		}
	}, nil
}

func (l *Lexer) doubleQuotedDoubleQuote(b *strings.Builder) (lexState, error) {
	return func(r rune) (lexState, error) {
		switch r {
		case '"':
			_, _ = b.WriteRune(r)
			return l.doubleQuoted(b)
		default:
			l.backup()
			l.emit(Token{Kind: TokenDoubleQuoted, Val: b.String()})
			return nil, nil
		}
	}, nil
}

func (l *Lexer) doubleQuotedSlash(b *strings.Builder) (lexState, error) {
	return func(r rune) (lexState, error) {
		switch {
		case r == etx:
			return nil, ErrInsufficient
		case r == 'x':
			_, _ = b.WriteRune(r)
			return l.doubleQuotedSlashHex(b)
		case unicode.IsNumber(r):
			_, _ = b.WriteRune(r)
			return l.doubleQuotedSlashOctal(b)
		default:
			_, _ = b.WriteRune(r)
			return l.doubleQuoted(b)
		}
	}, nil
}

func (l *Lexer) doubleQuotedSlashHex(b *strings.Builder) (lexState, error) {
	return func(r rune) (lexState, error) {
		switch {
		case r == etx:
			return nil, ErrInsufficient
		case isHex(r):
			_, _ = b.WriteRune(r)
			return l.doubleQuotedSlashHex(b)
		case r == '\\':
			_, _ = b.WriteRune(r)
			return l.doubleQuoted(b)
		default:
			return nil, UnexpectedRuneError{rune: r}
		}
	}, nil
}

func (l *Lexer) doubleQuotedSlashOctal(b *strings.Builder) (lexState, error) {
	return func(r rune) (lexState, error) {
		switch {
		case r == etx:
			return nil, ErrInsufficient
		case isOctal(r):
			_, _ = b.WriteRune(r)
			return l.doubleQuotedSlashOctal(b)
		case r == '\\':
			_, _ = b.WriteRune(r)
			return l.doubleQuoted(b)
		default:
			return nil, UnexpectedRuneError{rune: r}
		}
	}, nil
}

func isOctal(r rune) bool {
	return strings.ContainsRune("01234567", r)
}

func isHex(r rune) bool {
	return strings.ContainsRune("0123456789ABCDEF", unicode.ToUpper(r))
}

func isGraphic(r rune) bool {
	return strings.ContainsRune("#$&*+-./:<=>?@^~\\", r)
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

var spacing = [tokenKindLen][tokenKindLen]bool{
	TokenVariable: {
		TokenVariable: true,
		TokenInteger:  true,
		TokenFloat:    true,
		TokenIdent:    true,
	},
	TokenIdent: {
		TokenVariable: true,
		TokenInteger:  true,
		TokenFloat:    true,
		TokenIdent:    true,
	},
	TokenGraphic: {
		TokenGraphic: true,
		TokenSign:    true,
	},
	TokenComma: {
		TokenVariable:    true,
		TokenFloat:       true,
		TokenInteger:     true,
		TokenIdent:       true,
		TokenQuotedIdent: true,
		TokenGraphic:     true,
		TokenComma:       true,
		TokenPeriod:      true,
		TokenBar:         true,
		TokenParenL:      true,
		TokenParenR:      true,
		TokenBracketL:    true,
		TokenBracketR:    true,
		TokenBraceL:      true,
		TokenBraceR:      true,
		TokenSign:        true,
	},
	TokenSign: {
		TokenGraphic: true,
	},
}
