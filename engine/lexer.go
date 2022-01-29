package engine

import (
	"bufio"
	"errors"
	"fmt"
	"strings"
	"unicode"
	"unicode/utf8"
)

var initDispatch [256]func(*Lexer, *strings.Builder) (Token, error)

func init() {
	initDispatch = [256]func(*Lexer, *strings.Builder) (Token, error){
		'!':  initDispatchSingle(TokenIdent),
		'"':  (*Lexer).doubleQuoted,
		'#':  (*Lexer).graphic,
		'$':  (*Lexer).graphic,
		'%':  (*Lexer).singleLineComment,
		'&':  (*Lexer).graphic,
		'\'': (*Lexer).quotedIdent,
		'(':  initDispatchSingle(TokenParenL),
		')':  initDispatchSingle(TokenParenR),
		'*':  (*Lexer).graphic,
		'+':  (*Lexer).sign,
		',':  initDispatchSingle(TokenComma),
		'-':  (*Lexer).sign,
		'.':  (*Lexer).period,
		'/':  (*Lexer).multiLineCommentBegin,
		'0':  (*Lexer).integerZero,
		'1':  (*Lexer).integerDecimal,
		'2':  (*Lexer).integerDecimal,
		'3':  (*Lexer).integerDecimal,
		'4':  (*Lexer).integerDecimal,
		'5':  (*Lexer).integerDecimal,
		'6':  (*Lexer).integerDecimal,
		'7':  (*Lexer).integerDecimal,
		'8':  (*Lexer).integerDecimal,
		'9':  (*Lexer).integerDecimal,
		':':  (*Lexer).graphic,
		';':  initDispatchSingle(TokenIdent),
		'<':  (*Lexer).graphic,
		'=':  (*Lexer).graphic,
		'>':  (*Lexer).graphic,
		'?':  (*Lexer).graphic,
		'@':  (*Lexer).graphic,
		'A':  (*Lexer).variable,
		'B':  (*Lexer).variable,
		'C':  (*Lexer).variable,
		'D':  (*Lexer).variable,
		'E':  (*Lexer).variable,
		'F':  (*Lexer).variable,
		'G':  (*Lexer).variable,
		'H':  (*Lexer).variable,
		'I':  (*Lexer).variable,
		'J':  (*Lexer).variable,
		'K':  (*Lexer).variable,
		'L':  (*Lexer).variable,
		'M':  (*Lexer).variable,
		'N':  (*Lexer).variable,
		'O':  (*Lexer).variable,
		'P':  (*Lexer).variable,
		'Q':  (*Lexer).variable,
		'R':  (*Lexer).variable,
		'S':  (*Lexer).variable,
		'T':  (*Lexer).variable,
		'U':  (*Lexer).variable,
		'V':  (*Lexer).variable,
		'W':  (*Lexer).variable,
		'X':  (*Lexer).variable,
		'Y':  (*Lexer).variable,
		'Z':  (*Lexer).variable,
		'[':  (*Lexer).squareBracket,
		'\\': (*Lexer).graphic,
		']':  initDispatchSingle(TokenBracketR),
		'^':  (*Lexer).graphic,
		'_':  (*Lexer).variable,
		'a':  (*Lexer).normalAtom,
		'b':  (*Lexer).normalAtom,
		'c':  (*Lexer).normalAtom,
		'd':  (*Lexer).normalAtom,
		'e':  (*Lexer).normalAtom,
		'f':  (*Lexer).normalAtom,
		'g':  (*Lexer).normalAtom,
		'h':  (*Lexer).normalAtom,
		'i':  (*Lexer).normalAtom,
		'j':  (*Lexer).normalAtom,
		'k':  (*Lexer).normalAtom,
		'l':  (*Lexer).normalAtom,
		'm':  (*Lexer).normalAtom,
		'n':  (*Lexer).normalAtom,
		'o':  (*Lexer).normalAtom,
		'p':  (*Lexer).normalAtom,
		'q':  (*Lexer).normalAtom,
		'r':  (*Lexer).normalAtom,
		's':  (*Lexer).normalAtom,
		't':  (*Lexer).normalAtom,
		'u':  (*Lexer).normalAtom,
		'v':  (*Lexer).normalAtom,
		'w':  (*Lexer).normalAtom,
		'x':  (*Lexer).normalAtom,
		'y':  (*Lexer).normalAtom,
		'z':  (*Lexer).normalAtom,
		'{':  (*Lexer).curlyBracket,
		'|':  initDispatchSingle(TokenBar),
		'}':  initDispatchSingle(TokenBraceR),
		'~':  (*Lexer).graphic,
	}
}

// Lexer turns bytes into tokens.
type Lexer struct {
	input           *bufio.Reader
	charConversions map[rune]rune
	pos             int
	width           int
	reserved        Token
}

// NewLexer create a lexer with an input and char conversions.
func NewLexer(input *bufio.Reader, charConversions map[rune]rune) *Lexer {
	l := Lexer{input: input, charConversions: charConversions}
	return &l
}

// Token returns the next token.
func (l *Lexer) Token() (Token, error) {
	if l.reserved != (Token{}) {
		t := l.reserved
		l.reserved = Token{}
		return t, nil
	}

	r := l.next()
	for unicode.IsSpace(r) {
		r = l.next()
	}

	if r == utf8.RuneError {
		return Token{Kind: TokenEOF}, nil
	}

	var b strings.Builder
	_, _ = b.WriteRune(r)
	if int(r) < len(initDispatch) { // A rune can be bigger than the size of the array.
		f := initDispatch[r]
		if f != nil {
			return f(l, &b)
		}
	}
	l.backup()
	return Token{}, UnexpectedRuneError{rune: r}
}

func initDispatchSingle(k TokenKind) func(*Lexer, *strings.Builder) (Token, error) {
	return func(_ *Lexer, b *strings.Builder) (Token, error) {
		return Token{Kind: k, Val: b.String()}, nil
	}
}

func (l *Lexer) next() rune {
	r := l.rawNext()
	return l.conv(r)
}

func (l *Lexer) rawNext() rune {
	r, w, err := l.input.ReadRune()
	if err != nil {
		r = utf8.RuneError
		w = 1
	}
	l.width = w
	l.pos += l.width
	return r
}

func (l *Lexer) backup() {
	_ = l.input.UnreadRune()
	l.pos -= l.width
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
	// TokenEOF represents an end of token stream.
	TokenEOF TokenKind = iota

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
		TokenEOF:          "eos",
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

func (l *Lexer) period(b *strings.Builder) (Token, error) {
	r := l.next()
	switch {
	case r == utf8.RuneError, unicode.IsSpace(r):
		return Token{Kind: TokenPeriod, Val: b.String()}, nil
	default:
		_, _ = b.WriteRune(r)
		return l.graphic(b)
	}
}

func (l *Lexer) normalAtom(b *strings.Builder) (Token, error) {
	for {
		r := l.next()
		switch {
		case unicode.IsLetter(r), unicode.IsNumber(r), r == '_':
			_, _ = b.WriteRune(r)
		default:
			l.backup()
			return Token{Kind: TokenIdent, Val: b.String()}, nil
		}
	}
}

func (l *Lexer) quotedIdent(b *strings.Builder) (Token, error) {
	for {
		switch r := l.rawNext(); r {
		case utf8.RuneError:
			return Token{}, ErrInsufficient
		case '\'':
			_, _ = b.WriteRune(r)
			if l.quotedIdentEnd(b) {
				return Token{Kind: TokenQuotedIdent, Val: b.String()}, nil
			}
		case '\\':
			_, _ = b.WriteRune(r)
			if err := l.quotedIdentSlash(b); err != nil {
				return Token{}, err
			}
		default:
			_, _ = b.WriteRune(r)
		}
	}
}

func (l *Lexer) quotedIdentEnd(b *strings.Builder) bool {
	switch r := l.next(); r {
	case '\'':
		_, _ = b.WriteRune(r)
		return false
	default:
		l.backup()
		return true
	}
}

func (l *Lexer) quotedIdentSlash(b *strings.Builder) error {
	r := l.next()
	switch {
	case r == utf8.RuneError:
		return ErrInsufficient
	case r == 'x':
		_, _ = b.WriteRune(r)
		return l.quotedIdentSlashHex(b)
	case isOctal(r):
		_, _ = b.WriteRune(r)
		return l.quotedIdentSlashOctal(b)
	default:
		_, _ = b.WriteRune(r)
		return nil
	}
}

func (l *Lexer) quotedIdentSlashHex(b *strings.Builder) error {
	for {
		r := l.next()
		switch {
		case r == utf8.RuneError:
			return ErrInsufficient
		case isHex(r):
			_, _ = b.WriteRune(r)
		case r == '\\':
			_, _ = b.WriteRune(r)
			return nil
		default:
			return UnexpectedRuneError{rune: r}
		}
	}
}

func (l *Lexer) quotedIdentSlashOctal(b *strings.Builder) error {
	for {
		r := l.next()
		switch {
		case r == utf8.RuneError:
			return ErrInsufficient
		case isOctal(r):
			_, _ = b.WriteRune(r)
		case r == '\\':
			_, _ = b.WriteRune(r)
			return nil
		default:
			return UnexpectedRuneError{rune: r}
		}
	}
}

func (l *Lexer) squareBracket(b *strings.Builder) (Token, error) {
	r := l.next()
	switch {
	case r == ']':
		_, _ = b.WriteRune(r)
		return Token{Kind: TokenIdent, Val: b.String()}, nil
	default:
		l.backup()
		return Token{Kind: TokenBracketL, Val: b.String()}, nil
	}
}

func (l *Lexer) curlyBracket(b *strings.Builder) (Token, error) {
	r := l.next()
	switch {
	case r == '}':
		_, _ = b.WriteRune(r)
		return Token{Kind: TokenIdent, Val: b.String()}, nil
	default:
		l.backup()
		return Token{Kind: TokenBraceL, Val: "{"}, nil
	}
}

func (l *Lexer) floatMantissa(b *strings.Builder) (Token, error) {
	for {
		r := l.next()
		switch {
		case unicode.IsNumber(r):
			_, _ = b.WriteRune(r)
		case r == 'E' || r == 'e':
			_, _ = b.WriteRune(r)
			return l.floatE(b)
		default:
			l.backup()
			return Token{Kind: TokenFloat, Val: b.String()}, nil
		}
	}
}

func (l *Lexer) floatE(b *strings.Builder) (Token, error) {
	r := l.next()
	switch {
	case r == utf8.RuneError:
		return Token{}, ErrInsufficient
	case unicode.IsNumber(r), r == '+', r == '-':
		_, _ = b.WriteRune(r)
		return l.floatExponent(b)
	default:
		return Token{}, UnexpectedRuneError{rune: r}
	}
}

func (l *Lexer) floatExponent(b *strings.Builder) (Token, error) {
	for {
		r := l.next()
		switch {
		case unicode.IsNumber(r):
			_, _ = b.WriteRune(r)
		default:
			l.backup()
			return Token{Kind: TokenFloat, Val: b.String()}, nil
		}
	}
}

func (l *Lexer) integerZero(b *strings.Builder) (Token, error) {
	r := l.next()
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
		return l.integerPeriod(b)
	default:
		l.backup()
		return Token{Kind: TokenInteger, Val: b.String()}, nil
	}
}

func (l *Lexer) integerOctal(b *strings.Builder) (Token, error) {
	for {
		r := l.next()
		switch {
		case isOctal(r):
			_, _ = b.WriteRune(r)
		default:
			l.backup()
			return Token{Kind: TokenInteger, Val: b.String()}, nil
		}
	}
}

func (l *Lexer) integerHex(b *strings.Builder) (Token, error) {
	for {
		r := l.next()
		switch {
		case isHex(r):
			_, _ = b.WriteRune(r)
		default:
			l.backup()
			return Token{Kind: TokenInteger, Val: b.String()}, nil
		}
	}
}

func (l *Lexer) integerBinary(b *strings.Builder) (Token, error) {
	for {
		r := l.next()
		switch r {
		case '0', '1':
			_, _ = b.WriteRune(r)
		default:
			l.backup()
			return Token{Kind: TokenInteger, Val: b.String()}, nil
		}
	}
}

func (l *Lexer) integerChar(b *strings.Builder) (Token, error) {
	for {
		r := l.next()
		switch r {
		case utf8.RuneError:
			return Token{}, ErrInsufficient
		case '\\':
			return l.integerCharEscape(b)
		default:
			_, _ = b.WriteRune(r)
			return Token{Kind: TokenInteger, Val: b.String()}, nil
		}
	}
}

var integerCharEscapeRunes = [...]rune{
	'a':  '\a',
	'b':  '\b',
	'f':  '\f',
	'n':  '\n',
	'r':  '\r',
	't':  '\t',
	'v':  '\v',
	'\\': '\\',
	'\'': '\'',
}

func (l *Lexer) integerCharEscape(b *strings.Builder) (Token, error) {
	r := l.next()

	if r == utf8.RuneError {
		return Token{}, ErrInsufficient
	}

	if int(r) > len(integerCharEscapeRunes) || integerCharEscapeRunes[r] == 0 {
		return Token{}, UnexpectedRuneError{rune: r}
	}
	_, _ = b.WriteRune(integerCharEscapeRunes[r])

	return Token{Kind: TokenInteger, Val: b.String()}, nil
}

func (l *Lexer) integerDecimal(b *strings.Builder) (Token, error) {
	for {
		r := l.next()
		switch {
		case unicode.IsNumber(r):
			_, _ = b.WriteRune(r)
		case r == '.':
			return l.integerPeriod(b)
		default:
			l.backup()
			return Token{Kind: TokenInteger, Val: b.String()}, nil
		}
	}
}

func (l *Lexer) integerPeriod(b *strings.Builder) (Token, error) {
	r := l.next()
	switch {
	case unicode.IsDigit(r):
		_, _ = b.WriteRune('.')
		_, _ = b.WriteRune(r)
		return l.floatMantissa(b)
	default:
		l.backup()
		l.reserved = Token{Kind: TokenPeriod, Val: "."}
		return Token{Kind: TokenInteger, Val: b.String()}, nil
	}
}

func (l *Lexer) sign(b *strings.Builder) (Token, error) {
	r := l.next()
	switch {
	case unicode.IsNumber(r):
		l.backup()
		return Token{Kind: TokenSign, Val: b.String()}, nil
	case isGraphic(r):
		_, _ = b.WriteRune(r)
		return l.graphic(b)
	default:
		l.backup()
		return Token{Kind: TokenIdent, Val: b.String()}, nil
	}
}

func (l *Lexer) variable(b *strings.Builder) (Token, error) {
	for {
		r := l.next()
		switch {
		case unicode.IsLetter(r), unicode.IsNumber(r), r == '_':
			_, _ = b.WriteRune(r)
		default:
			l.backup()
			return Token{Kind: TokenVariable, Val: b.String()}, nil
		}
	}
}

func (l *Lexer) graphic(b *strings.Builder) (Token, error) {
	for {
		r := l.next()
		switch {
		case isGraphic(r):
			_, _ = b.WriteRune(r)
		default:
			l.backup()
			return Token{Kind: TokenGraphic, Val: b.String()}, nil
		}
	}
}

func (l *Lexer) singleLineComment(*strings.Builder) (Token, error) {
	for {
		r := l.next()
		switch r {
		case '\n', utf8.RuneError:
			return l.Token()
		}
	}
}

func (l *Lexer) multiLineCommentBegin(b *strings.Builder) (Token, error) {
	r := l.next()
	switch {
	case r == '*':
		return l.multiLineCommentBody()
	case isGraphic(r):
		_, _ = b.WriteRune(r)
		return l.graphic(b)
	default:
		l.backup()
		return Token{Kind: TokenGraphic, Val: b.String()}, nil
	}
}

func (l *Lexer) multiLineCommentBody() (Token, error) {
	for {
		r := l.next()
		switch r {
		case utf8.RuneError:
			return Token{}, ErrInsufficient
		case '*':
			if l.multiLineCommentEnd() {
				return l.Token()
			}
		}
	}
}

func (l *Lexer) multiLineCommentEnd() bool {
	r := l.next()
	return r == '/'
}

func (l *Lexer) doubleQuoted(b *strings.Builder) (Token, error) {
	for {
		switch r := l.rawNext(); r {
		case utf8.RuneError:
			return Token{}, ErrInsufficient
		case '"':
			_, _ = b.WriteRune(r)
			if l.doubleQuotedEnd(b) {
				return Token{Kind: TokenDoubleQuoted, Val: b.String()}, nil
			}
		case '\\':
			_, _ = b.WriteRune(r)
			if err := l.doubleQuotedSlash(b); err != nil {
				return Token{}, err
			}
		default:
			_, _ = b.WriteRune(r)
		}
	}
}

func (l *Lexer) doubleQuotedEnd(b *strings.Builder) bool {
	switch r := l.rawNext(); r {
	case '"':
		_, _ = b.WriteRune(r)
		return false
	default:
		l.backup()
		return true
	}
}

func (l *Lexer) doubleQuotedSlash(b *strings.Builder) error {
	r := l.next()
	switch {
	case r == utf8.RuneError:
		return ErrInsufficient
	case r == 'x':
		_, _ = b.WriteRune(r)
		return l.doubleQuotedSlashHex(b)
	case unicode.IsNumber(r):
		_, _ = b.WriteRune(r)
		return l.doubleQuotedSlashOctal(b)
	default:
		_, _ = b.WriteRune(r)
		return nil
	}
}

func (l *Lexer) doubleQuotedSlashHex(b *strings.Builder) error {
	for {
		r := l.next()
		switch {
		case r == utf8.RuneError:
			return ErrInsufficient
		case isHex(r):
			_, _ = b.WriteRune(r)
		case r == '\\':
			_, _ = b.WriteRune(r)
			return nil
		default:
			return UnexpectedRuneError{rune: r}
		}
	}
}

func (l *Lexer) doubleQuotedSlashOctal(b *strings.Builder) error {
	for {
		r := l.next()
		switch {
		case r == utf8.RuneError:
			return ErrInsufficient
		case isOctal(r):
			_, _ = b.WriteRune(r)
		case r == '\\':
			_, _ = b.WriteRune(r)
			return nil
		default:
			return UnexpectedRuneError{rune: r}
		}
	}
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
	return fmt.Sprintf("unexpected rune: %s(0x%x)", string(e.rune), e.rune)
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
