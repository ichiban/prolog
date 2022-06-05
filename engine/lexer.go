package engine

import (
	"bufio"
	"fmt"
	"strings"
	"unicode"
	"unicode/utf8"
)

// Lexer turns bytes into tokens.
type Lexer struct {
	input           *bufio.Reader
	charConversions map[rune]rune
	reserved        Token
}

// NewLexer create a lexer with an input and char conversions.
func NewLexer(input *bufio.Reader, charConversions map[rune]rune) *Lexer {
	l := Lexer{input: input, charConversions: charConversions}
	return &l
}

// Token returns the next token.
func (l *Lexer) Token() Token {
	if l.reserved != (Token{}) {
		t := l.reserved
		l.reserved = Token{}
		return t
	}

	return l.layoutTextSequence()
}

func (l *Lexer) next() rune {
	r := l.rawNext()
	return l.conv(r)
}

func (l *Lexer) rawNext() rune {
	r, _, err := l.input.ReadRune()
	if err != nil {
		r = utf8.RuneError
	}
	return r
}

func (l *Lexer) conv(r rune) rune {
	if r, ok := l.charConversions[r]; ok {
		return r
	}
	return r
}

func (l *Lexer) backup() {
	_ = l.input.UnreadRune()
}

// Token is a smallest meaningful unit of prolog program.
type Token struct {
	Kind TokenKind
	Val  string
}

func (t *Token) String() string {
	return fmt.Sprintf("<%s %s>", t.Kind, t.Val)
}

// TokenKind is a type of Token.
type TokenKind byte

const (
	// TokenInvalid represents an invalid token.
	TokenInvalid TokenKind = iota

	// TokenInsufficient represents an insufficient token.
	TokenInsufficient

	// TokenEOF represents an end of token stream.
	TokenEOF

	// TokenLetterDigit represents a letter digit token.
	TokenLetterDigit

	// TokenGraphic represents a graphical token.
	TokenGraphic

	// TokenQuoted represents a quoted token.
	TokenQuoted

	// TokenSemicolon represents a semicolon token.
	TokenSemicolon

	// TokenCut represents a cut token.
	TokenCut

	// TokenVariable represents a variable token.
	TokenVariable

	// TokenInteger represents an integer token.
	TokenInteger

	// TokenFloatNumber represents a floating-point token.
	TokenFloatNumber

	// TokenDoubleQuotedList represents a double-quoted string.
	TokenDoubleQuotedList

	// TokenOpen represents an open parenthesis.
	TokenOpen

	// TokenOpenCT represents an open CT parenthesis.
	TokenOpenCT

	// TokenClose represents a close parenthesis.
	TokenClose

	// TokenOpenList represents an open bracket.
	TokenOpenList

	// TokenCloseList represents a close bracket.
	TokenCloseList

	// TokenOpenCurly represents an open brace.
	TokenOpenCurly

	// TokenCloseCurly represents a close brace.
	TokenCloseCurly

	// TokenHTSep represents a bar.
	TokenHTSep

	// TokenComma represents a comma.
	TokenComma

	// TokenEnd represents a period.
	TokenEnd

	tokenKindLen
)

func (k TokenKind) GoString() string {
	return k.String()
}

func (k TokenKind) String() string {
	return [...]string{
		TokenInvalid:          "invalid",
		TokenInsufficient:     "insufficient",
		TokenEOF:              "eos",
		TokenLetterDigit:      "letter digit",
		TokenGraphic:          "graphic",
		TokenQuoted:           "quoted",
		TokenSemicolon:        "semicolon",
		TokenCut:              "cut",
		TokenVariable:         "variable",
		TokenInteger:          "integer",
		TokenFloatNumber:      "float number",
		TokenDoubleQuotedList: "double quoted list",
		TokenOpen:             "open",
		TokenOpenCT:           "open ct",
		TokenClose:            "close",
		TokenOpenList:         "open list",
		TokenCloseList:        "close list",
		TokenOpenCurly:        "open curly",
		TokenCloseCurly:       "close curly",
		TokenHTSep:            "ht sep",
		TokenComma:            "comma",
		TokenEnd:              "end",
	}[k]
}

var spacing = [tokenKindLen][tokenKindLen]bool{
	TokenVariable: {
		TokenVariable:    true,
		TokenInteger:     true,
		TokenFloatNumber: true,
		TokenLetterDigit: true,
	},
	TokenLetterDigit: {
		TokenVariable:    true,
		TokenInteger:     true,
		TokenFloatNumber: true,
		TokenLetterDigit: true,
	},
	TokenGraphic: {
		TokenGraphic: true,
	},
	TokenComma: {
		TokenVariable:    true,
		TokenFloatNumber: true,
		TokenInteger:     true,
		TokenLetterDigit: true,
		TokenQuoted:      true,
		TokenGraphic:     true,
		TokenComma:       true,
		TokenEnd:         true,
		TokenHTSep:       true,
		TokenOpen:        true,
		TokenClose:       true,
		TokenOpenList:    true,
		TokenCloseList:   true,
		TokenOpenCurly:   true,
		TokenCloseCurly:  true,
	},
}

// Tokens

func (l *Lexer) token(afterLayout bool) Token {
	switch r := l.next(); {
	case r == utf8.RuneError:
		return Token{Kind: TokenEOF}
	case isSmallLetterChar(r):
		var b strings.Builder
		_, _ = b.WriteRune(r)
		return l.letterDigitToken(&b)
	case r == '.':
		var b strings.Builder
		_, _ = b.WriteRune(r)
		switch r := l.next(); {
		case isLayoutChar(r), r == '%', r == utf8.RuneError:
			l.backup()
			return Token{Kind: TokenEnd, Val: b.String()}
		default:
			l.backup()
			return l.graphicToken(&b)
		}
	case isGraphicChar(r), r == '\\':
		var b strings.Builder
		_, _ = b.WriteRune(r)
		return l.graphicToken(&b)
	case r == '\'':
		var b strings.Builder
		_, _ = b.WriteRune(r)
		return l.quotedToken(&b)
	case r == ';':
		return Token{Kind: TokenSemicolon, Val: string(r)}
	case r == '!':
		return Token{Kind: TokenCut, Val: string(r)}
	case r == '_', isCapitalLetterChar(r):
		var b strings.Builder
		_, _ = b.WriteRune(r)
		return l.variableToken(&b)
	case isDecimalDigitChar(r):
		l.backup()
		return l.integerToken()
	case r == '"':
		var b strings.Builder
		_, _ = b.WriteRune(r)
		return l.doubleQuotedListToken(&b)
	case r == '(':
		if afterLayout {
			return Token{Kind: TokenOpen, Val: string(r)}
		}
		return Token{Kind: TokenOpenCT, Val: string(r)}
	case r == ')':
		return Token{Kind: TokenClose, Val: string(r)}
	case r == '[':
		return Token{Kind: TokenOpenList, Val: string(r)}
	case r == ']':
		return Token{Kind: TokenCloseList, Val: string(r)}
	case r == '{':
		return Token{Kind: TokenOpenCurly, Val: string(r)}
	case r == '}':
		return Token{Kind: TokenCloseCurly, Val: string(r)}
	case r == '|':
		return Token{Kind: TokenHTSep, Val: string(r)}
	case r == ',':
		return Token{Kind: TokenComma, Val: string(r)}
	default:
		return Token{Kind: TokenInvalid, Val: string(r)}
	}
}

//// Layout text

func (l *Lexer) layoutTextSequence() Token {
	var afterLayout bool
	for {
		switch r := l.next(); {
		case isLayoutChar(r):
			afterLayout = true
			continue
		case r == '%':
			return l.commentText(false)
		case r == '/':
			return l.commentOpen()
		default:
			l.backup()
			return l.token(afterLayout)
		}
	}
}

func (l *Lexer) commentText(bracketed bool) Token {
	if bracketed {
		for {
			switch r := l.next(); {
			case r == utf8.RuneError:
				return Token{Kind: TokenInsufficient}
			case r == '*':
				return l.commentClose()
			}
		}
	} else {
		for {
			switch r := l.next(); {
			case r == '\n', r == utf8.RuneError:
				return l.layoutTextSequence()
			}
		}
	}
}

func (l *Lexer) commentOpen() Token {
	switch r := l.next(); {
	case r == '*':
		return l.commentText(true)
	default:
		l.backup()
		var b strings.Builder
		_, _ = b.WriteRune('/')
		return l.graphicToken(&b)
	}
}

func (l *Lexer) commentClose() Token {
	switch r := l.next(); {
	case r == '/':
		return l.layoutTextSequence()
	default:
		return l.commentText(true)
	}
}

//// Names

func (l *Lexer) letterDigitToken(b *strings.Builder) Token {
	for {
		switch r := l.next(); {
		case isAlphanumericChar(r):
			_, _ = b.WriteRune(r)
		default:
			l.backup()
			return Token{Kind: TokenLetterDigit, Val: b.String()}
		}
	}
}

func (l *Lexer) graphicToken(b *strings.Builder) Token {
	for {
		switch r := l.next(); {
		case isGraphicChar(r), r == '\\':
			_, _ = b.WriteRune(r)
		default:
			l.backup()
			return Token{Kind: TokenGraphic, Val: b.String()}
		}
	}
}

func (l *Lexer) quotedToken(b *strings.Builder) Token {
	for {
		switch r := l.rawNext(); {
		case r == utf8.RuneError:
			return Token{Kind: TokenInsufficient, Val: b.String()}
		case r == '\'':
			_, _ = b.WriteRune(r)
			switch r := l.rawNext(); {
			case r == '\'':
				_, _ = b.WriteRune(r)
			default:
				l.backup()
				return Token{Kind: TokenQuoted, Val: b.String()}
			}
		case isGraphicChar(r), isAlphanumericChar(r), isSoloChar(r), unicode.IsSpace(r), r == '"', r == '`':
			_, _ = b.WriteRune(r)
		case r == '\\':
			_, _ = b.WriteRune(r)
			switch r := l.rawNext(); {
			case r == '\n':
				_, _ = b.WriteRune(r)
			default:
				l.backup()
				return l.escapeSequence(b, l.quotedToken)
			}
		default:
			_, _ = b.WriteRune(r)
			return Token{Kind: TokenInvalid, Val: b.String()}
		}
	}
}

func (l *Lexer) escapeSequence(b *strings.Builder, cont func(*strings.Builder) Token) Token {
	switch r := l.rawNext(); {
	case r == utf8.RuneError:
		return Token{Kind: TokenInsufficient, Val: b.String()}
	case isMetaChar(r), isSymbolicControlChar(r):
		_, _ = b.WriteRune(r)
		return cont(b)
	case isOctalDigitChar(r):
		_, _ = b.WriteRune(r)
		return l.octalEscapeSequence(b, cont)
	case r == 'x':
		_, _ = b.WriteRune(r)
		return l.hexadecimalEscapeSequence(b, cont)
	default:
		_, _ = b.WriteRune(r)
		return Token{Kind: TokenInvalid, Val: b.String()}
	}
}

func (l *Lexer) octalEscapeSequence(b *strings.Builder, cont func(*strings.Builder) Token) Token {
	for {
		switch r := l.rawNext(); {
		case r == utf8.RuneError:
			return Token{Kind: TokenInsufficient, Val: b.String()}
		case r == '\\':
			_, _ = b.WriteRune(r)
			return cont(b)
		case isOctalDigitChar(r):
			_, _ = b.WriteRune(r)
			continue
		default:
			_, _ = b.WriteRune(r)
			return Token{Kind: TokenInvalid, Val: b.String()}
		}
	}
}

func (l *Lexer) hexadecimalEscapeSequence(b *strings.Builder, cont func(*strings.Builder) Token) Token {
	switch r := l.rawNext(); {
	case r == utf8.RuneError:
		return Token{Kind: TokenInsufficient, Val: b.String()}
	case isHexadecimalDigitChar(r):
		_, _ = b.WriteRune(r)
	default:
		_, _ = b.WriteRune(r)
		return Token{Kind: TokenInvalid, Val: b.String()}
	}

	for {
		switch r := l.next(); {
		case r == utf8.RuneError:
			return Token{Kind: TokenInsufficient, Val: b.String()}
		case r == '\\':
			_, _ = b.WriteRune(r)
			return cont(b)
		case isHexadecimalDigitChar(r):
			_, _ = b.WriteRune(r)
			continue
		default:
			_, _ = b.WriteRune(r)
			return Token{Kind: TokenInvalid, Val: b.String()}
		}
	}
}

//// Variables

func (l *Lexer) variableToken(b *strings.Builder) Token {
	for {
		switch r := l.next(); {
		case isAlphanumericChar(r):
			_, _ = b.WriteRune(r)
		default:
			l.backup()
			return Token{Kind: TokenVariable, Val: b.String()}
		}
	}
}

//// Integer numbers

func (l *Lexer) integerToken() Token {
	switch r := l.next(); {
	case r == '0':
		var b strings.Builder
		_, _ = b.WriteRune(r)
		switch r = l.next(); {
		case r == '\'':
			_, _ = b.WriteRune(r)
			return l.characterCodeConstant(&b)
		case r == 'b':
			_, _ = b.WriteRune(r)
			return l.binaryConstant(&b)
		case r == 'o':
			_, _ = b.WriteRune(r)
			return l.octalConstant(&b)
		case r == 'x':
			_, _ = b.WriteRune(r)
			return l.hexadecimalConstant(&b)
		default:
			l.backup()
			return l.integerConstant(&b)
		}
	default:
		var b strings.Builder
		_, _ = b.WriteRune(r)
		return l.integerConstant(&b)
	}
}

func (l *Lexer) integerConstant(b *strings.Builder) Token {
	for {
		switch r := l.next(); {
		case isDecimalDigitChar(r):
			_, _ = b.WriteRune(r)
		case r == '.':
			switch r := l.next(); {
			case isDecimalDigitChar(r):
				_, _ = b.WriteRune('.')
				_, _ = b.WriteRune(r)
				return l.fraction(b)
			default:
				l.backup()
				l.reserved = Token{Kind: TokenEnd, Val: "."}
				return Token{Kind: TokenInteger, Val: b.String()}
			}
		default:
			l.backup()
			return Token{Kind: TokenInteger, Val: b.String()}
		}
	}
}

func (l *Lexer) characterCodeConstant(b *strings.Builder) Token {
	switch r := l.next(); {
	case r == utf8.RuneError:
		return Token{Kind: TokenInsufficient, Val: b.String()}
	case r == '\'':
		_, _ = b.WriteRune(r)
		switch r := l.next(); {
		case r == utf8.RuneError:
			return Token{Kind: TokenInsufficient, Val: b.String()}
		case r == '\'':
			_, _ = b.WriteRune(r)
			return Token{Kind: TokenInteger, Val: b.String()}
		default:
			_, _ = b.WriteRune(r)
			return Token{Kind: TokenInvalid, Val: b.String()}
		}
	case isGraphicChar(r), isAlphanumericChar(r), isSoloChar(r), unicode.IsSpace(r), r == '"', r == '`':
		_, _ = b.WriteRune(r)
		return Token{Kind: TokenInteger, Val: b.String()}
	case r == '\\':
		_, _ = b.WriteRune(r)
		return l.escapeSequence(b, func(b *strings.Builder) Token {
			return Token{Kind: TokenInteger, Val: b.String()}
		})
	default:
		_, _ = b.WriteRune(r)
		return Token{Kind: TokenInvalid, Val: b.String()}
	}
}

func (l *Lexer) binaryConstant(b *strings.Builder) Token {
	r := l.next()
	_, _ = b.WriteRune(r)
	if !isBinaryDigitChar(r) {
		return Token{Kind: TokenInvalid, Val: b.String()}
	}
	for {
		switch r := l.next(); {
		case isBinaryDigitChar(r):
			_, _ = b.WriteRune(r)
		default:
			l.backup()
			return Token{Kind: TokenInteger, Val: b.String()}
		}
	}
}

func (l *Lexer) octalConstant(b *strings.Builder) Token {
	r := l.next()
	_, _ = b.WriteRune(r)
	if !isOctalDigitChar(r) {
		return Token{Kind: TokenInvalid, Val: b.String()}
	}
	for {
		switch r := l.next(); {
		case isOctalDigitChar(r):
			_, _ = b.WriteRune(r)
		default:
			l.backup()
			return Token{Kind: TokenInteger, Val: b.String()}
		}
	}
}

func (l *Lexer) hexadecimalConstant(b *strings.Builder) Token {
	r := l.next()
	_, _ = b.WriteRune(r)
	if !isHexadecimalDigitChar(r) {
		return Token{Kind: TokenInvalid, Val: b.String()}
	}
	for {
		switch r := l.next(); {
		case isHexadecimalDigitChar(r):
			_, _ = b.WriteRune(r)
		default:
			l.backup()
			return Token{Kind: TokenInteger, Val: b.String()}
		}
	}
}

//// Floating point numbers

func (l *Lexer) fraction(b *strings.Builder) Token {
	for {
		switch r := l.next(); {
		case isDecimalDigitChar(r):
			_, _ = b.WriteRune(r)
		case r == 'e', r == 'E':
			_, _ = b.WriteRune(r)
			return l.exponent(b)
		default:
			l.backup()
			return Token{Kind: TokenFloatNumber, Val: b.String()}
		}
	}
}

func (l *Lexer) exponent(b *strings.Builder) Token {
	switch r := l.next(); {
	case r == '-', r == '+':
		_, _ = b.WriteRune(r)
	default:
		l.backup()
	}

	switch r := l.next(); {
	case r == utf8.RuneError:
		return Token{Kind: TokenInsufficient, Val: b.String()}
	case isDecimalDigitChar(r):
		_, _ = b.WriteRune(r)
	default:
		_, _ = b.WriteRune(r)
		return Token{Kind: TokenInvalid, Val: b.String()}
	}

	for {
		switch r := l.next(); {
		case isDecimalDigitChar(r):
			_, _ = b.WriteRune(r)
		default:
			l.backup()
			return Token{Kind: TokenFloatNumber, Val: b.String()}
		}
	}
}

//// Double quoted lists

func (l *Lexer) doubleQuotedListToken(b *strings.Builder) Token {
	for {
		switch r := l.rawNext(); {
		case r == utf8.RuneError:
			return Token{Kind: TokenInsufficient, Val: b.String()}
		case r == '"':
			_, _ = b.WriteRune(r)
			switch r := l.next(); {
			case r == '"':
				_, _ = b.WriteRune(r)
			default:
				l.backup()
				return Token{Kind: TokenDoubleQuotedList, Val: b.String()}
			}
		case isGraphicChar(r), isAlphanumericChar(r), isSoloChar(r), unicode.IsSpace(r), r == '\'', r == '`':
			_, _ = b.WriteRune(r)
		case r == '\\':
			_, _ = b.WriteRune(r)
			switch r := l.next(); {
			case r == '\n':
				_, _ = b.WriteRune(r)
			default:
				l.backup()
				return l.escapeSequence(b, l.doubleQuotedListToken)
			}
		default:
			_, _ = b.WriteRune(r)
			return Token{Kind: TokenInvalid, Val: b.String()}
		}
	}
}

// Characters

func isGraphicChar(r rune) bool {
	return strings.ContainsRune(`#$&*+-./:<=>?@^~`, r)
}

func isAlphanumericChar(r rune) bool {
	return isAlphaChar(r) || isDecimalDigitChar(r)
}

func isAlphaChar(r rune) bool {
	return isUnderscoreChar(r) || isLetterChar(r)
}

func isLetterChar(r rune) bool {
	return isCapitalLetterChar(r) || isSmallLetterChar(r)
}

func isSmallLetterChar(r rune) bool {
	return unicode.IsLower(r) || unicode.In(r,
		unicode.Unified_Ideograph,
		unicode.Hiragana,
		unicode.Katakana,
	)
}

func isCapitalLetterChar(r rune) bool {
	return unicode.IsUpper(r)
}

func isDecimalDigitChar(r rune) bool {
	return strings.ContainsRune(`0123456789`, r)
}

func isBinaryDigitChar(r rune) bool {
	return strings.ContainsRune(`01`, r)
}

func isOctalDigitChar(r rune) bool {
	return strings.ContainsRune("01234567", r)
}

func isHexadecimalDigitChar(r rune) bool {
	return strings.ContainsRune("0123456789ABCDEF", unicode.ToUpper(r))
}

func isUnderscoreChar(r rune) bool {
	return r == '_'
}

func isSoloChar(r rune) bool {
	return strings.ContainsRune(`!(),;[]{}|%`, r)
}

func isLayoutChar(r rune) bool {
	return unicode.IsSpace(r)
}

func isMetaChar(r rune) bool {
	return strings.ContainsRune("\\'\"`", r)
}

func isSymbolicControlChar(r rune) bool {
	return strings.ContainsRune(`abrftnv`, r)
}
