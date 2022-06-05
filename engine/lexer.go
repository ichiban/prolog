package engine

import (
	"bufio"
	"bytes"
	"fmt"
	"strings"
	"unicode"
	"unicode/utf8"
)

// Lexer turns bytes into tokens.
type Lexer struct {
	input           *bufio.Reader
	buf             bytes.Buffer
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

	l.buf.Reset()

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
		_, _ = l.buf.WriteRune(r)
		return l.letterDigitToken()
	case r == '.':
		_, _ = l.buf.WriteRune(r)
		switch r := l.next(); {
		case isLayoutChar(r), r == '%', r == utf8.RuneError:
			l.backup()
			return Token{Kind: TokenEnd, Val: l.buf.String()}
		default:
			l.backup()
			return l.graphicToken()
		}
	case isGraphicChar(r), r == '\\':
		_, _ = l.buf.WriteRune(r)
		return l.graphicToken()
	case r == '\'':
		_, _ = l.buf.WriteRune(r)
		return l.quotedToken()
	case r == ';':
		return Token{Kind: TokenSemicolon, Val: string(r)}
	case r == '!':
		return Token{Kind: TokenCut, Val: string(r)}
	case r == '_', isCapitalLetterChar(r):
		_, _ = l.buf.WriteRune(r)
		return l.variableToken()
	case isDecimalDigitChar(r):
		l.backup()
		return l.integerToken()
	case r == '"':
		_, _ = l.buf.WriteRune(r)
		return l.doubleQuotedListToken()
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
		_, _ = l.buf.WriteRune('/')
		return l.graphicToken()
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

func (l *Lexer) letterDigitToken() Token {
	for {
		switch r := l.next(); {
		case isAlphanumericChar(r):
			_, _ = l.buf.WriteRune(r)
		default:
			l.backup()
			return Token{Kind: TokenLetterDigit, Val: l.buf.String()}
		}
	}
}

func (l *Lexer) graphicToken() Token {
	for {
		switch r := l.next(); {
		case isGraphicChar(r), r == '\\':
			_, _ = l.buf.WriteRune(r)
		default:
			l.backup()
			return Token{Kind: TokenGraphic, Val: l.buf.String()}
		}
	}
}

func (l *Lexer) quotedToken() Token {
	for {
		switch r := l.rawNext(); {
		case r == utf8.RuneError:
			return Token{Kind: TokenInsufficient, Val: l.buf.String()}
		case r == '\'':
			_, _ = l.buf.WriteRune(r)
			switch r := l.rawNext(); {
			case r == '\'':
				_, _ = l.buf.WriteRune(r)
			default:
				l.backup()
				return Token{Kind: TokenQuoted, Val: l.buf.String()}
			}
		case isGraphicChar(r), isAlphanumericChar(r), isSoloChar(r), unicode.IsSpace(r), r == '"', r == '`':
			_, _ = l.buf.WriteRune(r)
		case r == '\\':
			_, _ = l.buf.WriteRune(r)
			switch r := l.rawNext(); {
			case r == '\n':
				_, _ = l.buf.WriteRune(r)
			default:
				l.backup()
				return l.escapeSequence(l.quotedToken)
			}
		default:
			_, _ = l.buf.WriteRune(r)
			return Token{Kind: TokenInvalid, Val: l.buf.String()}
		}
	}
}

func (l *Lexer) escapeSequence(cont func() Token) Token {
	switch r := l.rawNext(); {
	case r == utf8.RuneError:
		return Token{Kind: TokenInsufficient, Val: l.buf.String()}
	case isMetaChar(r), isSymbolicControlChar(r):
		_, _ = l.buf.WriteRune(r)
		return cont()
	case isOctalDigitChar(r):
		_, _ = l.buf.WriteRune(r)
		return l.octalEscapeSequence(cont)
	case r == 'x':
		_, _ = l.buf.WriteRune(r)
		return l.hexadecimalEscapeSequence(cont)
	default:
		_, _ = l.buf.WriteRune(r)
		return Token{Kind: TokenInvalid, Val: l.buf.String()}
	}
}

func (l *Lexer) octalEscapeSequence(cont func() Token) Token {
	for {
		switch r := l.rawNext(); {
		case r == utf8.RuneError:
			return Token{Kind: TokenInsufficient, Val: l.buf.String()}
		case r == '\\':
			_, _ = l.buf.WriteRune(r)
			return cont()
		case isOctalDigitChar(r):
			_, _ = l.buf.WriteRune(r)
			continue
		default:
			_, _ = l.buf.WriteRune(r)
			return Token{Kind: TokenInvalid, Val: l.buf.String()}
		}
	}
}

func (l *Lexer) hexadecimalEscapeSequence(cont func() Token) Token {
	switch r := l.rawNext(); {
	case r == utf8.RuneError:
		return Token{Kind: TokenInsufficient, Val: l.buf.String()}
	case isHexadecimalDigitChar(r):
		_, _ = l.buf.WriteRune(r)
	default:
		_, _ = l.buf.WriteRune(r)
		return Token{Kind: TokenInvalid, Val: l.buf.String()}
	}

	for {
		switch r := l.next(); {
		case r == utf8.RuneError:
			return Token{Kind: TokenInsufficient, Val: l.buf.String()}
		case r == '\\':
			_, _ = l.buf.WriteRune(r)
			return cont()
		case isHexadecimalDigitChar(r):
			_, _ = l.buf.WriteRune(r)
			continue
		default:
			_, _ = l.buf.WriteRune(r)
			return Token{Kind: TokenInvalid, Val: l.buf.String()}
		}
	}
}

//// Variables

func (l *Lexer) variableToken() Token {
	for {
		switch r := l.next(); {
		case isAlphanumericChar(r):
			_, _ = l.buf.WriteRune(r)
		default:
			l.backup()
			return Token{Kind: TokenVariable, Val: l.buf.String()}
		}
	}
}

//// Integer numbers

func (l *Lexer) integerToken() Token {
	switch r := l.next(); {
	case r == '0':
		_, _ = l.buf.WriteRune(r)
		switch r = l.next(); {
		case r == '\'':
			_, _ = l.buf.WriteRune(r)
			return l.characterCodeConstant()
		case r == 'b':
			_, _ = l.buf.WriteRune(r)
			return l.binaryConstant()
		case r == 'o':
			_, _ = l.buf.WriteRune(r)
			return l.octalConstant()
		case r == 'x':
			_, _ = l.buf.WriteRune(r)
			return l.hexadecimalConstant()
		default:
			l.backup()
			return l.integerConstant()
		}
	default:
		_, _ = l.buf.WriteRune(r)
		return l.integerConstant()
	}
}

func (l *Lexer) integerConstant() Token {
	for {
		switch r := l.next(); {
		case isDecimalDigitChar(r):
			_, _ = l.buf.WriteRune(r)
		case r == '.':
			switch r := l.next(); {
			case isDecimalDigitChar(r):
				_, _ = l.buf.WriteRune('.')
				_, _ = l.buf.WriteRune(r)
				return l.fraction()
			default:
				l.backup()
				l.reserved = Token{Kind: TokenEnd, Val: "."}
				return Token{Kind: TokenInteger, Val: l.buf.String()}
			}
		default:
			l.backup()
			return Token{Kind: TokenInteger, Val: l.buf.String()}
		}
	}
}

func (l *Lexer) characterCodeConstant() Token {
	switch r := l.next(); {
	case r == utf8.RuneError:
		return Token{Kind: TokenInsufficient, Val: l.buf.String()}
	case r == '\'':
		_, _ = l.buf.WriteRune(r)
		switch r := l.next(); {
		case r == utf8.RuneError:
			return Token{Kind: TokenInsufficient, Val: l.buf.String()}
		case r == '\'':
			_, _ = l.buf.WriteRune(r)
			return Token{Kind: TokenInteger, Val: l.buf.String()}
		default:
			_, _ = l.buf.WriteRune(r)
			return Token{Kind: TokenInvalid, Val: l.buf.String()}
		}
	case isGraphicChar(r), isAlphanumericChar(r), isSoloChar(r), unicode.IsSpace(r), r == '"', r == '`':
		_, _ = l.buf.WriteRune(r)
		return Token{Kind: TokenInteger, Val: l.buf.String()}
	case r == '\\':
		_, _ = l.buf.WriteRune(r)
		return l.escapeSequence(func() Token {
			return Token{Kind: TokenInteger, Val: l.buf.String()}
		})
	default:
		_, _ = l.buf.WriteRune(r)
		return Token{Kind: TokenInvalid, Val: l.buf.String()}
	}
}

func (l *Lexer) binaryConstant() Token {
	r := l.next()
	_, _ = l.buf.WriteRune(r)
	if !isBinaryDigitChar(r) {
		return Token{Kind: TokenInvalid, Val: l.buf.String()}
	}
	for {
		switch r := l.next(); {
		case isBinaryDigitChar(r):
			_, _ = l.buf.WriteRune(r)
		default:
			l.backup()
			return Token{Kind: TokenInteger, Val: l.buf.String()}
		}
	}
}

func (l *Lexer) octalConstant() Token {
	r := l.next()
	_, _ = l.buf.WriteRune(r)
	if !isOctalDigitChar(r) {
		return Token{Kind: TokenInvalid, Val: l.buf.String()}
	}
	for {
		switch r := l.next(); {
		case isOctalDigitChar(r):
			_, _ = l.buf.WriteRune(r)
		default:
			l.backup()
			return Token{Kind: TokenInteger, Val: l.buf.String()}
		}
	}
}

func (l *Lexer) hexadecimalConstant() Token {
	r := l.next()
	_, _ = l.buf.WriteRune(r)
	if !isHexadecimalDigitChar(r) {
		return Token{Kind: TokenInvalid, Val: l.buf.String()}
	}
	for {
		switch r := l.next(); {
		case isHexadecimalDigitChar(r):
			_, _ = l.buf.WriteRune(r)
		default:
			l.backup()
			return Token{Kind: TokenInteger, Val: l.buf.String()}
		}
	}
}

//// Floating point numbers

func (l *Lexer) fraction() Token {
	for {
		switch r := l.next(); {
		case isDecimalDigitChar(r):
			_, _ = l.buf.WriteRune(r)
		case r == 'e', r == 'E':
			_, _ = l.buf.WriteRune(r)
			return l.exponent()
		default:
			l.backup()
			return Token{Kind: TokenFloatNumber, Val: l.buf.String()}
		}
	}
}

func (l *Lexer) exponent() Token {
	switch r := l.next(); {
	case r == '-', r == '+':
		_, _ = l.buf.WriteRune(r)
	default:
		l.backup()
	}

	switch r := l.next(); {
	case r == utf8.RuneError:
		return Token{Kind: TokenInsufficient, Val: l.buf.String()}
	case isDecimalDigitChar(r):
		_, _ = l.buf.WriteRune(r)
	default:
		_, _ = l.buf.WriteRune(r)
		return Token{Kind: TokenInvalid, Val: l.buf.String()}
	}

	for {
		switch r := l.next(); {
		case isDecimalDigitChar(r):
			_, _ = l.buf.WriteRune(r)
		default:
			l.backup()
			return Token{Kind: TokenFloatNumber, Val: l.buf.String()}
		}
	}
}

//// Double quoted lists

func (l *Lexer) doubleQuotedListToken() Token {
	for {
		switch r := l.rawNext(); {
		case r == utf8.RuneError:
			return Token{Kind: TokenInsufficient, Val: l.buf.String()}
		case r == '"':
			_, _ = l.buf.WriteRune(r)
			switch r := l.next(); {
			case r == '"':
				_, _ = l.buf.WriteRune(r)
			default:
				l.backup()
				return Token{Kind: TokenDoubleQuotedList, Val: l.buf.String()}
			}
		case isGraphicChar(r), isAlphanumericChar(r), isSoloChar(r), unicode.IsSpace(r), r == '\'', r == '`':
			_, _ = l.buf.WriteRune(r)
		case r == '\\':
			_, _ = l.buf.WriteRune(r)
			switch r := l.next(); {
			case r == '\n':
				_, _ = l.buf.WriteRune(r)
			default:
				l.backup()
				return l.escapeSequence(l.doubleQuotedListToken)
			}
		default:
			_, _ = l.buf.WriteRune(r)
			return Token{Kind: TokenInvalid, Val: l.buf.String()}
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
