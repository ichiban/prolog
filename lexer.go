package prolog

import (
	"bytes"
	"fmt"
	"io"
	"strings"
	"unicode"
	"unicode/utf8"
	"unsafe"

	"github.com/ichiban/prolog/v2/internal/ring"
)

// lexer turns runes into tokens.
type lexer struct {
	input           *ring.RuneReader
	charConversions map[rune]rune

	buf    bytes.Buffer
	offset int
}

// Token returns the next token.
func (l *lexer) Token() (token, error) {
	l.offset = l.buf.Len()
	return l.layoutTextSequence(false)
}

func (l *lexer) next() (rune, error) {
	r, err := l.rawNext()
	return l.conv(r), err
}

func (l *lexer) rawNext() (rune, error) {
	r, _, err := l.input.ReadRune()
	return r, err
}

func (l *lexer) conv(r rune) rune {
	if r, ok := l.charConversions[r]; ok {
		return r
	}
	return r
}

func (l *lexer) backup() {
	_ = l.input.UnreadRune()
}

func (l *lexer) accept(r rune) {
	_, _ = l.buf.WriteRune(r)
}

func (l *lexer) chunk() string {
	b := l.buf.Bytes()[l.offset:]
	return *(*string)(unsafe.Pointer(&b))
}

// token is a smallest meaningful unit of prolog program.
type token struct {
	kind tokenKind
	val  string
}

func (t token) String() string {
	return fmt.Sprintf("%s(%s)", t.kind.String(), t.val)
}

// tokenKind is a type of token.
type tokenKind byte

const (
	// tokenInvalid represents an invalid token.
	tokenInvalid tokenKind = iota

	// tokenLetterDigit represents a letter digit token.
	tokenLetterDigit

	// tokenGraphic represents a graphical token.
	tokenGraphic

	// tokenQuoted represents a quoted token.
	tokenQuoted

	// tokenSemicolon represents a semicolon token.
	tokenSemicolon

	// tokenCut represents a cut token.
	tokenCut

	// tokenVariable represents a variable token.
	tokenVariable

	// tokenInteger represents an integer token.
	tokenInteger

	// tokenFloatNumber represents a floating-point token.
	tokenFloatNumber

	// tokenDoubleQuotedList represents a double-quoted string.
	tokenDoubleQuotedList

	// tokenOpen represents an open parenthesis.
	tokenOpen

	// tokenOpenCT represents an open CT parenthesis.
	tokenOpenCT

	// tokenClose represents a close parenthesis.
	tokenClose

	// tokenOpenList represents an open bracket.
	tokenOpenList

	// tokenCloseList represents a close bracket.
	tokenCloseList

	// tokenOpenCurly represents an open brace.
	tokenOpenCurly

	// tokenCloseCurly represents a close brace.
	tokenCloseCurly

	// tokenBar represents a bar.
	tokenBar

	// tokenComma represents a comma.
	tokenComma

	// tokenEnd represents a period.
	tokenEnd
)

// GoString returns a string representation of tokenKind.
func (k tokenKind) GoString() string {
	return k.String()
}

func (k tokenKind) String() string {
	return [...]string{
		tokenInvalid:          "invalid",
		tokenLetterDigit:      "letter digit",
		tokenGraphic:          "graphic",
		tokenQuoted:           "quoted",
		tokenSemicolon:        "semicolon",
		tokenCut:              "cut",
		tokenVariable:         "variable",
		tokenInteger:          "integer",
		tokenFloatNumber:      "float number",
		tokenDoubleQuotedList: "double quoted list",
		tokenOpen:             "open",
		tokenOpenCT:           "open ct",
		tokenClose:            "close",
		tokenOpenList:         "open list",
		tokenCloseList:        "close list",
		tokenOpenCurly:        "open curly",
		tokenCloseCurly:       "close curly",
		tokenBar:              "bar",
		tokenComma:            "comma",
		tokenEnd:              "end",
	}[k]
}

// Tokens

var soloTokenKinds = [...]tokenKind{
	';': tokenSemicolon,
	'!': tokenCut,
	')': tokenClose,
	'[': tokenOpenList,
	']': tokenCloseList,
	'{': tokenOpenCurly,
	'}': tokenCloseCurly,
	'|': tokenBar,
	',': tokenComma,
}

func (l *lexer) token(afterLayout bool) (token, error) {
	switch r, err := l.next(); {
	case err != nil:
		return token{}, err
	case isSmallLetterChar(r):
		l.accept(r)
		return l.letterDigitToken()
	case r == '.':
		l.accept(r)
		if l.wasEndChar() {
			return token{kind: tokenEnd, val: l.chunk()}, nil
		}
		return l.graphicToken()
	case isGraphicChar(r), r == '\\':
		l.accept(r)
		return l.graphicToken()
	case r == '\'':
		l.accept(r)
		return l.quotedToken()
	case r == '_', isCapitalLetterChar(r):
		l.accept(r)
		return l.variableToken()
	case isDecimalDigitChar(r):
		return l.integerToken(r)
	case r == '"':
		l.accept(r)
		return l.doubleQuotedListToken()
	case r == '(':
		l.accept(r)
		if afterLayout {
			return token{kind: tokenOpen, val: l.chunk()}, nil
		}
		return token{kind: tokenOpenCT, val: l.chunk()}, nil
	default:
		k := tokenInvalid
		if int(r) < len(soloTokenKinds) {
			k = soloTokenKinds[r]
		}
		l.accept(r)
		return token{kind: k, val: l.chunk()}, nil
	}
}

func (l *lexer) wasEndChar() bool {
	r, err := l.next()
	if err != nil {
		return true
	}
	l.backup()
	return isLayoutChar(r) || r == '%'
}

//// Layout text

func (l *lexer) layoutTextSequence(afterLayout bool) (token, error) {
	for {
		switch r, err := l.next(); {
		case err == io.EOF:
			return l.token(afterLayout)
		case err != nil:
			return token{}, err
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

func (l *lexer) commentText(bracketed bool) (token, error) {
	for {
		switch r, err := l.next(); {
		case err != nil:
			return token{}, err
		case bracketed && r == '*':
			return l.commentClose()
		case !bracketed && r == '\n':
			return l.layoutTextSequence(true)
		}
	}
}

func (l *lexer) commentOpen() (token, error) {
	switch r, err := l.next(); {
	case err == io.EOF:
		l.accept('/')
		return l.graphicToken()
	case err != nil:
		return token{}, err
	case r == '*':
		return l.commentText(true)
	default:
		l.backup()
		l.accept('/')
		return l.graphicToken()
	}
}

func (l *lexer) commentClose() (token, error) {
	switch r, err := l.next(); {
	case err != nil:
		return token{}, err
	case r == '/':
		return l.layoutTextSequence(true)
	case r == '*':
		return l.commentClose()
	default:
		return l.commentText(true)
	}
}

//// Names

func (l *lexer) letterDigitToken() (token, error) {
	for {
		switch r, err := l.next(); {
		case err == io.EOF:
			return token{kind: tokenLetterDigit, val: l.chunk()}, nil
		case err != nil:
			return token{}, err
		case isAlphanumericChar(r):
			l.accept(r)
		default:
			l.backup()
			return token{kind: tokenLetterDigit, val: l.chunk()}, nil
		}
	}
}

func (l *lexer) graphicToken() (token, error) {
	for {
		switch r, err := l.next(); {
		case err == io.EOF:
			return token{kind: tokenGraphic, val: l.chunk()}, nil
		case err != nil:
			return token{}, err
		case isGraphicChar(r), r == '\\':
			l.accept(r)
		default:
			l.backup()
			return token{kind: tokenGraphic, val: l.chunk()}, nil
		}
	}
}

func (l *lexer) quotedToken() (token, error) {
	for {
		switch r, err := l.rawNext(); {
		case err != nil:
			return token{}, err
		case isSingleQuotedCharacter(r):
			l.accept(r)
			continue
		case r == '\'':
			l.accept(r)
			switch r, err := l.rawNext(); {
			case err == io.EOF:
				break
			case err != nil:
				return token{}, err
			case r == '\'':
				l.accept(r)
				continue
			default:
				l.backup()
			}

			s := l.chunk()

			// Checks if it contains invalid octal or hexadecimal escape sequences.
			if strings.ContainsRune(unquote(s), utf8.RuneError) {
				return token{kind: tokenInvalid, val: s}, nil
			}

			return token{kind: tokenQuoted, val: s}, nil
		case r == '\\':
			l.accept(r)
			switch r, err := l.rawNext(); {
			case err == io.EOF:
				break
			case err != nil:
				return token{}, err
			case r == '\n':
				l.accept(r)
				continue
			default:
				l.backup()
			}

			return l.escapeSequence(l.quotedToken)
		default:
			l.accept(r)
			return token{kind: tokenInvalid, val: l.chunk()}, nil
		}
	}
}

func (l *lexer) escapeSequence(cont func() (token, error)) (token, error) {
	switch r, err := l.rawNext(); {
	case err != nil:
		return token{}, err
	case isMetaChar(r), isSymbolicControlChar(r):
		l.accept(r)
		return cont()
	case isOctalDigitChar(r):
		l.accept(r)
		return l.octalEscapeSequence(cont)
	case r == 'x':
		l.accept(r)
		return l.hexadecimalEscapeSequence(cont)
	default:
		l.accept(r)
		return token{kind: tokenInvalid, val: l.chunk()}, nil
	}
}

func (l *lexer) octalEscapeSequence(cont func() (token, error)) (token, error) {
	for {
		switch r, err := l.rawNext(); {
		case err != nil:
			return token{}, err
		case r == '\\':
			l.accept(r)
			return cont()
		case isOctalDigitChar(r):
			l.accept(r)
			continue
		default:
			l.accept(r)
			return token{kind: tokenInvalid, val: l.chunk()}, nil
		}
	}
}

func (l *lexer) hexadecimalEscapeSequence(cont func() (token, error)) (token, error) {
	switch r, err := l.rawNext(); {
	case err != nil:
		return token{}, err
	case isHexadecimalDigitChar(r):
		l.accept(r)
	default:
		l.accept(r)
		return token{kind: tokenInvalid, val: l.chunk()}, nil
	}

	for {
		switch r, err := l.next(); {
		case err != nil:
			return token{}, err
		case r == '\\':
			l.accept(r)
			return cont()
		case isHexadecimalDigitChar(r):
			l.accept(r)
			continue
		default:
			l.accept(r)
			return token{kind: tokenInvalid, val: l.chunk()}, nil
		}
	}
}

//// Variables

func (l *lexer) variableToken() (token, error) {
	for {
		switch r, err := l.next(); {
		case err == io.EOF:
			return token{kind: tokenVariable, val: l.chunk()}, nil
		case err != nil:
			return token{}, err
		case isAlphanumericChar(r):
			l.accept(r)
		default:
			l.backup()
			return token{kind: tokenVariable, val: l.chunk()}, nil
		}
	}
}

//// Integer numbers

func (l *lexer) integerToken(first rune) (token, error) {
	switch first {
	case '0':
		l.accept(first)
		switch r, err := l.next(); {
		case err == io.EOF:
			return l.integerConstant()
		case err != nil:
			return token{}, err
		case r == '\'':
			return l.integerTokenCharacterCode(r)
		case r == 'b':
			return l.integerTokenBinary(r)
		case r == 'o':
			return l.integerTokenOctal(r)
		case r == 'x':
			return l.integerTokenHexadecimal(r)
		default:
			l.backup()
			return l.integerConstant()
		}
	default:
		l.accept(first)
		return l.integerConstant()
	}
}

func (l *lexer) integerTokenCharacterCode(r rune) (token, error) {
	switch r, err := l.next(); {
	case err == io.EOF:
		break
	case err != nil:
		return token{}, err
	case r == '\'':
		switch r, err := l.next(); {
		case err == io.EOF:
			l.backup()
			l.backup()
			return token{kind: tokenInteger, val: l.chunk()}, nil // 0
		case err != nil:
			return token{}, err
		case r == '\'': // 0'''
			l.backup()
			l.backup()
		default:
			l.backup()
			l.backup()
			l.backup()
			return token{kind: tokenInteger, val: l.chunk()}, nil // 0
		}
	case r == '\\':
		switch r, err := l.next(); {
		case err == io.EOF:
			l.backup()
		case err != nil:
			return token{}, err
		case r == '\n':
			l.backup()
			l.backup()
			l.backup()
			return token{kind: tokenInteger, val: l.chunk()}, nil // 0
		default:
			l.backup()
			l.backup()
		}
	default:
		l.backup()
	}
	l.accept(r)
	return l.characterCodeConstant()
}

func (l *lexer) integerTokenBinary(r rune) (token, error) {
	switch r, err := l.next(); {
	case err == io.EOF:
		l.backup()
		return token{kind: tokenInteger, val: l.chunk()}, nil
	case err != nil:
		return token{}, err
	case isBinaryDigitChar(r):
		l.backup()
	default:
		l.backup()
		l.backup()
		return token{kind: tokenInteger, val: l.chunk()}, nil
	}
	l.accept(r)
	return l.binaryConstant()
}

func (l *lexer) integerTokenOctal(r rune) (token, error) {
	switch r, err := l.next(); {
	case err == io.EOF:
		l.backup()
		return token{kind: tokenInteger, val: l.chunk()}, nil
	case err != nil:
		return token{}, err
	case isOctalDigitChar(r):
		l.backup()
	default:
		l.backup()
		l.backup()
		return token{kind: tokenInteger, val: l.chunk()}, nil
	}
	l.accept(r)
	return l.octalConstant()
}

func (l *lexer) integerTokenHexadecimal(r rune) (token, error) {
	switch r, err := l.next(); {
	case err == io.EOF:
		l.backup()
		return token{kind: tokenInteger, val: l.chunk()}, nil
	case err != nil:
		return token{}, err
	case isHexadecimalDigitChar(r):
		l.backup()
	default:
		l.backup()
		l.backup()
		return token{kind: tokenInteger, val: l.chunk()}, nil
	}
	l.accept(r)
	return l.hexadecimalConstant()
}

func (l *lexer) integerConstant() (token, error) {
	for {
		switch r, err := l.next(); {
		case err == io.EOF:
			return token{kind: tokenInteger, val: l.chunk()}, nil
		case err != nil:
			return token{}, err
		case isDecimalDigitChar(r):
			l.accept(r)
		case r == '.':
			switch r, err := l.next(); {
			case err == io.EOF:
				l.backup()
				return token{kind: tokenInteger, val: l.chunk()}, nil
			case err != nil:
				return token{}, err
			case isDecimalDigitChar(r):
				l.accept('.')
				l.accept(r)
				return l.fraction()
			default:
				l.backup()
				l.backup()
				return token{kind: tokenInteger, val: l.chunk()}, nil
			}
		default:
			l.backup()
			return token{kind: tokenInteger, val: l.chunk()}, nil
		}
	}
}

func (l *lexer) characterCodeConstant() (token, error) {
	switch r, err := l.next(); {
	case err != nil:
		return token{}, err
	case r == '\'':
		l.accept(r)
		r, _ := l.next() // r == '\''
		l.accept(r)
		return token{kind: tokenInteger, val: l.chunk()}, nil
	case r == '\\':
		l.accept(r)
		return l.escapeSequence(func() (token, error) {
			return token{kind: tokenInteger, val: l.chunk()}, nil
		})
	case isGraphicChar(r), isAlphanumericChar(r), isSoloChar(r), r == ' ':
		l.accept(r)
		return token{kind: tokenInteger, val: l.chunk()}, nil
	default:
		l.accept(r)
		return token{kind: tokenInvalid, val: l.chunk()}, nil
	}
}

func (l *lexer) binaryConstant() (token, error) {
	for {
		switch r, err := l.next(); {
		case err == io.EOF:
			return token{kind: tokenInteger, val: l.chunk()}, nil
		case err != nil:
			return token{}, err
		case isBinaryDigitChar(r):
			l.accept(r)
		default:
			l.backup()
			return token{kind: tokenInteger, val: l.chunk()}, nil
		}
	}
}

func (l *lexer) octalConstant() (token, error) {
	for {
		switch r, err := l.next(); {
		case err == io.EOF:
			return token{kind: tokenInteger, val: l.chunk()}, nil
		case err != nil:
			return token{}, err
		case isOctalDigitChar(r):
			l.accept(r)
		default:
			l.backup()
			return token{kind: tokenInteger, val: l.chunk()}, nil
		}
	}
}

func (l *lexer) hexadecimalConstant() (token, error) {
	for {
		switch r, err := l.next(); {
		case err == io.EOF:
			return token{kind: tokenInteger, val: l.chunk()}, nil
		case err != nil:
			return token{}, err
		case isHexadecimalDigitChar(r):
			l.accept(r)
		default:
			l.backup()
			return token{kind: tokenInteger, val: l.chunk()}, nil
		}
	}
}

//// Floating point numbers

func (l *lexer) fraction() (token, error) {
	for {
		switch r, err := l.next(); {
		case err == io.EOF:
			return token{kind: tokenFloatNumber, val: l.chunk()}, nil
		case err != nil:
			return token{}, err
		case isDecimalDigitChar(r):
			l.accept(r)
		case isExponentChar(r):
			var sign rune
			switch r, err := l.next(); {
			case err == io.EOF:
				l.backup() // for 'e' or 'E'
				return token{kind: tokenFloatNumber, val: l.chunk()}, nil
			case err != nil:
				return token{}, err
			case isSignChar(r):
				sign = r
			default:
				l.backup()
			}

			switch r, err := l.next(); {
			case err == io.EOF:
				if sign != 0 {
					l.backup()
				}
				l.backup() // for 'e' or 'E'
				return token{kind: tokenFloatNumber, val: l.chunk()}, nil
			case err != nil:
				return token{}, err
			case isDecimalDigitChar(r):
				l.backup()
				break
			default:
				l.backup()
				if sign != 0 {
					l.backup()
				}
				l.backup() // for 'e' or 'E'
				return token{kind: tokenFloatNumber, val: l.chunk()}, nil
			}

			l.accept(r) // 'e' or 'E'
			if sign != 0 {
				l.accept(sign)
			}
			return l.exponent()
		default:
			l.backup()
			return token{kind: tokenFloatNumber, val: l.chunk()}, nil
		}
	}
}

func (l *lexer) exponent() (token, error) {
	for {
		switch r, err := l.next(); {
		case err == io.EOF:
			return token{kind: tokenFloatNumber, val: l.chunk()}, nil
		case err != nil:
			return token{}, err
		case isDecimalDigitChar(r):
			l.accept(r)
		default:
			l.backup()
			return token{kind: tokenFloatNumber, val: l.chunk()}, nil
		}
	}
}

//// Double quoted lists

func (l *lexer) doubleQuotedListToken() (token, error) {
	for {
		switch r, err := l.rawNext(); {
		case err != nil:
			return token{}, err
		case r == '"':
			l.accept(r)
			switch r, err := l.next(); {
			case err == io.EOF:
				return token{kind: tokenDoubleQuotedList, val: l.chunk()}, nil
			case err != nil:
				return token{}, err
			case r == '"':
				l.accept(r)
			default:
				l.backup()
				return token{kind: tokenDoubleQuotedList, val: l.chunk()}, nil
			}
		case r == '\\':
			l.accept(r)
			switch r, err := l.next(); {
			case err != nil:
				return token{}, err
			case r == '\n':
				l.accept(r)
			default:
				l.backup()
				return l.escapeSequence(l.doubleQuotedListToken)
			}
		default:
			l.accept(r)
		}
	}
}

// Characters

func isGraphicChar(r rune) bool {
	return strings.ContainsRune(`#$&*+-./:<=>?@^~`, r) || unicode.In(r, &unicode.RangeTable{
		R16: []unicode.Range16{
			{Lo: 0x2200, Hi: 0x22FF, Stride: 1}, // Mathematical Operators
			{Lo: 0x2A00, Hi: 0x2AFF, Stride: 1}, // Supplemental Mathematical Operators
		},
	})
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
	return unicode.In(r, unicode.Ll, unicode.Lo, unicode.Lm)
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

func isSoloChar(r rune) bool {
	return strings.ContainsRune(`!(),;[]{}|%`, r)
}

func isUnderscoreChar(r rune) bool {
	return r == '_'
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

func isSingleQuotedCharacter(r rune) bool {
	return isGraphicChar(r) || isAlphanumericChar(r) || isSoloChar(r) || r == ' ' || r == '"' || r == '`'
}

func isExponentChar(r rune) bool {
	return r == 'e' || r == 'E'
}

func isSignChar(r rune) bool {
	return r == '-' || r == '+'
}
