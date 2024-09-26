package internal

import (
	"bytes"
	"fmt"
	"github.com/ichiban/prolog/internal/ring"
	"io"
	"strings"
	"unicode"
	"unicode/utf8"
	"unsafe"
)

// Lexer turns runes into tokens.
type Lexer struct {
	module func() *Module
	input  runeRingBuffer

	buf    bytes.Buffer
	offset int
}

// Token returns the next token.
func (l *Lexer) Token() (Token, error) {
	if l.module == nil {
		l.module = func() *Module {
			return &Module{}
		}
	}
	l.offset = l.buf.Len()
	return l.layoutTextSequence(false)
}

func (l *Lexer) next() (rune, error) {
	r, err := l.rawNext()
	if err != nil {
		return r, err
	}
	return l.conv(r), nil
}

func (l *Lexer) rawNext() (rune, error) {
	r, _, err := l.input.ReadRune()
	return r, err
}

func (l *Lexer) conv(r rune) rune {
	if l.module == nil {
		return r
	}
	m := l.module()
	if r, ok := m.charConversions[r]; ok {
		return r
	}
	return r
}

func (l *Lexer) backup() {
	_ = l.input.UnreadRune()
}

func (l *Lexer) accept(r rune) {
	_, _ = l.buf.WriteRune(r)
}

func (l *Lexer) chunk() string {
	b := l.buf.Bytes()[l.offset:]
	return *(*string)(unsafe.Pointer(&b))
}

// Token is a smallest meaningful unit of prolog program.
type Token struct {
	kind tokenKind
	val  string
}

func (t Token) String() string {
	return fmt.Sprintf("%s(%s)", t.kind.String(), t.val)
}

// tokenKind is a type of Token.
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

func (l *Lexer) token(afterLayout bool) (Token, error) {
	switch r, err := l.next(); {
	case err != nil:
		return Token{}, err
	case isSmallLetterChar(r):
		l.accept(r)
		return l.letterDigitToken()
	case r == '.':
		l.accept(r)
		if l.wasEndChar() {
			return Token{kind: tokenEnd, val: l.chunk()}, nil
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
			return Token{kind: tokenOpen, val: l.chunk()}, nil
		}
		return Token{kind: tokenOpenCT, val: l.chunk()}, nil
	default:
		k := tokenInvalid
		if int(r) < len(soloTokenKinds) {
			k = soloTokenKinds[r]
		}
		l.accept(r)
		return Token{kind: k, val: l.chunk()}, nil
	}
}

func (l *Lexer) wasEndChar() bool {
	r, err := l.next()
	if err != nil {
		return true
	}
	l.backup()
	return isLayoutChar(r) || r == '%'
}

//// Layout text

func (l *Lexer) layoutTextSequence(afterLayout bool) (Token, error) {
	for {
		switch r, err := l.next(); {
		case err == io.EOF:
			return l.token(afterLayout)
		case err != nil:
			return Token{}, err
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

func (l *Lexer) commentText(bracketed bool) (Token, error) {
	if bracketed {
		for {
			switch r, err := l.next(); {
			case err != nil:
				return Token{}, err
			case r == '*':
				return l.commentClose()
			}
		}
	} else {
		for {
			switch r, err := l.next(); {
			case err != nil:
				return Token{}, err
			case r == '\n':
				return l.layoutTextSequence(true)
			}
		}
	}
}

func (l *Lexer) commentOpen() (Token, error) {
	switch r, err := l.next(); {
	case err == io.EOF:
		l.accept('/')
		return l.graphicToken()
	case err != nil:
		return Token{}, err
	case r == '*':
		return l.commentText(true)
	default:
		l.backup()
		l.accept('/')
		return l.graphicToken()
	}
}

func (l *Lexer) commentClose() (Token, error) {
	switch r, err := l.next(); {
	case err != nil:
		return Token{}, err
	case r == '/':
		return l.layoutTextSequence(true)
	default:
		return l.commentText(true)
	}
}

//// Names

func (l *Lexer) letterDigitToken() (Token, error) {
	for {
		switch r, err := l.next(); {
		case err == io.EOF:
			return Token{kind: tokenLetterDigit, val: l.chunk()}, nil
		case err != nil:
			return Token{}, err
		case isAlphanumericChar(r):
			l.accept(r)
		default:
			l.backup()
			return Token{kind: tokenLetterDigit, val: l.chunk()}, nil
		}
	}
}

func (l *Lexer) graphicToken() (Token, error) {
	for {
		switch r, err := l.next(); {
		case err == io.EOF:
			return Token{kind: tokenGraphic, val: l.chunk()}, nil
		case err != nil:
			return Token{}, err
		case isGraphicChar(r), r == '\\':
			l.accept(r)
		default:
			l.backup()
			return Token{kind: tokenGraphic, val: l.chunk()}, nil
		}
	}
}

func (l *Lexer) quotedToken() (Token, error) {
	for {
		switch r, err := l.rawNext(); {
		case err != nil:
			return Token{}, err
		case isSingleQuotedCharacter(r):
			l.accept(r)
			continue
		case r == '\'':
			l.accept(r)
			switch r, err := l.rawNext(); {
			case err == io.EOF:
				break
			case err != nil:
				return Token{}, err
			case r == '\'':
				l.accept(r)
				continue
			default:
				l.backup()
			}

			s := l.chunk()

			// Checks if it contains invalid octal or hexadecimal escape sequences.
			if strings.ContainsRune(unquote(s), utf8.RuneError) {
				return Token{kind: tokenInvalid, val: s}, nil
			}

			return Token{kind: tokenQuoted, val: s}, nil
		case r == '\\':
			l.accept(r)
			switch r, err := l.rawNext(); {
			case err == io.EOF:
				break
			case err != nil:
				return Token{}, err
			case r == '\n':
				l.accept(r)
				continue
			default:
				l.backup()
			}

			return l.escapeSequence(l.quotedToken)
		default:
			l.accept(r)
			return Token{kind: tokenInvalid, val: l.chunk()}, nil
		}
	}
}

func (l *Lexer) escapeSequence(cont func() (Token, error)) (Token, error) {
	switch r, err := l.rawNext(); {
	case err != nil:
		return Token{}, err
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
		return Token{kind: tokenInvalid, val: l.chunk()}, nil
	}
}

func (l *Lexer) octalEscapeSequence(cont func() (Token, error)) (Token, error) {
	for {
		switch r, err := l.rawNext(); {
		case err != nil:
			return Token{}, err
		case r == '\\':
			l.accept(r)
			return cont()
		case isOctalDigitChar(r):
			l.accept(r)
			continue
		default:
			l.accept(r)
			return Token{kind: tokenInvalid, val: l.chunk()}, nil
		}
	}
}

func (l *Lexer) hexadecimalEscapeSequence(cont func() (Token, error)) (Token, error) {
	switch r, err := l.rawNext(); {
	case err != nil:
		return Token{}, err
	case isHexadecimalDigitChar(r):
		l.accept(r)
	default:
		l.accept(r)
		return Token{kind: tokenInvalid, val: l.chunk()}, nil
	}

	for {
		switch r, err := l.next(); {
		case err != nil:
			return Token{}, err
		case r == '\\':
			l.accept(r)
			return cont()
		case isHexadecimalDigitChar(r):
			l.accept(r)
			continue
		default:
			l.accept(r)
			return Token{kind: tokenInvalid, val: l.chunk()}, nil
		}
	}
}

//// Variables

func (l *Lexer) variableToken() (Token, error) {
	for {
		switch r, err := l.next(); {
		case err == io.EOF:
			return Token{kind: tokenVariable, val: l.chunk()}, nil
		case err != nil:
			return Token{}, err
		case isAlphanumericChar(r):
			l.accept(r)
		default:
			l.backup()
			return Token{kind: tokenVariable, val: l.chunk()}, nil
		}
	}
}

//// Integer numbers

func (l *Lexer) integerToken(first rune) (Token, error) {
	switch first {
	case '0':
		l.accept(first)
		switch r, err := l.next(); {
		case err == io.EOF:
			return l.integerConstant()
		case err != nil:
			return Token{}, err
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

func (l *Lexer) integerTokenCharacterCode(r rune) (Token, error) {
	switch r, err := l.next(); {
	case err == io.EOF:
		break
	case err != nil:
		return Token{}, err
	case r == '\'':
		switch r, err := l.next(); {
		case err == io.EOF:
			l.backup()
			l.backup()
			return Token{kind: tokenInteger, val: l.chunk()}, nil // 0
		case err != nil:
			return Token{}, err
		case r == '\'': // 0'''
			l.backup()
			l.backup()
		default:
			l.backup()
			l.backup()
			l.backup()
			return Token{kind: tokenInteger, val: l.chunk()}, nil // 0
		}
	case r == '\\':
		switch r, err := l.next(); {
		case err == io.EOF:
			l.backup()
		case err != nil:
			return Token{}, err
		case r == '\n':
			l.backup()
			l.backup()
			l.backup()
			return Token{kind: tokenInteger, val: l.chunk()}, nil // 0
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

func (l *Lexer) integerTokenBinary(r rune) (Token, error) {
	switch r, err := l.next(); {
	case err == io.EOF:
		l.backup()
		return Token{kind: tokenInteger, val: l.chunk()}, nil
	case err != nil:
		return Token{}, err
	case isBinaryDigitChar(r):
		l.backup()
	default:
		l.backup()
		l.backup()
		return Token{kind: tokenInteger, val: l.chunk()}, nil
	}
	l.accept(r)
	return l.binaryConstant()
}

func (l *Lexer) integerTokenOctal(r rune) (Token, error) {
	switch r, err := l.next(); {
	case err == io.EOF:
		l.backup()
		return Token{kind: tokenInteger, val: l.chunk()}, nil
	case err != nil:
		return Token{}, err
	case isOctalDigitChar(r):
		l.backup()
	default:
		l.backup()
		l.backup()
		return Token{kind: tokenInteger, val: l.chunk()}, nil
	}
	l.accept(r)
	return l.octalConstant()
}

func (l *Lexer) integerTokenHexadecimal(r rune) (Token, error) {
	switch r, err := l.next(); {
	case err == io.EOF:
		l.backup()
		return Token{kind: tokenInteger, val: l.chunk()}, nil
	case err != nil:
		return Token{}, err
	case isHexadecimalDigitChar(r):
		l.backup()
	default:
		l.backup()
		l.backup()
		return Token{kind: tokenInteger, val: l.chunk()}, nil
	}
	l.accept(r)
	return l.hexadecimalConstant()
}

func (l *Lexer) integerConstant() (Token, error) {
	for {
		switch r, err := l.next(); {
		case err == io.EOF:
			return Token{kind: tokenInteger, val: l.chunk()}, nil
		case err != nil:
			return Token{}, err
		case isDecimalDigitChar(r):
			l.accept(r)
		case r == '.':
			switch r, err := l.next(); {
			case err == io.EOF:
				l.backup()
				return Token{kind: tokenInteger, val: l.chunk()}, nil
			case err != nil:
				return Token{}, err
			case isDecimalDigitChar(r):
				l.accept('.')
				l.accept(r)
				return l.fraction()
			default:
				l.backup()
				l.backup()
				return Token{kind: tokenInteger, val: l.chunk()}, nil
			}
		default:
			l.backup()
			return Token{kind: tokenInteger, val: l.chunk()}, nil
		}
	}
}

func (l *Lexer) characterCodeConstant() (Token, error) {
	switch r, err := l.next(); {
	case err != nil:
		return Token{}, err
	case r == '\'':
		l.accept(r)
		r, _ := l.next() // r == '\''
		l.accept(r)
		return Token{kind: tokenInteger, val: l.chunk()}, nil
	case r == '\\':
		l.accept(r)
		return l.escapeSequence(func() (Token, error) {
			return Token{kind: tokenInteger, val: l.chunk()}, nil
		})
	case isGraphicChar(r), isAlphanumericChar(r), isSoloChar(r), r == ' ':
		l.accept(r)
		return Token{kind: tokenInteger, val: l.chunk()}, nil
	default:
		l.accept(r)
		return Token{kind: tokenInvalid, val: l.chunk()}, nil
	}
}

func (l *Lexer) binaryConstant() (Token, error) {
	for {
		switch r, err := l.next(); {
		case err == io.EOF:
			return Token{kind: tokenInteger, val: l.chunk()}, nil
		case err != nil:
			return Token{}, err
		case isBinaryDigitChar(r):
			l.accept(r)
		default:
			l.backup()
			return Token{kind: tokenInteger, val: l.chunk()}, nil
		}
	}
}

func (l *Lexer) octalConstant() (Token, error) {
	for {
		switch r, err := l.next(); {
		case err == io.EOF:
			return Token{kind: tokenInteger, val: l.chunk()}, nil
		case err != nil:
			return Token{}, err
		case isOctalDigitChar(r):
			l.accept(r)
		default:
			l.backup()
			return Token{kind: tokenInteger, val: l.chunk()}, nil
		}
	}
}

func (l *Lexer) hexadecimalConstant() (Token, error) {
	for {
		switch r, err := l.next(); {
		case err == io.EOF:
			return Token{kind: tokenInteger, val: l.chunk()}, nil
		case err != nil:
			return Token{}, err
		case isHexadecimalDigitChar(r):
			l.accept(r)
		default:
			l.backup()
			return Token{kind: tokenInteger, val: l.chunk()}, nil
		}
	}
}

//// Floating point numbers

func (l *Lexer) fraction() (Token, error) {
	for {
		switch r, err := l.next(); {
		case err == io.EOF:
			return Token{kind: tokenFloatNumber, val: l.chunk()}, nil
		case err != nil:
			return Token{}, err
		case isDecimalDigitChar(r):
			l.accept(r)
		case isExponentChar(r):
			var sign rune
			switch r, err := l.next(); {
			case err == io.EOF:
				l.backup() // for 'e' or 'E'
				return Token{kind: tokenFloatNumber, val: l.chunk()}, nil
			case err != nil:
				return Token{}, err
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
				return Token{kind: tokenFloatNumber, val: l.chunk()}, nil
			case err != nil:
				return Token{}, err
			case isDecimalDigitChar(r):
				l.backup()
				break
			default:
				l.backup()
				if sign != 0 {
					l.backup()
				}
				l.backup() // for 'e' or 'E'
				return Token{kind: tokenFloatNumber, val: l.chunk()}, nil
			}

			l.accept(r) // 'e' or 'E'
			if sign != 0 {
				l.accept(sign)
			}
			return l.exponent()
		default:
			l.backup()
			return Token{kind: tokenFloatNumber, val: l.chunk()}, nil
		}
	}
}

func (l *Lexer) exponent() (Token, error) {
	for {
		switch r, err := l.next(); {
		case err == io.EOF:
			return Token{kind: tokenFloatNumber, val: l.chunk()}, nil
		case err != nil:
			return Token{}, err
		case isDecimalDigitChar(r):
			l.accept(r)
		default:
			l.backup()
			return Token{kind: tokenFloatNumber, val: l.chunk()}, nil
		}
	}
}

//// Double quoted lists

func (l *Lexer) doubleQuotedListToken() (Token, error) {
	for {
		switch r, err := l.rawNext(); {
		case err != nil:
			return Token{}, err
		case r == '"':
			l.accept(r)
			switch r, err := l.next(); {
			case err == io.EOF:
				return Token{kind: tokenDoubleQuotedList, val: l.chunk()}, nil
			case err != nil:
				return Token{}, err
			case r == '"':
				l.accept(r)
			default:
				l.backup()
				return Token{kind: tokenDoubleQuotedList, val: l.chunk()}, nil
			}
		case r == '\\':
			l.accept(r)
			switch r, err := l.next(); {
			case err != nil:
				return Token{}, err
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

type runeRingBuffer struct {
	base io.RuneReader
	buf  *ring.Buffer[rune]
}

func newRuneRingBuffer(r io.RuneReader) runeRingBuffer {
	return runeRingBuffer{base: r, buf: ring.NewBuffer[rune](4)}
}

func (b *runeRingBuffer) ReadRune() (rune, int, error) {
	if b.buf.Empty() {
		r, n, err := b.base.ReadRune()
		if err != nil {
			return r, n, err
		}
		b.buf.Put(r)
	}
	return b.buf.Get(), 0, nil
}

func (b *runeRingBuffer) UnreadRune() error {
	b.buf.Backup()
	return nil
}
