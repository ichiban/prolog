package engine

import (
	"bytes"
	"io"
	"strings"
	"unicode"
	"unicode/utf8"
	"unsafe"
)

// Lexer turns bytes into tokens.
type Lexer struct {
	input           runeRingBuffer
	buf             bytes.Buffer
	offset          int
	charConversions map[rune]rune
}

// Token returns the next token.
func (l *Lexer) Token() (Token, error) {
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
	if err == io.EOF {
		return utf8.RuneError, nil
	}
	return r, err
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

func (l *Lexer) accept(r rune) {
	_, _ = l.buf.WriteRune(r)
}

func (l *Lexer) chunk() string {
	b := l.buf.Bytes()[l.offset:]
	return *(*string)(unsafe.Pointer(&b))
}

// Token is a smallest meaningful unit of prolog program.
type Token struct {
	Kind TokenKind
	Val  string
}

// TokenKind is a type of Token.
type TokenKind byte

const (
	// TokenInvalid represents an invalid token.
	TokenInvalid TokenKind = iota

	// TokenInsufficient represents an insufficient token.
	TokenInsufficient

	// TokenEOF represents an error.
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

	// TokenBar represents a bar.
	TokenBar

	// TokenComma represents a comma.
	TokenComma

	// TokenEnd represents a period.
	TokenEnd

	tokenKindLen
)

// GoString returns a string representation of TokenKind.
func (k TokenKind) GoString() string {
	return k.String()
}

func (k TokenKind) String() string {
	return [...]string{
		TokenEOF:              "error",
		TokenInvalid:          "invalid",
		TokenInsufficient:     "insufficient",
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
		TokenBar:              "bar",
		TokenComma:            "comma",
		TokenEnd:              "end",
	}[k]
}

// Tokens

var soloTokenKinds = [...]TokenKind{
	';': TokenSemicolon,
	'!': TokenCut,
	')': TokenClose,
	'[': TokenOpenList,
	']': TokenCloseList,
	'{': TokenOpenCurly,
	'}': TokenCloseCurly,
	'|': TokenBar,
	',': TokenComma,
}

func (l *Lexer) token(afterLayout bool) (Token, error) {
	switch r, err := l.next(); {
	case err != nil:
		return Token{}, err
	case r == utf8.RuneError:
		return Token{Kind: TokenEOF}, nil
	case isSmallLetterChar(r):
		l.accept(r)
		return l.letterDigitToken()
	case r == '.':
		l.accept(r)
		if l.wasEndChar() {
			return Token{Kind: TokenEnd, Val: l.chunk()}, nil
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
		l.backup()
		return l.integerToken()
	case r == '"':
		l.accept(r)
		return l.doubleQuotedListToken()
	case r == '(':
		l.accept(r)
		if afterLayout {
			return Token{Kind: TokenOpen, Val: l.chunk()}, nil
		}
		return Token{Kind: TokenOpenCT, Val: l.chunk()}, nil
	default:
		k := TokenInvalid
		if int(r) < len(soloTokenKinds) {
			k = soloTokenKinds[r]
		}
		l.accept(r)
		return Token{Kind: k, Val: l.chunk()}, nil
	}
}

func (l *Lexer) wasEndChar() bool {
	r, err := l.next()
	if err != nil {
		return true
	}
	l.backup()
	return isLayoutChar(r) || r == '%' || r == utf8.RuneError
}

//// Layout text

func (l *Lexer) layoutTextSequence(afterLayout bool) (Token, error) {
	for {
		switch r, err := l.next(); {
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
			case r == utf8.RuneError:
				return Token{Kind: TokenInsufficient}, nil
			case r == '*':
				return l.commentClose()
			}
		}
	} else {
		for {
			switch r, err := l.next(); {
			case err != nil:
				return Token{}, err
			case r == '\n', r == utf8.RuneError:
				return l.layoutTextSequence(true)
			}
		}
	}
}

func (l *Lexer) commentOpen() (Token, error) {
	switch r, err := l.next(); {
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
		case err != nil:
			return Token{}, err
		case isAlphanumericChar(r):
			l.accept(r)
		default:
			l.backup()
			return Token{Kind: TokenLetterDigit, Val: l.chunk()}, nil
		}
	}
}

func (l *Lexer) graphicToken() (Token, error) {
	for {
		switch r, err := l.next(); {
		case err != nil:
			return Token{}, err
		case isGraphicChar(r), r == '\\':
			l.accept(r)
		default:
			l.backup()
			return Token{Kind: TokenGraphic, Val: l.chunk()}, nil
		}
	}
}

func (l *Lexer) quotedToken() (Token, error) {
	for {
		switch r, err := l.rawNext(); {
		case err != nil:
			return Token{}, err
		case r == utf8.RuneError:
			return Token{Kind: TokenInsufficient, Val: l.chunk()}, nil
		case r == '\'':
			l.accept(r)
			switch r, err := l.rawNext(); {
			case err != nil:
				return Token{}, err
			case r == '\'':
				l.accept(r)
			default:
				l.backup()
				s := l.chunk()

				// Checks if it contains invalid octal or hexadecimal escape sequences.
				if strings.ContainsRune(unquote(s), utf8.RuneError) {
					return Token{Kind: TokenInvalid, Val: s}, nil
				}

				return Token{Kind: TokenQuoted, Val: s}, nil
			}
		case r == '\\':
			l.accept(r)
			switch r, err := l.rawNext(); {
			case err != nil:
				return Token{}, err
			case r == '\n':
				l.accept(r)
			default:
				l.backup()
				return l.escapeSequence(l.quotedToken)
			}
		case isGraphicChar(r), isAlphanumericChar(r), isSoloChar(r), r == ' ', r == '"', r == '`':
			l.accept(r)
		default:
			l.accept(r)
			return Token{Kind: TokenInvalid, Val: l.chunk()}, nil
		}
	}
}

func (l *Lexer) escapeSequence(cont func() (Token, error)) (Token, error) {
	switch r, err := l.rawNext(); {
	case err != nil:
		return Token{}, err
	case r == utf8.RuneError:
		return Token{Kind: TokenInsufficient, Val: l.chunk()}, nil
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
		return Token{Kind: TokenInvalid, Val: l.chunk()}, nil
	}
}

func (l *Lexer) octalEscapeSequence(cont func() (Token, error)) (Token, error) {
	for {
		switch r, err := l.rawNext(); {
		case err != nil:
			return Token{}, err
		case r == utf8.RuneError:
			return Token{Kind: TokenInsufficient, Val: l.chunk()}, nil
		case r == '\\':
			l.accept(r)
			return cont()
		case isOctalDigitChar(r):
			l.accept(r)
			continue
		default:
			l.accept(r)
			return Token{Kind: TokenInvalid, Val: l.chunk()}, nil
		}
	}
}

func (l *Lexer) hexadecimalEscapeSequence(cont func() (Token, error)) (Token, error) {
	switch r, err := l.rawNext(); {
	case err != nil:
		return Token{}, err
	case r == utf8.RuneError:
		return Token{Kind: TokenInsufficient, Val: l.chunk()}, nil
	case isHexadecimalDigitChar(r):
		l.accept(r)
	default:
		l.accept(r)
		return Token{Kind: TokenInvalid, Val: l.chunk()}, nil
	}

	for {
		switch r, err := l.next(); {
		case err != nil:
			return Token{}, err
		case r == utf8.RuneError:
			return Token{Kind: TokenInsufficient, Val: l.chunk()}, nil
		case r == '\\':
			l.accept(r)
			return cont()
		case isHexadecimalDigitChar(r):
			l.accept(r)
			continue
		default:
			l.accept(r)
			return Token{Kind: TokenInvalid, Val: l.chunk()}, nil
		}
	}
}

//// Variables

func (l *Lexer) variableToken() (Token, error) {
	for {
		switch r, err := l.next(); {
		case err != nil:
			return Token{}, err
		case isAlphanumericChar(r):
			l.accept(r)
		default:
			l.backup()
			return Token{Kind: TokenVariable, Val: l.chunk()}, nil
		}
	}
}

//// Integer numbers

func (l *Lexer) integerToken() (Token, error) {
	switch r, err := l.next(); {
	case err != nil:
		return Token{}, err
	case r == '0':
		l.accept(r)
		switch r, err = l.next(); {
		case err != nil:
			return Token{}, err
		case r == '\'':
			switch r, err := l.next(); {
			case err != nil:
				return Token{}, err
			case r == '\'':
				switch r, err := l.next(); {
				case err != nil:
					return Token{}, err
				case r == '\'': // 0'''
					l.backup()
					l.backup()
				default:
					l.backup()
					l.backup()
					l.backup()
					return Token{Kind: TokenInteger, Val: l.chunk()}, nil // 0
				}
			case r == '\\':
				switch r, err := l.next(); {
				case err != nil:
					return Token{}, err
				case r == '\n':
					l.backup()
					l.backup()
					l.backup()
					return Token{Kind: TokenInteger, Val: l.chunk()}, nil // 0
				default:
					l.backup()
					l.backup()
				}
			default:
				l.backup()
			}
			l.accept(r)
			return l.characterCodeConstant()
		case r == 'b':
			switch r, err := l.next(); {
			case err != nil:
				return Token{}, err
			case isBinaryDigitChar(r):
				l.backup()
			default:
				l.backup()
				l.backup()
				return Token{Kind: TokenInteger, Val: l.chunk()}, nil
			}
			l.accept(r)
			return l.binaryConstant()
		case r == 'o':
			switch r, err := l.next(); {
			case err != nil:
				return Token{}, err
			case isOctalDigitChar(r):
				l.backup()
			default:
				l.backup()
				l.backup()
				return Token{Kind: TokenInteger, Val: l.chunk()}, nil
			}
			l.accept(r)
			return l.octalConstant()
		case r == 'x':
			switch r, err := l.next(); {
			case err != nil:
				return Token{}, err
			case isHexadecimalDigitChar(r):
				l.backup()
			default:
				l.backup()
				l.backup()
				return Token{Kind: TokenInteger, Val: l.chunk()}, err
			}
			l.accept(r)
			return l.hexadecimalConstant()
		default:
			l.backup()
			return l.integerConstant()
		}
	default:
		l.accept(r)
		return l.integerConstant()
	}
}

func (l *Lexer) integerConstant() (Token, error) {
	for {
		switch r, err := l.next(); {
		case err != nil:
			return Token{}, err
		case isDecimalDigitChar(r):
			l.accept(r)
		case r == '.':
			switch r, err := l.next(); {
			case err != nil:
				return Token{}, err
			case isDecimalDigitChar(r):
				l.accept('.')
				l.accept(r)
				return l.fraction()
			default:
				l.backup()
				l.backup()
				return Token{Kind: TokenInteger, Val: l.chunk()}, nil
			}
		default:
			l.backup()
			return Token{Kind: TokenInteger, Val: l.chunk()}, nil
		}
	}
}

func (l *Lexer) characterCodeConstant() (Token, error) {
	switch r, err := l.next(); {
	case err != nil:
		return Token{}, err
	case r == utf8.RuneError:
		return Token{Kind: TokenInsufficient, Val: l.chunk()}, nil
	case r == '\'':
		l.accept(r)
		r, err := l.next() // r == '\''
		if err != nil {
			return Token{}, err
		}
		l.accept(r)
		return Token{Kind: TokenInteger, Val: l.chunk()}, nil
	case r == '\\':
		l.accept(r)
		return l.escapeSequence(func() (Token, error) {
			return Token{Kind: TokenInteger, Val: l.chunk()}, nil
		})
	case isGraphicChar(r), isAlphanumericChar(r), isSoloChar(r), r == ' ':
		l.accept(r)
		return Token{Kind: TokenInteger, Val: l.chunk()}, nil
	default:
		l.accept(r)
		return Token{Kind: TokenInvalid, Val: l.chunk()}, nil
	}
}

func (l *Lexer) binaryConstant() (Token, error) {
	for {
		switch r, err := l.next(); {
		case err != nil:
			return Token{}, err
		case isBinaryDigitChar(r):
			l.accept(r)
		default:
			l.backup()
			return Token{Kind: TokenInteger, Val: l.chunk()}, nil
		}
	}
}

func (l *Lexer) octalConstant() (Token, error) {
	for {
		switch r, err := l.next(); {
		case err != nil:
			return Token{}, err
		case isOctalDigitChar(r):
			l.accept(r)
		default:
			l.backup()
			return Token{Kind: TokenInteger, Val: l.chunk()}, nil
		}
	}
}

func (l *Lexer) hexadecimalConstant() (Token, error) {
	for {
		switch r, err := l.next(); {
		case err != nil:
			return Token{}, err
		case isHexadecimalDigitChar(r):
			l.accept(r)
		default:
			l.backup()
			return Token{Kind: TokenInteger, Val: l.chunk()}, nil
		}
	}
}

//// Floating point numbers

func (l *Lexer) fraction() (Token, error) {
	for {
		switch r, err := l.next(); {
		case err != nil:
			return Token{}, err
		case isDecimalDigitChar(r):
			l.accept(r)
		case r == 'e', r == 'E':
			var sign rune
			switch r, err := l.next(); {
			case err != nil:
				return Token{}, err
			case r == '-', r == '+':
				sign = r
			default:
				l.backup()
			}

			switch r, err := l.next(); {
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
				return Token{Kind: TokenFloatNumber, Val: l.chunk()}, err
			}

			l.accept(r) // 'e' or 'E'
			if sign != 0 {
				l.accept(sign)
			}
			return l.exponent()
		default:
			l.backup()
			return Token{Kind: TokenFloatNumber, Val: l.chunk()}, nil
		}
	}
}

func (l *Lexer) exponent() (Token, error) {
	for {
		switch r, err := l.next(); {
		case err != nil:
			return Token{}, err
		case isDecimalDigitChar(r):
			l.accept(r)
		default:
			l.backup()
			return Token{Kind: TokenFloatNumber, Val: l.chunk()}, nil
		}
	}
}

//// Double quoted lists

func (l *Lexer) doubleQuotedListToken() (Token, error) {
	for {
		switch r, err := l.rawNext(); {
		case err != nil:
			return Token{}, err
		case r == utf8.RuneError:
			return Token{Kind: TokenInsufficient, Val: l.chunk()}, nil
		case r == '"':
			l.accept(r)
			switch r, err := l.next(); {
			case err != nil:
				return Token{}, err
			case r == '"':
				l.accept(r)
			default:
				l.backup()
				return Token{Kind: TokenDoubleQuotedList, Val: l.chunk()}, nil
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

type runeRingBuffer struct {
	base       io.RuneReader
	buf        [4]rune
	start, end int
}

func newRuneRingBuffer(r io.RuneReader) runeRingBuffer {
	return runeRingBuffer{base: r}
}

func (b *runeRingBuffer) ReadRune() (rune, int, error) {
	if b.empty() {
		r, _, err := b.base.ReadRune()
		switch err {
		case nil:
			b.put(r)
		case io.EOF:
			b.put(utf8.RuneError)
		default:
			return 0, 0, err
		}
	}
	return b.get(), 0, nil
}

func (b *runeRingBuffer) UnreadRune() error {
	b.backup()
	return nil
}

func (b *runeRingBuffer) put(r rune) {
	b.buf[b.end] = r
	b.end++
	b.end %= len(b.buf)
}

func (b *runeRingBuffer) get() rune {
	r := b.buf[b.start]
	b.start++
	b.start %= len(b.buf)
	return r
}

func (b *runeRingBuffer) empty() bool {
	return b.start == b.end
}

func (b *runeRingBuffer) backup() {
	b.start--
	b.start %= len(b.buf)
	if b.start < 0 {
		b.start += len(b.buf)
	}
}
