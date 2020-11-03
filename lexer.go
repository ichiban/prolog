package prolog

import (
	"unicode"
	"unicode/utf8"
)

type Lexer struct {
	input  string
	tokens chan Token

	pos   int
	width int
}

func NewLexer(input string) *Lexer {
	tokens := make(chan Token)

	l := Lexer{
		input:  input,
		tokens: tokens,
	}

	go func() {
		defer close(tokens)

		s := l.start
		for s != nil {
			s = s(l.next())
		}
	}()

	return &l
}

func (l *Lexer) Next() Token {
	t := <-l.tokens
	return t
}

func (l *Lexer) next() (rune, int) {
	r, w := utf8.DecodeRuneInString(l.input[l.pos:])
	l.width = w
	pos := l.pos
	l.pos += l.width
	return r, pos
}

func (l *Lexer) backup() {
	l.pos -= l.width
}

func (l *Lexer) emit(t Token) {
	l.tokens <- t
}

type Token struct {
	Kind TokenKind
	Val  string
}

type TokenKind int

const (
	TokenEOS TokenKind = iota
	TokenAtom
	TokenVariable
	TokenSeparator
)

type lexState func(rune, int) lexState

func (l *Lexer) start(r rune, pos int) lexState {
	switch {
	case r == utf8.RuneError:
		return nil
	case unicode.IsSpace(r):
		return l.start
	case unicode.IsLower(r):
		l.backup()
		return l.atom(pos)
	case isGraphic(r):
		l.backup()
		return l.graphic(pos)
	case unicode.IsUpper(r), r == '_':
		l.backup()
		return l.variable(pos)
	default:
		l.backup()
		return l.separator(pos)
	}
}

func (l *Lexer) atom(start int) lexState {
	return func(r rune, pos int) lexState {
		switch {
		case unicode.IsLetter(r), unicode.IsNumber(r), r == '_':
			return l.atom(start)
		default:
			l.backup()
			val := l.input[start:pos]
			l.emit(Token{
				Kind: TokenAtom,
				Val:  val,
			})
			return l.start
		}
	}
}

func isGraphic(r rune) bool {
	for _, o := range `#$&*+-./:<=>?^~\!` {
		if r == o {
			return true
		}
	}
	return false
}

func (l *Lexer) graphic(start int) lexState {
	return func(r rune, pos int) lexState {
		switch {
		case isGraphic(r):
			return l.graphic(start)
		default:
			l.backup()
			val := l.input[start:pos]
			kind := TokenAtom
			if val == "." {
				kind = TokenSeparator
			}
			l.emit(Token{
				Kind: kind,
				Val:  val,
			})
			return l.start
		}
	}
}

func (l *Lexer) variable(start int) lexState {
	return func(r rune, pos int) lexState {
		switch {
		case unicode.IsLetter(r), unicode.IsNumber(r), r == '_':
			return l.variable(start)
		default:
			l.backup()
			val := l.input[start:pos]
			l.emit(Token{
				Kind: TokenVariable,
				Val:  val,
			})
			return l.start
		}
	}
}

func (l *Lexer) separator(start int) lexState {
	return func(r rune, pos int) lexState {
		switch r {
		case '.', ',', ';', '(', ')', '[', ']', '|':
			val := l.input[start : pos+1]
			l.emit(Token{
				Kind: TokenSeparator,
				Val:  val,
			})
			return l.start
		default:
			l.backup()
			val := l.input[start:pos]
			l.emit(Token{
				Kind: TokenSeparator,
				Val:  val,
			})
			return l.start
		}
	}
}
