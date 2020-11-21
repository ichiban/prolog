package prolog

import (
	"fmt"
	"strings"
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

	l := Lexer{input: input, tokens: tokens}
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
	return <-l.tokens
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

type TokenKind byte

const (
	TokenEOS TokenKind = iota
	TokenAtom
	TokenInteger
	TokenVariable
	TokenSeparator
)

func (k TokenKind) String() string {
	switch k {
	case TokenEOS:
		return "eos"
	case TokenAtom:
		return "atom"
	case TokenInteger:
		return "integer"
	case TokenVariable:
		return "variable"
	case TokenSeparator:
		return "separator"
	default:
		return fmt.Sprintf("unknown(%d)", k)
	}
}

type lexState func(rune, int) lexState

func (l *Lexer) start(r rune, pos int) lexState {
	switch r {
	case '.':
		l.emit(Token{Kind: TokenSeparator, Val: string(r)})
		return l.start
	default:
		l.backup()
		return l.term(l.start)
	}
}

func (l *Lexer) term(ctx lexState) lexState {
	return func(r rune, pos int) lexState {
		switch {
		case r == utf8.RuneError:
			return nil
		case unicode.IsSpace(r):
			return l.term(ctx)
		case unicode.IsLower(r):
			l.backup()
			return l.atom(pos, ctx)
		case isGraphic(r):
			l.backup()
			return l.graphic(pos, ctx)
		case unicode.IsNumber(r):
			l.backup()
			return l.integer(pos, ctx)
		case unicode.IsUpper(r), r == '_':
			l.backup()
			return l.variable(pos, ctx)
		case r == '!', r == ',', r == ';':
			l.emit(Token{Kind: TokenAtom, Val: string(r)})
			return ctx
		case r == '[':
			return l.list(pos, ctx)
		case r == '(':
			l.emit(Token{Kind: TokenSeparator, Val: string(r)})
			return l.term(l.paren(ctx))
		default:
			l.backup()
			return ctx
		}
	}
}

func (l *Lexer) paren(ctx lexState) lexState {
	return func(r rune, pos int) lexState {
		switch r {
		case ')':
			l.emit(Token{Kind: TokenSeparator, Val: string(r)})
			return ctx
		default:
			l.backup()
			return l.term(l.paren(ctx))
		}
	}
}

func (l *Lexer) args(ctx lexState) lexState {
	return func(r rune, pos int) lexState {
		switch r {
		case ')':
			l.emit(Token{Kind: TokenSeparator, Val: string(r)})
			return ctx
		case ',':
			l.emit(Token{Kind: TokenSeparator, Val: string(r)})
			return l.term(l.args(ctx))
		default:
			return l.term(l.args(ctx))
		}
	}
}

func (l *Lexer) elems(ctx lexState) lexState {
	return func(r rune, pos int) lexState {
		switch r {
		case ']':
			l.emit(Token{Kind: TokenSeparator, Val: string(r)})
			return ctx
		case ',':
			l.emit(Token{Kind: TokenSeparator, Val: string(r)})
			return l.term(l.elems(ctx))
		case '|':
			l.emit(Token{Kind: TokenSeparator, Val: string(r)})
			return l.term(l.elems(ctx))
		default:
			return l.term(l.elems(ctx))
		}
	}
}

func (l *Lexer) atom(start int, ctx lexState) lexState {
	return func(r rune, pos int) lexState {
		switch {
		case unicode.IsLetter(r), unicode.IsNumber(r), r == '_':
			return l.atom(start, ctx)
		case r == '(':
			val := l.input[start:pos]
			l.emit(Token{Kind: TokenAtom, Val: val})
			l.emit(Token{Kind: TokenSeparator, Val: string(r)})
			return l.term(l.args(ctx))
		default:
			l.backup()
			val := l.input[start:pos]
			l.emit(Token{Kind: TokenAtom, Val: val})
			return ctx
		}
	}
}

func (l *Lexer) integer(start int, ctx lexState) lexState {
	return func(r rune, pos int) lexState {
		switch {
		case unicode.IsNumber(r):
			return l.integer(start, ctx)
		default:
			l.backup()
			val := l.input[start:pos]
			l.emit(Token{Kind: TokenInteger, Val: val})
			return ctx
		}
	}
}

func (l *Lexer) variable(start int, ctx lexState) lexState {
	return func(r rune, pos int) lexState {
		switch {
		case unicode.IsLetter(r), unicode.IsNumber(r), r == '_':
			return l.variable(start, ctx)
		default:
			l.backup()
			val := l.input[start:pos]
			l.emit(Token{Kind: TokenVariable, Val: val})
			return ctx
		}
	}
}

func (l *Lexer) graphic(start int, ctx lexState) lexState {
	return func(r rune, pos int) lexState {
		switch {
		// TODO: comments
		case isGraphic(r):
			return l.graphic(start, ctx)
		case r == '(':
			val := l.input[start:pos]
			l.emit(Token{Kind: TokenAtom, Val: val})
			l.emit(Token{Kind: TokenSeparator, Val: string(r)})
			return l.term(l.args(ctx))
		default:
			l.backup()
			val := l.input[start:pos]
			l.emit(Token{Kind: TokenAtom, Val: val})
			return ctx
		}
	}
}

func (l *Lexer) list(start int, ctx lexState) lexState {
	return func(r rune, pos int) lexState {
		switch r {
		case ']':
			val := l.input[start : pos+1]
			l.emit(Token{Kind: TokenAtom, Val: val})
			return ctx
		default:
			l.emit(Token{Kind: TokenSeparator, Val: "["})
			l.backup()
			return l.term(l.elems(ctx))
		}
	}
}

func isGraphic(r rune) bool {
	return strings.ContainsRune("#$&*+-./:<=>?@^~\\", r)
}
