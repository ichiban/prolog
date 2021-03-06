package internal

import (
	"bufio"
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
	state           lexState
	tokens          []Token
	pos             int
	width           int
}

// NewLexer create a lexer with an input and char conversions.
func NewLexer(input *bufio.Reader, charConversions map[rune]rune) *Lexer {
	l := Lexer{input: input, charConversions: charConversions}
	l.state = l.program
	return &l
}

// Next returns the next token.
func (l *Lexer) Next() (Token, error) {
	for l.state != nil && len(l.tokens) == 0 {
		r, err := l.next()
		if err != nil {
			return Token{}, err
		}
		l.state = l.state(r)
	}

	if len(l.tokens) > 0 {
		var t Token
		t, l.tokens = l.tokens[0], l.tokens[1:]
		return t, nil
	}

	return Token{}, nil
}

const etx = '\u0002'

func (l *Lexer) next() (rune, error) {
	r, w, err := l.input.ReadRune()
	switch err {
	case nil:
		break
	case io.EOF:
		w = etx
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

	// TokenSeparator represents a separator, merely syntactical token.
	TokenSeparator
)

func (k TokenKind) String() string {
	switch k {
	case TokenEOS:
		return "eos"
	case TokenVariable:
		return "variable"
	case TokenFloat:
		return "float"
	case TokenInteger:
		return "integer"
	case TokenAtom:
		return "atom"
	case TokenSeparator:
		return "separator"
	default:
		return fmt.Sprintf("unknown(%d)", k)
	}
}

func (l *Lexer) conv(r rune) rune {
	if r, ok := l.charConversions[r]; ok {
		return r
	}
	return r
}

type lexState func(rune) lexState

func (l *Lexer) program(r rune) lexState {
	r = l.conv(r)
	switch r {
	case '.':
		l.emit(Token{Kind: TokenSeparator, Val: string(r)})
		return l.program
	default:
		l.backup()
		return l.term(l.program)
	}
}

func (l *Lexer) term(ctx lexState) lexState {
	return func(r rune) lexState {
		r = l.conv(r)
		switch {
		case unicode.IsSpace(r):
			return l.term(ctx)
		case r == '%':
			return l.singleLineComment(l.term(ctx))
		case r == '/':
			l.backup()
			var b strings.Builder
			return l.multiLineComment(&b, l.term(ctx), ctx)
		case unicode.IsLower(r):
			l.backup()
			var b strings.Builder
			return l.atom(&b, ctx)
		case r == '\'':
			var b strings.Builder
			return l.quotedAtom(&b, ctx)
		case isGraphic(r):
			l.backup()
			var b strings.Builder
			return l.graphic(&b, ctx)
		case unicode.IsNumber(r):
			l.backup()
			var b strings.Builder
			return l.integer(&b, ctx)
		case unicode.IsUpper(r), r == '_':
			l.backup()
			var b strings.Builder
			return l.variable(&b, ctx)
		case r == '!':
			l.emit(Token{Kind: TokenAtom, Val: string(r)})
			return ctx
		case r == ',', r == ';':
			var b strings.Builder
			if _, err := b.WriteRune(r); err != nil {
				return nil
			}
			return l.graphic(&b, ctx)
		case r == '[':
			return l.list(ctx)
		case r == '(':
			l.emit(Token{Kind: TokenSeparator, Val: string(r)})
			return l.term(l.paren(ctx))
		default:
			return nil
		}
	}
}

func (l *Lexer) paren(ctx lexState) lexState {
	return func(r rune) lexState {
		r = l.conv(r)
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
	return func(r rune) lexState {
		r = l.conv(r)
		switch r {
		case ')':
			l.emit(Token{Kind: TokenSeparator, Val: string(r)})
			return ctx
		case ',':
			l.emit(Token{Kind: TokenSeparator, Val: string(r)})
			return l.term(l.args(ctx))
		default:
			l.backup()
			return l.term(l.args(ctx))
		}
	}
}

func (l *Lexer) elems(ctx lexState) lexState {
	return func(r rune) lexState {
		r = l.conv(r)
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

func (l *Lexer) atom(b *strings.Builder, ctx lexState) lexState {
	return func(r rune) lexState {
		r = l.conv(r)
		switch {
		case unicode.IsLetter(r), unicode.IsNumber(r), r == '_':
			if _, err := b.WriteRune(r); err != nil {
				return nil
			}
			return l.atom(b, ctx)
		case r == '(':
			l.emit(Token{Kind: TokenAtom, Val: b.String()})
			l.emit(Token{Kind: TokenSeparator, Val: string(r)})
			return l.term(l.args(ctx))
		default:
			l.backup()
			l.emit(Token{Kind: TokenAtom, Val: b.String()})
			return ctx
		}
	}
}

func (l *Lexer) quotedAtom(b *strings.Builder, ctx lexState) lexState {
	return func(r rune) lexState {
		switch r {
		case '\'':
			return l.quotedAtomQuote(b, ctx)
		case '\\':
			return l.quotedAtomSlash(b, ctx)
		default:
			if _, err := b.WriteRune(r); err != nil {
				return nil
			}
			return l.quotedAtom(b, ctx)
		}
	}
}

func (l *Lexer) quotedAtomQuote(b *strings.Builder, ctx lexState) lexState {
	return func(r rune) lexState {
		switch r {
		case '\'':
			if _, err := b.WriteRune(r); err != nil {
				return nil
			}
			return l.quotedAtom(b, ctx)
		default:
			l.backup()
			l.emit(Token{Kind: TokenAtom, Val: b.String()})
			return ctx
		}
	}
}

func (l *Lexer) quotedAtomSlash(b *strings.Builder, ctx lexState) lexState {
	return func(r rune) lexState {
		switch {
		case r == '\n':
			return l.quotedAtom(b, ctx)
		case r == 'x':
			var val strings.Builder
			return l.quotedAtomSlashCode(b, ctx, 16, &val)
		case unicode.IsNumber(r):
			var val strings.Builder
			if _, err := val.WriteRune(r); err != nil {
				return nil
			}
			return l.quotedAtomSlashCode(b, ctx, 8, &val)
		case r == 'a':
			if _, err := b.WriteRune('\a'); err != nil {
				return nil
			}
			return l.quotedAtom(b, ctx)
		case r == 'b':
			if _, err := b.WriteRune('\b'); err != nil {
				return nil
			}
			return l.quotedAtom(b, ctx)
		case r == 'f':
			if _, err := b.WriteRune('\f'); err != nil {
				return nil
			}
			return l.quotedAtom(b, ctx)
		case r == 'n':
			if _, err := b.WriteRune('\n'); err != nil {
				return nil
			}
			return l.quotedAtom(b, ctx)
		case r == 'r':
			if _, err := b.WriteRune('\r'); err != nil {
				return nil
			}
			return l.quotedAtom(b, ctx)
		case r == 't':
			if _, err := b.WriteRune('\t'); err != nil {
				return nil
			}
			return l.quotedAtom(b, ctx)
		case r == 'v':
			if _, err := b.WriteRune('\v'); err != nil {
				return nil
			}
			return l.quotedAtom(b, ctx)
		case r == '\\':
			if _, err := b.WriteRune('\\'); err != nil {
				return nil
			}
			return l.quotedAtom(b, ctx)
		case r == '\'':
			if _, err := b.WriteRune('\''); err != nil {
				return nil
			}
			return l.quotedAtom(b, ctx)
		case r == '"':
			if _, err := b.WriteRune('"'); err != nil {
				return nil
			}
			return l.quotedAtom(b, ctx)
		case r == '`':
			if _, err := b.WriteRune('`'); err != nil {
				return nil
			}
			return l.quotedAtom(b, ctx)
		default:
			return nil
		}
	}
}

func (l *Lexer) quotedAtomSlashCode(b *strings.Builder, ctx lexState, base int, val *strings.Builder) lexState {
	return func(r rune) lexState {
		switch {
		case unicode.IsNumber(r):
			if _, err := val.WriteRune(r); err != nil {
				return nil
			}
			return l.quotedAtomSlashCode(b, ctx, base, val)
		case r == '\\':
			i, err := strconv.ParseInt(val.String(), base, 4*8) // rune is up to 4 bytes
			if err != nil {
				return nil
			}
			if _, err := b.WriteRune(rune(i)); err != nil {
				return nil
			}
			return l.quotedAtom(b, ctx)
		default:
			return nil
		}
	}
}

func (l *Lexer) decimal(b *strings.Builder, ctx lexState) lexState {
	return func(r rune) lexState {
		r = l.conv(r)
		switch {
		case unicode.IsNumber(r):
			if _, err := b.WriteRune('.'); err != nil {
				return nil
			}
			if _, err := b.WriteRune(r); err != nil {
				return nil
			}
			return l.float(b, ctx)
		default:
			s := b.String()
			l.backup()
			l.emit(Token{Kind: TokenInteger, Val: s})
			l.emit(Token{Kind: TokenSeparator, Val: "."})
			return ctx
		}
	}
}

func (l *Lexer) float(b *strings.Builder, ctx lexState) lexState {
	return func(r rune) lexState {
		r = l.conv(r)
		switch {
		case unicode.IsNumber(r):
			if _, err := b.WriteRune(r); err != nil {
				return nil
			}
			return l.float(b, ctx)
		default:
			l.backup()
			l.emit(Token{Kind: TokenFloat, Val: b.String()})
			return ctx
		}
	}
}

func (l *Lexer) integer(b *strings.Builder, ctx lexState) lexState {
	return func(r rune) lexState {
		r = l.conv(r)
		switch {
		case unicode.IsNumber(r):
			if _, err := b.WriteRune(r); err != nil {
				return nil
			}
			return l.integer(b, ctx)
		case r == '.':
			return l.decimal(b, ctx)
		default:
			l.backup()
			l.emit(Token{Kind: TokenInteger, Val: b.String()})
			return ctx
		}
	}
}

func (l *Lexer) variable(b *strings.Builder, ctx lexState) lexState {
	return func(r rune) lexState {
		r = l.conv(r)
		switch {
		case unicode.IsLetter(r), unicode.IsNumber(r), r == '_':
			if _, err := b.WriteRune(r); err != nil {
				return nil
			}
			return l.variable(b, ctx)
		default:
			l.backup()
			l.emit(Token{Kind: TokenVariable, Val: b.String()})
			return ctx
		}
	}
}

func (l *Lexer) graphic(b *strings.Builder, ctx lexState) lexState {
	return func(r rune) lexState {
		r = l.conv(r)
		switch {
		case isGraphic(r):
			if _, err := b.WriteRune(r); err != nil {
				return nil
			}
			return l.graphic(b, ctx)
		case r == '(':
			l.emit(Token{Kind: TokenAtom, Val: b.String()})
			l.emit(Token{Kind: TokenSeparator, Val: string(r)})
			return l.term(l.args(ctx))
		default:
			l.backup()
			l.emit(Token{Kind: TokenAtom, Val: b.String()})
			return ctx
		}
	}
}

func (l *Lexer) list(ctx lexState) lexState {
	return func(r rune) lexState {
		r = l.conv(r)
		switch r {
		case ']':
			l.emit(Token{Kind: TokenAtom, Val: "[]"})
			return ctx
		default:
			l.emit(Token{Kind: TokenSeparator, Val: "["})
			l.backup()
			return l.term(l.elems(ctx))
		}
	}
}

func (l *Lexer) singleLineComment(ctx lexState) lexState {
	return func(r rune) lexState {
		r = l.conv(r)
		switch r {
		case '\n':
			return ctx
		default:
			return l.singleLineComment(ctx)
		}
	}
}

func (l *Lexer) multiLineComment(b *strings.Builder, ctx, elseCtx lexState) lexState {
	return func(r rune) lexState {
		r = l.conv(r)
		switch r {
		case '/':
			if _, err := b.WriteRune(r); err != nil {
				return nil
			}
			return l.multiLineCommentBegin(b, ctx, elseCtx)
		default:
			l.backup()
			return l.graphic(b, elseCtx)
		}
	}
}

func (l *Lexer) multiLineCommentBegin(b *strings.Builder, ctx, elseCtx lexState) lexState {
	return func(r rune) lexState {
		r = l.conv(r)
		switch {
		case r == '*':
			return l.multiLineCommentBody(ctx)
		case isGraphic(r):
			if _, err := b.WriteRune(r); err != nil {
				return nil
			}
			return l.graphic(b, elseCtx)
		default:
			l.emit(Token{Kind: TokenAtom, Val: "/"})
			l.backup()
			return elseCtx
		}
	}
}

func (l *Lexer) multiLineCommentBody(ctx lexState) lexState {
	return func(r rune) lexState {
		r = l.conv(r)
		switch r {
		case '*':
			return l.multiLineCommentEnd(ctx)
		default:
			return l.multiLineCommentBody(ctx)
		}
	}
}

func (l *Lexer) multiLineCommentEnd(ctx lexState) lexState {
	return func(r rune) lexState {
		r = l.conv(r)
		switch r {
		case '/':
			return ctx
		default:
			return l.multiLineCommentBody(ctx)
		}
	}
}

func isGraphic(r rune) bool {
	return strings.ContainsRune("#$&*+-./:<=>?@^~\\", r)
}

// IsExtendedGraphic checks if the rune is a graphic token, comma, or semicolon.
func IsExtendedGraphic(r rune) bool {
	return strings.ContainsRune(",;", r) || isGraphic(r)
}
