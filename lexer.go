package prolog

import (
	"fmt"
	"regexp"
	"strconv"
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

		s := l.program
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
	TokenVariable
	TokenFloat
	TokenInteger
	TokenAtom
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

type lexState func(rune, int) lexState

func (l *Lexer) program(r rune, pos int) lexState {
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
	return func(r rune, pos int) lexState {
		switch {
		case r == utf8.RuneError:
			return nil
		case unicode.IsSpace(r):
			return l.term(ctx)
		case r == '%':
			return l.singleLineComment(l.term(ctx))
		case r == '/':
			l.backup()
			return l.multiLineComment(pos, l.term(ctx), ctx)
		case unicode.IsLower(r):
			l.backup()
			return l.atom(pos, ctx)
		case r == '\'':
			return l.quotedAtom(pos, ctx)
		case isGraphic(r):
			l.backup()
			return l.graphic(pos, ctx)
		case unicode.IsNumber(r):
			l.backup()
			return l.integer(pos, ctx)
		case unicode.IsUpper(r), r == '_':
			l.backup()
			return l.variable(pos, ctx)
		case r == '!':
			l.emit(Token{Kind: TokenAtom, Val: string(r)})
			return ctx
		case r == ',', r == ';':
			return l.graphic(pos, ctx)
		case r == '[':
			return l.list(pos, ctx)
		case r == '(':
			l.emit(Token{Kind: TokenSeparator, Val: string(r)})
			return l.term(l.paren(ctx))
		default:
			return nil
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
			l.backup()
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

func (l *Lexer) quotedAtom(start int, ctx lexState) lexState {
	return func(r rune, pos int) lexState {
		switch r {
		case '\'':
			return l.quotedAtomQuote(start, ctx)
		case '\\':
			return l.quotedAtomSlash(start, ctx)
		default:
			return l.quotedAtom(start, ctx)
		}
	}
}

var quotedAtomCodePattern = regexp.MustCompile("''|\\\\\n|\\\\a|\\\\b|\\\\f|\\\\n|\\\\r|\\\\t|\\\\v|\\\\x?\\d+\\\\|\\\\\\\\|\\\\'|\\\\\"|\\\\`")

func (l *Lexer) quotedAtomQuote(start int, ctx lexState) lexState {
	return func(r rune, pos int) lexState {
		switch r {
		case '\'':
			return l.quotedAtom(start, ctx)
		default:
			l.backup()
			val := l.input[start+1 : pos-1]
			val = quotedAtomCodePattern.ReplaceAllStringFunc(val, func(s string) string {
				switch {
				case s == `''`:
					return `'`
				case s == "\\\n":
					return ""
				case s == `\a`:
					return "\a"
				case s == `\b`:
					return "\b"
				case s == `\f`:
					return "\f"
				case s == `\n`:
					return "\n"
				case s == `\r`:
					return "\r"
				case s == `\t`:
					return "\t"
				case s == `\v`:
					return "\v"
				case strings.HasSuffix(s, `\`):
					switch {
					case s == `\\`:
						return `\`
					case strings.HasPrefix(s, `\x`):
						i, err := strconv.ParseInt(s[2:len(s)-1], 16, 4*8) // rune is up to 4 bytes
						if err != nil {
							return s
						}
						return string(rune(i))
					default:
						i, err := strconv.ParseInt(s[1:len(s)-1], 8, 4*8) // rune is up to 4 bytes
						if err != nil {
							return s
						}
						return string(rune(i))
					}
				case s == `\'`:
					return `'`
				case s == `\"`:
					return `"`
				case s == "\\`":
					return "`"
				default:
					return s
				}
			})
			l.emit(Token{Kind: TokenAtom, Val: val})
			return ctx
		}
	}
}

func (l *Lexer) quotedAtomSlash(start int, ctx lexState) lexState {
	return func(r rune, pos int) lexState {
		switch {
		case r == 'x' || unicode.IsNumber(r):
			return l.quotedAtomSlashCode(start, ctx)
		default:
			return l.quotedAtom(start, ctx)
		}
	}
}

func (l *Lexer) quotedAtomSlashCode(start int, ctx lexState) lexState {
	return func(r rune, pos int) lexState {
		switch {
		case unicode.IsNumber(r):
			return l.quotedAtomSlashCode(start, ctx)
		case r == '\\':
			return l.quotedAtom(start, ctx)
		default:
			return nil
		}
	}
}

func (l *Lexer) decimal(start int, ctx lexState) lexState {
	return func(r rune, pos int) lexState {
		switch {
		case unicode.IsNumber(r):
			return l.float(start, ctx)
		default:
			l.backup()
			val := l.input[start : pos-1]
			l.emit(Token{Kind: TokenInteger, Val: val})
			l.emit(Token{Kind: TokenSeparator, Val: "."})
			return ctx
		}
	}
}

func (l *Lexer) float(start int, ctx lexState) lexState {
	return func(r rune, pos int) lexState {
		switch {
		case unicode.IsNumber(r):
			return l.float(start, ctx)
		default:
			l.backup()
			val := l.input[start:pos]
			l.emit(Token{Kind: TokenFloat, Val: val})
			return ctx
		}
	}
}

func (l *Lexer) integer(start int, ctx lexState) lexState {
	return func(r rune, pos int) lexState {
		switch {
		case unicode.IsNumber(r):
			return l.integer(start, ctx)
		case r == '.':
			return l.decimal(start, ctx)
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

func (l *Lexer) singleLineComment(ctx lexState) lexState {
	return func(r rune, pos int) lexState {
		switch r {
		case '\n':
			return ctx
		default:
			return l.singleLineComment(ctx)
		}
	}
}

func (l *Lexer) multiLineComment(start int, ctx, elseCtx lexState) lexState {
	return func(r rune, pos int) lexState {
		switch r {
		case '/':
			return l.multiLineCommentBegin(start, ctx, elseCtx)
		default:
			l.backup()
			return l.graphic(start, elseCtx)
		}
	}
}

func (l *Lexer) multiLineCommentBegin(start int, ctx, elseCtx lexState) lexState {
	return func(r rune, pos int) lexState {
		switch {
		case r == '*':
			return l.multiLineCommentBody(ctx)
		case isGraphic(r):
			return l.graphic(start, elseCtx)
		default:
			l.emit(Token{Kind: TokenAtom, Val: "/"})
			l.backup()
			return elseCtx
		}
	}
}

func (l *Lexer) multiLineCommentBody(ctx lexState) lexState {
	return func(r rune, pos int) lexState {
		switch r {
		case '*':
			return l.multiLineCommentEnd(ctx)
		default:
			return l.multiLineCommentBody(ctx)
		}
	}
}

func (l *Lexer) multiLineCommentEnd(ctx lexState) lexState {
	return func(r rune, pos int) lexState {
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

func isExtendedGraphic(r rune) bool {
	return strings.ContainsRune(",;", r) || isGraphic(r)
}
