package engine

import (
	"bufio"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestParser_Term(t *testing.T) {
	t.Run("atom", func(t *testing.T) {
		t.Run("quoted", func(t *testing.T) {
			t.Run("no escape", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`'abc'.`)), nil)
				a, err := p.Term()
				assert.NoError(t, err)
				assert.Equal(t, Atom("abc"), a)
			})

			t.Run("double single quotes", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`'don''t panic'.`)), nil)
				a, err := p.Term()
				assert.NoError(t, err)
				assert.Equal(t, Atom("don't panic"), a)
			})

			t.Run("backslash at the very end of the line", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`'this is \
a quoted ident'.`)), nil)
				a, err := p.Term()
				assert.NoError(t, err)
				assert.Equal(t, Atom("this is a quoted ident"), a)
			})

			t.Run("alert", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`'\a'.`)), nil)
				a, err := p.Term()
				assert.NoError(t, err)
				assert.Equal(t, Atom("\a"), a)
			})

			t.Run("backspace", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`'\b'.`)), nil)
				a, err := p.Term()
				assert.NoError(t, err)
				assert.Equal(t, Atom("\b"), a)
			})

			t.Run("formfeed", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`'\f'.`)), nil)
				a, err := p.Term()
				assert.NoError(t, err)
				assert.Equal(t, Atom("\f"), a)
			})

			t.Run("newline", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`'\n'.`)), nil)
				a, err := p.Term()
				assert.NoError(t, err)
				assert.Equal(t, Atom("\n"), a)
			})

			t.Run("return", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`'\r'.`)), nil)
				a, err := p.Term()
				assert.NoError(t, err)
				assert.Equal(t, Atom("\r"), a)
			})

			t.Run("tab", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`'\t'.`)), nil)
				a, err := p.Term()
				assert.NoError(t, err)
				assert.Equal(t, Atom("\t"), a)
			})

			t.Run("vertical tab", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`'\v'.`)), nil)
				a, err := p.Term()
				assert.NoError(t, err)
				assert.Equal(t, Atom("\v"), a)
			})

			t.Run("hex code", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`'\xa3\'.`)), nil)
				a, err := p.Term()
				assert.NoError(t, err)
				assert.Equal(t, Atom("£"), a)
			})

			t.Run("oct code", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`'\43\'.`)), nil)
				a, err := p.Term()
				assert.NoError(t, err)
				assert.Equal(t, Atom("#"), a)
			})

			t.Run("backslash", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`'\\'.`)), nil)
				a, err := p.Term()
				assert.NoError(t, err)
				assert.Equal(t, Atom(`\`), a)
			})

			t.Run("single quote", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`'\''.`)), nil)
				a, err := p.Term()
				assert.NoError(t, err)
				assert.Equal(t, Atom(`'`), a)
			})

			t.Run("double quote", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`'\"'.`)), nil)
				a, err := p.Term()
				assert.NoError(t, err)
				assert.Equal(t, Atom(`"`), a)
			})

			t.Run("backquote", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader("'\\`'.")), nil)
				a, err := p.Term()
				assert.NoError(t, err)
				assert.Equal(t, Atom("`"), a)
			})
		})
	})

	t.Run("compound", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			p := newParser(bufio.NewReader(strings.NewReader(`foo(a, b).`)), nil)
			term, err := p.Term()
			assert.NoError(t, err)
			assert.Equal(t, &Compound{
				Functor: "foo",
				Args:    []Term{Atom("a"), Atom("b")},
			}, term)
		})

		t.Run("missing closing parenthesis", func(t *testing.T) {
			p := newParser(bufio.NewReader(strings.NewReader(`foo(a, b.`)), nil)
			_, err := p.Term()
			assert.Error(t, err)
		})

		t.Run("syntax error inside args", func(t *testing.T) {
			p := newParser(bufio.NewReader(strings.NewReader(`foo(a, b c).`)), nil)
			_, err := p.Term()
			assert.Error(t, err)
		})
	})

	t.Run("variable", func(t *testing.T) {
		t.Run("named", func(t *testing.T) {
			p := newParser(bufio.NewReader(strings.NewReader(`X.`)), nil)
			v, err := p.Term()
			assert.NoError(t, err)
			assert.Equal(t, Variable("X"), v)
		})

		t.Run("anonymous", func(t *testing.T) {
			p := newParser(bufio.NewReader(strings.NewReader(`_.`)), nil)
			v, err := p.Term()
			assert.NoError(t, err)
			va, ok := v.(Variable)
			assert.True(t, ok)
			assert.True(t, va.Generated())
		})
	})

	t.Run("fact", func(t *testing.T) {
		p := newParser(bufio.NewReader(strings.NewReader(`
append(nil,L,L).
`)), nil)
		c, err := p.Term()
		assert.NoError(t, err)

		assert.Equal(t, &Compound{
			Functor: "append",
			Args: []Term{
				Atom("nil"),
				Variable("L"),
				Variable("L"),
			},
		}, c)
	})

	t.Run("rule", func(t *testing.T) {
		ops := operators{
			{priority: 1200, specifier: operatorSpecifierXFX, name: `:-`},
		}
		p := newParser(bufio.NewReader(strings.NewReader(`
append(cons(X,L1),L2,cons(X,L3)) :- append(L1,L2,L3).
`)), nil, withOperators(&ops))
		c, err := p.Term()
		assert.NoError(t, err)

		assert.Equal(t, &Compound{
			Functor: ":-",
			Args: []Term{
				&Compound{
					Functor: "append",
					Args: []Term{
						&Compound{
							Functor: "cons",
							Args: []Term{
								Variable("X"),
								Variable("L1"),
							},
						},
						Variable("L2"),
						&Compound{
							Functor: "cons",
							Args: []Term{
								Variable("X"),
								Variable("L3"),
							},
						},
					},
				},
				&Compound{
					Functor: "append",
					Args: []Term{
						Variable("L1"),
						Variable("L2"),
						Variable("L3"),
					},
				},
			},
		}, c)
	})

	t.Run("conjunction", func(t *testing.T) {
		ops := operators{
			{priority: 1200, specifier: operatorSpecifierXFX, name: `:-`},
			{priority: 1000, specifier: operatorSpecifierXFY, name: `,`},
		}
		p := newParser(bufio.NewReader(strings.NewReader(`P, Q :- P, Q.`)), nil, withOperators(&ops))
		c, err := p.Term()
		assert.NoError(t, err)
		assert.Equal(t, &Compound{
			Functor: ":-",
			Args: []Term{
				&Compound{
					Functor: ",",
					Args: []Term{
						Variable("P"),
						Variable("Q"),
					},
				},
				&Compound{
					Functor: ",",
					Args: []Term{
						Variable("P"),
						Variable("Q"),
					},
				},
			},
		}, c)
	})

	t.Run("qualifier", func(t *testing.T) {
		ops := operators{
			{priority: 1000, specifier: operatorSpecifierXFY, name: `,`},
			{priority: 200, specifier: operatorSpecifierXFY, name: `^`},
		}
		p := newParser(bufio.NewReader(strings.NewReader(`bagof(C, A^foo(A, B, C), Cs).`)), nil, withOperators(&ops))
		c, err := p.Term()
		assert.NoError(t, err)
		assert.Equal(t, &Compound{
			Functor: "bagof",
			Args: []Term{
				Variable("C"),
				&Compound{
					Functor: "^",
					Args: []Term{
						Variable("A"),
						&Compound{
							Functor: "foo",
							Args: []Term{
								Variable("A"),
								Variable("B"),
								Variable("C"),
							},
						},
					},
				},
				Variable("Cs"),
			},
		}, c)
	})

	t.Run("multiple qualifiers", func(t *testing.T) {
		ops := operators{
			{priority: 1000, specifier: operatorSpecifierXFY, name: `,`},
			{priority: 200, specifier: operatorSpecifierXFY, name: `^`},
		}
		p := newParser(bufio.NewReader(strings.NewReader(`bagof(C, (A, B)^foo(A, B, C), Cs).`)), nil, withOperators(&ops))
		c, err := p.Term()
		assert.NoError(t, err)
		assert.Equal(t, &Compound{
			Functor: "bagof",
			Args: []Term{
				Variable("C"),
				&Compound{
					Functor: "^",
					Args: []Term{
						&Compound{
							Functor: ",",
							Args: []Term{
								Variable("A"),
								Variable("B"),
							},
						},
						&Compound{
							Functor: "foo",
							Args: []Term{
								Variable("A"),
								Variable("B"),
								Variable("C"),
							},
						},
					},
				},
				Variable("Cs"),
			},
		}, c)
	})

	t.Run("expression", func(t *testing.T) {
		ops := operators{
			{priority: 500, specifier: operatorSpecifierYFX, name: `+`},
			{priority: 400, specifier: operatorSpecifierYFX, name: `*`},
		}
		p := newParser(bufio.NewReader(strings.NewReader(`a + b * c * d + e.`)), nil, withOperators(&ops))
		term, err := p.Term()
		assert.NoError(t, err)
		assert.Equal(t, term, &Compound{
			Functor: "+",
			Args: []Term{
				&Compound{
					Functor: "+",
					Args: []Term{
						Atom("a"),
						&Compound{
							Functor: "*",
							Args: []Term{
								&Compound{
									Functor: "*",
									Args:    []Term{Atom("b"), Atom("c")},
								},
								Atom("d"),
							},
						},
					},
				},
				Atom("e"),
			},
		})
	})

	t.Run("prefix", func(t *testing.T) {
		ops := operators{
			{priority: 200, specifier: operatorSpecifierFY, name: `+`},
		}

		t.Run("ok", func(t *testing.T) {
			p := newParser(bufio.NewReader(strings.NewReader(`+ 1.`)), nil, withOperators(&ops))
			term, err := p.Term()
			assert.NoError(t, err)
			assert.Equal(t, &Compound{
				Functor: `+`,
				Args: []Term{
					Integer(1),
				},
			}, term)
		})

		t.Run("syntax error right after the operator", func(t *testing.T) {
			p := newParser(bufio.NewReader(strings.NewReader(`+ ).`)), nil, withOperators(&ops))
			_, err := p.Term()
			assert.Error(t, err)
		})
	})

	t.Run("parenthesis", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			p := newParser(bufio.NewReader(strings.NewReader(`(foo(a, b)).`)), nil)
			term, err := p.Term()
			assert.NoError(t, err)
			assert.Equal(t, &Compound{
				Functor: "foo",
				Args:    []Term{Atom("a"), Atom("b")},
			}, term)
		})

		t.Run("syntax error inside the parentheses", func(t *testing.T) {
			p := newParser(bufio.NewReader(strings.NewReader(`(foo(a, )).`)), nil)
			_, err := p.Term()
			assert.Error(t, err)
		})

		t.Run("missing closing parenthesis", func(t *testing.T) {
			p := newParser(bufio.NewReader(strings.NewReader(`(foo(a, b).`)), nil)
			_, err := p.Term()
			assert.Error(t, err)
		})
	})

	t.Run("list", func(t *testing.T) {
		ops := operators{
			{priority: 1105, specifier: operatorSpecifierXFY, name: `|`},
		}

		t.Run("ok", func(t *testing.T) {
			t.Run("without bar", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`[a, b, c].`)), nil)
				term, err := p.Term()
				assert.NoError(t, err)

				assert.Equal(t, List(Atom("a"), Atom("b"), Atom("c")), term)
			})

			t.Run("with bar", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`[a, b, c|X].`)), nil, withOperators(&ops))
				term, err := p.Term()
				assert.NoError(t, err)

				// A bar in list is not an operator but a separator.
				assert.Equal(t, ListRest(Variable("X"), Atom("a"), Atom("b"), Atom("c")), term)
			})
		})

		t.Run("missing closing bracket", func(t *testing.T) {
			t.Run("without bar", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`[a, b, c.`)), nil)
				_, err := p.Term()
				assert.Error(t, err)
			})

			t.Run("with bar", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`[a, b, c|X.`)), nil, withOperators(&ops))
				_, err := p.Term()
				assert.Error(t, err)
			})
		})

		t.Run("syntax error inside", func(t *testing.T) {
			t.Run("without bar", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`[a, b, ()].`)), nil)
				_, err := p.Term()
				assert.Error(t, err)
			})

			t.Run("with bar", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`[a, b, c|()].`)), nil, withOperators(&ops))
				_, err := p.Term()
				assert.Error(t, err)
			})
		})
	})

	t.Run("curlyBracketedTerm", func(t *testing.T) {
		ops := operators{
			{priority: 1000, specifier: operatorSpecifierXFY, name: `,`},
		}

		t.Run("ok", func(t *testing.T) {
			p := newParser(bufio.NewReader(strings.NewReader(`{a, b, c}.`)), nil, withOperators(&ops))
			term, err := p.Term()
			assert.NoError(t, err)
			assert.Equal(t, &Compound{
				Functor: "{}",
				Args: []Term{
					&Compound{
						Functor: ",",
						Args: []Term{
							Atom("a"),
							&Compound{
								Functor: ",",
								Args: []Term{
									Atom("b"),
									Atom("c"),
								},
							},
						},
					},
				},
			}, term)
		})

		t.Run("syntax error inside the braces", func(t *testing.T) {
			p := newParser(bufio.NewReader(strings.NewReader(`{a, b, }.`)), nil, withOperators(&ops))
			_, err := p.Term()
			assert.Error(t, err)
		})

		t.Run("missing closing brace", func(t *testing.T) {
			p := newParser(bufio.NewReader(strings.NewReader(`{a, b, c.`)), nil, withOperators(&ops))
			_, err := p.Term()
			assert.Error(t, err)
		})
	})

	t.Run("principal functor", func(t *testing.T) {
		ops := operators{
			{priority: 400, specifier: operatorSpecifierYFX, name: "/"},
		}
		p := newParser(bufio.NewReader(strings.NewReader(`(==)/2.`)), nil, withOperators(&ops))
		term, err := p.Term()
		assert.NoError(t, err)
		assert.Equal(t, &Compound{
			Functor: "/",
			Args: []Term{
				Atom("=="),
				Integer(2),
			},
		}, term)
	})

	t.Run("prefix as an arg", func(t *testing.T) {
		ops := operators{
			{priority: 200, specifier: operatorSpecifierFY, name: "+"},
		}

		t.Run("unary", func(t *testing.T) {
			p := newParser(bufio.NewReader(strings.NewReader(`p(+).`)), nil, withOperators(&ops))
			term, err := p.Term()
			assert.NoError(t, err)
			assert.Equal(t, &Compound{
				Functor: "p",
				Args: []Term{
					Atom("+"),
				},
			}, term)
		})

		t.Run("binary", func(t *testing.T) {
			p := newParser(bufio.NewReader(strings.NewReader(`p(+, a).`)), nil, withOperators(&ops))
			term, err := p.Term()
			assert.NoError(t, err)
			assert.Equal(t, &Compound{
				Functor: "p",
				Args: []Term{
					Atom("+"),
					Atom("a"),
				},
			}, term)
		})
	})

	t.Run("double quotes", func(t *testing.T) {
		ops := operators{
			{priority: 700, specifier: operatorSpecifierXFX, name: `=`},
		}

		t.Run("codes", func(t *testing.T) {
			p := newParser(bufio.NewReader(strings.NewReader(`X = "abc".`)), nil, withOperators(&ops), withDoubleQuotes(doubleQuotesCodes))
			term, err := p.Term()
			assert.NoError(t, err)
			assert.Equal(t, &Compound{
				Functor: "=",
				Args: []Term{
					Variable("X"),
					List(Integer(97), Integer(98), Integer(99)),
				},
			}, term)
		})

		t.Run("chars", func(t *testing.T) {
			p := newParser(bufio.NewReader(strings.NewReader(`X = "abc".`)), nil, withOperators(&ops), withDoubleQuotes(doubleQuotesChars))
			term, err := p.Term()
			assert.NoError(t, err)
			assert.Equal(t, &Compound{
				Functor: "=",
				Args: []Term{
					Variable("X"),
					List(Atom("a"), Atom("b"), Atom("c")),
				},
			}, term)
		})

		t.Run("atom", func(t *testing.T) {
			p := newParser(bufio.NewReader(strings.NewReader(`X = "abc".`)), nil, withOperators(&ops), withDoubleQuotes(doubleQuotesAtom))
			term, err := p.Term()
			assert.NoError(t, err)
			assert.Equal(t, &Compound{
				Functor: "=",
				Args: []Term{
					Variable("X"),
					Atom("abc"),
				},
			}, term)
		})

		t.Run("escape", func(t *testing.T) {
			t.Run("double double quotes", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`"don""t panic".`)), nil, withDoubleQuotes(doubleQuotesAtom))
				a, err := p.Term()
				assert.NoError(t, err)
				assert.Equal(t, Atom("don\"t panic"), a)
			})

			t.Run("backslash at the very end of the line", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`"this is \
a double-quoted string".`)), nil, withDoubleQuotes(doubleQuotesAtom))
				a, err := p.Term()
				assert.NoError(t, err)
				assert.Equal(t, Atom("this is a double-quoted string"), a)
			})

			t.Run("alert", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`"\a".`)), nil, withDoubleQuotes(doubleQuotesAtom))
				a, err := p.Term()
				assert.NoError(t, err)
				assert.Equal(t, Atom("\a"), a)
			})

			t.Run("backspace", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`"\b".`)), nil, withDoubleQuotes(doubleQuotesAtom))
				a, err := p.Term()
				assert.NoError(t, err)
				assert.Equal(t, Atom("\b"), a)
			})

			t.Run("formfeed", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`"\f".`)), nil, withDoubleQuotes(doubleQuotesAtom))
				a, err := p.Term()
				assert.NoError(t, err)
				assert.Equal(t, Atom("\f"), a)
			})

			t.Run("newline", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`"\n".`)), nil, withDoubleQuotes(doubleQuotesAtom))
				a, err := p.Term()
				assert.NoError(t, err)
				assert.Equal(t, Atom("\n"), a)
			})

			t.Run("return", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`"\r".`)), nil, withDoubleQuotes(doubleQuotesAtom))
				a, err := p.Term()
				assert.NoError(t, err)
				assert.Equal(t, Atom("\r"), a)
			})

			t.Run("tab", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`"\t".`)), nil, withDoubleQuotes(doubleQuotesAtom))
				a, err := p.Term()
				assert.NoError(t, err)
				assert.Equal(t, Atom("\t"), a)
			})

			t.Run("vertical tab", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`"\v".`)), nil, withDoubleQuotes(doubleQuotesAtom))
				a, err := p.Term()
				assert.NoError(t, err)
				assert.Equal(t, Atom("\v"), a)
			})

			t.Run("hex code", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`"\xa3\".`)), nil, withDoubleQuotes(doubleQuotesAtom))
				a, err := p.Term()
				assert.NoError(t, err)
				assert.Equal(t, Atom("£"), a)
			})

			t.Run("oct code", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`"\43\".`)), nil, withDoubleQuotes(doubleQuotesAtom))
				a, err := p.Term()
				assert.NoError(t, err)
				assert.Equal(t, Atom("#"), a)
			})

			t.Run("backslash", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`"\\".`)), nil, withDoubleQuotes(doubleQuotesAtom))
				a, err := p.Term()
				assert.NoError(t, err)
				assert.Equal(t, Atom(`\`), a)
			})

			t.Run("single quote", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`"\'".`)), nil, withDoubleQuotes(doubleQuotesAtom))
				a, err := p.Term()
				assert.NoError(t, err)
				assert.Equal(t, Atom(`'`), a)
			})

			t.Run("double quote", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`"\"".`)), nil, withDoubleQuotes(doubleQuotesAtom))
				a, err := p.Term()
				assert.NoError(t, err)
				assert.Equal(t, Atom(`"`), a)
			})

			t.Run("backquote", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader("\"\\`\".")), nil, withDoubleQuotes(doubleQuotesAtom))
				a, err := p.Term()
				assert.NoError(t, err)
				assert.Equal(t, Atom("`"), a)
			})
		})
	})

	t.Run("misc", func(t *testing.T) {
		p := newParser(bufio.NewReader(strings.NewReader(`:-(op(1200, xfx, :-)).`)), nil)
		_, err := p.Term()
		assert.NoError(t, err)
	})
}

func TestParser_Replace(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		p := newParser(bufio.NewReader(strings.NewReader(`[?, ?, ?, ?, ?].`)), nil)
		assert.NoError(t, p.Replace("?", 1.0, 2, "foo", []string{"a", "b", "c"}, &Compound{
			Functor: "f",
			Args:    []Term{Atom("x")},
		}))

		list, err := p.Term()
		assert.NoError(t, err)
		assert.Equal(t, List(Float(1.0), Integer(2), Atom("foo"), List(Atom("a"), Atom("b"), Atom("c")), &Compound{
			Functor: "f",
			Args:    []Term{Atom("x")},
		}), list)
	})

	t.Run("invalid argument", func(t *testing.T) {
		p := newParser(bufio.NewReader(strings.NewReader(`[?].`)), nil)
		assert.Error(t, p.Replace("?", []struct{}{{}}))
	})

	t.Run("too few arguments", func(t *testing.T) {
		p := newParser(bufio.NewReader(strings.NewReader(`[?, ?, ?, ?, ?].`)), nil)
		assert.NoError(t, p.Replace("?", 1.0, 2, "foo", []string{"a", "b", "c"}))

		_, err := p.Term()
		assert.Error(t, err)
	})

	t.Run("too many arguments", func(t *testing.T) {
		p := newParser(bufio.NewReader(strings.NewReader(`[?, ?, ?, ?, ?].`)), nil)
		assert.NoError(t, p.Replace("?", 1.0, 2, "foo", []string{"a", "b", "c"}, &Compound{
			Functor: "f",
			Args:    []Term{Atom("x")},
		}, "extra"))

		_, err := p.Term()
		assert.Error(t, err)
	})
}

func TestParser_Number(t *testing.T) {
	t.Run("extra token", func(t *testing.T) {
		p := newParser(bufio.NewReader(strings.NewReader(`33 three`)), nil)
		_, err := p.Number()
		assert.Error(t, err)
	})

	t.Run("not a number", func(t *testing.T) {
		p := newParser(bufio.NewReader(strings.NewReader(`three`)), nil)
		_, err := p.Number()
		assert.Error(t, err)
	})

	t.Run("integer", func(t *testing.T) {
		t.Run("without sign", func(t *testing.T) {
			p := newParser(bufio.NewReader(strings.NewReader(`33`)), nil)
			n, err := p.Number()
			assert.NoError(t, err)
			assert.Equal(t, Integer(33), n)
		})

		t.Run("with plus", func(t *testing.T) {
			p := newParser(bufio.NewReader(strings.NewReader(`+33`)), nil)
			_, err := p.Number()
			assert.Equal(t, errNotANumber, err)
		})

		t.Run("with minus", func(t *testing.T) {
			t.Run("unquoted", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`-33`)), nil)
				n, err := p.Number()
				assert.NoError(t, err)
				assert.Equal(t, Integer(-33), n)
			})

			t.Run("quoted", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`'-' 33`)), nil)
				n, err := p.Number()
				assert.NoError(t, err)
				assert.Equal(t, Integer(-33), n)
			})
		})

		t.Run("just minus", func(t *testing.T) {
			p := newParser(bufio.NewReader(strings.NewReader(`-`)), nil)
			_, err := p.Number()
			assert.Equal(t, errNotANumber, err)
		})

		t.Run("char", func(t *testing.T) {
			t.Run("without sign", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`0'!`)), nil)
				n, err := p.Number()
				assert.NoError(t, err)
				assert.Equal(t, Integer(33), n)
			})

			t.Run("with plus", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`+0'!`)), nil)
				_, err := p.Number()
				assert.Equal(t, errNotANumber, err)
			})

			t.Run("with minus", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`-0'!`)), nil)
				n, err := p.Number()
				assert.NoError(t, err)
				assert.Equal(t, Integer(-33), n)
			})
		})
	})

	t.Run("float", func(t *testing.T) {
		t.Run("without sign", func(t *testing.T) {
			p := newParser(bufio.NewReader(strings.NewReader(`3.3`)), nil)
			n, err := p.Number()
			assert.NoError(t, err)
			assert.Equal(t, Float(3.3), n)
		})

		t.Run("with plus", func(t *testing.T) {
			p := newParser(bufio.NewReader(strings.NewReader(`+3.3`)), nil)
			_, err := p.Number()
			assert.Equal(t, errNotANumber, err)
		})

		t.Run("with minus", func(t *testing.T) {
			t.Run("unquoted", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`-3.3`)), nil)
				n, err := p.Number()
				assert.NoError(t, err)
				assert.Equal(t, Float(-3.3), n)
			})

			t.Run("quoted", func(t *testing.T) {
				p := newParser(bufio.NewReader(strings.NewReader(`'-' 3.3`)), nil)
				n, err := p.Number()
				assert.NoError(t, err)
				assert.Equal(t, Float(-3.3), n)
			})
		})
	})
}

func TestParser_More(t *testing.T) {
	p := newParser(bufio.NewReader(strings.NewReader(`foo. bar.`)), nil)
	term, err := p.Term()
	assert.NoError(t, err)
	assert.Equal(t, Atom("foo"), term)
	assert.True(t, p.More())
	term, err = p.Term()
	assert.NoError(t, err)
	assert.Equal(t, Atom("bar"), term)
	assert.False(t, p.More())
}
