package engine

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestCompound_Unify(t *testing.T) {
	unit := Compound{
		Functor: "foo",
		Args:    []Term{Atom("bar")},
	}

	t.Run("atom", func(t *testing.T) {
		env, ok := unit.Unify(Atom("foo"), false, nil)
		assert.False(t, ok)
		assert.Nil(t, env)
	})

	t.Run("integer", func(t *testing.T) {
		env, ok := unit.Unify(Integer(1), false, nil)
		assert.False(t, ok)
		assert.Nil(t, env)
	})

	t.Run("variable", func(t *testing.T) {
		t.Run("free", func(t *testing.T) {
			v := Variable("X")
			env, ok := unit.Unify(v, false, nil)
			assert.True(t, ok)
			assert.Equal(t, &unit, env.Resolve(v))
		})
		t.Run("bound to the same value", func(t *testing.T) {
			v := Variable("X")
			env := NewEnv().
				Bind(v, &unit)
			_, ok := unit.Unify(v, false, env)
			assert.True(t, ok)
		})
		t.Run("bound to a different value", func(t *testing.T) {
			v := Variable("X")
			env := NewEnv().
				Bind(v, &Compound{
					Functor: "foo",
					Args:    []Term{Atom("baz")},
				})

			_, ok := unit.Unify(&v, false, env)
			assert.False(t, ok)
		})
	})

	t.Run("compound", func(t *testing.T) {
		_, ok := unit.Unify(&Compound{
			Functor: "foo",
			Args:    []Term{Atom("bar")},
		}, false, nil)
		assert.True(t, ok)
		_, ok = unit.Unify(&Compound{
			Functor: "foo",
			Args:    []Term{Atom("bar"), Atom("baz")},
		}, false, nil)
		assert.False(t, ok)
		_, ok = unit.Unify(&Compound{
			Functor: "baz",
			Args:    []Term{Atom("bar")},
		}, false, nil)
		assert.False(t, ok)
		_, ok = unit.Unify(&Compound{
			Functor: "foo",
			Args:    []Term{Atom("baz")},
		}, false, nil)
		assert.False(t, ok)
		v := Variable("X")
		env, ok := unit.Unify(&Compound{
			Functor: "foo",
			Args:    []Term{v},
		}, false, nil)
		assert.True(t, ok)
		assert.Equal(t, Atom("bar"), env.Resolve(v))
	})
}

func TestCompound_Unparse(t *testing.T) {
	t.Run("list", func(t *testing.T) {
		t.Run("proper", func(t *testing.T) {
			var ret []Token
			List(Atom("a"), Atom("b"), Atom("c")).Unparse(func(token Token) {
				ret = append(ret, token)
			}, nil, WithQuoted(true))
			assert.Equal(t, []Token{
				{Kind: TokenOpenList, Val: "["},
				{Kind: TokenLetterDigit, Val: "a"},
				{Kind: TokenComma, Val: ","},
				{Kind: TokenLetterDigit, Val: "b"},
				{Kind: TokenComma, Val: ","},
				{Kind: TokenLetterDigit, Val: "c"},
				{Kind: TokenCloseList, Val: "]"},
			}, ret)
		})

		t.Run("rest", func(t *testing.T) {
			var ret []Token
			ListRest(Atom("rest"), Atom("a"), Atom("b")).Unparse(func(token Token) {
				ret = append(ret, token)
			}, nil)
			assert.Equal(t, []Token{
				{Kind: TokenOpenList, Val: "["},
				{Kind: TokenLetterDigit, Val: "a"},
				{Kind: TokenComma, Val: ","},
				{Kind: TokenLetterDigit, Val: "b"},
				{Kind: TokenBar, Val: "|"},
				{Kind: TokenLetterDigit, Val: "rest"},
				{Kind: TokenCloseList, Val: "]"},
			}, ret)
		})

		t.Run("circular", func(t *testing.T) {
			v := Variable("L")
			l := ListRest(v, Atom("a"), Atom("b"))
			env := NewEnv().Bind(v, l)

			var ret []Token
			l.Unparse(func(token Token) {
				ret = append(ret, token)
			}, env)
			assert.Equal(t, []Token{
				{Kind: TokenOpenList, Val: "["},
				{Kind: TokenLetterDigit, Val: "a"},
				{Kind: TokenComma, Val: ","},
				{Kind: TokenLetterDigit, Val: "b"},
				{Kind: TokenBar, Val: "|"},
				{Kind: TokenGraphic, Val: "..."},
				{Kind: TokenCloseList, Val: "]"},
			}, ret)
		})
	})

	t.Run("curlyBracketedTerm", func(t *testing.T) {
		var ret []Token
		c := Compound{
			Functor: "{}",
			Args:    []Term{Atom("foo")},
		}
		c.Unparse(func(token Token) {
			ret = append(ret, token)
		}, nil)
		assert.Equal(t, []Token{
			{Kind: TokenOpenCurly, Val: "{"},
			{Kind: TokenLetterDigit, Val: "foo"},
			{Kind: TokenCloseCurly, Val: "}"},
		}, ret)
	})

	t.Run("unary operator", func(t *testing.T) {
		t.Run("FX", func(t *testing.T) {
			c := Compound{
				Functor: ":-",
				Args: []Term{
					&Compound{
						Functor: ":-",
						Args: []Term{
							Atom("foo"),
						},
					},
				},
			}
			ops := operators{
				{priority: 1200, specifier: operatorSpecifierFX, name: `:-`},
			}

			var tokens []Token
			c.Unparse(func(token Token) {
				tokens = append(tokens, token)
			}, nil, withOps(ops), WithPriority(1200))
			assert.Equal(t, []Token{
				{Kind: TokenGraphic, Val: ":-"},
				{Kind: TokenOpen, Val: "("},
				{Kind: TokenGraphic, Val: ":-"},
				{Kind: TokenLetterDigit, Val: "foo"},
				{Kind: TokenClose, Val: ")"},
			}, tokens)
		})

		t.Run("FY", func(t *testing.T) {
			c := Compound{
				Functor: "\\+",
				Args: []Term{
					&Compound{
						Functor: "-",
						Args: []Term{
							&Compound{
								Functor: "\\+",
								Args: []Term{
									Atom("foo"),
								},
							},
						},
					},
				},
			}
			ops := operators{
				{priority: 900, specifier: operatorSpecifierFY, name: `\+`},
				{priority: 200, specifier: operatorSpecifierFY, name: `-`},
			}

			var tokens []Token
			c.Unparse(func(token Token) {
				tokens = append(tokens, token)
			}, nil, withOps(ops), WithPriority(1200))
			assert.Equal(t, []Token{
				{Kind: TokenGraphic, Val: "\\+"},
				{Kind: TokenGraphic, Val: "-"},
				{Kind: TokenOpen, Val: "("},
				{Kind: TokenGraphic, Val: "\\+"},
				{Kind: TokenLetterDigit, Val: "foo"},
				{Kind: TokenClose, Val: ")"},
			}, tokens)
		})

		t.Run("XF", func(t *testing.T) {
			c := Compound{
				Functor: "-:",
				Args: []Term{
					&Compound{
						Functor: "-:",
						Args: []Term{
							Atom("foo"),
						},
					},
				},
			}
			ops := operators{
				{priority: 1200, specifier: operatorSpecifierXF, name: `-:`},
			}

			var tokens []Token
			c.Unparse(func(token Token) {
				tokens = append(tokens, token)
			}, nil, withOps(ops), WithPriority(1200))
			assert.Equal(t, []Token{
				{Kind: TokenOpen, Val: "("},
				{Kind: TokenLetterDigit, Val: "foo"},
				{Kind: TokenGraphic, Val: "-:"},
				{Kind: TokenClose, Val: ")"},
				{Kind: TokenGraphic, Val: "-:"},
			}, tokens)
		})

		t.Run("YF", func(t *testing.T) {
			c := Compound{
				Functor: "+/",
				Args: []Term{
					&Compound{
						Functor: "-",
						Args: []Term{
							&Compound{
								Functor: "+/",
								Args: []Term{
									Atom("foo"),
								},
							},
						},
					},
				},
			}
			ops := operators{
				{priority: 900, specifier: operatorSpecifierYF, name: `+/`},
				{priority: 200, specifier: operatorSpecifierYF, name: `-`},
			}

			var tokens []Token
			c.Unparse(func(token Token) {
				tokens = append(tokens, token)
			}, nil, withOps(ops), WithPriority(1200))
			assert.Equal(t, []Token{
				{Kind: TokenOpen, Val: "("},
				{Kind: TokenLetterDigit, Val: "foo"},
				{Kind: TokenGraphic, Val: "+/"},
				{Kind: TokenClose, Val: ")"},
				{Kind: TokenGraphic, Val: "-"},
				{Kind: TokenGraphic, Val: "+/"},
			}, tokens)
		})
	})

	t.Run("binary operator", func(t *testing.T) {
		t.Run("XFX", func(t *testing.T) {
			c := Compound{
				Functor: ":-",
				Args: []Term{
					Atom("foo"),
					&Compound{
						Functor: ":-",
						Args: []Term{
							Atom("bar"),
							Atom("baz"),
						},
					},
				},
			}
			ops := operators{
				{priority: 1200, specifier: operatorSpecifierXFX, name: `:-`},
			}

			var tokens []Token
			c.Unparse(func(token Token) {
				tokens = append(tokens, token)
			}, nil, withOps(ops), WithPriority(1200))
			assert.Equal(t, []Token{
				{Kind: TokenLetterDigit, Val: "foo"},
				{Kind: TokenGraphic, Val: ":-"},
				{Kind: TokenOpen, Val: "("},
				{Kind: TokenLetterDigit, Val: "bar"},
				{Kind: TokenGraphic, Val: ":-"},
				{Kind: TokenLetterDigit, Val: "baz"},
				{Kind: TokenClose, Val: ")"},
			}, tokens)
		})

		t.Run("XFY", func(t *testing.T) {
			c := Compound{
				Functor: ";",
				Args: []Term{
					&Compound{
						Functor: ";",
						Args: []Term{
							Atom("foo"),
							Atom("bar"),
						},
					},
					Atom("baz"),
				},
			}
			ops := operators{
				{priority: 1200, specifier: operatorSpecifierXFY, name: `;`},
			}

			var tokens []Token
			c.Unparse(func(token Token) {
				tokens = append(tokens, token)
			}, nil, withOps(ops), WithPriority(1200))
			assert.Equal(t, []Token{
				{Kind: TokenOpen, Val: "("},
				{Kind: TokenLetterDigit, Val: "foo"},
				{Kind: TokenSemicolon, Val: ";"},
				{Kind: TokenLetterDigit, Val: "bar"},
				{Kind: TokenClose, Val: ")"},
				{Kind: TokenSemicolon, Val: ";"},
				{Kind: TokenLetterDigit, Val: "baz"},
			}, tokens)
		})

		t.Run("YFX", func(t *testing.T) {
			c := Compound{
				Functor: "*",
				Args: []Term{
					Integer(2),
					&Compound{
						Functor: "+",
						Args: []Term{
							Integer(2),
							Integer(2),
						},
					},
				},
			}

			ops := operators{
				{priority: 500, specifier: operatorSpecifierYFX, name: `+`},
				{priority: 400, specifier: operatorSpecifierYFX, name: `*`},
			}

			var tokens []Token
			c.Unparse(func(token Token) {
				tokens = append(tokens, token)
			}, nil, withOps(ops), WithPriority(1200))
			assert.Equal(t, []Token{
				{Kind: TokenInteger, Val: "2"},
				{Kind: TokenGraphic, Val: "*"},
				{Kind: TokenOpen, Val: "("},
				{Kind: TokenInteger, Val: "2"},
				{Kind: TokenGraphic, Val: "+"},
				{Kind: TokenInteger, Val: "2"},
				{Kind: TokenClose, Val: ")"},
			}, tokens)
		})
	})

	t.Run("ignore_ops", func(t *testing.T) {
		c := Compound{
			Functor: "+",
			Args: []Term{
				Integer(2),
				Integer(-2),
			},
		}

		t.Run("false", func(t *testing.T) {
			ops := operators{
				{priority: 500, specifier: operatorSpecifierYFX, name: "+"},
				{priority: 200, specifier: operatorSpecifierFY, name: "-"},
			}

			var tokens []Token
			c.Unparse(func(token Token) {
				tokens = append(tokens, token)
			}, nil, withOps(ops), WithPriority(1200))
			assert.Equal(t, []Token{
				{Kind: TokenInteger, Val: "2"},
				{Kind: TokenGraphic, Val: "+"},
				{Kind: TokenGraphic, Val: "-"},
				{Kind: TokenInteger, Val: "2"},
			}, tokens)
		})

		t.Run("true", func(t *testing.T) {
			var tokens []Token
			c.Unparse(func(token Token) {
				tokens = append(tokens, token)
			}, nil, withOps(nil), WithPriority(1200))
			assert.Equal(t, []Token{
				{Kind: TokenGraphic, Val: "+"},
				{Kind: TokenOpen, Val: "("},
				{Kind: TokenInteger, Val: "2"},
				{Kind: TokenComma, Val: ","},
				{Kind: TokenGraphic, Val: "-"},
				{Kind: TokenInteger, Val: "2"},
				{Kind: TokenClose, Val: ")"},
			}, tokens)
		})
	})

	t.Run("numbervars", func(t *testing.T) {
		c := Compound{
			Functor: "f",
			Args: []Term{
				&Compound{Functor: "$VAR", Args: []Term{Integer(0)}},
				&Compound{Functor: "$VAR", Args: []Term{Integer(1)}},
				&Compound{Functor: "$VAR", Args: []Term{Integer(25)}},
				&Compound{Functor: "$VAR", Args: []Term{Integer(26)}},
				&Compound{Functor: "$VAR", Args: []Term{Integer(27)}},
			},
		}

		t.Run("false", func(t *testing.T) {
			var tokens []Token
			c.Unparse(func(token Token) {
				tokens = append(tokens, token)
			}, nil, WithNumberVars(false), WithPriority(1200))
			assert.Equal(t, []Token{
				{Kind: TokenLetterDigit, Val: "f"},
				{Kind: TokenOpen, Val: "("},
				{Kind: TokenLetterDigit, Val: "$VAR"},
				{Kind: TokenOpen, Val: "("},
				{Kind: TokenInteger, Val: "0"},
				{Kind: TokenClose, Val: ")"},
				{Kind: TokenComma, Val: ","},
				{Kind: TokenLetterDigit, Val: "$VAR"},
				{Kind: TokenOpen, Val: "("},
				{Kind: TokenInteger, Val: "1"},
				{Kind: TokenClose, Val: ")"},
				{Kind: TokenComma, Val: ","},
				{Kind: TokenLetterDigit, Val: "$VAR"},
				{Kind: TokenOpen, Val: "("},
				{Kind: TokenInteger, Val: "25"},
				{Kind: TokenClose, Val: ")"},
				{Kind: TokenComma, Val: ","},
				{Kind: TokenLetterDigit, Val: "$VAR"},
				{Kind: TokenOpen, Val: "("},
				{Kind: TokenInteger, Val: "26"},
				{Kind: TokenClose, Val: ")"},
				{Kind: TokenComma, Val: ","},
				{Kind: TokenLetterDigit, Val: "$VAR"},
				{Kind: TokenOpen, Val: "("},
				{Kind: TokenInteger, Val: "27"},
				{Kind: TokenClose, Val: ")"},
				{Kind: TokenClose, Val: ")"},
			}, tokens)
		})

		t.Run("true", func(t *testing.T) {
			var tokens []Token
			c.Unparse(func(token Token) {
				tokens = append(tokens, token)
			}, nil, WithNumberVars(true), WithPriority(1200))
			assert.Equal(t, []Token{
				{Kind: TokenLetterDigit, Val: "f"},
				{Kind: TokenOpen, Val: "("},
				{Kind: TokenVariable, Val: "A"},
				{Kind: TokenComma, Val: ","},
				{Kind: TokenVariable, Val: "B"},
				{Kind: TokenComma, Val: ","},
				{Kind: TokenVariable, Val: "Z"},
				{Kind: TokenComma, Val: ","},
				{Kind: TokenVariable, Val: "A1"},
				{Kind: TokenComma, Val: ","},
				{Kind: TokenVariable, Val: "B1"},
				{Kind: TokenClose, Val: ")"},
			}, tokens)
		})
	})
}

func TestEnv_Set(t *testing.T) {
	env := NewEnv()
	assert.Equal(t, List(), env.Set())
	assert.Equal(t, List(Atom("a")), env.Set(Atom("a")))
	assert.Equal(t, List(Atom("a")), env.Set(Atom("a"), Atom("a"), Atom("a")))
	assert.Equal(t, List(Atom("a"), Atom("b"), Atom("c")), env.Set(Atom("c"), Atom("b"), Atom("a")))
}

func TestSeq(t *testing.T) {
	assert.Equal(t, Atom("a"), Seq(",", Atom("a")))
	assert.Equal(t, &Compound{
		Functor: ",",
		Args: []Term{
			Atom("a"),
			Atom("b"),
		},
	}, Seq(",", Atom("a"), Atom("b")))
	assert.Equal(t, &Compound{
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
	}, Seq(",", Atom("a"), Atom("b"), Atom("c")))
}

func TestCompound_Compare(t *testing.T) {
	var m mockTerm
	defer m.AssertExpectations(t)

	assert.Equal(t, int64(-1), (&Compound{Functor: "f"}).Compare(&Compound{Functor: "g"}, nil))
	assert.Equal(t, int64(-1), (&Compound{Functor: "f", Args: make([]Term, 1)}).Compare(&Compound{Functor: "f", Args: make([]Term, 2)}, nil))
	assert.Equal(t, int64(-1), (&Compound{Functor: "f", Args: []Term{Atom("a"), Atom("a")}}).Compare(&Compound{Functor: "f", Args: []Term{Atom("a"), Atom("b")}}, nil))
	assert.Equal(t, int64(0), (&Compound{Functor: "f", Args: []Term{Atom("a"), Atom("b")}}).Compare(&Compound{Functor: "f", Args: []Term{Atom("a"), Atom("b")}}, nil))
	assert.Equal(t, int64(1), (&Compound{Functor: "f", Args: []Term{Atom("a"), Atom("b")}}).Compare(&Compound{Functor: "f", Args: []Term{Atom("a"), Atom("a")}}, nil))
	assert.Equal(t, int64(1), (&Compound{Functor: "f", Args: make([]Term, 2)}).Compare(&Compound{Functor: "f", Args: make([]Term, 1)}, nil))
	assert.Equal(t, int64(1), (&Compound{Functor: "g"}).Compare(&Compound{Functor: "f"}, nil))
	assert.Equal(t, int64(1), (&Compound{Functor: "f"}).Compare(&m, nil))
}
