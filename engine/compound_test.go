package engine

import (
	"errors"
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
			}, WriteTermOptions{}, nil)
			assert.Equal(t, []Token{
				{Kind: TokenBracketL, Val: "["},
				{Kind: TokenIdent, Val: "a"},
				{Kind: TokenComma, Val: ","},
				{Kind: TokenIdent, Val: "b"},
				{Kind: TokenComma, Val: ","},
				{Kind: TokenIdent, Val: "c"},
				{Kind: TokenBracketR, Val: "]"},
			}, ret)
		})

		t.Run("rest", func(t *testing.T) {
			var ret []Token
			ListRest(Atom("rest"), Atom("a"), Atom("b")).Unparse(func(token Token) {
				ret = append(ret, token)
			}, WriteTermOptions{}, nil)
			assert.Equal(t, []Token{
				{Kind: TokenBracketL, Val: "["},
				{Kind: TokenIdent, Val: "a"},
				{Kind: TokenComma, Val: ","},
				{Kind: TokenIdent, Val: "b"},
				{Kind: TokenBar, Val: "|"},
				{Kind: TokenIdent, Val: "rest"},
				{Kind: TokenBracketR, Val: "]"},
			}, ret)
		})
	})

	t.Run("block", func(t *testing.T) {
		var ret []Token
		c := Compound{
			Functor: "{}",
			Args:    []Term{Atom("foo")},
		}
		c.Unparse(func(token Token) {
			ret = append(ret, token)
		}, WriteTermOptions{}, nil)
		assert.Equal(t, []Token{
			{Kind: TokenBraceL, Val: "{"},
			{Kind: TokenIdent, Val: "foo"},
			{Kind: TokenBraceR, Val: "}"},
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
			ops := Operators{
				{Priority: 1200, Specifier: OperatorSpecifierFX, Name: `:-`},
			}

			var tokens []Token
			c.Unparse(func(token Token) {
				tokens = append(tokens, token)
			}, WriteTermOptions{Ops: ops, Priority: 1200}, nil)
			assert.Equal(t, []Token{
				{Kind: TokenGraphic, Val: ":-"},
				{Kind: TokenParenL, Val: "("},
				{Kind: TokenGraphic, Val: ":-"},
				{Kind: TokenIdent, Val: "foo"},
				{Kind: TokenParenR, Val: ")"},
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
			ops := Operators{
				{Priority: 900, Specifier: OperatorSpecifierFY, Name: `\+`},
				{Priority: 200, Specifier: OperatorSpecifierFY, Name: `-`},
			}

			var tokens []Token
			c.Unparse(func(token Token) {
				tokens = append(tokens, token)
			}, WriteTermOptions{Ops: ops, Priority: 1200}, nil)
			assert.Equal(t, []Token{
				{Kind: TokenGraphic, Val: "\\+"},
				{Kind: TokenGraphic, Val: "-"},
				{Kind: TokenParenL, Val: "("},
				{Kind: TokenGraphic, Val: "\\+"},
				{Kind: TokenIdent, Val: "foo"},
				{Kind: TokenParenR, Val: ")"},
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
			ops := Operators{
				{Priority: 1200, Specifier: OperatorSpecifierXF, Name: `-:`},
			}

			var tokens []Token
			c.Unparse(func(token Token) {
				tokens = append(tokens, token)
			}, WriteTermOptions{Ops: ops, Priority: 1200}, nil)
			assert.Equal(t, []Token{
				{Kind: TokenParenL, Val: "("},
				{Kind: TokenIdent, Val: "foo"},
				{Kind: TokenGraphic, Val: "-:"},
				{Kind: TokenParenR, Val: ")"},
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
			ops := Operators{
				{Priority: 900, Specifier: OperatorSpecifierYF, Name: `+/`},
				{Priority: 200, Specifier: OperatorSpecifierYF, Name: `-`},
			}

			var tokens []Token
			c.Unparse(func(token Token) {
				tokens = append(tokens, token)
			}, WriteTermOptions{Ops: ops, Priority: 1200}, nil)
			assert.Equal(t, []Token{
				{Kind: TokenParenL, Val: "("},
				{Kind: TokenIdent, Val: "foo"},
				{Kind: TokenGraphic, Val: "+/"},
				{Kind: TokenParenR, Val: ")"},
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
			ops := Operators{
				{Priority: 1200, Specifier: OperatorSpecifierXFX, Name: `:-`},
			}

			var tokens []Token
			c.Unparse(func(token Token) {
				tokens = append(tokens, token)
			}, WriteTermOptions{Ops: ops, Priority: 1200}, nil)
			assert.Equal(t, []Token{
				{Kind: TokenIdent, Val: "foo"},
				{Kind: TokenGraphic, Val: ":-"},
				{Kind: TokenParenL, Val: "("},
				{Kind: TokenIdent, Val: "bar"},
				{Kind: TokenGraphic, Val: ":-"},
				{Kind: TokenIdent, Val: "baz"},
				{Kind: TokenParenR, Val: ")"},
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
			ops := Operators{
				{Priority: 1200, Specifier: OperatorSpecifierXFY, Name: `;`},
			}

			var tokens []Token
			c.Unparse(func(token Token) {
				tokens = append(tokens, token)
			}, WriteTermOptions{Ops: ops, Priority: 1200}, nil)
			assert.Equal(t, []Token{
				{Kind: TokenParenL, Val: "("},
				{Kind: TokenIdent, Val: "foo"},
				{Kind: TokenIdent, Val: ";"},
				{Kind: TokenIdent, Val: "bar"},
				{Kind: TokenParenR, Val: ")"},
				{Kind: TokenIdent, Val: ";"},
				{Kind: TokenIdent, Val: "baz"},
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

			ops := Operators{
				{Priority: 500, Specifier: OperatorSpecifierYFX, Name: `+`},
				{Priority: 400, Specifier: OperatorSpecifierYFX, Name: `*`},
			}

			var tokens []Token
			c.Unparse(func(token Token) {
				tokens = append(tokens, token)
			}, WriteTermOptions{Ops: ops, Priority: 1200}, nil)
			assert.Equal(t, []Token{
				{Kind: TokenInteger, Val: "2"},
				{Kind: TokenGraphic, Val: "*"},
				{Kind: TokenParenL, Val: "("},
				{Kind: TokenInteger, Val: "2"},
				{Kind: TokenGraphic, Val: "+"},
				{Kind: TokenInteger, Val: "2"},
				{Kind: TokenParenR, Val: ")"},
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
			ops := Operators{
				{Priority: 500, Specifier: OperatorSpecifierYFX, Name: "+"},
				{Priority: 200, Specifier: OperatorSpecifierFY, Name: "-"},
			}

			var tokens []Token
			c.Unparse(func(token Token) {
				tokens = append(tokens, token)
			}, WriteTermOptions{Ops: ops, Priority: 1200}, nil)
			assert.Equal(t, []Token{
				{Kind: TokenInteger, Val: "2"},
				{Kind: TokenGraphic, Val: "+"},
				{Kind: TokenSign, Val: "-"},
				{Kind: TokenInteger, Val: "2"},
			}, tokens)
		})

		t.Run("true", func(t *testing.T) {
			var tokens []Token
			c.Unparse(func(token Token) {
				tokens = append(tokens, token)
			}, WriteTermOptions{Ops: nil, Priority: 1200}, nil)
			assert.Equal(t, []Token{
				{Kind: TokenGraphic, Val: "+"},
				{Kind: TokenParenL, Val: "("},
				{Kind: TokenInteger, Val: "2"},
				{Kind: TokenComma, Val: ","},
				{Kind: TokenSign, Val: "-"},
				{Kind: TokenInteger, Val: "2"},
				{Kind: TokenParenR, Val: ")"},
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
			}, WriteTermOptions{NumberVars: false, Priority: 1200}, nil)
			assert.Equal(t, []Token{
				{Kind: TokenIdent, Val: "f"},
				{Kind: TokenParenL, Val: "("},
				{Kind: TokenIdent, Val: "$VAR"},
				{Kind: TokenParenL, Val: "("},
				{Kind: TokenInteger, Val: "0"},
				{Kind: TokenParenR, Val: ")"},
				{Kind: TokenComma, Val: ","},
				{Kind: TokenIdent, Val: "$VAR"},
				{Kind: TokenParenL, Val: "("},
				{Kind: TokenInteger, Val: "1"},
				{Kind: TokenParenR, Val: ")"},
				{Kind: TokenComma, Val: ","},
				{Kind: TokenIdent, Val: "$VAR"},
				{Kind: TokenParenL, Val: "("},
				{Kind: TokenInteger, Val: "25"},
				{Kind: TokenParenR, Val: ")"},
				{Kind: TokenComma, Val: ","},
				{Kind: TokenIdent, Val: "$VAR"},
				{Kind: TokenParenL, Val: "("},
				{Kind: TokenInteger, Val: "26"},
				{Kind: TokenParenR, Val: ")"},
				{Kind: TokenComma, Val: ","},
				{Kind: TokenIdent, Val: "$VAR"},
				{Kind: TokenParenL, Val: "("},
				{Kind: TokenInteger, Val: "27"},
				{Kind: TokenParenR, Val: ")"},
				{Kind: TokenParenR, Val: ")"},
			}, tokens)
		})

		t.Run("true", func(t *testing.T) {
			var tokens []Token
			c.Unparse(func(token Token) {
				tokens = append(tokens, token)
			}, WriteTermOptions{NumberVars: true, Priority: 1200}, nil)
			assert.Equal(t, []Token{
				{Kind: TokenIdent, Val: "f"},
				{Kind: TokenParenL, Val: "("},
				{Kind: TokenVariable, Val: "A"},
				{Kind: TokenComma, Val: ","},
				{Kind: TokenVariable, Val: "B"},
				{Kind: TokenComma, Val: ","},
				{Kind: TokenVariable, Val: "Z"},
				{Kind: TokenComma, Val: ","},
				{Kind: TokenVariable, Val: "A1"},
				{Kind: TokenComma, Val: ","},
				{Kind: TokenVariable, Val: "B1"},
				{Kind: TokenParenR, Val: ")"},
			}, tokens)
		})
	})
}

func TestSet(t *testing.T) {
	assert.Equal(t, List(), Set())
	assert.Equal(t, List(Atom("a")), Set(Atom("a")))
	assert.Equal(t, List(Atom("a")), Set(Atom("a"), Atom("a"), Atom("a")))
	assert.Equal(t, List(Atom("a"), Atom("b"), Atom("c")), Set(Atom("c"), Atom("b"), Atom("a")))
}

func TestEachList(t *testing.T) {
	t.Run("variable", func(t *testing.T) {
		var ret []Term
		assert.Equal(t, InstantiationError(Variable("X")), EachList(Variable("X"), func(elem Term) error {
			ret = append(ret, elem)
			return nil
		}, nil))
		assert.Empty(t, ret)
	})

	t.Run("atom", func(t *testing.T) {
		var ret []Term
		assert.Equal(t, typeErrorList(Atom("a")), EachList(Atom("a"), func(elem Term) error {
			ret = append(ret, elem)
			return nil
		}, nil))
		assert.Empty(t, ret)
	})

	t.Run("compound", func(t *testing.T) {
		var ret []Term
		assert.Equal(t, typeErrorList(&Compound{
			Functor: "f",
			Args:    []Term{Atom("a")},
		}), EachList(&Compound{
			Functor: "f",
			Args:    []Term{Atom("a")},
		}, func(elem Term) error {
			ret = append(ret, elem)
			return nil
		}, nil))
		assert.Empty(t, ret)
	})

	t.Run("integer", func(t *testing.T) {
		var ret []Term
		assert.Equal(t, typeErrorList(Integer(1)), EachList(Integer(1), func(elem Term) error {
			ret = append(ret, elem)
			return nil
		}, nil))
		assert.Empty(t, ret)
	})

	t.Run("proper list", func(t *testing.T) {
		var ret []Term
		assert.NoError(t, EachList(List(Atom("a"), Atom("b"), Atom("c")), func(elem Term) error {
			ret = append(ret, elem)
			return nil
		}, nil))
		assert.Equal(t, []Term{Atom("a"), Atom("b"), Atom("c")}, ret)
	})

	t.Run("failed callback", func(t *testing.T) {
		errSomething := errors.New("something")

		var ret []Term
		assert.Equal(t, errSomething, EachList(List(Atom("a"), Atom("b"), Atom("c")), func(elem Term) error {
			ret = append(ret, elem)
			return errSomething
		}, nil))
		assert.Equal(t, []Term{Atom("a")}, ret)
	})
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

func TestEachSeq(t *testing.T) {
	t.Run("success", func(t *testing.T) {
		var ret []Term
		assert.NoError(t, EachSeq(Seq(",", Atom("a"), Atom("b"), Atom("c")), ",", func(elem Term) error {
			ret = append(ret, elem)
			return nil
		}, nil))
		assert.Equal(t, []Term{Atom("a"), Atom("b"), Atom("c")}, ret)
	})

	t.Run("failure", func(t *testing.T) {
		errSomething := errors.New("something")

		var ret []Term
		assert.Equal(t, errSomething, EachSeq(Seq(",", Atom("a"), Atom("b"), Atom("c")), ",", func(elem Term) error {
			ret = append(ret, elem)
			return errSomething
		}, nil))
		assert.Equal(t, []Term{Atom("a")}, ret)
	})
}

func TestEach(t *testing.T) {
	t.Run("sequence", func(t *testing.T) {
		var ret []Term
		assert.NoError(t, Each(Seq(",", Atom("a"), Atom("b"), Atom("c")), func(elem Term) error {
			ret = append(ret, elem)
			return nil
		}, nil))
		assert.Equal(t, []Term{Atom("a"), Atom("b"), Atom("c")}, ret)
	})

	t.Run("list", func(t *testing.T) {
		var ret []Term
		assert.NoError(t, Each(List(Atom("a"), Atom("b"), Atom("c")), func(elem Term) error {
			ret = append(ret, elem)
			return nil
		}, nil))
		assert.Equal(t, []Term{Atom("a"), Atom("b"), Atom("c")}, ret)
	})
}
