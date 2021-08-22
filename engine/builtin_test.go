package engine

import (
	"bufio"
	"bytes"
	"errors"
	"fmt"
	"io"
	"io/ioutil"
	"math"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"unicode"

	"github.com/ichiban/prolog/nondet"
	"github.com/ichiban/prolog/term"

	"github.com/stretchr/testify/mock"

	"github.com/stretchr/testify/assert"
)

func TestVM_Call(t *testing.T) {
	var vm VM

	t.Run("undefined atom", func(t *testing.T) {
		env := term.Env{}
		ok, err := vm.Call(term.Atom("foo"), Success, &env).Force()
		assert.Equal(t, existenceErrorProcedure(&term.Compound{
			Functor: "/",
			Args:    []term.Interface{term.Atom("foo"), term.Integer(0)},
		}), err)
		assert.False(t, ok)
	})

	vm.procedures = map[procedureIndicator]procedure{{name: "foo", arity: 0}: clauses{}}

	t.Run("defined atom", func(t *testing.T) {
		env := term.Env{}
		ok, err := vm.Call(term.Atom("foo"), Success, &env).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("undefined compound", func(t *testing.T) {
		ok, err := vm.Call(&term.Compound{Functor: "bar", Args: []term.Interface{term.NewVariable(), term.NewVariable()}}, Success, &term.Env{}).Force()
		assert.Equal(t, existenceErrorProcedure(&term.Compound{
			Functor: "/",
			Args:    []term.Interface{term.Atom("bar"), term.Integer(2)},
		}), err)
		assert.False(t, ok)
	})

	vm.procedures = map[procedureIndicator]procedure{{name: "bar", arity: 2}: clauses{}}

	t.Run("defined compound", func(t *testing.T) {
		ok, err := vm.Call(&term.Compound{Functor: "bar", Args: []term.Interface{term.NewVariable(), term.NewVariable()}}, Success, &term.Env{}).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("variable", func(t *testing.T) {
		env := term.Env{}
		x := term.Variable("X")

		ok, err := vm.Call(x, Success, &env).Force()
		assert.Equal(t, existenceErrorProcedure(&term.Compound{
			Functor: "/",
			Args:    []term.Interface{term.Atom("call"), term.Integer(1)},
		}), err)
		assert.False(t, ok)
	})

	t.Run("not callable", func(t *testing.T) {
		env := term.Env{}
		ok, err := vm.Call(term.Integer(0), Success, &env).Force()
		assert.Equal(t, typeErrorCallable(term.Integer(0)), err)
		assert.False(t, ok)
	})
}

func TestUnify(t *testing.T) {
	t.Run("unifiable", func(t *testing.T) {
		env := term.Env{}
		x := term.Variable("X")
		ok, err := Unify(x, &term.Compound{
			Functor: "f",
			Args:    []term.Interface{term.Atom("a")},
		}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, &term.Compound{
			Functor: "f",
			Args:    []term.Interface{term.Atom("a")},
		}, env.Resolve(x))
	})

	t.Run("not unifiable", func(t *testing.T) {
		env := term.Env{}
		ok, err := Unify(term.Atom("a"), &term.Compound{
			Functor: "f",
			Args:    []term.Interface{term.Atom("a")},
		}, Success, &env).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("loop", func(t *testing.T) {
		env := term.Env{}
		x := term.Variable("X")
		ok, err := Unify(x, &term.Compound{
			Functor: "f",
			Args:    []term.Interface{x},
		}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})
}

func TestUnifyWithOccursCheck(t *testing.T) {
	t.Run("unifiable", func(t *testing.T) {
		env := term.Env{}
		x := term.Variable("X")
		ok, err := UnifyWithOccursCheck(x, &term.Compound{
			Functor: "f",
			Args:    []term.Interface{term.Atom("a")},
		}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, &term.Compound{
			Functor: "f",
			Args:    []term.Interface{term.Atom("a")},
		}, env.Resolve(x))
	})

	t.Run("not unifiable", func(t *testing.T) {
		env := term.Env{}
		ok, err := UnifyWithOccursCheck(term.Atom("a"), &term.Compound{
			Functor: "f",
			Args:    []term.Interface{term.Atom("a")},
		}, Success, &env).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("loop", func(t *testing.T) {
		env := term.Env{}
		x := term.Variable("X")
		ok, err := UnifyWithOccursCheck(x, &term.Compound{
			Functor: "f",
			Args:    []term.Interface{x},
		}, Success, &env).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestTypeVar(t *testing.T) {
	t.Run("var", func(t *testing.T) {
		env := term.Env{}
		ok, err := TypeVar(term.NewVariable(), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not var", func(t *testing.T) {
		env := term.Env{}
		ok, err := TypeVar(term.Atom("foo"), Success, &env).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestTypeFloat(t *testing.T) {
	t.Run("float", func(t *testing.T) {
		env := term.Env{}
		ok, err := TypeFloat(term.Float(1.0), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not float", func(t *testing.T) {
		env := term.Env{}
		ok, err := TypeFloat(term.Atom("foo"), Success, &env).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestTypeInteger(t *testing.T) {
	t.Run("integer", func(t *testing.T) {
		env := term.Env{}
		ok, err := TypeInteger(term.Integer(1), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not integer", func(t *testing.T) {
		env := term.Env{}
		ok, err := TypeInteger(term.Atom("foo"), Success, &env).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestTypeAtom(t *testing.T) {
	t.Run("atom", func(t *testing.T) {
		env := term.Env{}
		ok, err := TypeAtom(term.Atom("foo"), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not atom", func(t *testing.T) {
		env := term.Env{}
		ok, err := TypeAtom(term.Integer(1), Success, &env).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestTypeCompound(t *testing.T) {
	t.Run("compound", func(t *testing.T) {
		env := term.Env{}
		ok, err := TypeCompound(&term.Compound{
			Functor: "foo",
			Args:    []term.Interface{term.Atom("a")},
		}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not compound", func(t *testing.T) {
		env := term.Env{}
		ok, err := TypeCompound(term.Atom("foo"), Success, &env).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestFunctor(t *testing.T) {
	t.Run("term is instantiated", func(t *testing.T) {
		t.Run("float", func(t *testing.T) {
			env := term.Env{}
			name, arity := term.Variable("Name"), term.Variable("Arity")
			ok, err := Functor(term.Float(2.0), name, arity, func(env term.Env) *nondet.Promise {
				assert.Equal(t, term.Float(2.0), env.Resolve(name))
				assert.Equal(t, term.Integer(0), env.Resolve(arity))
				return nondet.Bool(true)
			}, &env).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("integer", func(t *testing.T) {
			env := term.Env{}
			name, arity := term.NewVariable(), term.NewVariable()
			ok, err := Functor(term.Integer(2), name, arity, func(env term.Env) *nondet.Promise {
				assert.Equal(t, term.Integer(2), env.Resolve(name))
				assert.Equal(t, term.Integer(0), env.Resolve(arity))
				return nondet.Bool(true)
			}, &env).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("atom", func(t *testing.T) {
			env := term.Env{}
			name, arity := term.NewVariable(), term.NewVariable()
			ok, err := Functor(term.Atom("foo"), name, arity, func(env term.Env) *nondet.Promise {
				assert.Equal(t, term.Atom("foo"), env.Resolve(name))
				assert.Equal(t, term.Integer(0), env.Resolve(arity))
				return nondet.Bool(true)
			}, &env).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("compound", func(t *testing.T) {
			env := term.Env{}
			name, arity := term.NewVariable(), term.NewVariable()
			ok, err := Functor(&term.Compound{
				Functor: "f",
				Args:    []term.Interface{term.Atom("a"), term.Atom("b"), term.Atom("c")},
			}, name, arity, func(env term.Env) *nondet.Promise {
				assert.Equal(t, term.Atom("f"), env.Resolve(name))
				assert.Equal(t, term.Integer(3), env.Resolve(arity))
				return nondet.Bool(true)
			}, &env).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("term is not instantiated", func(t *testing.T) {
		t.Run("atom", func(t *testing.T) {
			env := term.Env{}
			v := term.NewVariable()
			ok, err := Functor(v, term.Atom("foo"), term.Integer(0), Success, &env).Force()
			assert.NoError(t, err)
			assert.True(t, ok)

			assert.Equal(t, term.Atom("foo"), env.Resolve(v))
		})

		t.Run("compound", func(t *testing.T) {
			env := term.Env{}
			v := term.NewVariable()
			ok, err := Functor(v, term.Atom("f"), term.Integer(2), func(env term.Env) *nondet.Promise {
				c, ok := env.Resolve(v).(*term.Compound)
				assert.True(t, ok)
				assert.Equal(t, term.Atom("f"), c.Functor)
				assert.Len(t, c.Args, 2)
				assert.True(t, c.Args[0].(term.Variable).Anonymous())
				assert.True(t, c.Args[1].(term.Variable).Anonymous())
				return nondet.Bool(true)
			}, &env).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("name is not an atom", func(t *testing.T) {
			env := term.Env{}
			ok, err := Functor(term.NewVariable(), term.Integer(0), term.Integer(2), Success, &env).Force()
			assert.Equal(t, &Exception{
				Term: &term.Compound{
					Functor: "error",
					Args: []term.Interface{
						&term.Compound{
							Functor: "type_error",
							Args: []term.Interface{
								term.Atom("atom"),
								term.Integer(0),
							},
						},
						term.Atom("0 is not an atom."),
					},
				},
			}, err)
			assert.False(t, ok)
		})

		t.Run("arity is not an integer", func(t *testing.T) {
			env := term.Env{}
			ok, err := Functor(term.NewVariable(), term.Atom("f"), term.Float(2.0), Success, &env).Force()
			assert.Equal(t, &Exception{
				Term: &term.Compound{
					Functor: "error",
					Args: []term.Interface{
						&term.Compound{
							Functor: "type_error",
							Args: []term.Interface{
								term.Atom("integer"),
								term.Float(2.0),
							},
						},
						term.Atom("2 is not an integer."), // TODO: should it be 2.0?
					},
				},
			}, err)
			assert.False(t, ok)
		})

		t.Run("arity is negative", func(t *testing.T) {
			env := term.Env{}
			ok, err := Functor(term.NewVariable(), term.Atom("f"), term.Integer(-2), Success, &env).Force()
			assert.Equal(t, &Exception{
				Term: &term.Compound{
					Functor: "error",
					Args: []term.Interface{
						&term.Compound{
							Functor: "domain_error",
							Args: []term.Interface{
								term.Atom("not_less_than_zero"),
								term.Integer(-2),
							},
						},
						term.Atom("-2 is less than zero."),
					},
				},
			}, err)
			assert.False(t, ok)
		})
	})
}

func TestArg(t *testing.T) {
	t.Run("term is not a compound", func(t *testing.T) {
		env := term.Env{}
		ok, err := Arg(term.NewVariable(), term.Atom("foo"), term.NewVariable(), Success, &env).Force()
		assert.Equal(t, typeErrorCompound(term.Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("nth is a variable", func(t *testing.T) {
		var (
			env = term.Env{}
			nth = term.NewVariable()
			c   int
		)
		ok, err := Arg(nth, &term.Compound{
			Functor: "f",
			Args:    []term.Interface{term.Atom("a"), term.Atom("b"), term.Atom("a")},
		}, term.Atom("a"), func(env term.Env) *nondet.Promise {
			switch c {
			case 0:
				assert.Equal(t, term.Integer(1), env.Resolve(nth))
			case 1:
				assert.Equal(t, term.Integer(3), env.Resolve(nth))
			default:
				assert.Fail(t, "unreachable")
			}
			c++
			return nondet.Bool(false)
		}, &env).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("nth is an integer", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			env := term.Env{}
			ok, err := Arg(term.Integer(2), &term.Compound{
				Functor: "f",
				Args:    []term.Interface{term.Atom("a"), term.Atom("b"), term.Atom("c")},
			}, term.Atom("b"), Success, &env).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("ng", func(t *testing.T) {
			env := term.Env{}
			ok, err := Arg(term.Integer(4), &term.Compound{
				Functor: "f",
				Args:    []term.Interface{term.Atom("a"), term.Atom("b"), term.Atom("c")},
			}, term.Atom("b"), Success, &env).Force()
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("negative", func(t *testing.T) {
			env := term.Env{}
			ok, err := Arg(term.Integer(-2), &term.Compound{
				Functor: "f",
				Args:    []term.Interface{term.Atom("a"), term.Atom("b"), term.Atom("c")},
			}, term.Atom("b"), Success, &env).Force()
			assert.Equal(t, domainErrorNotLessThanZero(term.Integer(-2)), err)
			assert.False(t, ok)
		})
	})

	t.Run("nth is neither a variable nor an integer", func(t *testing.T) {
		env := term.Env{}
		ok, err := Arg(term.Atom("foo"), &term.Compound{
			Functor: "f",
			Args:    []term.Interface{term.Atom("a"), term.Atom("b"), term.Atom("c")},
		}, term.Atom("b"), Success, &env).Force()
		assert.Equal(t, typeErrorInteger(term.Atom("foo")), err)
		assert.False(t, ok)
	})
}

func TestUniv(t *testing.T) {
	t.Run("term is a variable", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			env := term.Env{}
			v := term.NewVariable()
			ok, err := Univ(v, term.List(term.Atom("f"), term.Atom("a"), term.Atom("b")), func(env term.Env) *nondet.Promise {
				assert.Equal(t, &term.Compound{
					Functor: "f",
					Args:    []term.Interface{term.Atom("a"), term.Atom("b")},
				}, env.Resolve(v))
				return nondet.Bool(true)
			}, &env).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("list is empty", func(t *testing.T) {
			env := term.Env{}
			v := term.NewVariable()
			ok, err := Univ(v, term.List(), Success, &env).Force()
			assert.Equal(t, domainErrorNotEmptyList(term.Atom("[]")), err)
			assert.False(t, ok)
		})

		t.Run("list is not a list", func(t *testing.T) {
			env := term.Env{}
			v := term.NewVariable()
			ok, err := Univ(v, term.Atom("list"), Success, &env).Force()
			assert.Equal(t, typeErrorList(term.Atom("list")), err)
			assert.False(t, ok)
		})

		t.Run("list's first element is not an atom", func(t *testing.T) {
			env := term.Env{}
			v := term.NewVariable()
			ok, err := Univ(v, term.List(term.Integer(0), term.Atom("a"), term.Atom("b")), Success, &env).Force()
			assert.Equal(t, typeErrorAtom(term.Integer(0)), err)
			assert.False(t, ok)
		})

		t.Run("list is not fully instantiated", func(t *testing.T) {
			env := term.Env{}
			v, rest := term.NewVariable(), term.Variable("Rest")
			ok, err := Univ(v, term.ListRest(rest, term.Atom("f"), term.Atom("a"), term.Atom("b")), Success, &env).Force()
			assert.Equal(t, instantiationError(term.ListRest(rest, term.Atom("a"), term.Atom("b"))), err)
			assert.False(t, ok)
		})
	})

	t.Run("term is a compound", func(t *testing.T) {
		env := term.Env{}
		ok, err := Univ(&term.Compound{
			Functor: "f",
			Args:    []term.Interface{term.Atom("a"), term.Atom("b")},
		}, term.List(term.Atom("f"), term.Atom("a"), term.Atom("b")), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("term is neither a variable nor a compound", func(t *testing.T) {
		env := term.Env{}
		ok, err := Univ(term.Atom("foo"), term.List(term.Atom("foo")), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})
}

func TestCopyTerm(t *testing.T) {
	in := term.Variable("In")
	out := term.Variable("Out")
	env := term.Env{
		{
			Variable: in,
			Value:    term.Atom("a"),
		},
	}
	ok, err := CopyTerm(in, out, Success, &env).Force()
	assert.NoError(t, err)
	assert.True(t, ok)
	assert.Equal(t, term.Atom("a"), env.Resolve(out))
}

func TestVM_Op(t *testing.T) {
	t.Run("insert", func(t *testing.T) {
		env := term.Env{}
		vm := VM{
			Operators: term.Operators{
				{
					Priority:  900,
					Specifier: "xfx",
					Name:      "+++",
				},
				{
					Priority:  1100,
					Specifier: "xfx",
					Name:      "+",
				},
			},
		}
		ok, err := vm.Op(term.Integer(1000), term.Atom("xfx"), term.Atom("++"), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, term.Operators{
			{
				Priority:  900,
				Specifier: "xfx",
				Name:      "+++",
			},
			{
				Priority:  1000,
				Specifier: "xfx",
				Name:      "++",
			},
			{
				Priority:  1100,
				Specifier: "xfx",
				Name:      "+",
			},
		}, vm.Operators)
	})

	t.Run("remove", func(t *testing.T) {
		env := term.Env{}
		vm := VM{
			Operators: term.Operators{
				{
					Priority:  900,
					Specifier: "xfx",
					Name:      "+++",
				},
				{
					Priority:  1000,
					Specifier: "xfx",
					Name:      "++",
				},
				{
					Priority:  1100,
					Specifier: "xfx",
					Name:      "+",
				},
			},
		}
		ok, err := vm.Op(term.Integer(0), term.Atom("xfx"), term.Atom("++"), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, term.Operators{
			{
				Priority:  900,
				Specifier: "xfx",
				Name:      "+++",
			},
			{
				Priority:  1100,
				Specifier: "xfx",
				Name:      "+",
			},
		}, vm.Operators)
	})

	t.Run("priority is not an integer", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.Op(term.Atom("foo"), term.Atom("xfx"), term.Atom("+"), Success, &env).Force()
		assert.Equal(t, typeErrorInteger(term.Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("priority is negative", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.Op(term.Integer(-1), term.Atom("xfx"), term.Atom("+"), Success, &env).Force()
		assert.Equal(t, domainErrorOperatorPriority(term.Integer(-1)), err)
		assert.False(t, ok)
	})

	t.Run("priority is more than 1200", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.Op(term.Integer(1201), term.Atom("xfx"), term.Atom("+"), Success, &env).Force()
		assert.Equal(t, domainErrorOperatorPriority(term.Integer(1201)), err)
		assert.False(t, ok)
	})

	t.Run("specifier is not an atom", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.Op(term.Integer(1000), term.Integer(0), term.Atom("+"), Success, &env).Force()
		assert.Equal(t, typeErrorAtom(term.Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("specifier is not a valid operator specifier", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.Op(term.Integer(1000), term.Atom("foo"), term.Atom("+"), Success, &env).Force()
		assert.Equal(t, domainErrorOperatorSpecifier(term.Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("operator is not an atom", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.Op(term.Integer(1000), term.Atom("xfx"), term.Integer(0), Success, &env).Force()
		assert.Equal(t, typeErrorAtom(term.Integer(0)), err)
		assert.False(t, ok)
	})
}

func TestVM_CurrentOp(t *testing.T) {
	vm := VM{
		Operators: term.Operators{
			{
				Priority:  900,
				Specifier: "xfx",
				Name:      "+++",
			},
			{
				Priority:  1000,
				Specifier: "xfx",
				Name:      "++",
			},
			{
				Priority:  1100,
				Specifier: "xfx",
				Name:      "+",
			},
		},
	}

	t.Run("single solution", func(t *testing.T) {
		env := term.Env{}
		ok, err := vm.CurrentOp(term.Integer(1100), term.Atom("xfx"), term.Atom("+"), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("multiple solutions", func(t *testing.T) {
		var (
			env                           = term.Env{}
			priority, specifier, operator = term.Variable("Priority"), term.Variable("Specifier"), term.Variable("Operator")
			c                             int
		)
		ok, err := vm.CurrentOp(priority, specifier, operator, func(env term.Env) *nondet.Promise {
			switch c {
			case 0:
				assert.Equal(t, term.Integer(900), env.Resolve(priority))
				assert.Equal(t, term.Atom("xfx"), env.Resolve(specifier))
				assert.Equal(t, term.Atom("+++"), env.Resolve(operator))
			case 1:
				assert.Equal(t, term.Integer(1000), env.Resolve(priority))
				assert.Equal(t, term.Atom("xfx"), env.Resolve(specifier))
				assert.Equal(t, term.Atom("++"), env.Resolve(operator))
			case 2:
				assert.Equal(t, term.Integer(1100), env.Resolve(priority))
				assert.Equal(t, term.Atom("xfx"), env.Resolve(specifier))
				assert.Equal(t, term.Atom("+"), env.Resolve(operator))
			default:
				assert.Fail(t, "unreachable")
			}
			c++
			return nondet.Bool(false)
		}, &env).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("priority is not an operator priority", func(t *testing.T) {
		t.Run("priority is not an integer", func(t *testing.T) {
			env := term.Env{}
			ok, err := vm.CurrentOp(term.Atom("foo"), term.Atom("xfx"), term.Atom("+"), Success, &env).Force()
			assert.Equal(t, domainErrorOperatorPriority(term.Atom("foo")), err)
			assert.False(t, ok)
		})

		t.Run("priority is negative", func(t *testing.T) {
			env := term.Env{}
			ok, err := vm.CurrentOp(term.Integer(-1), term.Atom("xfx"), term.Atom("+"), Success, &env).Force()
			assert.Equal(t, domainErrorOperatorPriority(term.Integer(-1)), err)
			assert.False(t, ok)
		})
	})

	t.Run("specifier is not an operator specifier", func(t *testing.T) {
		t.Run("specifier is not an atom", func(t *testing.T) {
			env := term.Env{}
			ok, err := vm.CurrentOp(term.Integer(1100), term.Integer(0), term.Atom("+"), Success, &env).Force()
			assert.Equal(t, domainErrorOperatorSpecifier(term.Integer(0)), err)
			assert.False(t, ok)
		})

		t.Run("specifier is a non-specifier atom", func(t *testing.T) {
			env := term.Env{}
			ok, err := vm.CurrentOp(term.Integer(1100), term.Atom("foo"), term.Atom("+"), Success, &env).Force()
			assert.Equal(t, domainErrorOperatorSpecifier(term.Atom("foo")), err)
			assert.False(t, ok)
		})
	})

	t.Run("operator is not an atom", func(t *testing.T) {
		env := term.Env{}
		ok, err := vm.CurrentOp(term.Integer(1100), term.Atom("xfx"), term.Integer(0), Success, &env).Force()
		assert.Equal(t, typeErrorAtom(term.Integer(0)), err)
		assert.False(t, ok)
	})
}

func TestRepeat(t *testing.T) {
	env := term.Env{}
	c := 3
	ok, err := Repeat(func(env term.Env) *nondet.Promise {
		c--
		return nondet.Bool(c == 0)
	}, &env).Force()
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Repeat(func(env term.Env) *nondet.Promise {
		return nondet.Error(errors.New(""))
	}, &env).Force()
	assert.Error(t, err)
	assert.False(t, ok)
}

func TestVM_BagOf(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		vm := VM{
			procedures: map[procedureIndicator]procedure{
				{name: "foo", arity: 3}: clauses{
					{xrTable: []term.Interface{term.Atom("a"), term.Atom("b"), term.Atom("c")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opConst, operand: 1},
						{opcode: opConst, operand: 2},
						{opcode: opExit},
					}},
					{xrTable: []term.Interface{term.Atom("a"), term.Atom("b"), term.Atom("d")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opConst, operand: 1},
						{opcode: opConst, operand: 2},
						{opcode: opExit},
					}},
					{xrTable: []term.Interface{term.Atom("b"), term.Atom("c"), term.Atom("e")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opConst, operand: 1},
						{opcode: opConst, operand: 2},
						{opcode: opExit},
					}},
					{xrTable: []term.Interface{term.Atom("b"), term.Atom("c"), term.Atom("f")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opConst, operand: 1},
						{opcode: opConst, operand: 2},
						{opcode: opExit},
					}},
					{xrTable: []term.Interface{term.Atom("c"), term.Atom("c"), term.Atom("g")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opConst, operand: 1},
						{opcode: opConst, operand: 2},
						{opcode: opExit},
					}},
				},
			},
		}

		t.Run("without qualifier", func(t *testing.T) {
			var (
				env         = term.Env{}
				count       int
				a, b, c, cs = term.Variable("A"), term.Variable("B"), term.Variable("C"), term.Variable("Cs")
			)
			ok, err := vm.BagOf(c, &term.Compound{
				Functor: "foo",
				Args:    []term.Interface{a, b, c},
			}, cs, func(env term.Env) *nondet.Promise {
				switch count {
				case 0:
					assert.Equal(t, term.Atom("a"), env.Resolve(a))
					assert.Equal(t, term.Atom("b"), env.Resolve(b))
					assert.Equal(t, c, env.Resolve(c))
					assert.True(t, term.List(term.Atom("c"), term.Atom("d")).Unify(cs, false, &env))
				case 1:
					assert.Equal(t, term.Atom("b"), env.Resolve(a))
					assert.Equal(t, term.Atom("c"), env.Resolve(b))
					assert.Equal(t, c, env.Resolve(c))
					assert.True(t, term.List(term.Atom("e"), term.Atom("f")).Unify(cs, false, &env))
				case 2:
					assert.Equal(t, term.Atom("c"), env.Resolve(a))
					assert.Equal(t, term.Atom("c"), env.Resolve(b))
					assert.Equal(t, c, env.Resolve(c))
					assert.True(t, term.List(term.Atom("g")).Unify(cs, false, &env))
				default:
					assert.Fail(t, "unreachable")
				}
				count++
				return nondet.Bool(false)
			}, &env).Force()
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("with qualifier", func(t *testing.T) {
			var (
				env         = term.Env{}
				count       int
				a, b, c, cs = term.Variable("A"), term.Variable("B"), term.Variable("C"), term.Variable("Cs")
			)
			ok, err := vm.BagOf(c, &term.Compound{
				Functor: "^",
				Args: []term.Interface{a, &term.Compound{
					Functor: "foo",
					Args:    []term.Interface{a, b, c},
				}},
			}, cs, func(env term.Env) *nondet.Promise {
				switch count {
				case 0:
					assert.True(t, env.Resolve(a).(term.Variable).Anonymous())
					assert.Equal(t, term.Atom("b"), env.Resolve(b))
					assert.Equal(t, c, env.Resolve(c))
					assert.True(t, term.List(term.Atom("c"), term.Atom("d")).Unify(cs, false, &env))
				case 1:
					assert.True(t, env.Resolve(a).(term.Variable).Anonymous())
					assert.Equal(t, term.Atom("c"), env.Resolve(b))
					assert.Equal(t, c, env.Resolve(c))
					assert.True(t, term.List(term.Atom("e"), term.Atom("f"), term.Atom("g")).Unify(cs, false, &env))
				default:
					assert.Fail(t, "unreachable")
				}
				count++
				return nondet.Bool(false)
			}, &env).Force()
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("with multiple qualifiers", func(t *testing.T) {
			var (
				env         = term.Env{}
				count       int
				a, b, c, cs = term.Variable("A"), term.Variable("B"), term.Variable("C"), term.Variable("Cs")
			)
			ok, err := vm.BagOf(c, &term.Compound{
				Functor: "^",
				Args: []term.Interface{
					&term.Compound{
						Functor: ",",
						Args:    []term.Interface{a, b},
					},
					&term.Compound{
						Functor: "foo",
						Args:    []term.Interface{a, b, c},
					},
				},
			}, cs, func(env term.Env) *nondet.Promise {
				switch count {
				case 0:
					assert.Equal(t, a, env.Resolve(a))
					assert.Equal(t, b, env.Resolve(b))
					assert.Equal(t, c, env.Resolve(c))
					assert.True(t, term.List(term.Atom("c"), term.Atom("d"), term.Atom("e"), term.Atom("f"), term.Atom("g")).Unify(cs, false, &env))
				default:
					assert.Fail(t, "unreachable")
				}
				count++
				return nondet.Bool(false)
			}, &env).Force()
			assert.NoError(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("goal is a variable", func(t *testing.T) {
		env := term.Env{}
		goal := term.Variable("Goal")

		var vm VM
		ok, err := vm.BagOf(term.NewVariable(), goal, term.NewVariable(), Success, &env).Force()
		assert.Equal(t, instantiationError(&goal), err)
		assert.False(t, ok)
	})

	t.Run("goal is neither a variable nor a callable term", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.BagOf(term.NewVariable(), term.Integer(0), term.NewVariable(), Success, &env).Force()
		assert.Equal(t, typeErrorCallable(term.Integer(0)), err)
		assert.False(t, ok)
	})
}

func TestSetOf(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		vm := VM{
			procedures: map[procedureIndicator]procedure{
				{name: "foo", arity: 3}: clauses{
					{xrTable: []term.Interface{term.Atom("a"), term.Atom("b"), term.Atom("c")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opConst, operand: 1},
						{opcode: opConst, operand: 2},
						{opcode: opExit},
					}},
					{xrTable: []term.Interface{term.Atom("a"), term.Atom("b"), term.Atom("d")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opConst, operand: 1},
						{opcode: opConst, operand: 2},
						{opcode: opExit},
					}},
					{xrTable: []term.Interface{term.Atom("a"), term.Atom("b"), term.Atom("c")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opConst, operand: 1},
						{opcode: opConst, operand: 2},
						{opcode: opExit},
					}},
					{xrTable: []term.Interface{term.Atom("b"), term.Atom("c"), term.Atom("e")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opConst, operand: 1},
						{opcode: opConst, operand: 2},
						{opcode: opExit},
					}},
					{xrTable: []term.Interface{term.Atom("b"), term.Atom("c"), term.Atom("f")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opConst, operand: 1},
						{opcode: opConst, operand: 2},
						{opcode: opExit},
					}},
					{xrTable: []term.Interface{term.Atom("b"), term.Atom("c"), term.Atom("e")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opConst, operand: 1},
						{opcode: opConst, operand: 2},
						{opcode: opExit},
					}},
					{xrTable: []term.Interface{term.Atom("c"), term.Atom("c"), term.Atom("g")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opConst, operand: 1},
						{opcode: opConst, operand: 2},
						{opcode: opExit},
					}},
					{xrTable: []term.Interface{term.Atom("c"), term.Atom("c"), term.Atom("g")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opConst, operand: 1},
						{opcode: opConst, operand: 2},
						{opcode: opExit},
					}},
				},
			},
		}

		t.Run("without qualifier", func(t *testing.T) {
			var (
				env         = term.Env{}
				count       int
				a, b, c, cs = term.Variable("A"), term.Variable("B"), term.Variable("C"), term.Variable("Cs")
			)
			ok, err := vm.SetOf(c, &term.Compound{
				Functor: "foo",
				Args:    []term.Interface{a, b, c},
			}, cs, func(env term.Env) *nondet.Promise {
				switch count {
				case 0:
					assert.Equal(t, term.Atom("a"), env.Resolve(a))
					assert.Equal(t, term.Atom("b"), env.Resolve(b))
					assert.Equal(t, c, env.Resolve(c))
					assert.Equal(t, term.List(term.Atom("c"), term.Atom("d")), env.Resolve(cs))
					assert.True(t, term.List(term.Atom("c"), term.Atom("d")).Unify(cs, false, &env))
				case 1:
					assert.Equal(t, term.Atom("b"), env.Resolve(a))
					assert.Equal(t, term.Atom("c"), env.Resolve(b))
					assert.Equal(t, c, env.Resolve(c))
					assert.True(t, term.List(term.Atom("e"), term.Atom("f")).Unify(cs, false, &env))
				case 2:
					assert.Equal(t, term.Atom("c"), env.Resolve(a))
					assert.Equal(t, term.Atom("c"), env.Resolve(b))
					assert.Equal(t, c, env.Resolve(c))
					assert.True(t, term.List(term.Atom("g")).Unify(cs, false, &env))
				default:
					assert.Fail(t, "unreachable")
				}
				count++
				return nondet.Bool(false)
			}, &env).Force()
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("with qualifier", func(t *testing.T) {
			var (
				env         = term.Env{}
				count       int
				a, b, c, cs = term.Variable("A"), term.Variable("B"), term.Variable("C"), term.Variable("Cs")
			)
			ok, err := vm.SetOf(c, &term.Compound{
				Functor: "^",
				Args: []term.Interface{a, &term.Compound{
					Functor: "foo",
					Args:    []term.Interface{a, b, c},
				}},
			}, cs, func(env term.Env) *nondet.Promise {
				switch count {
				case 0:
					assert.True(t, env.Resolve(a).(term.Variable).Anonymous())
					assert.Equal(t, term.Atom("b"), env.Resolve(b))
					assert.Equal(t, c, env.Resolve(c))
					assert.True(t, term.List(term.Atom("c"), term.Atom("d")).Unify(cs, false, &env))
				case 1:
					assert.True(t, env.Resolve(a).(term.Variable).Anonymous())
					assert.Equal(t, term.Atom("c"), env.Resolve(b))
					assert.Equal(t, c, env.Resolve(c))
					assert.True(t, term.List(term.Atom("e"), term.Atom("f"), term.Atom("g")).Unify(cs, false, &env))
				default:
					assert.Fail(t, "unreachable")
				}
				count++
				return nondet.Bool(false)
			}, &env).Force()
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("with multiple qualifiers", func(t *testing.T) {
			var (
				env         = term.Env{}
				count       int
				a, b, c, cs = term.Variable("A"), term.Variable("B"), term.Variable("C"), term.Variable("Cs")
			)
			ok, err := vm.SetOf(c, &term.Compound{
				Functor: "^",
				Args: []term.Interface{
					&term.Compound{
						Functor: ",",
						Args:    []term.Interface{a, b},
					},
					&term.Compound{
						Functor: "foo",
						Args:    []term.Interface{a, b, c},
					},
				},
			}, cs, func(env term.Env) *nondet.Promise {
				switch count {
				case 0:
					assert.Equal(t, a, env.Resolve(a))
					assert.Equal(t, b, env.Resolve(b))
					assert.Equal(t, c, env.Resolve(c))
					assert.True(t, term.List(term.Atom("c"), term.Atom("d"), term.Atom("e"), term.Atom("f"), term.Atom("g")).Unify(cs, false, &env))
				default:
					assert.Fail(t, "unreachable")
				}
				count++
				return nondet.Bool(false)
			}, &env).Force()
			assert.NoError(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("goal is a variable", func(t *testing.T) {
		env := term.Env{}
		goal := term.Variable("Goal")

		var vm VM
		ok, err := vm.SetOf(term.NewVariable(), goal, term.NewVariable(), Success, &env).Force()
		assert.Equal(t, instantiationError(goal), err)
		assert.False(t, ok)
	})

	t.Run("goal is neither a variable nor a callable term", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.SetOf(term.NewVariable(), term.Integer(0), term.NewVariable(), Success, &env).Force()
		assert.Equal(t, typeErrorCallable(term.Integer(0)), err)
		assert.False(t, ok)
	})
}

func TestCompare(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		env := term.Env{}

		x, y := term.Variable("X"), term.Variable("Y")
		ok, err := Compare(term.Atom("<"), x, y, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(term.Atom("="), x, x, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(term.Atom(">"), y, x, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		env = append(env, term.Binding{
			Variable: x,
			Value:    term.Atom("b"),
		}, term.Binding{
			Variable: y,
			Value:    term.Atom("a"),
		})
		ok, err = Compare(term.Atom(">"), x, y, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(term.Atom("<"), term.NewVariable(), term.Integer(0), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(term.Atom("<"), term.NewVariable(), term.Atom(""), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(term.Atom("<"), term.NewVariable(), &term.Compound{}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(term.Atom(">"), term.Integer(0), term.NewVariable(), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(term.Atom("<"), term.Integer(0), term.Integer(1), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(term.Atom("="), term.Integer(0), term.Integer(0), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(term.Atom(">"), term.Integer(1), term.Integer(0), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(term.Atom("<"), term.Integer(0), term.Atom(""), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(term.Atom("<"), term.Integer(0), &term.Compound{}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(term.Atom(">"), term.Atom(""), term.NewVariable(), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(term.Atom(">"), term.Atom(""), term.Integer(0), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(term.Atom("<"), term.Atom("a"), term.Atom("b"), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(term.Atom("="), term.Atom("a"), term.Atom("a"), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(term.Atom(">"), term.Atom("b"), term.Atom("a"), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(term.Atom("<"), term.Atom(""), &term.Compound{}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(term.Atom(">"), &term.Compound{}, term.NewVariable(), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(term.Atom(">"), &term.Compound{}, term.Integer(0), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(term.Atom(">"), &term.Compound{}, term.Atom(""), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(term.Atom("<"), &term.Compound{Functor: "a"}, &term.Compound{Functor: "b"}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(term.Atom("="), &term.Compound{Functor: "a"}, &term.Compound{Functor: "a"}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(term.Atom(">"), &term.Compound{Functor: "b"}, &term.Compound{Functor: "a"}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(term.Atom(">"), &term.Compound{Functor: "f", Args: []term.Interface{term.Atom("a")}}, &term.Compound{Functor: "f"}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(term.Atom("="), &term.Compound{Functor: "f", Args: []term.Interface{term.Atom("a")}}, &term.Compound{Functor: "f", Args: []term.Interface{term.Atom("a")}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(term.Atom("<"), &term.Compound{Functor: "f"}, &term.Compound{Functor: "f", Args: []term.Interface{term.Atom("a")}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(term.Atom(">"), &term.Compound{Functor: "f", Args: []term.Interface{term.Atom("b")}}, &term.Compound{Functor: "f", Args: []term.Interface{term.Atom("a")}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(term.Atom("<"), &term.Compound{Functor: "f", Args: []term.Interface{term.Atom("a")}}, &term.Compound{Functor: "f", Args: []term.Interface{term.Atom("b")}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("detect order", func(t *testing.T) {
		env := term.Env{}

		order := term.Variable("Order")
		ok, err := Compare(order, term.Atom("a"), term.Atom("b"), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, term.Atom("<"), env.Resolve(order))
	})

	t.Run("order is neither a variable nor an atom", func(t *testing.T) {
		env := term.Env{}
		ok, err := Compare(term.Integer(0), term.NewVariable(), term.NewVariable(), Success, &env).Force()
		assert.Equal(t, typeErrorAtom(term.Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("order is an atom but not <, =, or >", func(t *testing.T) {
		env := term.Env{}
		ok, err := Compare(term.Atom("foo"), term.NewVariable(), term.NewVariable(), Success, &env).Force()
		assert.Equal(t, domainErrorOrder(term.Atom("foo")), err)
		assert.False(t, ok)
	})
}

func TestThrow(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		env := term.Env{}
		ok, err := Throw(term.Atom("a"), Success, &env).Force()
		assert.Equal(t, &Exception{Term: term.Atom("a")}, err)
		assert.False(t, ok)
	})

	t.Run("ball is a variable", func(t *testing.T) {
		env := term.Env{}
		ball := term.Variable("Ball")

		ok, err := Throw(ball, Success, &env).Force()
		assert.Equal(t, instantiationError(&ball), err)
		assert.False(t, ok)
	})
}

func TestVM_Catch(t *testing.T) {
	var vm VM
	vm.Register2("=", Unify)
	vm.Register1("throw", Throw)
	vm.Register0("true", func(k func(term.Env) *nondet.Promise, env *term.Env) *nondet.Promise {
		return k(*env)
	})
	vm.Register0("fail", func(_ func(term.Env) *nondet.Promise, _ *term.Env) *nondet.Promise {
		return nondet.Bool(false)
	})

	t.Run("match", func(t *testing.T) {
		env := term.Env{}
		v := term.NewVariable()
		ok, err := vm.Catch(&term.Compound{
			Functor: "throw",
			Args:    []term.Interface{term.Atom("a")},
		}, v, &term.Compound{
			Functor: "=",
			Args:    []term.Interface{v, term.Atom("a")},
		}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not match", func(t *testing.T) {
		env := term.Env{}
		ok, err := vm.Catch(&term.Compound{
			Functor: "throw",
			Args:    []term.Interface{term.Atom("a")},
		}, term.Atom("b"), term.Atom("fail"), Success, &env).Force()
		assert.Equal(t, &Exception{Term: term.Atom("a")}, err)
		assert.False(t, ok)
	})

	t.Run("true", func(t *testing.T) {
		env := term.Env{}
		ok, err := vm.Catch(term.Atom("true"), term.Atom("b"), term.Atom("fail"), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("false", func(t *testing.T) {
		env := term.Env{}
		ok, err := vm.Catch(term.Atom("fail"), term.Atom("b"), term.Atom("fail"), Success, &env).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("non-exception error", func(t *testing.T) {
		env := term.Env{}
		ok, err := vm.Catch(term.Atom("true"), term.NewVariable(), term.Atom("true"), func(env term.Env) *nondet.Promise {
			return nondet.Error(errors.New("failed"))
		}, &env).Force()
		assert.Error(t, err)
		assert.False(t, ok)
	})
}

func TestVM_CurrentPredicate(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		env := term.Env{}
		v := term.Variable("V")

		vm := VM{procedures: map[procedureIndicator]procedure{
			{name: "=", arity: 2}: nil,
		}}
		ok, err := vm.CurrentPredicate(v, func(env term.Env) *nondet.Promise {
			assert.Equal(t, &term.Compound{
				Functor: "/",
				Args: []term.Interface{
					term.Atom("="),
					term.Integer(2),
				},
			}, env.Resolve(v))
			return nondet.Bool(true)
		}, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = vm.CurrentPredicate(v, Failure, &env).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("pi is neither a variable nor a predicate indicator", func(t *testing.T) {
		t.Run("atom", func(t *testing.T) {
			env := term.Env{}
			var vm VM
			ok, err := vm.CurrentPredicate(term.Atom("foo"), Success, &env).Force()
			assert.Equal(t, typeErrorPredicateIndicator(term.Atom("foo")), err)
			assert.False(t, ok)
		})

		t.Run("compound", func(t *testing.T) {
			t.Run("non slash", func(t *testing.T) {
				env := term.Env{}
				var vm VM
				ok, err := vm.CurrentPredicate(&term.Compound{
					Functor: "f",
					Args:    []term.Interface{term.Atom("a")},
				}, Success, &env).Force()
				assert.Equal(t, typeErrorPredicateIndicator(&term.Compound{
					Functor: "f",
					Args:    []term.Interface{term.Atom("a")},
				}), err)
				assert.False(t, ok)
			})

			t.Run("slash but number", func(t *testing.T) {
				env := term.Env{}
				var vm VM
				ok, err := vm.CurrentPredicate(&term.Compound{
					Functor: "/",
					Args:    []term.Interface{term.Integer(0), term.Integer(0)},
				}, Success, &env).Force()
				assert.Equal(t, typeErrorPredicateIndicator(&term.Compound{
					Functor: "/",
					Args:    []term.Interface{term.Integer(0), term.Integer(0)},
				}), err)
				assert.False(t, ok)
			})

			t.Run("slash but path", func(t *testing.T) {
				env := term.Env{}
				var vm VM
				ok, err := vm.CurrentPredicate(&term.Compound{
					Functor: "/",
					Args:    []term.Interface{term.Atom("foo"), term.Atom("bar")},
				}, Success, &env).Force()
				assert.Equal(t, typeErrorPredicateIndicator(&term.Compound{
					Functor: "/",
					Args:    []term.Interface{term.Atom("foo"), term.Atom("bar")},
				}), err)
				assert.False(t, ok)
			})
		})
	})
}

func TestVM_Assertz(t *testing.T) {
	t.Run("append", func(t *testing.T) {
		env := term.Env{}
		var vm VM

		ok, err := vm.Assertz(&term.Compound{
			Functor: "foo",
			Args:    []term.Interface{term.Atom("a")},
		}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = vm.Assertz(&term.Compound{
			Functor: "foo",
			Args:    []term.Interface{term.Atom("b")},
		}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, clauses{
			{
				pi: procedureIndicator{
					name:  "foo",
					arity: 1,
				},
				raw: &term.Compound{
					Functor: "foo",
					Args:    []term.Interface{term.Atom("a")},
				},
				xrTable: []term.Interface{term.Atom("a")},
				bytecode: bytecode{
					{opcode: opConst, operand: 0},
					{opcode: opExit},
				},
			},
			{
				pi: procedureIndicator{
					name:  "foo",
					arity: 1,
				},
				raw: &term.Compound{
					Functor: "foo",
					Args:    []term.Interface{term.Atom("b")},
				},
				xrTable: []term.Interface{term.Atom("b")},
				bytecode: bytecode{
					{opcode: opConst, operand: 0},
					{opcode: opExit},
				},
			},
		}, vm.procedures[procedureIndicator{
			name:  "foo",
			arity: 1,
		}])
	})

	t.Run("directive", func(t *testing.T) {
		env := term.Env{}

		var called bool
		vm := VM{
			procedures: map[procedureIndicator]procedure{
				{name: "directive", arity: 0}: predicate0(func(k func(term.Env) *nondet.Promise, env *term.Env) *nondet.Promise {
					called = true
					return k(*env)
				}),
			},
		}

		ok, err := vm.Assertz(&term.Compound{
			Functor: ":-",
			Args:    []term.Interface{term.Atom("directive")},
		}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.True(t, called)
	})

	t.Run("clause is a variable", func(t *testing.T) {
		env := term.Env{}
		clause := term.Variable("Term")

		var vm VM
		ok, err := vm.Assertz(clause, Success, &env).Force()
		assert.Equal(t, instantiationError(&clause), err)
		assert.False(t, ok)
	})

	t.Run("clause is neither a variable, nor callable", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.Assertz(term.Integer(0), Success, &env).Force()
		assert.Equal(t, typeErrorCallable(term.Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("head is a variable", func(t *testing.T) {
		env := term.Env{}
		head := term.Variable("Head")

		var vm VM
		ok, err := vm.Assertz(&term.Compound{
			Functor: ":-",
			Args:    []term.Interface{head, term.Atom("true")},
		}, Success, &env).Force()
		assert.Equal(t, instantiationError(&head), err)
		assert.False(t, ok)
	})

	t.Run("head is neither a variable, nor callable", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.Assertz(&term.Compound{
			Functor: ":-",
			Args:    []term.Interface{term.Integer(0), term.Atom("true")},
		}, Success, &env).Force()
		assert.Equal(t, typeErrorCallable(term.Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("directive is a variable", func(t *testing.T) {
		env := term.Env{}
		directive := term.Variable("Directive")

		var vm VM
		ok, err := vm.Assertz(&term.Compound{
			Functor: ":-",
			Args:    []term.Interface{directive},
		}, Success, &env).Force()
		assert.Equal(t, instantiationError(directive), err)
		assert.False(t, ok)
	})

	t.Run("directive is neither a variable, nor callable", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.Assertz(&term.Compound{
			Functor: ":-",
			Args:    []term.Interface{term.Integer(0)},
		}, Success, &env).Force()
		assert.Equal(t, typeErrorCallable(term.Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("body contains a term which is not callable", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.Assertz(&term.Compound{
			Functor: ":-",
			Args: []term.Interface{term.Atom("foo"), &term.Compound{
				Functor: ",",
				Args:    []term.Interface{term.Atom("true"), term.Integer(0)},
			}},
		}, Success, &env).Force()
		assert.Equal(t, typeErrorCallable(term.Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("static", func(t *testing.T) {
		env := term.Env{}
		vm := VM{
			procedures: map[procedureIndicator]procedure{
				{name: "static", arity: 0}: predicate0(func(k func(term.Env) *nondet.Promise, env *term.Env) *nondet.Promise {
					return k(*env)
				}),
			},
		}

		ok, err := vm.Assertz(term.Atom("static"), Success, &env).Force()
		assert.Equal(t, permissionErrorModifyStaticProcedure(&term.Compound{
			Functor: "/",
			Args: []term.Interface{
				term.Atom("static"),
				term.Integer(0),
			},
		}), err)
		assert.False(t, ok)
	})
}

func TestVM_Asserta(t *testing.T) {
	t.Run("prepend", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.Asserta(&term.Compound{
			Functor: "foo",
			Args:    []term.Interface{term.Atom("a")},
		}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = vm.Asserta(&term.Compound{
			Functor: "foo",
			Args:    []term.Interface{term.Atom("b")},
		}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, clauses{
			{
				pi: procedureIndicator{name: "foo", arity: 1},
				raw: &term.Compound{
					Functor: "foo",
					Args:    []term.Interface{term.Atom("b")},
				},
				xrTable: []term.Interface{term.Atom("b")},
				bytecode: bytecode{
					{opcode: opConst, operand: 0},
					{opcode: opExit},
				},
			},
			{
				pi: procedureIndicator{name: "foo", arity: 1},
				raw: &term.Compound{
					Functor: "foo",
					Args:    []term.Interface{term.Atom("a")},
				},
				xrTable: []term.Interface{term.Atom("a")},
				bytecode: bytecode{
					{opcode: opConst, operand: 0},
					{opcode: opExit},
				},
			},
		}, vm.procedures[procedureIndicator{name: "foo", arity: 1}])
	})

	t.Run("directive", func(t *testing.T) {
		env := term.Env{}
		var called bool
		vm := VM{
			procedures: map[procedureIndicator]procedure{
				{name: "directive", arity: 0}: predicate0(func(k func(term.Env) *nondet.Promise, env *term.Env) *nondet.Promise {
					called = true
					return k(*env)
				}),
			},
		}

		ok, err := vm.Asserta(&term.Compound{
			Functor: ":-",
			Args:    []term.Interface{term.Atom("directive")},
		}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.True(t, called)
	})

	t.Run("clause is a variable", func(t *testing.T) {
		env := term.Env{}
		clause := term.Variable("Term")

		var vm VM
		ok, err := vm.Asserta(clause, Success, &env).Force()
		assert.Equal(t, instantiationError(&clause), err)
		assert.False(t, ok)
	})

	t.Run("clause is neither a variable, nor callable", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.Asserta(term.Integer(0), Success, &env).Force()
		assert.Equal(t, typeErrorCallable(term.Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("head is a variable", func(t *testing.T) {
		env := term.Env{}
		head := term.Variable("Head")

		var vm VM
		ok, err := vm.Asserta(&term.Compound{
			Functor: ":-",
			Args:    []term.Interface{head, term.Atom("true")},
		}, Success, &env).Force()
		assert.Equal(t, instantiationError(head), err)
		assert.False(t, ok)
	})

	t.Run("head is neither a variable, nor callable", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.Asserta(&term.Compound{
			Functor: ":-",
			Args:    []term.Interface{term.Integer(0), term.Atom("true")},
		}, Success, &env).Force()
		assert.Equal(t, typeErrorCallable(term.Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("directive is a variable", func(t *testing.T) {
		env := term.Env{}
		directive := term.Variable("Directive")

		var vm VM
		ok, err := vm.Asserta(&term.Compound{
			Functor: ":-",
			Args:    []term.Interface{directive},
		}, Success, &env).Force()
		assert.Equal(t, instantiationError(directive), err)
		assert.False(t, ok)
	})

	t.Run("directive is neither a variable, nor callable", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.Asserta(&term.Compound{
			Functor: ":-",
			Args:    []term.Interface{term.Integer(0)},
		}, Success, &env).Force()
		assert.Equal(t, typeErrorCallable(term.Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("body contains a term which is not callable", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.Asserta(&term.Compound{
			Functor: ":-",
			Args: []term.Interface{term.Atom("foo"), &term.Compound{
				Functor: ",",
				Args:    []term.Interface{term.Atom("true"), term.Integer(0)},
			}},
		}, Success, &env).Force()
		assert.Equal(t, typeErrorCallable(term.Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("static", func(t *testing.T) {
		env := term.Env{}
		vm := VM{
			procedures: map[procedureIndicator]procedure{
				{name: "static", arity: 0}: predicate0(func(k func(term.Env) *nondet.Promise, env *term.Env) *nondet.Promise {
					return k(*env)
				}),
			},
		}

		ok, err := vm.Asserta(term.Atom("static"), Success, &env).Force()
		assert.Equal(t, permissionErrorModifyStaticProcedure(&term.Compound{
			Functor: "/",
			Args: []term.Interface{
				term.Atom("static"),
				term.Integer(0),
			},
		}), err)
		assert.False(t, ok)
	})
}

func TestVM_Retract(t *testing.T) {
	t.Run("retract the first one", func(t *testing.T) {
		env := term.Env{}
		vm := VM{
			procedures: map[procedureIndicator]procedure{
				{name: "foo", arity: 1}: clauses{
					{raw: &term.Compound{Functor: "foo", Args: []term.Interface{term.Atom("a")}}},
					{raw: &term.Compound{Functor: "foo", Args: []term.Interface{term.Atom("b")}}},
					{raw: &term.Compound{Functor: "foo", Args: []term.Interface{term.Atom("c")}}},
				},
			},
		}

		ok, err := vm.Retract(&term.Compound{
			Functor: "foo",
			Args:    []term.Interface{term.Variable("X")},
		}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, clauses{
			{raw: &term.Compound{Functor: "foo", Args: []term.Interface{term.Atom("b")}}},
			{raw: &term.Compound{Functor: "foo", Args: []term.Interface{term.Atom("c")}}},
		}, vm.procedures[procedureIndicator{name: "foo", arity: 1}])
	})

	t.Run("retract the specific one", func(t *testing.T) {
		env := term.Env{}
		vm := VM{
			procedures: map[procedureIndicator]procedure{
				{name: "foo", arity: 1}: clauses{
					{raw: &term.Compound{Functor: "foo", Args: []term.Interface{term.Atom("a")}}},
					{raw: &term.Compound{Functor: "foo", Args: []term.Interface{term.Atom("b")}}},
					{raw: &term.Compound{Functor: "foo", Args: []term.Interface{term.Atom("c")}}},
				},
			},
		}

		ok, err := vm.Retract(&term.Compound{
			Functor: "foo",
			Args:    []term.Interface{term.Atom("b")},
		}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, clauses{
			{raw: &term.Compound{Functor: "foo", Args: []term.Interface{term.Atom("a")}}},
			{raw: &term.Compound{Functor: "foo", Args: []term.Interface{term.Atom("c")}}},
		}, vm.procedures[procedureIndicator{name: "foo", arity: 1}])
	})

	t.Run("retract all", func(t *testing.T) {
		env := term.Env{}
		vm := VM{
			procedures: map[procedureIndicator]procedure{
				{name: "foo", arity: 1}: clauses{
					{raw: &term.Compound{Functor: "foo", Args: []term.Interface{term.Atom("a")}}},
					{raw: &term.Compound{Functor: "foo", Args: []term.Interface{term.Atom("b")}}},
					{raw: &term.Compound{Functor: "foo", Args: []term.Interface{term.Atom("c")}}},
				},
			},
		}
		ok, err := vm.Retract(&term.Compound{
			Functor: "foo",
			Args:    []term.Interface{term.Variable("X")},
		}, Failure, &env).Force()
		assert.NoError(t, err)
		assert.False(t, ok)

		assert.Empty(t, vm.procedures[procedureIndicator{name: "foo", arity: 1}])
	})

	t.Run("variable", func(t *testing.T) {
		env := term.Env{}
		x := term.Variable("X")

		var vm VM
		ok, err := vm.Retract(x, Success, &env).Force()
		assert.Equal(t, instantiationError(x), err)
		assert.False(t, ok)
	})

	t.Run("not callable", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.Retract(term.Integer(0), Success, &env).Force()
		assert.Equal(t, typeErrorCallable(term.Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("no clause matches", func(t *testing.T) {
		env := term.Env{}
		var vm VM

		ok, err := vm.Retract(&term.Compound{
			Functor: "foo",
			Args:    []term.Interface{term.Variable("X")},
		}, Success, &env).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("static", func(t *testing.T) {
		env := term.Env{}
		vm := VM{
			procedures: map[procedureIndicator]procedure{
				{name: "foo", arity: 0}: predicate0(nil),
			},
		}

		ok, err := vm.Retract(term.Atom("foo"), Success, &env).Force()
		assert.Equal(t, permissionErrorModifyStaticProcedure(&term.Compound{
			Functor: "/",
			Args:    []term.Interface{term.Atom("foo"), term.Integer(0)},
		}), err)
		assert.False(t, ok)
	})

	t.Run("exception in continuation", func(t *testing.T) {
		env := term.Env{}
		vm := VM{
			procedures: map[procedureIndicator]procedure{
				{name: "foo", arity: 1}: clauses{
					{raw: &term.Compound{Functor: "foo", Args: []term.Interface{term.Atom("a")}}},
				},
			},
		}

		ok, err := vm.Retract(&term.Compound{
			Functor: "foo",
			Args:    []term.Interface{term.Variable("X")},
		}, func(_ term.Env) *nondet.Promise {
			return nondet.Error(errors.New("failed"))
		}, &env).Force()
		assert.Error(t, err)
		assert.False(t, ok)

		// removed
		assert.Empty(t, vm.procedures[procedureIndicator{name: "foo", arity: 1}])
	})
}

func TestVM_Abolish(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		env := term.Env{}
		vm := VM{
			procedures: map[procedureIndicator]procedure{
				{name: "foo", arity: 1}: clauses{
					{raw: &term.Compound{Functor: "foo", Args: []term.Interface{term.Atom("a")}}},
					{raw: &term.Compound{Functor: "foo", Args: []term.Interface{term.Atom("b")}}},
					{raw: &term.Compound{Functor: "foo", Args: []term.Interface{term.Atom("c")}}},
				},
			},
		}

		ok, err := vm.Abolish(&term.Compound{
			Functor: "/",
			Args:    []term.Interface{term.Atom("foo"), term.Integer(1)},
		}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		_, ok = vm.procedures[procedureIndicator{name: "foo", arity: 1}]
		assert.False(t, ok)
	})

	t.Run("pi is a variable", func(t *testing.T) {
		env := term.Env{}
		pi := term.Variable("PI")

		var vm VM
		ok, err := vm.Abolish(pi, Success, &env).Force()
		assert.Equal(t, instantiationError(&pi), err)
		assert.False(t, ok)
	})

	t.Run("pi is a term Name/Arity and either Name or Arity is a variable", func(t *testing.T) {
		t.Run("Name is a variable", func(t *testing.T) {
			env := term.Env{}
			name := term.Variable("Name")

			var vm VM
			ok, err := vm.Abolish(&term.Compound{
				Functor: "/",
				Args:    []term.Interface{name, term.Integer(2)},
			}, Success, &env).Force()
			assert.Equal(t, instantiationError(name), err)
			assert.False(t, ok)
		})

		t.Run("Arity is a variable", func(t *testing.T) {
			env := term.Env{}
			arity := term.Variable("Arity")

			var vm VM
			ok, err := vm.Abolish(&term.Compound{
				Functor: "/",
				Args:    []term.Interface{term.Atom("foo"), arity},
			}, Success, &env).Force()
			assert.Equal(t, instantiationError(arity), err)
			assert.False(t, ok)
		})
	})

	t.Run("pi is neither a variable nor a predicate indicator", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.Abolish(term.Integer(0), Success, &env).Force()
		assert.Equal(t, typeErrorPredicateIndicator(term.Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("pi is a term Name/Arity and Name is neither a variable nor an atom", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.Abolish(&term.Compound{
			Functor: "/",
			Args:    []term.Interface{term.Integer(0), term.Integer(2)},
		}, Success, &env).Force()
		assert.Equal(t, typeErrorAtom(term.Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("pi is a term Name/Arity and Arity is neither a variable nor an integer", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.Abolish(&term.Compound{
			Functor: "/",
			Args:    []term.Interface{term.Atom("foo"), term.Atom("bar")},
		}, Success, &env).Force()
		assert.Equal(t, typeErrorInteger(term.Atom("bar")), err)
		assert.False(t, ok)
	})

	t.Run("pi is a term Name/Arity and Arity is an integer less than zero", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.Abolish(&term.Compound{
			Functor: "/",
			Args:    []term.Interface{term.Atom("foo"), term.Integer(-2)},
		}, Success, &env).Force()
		assert.Equal(t, domainErrorNotLessThanZero(term.Integer(-2)), err)
		assert.False(t, ok)
	})

	t.Run("The predicate indicator pi is that of a static procedure", func(t *testing.T) {
		env := term.Env{}
		vm := VM{
			procedures: map[procedureIndicator]procedure{
				{name: "foo", arity: 0}: predicate0(nil),
			},
		}
		ok, err := vm.Abolish(&term.Compound{
			Functor: "/",
			Args:    []term.Interface{term.Atom("foo"), term.Integer(0)},
		}, Success, &env).Force()
		assert.Equal(t, permissionErrorModifyStaticProcedure(&term.Compound{
			Functor: "/",
			Args:    []term.Interface{term.Atom("foo"), term.Integer(0)},
		}), err)
		assert.False(t, ok)
	})
}

func TestVM_CurrentInput(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		env := term.Env{}
		var s term.Stream
		vm := VM{
			input: &s,
		}

		ok, err := vm.CurrentInput(&s, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("stream is neither a variable nor a stream", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.CurrentInput(term.Integer(0), Success, &env).Force()
		assert.Equal(t, domainErrorStream(term.Integer(0)), err)
		assert.False(t, ok)
	})
}

func TestVM_CurrentOutput(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		env := term.Env{}
		var s term.Stream
		vm := VM{
			output: &s,
		}

		ok, err := vm.CurrentOutput(&s, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("stream is neither a variable nor a stream", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.CurrentOutput(term.Integer(0), Success, &env).Force()
		assert.Equal(t, domainErrorStream(term.Integer(0)), err)
		assert.False(t, ok)
	})
}

func TestVM_SetInput(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		v := term.Variable("Stream")
		s := term.Stream{Source: os.Stdin}
		env := term.Env{
			{
				Variable: v,
				Value:    &s,
			},
		}
		var vm VM
		ok, err := vm.SetInput(v, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
		assert.Equal(t, &s, vm.input)
	})

	t.Run("alias", func(t *testing.T) {
		v := term.Variable("Stream")
		s := term.Stream{Source: os.Stdin}
		env := term.Env{
			{
				Variable: v,
				Value:    &s,
			},
		}
		vm := VM{
			streams: map[term.Interface]*term.Stream{
				term.Atom("x"): &s,
			},
		}
		ok, err := vm.SetInput(v, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
		assert.Equal(t, &s, vm.input)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		env := term.Env{}
		streamOrAlias := term.Variable("Stream")

		var vm VM
		ok, err := vm.SetInput(streamOrAlias, Success, &env).Force()
		assert.Equal(t, instantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable, nor a stream term or alias", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.SetInput(term.Integer(0), Success, &env).Force()
		assert.Equal(t, domainErrorStreamOrAlias(term.Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.SetInput(term.Atom("x"), Success, &env).Force()
		assert.Equal(t, existenceErrorStream(term.Atom("x")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		v := term.Variable("Stream")
		env := term.Env{
			{
				Variable: v,
				Value:    &term.Stream{Sink: os.Stdout},
			},
		}
		var vm VM
		ok, err := vm.SetInput(v, Success, &env).Force()
		assert.Equal(t, permissionErrorInputStream(v), err)
		assert.False(t, ok)
	})
}

func TestVM_SetOutput(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		v := term.Variable("Stream")
		s := term.Stream{Sink: os.Stdout}
		env := term.Env{
			{
				Variable: v,
				Value:    &s,
			},
		}
		var vm VM
		ok, err := vm.SetOutput(v, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
		assert.Equal(t, &s, vm.output)
	})

	t.Run("alias", func(t *testing.T) {
		env := term.Env{}
		s := term.Stream{Sink: os.Stdout}
		vm := VM{
			streams: map[term.Interface]*term.Stream{
				term.Atom("x"): &s,
			},
		}
		ok, err := vm.SetOutput(term.Atom("x"), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
		assert.Equal(t, &s, vm.output)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		env := term.Env{}
		streamOrAlias := term.Variable("Stream")

		var vm VM
		ok, err := vm.SetOutput(streamOrAlias, Success, &env).Force()
		assert.Equal(t, instantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable, nor a stream term or alias", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.SetOutput(term.Integer(0), Success, &env).Force()
		assert.Equal(t, domainErrorStreamOrAlias(term.Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.SetOutput(term.Atom("x"), Success, &env).Force()
		assert.Equal(t, existenceErrorStream(term.Atom("x")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an input stream", func(t *testing.T) {
		s := term.Variable("Stream")
		env := term.Env{
			{
				Variable: s,
				Value:    &term.Stream{Source: os.Stdin},
			},
		}

		var vm VM
		ok, err := vm.SetOutput(s, Success, &env).Force()
		assert.Equal(t, permissionErrorOutputStream(s), err)
		assert.False(t, ok)
	})
}

func TestVM_Open(t *testing.T) {
	var vm VM

	t.Run("read", func(t *testing.T) {
		f, err := ioutil.TempFile("", "open_test_read")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, os.Remove(f.Name()))
		}()

		_, err = fmt.Fprintf(f, "test\n")
		assert.NoError(t, err)

		assert.NoError(t, f.Close())

		env := term.Env{}
		v := term.Variable("Stream")

		ok, err := vm.Open(term.Atom(f.Name()), term.Atom("read"), v, term.List(&term.Compound{
			Functor: "alias",
			Args:    []term.Interface{term.Atom("input")},
		}), func(env term.Env) *nondet.Promise {
			ref, ok := env.Lookup(v)
			assert.True(t, ok)
			s, ok := ref.(*term.Stream)
			assert.True(t, ok)

			assert.Equal(t, vm.streams[term.Atom("input")], s)

			b, err := ioutil.ReadAll(s.Source)
			assert.NoError(t, err)
			assert.Equal(t, "test\n", string(b))

			return nondet.Bool(true)
		}, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("write", func(t *testing.T) {
		n := filepath.Join(os.TempDir(), "open_test_write")
		defer func() {
			assert.NoError(t, os.Remove(n))
		}()

		env := term.Env{}
		v := term.Variable("Stream")

		ok, err := vm.Open(term.Atom(n), term.Atom("write"), v, term.List(&term.Compound{
			Functor: "alias",
			Args:    []term.Interface{term.Atom("output")},
		}), func(env term.Env) *nondet.Promise {
			ref, ok := env.Lookup(v)
			assert.True(t, ok)
			s, ok := ref.(*term.Stream)
			assert.True(t, ok)

			assert.Equal(t, vm.streams[term.Atom("output")], s)

			_, err := fmt.Fprintf(s.Sink, "test\n")
			assert.NoError(t, err)

			f, err := os.Open(n)
			assert.NoError(t, err)
			defer func() {
				assert.NoError(t, f.Close())
			}()

			b, err := ioutil.ReadAll(f)
			assert.NoError(t, err)
			assert.Equal(t, "test\n", string(b))

			return nondet.Bool(true)
		}, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("append", func(t *testing.T) {
		f, err := ioutil.TempFile("", "open_test_append")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, os.Remove(f.Name()))
		}()

		_, err = fmt.Fprintf(f, "test\n")
		assert.NoError(t, err)

		assert.NoError(t, f.Close())

		env := term.Env{}
		v := term.Variable("Stream")

		ok, err := vm.Open(term.Atom(f.Name()), term.Atom("append"), v, term.List(&term.Compound{
			Functor: "alias",
			Args:    []term.Interface{term.Atom("append")},
		}), func(env term.Env) *nondet.Promise {
			ref, ok := env.Lookup(v)
			assert.True(t, ok)
			s, ok := ref.(*term.Stream)
			assert.True(t, ok)

			assert.Equal(t, vm.streams[term.Atom("append")], s)

			_, err = fmt.Fprintf(s.Sink, "test\n")
			assert.NoError(t, err)

			f, err = os.Open(f.Name())
			assert.NoError(t, err)
			defer func() {
				assert.NoError(t, f.Close())
			}()

			b, err := ioutil.ReadAll(f)
			assert.NoError(t, err)
			assert.Equal(t, "test\ntest\n", string(b))

			return nondet.Bool(true)
		}, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("sourceSink is a variable", func(t *testing.T) {
		env := term.Env{}
		sourceSink := term.Variable("Source_Sink")

		var vm VM
		ok, err := vm.Open(sourceSink, term.Atom("read"), term.Variable("Stream"), term.List(), Success, &env).Force()
		assert.Equal(t, instantiationError(sourceSink), err)
		assert.False(t, ok)
	})

	t.Run("mode is a variable", func(t *testing.T) {
		env := term.Env{}
		mode := term.Variable("Mode")

		var vm VM
		ok, err := vm.Open(term.Atom("/dev/null"), mode, term.Variable("Stream"), term.List(), Success, &env).Force()
		assert.Equal(t, instantiationError(mode), err)
		assert.False(t, ok)
	})

	t.Run("options is a partial list or a list with an element E which is a variable", func(t *testing.T) {
		t.Run("partial list", func(t *testing.T) {
			env := term.Env{}
			options := term.ListRest(term.Variable("Rest"),
				&term.Compound{Functor: "type", Args: []term.Interface{term.Atom("text")}},
				&term.Compound{Functor: "alias", Args: []term.Interface{term.Atom("foo")}},
			)

			var vm VM
			ok, err := vm.Open(term.Atom("/dev/null"), term.Atom("read"), term.Variable("Stream"), options, Success, &env).Force()
			assert.Equal(t, instantiationError(options), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			env := term.Env{}
			option := term.Variable("Option")

			var vm VM
			ok, err := vm.Open(term.Atom("/dev/null"), term.Atom("read"), term.Variable("Stream"), term.List(
				option,
				&term.Compound{Functor: "type", Args: []term.Interface{term.Atom("text")}},
				&term.Compound{Functor: "alias", Args: []term.Interface{term.Atom("foo")}},
			), Success, &env).Force()
			assert.Equal(t, instantiationError(option), err)
			assert.False(t, ok)
		})
	})

	t.Run("mode is neither a variable nor an atom", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.Open(term.Atom("/dev/null"), term.Integer(0), term.Variable("Stream"), term.List(), Success, &env).Force()
		assert.Equal(t, typeErrorAtom(term.Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("options is neither a partial list nor a list", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.Open(term.Atom("/dev/null"), term.Atom("read"), term.Variable("Stream"), term.Atom("list"), Success, &env).Force()
		assert.Equal(t, typeErrorList(term.Atom("list")), err)
		assert.False(t, ok)
	})

	t.Run("stream is not a variable", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.Open(term.Atom("/dev/null"), term.Atom("read"), term.Atom("stream"), term.List(), Success, &env).Force()
		assert.Equal(t, typeErrorVariable(term.Atom("stream")), err)
		assert.False(t, ok)
	})

	t.Run("sourceSink is neither a variable nor a source/sink", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.Open(term.Integer(0), term.Atom("read"), term.Variable("Stream"), term.List(), Success, &env).Force()
		assert.Equal(t, domainErrorSourceSink(term.Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("mode is an atom but not an input/output mode", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.Open(term.Atom("/dev/null"), term.Atom("foo"), term.Variable("Stream"), term.List(), Success, &env).Force()
		assert.Equal(t, domainErrorIOMode(term.Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the options list is neither a variable nor a stream-option", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.Open(term.Atom("/dev/null"), term.Atom("read"), term.Variable("Stream"), term.List(term.Atom("foo")), Success, &env).Force()
		assert.Equal(t, domainErrorStreamOption(term.Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("the source/sink specified by sourceSink does not exist", func(t *testing.T) {
		env := term.Env{}
		f, err := ioutil.TempFile("", "open_test_existence")
		assert.NoError(t, err)
		assert.NoError(t, os.Remove(f.Name()))

		var vm VM
		ok, err := vm.Open(term.Atom(f.Name()), term.Atom("read"), term.Variable("Stream"), term.List(), Success, &env).Force()
		assert.Equal(t, existenceErrorSourceSink(term.Atom(f.Name())), err)
		assert.False(t, ok)
	})

	t.Run("the source/sink specified by sourceSink cannot be opened", func(t *testing.T) {
		env := term.Env{}
		f, err := ioutil.TempFile("", "open_test_permission")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, os.Remove(f.Name()))
		}()

		assert.NoError(t, f.Chmod(0200))

		var vm VM
		ok, err := vm.Open(term.Atom(f.Name()), term.Atom("read"), term.Variable("Stream"), term.List(), Success, &env).Force()
		assert.Equal(t, permissionError(term.Atom("open"), term.Atom("source_sink"), term.Atom(f.Name()), term.Atom(fmt.Sprintf("'%s' cannot be opened.", f.Name()))), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the options list is alias and A is already associated with an open stream", func(t *testing.T) {
		env := term.Env{}
		f, err := ioutil.TempFile("", "open_test_dup_alias")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, os.Remove(f.Name()))
		}()

		vm := VM{
			streams: map[term.Interface]*term.Stream{
				term.Atom("foo"): nil,
			},
		}
		ok, err := vm.Open(term.Atom(f.Name()), term.Atom("read"), term.Variable("Stream"), term.List(&term.Compound{
			Functor: "alias",
			Args:    []term.Interface{term.Atom("foo")},
		}), Success, &env).Force()
		assert.Equal(t, permissionError(term.Atom("open"), term.Atom("source_sink"), &term.Compound{
			Functor: "alias",
			Args:    []term.Interface{term.Atom("foo")},
		}, term.Atom("foo is already defined as an alias.")), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the options list is reposition(true) and it is not possible to reposition", func(t *testing.T) {
		// TODO:
	})
}

func TestVM_Close(t *testing.T) {
	t.Run("without options", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			env := term.Env{}

			var m mockCloser
			m.On("Close").Return(nil).Once()
			defer m.AssertExpectations(t)

			var vm VM
			ok, err := vm.Close(&term.Stream{Closer: &m}, term.List(), Success, &env).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("ng", func(t *testing.T) {
			env := term.Env{}

			var m mockCloser
			m.On("Close").Return(errors.New("")).Once()
			defer m.AssertExpectations(t)

			var vm VM
			_, err := vm.Close(&term.Stream{Closer: &m}, term.List(), Success, &env).Force()
			assert.Error(t, err)
		})
	})

	t.Run("force false", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			env := term.Env{}

			var m mockCloser
			m.On("Close").Return(nil).Once()
			defer m.AssertExpectations(t)

			var vm VM
			ok, err := vm.Close(&term.Stream{Closer: &m}, term.List(&term.Compound{
				Functor: "force",
				Args:    []term.Interface{term.Atom("false")},
			}), Success, &env).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("ng", func(t *testing.T) {
			env := term.Env{}

			var m mockCloser
			m.On("Close").Return(errors.New("something happened")).Once()
			defer m.AssertExpectations(t)

			s := term.Stream{Closer: &m}

			var vm VM
			ok, err := vm.Close(&s, term.List(&term.Compound{
				Functor: "force",
				Args:    []term.Interface{term.Atom("false")},
			}), Success, &env).Force()
			assert.Equal(t, resourceError(&s, term.Atom("something happened")), err)
			assert.False(t, ok)
		})
	})

	t.Run("force true", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			env := term.Env{}

			var m mockCloser
			m.On("Close").Return(nil).Once()
			defer m.AssertExpectations(t)

			var vm VM
			ok, err := vm.Close(&term.Stream{Closer: &m}, term.List(&term.Compound{
				Functor: "force",
				Args:    []term.Interface{term.Atom("true")},
			}), Success, &env).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("ng", func(t *testing.T) {
			env := term.Env{}

			var m mockCloser
			m.On("Close").Return(errors.New("")).Once()
			defer m.AssertExpectations(t)

			var vm VM
			ok, err := vm.Close(&term.Stream{Closer: &m}, term.List(&term.Compound{
				Functor: "force",
				Args:    []term.Interface{term.Atom("true")},
			}), Success, &env).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("valid stream alias", func(t *testing.T) {
		env := term.Env{}

		var m mockCloser
		m.On("Close").Return(nil).Once()
		defer m.AssertExpectations(t)

		vm := VM{
			streams: map[term.Interface]*term.Stream{
				term.Atom("foo"): {Closer: &m},
			},
		}
		ok, err := vm.Close(term.Atom("foo"), term.List(), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("streamOrAlias ia a variable", func(t *testing.T) {
		env := term.Env{}
		streamOrAlias := term.Variable("Stream")

		var vm VM
		ok, err := vm.Close(streamOrAlias, term.List(), Success, &env).Force()
		assert.Equal(t, instantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("options is a partial list or a list with an element E which is a variable", func(t *testing.T) {
		t.Run("partial list", func(t *testing.T) {
			env := term.Env{}
			options := term.ListRest(term.Variable("Rest"),
				&term.Compound{Functor: "force", Args: []term.Interface{term.Atom("true")}},
			)

			var vm VM
			ok, err := vm.Close(&term.Stream{}, options, Success, &env).Force()
			assert.Equal(t, instantiationError(options), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			env := term.Env{}
			option := term.Variable("Option")

			var vm VM
			ok, err := vm.Close(&term.Stream{}, term.List(option, &term.Compound{Functor: "force", Args: []term.Interface{term.Atom("true")}}), Success, &env).Force()
			assert.Equal(t, instantiationError(option), err)
			assert.False(t, ok)
		})
	})

	t.Run("options is neither a partial list nor a list", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.Close(&term.Stream{}, term.Atom("foo"), Success, &env).Force()
		assert.Equal(t, typeErrorList(term.Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream-term or alias", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.Close(term.Integer(0), term.List(), Success, &env).Force()
		assert.Equal(t, domainErrorStreamOrAlias(term.Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the Options list is neither a variable nor a stream-option", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.Close(&term.Stream{}, term.List(term.Atom("foo")), Success, &env).Force()
		assert.Equal(t, domainErrorStreamOption(term.Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.Close(term.Atom("foo"), term.List(), Success, &env).Force()
		assert.Equal(t, existenceErrorStream(term.Atom("foo")), err)
		assert.False(t, ok)
	})
}

type mockReader struct {
	mock.Mock
}

func (m *mockReader) Read(p []byte) (n int, err error) {
	args := m.Called(p)
	return args.Int(0), args.Error(1)
}

type mockWriter struct {
	mock.Mock
}

func (m *mockWriter) Write(p []byte) (n int, err error) {
	args := m.Called(p)
	return args.Int(0), args.Error(1)
}

type mockCloser struct {
	mock.Mock
}

func (m *mockCloser) Close() error {
	args := m.Called()
	return args.Error(0)
}

func TestVM_FlushOutput(t *testing.T) {
	t.Run("non flusher", func(t *testing.T) {
		env := term.Env{}

		var m mockWriter
		defer m.AssertExpectations(t)

		var vm VM
		ok, err := vm.FlushOutput(&term.Stream{Sink: &m}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("flusher", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			env := term.Env{}

			var m struct {
				mockWriter
				mockFlusher
			}
			m.mockFlusher.On("Flush").Return(nil).Once()
			defer m.mockWriter.AssertExpectations(t)
			defer m.mockFlusher.AssertExpectations(t)

			var vm VM
			ok, err := vm.FlushOutput(&term.Stream{Sink: &m}, Success, &env).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("ng", func(t *testing.T) {
			env := term.Env{}

			var m struct {
				mockWriter
				mockFlusher
			}
			m.mockFlusher.On("Flush").Return(errors.New("")).Once()
			defer m.mockWriter.AssertExpectations(t)
			defer m.mockFlusher.AssertExpectations(t)

			var vm VM
			_, err := vm.FlushOutput(&term.Stream{Sink: &m}, Success, &env).Force()
			assert.Error(t, err)
		})
	})

	t.Run("valid stream alias", func(t *testing.T) {
		env := term.Env{}

		var m mockWriter
		defer m.AssertExpectations(t)

		vm := VM{
			streams: map[term.Interface]*term.Stream{
				term.Atom("foo"): {Sink: &m},
			},
		}
		ok, err := vm.FlushOutput(term.Atom("foo"), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		env := term.Env{}
		streamOrAlias := term.Variable("Stream")

		var vm VM
		ok, err := vm.FlushOutput(streamOrAlias, Success, &env).Force()
		assert.Equal(t, instantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream-term or alias", func(t *testing.T) {
		env := term.Env{}

		var vm VM
		ok, err := vm.FlushOutput(term.Integer(0), Success, &env).Force()
		assert.Equal(t, domainErrorStreamOrAlias(term.Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.FlushOutput(term.Atom("foo"), Success, &env).Force()
		assert.Equal(t, existenceErrorStream(term.Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("SorA is an input stream", func(t *testing.T) {
		env := term.Env{}
		s := term.Stream{Source: &mockReader{}}

		var vm VM
		ok, err := vm.FlushOutput(&s, Success, &env).Force()
		assert.Equal(t, permissionErrorOutputStream(&s), err)
		assert.False(t, ok)
	})
}

type mockFlusher struct {
	mock.Mock
}

func (m *mockFlusher) Flush() error {
	args := m.Called()
	return args.Error(0)
}

func TestVM_WriteTerm(t *testing.T) {
	var w mockWriter
	defer w.AssertExpectations(t)

	s := term.Stream{Sink: &w}

	ops := term.Operators{
		{Priority: 500, Specifier: "yfx", Name: "+"},
		{Priority: 200, Specifier: "fy", Name: "-"},
	}

	vm := VM{
		Operators: ops,
		streams: map[term.Interface]*term.Stream{
			term.Atom("foo"): &s,
		},
	}

	t.Run("without options", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			env := term.Env{}

			var m mockTerm
			m.On("WriteTerm", s.Sink, term.WriteTermOptions{Ops: ops}, env).Return(nil).Once()
			defer m.AssertExpectations(t)

			ok, err := vm.WriteTerm(&s, &m, term.List(), Success, &env).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("ng", func(t *testing.T) {
			env := term.Env{}

			var m mockTerm
			m.On("WriteTerm", s.Sink, term.WriteTermOptions{Ops: ops}, env).Return(errors.New("")).Once()
			defer m.AssertExpectations(t)

			_, err := vm.WriteTerm(&s, &m, term.List(), Success, &env).Force()
			assert.Error(t, err)
		})
	})

	t.Run("quoted", func(t *testing.T) {
		t.Run("false", func(t *testing.T) {
			env := term.Env{}

			var m mockTerm
			m.On("WriteTerm", s.Sink, term.WriteTermOptions{Quoted: false, Ops: ops}, env).Return(nil).Once()
			defer m.AssertExpectations(t)

			ok, err := vm.WriteTerm(&s, &m, term.List(&term.Compound{
				Functor: "quoted",
				Args:    []term.Interface{term.Atom("false")},
			}), Success, &env).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("true", func(t *testing.T) {
			env := term.Env{}

			var m mockTerm
			m.On("WriteTerm", s.Sink, term.WriteTermOptions{Quoted: true, Ops: ops}, env).Return(nil).Once()
			defer m.AssertExpectations(t)

			ok, err := vm.WriteTerm(&s, &m, term.List(&term.Compound{
				Functor: "quoted",
				Args:    []term.Interface{term.Atom("true")},
			}), Success, &env).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("ignore_ops", func(t *testing.T) {
		t.Run("false", func(t *testing.T) {
			env := term.Env{}

			var m mockTerm
			m.On("WriteTerm", s.Sink, term.WriteTermOptions{Ops: ops}, env).Return(nil).Once()
			defer m.AssertExpectations(t)

			ok, err := vm.WriteTerm(&s, &m, term.List(&term.Compound{
				Functor: "ignore_ops",
				Args:    []term.Interface{term.Atom("false")},
			}), Success, &env).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("true", func(t *testing.T) {
			env := term.Env{}

			var m mockTerm
			m.On("WriteTerm", s.Sink, term.WriteTermOptions{Ops: nil}, env).Return(nil).Once()
			defer m.AssertExpectations(t)

			ok, err := vm.WriteTerm(&s, &m, term.List(&term.Compound{
				Functor: "ignore_ops",
				Args:    []term.Interface{term.Atom("true")},
			}), Success, &env).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("numbervars", func(t *testing.T) {
		t.Run("false", func(t *testing.T) {
			env := term.Env{}

			var m mockTerm
			m.On("WriteTerm", s.Sink, term.WriteTermOptions{Ops: ops, NumberVars: false}, env).Return(nil).Once()
			defer m.AssertExpectations(t)

			ok, err := vm.WriteTerm(&s, &m, term.List(&term.Compound{
				Functor: "numbervars",
				Args:    []term.Interface{term.Atom("false")},
			}), Success, &env).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("true", func(t *testing.T) {
			env := term.Env{}

			var m mockTerm
			m.On("WriteTerm", s.Sink, term.WriteTermOptions{Ops: ops, NumberVars: true}, env).Return(nil).Once()
			defer m.AssertExpectations(t)

			ok, err := vm.WriteTerm(&s, &m, term.List(&term.Compound{
				Functor: "numbervars",
				Args:    []term.Interface{term.Atom("true")},
			}), Success, &env).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		env := term.Env{}
		streamOrAlias := term.Variable("Stream")

		var vm VM
		ok, err := vm.WriteTerm(streamOrAlias, term.Atom("foo"), term.List(), Success, &env).Force()
		assert.Equal(t, instantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("options is a partial list or a list with an element which is a variable", func(t *testing.T) {
		t.Run("partial list", func(t *testing.T) {
			env := term.Env{}
			options := term.ListRest(term.Variable("Rest"),
				&term.Compound{Functor: "quoted", Args: []term.Interface{term.Atom("true")}},
			)

			var vm VM
			ok, err := vm.WriteTerm(&term.Stream{Sink: &mockWriter{}}, term.Atom("foo"), options, Success, &env).Force()
			assert.Equal(t, instantiationError(options), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			env := term.Env{}
			option := term.Variable("Option")

			var vm VM
			ok, err := vm.WriteTerm(&term.Stream{Sink: &mockWriter{}}, term.Atom("foo"), term.List(option, &term.Compound{Functor: "quoted", Args: []term.Interface{term.Atom("true")}}), Success, &env).Force()
			assert.Equal(t, instantiationError(option), err)
			assert.False(t, ok)
		})
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.WriteTerm(term.Integer(0), term.Atom("foo"), term.List(), Success, &env).Force()
		assert.Equal(t, domainErrorStreamOrAlias(term.Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("options is neither a partial list nor a list", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.WriteTerm(&term.Stream{Sink: &mockWriter{}}, term.Atom("foo"), term.Atom("options"), Success, &env).Force()
		assert.Equal(t, typeErrorList(term.Atom("options")), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the Options list is neither a variable nor a valid write-option", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.WriteTerm(&term.Stream{Sink: &mockWriter{}}, term.Atom("foo"), term.List(&term.Compound{
			Functor: "unknown",
			Args:    []term.Interface{term.Atom("option")},
		}), Success, &env).Force()
		assert.Equal(t, domainErrorWriteOption(&term.Compound{
			Functor: "unknown",
			Args:    []term.Interface{term.Atom("option")},
		}), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.WriteTerm(term.Atom("stream"), term.Atom("foo"), term.List(), Success, &env).Force()
		assert.Equal(t, existenceErrorStream(term.Atom("stream")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an input stream", func(t *testing.T) {
		env := term.Env{}
		s := term.Stream{Source: &mockReader{}}

		var vm VM
		ok, err := vm.WriteTerm(&s, term.Atom("foo"), term.List(), Success, &env).Force()
		assert.Equal(t, permissionErrorOutputStream(&s), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a binary stream", func(t *testing.T) {
		env := term.Env{}
		s := term.Stream{Sink: &mockWriter{}, StreamType: term.StreamTypeBinary}

		var vm VM
		ok, err := vm.WriteTerm(&s, term.Atom("foo"), term.List(), Success, &env).Force()
		assert.Equal(t, permissionErrorOutputBinaryStream(&s), err)
		assert.False(t, ok)
	})
}

type mockTerm struct {
	mock.Mock
}

func (m *mockTerm) String() string {
	args := m.Called()
	return args.String(0)
}

func (m *mockTerm) WriteTerm(w io.Writer, opts term.WriteTermOptions, env term.Env) error {
	args := m.Called(w, opts, env)
	return args.Error(0)
}

func (m *mockTerm) Unify(t term.Interface, occursCheck bool, env *term.Env) bool {
	args := m.Called(t, occursCheck, env)
	return args.Bool(0)
}

func (m *mockTerm) Copy() term.Interface {
	args := m.Called()
	return args.Get(0).(term.Interface)
}

func TestCharCode(t *testing.T) {
	t.Run("ascii", func(t *testing.T) {
		env := term.Env{}
		ok, err := CharCode(term.Atom("a"), term.Integer(97), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("emoji", func(t *testing.T) {
		env := term.Env{}
		ok, err := CharCode(term.Atom("😀"), term.Integer(128512), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("query char", func(t *testing.T) {
		env := term.Env{}
		v := term.Variable("Char")

		ok, err := CharCode(v, term.Integer(128512), func(env term.Env) *nondet.Promise {
			assert.Equal(t, term.Atom("😀"), env.Resolve(v))
			return nondet.Bool(true)
		}, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("query code", func(t *testing.T) {
		env := term.Env{}
		v := term.Variable("Code")
		ok, err := CharCode(term.Atom("😀"), v, func(env term.Env) *nondet.Promise {
			assert.Equal(t, term.Integer(128512), env.Resolve(v))
			return nondet.Bool(true)
		}, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("char and code are variables", func(t *testing.T) {
		env := term.Env{}
		char, code := term.Variable("Char"), term.Variable("Code")

		ok, err := CharCode(char, code, Success, &env).Force()
		assert.Equal(t, instantiationError(&term.Compound{
			Functor: ",",
			Args:    []term.Interface{char, code},
		}), err)
		assert.False(t, ok)
	})

	t.Run("char is neither a variable nor a one character atom", func(t *testing.T) {
		t.Run("atom", func(t *testing.T) {
			env := term.Env{}
			ok, err := CharCode(term.Atom("foo"), term.NewVariable(), Success, &env).Force()
			assert.Equal(t, typeErrorCharacter(term.Atom("foo")), err)
			assert.False(t, ok)
		})

		t.Run("non-atom", func(t *testing.T) {
			env := term.Env{}
			ok, err := CharCode(term.Integer(0), term.NewVariable(), Success, &env).Force()
			assert.Equal(t, typeErrorCharacter(term.Integer(0)), err)
			assert.False(t, ok)
		})
	})

	t.Run("code is neither a variable nor an integer", func(t *testing.T) {
		env := term.Env{}
		ok, err := CharCode(term.NewVariable(), term.Atom("foo"), Success, &env).Force()
		assert.Equal(t, typeErrorInteger(term.Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("code is neither a variable nor a character-code", func(t *testing.T) {
		env := term.Env{}
		ok, err := CharCode(term.NewVariable(), term.Integer(-1), Success, &env).Force()
		assert.Equal(t, representationError(term.Atom("character_code"), term.Atom("-1 is not a valid unicode code point.")), err)
		assert.False(t, ok)
	})
}

func TestVM_PutByte(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		env := term.Env{}

		var w mockWriter
		w.On("Write", []byte{97}).Return(1, nil).Once()
		defer w.AssertExpectations(t)

		s := term.Stream{Sink: &w, StreamType: term.StreamTypeBinary}

		var vm VM
		ok, err := vm.PutByte(&s, term.Integer(97), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("ng", func(t *testing.T) {
		env := term.Env{}

		var w mockWriter
		w.On("Write", []byte{97}).Return(0, errors.New("")).Once()
		defer w.AssertExpectations(t)

		s := term.Stream{Sink: &w, StreamType: term.StreamTypeBinary}

		var vm VM
		_, err := vm.PutByte(&s, term.Integer(97), Success, &env).Force()
		assert.Error(t, err)
	})

	t.Run("valid stream alias", func(t *testing.T) {
		env := term.Env{}

		var w mockWriter
		w.On("Write", []byte{97}).Return(1, nil).Once()
		defer w.AssertExpectations(t)

		s := term.Stream{Sink: &w, StreamType: term.StreamTypeBinary}

		vm := VM{
			streams: map[term.Interface]*term.Stream{
				term.Atom("foo"): &s,
			},
		}
		ok, err := vm.PutByte(term.Atom("foo"), term.Integer(97), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		env := term.Env{}
		streamOrAlias := term.Variable("Stream")

		var vm VM
		ok, err := vm.PutByte(streamOrAlias, term.Integer(97), Success, &env).Force()
		assert.Equal(t, instantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("byt is a variable", func(t *testing.T) {
		env := term.Env{}
		byt := term.Variable("Byte")

		var vm VM
		ok, err := vm.PutByte(&term.Stream{Sink: &mockWriter{}, StreamType: term.StreamTypeBinary}, byt, Success, &env).Force()
		assert.Equal(t, instantiationError(byt), err)
		assert.False(t, ok)
	})

	t.Run("byt is neither a variable nor an byte", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.PutByte(&term.Stream{Sink: &mockWriter{}, StreamType: term.StreamTypeBinary}, term.Atom("byte"), Success, &env).Force()
		assert.Equal(t, typeErrorByte(term.Atom("byte")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.PutByte(term.Integer(0), term.Integer(97), Success, &env).Force()
		assert.Equal(t, domainErrorStreamOrAlias(term.Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an input stream", func(t *testing.T) {
		s := term.Variable("Stream")
		env := term.Env{
			{
				Variable: s,
				Value:    &term.Stream{Source: &mockReader{}},
			},
		}

		var vm VM
		ok, err := vm.PutByte(s, term.Integer(97), Success, &env).Force()
		assert.Equal(t, permissionErrorOutputStream(s), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a text stream", func(t *testing.T) {
		s := term.Variable("Stream")
		env := term.Env{
			{
				Variable: s,
				Value:    &term.Stream{Sink: &mockWriter{}, StreamType: term.StreamTypeText},
			},
		}

		var vm VM
		ok, err := vm.PutByte(s, term.Integer(97), Success, &env).Force()
		assert.Equal(t, permissionErrorOutputTextStream(s), err)
		assert.False(t, ok)
	})
}

func TestVM_PutCode(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		env := term.Env{}

		var w mockWriter
		w.On("Write", []byte{0xf0, 0x9f, 0x98, 0x80}).Return(1, nil).Once()
		defer w.AssertExpectations(t)

		s := term.Stream{Sink: &w}

		var vm VM
		ok, err := vm.PutCode(&s, term.Integer('😀'), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("ng", func(t *testing.T) {
		env := term.Env{}

		var w mockWriter
		w.On("Write", []byte{0xf0, 0x9f, 0x98, 0x80}).Return(0, errors.New("")).Once()
		defer w.AssertExpectations(t)

		s := term.Stream{Sink: &w}

		var vm VM
		_, err := vm.PutCode(&s, term.Integer('😀'), Success, &env).Force()
		assert.Error(t, err)
	})

	t.Run("valid stream alias", func(t *testing.T) {
		env := term.Env{}

		var w mockWriter
		w.On("Write", []byte{0xf0, 0x9f, 0x98, 0x80}).Return(1, nil).Once()
		defer w.AssertExpectations(t)

		s := term.Stream{Sink: &w}

		vm := VM{
			streams: map[term.Interface]*term.Stream{
				term.Atom("foo"): &s,
			},
		}
		ok, err := vm.PutCode(term.Atom("foo"), term.Integer('😀'), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		env := term.Env{}
		streamOrAlias := term.Variable("Stream")

		var vm VM
		ok, err := vm.PutCode(streamOrAlias, term.Integer(97), Success, &env).Force()
		assert.Equal(t, instantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("code is a variable", func(t *testing.T) {
		env := term.Env{}
		code := term.Variable("Code")

		var vm VM
		ok, err := vm.PutCode(&term.Stream{Sink: &mockWriter{}}, code, Success, &env).Force()
		assert.Equal(t, instantiationError(code), err)
		assert.False(t, ok)
	})

	t.Run("code is neither a variable nor an integer", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.PutCode(&term.Stream{Sink: &mockWriter{}}, term.Atom("code"), Success, &env).Force()
		assert.Equal(t, typeErrorInteger(term.Atom("code")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.PutCode(term.Integer(0), term.Integer(97), Success, &env).Force()
		assert.Equal(t, domainErrorStreamOrAlias(term.Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.PutCode(term.Atom("foo"), term.Integer(97), Success, &env).Force()
		assert.Equal(t, existenceErrorStream(term.Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an input stream", func(t *testing.T) {
		s := term.Variable("Stream")
		env := term.Env{
			{
				Variable: s,
				Value:    &term.Stream{Source: &mockReader{}},
			},
		}

		var vm VM
		ok, err := vm.PutCode(s, term.Integer(97), Success, &env).Force()
		assert.Equal(t, permissionErrorOutputStream(s), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a binary stream", func(t *testing.T) {
		s := term.Variable("Stream")
		env := term.Env{
			{
				Variable: s,
				Value:    &term.Stream{Sink: &mockWriter{}, StreamType: term.StreamTypeBinary},
			},
		}

		var vm VM
		ok, err := vm.PutCode(s, term.Integer(97), Success, &env).Force()
		assert.Equal(t, permissionErrorOutputBinaryStream(s), err)
		assert.False(t, ok)
	})

	t.Run("code is an integer but not an character code", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.PutCode(&term.Stream{Sink: &mockWriter{}}, term.Integer(-1), Success, &env).Force()
		assert.Equal(t, representationError(term.Atom("character_code"), term.Atom("-1 is not a valid unicode code point.")), err)
		assert.False(t, ok)
	})

	t.Run("unknown stream alias", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		_, err := vm.PutCode(term.Atom("foo"), term.Integer('😀'), Success, &env).Force()
		assert.Error(t, err)
	})

	t.Run("not a stream", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		_, err := vm.PutCode(term.NewVariable(), term.Integer('😀'), Success, &env).Force()
		assert.Error(t, err)
	})

	t.Run("not a code", func(t *testing.T) {
		var w mockWriter
		defer w.AssertExpectations(t)

		s := term.Stream{Sink: &w}

		t.Run("not an integer", func(t *testing.T) {
			env := term.Env{}
			var vm VM
			_, err := vm.PutCode(&s, term.Atom("a"), Success, &env).Force()
			assert.Error(t, err)
		})
	})
}

func TestVM_ReadTerm(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		env := term.Env{}
		v := term.Variable("Term")

		var vm VM
		ok, err := vm.ReadTerm(&term.Stream{Source: bufio.NewReader(strings.NewReader("foo."))}, v, term.List(), func(env term.Env) *nondet.Promise {
			assert.Equal(t, term.Atom("foo"), env.Resolve(v))
			return nondet.Bool(true)
		}, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("valid stream alias", func(t *testing.T) {
		env := term.Env{}
		v := term.Variable("Term")

		s := term.Stream{Source: bufio.NewReader(strings.NewReader("foo."))}

		vm := VM{
			streams: map[term.Interface]*term.Stream{
				term.Atom("foo"): &s,
			},
		}
		ok, err := vm.ReadTerm(term.Atom("foo"), v, term.List(), func(env term.Env) *nondet.Promise {
			assert.Equal(t, term.Atom("foo"), env.Resolve(v))
			return nondet.Bool(true)
		}, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("singletons", func(t *testing.T) {
		env := term.Env{}
		v, singletons := term.Variable("Term"), term.Variable("Singletons")

		var vm VM
		ok, err := vm.ReadTerm(&term.Stream{Source: bufio.NewReader(strings.NewReader("f(X, X, Y)."))}, v, term.List(&term.Compound{
			Functor: "singletons",
			Args:    []term.Interface{singletons},
		}), func(env term.Env) *nondet.Promise {
			assert.Equal(t, &term.Compound{
				Functor: "f",
				Args: []term.Interface{
					term.Variable("X"),
					term.Variable("X"),
					term.Variable("Y"),
				},
			}, env.Resolve(v))

			assert.Equal(t, term.List(term.Variable("Y")), env.Resolve(singletons))

			return nondet.Bool(true)
		}, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("variables", func(t *testing.T) {
		env := term.Env{}
		v, variables := term.Variable("Term"), term.Variable("Variables")

		var vm VM
		ok, err := vm.ReadTerm(&term.Stream{Source: bufio.NewReader(strings.NewReader("f(X, X, Y)."))}, v, term.List(&term.Compound{
			Functor: "variables",
			Args:    []term.Interface{variables},
		}), func(env term.Env) *nondet.Promise {
			assert.Equal(t, &term.Compound{
				Functor: "f",
				Args: []term.Interface{
					term.Variable("X"),
					term.Variable("X"),
					term.Variable("Y"),
				},
			}, env.Resolve(v))

			assert.Equal(t, term.List(term.Variable("X"), term.Variable("Y")), env.Resolve(variables))

			return nondet.Bool(true)
		}, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("variable_names", func(t *testing.T) {
		env := term.Env{}
		v, variableNames := term.Variable("Term"), term.Variable("VariableNames")

		var vm VM
		ok, err := vm.ReadTerm(&term.Stream{Source: bufio.NewReader(strings.NewReader("f(X, X, Y)."))}, v, term.List(&term.Compound{
			Functor: "variable_names",
			Args:    []term.Interface{variableNames},
		}), func(env term.Env) *nondet.Promise {
			assert.Equal(t, &term.Compound{
				Functor: "f",
				Args: []term.Interface{
					term.Variable("X"),
					term.Variable("X"),
					term.Variable("Y"),
				},
			}, env.Resolve(v))

			assert.Equal(t, term.List(
				&term.Compound{
					Functor: "=",
					Args:    []term.Interface{term.Atom("X"), term.Variable("X")},
				},
				&term.Compound{
					Functor: "=",
					Args:    []term.Interface{term.Atom("Y"), term.Variable("Y")},
				},
			), env.Resolve(variableNames))

			return nondet.Bool(true)
		}, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("multiple reads", func(t *testing.T) {
		s := term.Stream{Source: bufio.NewReader(strings.NewReader(`
foo(a).
foo(b).
foo(c).
`))}

		v := term.Variable("Term")

		var vm VM

		env := term.Env{}
		ok, err := vm.ReadTerm(&s, v, term.List(), func(env term.Env) *nondet.Promise {
			assert.Equal(t, &term.Compound{Functor: "foo", Args: []term.Interface{term.Atom("a")}}, env.Resolve(v))
			return nondet.Bool(true)
		}, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		env = term.Env{}
		ok, err = vm.ReadTerm(&s, v, term.List(), func(env term.Env) *nondet.Promise {
			assert.Equal(t, &term.Compound{Functor: "foo", Args: []term.Interface{term.Atom("b")}}, env.Resolve(v))
			return nondet.Bool(true)
		}, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		env = term.Env{}
		ok, err = vm.ReadTerm(&s, &v, term.List(), func(env term.Env) *nondet.Promise {
			assert.Equal(t, &term.Compound{Functor: "foo", Args: []term.Interface{term.Atom("c")}}, env.Resolve(v))
			return nondet.Bool(true)
		}, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		env := term.Env{}
		streamOrAlias := term.Variable("Stream")

		var vm VM
		ok, err := vm.ReadTerm(streamOrAlias, term.NewVariable(), term.List(), Success, &env).Force()
		assert.Equal(t, instantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("options is a partial list or a list with an element which is a variable", func(t *testing.T) {
		t.Run("partial list", func(t *testing.T) {
			env := term.Env{}

			options := term.ListRest(term.Variable("Rest"),
				&term.Compound{Functor: "variables", Args: []term.Interface{term.Variable("VL")}},
			)

			var vm VM
			ok, err := vm.ReadTerm(&term.Stream{Source: &mockReader{}}, term.NewVariable(), options, Success, &env).Force()
			assert.Equal(t, instantiationError(options), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			env := term.Env{}
			option := term.Variable("Option")

			var vm VM
			ok, err := vm.ReadTerm(&term.Stream{Source: &mockReader{}}, term.NewVariable(), term.List(option, &term.Compound{Functor: "variables", Args: []term.Interface{term.Variable("VL")}}), Success, &env).Force()
			assert.Equal(t, instantiationError(option), err)
			assert.False(t, ok)
		})
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.ReadTerm(term.Integer(0), term.NewVariable(), term.List(), Success, &env).Force()
		assert.Equal(t, domainErrorStreamOrAlias(term.Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("options is neither a partial list nor a list", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.ReadTerm(&term.Stream{Source: &mockReader{}}, term.NewVariable(), term.Atom("options"), Success, &env).Force()
		assert.Equal(t, typeErrorList(term.Atom("options")), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the Options list is neither a variable nor a valid read-option", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.ReadTerm(&term.Stream{Source: &mockReader{}}, term.NewVariable(), term.List(&term.Compound{
			Functor: "unknown",
			Args:    []term.Interface{term.Atom("option")},
		}), Success, &env).Force()
		assert.Equal(t, domainErrorReadOption(&term.Compound{
			Functor: "unknown",
			Args:    []term.Interface{term.Atom("option")},
		}), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.ReadTerm(term.Atom("foo"), term.NewVariable(), term.List(), Success, &env).Force()
		assert.Equal(t, existenceErrorStream(term.Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		s := term.Variable("Stream")
		env := term.Env{
			{
				Variable: s,
				Value:    &term.Stream{Sink: &mockWriter{}},
			},
		}

		var vm VM
		ok, err := vm.ReadTerm(s, term.NewVariable(), term.List(), Success, &env).Force()
		assert.Equal(t, permissionErrorInputStream(s), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a binary stream", func(t *testing.T) {
		s := term.Variable("Stream")
		env := term.Env{
			{
				Variable: s,
				Value:    &term.Stream{Source: bufio.NewReader(&mockReader{}), StreamType: term.StreamTypeBinary},
			},
		}

		var vm VM
		ok, err := vm.ReadTerm(s, term.NewVariable(), term.List(), Success, &env).Force()
		assert.Equal(t, permissionErrorInputBinaryStream(s), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias has stream properties end_of_stream(past) and eof_action(error)", func(t *testing.T) {
		var r mockReader
		r.On("Read", mock.Anything).Return(0, io.EOF)

		s := term.Variable("Stream")
		env := term.Env{
			{
				Variable: s,
				Value: &term.Stream{
					Source:    bufio.NewReader(&r),
					EofAction: term.EofActionError,
				},
			},
		}

		var vm VM
		ok, err := vm.ReadTerm(s, term.NewVariable(), term.List(), Success, &env).Force()
		assert.Equal(t, permissionErrorInputPastEndOfStream(s), err)
		assert.False(t, ok)
	})

	t.Run("one or more characters were input, but they cannot be parsed as a sequence of tokens", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.ReadTerm(&term.Stream{Source: bufio.NewReader(strings.NewReader("foo bar baz."))}, term.NewVariable(), term.List(), Success, &env).Force()
		assert.Equal(t, syntaxErrorUnexpectedChar(term.Atom("unexpected char: b")), err)
		assert.False(t, ok)
	})

	t.Run("the sequence of tokens cannot be parsed as a term using the current set of operator definitions", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.ReadTerm(&term.Stream{Source: bufio.NewReader(strings.NewReader("X = a."))}, term.NewVariable(), term.List(), Success, &env).Force()
		assert.Equal(t, syntaxErrorUnexpectedChar(term.Atom("unexpected char: =")), err)
		assert.False(t, ok)
	})
}

func TestVM_GetByte(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		s := term.Stream{Source: strings.NewReader("a"), StreamType: term.StreamTypeBinary}

		env := term.Env{}
		v := term.Variable("Byte")

		var vm VM
		ok, err := vm.GetByte(&s, v, func(env term.Env) *nondet.Promise {
			assert.Equal(t, term.Integer(97), env.Resolve(v))
			return nondet.Bool(true)
		}, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("valid stream alias", func(t *testing.T) {
		s := term.Stream{Source: strings.NewReader("a"), StreamType: term.StreamTypeBinary}

		env := term.Env{}
		v := term.Variable("Byte")

		vm := VM{
			streams: map[term.Interface]*term.Stream{
				term.Atom("foo"): &s,
			},
		}
		ok, err := vm.GetByte(term.Atom("foo"), v, func(env term.Env) *nondet.Promise {
			assert.Equal(t, term.Integer(97), env.Resolve(v))
			return nondet.Bool(true)
		}, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("eof", func(t *testing.T) {
		s := term.Stream{Source: strings.NewReader(""), StreamType: term.StreamTypeBinary}

		env := term.Env{}
		v := term.Variable("Byte")

		var vm VM
		ok, err := vm.GetByte(&s, v, func(env term.Env) *nondet.Promise {
			assert.Equal(t, term.Integer(-1), env.Resolve(v))
			return nondet.Bool(true)
		}, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("error", func(t *testing.T) {
		env := term.Env{}

		var m mockReader
		m.On("Read", make([]byte, 1)).Return(0, errors.New("failed")).Once()
		defer m.AssertExpectations(t)

		s := term.Stream{Source: &m, StreamType: term.StreamTypeBinary}

		var vm VM

		v := term.Variable("V")
		_, err := vm.GetByte(&s, v, Success, &env).Force()
		assert.Error(t, err)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		env := term.Env{}
		streamOrAlias := term.Variable("Stream")
		var vm VM
		ok, err := vm.GetByte(streamOrAlias, term.Variable("InByte"), Success, &env).Force()
		assert.Equal(t, instantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("inByte is neither a variable nor an in-byte", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.GetByte(&term.Stream{Source: &mockReader{}, StreamType: term.StreamTypeBinary}, term.Atom("inByte"), Success, &env).Force()
		assert.Equal(t, typeErrorInByte(term.Atom("inByte")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream-term or alias", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.GetByte(term.Integer(0), term.Variable("InByte"), Success, &env).Force()
		assert.Equal(t, domainErrorStreamOrAlias(term.Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.GetByte(term.Atom("foo"), term.Variable("InByte"), Success, &env).Force()
		assert.Equal(t, existenceErrorStream(term.Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		streamOrAlias := term.Variable("Stream")
		env := term.Env{
			{
				Variable: streamOrAlias,
				Value:    &term.Stream{Sink: &mockWriter{}},
			},
		}

		var vm VM
		ok, err := vm.GetByte(streamOrAlias, term.Variable("InByte"), Success, &env).Force()
		assert.Equal(t, permissionErrorInputStream(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a text stream", func(t *testing.T) {
		streamOrAlias := term.Variable("Stream")
		env := term.Env{
			{
				Variable: streamOrAlias,
				Value:    &term.Stream{Source: &mockReader{}},
			},
		}

		var vm VM
		ok, err := vm.GetByte(streamOrAlias, term.Variable("InByte"), Success, &env).Force()
		assert.Equal(t, permissionErrorInputTextStream(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias has stream properties end_of_stream(past) and eof_action(error)", func(t *testing.T) {
		var r mockReader
		r.On("Read", mock.Anything).Return(0, io.EOF)

		streamOrAlias := term.Variable("Stream")
		env := term.Env{
			{
				Variable: streamOrAlias,
				Value: &term.Stream{
					Source:     &r,
					StreamType: term.StreamTypeBinary,
					EofAction:  term.EofActionError,
				},
			},
		}

		var vm VM
		ok, err := vm.GetByte(streamOrAlias, term.Variable("InByte"), Success, &env).Force()
		assert.Equal(t, permissionErrorInputPastEndOfStream(streamOrAlias), err)
		assert.False(t, ok)
	})
}

func TestVM_GetChar(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		s := term.Stream{Source: bufio.NewReader(strings.NewReader("😀"))}

		env := term.Env{}
		v := term.Variable("Char")

		var vm VM
		ok, err := vm.GetChar(&s, v, func(env term.Env) *nondet.Promise {
			assert.Equal(t, term.Atom("😀"), env.Resolve(v))
			return nondet.Bool(true)
		}, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("valid stream alias", func(t *testing.T) {
		s := term.Stream{Source: bufio.NewReader(strings.NewReader("😀"))}

		env := term.Env{}
		v := term.Variable("Char")

		vm := VM{
			streams: map[term.Interface]*term.Stream{
				term.Atom("foo"): &s,
			},
		}
		ok, err := vm.GetChar(term.Atom("foo"), v, func(env term.Env) *nondet.Promise {
			assert.Equal(t, term.Atom("😀"), env.Resolve(v))
			return nondet.Bool(true)
		}, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("non buffered stream", func(t *testing.T) {
		s := term.Variable("Stream")
		env := term.Env{
			{
				Variable: s,
				Value:    &term.Stream{Source: strings.NewReader("")},
			},
		}

		var vm VM
		ok, err := vm.GetChar(s, term.NewVariable(), Success, &env).Force()
		assert.Equal(t, permissionErrorInputBufferedStream(s), err)
		assert.False(t, ok)
	})

	t.Run("eof", func(t *testing.T) {
		s := term.Stream{Source: bufio.NewReader(strings.NewReader(""))}

		env := term.Env{}
		v := term.Variable("Char")

		var vm VM
		ok, err := vm.GetChar(&s, v, func(env term.Env) *nondet.Promise {
			assert.Equal(t, term.Atom("end_of_file"), env.Resolve(v))
			return nondet.Bool(true)
		}, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("error", func(t *testing.T) {
		env := term.Env{}

		var m mockReader
		m.On("Read", mock.Anything).Return(0, errors.New("failed")).Once()
		defer m.AssertExpectations(t)

		s := term.Stream{Source: bufio.NewReader(&m)}

		v := term.Variable("V")

		var vm VM
		ok, err := vm.GetChar(&s, v, Success, &env).Force()
		assert.Equal(t, systemError(errors.New("failed")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		env := term.Env{}
		streamOrAlias := term.Variable("Stream")

		var vm VM
		ok, err := vm.GetChar(streamOrAlias, term.Variable("Char"), Success, &env).Force()
		assert.Equal(t, instantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("char is neither a variable nor an in-character", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.GetChar(&term.Stream{Source: bufio.NewReader(&mockReader{})}, term.Integer(0), Success, &env).Force()
		assert.Equal(t, typeErrorInCharacter(term.Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.GetChar(term.Integer(0), term.Variable("Char"), Success, &env).Force()
		assert.Equal(t, domainErrorStreamOrAlias(term.Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		streamOrAlias := term.Variable("Stream")
		env := term.Env{
			{
				Variable: streamOrAlias,
				Value:    &term.Stream{Sink: &mockWriter{}},
			},
		}

		var vm VM
		ok, err := vm.GetChar(streamOrAlias, term.Variable("Char"), Success, &env).Force()
		assert.Equal(t, permissionErrorInputStream(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a binary stream", func(t *testing.T) {
		streamOrAlias := term.Variable("Stream")
		env := term.Env{
			{
				Variable: streamOrAlias,
				Value:    &term.Stream{Source: bufio.NewReader(&mockReader{}), StreamType: term.StreamTypeBinary},
			},
		}

		var vm VM
		ok, err := vm.GetChar(streamOrAlias, term.Variable("Char"), Success, &env).Force()
		assert.Equal(t, permissionErrorInputBinaryStream(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias has stream properties end_of_stream(past) and eof_action(error)", func(t *testing.T) {
		var r mockReader
		r.On("Read", mock.Anything).Return(0, io.EOF)

		streamOrAlias := term.Variable("Stream")
		env := term.Env{
			{
				Variable: streamOrAlias,
				Value: &term.Stream{
					Source:    bufio.NewReader(&r),
					EofAction: term.EofActionError,
				},
			},
		}

		var vm VM
		ok, err := vm.GetChar(streamOrAlias, term.Variable("Char"), Success, &env).Force()
		assert.Equal(t, permissionErrorInputPastEndOfStream(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("the entity input from the stream is not a character", func(t *testing.T) {
		streamOrAlias := term.Variable("Stream")
		env := term.Env{
			{
				Variable: streamOrAlias,
				Value:    &term.Stream{Source: bufio.NewReader(bytes.NewBufferString(string(unicode.ReplacementChar)))},
			},
		}

		var vm VM
		ok, err := vm.GetChar(streamOrAlias, term.Variable("Char"), Success, &env).Force()
		assert.Equal(t, representationError(term.Atom("character"), term.Atom("invalid character.")), err)
		assert.False(t, ok)
	})
}

func TestVM_PeekByte(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		s := term.Stream{Source: bufio.NewReader(strings.NewReader("abc")), StreamType: term.StreamTypeBinary}

		env := term.Env{}
		v := term.Variable("Byte")

		var vm VM
		ok, err := vm.PeekByte(&s, v, func(env term.Env) *nondet.Promise {
			assert.Equal(t, term.Integer(97), env.Resolve(v))
			return nondet.Bool(true)
		}, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = vm.PeekByte(&s, v, Success, &env).Force() // 'a' again
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("valid stream alias", func(t *testing.T) {
		s := term.Stream{Source: bufio.NewReader(strings.NewReader("abc")), StreamType: term.StreamTypeBinary}

		env := term.Env{}
		v := term.Variable("Byte")

		vm := VM{
			streams: map[term.Interface]*term.Stream{
				term.Atom("foo"): &s,
			},
		}
		ok, err := vm.PeekByte(term.Atom("foo"), v, func(env term.Env) *nondet.Promise {
			assert.Equal(t, term.Integer(97), env.Resolve(v))
			return nondet.Bool(true)
		}, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("non buffered stream", func(t *testing.T) {
		s := term.Variable("Stream")
		env := term.Env{
			{
				Variable: s,
				Value:    &term.Stream{Source: strings.NewReader(""), StreamType: term.StreamTypeBinary},
			},
		}

		var vm VM
		ok, err := vm.PeekByte(s, term.NewVariable(), Success, &env).Force()
		assert.Equal(t, permissionErrorInputBufferedStream(s), err)
		assert.False(t, ok)
	})

	t.Run("eof", func(t *testing.T) {
		s := term.Stream{Source: bufio.NewReader(strings.NewReader("")), StreamType: term.StreamTypeBinary}

		env := term.Env{}
		v := term.Variable("Byte")

		var vm VM
		ok, err := vm.PeekByte(&s, v, func(env term.Env) *nondet.Promise {
			assert.Equal(t, term.Integer(-1), env.Resolve(v))
			return nondet.Bool(true)
		}, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("error", func(t *testing.T) {
		env := term.Env{}

		var m mockReader
		m.On("Read", mock.Anything).Return(0, errors.New("failed")).Once()
		defer m.AssertExpectations(t)

		s := term.Stream{Source: bufio.NewReader(&m), StreamType: term.StreamTypeBinary}

		v := term.Variable("V")

		var vm VM
		ok, err := vm.PeekByte(&s, v, Success, &env).Force()
		assert.Equal(t, systemError(errors.New("failed")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		env := term.Env{}
		streamOrAlias := term.Variable("Stream")

		var vm VM
		ok, err := vm.PeekByte(streamOrAlias, term.Variable("Byte"), Success, &env).Force()
		assert.Equal(t, instantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("inByte is neither a variable nor an in-byte", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.PeekByte(&term.Stream{Source: bufio.NewReader(&mockReader{}), StreamType: term.StreamTypeBinary}, term.Atom("byte"), Success, &env).Force()
		assert.Equal(t, typeErrorInByte(term.Atom("byte")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.PeekByte(term.Integer(0), term.Variable("Byte"), Success, &env).Force()
		assert.Equal(t, domainErrorStreamOrAlias(term.Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		streamOrAlias := term.Variable("Stream")
		env := term.Env{
			{
				Variable: streamOrAlias,
				Value:    &term.Stream{Sink: &mockWriter{}},
			},
		}

		var vm VM
		ok, err := vm.PeekByte(streamOrAlias, term.Variable("Byte"), Success, &env).Force()
		assert.Equal(t, permissionErrorInputStream(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a text stream", func(t *testing.T) {
		streamOrAlias := term.Variable("Stream")
		env := term.Env{
			{
				Variable: streamOrAlias,
				Value:    &term.Stream{Source: bufio.NewReader(&mockReader{}), StreamType: term.StreamTypeText},
			},
		}

		var vm VM
		ok, err := vm.PeekByte(streamOrAlias, term.Variable("Byte"), Success, &env).Force()
		assert.Equal(t, permissionErrorInputTextStream(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias has stream properties end_of_stream(past) and eof_action(error)", func(t *testing.T) {
		var r mockReader
		r.On("Read", mock.Anything).Return(0, io.EOF)

		streamOrAlias := term.Variable("Stream")
		env := term.Env{
			{
				Variable: streamOrAlias,
				Value: &term.Stream{
					Source:     bufio.NewReader(&r),
					StreamType: term.StreamTypeBinary,
					EofAction:  term.EofActionError,
				},
			},
		}

		var vm VM
		ok, err := vm.PeekByte(streamOrAlias, term.Variable("Byte"), Success, &env).Force()
		assert.Equal(t, permissionErrorInputPastEndOfStream(streamOrAlias), err)
		assert.False(t, ok)
	})
}

func TestVM_PeekChar(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		s := term.Stream{Source: bufio.NewReader(strings.NewReader("😀❗"))}

		env := term.Env{}
		v := term.Variable("Char")

		var vm VM
		ok, err := vm.PeekChar(&s, v, func(env term.Env) *nondet.Promise {
			assert.Equal(t, term.Atom("😀"), env.Resolve(v))
			return nondet.Bool(true)
		}, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = vm.PeekChar(&s, v, Success, &env).Force() // '😀' again
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("valid stream alias", func(t *testing.T) {
		s := term.Stream{Source: bufio.NewReader(strings.NewReader("😀❗"))}

		env := term.Env{}
		v := term.Variable("Char")

		vm := VM{
			streams: map[term.Interface]*term.Stream{
				term.Atom("foo"): &s,
			},
		}
		ok, err := vm.PeekChar(term.Atom("foo"), v, func(env term.Env) *nondet.Promise {
			assert.Equal(t, term.Atom("😀"), env.Resolve(v))
			return nondet.Bool(true)
		}, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("non buffered stream", func(t *testing.T) {
		s := term.Variable("Stream")
		env := term.Env{
			{
				Variable: s,
				Value:    &term.Stream{Source: strings.NewReader("")},
			},
		}

		var vm VM
		ok, err := vm.PeekChar(s, term.NewVariable(), Success, &env).Force()
		assert.Equal(t, permissionErrorInputBufferedStream(s), err)
		assert.False(t, ok)
	})

	t.Run("eof", func(t *testing.T) {
		s := term.Stream{Source: bufio.NewReader(strings.NewReader(""))}

		env := term.Env{}
		v := term.Variable("Char")

		var vm VM
		ok, err := vm.PeekChar(&s, v, func(env term.Env) *nondet.Promise {
			assert.Equal(t, term.Atom("end_of_file"), env.Resolve(v))
			return nondet.Bool(true)
		}, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("error", func(t *testing.T) {
		env := term.Env{}

		var m mockReader
		m.On("Read", mock.Anything).Return(0, errors.New("failed")).Once()
		defer m.AssertExpectations(t)

		s := term.Stream{Source: bufio.NewReader(&m)}

		v := term.Variable("V")

		var vm VM
		ok, err := vm.PeekChar(&s, v, Success, &env).Force()
		assert.Equal(t, systemError(errors.New("failed")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		env := term.Env{}
		streamOrAlias := term.Variable("Stream")

		var vm VM
		ok, err := vm.PeekChar(streamOrAlias, term.Variable("Char"), Success, &env).Force()
		assert.Equal(t, instantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("char is neither a variable nor an in-character", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.PeekChar(&term.Stream{Source: bufio.NewReader(&mockReader{})}, term.Integer(0), Success, &env).Force()
		assert.Equal(t, typeErrorInCharacter(term.Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.PeekChar(term.Integer(0), term.Variable("Char"), Success, &env).Force()
		assert.Equal(t, domainErrorStreamOrAlias(term.Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		streamOrAlias := term.Variable("Stream")
		env := term.Env{
			{
				Variable: streamOrAlias,
				Value:    &term.Stream{Sink: &mockWriter{}},
			},
		}

		var vm VM
		ok, err := vm.PeekChar(streamOrAlias, term.Variable("Char"), Success, &env).Force()
		assert.Equal(t, permissionErrorInputStream(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a binary stream", func(t *testing.T) {
		streamOrAlias := term.Variable("Stream")
		env := term.Env{
			{
				Variable: streamOrAlias,
				Value: &term.Stream{
					Source:     bufio.NewReader(&mockReader{}),
					StreamType: term.StreamTypeBinary,
				},
			},
		}

		var vm VM
		ok, err := vm.PeekChar(streamOrAlias, term.Variable("Char"), Success, &env).Force()
		assert.Equal(t, permissionErrorInputBinaryStream(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias has stream properties end_of_stream(past) and eof_action(error)", func(t *testing.T) {
		var r mockReader
		r.On("Read", mock.Anything).Return(0, io.EOF)

		streamOrAlias := term.Variable("Stream")
		env := term.Env{
			{
				Variable: streamOrAlias,
				Value: &term.Stream{
					Source:    bufio.NewReader(&r),
					EofAction: term.EofActionError,
				},
			},
		}

		var vm VM
		ok, err := vm.PeekChar(streamOrAlias, term.Variable("Char"), Success, &env).Force()
		assert.Equal(t, permissionErrorInputPastEndOfStream(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("the entity input from the stream is not a character", func(t *testing.T) {
		streamOrAlias := term.Variable("Stream")
		env := term.Env{
			{
				Variable: streamOrAlias,
				Value:    &term.Stream{Source: bufio.NewReader(bytes.NewBufferString(string(unicode.ReplacementChar)))},
			},
		}

		var vm VM
		ok, err := vm.PeekChar(streamOrAlias, term.Variable("Char"), Success, &env).Force()
		assert.Equal(t, representationError(term.Atom("character"), term.Atom("invalid character.")), err)
		assert.False(t, ok)
	})
}

func TestVM_Halt(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		env := term.Env{}
		var exitCalled bool
		osExit = func(code int) {
			assert.Equal(t, 2, code)
			exitCalled = true
		}
		defer func() {
			osExit = os.Exit
		}()

		var vm VM
		ok, err := vm.Halt(term.Integer(2), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.True(t, exitCalled)
	})

	t.Run("n is a variable", func(t *testing.T) {
		env := term.Env{}
		n := term.Variable("N")

		var vm VM
		ok, err := vm.Halt(n, Success, &env).Force()
		assert.Equal(t, instantiationError(n), err)
		assert.False(t, ok)
	})

	t.Run("n is neither a variable nor an integer", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.Halt(term.Atom("foo"), Success, &env).Force()
		assert.Equal(t, typeErrorInteger(term.Atom("foo")), err)
		assert.False(t, ok)
	})
}

func TestVM_Clause(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		env := term.Env{}
		x := term.Variable("X")
		what, body := term.Variable("What"), term.Variable("Body")

		var c int

		vm := VM{
			procedures: map[procedureIndicator]procedure{
				{name: "green", arity: 1}: clauses{
					{raw: &term.Compound{
						Functor: ":-", Args: []term.Interface{
							&term.Compound{Functor: "green", Args: []term.Interface{x}},
							&term.Compound{Functor: "moldy", Args: []term.Interface{x}},
						},
					}},
					{raw: &term.Compound{Functor: "green", Args: []term.Interface{term.Atom("kermit")}}},
				},
			},
		}
		ok, err := vm.Clause(&term.Compound{
			Functor: "green",
			Args:    []term.Interface{what},
		}, body, func(env term.Env) *nondet.Promise {
			switch c {
			case 0:
				assert.True(t, env.Resolve(what).(term.Variable).Anonymous())
				b, ok := env.Resolve(body).(*term.Compound)
				assert.True(t, ok)
				assert.Equal(t, term.Atom("moldy"), b.Functor)
				assert.Len(t, b.Args, 1)
				assert.True(t, b.Args[0].(term.Variable).Anonymous())
			case 1:
				assert.Equal(t, term.Atom("kermit"), env.Resolve(what))
				assert.Equal(t, term.Atom("true"), env.Resolve(body))
			default:
				assert.Fail(t, "unreachable")
			}
			c++
			return nondet.Bool(false)
		}, &env).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("head is a variable", func(t *testing.T) {
		env := term.Env{}
		head := term.Variable("Head")

		var vm VM
		ok, err := vm.Clause(head, term.Atom("true"), Success, &env).Force()
		assert.Equal(t, instantiationError(head), err)
		assert.False(t, ok)
	})

	t.Run("head is neither a variable nor a predication", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.Clause(term.Integer(0), term.Atom("true"), Success, &env).Force()
		assert.Equal(t, typeErrorCallable(term.Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("the predicate indicator Pred of Head is that of a private (ie. Not public) procedure", func(t *testing.T) {
		// TODO: we haven't introduced the concept of private procedure yet.
	})

	t.Run("body is neither a variable nor a callable term", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.Clause(term.Atom("foo"), term.Integer(0), Success, &env).Force()
		assert.Equal(t, typeErrorCallable(term.Integer(0)), err)
		assert.False(t, ok)
	})
}

func TestAtomLength(t *testing.T) {
	t.Run("ascii", func(t *testing.T) {
		env := term.Env{}
		ok, err := AtomLength(term.Atom("abc"), term.Integer(3), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("emoji", func(t *testing.T) {
		env := term.Env{}
		ok, err := AtomLength(term.Atom("😀"), term.Integer(1), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("atom is a variable", func(t *testing.T) {
		env := term.Env{}
		atom := term.Variable("Atom")
		ok, err := AtomLength(atom, term.Integer(0), Success, &env).Force()
		assert.Equal(t, instantiationError(atom), err)
		assert.False(t, ok)
	})

	t.Run("atom is neither a variable nor an atom", func(t *testing.T) {
		env := term.Env{}
		ok, err := AtomLength(term.Integer(2), term.Integer(0), Success, &env).Force()
		assert.Equal(t, typeErrorAtom(term.Integer(2)), err)
		assert.False(t, ok)
	})

	t.Run("length is neither a variable nor an integer", func(t *testing.T) {
		env := term.Env{}
		ok, err := AtomLength(term.Atom("😀"), term.Atom("1"), Success, &env).Force()
		assert.Equal(t, typeErrorInteger(term.Atom("1")), err)
		assert.False(t, ok)
	})

	t.Run("length is an integer less than zero", func(t *testing.T) {
		env := term.Env{}
		ok, err := AtomLength(term.Atom("😀"), term.Integer(-1), Success, &env).Force()
		assert.Equal(t, domainErrorNotLessThanZero(term.Integer(-1)), err)
		assert.False(t, ok)
	})
}

func TestAtomConcat(t *testing.T) {
	t.Run("atom3 is a variable", func(t *testing.T) {
		env := term.Env{}
		atom3 := term.Variable("Atom3")

		ok, err := AtomConcat(term.Atom("foo"), term.Atom("bar"), atom3, func(env term.Env) *nondet.Promise {
			assert.Equal(t, term.Atom("foobar"), env.Resolve(atom3))
			return nondet.Bool(true)
		}, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("atom3 is an atom", func(t *testing.T) {
		env := term.Env{}
		var c int
		v1, v2 := term.Variable("V1"), term.Variable("V2")
		ok, err := AtomConcat(v1, v2, term.Atom("foo"), func(env term.Env) *nondet.Promise {
			switch c {
			case 0:
				assert.Equal(t, term.Atom(""), env.Resolve(v1))
				assert.Equal(t, term.Atom("foo"), env.Resolve(v2))
			case 1:
				assert.Equal(t, term.Atom("f"), env.Resolve(v1))
				assert.Equal(t, term.Atom("oo"), env.Resolve(v2))
			case 2:
				assert.Equal(t, term.Atom("fo"), env.Resolve(v1))
				assert.Equal(t, term.Atom("o"), env.Resolve(v2))
			case 3:
				assert.Equal(t, term.Atom("foo"), env.Resolve(v1))
				assert.Equal(t, term.Atom(""), env.Resolve(v2))
			default:
				assert.Fail(t, "unreachable")
			}
			c++
			return nondet.Bool(false)
		}, &env).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("atom1 and atom3 are variables", func(t *testing.T) {
		env := term.Env{}
		atom1, atom3 := term.Variable("Atom1"), term.Variable("Atom3")

		ok, err := AtomConcat(atom1, term.Atom("bar"), atom3, Success, &env).Force()
		assert.Equal(t, instantiationError(&term.Compound{
			Functor: ",",
			Args:    []term.Interface{atom1, atom3},
		}), err)
		assert.False(t, ok)
	})

	t.Run("atom2 and atom3 are variables", func(t *testing.T) {
		env := term.Env{}
		atom2, atom3 := term.Variable("Atom2"), term.Variable("Atom3")

		ok, err := AtomConcat(term.Atom("foo"), atom2, atom3, Success, &env).Force()
		assert.Equal(t, instantiationError(&term.Compound{
			Functor: ",",
			Args:    []term.Interface{atom2, atom3},
		}), err)
		assert.False(t, ok)
	})

	t.Run("atom1 is neither a variable nor an atom", func(t *testing.T) {
		t.Run("atom3 is a variable", func(t *testing.T) {
			env := term.Env{}
			ok, err := AtomConcat(term.Integer(1), term.Atom("bar"), term.Variable("Atom3"), Success, &env).Force()
			assert.Equal(t, typeErrorAtom(term.Integer(1)), err)
			assert.False(t, ok)
		})

		t.Run("atom3 is an atom", func(t *testing.T) {
			env := term.Env{}
			ok, err := AtomConcat(term.Integer(1), term.Atom("bar"), term.Atom("foobar"), Success, &env).Force()
			assert.Equal(t, typeErrorAtom(term.Integer(1)), err)
			assert.False(t, ok)
		})
	})

	t.Run("atom2 is neither a variable nor an atom", func(t *testing.T) {
		t.Run("atom3 is a variable", func(t *testing.T) {
			env := term.Env{}
			ok, err := AtomConcat(term.Atom("foo"), term.Integer(2), term.Variable("Atom3"), Success, &env).Force()
			assert.Equal(t, typeErrorAtom(term.Integer(2)), err)
			assert.False(t, ok)
		})

		t.Run("atom3 is an atom", func(t *testing.T) {
			env := term.Env{}
			ok, err := AtomConcat(term.Atom("foo"), term.Integer(2), term.Atom("foobar"), Success, &env).Force()
			assert.Equal(t, typeErrorAtom(term.Integer(2)), err)
			assert.False(t, ok)
		})
	})

	t.Run("atom3 is neither a variable nor an atom", func(t *testing.T) {
		env := term.Env{}
		ok, err := AtomConcat(term.Atom("foo"), term.Atom("bar"), term.Integer(3), Success, &env).Force()
		assert.Equal(t, typeErrorAtom(term.Integer(3)), err)
		assert.False(t, ok)
	})
}

func TestSubAtom(t *testing.T) {
	t.Run("multiple solutions", func(t *testing.T) {
		env := term.Env{}
		before, length, after := term.Variable("Before"), term.Variable("Length"), term.Variable("After")
		var c int
		ok, err := SubAtom(term.Atom("xATGATGAxATGAxATGAx"), before, length, after, term.Atom("ATGA"), func(env term.Env) *nondet.Promise {
			switch c {
			case 0:
				assert.Equal(t, term.Integer(1), env.Resolve(before))
				assert.Equal(t, term.Integer(4), env.Resolve(length))
				assert.Equal(t, term.Integer(14), env.Resolve(after))
			case 1:
				assert.Equal(t, term.Integer(4), env.Resolve(before))
				assert.Equal(t, term.Integer(4), env.Resolve(length))
				assert.Equal(t, term.Integer(11), env.Resolve(after))
			case 2:
				assert.Equal(t, term.Integer(9), env.Resolve(before))
				assert.Equal(t, term.Integer(4), env.Resolve(length))
				assert.Equal(t, term.Integer(6), env.Resolve(after))
			case 3:
				assert.Equal(t, term.Integer(14), env.Resolve(before))
				assert.Equal(t, term.Integer(4), env.Resolve(length))
				assert.Equal(t, term.Integer(1), env.Resolve(after))
			default:
				assert.Fail(t, "unreachable")
			}
			c++
			return nondet.Bool(false)
		}, &env).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("get the first char", func(t *testing.T) {
		env := term.Env{}
		char := term.Variable("Char")
		ok, err := SubAtom(term.Atom("a"), term.Integer(0), term.Integer(1), term.Integer(0), char, func(env term.Env) *nondet.Promise {
			assert.Equal(t, term.Atom("a"), env.Resolve(char))
			return nondet.Bool(true)
		}, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("atom is a variable", func(t *testing.T) {
		env := term.Env{}
		atom := term.Variable("Atom")
		ok, err := SubAtom(atom, term.Variable("Before"), term.Variable("Length"), term.Variable("After"), term.Variable("SubAtom"), Success, &env).Force()
		assert.Equal(t, instantiationError(atom), err)
		assert.False(t, ok)
	})

	t.Run("atom is neither a variable nor an atom", func(t *testing.T) {
		env := term.Env{}
		ok, err := SubAtom(term.Integer(0), term.Variable("Before"), term.Variable("Length"), term.Variable("After"), term.Variable("SubAtom"), Success, &env).Force()
		assert.Equal(t, typeErrorAtom(term.Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("subAtom is neither a variable nor an atom", func(t *testing.T) {
		env := term.Env{}
		ok, err := SubAtom(term.Atom("foo"), term.Variable("Before"), term.Variable("Length"), term.Variable("After"), term.Integer(0), Success, &env).Force()
		assert.Equal(t, typeErrorAtom(term.Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("before is neither a variable nor an integer", func(t *testing.T) {
		env := term.Env{}
		ok, err := SubAtom(term.Atom("foo"), term.Atom("before"), term.Variable("Length"), term.Variable("After"), term.Variable("SubAtom"), Success, &env).Force()
		assert.Equal(t, typeErrorInteger(term.Atom("before")), err)
		assert.False(t, ok)
	})

	t.Run("length is neither a variable nor an integer", func(t *testing.T) {
		env := term.Env{}
		ok, err := SubAtom(term.Atom("foo"), term.Variable("Before"), term.Atom("length"), term.Variable("After"), term.Variable("SubAtom"), Success, &env).Force()
		assert.Equal(t, typeErrorInteger(term.Atom("length")), err)
		assert.False(t, ok)
	})

	t.Run("after is neither a variable nor an integer", func(t *testing.T) {
		env := term.Env{}
		ok, err := SubAtom(term.Atom("foo"), term.Variable("Before"), term.Variable("Length"), term.Atom("after"), term.Variable("SubAtom"), Success, &env).Force()
		assert.Equal(t, typeErrorInteger(term.Atom("after")), err)
		assert.False(t, ok)
	})

	t.Run("before is an integer less than zero", func(t *testing.T) {
		env := term.Env{}
		ok, err := SubAtom(term.Atom("foo"), term.Integer(-1), term.Variable("Length"), term.Variable("After"), term.Variable("SubAtom"), Success, &env).Force()
		assert.Equal(t, domainErrorNotLessThanZero(term.Integer(-1)), err)
		assert.False(t, ok)
	})

	t.Run("length is an integer less than zero", func(t *testing.T) {
		env := term.Env{}
		ok, err := SubAtom(term.Atom("foo"), term.Variable("Before"), term.Integer(-1), term.Variable("After"), term.Variable("SubAtom"), Success, &env).Force()
		assert.Equal(t, domainErrorNotLessThanZero(term.Integer(-1)), err)
		assert.False(t, ok)
	})

	t.Run("after is an integer less than zero", func(t *testing.T) {
		env := term.Env{}
		ok, err := SubAtom(term.Atom("foo"), term.Variable("Before"), term.Variable("Length"), term.Integer(-1), term.Variable("SubAtom"), Success, &env).Force()
		assert.Equal(t, domainErrorNotLessThanZero(term.Integer(-1)), err)
		assert.False(t, ok)
	})
}

func TestAtomChars(t *testing.T) {
	t.Run("break down", func(t *testing.T) {
		env := term.Env{}
		chars := term.Variable("Char")

		ok, err := AtomChars(term.Atom("foo"), chars, func(env term.Env) *nondet.Promise {
			assert.Equal(t, term.List(term.Atom("f"), term.Atom("o"), term.Atom("o")), env.Resolve(chars))
			return nondet.Bool(true)
		}, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("construct", func(t *testing.T) {
		env := term.Env{}
		atom := term.Variable("Atom")

		ok, err := AtomChars(atom, term.List(term.Atom("f"), term.Atom("o"), term.Atom("o")), func(env term.Env) *nondet.Promise {
			assert.Equal(t, term.Atom("foo"), env.Resolve(atom))
			return nondet.Bool(true)
		}, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		_, err = AtomChars(term.NewVariable(), term.List(term.Integer(102), term.Integer(111), term.Integer(111)), Success, &env).Force()
		assert.Error(t, err)
	})

	t.Run("atom is a variable and List is a partial list or list with an element which is a variable", func(t *testing.T) {
		t.Run("partial list", func(t *testing.T) {
			env := term.Env{}
			chars := term.ListRest(term.Variable("Rest"),
				term.Atom("0"),
				term.Atom("0"),
			)

			ok, err := AtomChars(term.NewVariable(), chars, Success, &env).Force()
			assert.Equal(t, instantiationError(chars), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			env := term.Env{}
			char := term.Variable("Char")
			ok, err := AtomChars(term.NewVariable(), term.List(char, term.Atom("o"), term.Atom("o")), Success, &env).Force()
			assert.Equal(t, instantiationError(char), err)
			assert.False(t, ok)
		})
	})

	t.Run("atom is neither a variable nor an atom", func(t *testing.T) {
		env := term.Env{}
		ok, err := AtomChars(term.Integer(0), term.NewVariable(), Success, &env).Force()
		assert.Equal(t, typeErrorAtom(term.Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("atom is a variable and List is neither a list nor a partial list", func(t *testing.T) {
		env := term.Env{}
		ok, err := AtomChars(term.NewVariable(), term.Atom("chars"), Success, &env).Force()
		assert.Equal(t, typeErrorList(term.Atom("chars")), err)
		assert.False(t, ok)
	})

	t.Run("atom is a variable and an element E of the list List is neither a variable nor a one-character atom", func(t *testing.T) {
		t.Run("not a one-character atom", func(t *testing.T) {
			env := term.Env{}
			ok, err := AtomChars(term.NewVariable(), term.List(term.Atom("chars")), Success, &env).Force()
			assert.Equal(t, typeErrorCharacter(term.Atom("chars")), err)
			assert.False(t, ok)
		})

		t.Run("not an atom", func(t *testing.T) {
			env := term.Env{}
			ok, err := AtomChars(term.NewVariable(), term.List(term.Integer(0)), Success, &env).Force()
			assert.Equal(t, typeErrorCharacter(term.Integer(0)), err)
			assert.False(t, ok)
		})
	})
}

func TestAtomCodes(t *testing.T) {
	t.Run("break up", func(t *testing.T) {
		env := term.Env{}
		codes := term.Variable("Codes")

		ok, err := AtomCodes(term.Atom("foo"), codes, func(env term.Env) *nondet.Promise {
			assert.Equal(t, term.List(term.Integer(102), term.Integer(111), term.Integer(111)), env.Resolve(codes))
			return nondet.Bool(true)
		}, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("construct", func(t *testing.T) {
		env := term.Env{}
		atom := term.Variable("Atom")

		ok, err := AtomCodes(atom, term.List(term.Integer(102), term.Integer(111), term.Integer(111)), func(env term.Env) *nondet.Promise {
			assert.Equal(t, term.Atom("foo"), env.Resolve(atom))
			return nondet.Bool(true)
		}, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("atom is a variable and List is a partial list or list with an element which is a variable", func(t *testing.T) {
		t.Run("partial list", func(t *testing.T) {
			env := term.Env{}
			codes := term.ListRest(term.Variable("Rest"),
				term.Integer(111),
				term.Integer(111),
			)
			ok, err := AtomCodes(term.NewVariable(), codes, Success, &env).Force()
			assert.Equal(t, instantiationError(codes), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			env := term.Env{}
			code := term.Variable("Code")

			ok, err := AtomCodes(term.NewVariable(), term.List(code, term.Integer(111), term.Integer(111)), Success, &env).Force()
			assert.Equal(t, instantiationError(code), err)
			assert.False(t, ok)
		})
	})

	t.Run("atom is neither a variable nor an atom", func(t *testing.T) {
		env := term.Env{}
		ok, err := AtomCodes(term.Integer(0), term.List(term.Integer(102), term.Integer(111), term.Integer(111)), Success, &env).Force()
		assert.Equal(t, typeErrorAtom(term.Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("atom is a variable and List is neither a list nor a partial list", func(t *testing.T) {
		env := term.Env{}
		ok, err := AtomCodes(term.NewVariable(), term.Atom("codes"), Success, &env).Force()
		assert.Equal(t, typeErrorList(term.Atom("codes")), err)
		assert.False(t, ok)
	})

	t.Run("atom is a variable and an element E of the list List is neither a variable nor a character-code", func(t *testing.T) {
		env := term.Env{}
		ok, err := AtomCodes(term.NewVariable(), term.List(term.Atom("f"), term.Integer(111), term.Integer(111)), Success, &env).Force()
		assert.Equal(t, representationError(term.Atom("character_code"), term.Atom("invalid character code.")), err)
		assert.False(t, ok)
	})
}

func TestNumberChars(t *testing.T) {
	t.Run("number to chars", func(t *testing.T) {
		env := term.Env{}
		chars := term.Variable("Chars")

		ok, err := NumberChars(term.Float(23.4), chars, func(env term.Env) *nondet.Promise {
			assert.Equal(t, term.List(term.Atom("2"), term.Atom("3"), term.Atom("."), term.Atom("4")), env.Resolve(chars))
			return nondet.Bool(true)
		}, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("chars to number", func(t *testing.T) {
		env := term.Env{}
		num := term.Variable("Num")

		ok, err := NumberChars(num, term.List(term.Atom("2"), term.Atom("3"), term.Atom("."), term.Atom("4")), func(env term.Env) *nondet.Promise {
			assert.Equal(t, term.Float(23.4), env.Resolve(num))
			return nondet.Bool(true)
		}, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("num is a variable and chars is a partial list or list with an element which is a variable", func(t *testing.T) {
		t.Run("partial list", func(t *testing.T) {
			env := term.Env{}
			codes := term.ListRest(term.Variable("Rest"),
				term.Atom("2"), term.Atom("3"), term.Atom("."), term.Atom("4"),
			)

			ok, err := NumberChars(term.NewVariable(), codes, Success, &env).Force()
			assert.Equal(t, instantiationError(codes), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			env := term.Env{}
			code := term.Variable("Code")

			ok, err := NumberChars(term.NewVariable(), term.List(code, term.Atom("3"), term.Atom("."), term.Atom("4")), Success, &env).Force()
			assert.Equal(t, instantiationError(code), err)
			assert.False(t, ok)
		})
	})

	t.Run("num is neither a variable nor a number", func(t *testing.T) {
		env := term.Env{}
		ok, err := NumberChars(term.Atom("23.4"), term.List(term.Atom("2"), term.Atom("3"), term.Atom("."), term.Atom("4")), Success, &env).Force()
		assert.Equal(t, typeErrorNumber(term.Atom("23.4")), err)
		assert.False(t, ok)
	})

	t.Run("num is a variable and chars is neither a list nor partial list", func(t *testing.T) {
		env := term.Env{}
		ok, err := NumberChars(term.NewVariable(), term.Atom("23.4"), Success, &env).Force()
		assert.Equal(t, typeErrorList(term.Atom("23.4")), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the list chars is neither a variable nor a one-character atom", func(t *testing.T) {
		env := term.Env{}
		ok, err := NumberChars(term.NewVariable(), term.List(term.Integer(2), term.Atom("3"), term.Atom("."), term.Atom("4")), Success, &env).Force()
		assert.Equal(t, typeErrorCharacter(term.Integer(2)), err)
		assert.False(t, ok)
	})

	t.Run("chars is a list of one-char atoms but is not parsable as a number", func(t *testing.T) {
		env := term.Env{}
		ok, err := NumberChars(term.NewVariable(), term.List(term.Atom("f"), term.Atom("o"), term.Atom("o")), Success, &env).Force()
		assert.Equal(t, syntaxErrorNotANumber(), err)
		assert.False(t, ok)
	})
}

func TestNumberCodes(t *testing.T) {
	t.Run("number to codes", func(t *testing.T) {
		env := term.Env{}
		codes := term.Variable("Codes")

		ok, err := NumberCodes(term.Float(23.4), codes, func(env term.Env) *nondet.Promise {
			assert.Equal(t, term.List(term.Integer(50), term.Integer(51), term.Integer(46), term.Integer(52)), env.Resolve(codes))
			return nondet.Bool(true)
		}, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("codes to number", func(t *testing.T) {
		env := term.Env{}
		num := term.Variable("Num")

		ok, err := NumberCodes(num, term.List(term.Integer(50), term.Integer(51), term.Integer(46), term.Integer(52)), func(env term.Env) *nondet.Promise {
			assert.Equal(t, term.Float(23.4), env.Resolve(num))
			return nondet.Bool(true)
		}, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("num is a variable and codes is a partial list or list with an element which is a variable", func(t *testing.T) {
		t.Run("partial list", func(t *testing.T) {
			env := term.Env{}
			codes := term.ListRest(term.Variable("Rest"),
				term.Integer(50), term.Integer(51), term.Integer(46), term.Integer(52),
			)

			ok, err := NumberCodes(term.NewVariable(), codes, Success, &env).Force()
			assert.Equal(t, instantiationError(codes), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			env := term.Env{}
			code := term.Variable("Code")

			ok, err := NumberCodes(term.NewVariable(), term.List(code, term.Integer(50), term.Integer(51), term.Integer(46), term.Integer(52)), Success, &env).Force()
			assert.Equal(t, instantiationError(code), err)
			assert.False(t, ok)
		})
	})

	t.Run("num is neither a variable nor a number", func(t *testing.T) {
		env := term.Env{}
		ok, err := NumberCodes(term.Atom("23.4"), term.List(term.Integer(50), term.Integer(51), term.Integer(46), term.Integer(52)), Success, &env).Force()
		assert.Equal(t, typeErrorNumber(term.Atom("23.4")), err)
		assert.False(t, ok)
	})

	t.Run("num is a variable and codes is neither a list nor partial list", func(t *testing.T) {
		env := term.Env{}
		ok, err := NumberCodes(term.NewVariable(), term.Atom("23.4"), Success, &env).Force()
		assert.Equal(t, typeErrorList(term.Atom("23.4")), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the list codes is neither a variable nor a one-character atom", func(t *testing.T) {
		env := term.Env{}
		ok, err := NumberCodes(term.NewVariable(), term.List(term.Atom("2"), term.Integer(51), term.Integer(46), term.Integer(52)), Success, &env).Force()
		assert.Equal(t, representationError(term.Atom("character_code"), term.Atom("'2' is not a valid character code.")), err)
		assert.False(t, ok)
	})

	t.Run("codes is a list of one-char atoms but is not parsable as a number", func(t *testing.T) {
		env := term.Env{}
		ok, err := NumberCodes(term.NewVariable(), term.List(term.Integer(102), term.Integer(111), term.Integer(111)), Success, &env).Force()
		assert.Equal(t, syntaxErrorNotANumber(), err)
		assert.False(t, ok)
	})
}

func TestFunctionSet_Is(t *testing.T) {
	t.Run("addition", func(t *testing.T) {
		env := term.Env{}
		ok, err := DefaultFunctionSet.Is(term.Integer(3), &term.Compound{Functor: "+", Args: []term.Interface{term.Integer(1), term.Integer(2)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.Float(3), &term.Compound{Functor: "+", Args: []term.Interface{term.Integer(1), term.Float(2)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.Float(3), &term.Compound{Functor: "+", Args: []term.Interface{term.Float(1), term.Integer(2)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.Float(3), &term.Compound{Functor: "+", Args: []term.Interface{term.Float(1), term.Float(2)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("subtraction", func(t *testing.T) {
		env := term.Env{}
		ok, err := DefaultFunctionSet.Is(term.Integer(1), &term.Compound{Functor: "-", Args: []term.Interface{term.Integer(3), term.Integer(2)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.Float(1), &term.Compound{Functor: "-", Args: []term.Interface{term.Integer(3), term.Float(2)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.Float(1), &term.Compound{Functor: "-", Args: []term.Interface{term.Float(3), term.Integer(2)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.Float(1), &term.Compound{Functor: "-", Args: []term.Interface{term.Float(3), term.Float(2)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("multiplication", func(t *testing.T) {
		env := term.Env{}
		ok, err := DefaultFunctionSet.Is(term.Integer(6), &term.Compound{Functor: "*", Args: []term.Interface{term.Integer(3), term.Integer(2)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.Float(6), &term.Compound{Functor: "*", Args: []term.Interface{term.Integer(3), term.Float(2)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.Float(6), &term.Compound{Functor: "*", Args: []term.Interface{term.Float(3), term.Integer(2)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.Float(6), &term.Compound{Functor: "*", Args: []term.Interface{term.Float(3), term.Float(2)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("floating-point division", func(t *testing.T) {
		env := term.Env{}
		ok, err := DefaultFunctionSet.Is(term.Float(2), &term.Compound{Functor: "/", Args: []term.Interface{term.Integer(4), term.Integer(2)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.Float(2), &term.Compound{Functor: "/", Args: []term.Interface{term.Integer(4), term.Float(2)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.Float(2), &term.Compound{Functor: "/", Args: []term.Interface{term.Float(4), term.Integer(2)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.Float(2), &term.Compound{Functor: "/", Args: []term.Interface{term.Float(4), term.Float(2)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("integer division", func(t *testing.T) {
		env := term.Env{}
		ok, err := DefaultFunctionSet.Is(term.Integer(2), &term.Compound{Functor: "//", Args: []term.Interface{term.Integer(4), term.Integer(2)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.NewVariable(), &term.Compound{Functor: "//", Args: []term.Interface{term.Integer(4), term.Float(2)}}, Success, &env).Force()
		assert.Equal(t, typeErrorInteger(term.Float(2)), err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Is(term.NewVariable(), &term.Compound{Functor: "//", Args: []term.Interface{term.Float(4), term.Integer(2)}}, Success, &env).Force()
		assert.Equal(t, typeErrorInteger(term.Float(4)), err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Is(term.NewVariable(), &term.Compound{Functor: "//", Args: []term.Interface{term.Integer(4), term.Integer(0)}}, Success, &env).Force()
		assert.Equal(t, evaluationErrorZeroDivisor(), err)
		assert.False(t, ok)
	})

	t.Run("remainder", func(t *testing.T) {
		env := term.Env{}
		ok, err := DefaultFunctionSet.Is(term.Integer(-1), &term.Compound{Functor: "rem", Args: []term.Interface{term.Integer(-21), term.Integer(4)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.NewVariable(), &term.Compound{Functor: "rem", Args: []term.Interface{term.Integer(-21), term.Float(4)}}, Success, &env).Force()
		assert.Equal(t, typeErrorInteger(term.Float(4)), err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Is(term.NewVariable(), &term.Compound{Functor: "rem", Args: []term.Interface{term.Float(-21), term.Integer(4)}}, Success, &env).Force()
		assert.Equal(t, typeErrorInteger(term.Float(-21)), err)
		assert.False(t, ok)
	})

	t.Run("mod", func(t *testing.T) {
		env := term.Env{}
		ok, err := DefaultFunctionSet.Is(term.Integer(3), &term.Compound{Functor: "mod", Args: []term.Interface{term.Integer(-21), term.Integer(4)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.NewVariable(), &term.Compound{Functor: "mod", Args: []term.Interface{term.Integer(-21), term.Float(4)}}, Success, &env).Force()
		assert.Equal(t, typeErrorInteger(term.Float(4)), err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Is(term.NewVariable(), &term.Compound{Functor: "mod", Args: []term.Interface{term.Float(-21), term.Integer(4)}}, Success, &env).Force()
		assert.Equal(t, typeErrorInteger(term.Float(-21)), err)
		assert.False(t, ok)
	})

	t.Run("exponential", func(t *testing.T) {
		env := term.Env{}
		ok, err := DefaultFunctionSet.Is(term.Float(16), &term.Compound{Functor: "**", Args: []term.Interface{term.Integer(4), term.Integer(2)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.Float(16), &term.Compound{Functor: "**", Args: []term.Interface{term.Integer(4), term.Float(2)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.Float(16), &term.Compound{Functor: "**", Args: []term.Interface{term.Float(4), term.Integer(2)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.Float(16), &term.Compound{Functor: "**", Args: []term.Interface{term.Float(4), term.Float(2)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("sign reversal", func(t *testing.T) {
		env := term.Env{}
		ok, err := DefaultFunctionSet.Is(term.Integer(-2), &term.Compound{Functor: "-", Args: []term.Interface{term.Integer(2)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.Float(-2), &term.Compound{Functor: "-", Args: []term.Interface{term.Float(2)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("absolute value", func(t *testing.T) {
		env := term.Env{}
		ok, err := DefaultFunctionSet.Is(term.Float(2), &term.Compound{Functor: "abs", Args: []term.Interface{term.Integer(-2)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.Float(2), &term.Compound{Functor: "abs", Args: []term.Interface{term.Float(-2)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("arctangent", func(t *testing.T) {
		env := term.Env{}
		ok, err := DefaultFunctionSet.Is(term.Float(0), &term.Compound{Functor: "atan", Args: []term.Interface{term.Integer(0)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.Float(0), &term.Compound{Functor: "atan", Args: []term.Interface{term.Float(0)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("ceiling", func(t *testing.T) {
		env := term.Env{}
		ok, err := DefaultFunctionSet.Is(term.Float(1), &term.Compound{Functor: "ceiling", Args: []term.Interface{term.Integer(1)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.Float(1), &term.Compound{Functor: "ceiling", Args: []term.Interface{term.Float(0.9)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("cosine", func(t *testing.T) {
		env := term.Env{}
		ok, err := DefaultFunctionSet.Is(term.Float(1.0), &term.Compound{Functor: "cos", Args: []term.Interface{term.Integer(0)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.Float(1.0), &term.Compound{Functor: "cos", Args: []term.Interface{term.Float(0)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("natural antilogarithm", func(t *testing.T) {
		env := term.Env{}
		ok, err := DefaultFunctionSet.Is(term.Float(1.0), &term.Compound{Functor: "exp", Args: []term.Interface{term.Integer(0)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.Float(1.0), &term.Compound{Functor: "exp", Args: []term.Interface{term.Float(0)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("square root", func(t *testing.T) {
		env := term.Env{}
		ok, err := DefaultFunctionSet.Is(term.Float(1.0), &term.Compound{Functor: "sqrt", Args: []term.Interface{term.Integer(1)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.Float(1.0), &term.Compound{Functor: "sqrt", Args: []term.Interface{term.Float(1)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("sign", func(t *testing.T) {
		env := term.Env{}
		ok, err := DefaultFunctionSet.Is(term.Integer(1), &term.Compound{Functor: "sign", Args: []term.Interface{term.Integer(1)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.Integer(1), &term.Compound{Functor: "sign", Args: []term.Interface{term.Integer(math.MaxInt64)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.Integer(0), &term.Compound{Functor: "sign", Args: []term.Interface{term.Integer(0)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.Integer(-1), &term.Compound{Functor: "sign", Args: []term.Interface{term.Integer(-1)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.Integer(-1), &term.Compound{Functor: "sign", Args: []term.Interface{term.Integer(math.MinInt64)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.Float(1), &term.Compound{Functor: "sign", Args: []term.Interface{term.Float(1)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.Float(1), &term.Compound{Functor: "sign", Args: []term.Interface{term.Float(math.MaxFloat64)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.Float(0), &term.Compound{Functor: "sign", Args: []term.Interface{term.Float(0)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.Float(-1), &term.Compound{Functor: "sign", Args: []term.Interface{term.Float(-1)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.Float(-1), &term.Compound{Functor: "sign", Args: []term.Interface{term.Float(-math.MaxFloat64)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		v := term.Variable("N")
		ok, err = DefaultFunctionSet.Is(v, &term.Compound{Functor: "sign", Args: []term.Interface{term.Float(math.NaN())}}, func(env term.Env) *nondet.Promise {
			assert.True(t, math.IsNaN(float64(env.Resolve(v).(term.Float))))
			return nondet.Bool(true)
		}, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("float", func(t *testing.T) {
		env := term.Env{}
		ok, err := DefaultFunctionSet.Is(term.Float(1.0), &term.Compound{Functor: "float", Args: []term.Interface{term.Integer(1)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.Float(1.0), &term.Compound{Functor: "float", Args: []term.Interface{term.Float(1)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("floor", func(t *testing.T) {
		env := term.Env{}
		ok, err := DefaultFunctionSet.Is(term.Float(1), &term.Compound{Functor: "floor", Args: []term.Interface{term.Integer(1)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.Float(1), &term.Compound{Functor: "floor", Args: []term.Interface{term.Float(1.1)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("natural logarithm", func(t *testing.T) {
		env := term.Env{}
		ok, err := DefaultFunctionSet.Is(term.Float(0), &term.Compound{Functor: "log", Args: []term.Interface{term.Integer(1)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.Float(0), &term.Compound{Functor: "log", Args: []term.Interface{term.Float(1)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("sine", func(t *testing.T) {
		env := term.Env{}
		ok, err := DefaultFunctionSet.Is(term.Float(0), &term.Compound{Functor: "sin", Args: []term.Interface{term.Integer(0)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.Float(0), &term.Compound{Functor: "sin", Args: []term.Interface{term.Float(0)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("truncate", func(t *testing.T) {
		env := term.Env{}
		ok, err := DefaultFunctionSet.Is(term.Float(1), &term.Compound{Functor: "truncate", Args: []term.Interface{term.Integer(1)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.Float(1), &term.Compound{Functor: "truncate", Args: []term.Interface{term.Float(1.1)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("round", func(t *testing.T) {
		env := term.Env{}
		ok, err := DefaultFunctionSet.Is(term.Float(1), &term.Compound{Functor: "round", Args: []term.Interface{term.Integer(1)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.Float(1), &term.Compound{Functor: "round", Args: []term.Interface{term.Float(1.1)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("bit-shift right", func(t *testing.T) {
		env := term.Env{}
		ok, err := DefaultFunctionSet.Is(term.Integer(2), &term.Compound{Functor: ">>", Args: []term.Interface{term.Integer(4), term.Integer(1)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.NewVariable(), &term.Compound{Functor: ">>", Args: []term.Interface{term.Float(4), term.Integer(1)}}, Success, &env).Force()
		assert.Equal(t, typeErrorInteger(term.Float(4)), err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Is(term.NewVariable(), &term.Compound{Functor: ">>", Args: []term.Interface{term.Integer(4), term.Float(1)}}, Success, &env).Force()
		assert.Equal(t, typeErrorInteger(term.Float(1)), err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Is(term.NewVariable(), &term.Compound{Functor: ">>", Args: []term.Interface{term.Float(4), term.Float(1)}}, Success, &env).Force()
		assert.Equal(t, typeErrorInteger(term.Float(4)), err)
		assert.False(t, ok)
	})

	t.Run("bit-shift left", func(t *testing.T) {
		env := term.Env{}
		ok, err := DefaultFunctionSet.Is(term.Integer(8), &term.Compound{Functor: "<<", Args: []term.Interface{term.Integer(4), term.Integer(1)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.NewVariable(), &term.Compound{Functor: "<<", Args: []term.Interface{term.Float(4), term.Integer(1)}}, Success, &env).Force()
		assert.Equal(t, typeErrorInteger(term.Float(4)), err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Is(term.NewVariable(), &term.Compound{Functor: "<<", Args: []term.Interface{term.Integer(4), term.Float(1)}}, Success, &env).Force()
		assert.Equal(t, typeErrorInteger(term.Float(1)), err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Is(term.NewVariable(), &term.Compound{Functor: "<<", Args: []term.Interface{term.Float(4), term.Float(1)}}, Success, &env).Force()
		assert.Equal(t, typeErrorInteger(term.Float(4)), err)
		assert.False(t, ok)
	})

	t.Run("bitwise and", func(t *testing.T) {
		env := term.Env{}
		ok, err := DefaultFunctionSet.Is(term.Integer(1), &term.Compound{Functor: "/\\", Args: []term.Interface{term.Integer(5), term.Integer(1)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.NewVariable(), &term.Compound{Functor: "/\\", Args: []term.Interface{term.Float(5), term.Integer(1)}}, Success, &env).Force()
		assert.Equal(t, typeErrorInteger(term.Float(5)), err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Is(term.NewVariable(), &term.Compound{Functor: "/\\", Args: []term.Interface{term.Integer(5), term.Float(1)}}, Success, &env).Force()
		assert.Equal(t, typeErrorInteger(term.Float(1)), err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Is(term.NewVariable(), &term.Compound{Functor: "/\\", Args: []term.Interface{term.Float(5), term.Float(1)}}, Success, &env).Force()
		assert.Equal(t, typeErrorInteger(term.Float(5)), err)
		assert.False(t, ok)
	})

	t.Run("bitwise or", func(t *testing.T) {
		env := term.Env{}
		ok, err := DefaultFunctionSet.Is(term.Integer(5), &term.Compound{Functor: "\\/", Args: []term.Interface{term.Integer(4), term.Integer(1)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.NewVariable(), &term.Compound{Functor: "\\/", Args: []term.Interface{term.Float(4), term.Integer(1)}}, Success, &env).Force()
		assert.Equal(t, typeErrorInteger(term.Float(4)), err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Is(term.NewVariable(), &term.Compound{Functor: "\\/", Args: []term.Interface{term.Integer(4), term.Float(1)}}, Success, &env).Force()
		assert.Equal(t, typeErrorInteger(term.Float(1)), err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Is(term.NewVariable(), &term.Compound{Functor: "\\/", Args: []term.Interface{term.Float(4), term.Float(1)}}, Success, &env).Force()
		assert.Equal(t, typeErrorInteger(term.Float(4)), err)
		assert.False(t, ok)
	})

	t.Run("bitwise complement", func(t *testing.T) {
		env := term.Env{}
		ok, err := DefaultFunctionSet.Is(term.Integer(-1), &term.Compound{Functor: "\\", Args: []term.Interface{term.Integer(0)}}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(term.NewVariable(), &term.Compound{Functor: "\\", Args: []term.Interface{term.Float(0)}}, Success, &env).Force()
		assert.Equal(t, typeErrorInteger(term.Float(0)), err)
		assert.False(t, ok)
	})

	t.Run("expression is a variable", func(t *testing.T) {
		env := term.Env{}
		expression := term.Variable("Exp")

		ok, err := DefaultFunctionSet.Is(term.Integer(0), expression, Success, &env).Force()
		assert.Equal(t, instantiationError(expression), err)
		assert.False(t, ok)
	})
}

func TestFunctionSet_Equal(t *testing.T) {
	t.Run("same", func(t *testing.T) {
		env := term.Env{}
		ok, err := DefaultFunctionSet.Equal(term.Integer(1), term.Integer(1), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Equal(term.Float(1), term.Integer(1), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Equal(term.Integer(1), term.Float(1), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Equal(term.Float(1), term.Float(1), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("different", func(t *testing.T) {
		env := term.Env{}
		ok, err := DefaultFunctionSet.Equal(term.Integer(1), term.Integer(2), Success, &env).Force()
		assert.NoError(t, err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Equal(term.Float(1), term.Integer(2), Success, &env).Force()
		assert.NoError(t, err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Equal(term.Integer(1), term.Float(2), Success, &env).Force()
		assert.NoError(t, err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Equal(term.Float(1), term.Float(2), Success, &env).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("lhs is a variable", func(t *testing.T) {
		env := term.Env{}
		lhs := term.Variable("LHS")

		ok, err := DefaultFunctionSet.Equal(lhs, term.Integer(1), Success, &env).Force()
		assert.Equal(t, instantiationError(lhs), err)
		assert.False(t, ok)
	})

	t.Run("rhs is a variable", func(t *testing.T) {
		env := term.Env{}
		rhs := term.Variable("RHS")

		ok, err := DefaultFunctionSet.Equal(term.Integer(1), rhs, Success, &env).Force()
		assert.Equal(t, instantiationError(rhs), err)
		assert.False(t, ok)
	})
}

func TestFunctionSet_NotEqual(t *testing.T) {
	t.Run("same", func(t *testing.T) {
		env := term.Env{}
		ok, err := DefaultFunctionSet.NotEqual(term.Integer(1), term.Integer(1), Success, &env).Force()
		assert.NoError(t, err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.NotEqual(term.Float(1), term.Integer(1), Success, &env).Force()
		assert.NoError(t, err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.NotEqual(term.Integer(1), term.Float(1), Success, &env).Force()
		assert.NoError(t, err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.NotEqual(term.Float(1), term.Float(1), Success, &env).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("different", func(t *testing.T) {
		env := term.Env{}
		ok, err := DefaultFunctionSet.NotEqual(term.Integer(1), term.Integer(2), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.NotEqual(term.Float(1), term.Integer(2), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.NotEqual(term.Integer(1), term.Float(2), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.NotEqual(term.Float(1), term.Float(2), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("lhs is a variable", func(t *testing.T) {
		env := term.Env{}
		lhs := term.Variable("LHS")

		ok, err := DefaultFunctionSet.NotEqual(lhs, term.Integer(1), Success, &env).Force()
		assert.Equal(t, instantiationError(lhs), err)
		assert.False(t, ok)
	})

	t.Run("rhs is a variable", func(t *testing.T) {
		env := term.Env{}
		rhs := term.Variable("RHS")

		ok, err := DefaultFunctionSet.NotEqual(term.Integer(1), rhs, Success, &env).Force()
		assert.Equal(t, instantiationError(rhs), err)
		assert.False(t, ok)
	})
}

func TestFunctionSet_LessThan(t *testing.T) {
	env := term.Env{}
	ok, err := DefaultFunctionSet.LessThan(term.Integer(1), term.Integer(2), Success, &env).Force()
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.LessThan(term.Float(1), term.Integer(2), Success, &env).Force()
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.LessThan(term.Integer(1), term.Float(2), Success, &env).Force()
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.LessThan(term.Float(1), term.Float(2), Success, &env).Force()
	assert.NoError(t, err)
	assert.True(t, ok)
}

func TestFunctionSet_GreaterThan(t *testing.T) {
	env := term.Env{}
	ok, err := DefaultFunctionSet.GreaterThan(term.Integer(2), term.Integer(1), Success, &env).Force()
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.GreaterThan(term.Float(2), term.Integer(1), Success, &env).Force()
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.GreaterThan(term.Integer(2), term.Float(1), Success, &env).Force()
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.GreaterThan(term.Float(2), term.Float(1), Success, &env).Force()
	assert.NoError(t, err)
	assert.True(t, ok)
}

func TestFunctionSet_LessThanOrEqual(t *testing.T) {
	env := term.Env{}
	ok, err := DefaultFunctionSet.LessThanOrEqual(term.Integer(1), term.Integer(2), Success, &env).Force()
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.LessThanOrEqual(term.Float(1), term.Integer(2), Success, &env).Force()
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.LessThanOrEqual(term.Integer(1), term.Float(2), Success, &env).Force()
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.LessThanOrEqual(term.Float(1), term.Float(2), Success, &env).Force()
	assert.NoError(t, err)
	assert.True(t, ok)
}

func TestFunctionSet_GreaterThanOrEqual(t *testing.T) {
	env := term.Env{}
	ok, err := DefaultFunctionSet.GreaterThanOrEqual(term.Integer(2), term.Integer(1), Success, &env).Force()
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.GreaterThanOrEqual(term.Float(2), term.Integer(1), Success, &env).Force()
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.GreaterThanOrEqual(term.Integer(2), term.Float(1), Success, &env).Force()
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.GreaterThanOrEqual(term.Float(2), term.Float(1), Success, &env).Force()
	assert.NoError(t, err)
	assert.True(t, ok)
}

func TestVM_StreamProperty(t *testing.T) {
	f, err := ioutil.TempFile("", "")
	assert.NoError(t, err)

	defer func() {
		assert.NoError(t, os.Remove(f.Name()))
	}()

	t.Run("stream", func(t *testing.T) {
		env := term.Env{}
		expected := []term.Interface{
			&term.Compound{Functor: "mode", Args: []term.Interface{term.Atom("read")}},
			&term.Compound{Functor: "alias", Args: []term.Interface{term.Atom("null")}},
			&term.Compound{Functor: "eof_action", Args: []term.Interface{term.Atom("eof_code")}},
			term.Atom("input"),
			&term.Compound{Functor: "buffer", Args: []term.Interface{term.Atom("true")}},
			&term.Compound{Functor: "file_name", Args: []term.Interface{term.Atom(f.Name())}},
			&term.Compound{Functor: "position", Args: []term.Interface{term.Integer(0)}},
			&term.Compound{Functor: "end_of_stream", Args: []term.Interface{term.Atom("at")}},
			&term.Compound{Functor: "reposition", Args: []term.Interface{term.Atom("false")}},
			&term.Compound{Functor: "type", Args: []term.Interface{term.Atom("text")}},
		}

		v := term.Variable("V")
		c := 0
		var vm VM
		ok, err := vm.StreamProperty(&term.Stream{
			Source: bufio.NewReader(f),
			Closer: f,
			Mode:   term.StreamModeRead,
			Alias:  "null",
		}, v, func(env term.Env) *nondet.Promise {
			assert.Equal(t, expected[c], env.Resolve(v))
			c++
			return nondet.Bool(false)
		}, &env).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("stream alias", func(t *testing.T) {
		env := term.Env{}
		expected := []term.Interface{
			&term.Compound{Functor: "mode", Args: []term.Interface{term.Atom("write")}},
			&term.Compound{Functor: "alias", Args: []term.Interface{term.Atom("null")}},
			&term.Compound{Functor: "eof_action", Args: []term.Interface{term.Atom("eof_code")}},
			term.Atom("output"),
			&term.Compound{Functor: "buffer", Args: []term.Interface{term.Atom("true")}},
			&term.Compound{Functor: "file_name", Args: []term.Interface{term.Atom(f.Name())}},
			&term.Compound{Functor: "position", Args: []term.Interface{term.Integer(0)}},
			&term.Compound{Functor: "end_of_stream", Args: []term.Interface{term.Atom("at")}},
			&term.Compound{Functor: "reposition", Args: []term.Interface{term.Atom("false")}},
			&term.Compound{Functor: "type", Args: []term.Interface{term.Atom("text")}},
		}

		vm := VM{
			streams: map[term.Interface]*term.Stream{
				term.Atom("null"): {
					Sink:   bufio.NewWriter(f),
					Closer: f,
					Mode:   term.StreamModeWrite,
					Alias:  "null",
				},
			},
		}
		v := term.Variable("V")
		c := 0
		ok, err := vm.StreamProperty(term.Atom("null"), v, func(env term.Env) *nondet.Promise {
			assert.Equal(t, expected[c], env.Resolve(v))
			c++
			return nondet.Bool(false)
		}, &env).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("correct property value", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.StreamProperty(&term.Stream{Mode: term.StreamModeRead}, &term.Compound{
			Functor: "mode",
			Args:    []term.Interface{term.Atom("read")},
		}, Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("streamOrAlias is neither a variable, a stream-term, nor an alias", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.StreamProperty(term.Integer(0), term.NewVariable(), Success, &env).Force()
		assert.Equal(t, domainErrorStreamOrAlias(term.Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("property is neither a variable nor a stream property", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.StreamProperty(term.NewVariable(), term.Atom("property"), Success, &env).Force()
		assert.Equal(t, domainErrorStreamProperty(term.Atom("property")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.StreamProperty(term.Atom("foo"), term.NewVariable(), Success, &env).Force()
		assert.Equal(t, existenceErrorStream(term.Atom("foo")), err)
		assert.False(t, ok)
	})
}

func TestVM_SetStreamPosition(t *testing.T) {
	f, err := ioutil.TempFile("", "")
	assert.NoError(t, err)

	defer func() {
		assert.NoError(t, os.Remove(f.Name()))
	}()

	t.Run("ok", func(t *testing.T) {
		env := term.Env{}
		s := term.Stream{
			Source: f,
			Closer: f,
			Mode:   term.StreamModeRead,
		}

		var vm VM
		ok, err := vm.SetStreamPosition(&s, term.Integer(0), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		env := term.Env{}
		streamOrAlias := term.Variable("Stream")

		var vm VM
		ok, err := vm.SetStreamPosition(streamOrAlias, term.Integer(0), Success, &env).Force()
		assert.Equal(t, instantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("position is a variable", func(t *testing.T) {
		env := term.Env{}
		s := term.Stream{
			Source: f,
			Closer: f,
			Mode:   term.StreamModeRead,
		}
		position := term.Variable("Pos")

		var vm VM
		ok, err := vm.SetStreamPosition(&s, position, Success, &env).Force()
		assert.Equal(t, instantiationError(position), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.SetStreamPosition(term.Integer(2), term.Integer(0), Success, &env).Force()
		assert.Equal(t, domainErrorStreamOrAlias(term.Integer(2)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.SetStreamPosition(term.Atom("foo"), term.Integer(0), Success, &env).Force()
		assert.Equal(t, existenceErrorStream(term.Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias has stream property reposition(false)", func(t *testing.T) {
		s := term.Variable("Stream")
		env := term.Env{
			{
				Variable: s,
				Value: &term.Stream{
					Source: bytes.NewReader(nil),
					Mode:   term.StreamModeRead,
				},
			},
		}

		var vm VM
		ok, err := vm.SetStreamPosition(s, term.Integer(0), Success, &env).Force()
		assert.Equal(t, permissionError(term.Atom("reposition"), term.Atom("stream"), s, term.Atom("Stream is not a file.")), err)
		assert.False(t, ok)
	})
}

func TestVM_CharConversion(t *testing.T) {
	t.Run("register", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.CharConversion(term.Atom("a"), term.Atom("b"), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, 'b', vm.CharConversions['a'])
	})

	t.Run("remove", func(t *testing.T) {
		env := term.Env{}
		vm := VM{
			CharConversions: map[rune]rune{
				'a': 'b',
			},
		}
		ok, err := vm.CharConversion(term.Atom("a"), term.Atom("a"), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		_, ok = vm.CharConversions['a']
		assert.False(t, ok)
	})

	t.Run("inChar is a variable", func(t *testing.T) {
		env := term.Env{}
		inChar := term.Variable("In")

		var vm VM
		ok, err := vm.CharConversion(inChar, term.Atom("a"), Success, &env).Force()
		assert.Equal(t, instantiationError(inChar), err)
		assert.False(t, ok)
	})

	t.Run("outChar is a variable", func(t *testing.T) {
		env := term.Env{}
		outChar := term.Variable("Out")

		var vm VM
		ok, err := vm.CharConversion(term.Atom("a"), outChar, Success, &env).Force()
		assert.Equal(t, instantiationError(outChar), err)
		assert.False(t, ok)
	})

	t.Run("inChar is neither a variable nor a one character atom", func(t *testing.T) {
		t.Run("not even an atom", func(t *testing.T) {
			env := term.Env{}
			var vm VM
			ok, err := vm.CharConversion(term.Integer(0), term.Atom("a"), Success, &env).Force()
			assert.Equal(t, representationError(term.Atom("character"), term.Atom("0 is not a character.")), err)
			assert.False(t, ok)
		})

		t.Run("multi-character atom", func(t *testing.T) {
			env := term.Env{}
			var vm VM
			ok, err := vm.CharConversion(term.Atom("foo"), term.Atom("a"), Success, &env).Force()
			assert.Equal(t, representationError(term.Atom("character"), term.Atom("foo is not a character.")), err)
			assert.False(t, ok)
		})
	})

	t.Run("outChar is neither a variable nor a one character atom", func(t *testing.T) {
		t.Run("not even an atom", func(t *testing.T) {
			env := term.Env{}
			var vm VM
			ok, err := vm.CharConversion(term.Atom("a"), term.Integer(0), Success, &env).Force()
			assert.Equal(t, representationError(term.Atom("character"), term.Atom("0 is not a character.")), err)
			assert.False(t, ok)
		})

		t.Run("multi-character atom", func(t *testing.T) {
			env := term.Env{}
			var vm VM
			ok, err := vm.CharConversion(term.Atom("a"), term.Atom("foo"), Success, &env).Force()
			assert.Equal(t, representationError(term.Atom("character"), term.Atom("foo is not a character.")), err)
			assert.False(t, ok)
		})
	})
}

func TestVM_CurrentCharConversion(t *testing.T) {
	t.Run("specified", func(t *testing.T) {
		t.Run("as is", func(t *testing.T) {
			env := term.Env{}
			var vm VM
			ok, err := vm.CurrentCharConversion(term.Atom("a"), term.Atom("a"), Success, &env).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("converted", func(t *testing.T) {
			env := term.Env{}
			vm := VM{
				CharConversions: map[rune]rune{
					'a': 'b',
				},
			}
			ok, err := vm.CurrentCharConversion(term.Atom("a"), term.Atom("b"), Success, &env).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("not specified", func(t *testing.T) {
		env := term.Env{}
		x, y := term.Variable("X"), term.Variable("Y")

		var r rune
		var vm VM
		ok, err := vm.CurrentCharConversion(x, y, func(env term.Env) *nondet.Promise {
			ref, ok := env.Lookup(x)
			assert.True(t, ok)
			x, ok := ref.(term.Atom)
			assert.True(t, ok)
			assert.Len(t, []rune(x), 1)

			ref, ok = env.Lookup(y)
			assert.True(t, ok)
			y, ok := ref.(term.Atom)
			assert.True(t, ok)
			assert.Len(t, []rune(y), 1)

			assert.Equal(t, r, []rune(x)[0])
			assert.Equal(t, r, []rune(y)[0])
			r++
			return nondet.Bool(false)
		}, &env).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
		assert.Equal(t, rune(256), r)
	})

	t.Run("inChar is neither a variable nor a one character atom", func(t *testing.T) {
		t.Run("not even an atom", func(t *testing.T) {
			env := term.Env{}
			var vm VM
			ok, err := vm.CurrentCharConversion(term.Integer(0), term.Atom("b"), Success, &env).Force()
			assert.Equal(t, representationError(term.Atom("character"), term.Atom("0 is not a character.")), err)
			assert.False(t, ok)
		})

		t.Run("multi-character atom", func(t *testing.T) {
			env := term.Env{}
			var vm VM
			ok, err := vm.CurrentCharConversion(term.Atom("foo"), term.Atom("b"), Success, &env).Force()
			assert.Equal(t, representationError(term.Atom("character"), term.Atom("foo is not a character.")), err)
			assert.False(t, ok)
		})
	})

	t.Run("outChar is neither a variable nor a one character atom", func(t *testing.T) {
		t.Run("not even an atom", func(t *testing.T) {
			env := term.Env{}
			var vm VM
			ok, err := vm.CurrentCharConversion(term.Atom("a"), term.Integer(0), Success, &env).Force()
			assert.Equal(t, representationError(term.Atom("character"), term.Atom("0 is not a character.")), err)
			assert.False(t, ok)
		})

		t.Run("multi-character atom", func(t *testing.T) {
			env := term.Env{}
			var vm VM
			ok, err := vm.CurrentCharConversion(term.Atom("a"), term.Atom("bar"), Success, &env).Force()
			assert.Equal(t, representationError(term.Atom("character"), term.Atom("bar is not a character.")), err)
			assert.False(t, ok)
		})
	})
}

func TestVM_SetPrologFlag(t *testing.T) {
	t.Run("bounded", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.SetPrologFlag(term.Atom("bounded"), term.NewVariable(), Success, &env).Force()
		assert.Equal(t, permissionError(term.Atom("modify"), term.Atom("flag"), term.Atom("bounded"), term.Atom("bounded is not modifiable.")), err)
		assert.False(t, ok)
	})

	t.Run("max_integer", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.SetPrologFlag(term.Atom("max_integer"), term.NewVariable(), Success, &env).Force()
		assert.Equal(t, permissionError(term.Atom("modify"), term.Atom("flag"), term.Atom("max_integer"), term.Atom("max_integer is not modifiable.")), err)
		assert.False(t, ok)
	})

	t.Run("min_integer", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.SetPrologFlag(term.Atom("min_integer"), term.NewVariable(), Success, &env).Force()
		assert.Equal(t, permissionError(term.Atom("modify"), term.Atom("flag"), term.Atom("min_integer"), term.Atom("min_integer is not modifiable.")), err)
		assert.False(t, ok)
	})

	t.Run("integer_rounding_function", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.SetPrologFlag(term.Atom("integer_rounding_function"), term.NewVariable(), Success, &env).Force()
		assert.Equal(t, permissionError(term.Atom("modify"), term.Atom("flag"), term.Atom("integer_rounding_function"), term.Atom("integer_rounding_function is not modifiable.")), err)
		assert.False(t, ok)
	})

	t.Run("char_conversion", func(t *testing.T) {
		t.Run("on", func(t *testing.T) {
			env := term.Env{}
			var vm VM
			ok, err := vm.SetPrologFlag(term.Atom("char_conversion"), term.Atom("on"), Success, &env).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.True(t, vm.charConvEnabled)
		})

		t.Run("off", func(t *testing.T) {
			env := term.Env{}
			vm := VM{charConvEnabled: true}
			ok, err := vm.SetPrologFlag(term.Atom("char_conversion"), term.Atom("off"), Success, &env).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.False(t, vm.charConvEnabled)
		})
	})

	t.Run("debug", func(t *testing.T) {
		t.Run("on", func(t *testing.T) {
			env := term.Env{}
			var vm VM
			ok, err := vm.SetPrologFlag(term.Atom("debug"), term.Atom("on"), Success, &env).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.True(t, vm.debug)
		})

		t.Run("off", func(t *testing.T) {
			env := term.Env{}
			vm := VM{debug: true}
			ok, err := vm.SetPrologFlag(term.Atom("debug"), term.Atom("off"), Success, &env).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.False(t, vm.debug)
		})
	})

	t.Run("max_arity", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.SetPrologFlag(term.Atom("max_arity"), term.NewVariable(), Success, &env).Force()
		assert.Equal(t, permissionError(term.Atom("modify"), term.Atom("flag"), term.Atom("max_arity"), term.Atom("max_arity is not modifiable.")), err)
		assert.False(t, ok)
	})

	t.Run("unknown", func(t *testing.T) {
		t.Run("error", func(t *testing.T) {
			env := term.Env{}
			vm := VM{unknown: unknownFail}
			ok, err := vm.SetPrologFlag(term.Atom("unknown"), term.Atom("error"), Success, &env).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.Equal(t, unknownError, vm.unknown)
		})

		t.Run("warning", func(t *testing.T) {
			env := term.Env{}
			var vm VM
			ok, err := vm.SetPrologFlag(term.Atom("unknown"), term.Atom("warning"), Success, &env).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.Equal(t, unknownWarning, vm.unknown)
		})

		t.Run("fail", func(t *testing.T) {
			env := term.Env{}
			var vm VM
			ok, err := vm.SetPrologFlag(term.Atom("unknown"), term.Atom("fail"), Success, &env).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.Equal(t, unknownFail, vm.unknown)
		})
	})

	t.Run("flag is a variable", func(t *testing.T) {
		env := term.Env{}
		flag := term.Variable("Flag")

		var vm VM
		ok, err := vm.SetPrologFlag(flag, term.Atom("fail"), Success, &env).Force()
		assert.Equal(t, instantiationError(flag), err)
		assert.False(t, ok)
	})

	t.Run("value is a variable", func(t *testing.T) {
		env := term.Env{}
		value := term.Variable("Value")

		var vm VM
		ok, err := vm.SetPrologFlag(term.Atom("unknown"), value, Success, &env).Force()
		assert.Equal(t, instantiationError(value), err)
		assert.False(t, ok)
	})

	t.Run("flag is neither a variable nor an atom", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.SetPrologFlag(term.Integer(0), term.Atom("fail"), Success, &env).Force()
		assert.Equal(t, typeErrorAtom(term.Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("flag is an atom but an invalid flag for the processor", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.SetPrologFlag(term.Atom("foo"), term.Atom("fail"), Success, &env).Force()
		assert.Equal(t, domainErrorPrologFlag(term.Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("value is inadmissible for flag", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.SetPrologFlag(term.Atom("unknown"), term.Integer(0), Success, &env).Force()
		assert.Equal(t, domainErrorFlagValue(&term.Compound{
			Functor: "+",
			Args:    []term.Interface{term.Atom("unknown"), term.Integer(0)},
		}), err)
		assert.False(t, ok)
	})

	t.Run("value is admissible for flag but the flag is not modifiable", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.SetPrologFlag(term.Atom("bounded"), term.Atom("true"), Success, &env).Force()
		assert.Equal(t, permissionError(term.Atom("modify"), term.Atom("flag"), term.Atom("bounded"), term.Atom("bounded is not modifiable.")), err)
		assert.False(t, ok)
	})
}

func TestVM_CurrentPrologFlag(t *testing.T) {
	var vm VM

	t.Run("specified", func(t *testing.T) {
		env := term.Env{}
		ok, err := vm.CurrentPrologFlag(term.Atom("bounded"), term.Atom("true"), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = vm.CurrentPrologFlag(term.Atom("max_integer"), term.Integer(math.MaxInt64), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = vm.CurrentPrologFlag(term.Atom("min_integer"), term.Integer(math.MinInt64), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = vm.CurrentPrologFlag(term.Atom("integer_rounding_function"), term.Atom("toward_zero"), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = vm.CurrentPrologFlag(term.Atom("char_conversion"), term.Atom("off"), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = vm.CurrentPrologFlag(term.Atom("debug"), term.Atom("off"), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = vm.CurrentPrologFlag(term.Atom("max_arity"), term.Atom("unbounded"), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = vm.CurrentPrologFlag(term.Atom("unknown"), term.Atom("error"), Success, &env).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not specified", func(t *testing.T) {
		env := term.Env{}
		flag, value := term.Variable("Flag"), term.Variable("Value")
		var c int
		ok, err := vm.CurrentPrologFlag(flag, value, func(env term.Env) *nondet.Promise {
			switch c {
			case 0:
				assert.Equal(t, term.Atom("bounded"), env.Resolve(flag))
				assert.Equal(t, term.Atom("true"), env.Resolve(value))
			case 1:
				assert.Equal(t, term.Atom("max_integer"), env.Resolve(flag))
				assert.Equal(t, term.Integer(math.MaxInt64), env.Resolve(value))
			case 2:
				assert.Equal(t, term.Atom("min_integer"), env.Resolve(flag))
				assert.Equal(t, term.Integer(math.MinInt64), env.Resolve(value))
			case 3:
				assert.Equal(t, term.Atom("integer_rounding_function"), env.Resolve(flag))
				assert.Equal(t, term.Atom("toward_zero"), env.Resolve(value))
			case 4:
				assert.Equal(t, term.Atom("char_conversion"), env.Resolve(flag))
				assert.Equal(t, term.Atom("off"), env.Resolve(value))
			case 5:
				assert.Equal(t, term.Atom("debug"), env.Resolve(flag))
				assert.Equal(t, term.Atom("off"), env.Resolve(value))
			case 6:
				assert.Equal(t, term.Atom("max_arity"), env.Resolve(flag))
				assert.Equal(t, term.Atom("unbounded"), env.Resolve(value))
			case 7:
				assert.Equal(t, term.Atom("unknown"), env.Resolve(flag))
				assert.Equal(t, term.Atom(vm.unknown.String()), env.Resolve(value))
			default:
				assert.Fail(t, "unreachable")
			}
			c++
			return nondet.Bool(false)
		}, &env).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
		assert.Equal(t, 8, c)
	})

	t.Run("flag is neither a variable nor an atom", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.CurrentPrologFlag(term.Integer(0), term.Atom("error"), Success, &env).Force()
		assert.Equal(t, typeErrorAtom(term.Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("flag is an atom but an invalid flag for the processor", func(t *testing.T) {
		env := term.Env{}
		var vm VM
		ok, err := vm.CurrentPrologFlag(term.Atom("foo"), term.Atom("error"), Success, &env).Force()
		assert.Equal(t, domainErrorPrologFlag(term.Atom("foo")), err)
		assert.False(t, ok)
	})
}
