package engine

import (
	"bufio"
	"bytes"
	"context"
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

	"github.com/stretchr/testify/mock"

	"github.com/stretchr/testify/assert"
)

func TestVM_Call(t *testing.T) {
	var vm VM

	t.Run("undefined atom", func(t *testing.T) {
		ok, err := vm.Call(Atom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, existenceErrorProcedure(&Compound{
			Functor: "/",
			Args:    []Term{Atom("foo"), Integer(0)},
		}), err)
		assert.False(t, ok)
	})

	vm.procedures = map[ProcedureIndicator]procedure{{Name: "foo", Arity: 0}: clauses{}}

	t.Run("defined atom", func(t *testing.T) {
		ok, err := vm.Call(Atom("foo"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("undefined compound", func(t *testing.T) {
		ok, err := vm.Call(&Compound{Functor: "bar", Args: []Term{NewVariable(), NewVariable()}}, Success, nil).Force(context.Background())
		assert.Equal(t, existenceErrorProcedure(&Compound{
			Functor: "/",
			Args:    []Term{Atom("bar"), Integer(2)},
		}), err)
		assert.False(t, ok)
	})

	vm.procedures = map[ProcedureIndicator]procedure{{Name: "bar", Arity: 2}: clauses{}}

	t.Run("defined compound", func(t *testing.T) {
		ok, err := vm.Call(&Compound{Functor: "bar", Args: []Term{NewVariable(), NewVariable()}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("variable", func(t *testing.T) {
		t.Run("single predicate", func(t *testing.T) {
			x := Variable("X")

			ok, err := vm.Call(x, Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(x), err)
			assert.False(t, ok)
		})

		t.Run("multiple predicates", func(t *testing.T) {
			x := Variable("X")
			vm.Register0("fail", func(f func(*Env) *Promise, env *Env) *Promise {
				return Bool(false)
			})
			ok, err := vm.Call(&Compound{
				Functor: ",",
				Args: []Term{
					Atom("fail"),
					x,
				},
			}, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("not callable", func(t *testing.T) {
		t.Run("single predicate", func(t *testing.T) {
			ok, err := vm.Call(Integer(0), Success, nil).Force(context.Background())
			assert.Equal(t, typeErrorCallable(Integer(0)), err)
			assert.False(t, ok)
		})

		t.Run("multiple predicates", func(t *testing.T) {
			t.Run("conjunction", func(t *testing.T) {
				ok, err := vm.Call(&Compound{
					Functor: ",",
					Args: []Term{
						Atom("true"),
						Integer(0),
					},
				}, Success, nil).Force(context.Background())
				assert.Equal(t, typeErrorCallable(&Compound{
					Functor: ",",
					Args: []Term{
						Atom("true"),
						Integer(0),
					},
				}), err)
				assert.False(t, ok)
			})

			t.Run("disjunction", func(t *testing.T) {
				ok, err := vm.Call(&Compound{
					Functor: ";",
					Args: []Term{
						Integer(1),
						Atom("true"),
					},
				}, Success, nil).Force(context.Background())
				assert.Equal(t, typeErrorCallable(&Compound{
					Functor: ";",
					Args: []Term{
						Integer(1),
						Atom("true"),
					},
				}), err)
				assert.False(t, ok)
			})
		})
	})
}

func TestUnify(t *testing.T) {
	t.Run("unifiable", func(t *testing.T) {
		x := Variable("X")
		ok, err := Unify(x, &Compound{
			Functor: "f",
			Args:    []Term{Atom("a")},
		}, func(env *Env) *Promise {
			assert.Equal(t, &Compound{
				Functor: "f",
				Args:    []Term{Atom("a")},
			}, env.Resolve(x))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not unifiable", func(t *testing.T) {
		ok, err := Unify(Atom("a"), &Compound{
			Functor: "f",
			Args:    []Term{Atom("a")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("loop", func(t *testing.T) {
		x := Variable("X")
		ok, err := Unify(x, &Compound{
			Functor: "f",
			Args:    []Term{x},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})
}

func TestUnifyWithOccursCheck(t *testing.T) {
	t.Run("unifiable", func(t *testing.T) {
		x := Variable("X")
		ok, err := UnifyWithOccursCheck(x, &Compound{
			Functor: "f",
			Args:    []Term{Atom("a")},
		}, func(env *Env) *Promise {
			assert.Equal(t, &Compound{
				Functor: "f",
				Args:    []Term{Atom("a")},
			}, env.Resolve(x))

			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not unifiable", func(t *testing.T) {
		ok, err := UnifyWithOccursCheck(Atom("a"), &Compound{
			Functor: "f",
			Args:    []Term{Atom("a")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("loop", func(t *testing.T) {
		x := Variable("X")
		ok, err := UnifyWithOccursCheck(x, &Compound{
			Functor: "f",
			Args:    []Term{x},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestTypeVar(t *testing.T) {
	t.Run("var", func(t *testing.T) {
		ok, err := TypeVar(NewVariable(), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not var", func(t *testing.T) {
		ok, err := TypeVar(Atom("foo"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestTypeFloat(t *testing.T) {
	t.Run("float", func(t *testing.T) {
		ok, err := TypeFloat(Float(1.0), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not float", func(t *testing.T) {
		ok, err := TypeFloat(Atom("foo"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestTypeInteger(t *testing.T) {
	t.Run("integer", func(t *testing.T) {
		ok, err := TypeInteger(Integer(1), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not integer", func(t *testing.T) {
		ok, err := TypeInteger(Atom("foo"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestTypeAtom(t *testing.T) {
	t.Run("atom", func(t *testing.T) {
		ok, err := TypeAtom(Atom("foo"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not atom", func(t *testing.T) {
		ok, err := TypeAtom(Integer(1), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestTypeCompound(t *testing.T) {
	t.Run("compound", func(t *testing.T) {
		ok, err := TypeCompound(&Compound{
			Functor: "foo",
			Args:    []Term{Atom("a")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not compound", func(t *testing.T) {
		ok, err := TypeCompound(Atom("foo"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestFunctor(t *testing.T) {
	t.Run("term is instantiated", func(t *testing.T) {
		t.Run("float", func(t *testing.T) {
			name, arity := Variable("Name"), Variable("Arity")
			ok, err := Functor(Float(2.0), name, arity, func(env *Env) *Promise {
				assert.Equal(t, Float(2.0), env.Resolve(name))
				assert.Equal(t, Integer(0), env.Resolve(arity))
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("integer", func(t *testing.T) {
			name, arity := NewVariable(), NewVariable()
			ok, err := Functor(Integer(2), name, arity, func(env *Env) *Promise {
				assert.Equal(t, Integer(2), env.Resolve(name))
				assert.Equal(t, Integer(0), env.Resolve(arity))
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("atom", func(t *testing.T) {
			name, arity := NewVariable(), NewVariable()
			ok, err := Functor(Atom("foo"), name, arity, func(env *Env) *Promise {
				assert.Equal(t, Atom("foo"), env.Resolve(name))
				assert.Equal(t, Integer(0), env.Resolve(arity))
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("compound", func(t *testing.T) {
			name, arity := NewVariable(), NewVariable()
			ok, err := Functor(&Compound{
				Functor: "f",
				Args:    []Term{Atom("a"), Atom("b"), Atom("c")},
			}, name, arity, func(env *Env) *Promise {
				assert.Equal(t, Atom("f"), env.Resolve(name))
				assert.Equal(t, Integer(3), env.Resolve(arity))
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("term is not instantiated", func(t *testing.T) {
		t.Run("atom", func(t *testing.T) {
			v := NewVariable()
			ok, err := Functor(v, Atom("foo"), Integer(0), func(env *Env) *Promise {
				assert.Equal(t, Atom("foo"), env.Resolve(v))
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("compound", func(t *testing.T) {
			v := NewVariable()
			ok, err := Functor(v, Atom("f"), Integer(2), func(env *Env) *Promise {
				c, ok := env.Resolve(v).(*Compound)
				assert.True(t, ok)
				assert.Equal(t, Atom("f"), c.Functor)
				assert.Len(t, c.Args, 2)
				assert.True(t, c.Args[0].(Variable).Generated())
				assert.True(t, c.Args[1].(Variable).Generated())
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("name is a variable", func(t *testing.T) {
			name := NewVariable()
			ok, err := Functor(NewVariable(), name, Integer(2), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(name), err)
			assert.False(t, ok)
		})

		t.Run("name is not atomic", func(t *testing.T) {
			ok, err := Functor(NewVariable(), &Compound{
				Functor: "foo",
				Args: []Term{
					Atom("a"),
				},
			}, Integer(1), Success, nil).Force(context.Background())
			assert.Equal(t, typeErrorAtomic(&Compound{
				Functor: "foo",
				Args: []Term{
					Atom("a"),
				},
			}), err)
			assert.False(t, ok)
		})

		t.Run("name is not an atom", func(t *testing.T) {
			ok, err := Functor(NewVariable(), Integer(0), Integer(2), Success, nil).Force(context.Background())
			assert.Equal(t, typeErrorAtom(Integer(0)), err)
			assert.False(t, ok)
		})

		t.Run("arity is a variable", func(t *testing.T) {
			arity := NewVariable()
			ok, err := Functor(NewVariable(), Atom("f"), arity, Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(arity), err)
			assert.False(t, ok)
		})

		t.Run("arity is not an integer", func(t *testing.T) {
			ok, err := Functor(NewVariable(), Atom("f"), Float(2.0), Success, nil).Force(context.Background())
			assert.Equal(t, typeErrorInteger(Float(2.0)), err)
			assert.False(t, ok)
		})

		t.Run("arity is negative", func(t *testing.T) {
			ok, err := Functor(NewVariable(), Atom("f"), Integer(-2), Success, nil).Force(context.Background())
			assert.Equal(t, domainErrorNotLessThanZero(Integer(-2)), err)
			assert.False(t, ok)
		})
	})
}

func TestArg(t *testing.T) {
	t.Run("term is a variable", func(t *testing.T) {
		v := NewVariable()
		ok, err := Arg(NewVariable(), v, NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(v), err)
		assert.False(t, ok)
	})

	t.Run("term is not a compound", func(t *testing.T) {
		ok, err := Arg(NewVariable(), Atom("foo"), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorCompound(Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("nth is a variable", func(t *testing.T) {
		nth := NewVariable()
		_, err := Arg(nth, &Compound{
			Functor: "f",
			Args:    []Term{Atom("a"), Atom("b"), Atom("a")},
		}, Atom("a"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nth), err)
	})

	t.Run("nth is an integer", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			ok, err := Arg(Integer(2), &Compound{
				Functor: "f",
				Args:    []Term{Atom("a"), Atom("b"), Atom("c")},
			}, Atom("b"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("ng", func(t *testing.T) {
			ok, err := Arg(Integer(4), &Compound{
				Functor: "f",
				Args:    []Term{Atom("a"), Atom("b"), Atom("c")},
			}, Atom("b"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("negative", func(t *testing.T) {
			ok, err := Arg(Integer(-2), &Compound{
				Functor: "f",
				Args:    []Term{Atom("a"), Atom("b"), Atom("c")},
			}, Atom("b"), Success, nil).Force(context.Background())
			assert.Equal(t, domainErrorNotLessThanZero(Integer(-2)), err)
			assert.False(t, ok)
		})
	})

	t.Run("nth is neither a variable nor an integer", func(t *testing.T) {
		ok, err := Arg(Atom("foo"), &Compound{
			Functor: "f",
			Args:    []Term{Atom("a"), Atom("b"), Atom("c")},
		}, Atom("b"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Atom("foo")), err)
		assert.False(t, ok)
	})
}

func TestUniv(t *testing.T) {
	t.Run("term is a variable", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			v := NewVariable()
			ok, err := Univ(v, List(Atom("f"), Atom("a"), Atom("b")), func(env *Env) *Promise {
				assert.Equal(t, &Compound{
					Functor: "f",
					Args:    []Term{Atom("a"), Atom("b")},
				}, env.Resolve(v))
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("list is empty", func(t *testing.T) {
			v := NewVariable()
			ok, err := Univ(v, List(), Success, nil).Force(context.Background())
			assert.Equal(t, domainErrorNotEmptyList(Atom("[]")), err)
			assert.False(t, ok)
		})

		t.Run("list is not a list", func(t *testing.T) {
			v := NewVariable()
			ok, err := Univ(v, Atom("list"), Success, nil).Force(context.Background())
			assert.Equal(t, typeErrorList(Atom("list")), err)
			assert.False(t, ok)
		})

		t.Run("list's first element is not an atom", func(t *testing.T) {
			v := NewVariable()
			ok, err := Univ(v, List(Integer(0), Atom("a"), Atom("b")), Success, nil).Force(context.Background())
			assert.Equal(t, typeErrorAtom(Integer(0)), err)
			assert.False(t, ok)
		})

		t.Run("list is not fully instantiated", func(t *testing.T) {
			v, rest := NewVariable(), Variable("Rest")
			ok, err := Univ(v, ListRest(rest, Atom("f"), Atom("a"), Atom("b")), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(ListRest(rest, Atom("a"), Atom("b"))), err)
			assert.False(t, ok)
		})
	})

	t.Run("term is a compound", func(t *testing.T) {
		ok, err := Univ(&Compound{
			Functor: "f",
			Args:    []Term{Atom("a"), Atom("b")},
		}, List(Atom("f"), Atom("a"), Atom("b")), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("term is neither a variable nor a compound", func(t *testing.T) {
		ok, err := Univ(Atom("foo"), List(Atom("foo")), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})
}

func TestCopyTerm(t *testing.T) {
	in := Variable("In")
	out := Variable("Out")
	env := NewEnv().
		Bind(in, Atom("a"))
	ok, err := CopyTerm(in, out, func(env *Env) *Promise {
		assert.Equal(t, Atom("a"), env.Resolve(out))
		return Bool(true)
	}, env).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)
}

func TestVM_Op(t *testing.T) {
	t.Run("insert", func(t *testing.T) {
		vm := VM{
			operators: Operators{
				{
					Priority:  900,
					Specifier: OperatorSpecifierXFX,
					Name:      "+++",
				},
				{
					Priority:  1100,
					Specifier: OperatorSpecifierXFX,
					Name:      "+",
				},
			},
		}
		ok, err := vm.Op(Integer(1000), Atom("xfx"), Atom("++"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, Operators{
			{
				Priority:  900,
				Specifier: OperatorSpecifierXFX,
				Name:      "+++",
			},
			{
				Priority:  1000,
				Specifier: OperatorSpecifierXFX,
				Name:      "++",
			},
			{
				Priority:  1100,
				Specifier: OperatorSpecifierXFX,
				Name:      "+",
			},
		}, vm.operators)
	})

	t.Run("remove", func(t *testing.T) {
		vm := VM{
			operators: Operators{
				{
					Priority:  900,
					Specifier: OperatorSpecifierXFX,
					Name:      "+++",
				},
				{
					Priority:  1000,
					Specifier: OperatorSpecifierXFX,
					Name:      "++",
				},
				{
					Priority:  1100,
					Specifier: OperatorSpecifierXFX,
					Name:      "+",
				},
			},
		}
		ok, err := vm.Op(Integer(0), Atom("xfx"), Atom("++"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, Operators{
			{
				Priority:  900,
				Specifier: OperatorSpecifierXFX,
				Name:      "+++",
			},
			{
				Priority:  1100,
				Specifier: OperatorSpecifierXFX,
				Name:      "+",
			},
		}, vm.operators)
	})

	t.Run("priority is not an integer", func(t *testing.T) {
		var vm VM
		ok, err := vm.Op(Atom("foo"), Atom("xfx"), Atom("+"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("priority is negative", func(t *testing.T) {
		var vm VM
		ok, err := vm.Op(Integer(-1), Atom("xfx"), Atom("+"), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorOperatorPriority(Integer(-1)), err)
		assert.False(t, ok)
	})

	t.Run("priority is more than 1200", func(t *testing.T) {
		var vm VM
		ok, err := vm.Op(Integer(1201), Atom("xfx"), Atom("+"), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorOperatorPriority(Integer(1201)), err)
		assert.False(t, ok)
	})

	t.Run("specifier is not an atom", func(t *testing.T) {
		var vm VM
		ok, err := vm.Op(Integer(1000), Integer(0), Atom("+"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorAtom(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("specifier is not a valid operator specifier", func(t *testing.T) {
		var vm VM
		ok, err := vm.Op(Integer(1000), Atom("foo"), Atom("+"), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorOperatorSpecifier(Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("operator is not an atom", func(t *testing.T) {
		var vm VM
		ok, err := vm.Op(Integer(1000), Atom("xfx"), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorAtom(Integer(0)), err)
		assert.False(t, ok)
	})
}

func TestVM_CurrentOp(t *testing.T) {
	vm := VM{
		operators: Operators{
			{
				Priority:  900,
				Specifier: OperatorSpecifierXFX,
				Name:      "+++",
			},
			{
				Priority:  1000,
				Specifier: OperatorSpecifierXFX,
				Name:      "++",
			},
			{
				Priority:  1100,
				Specifier: OperatorSpecifierXFX,
				Name:      "+",
			},
		},
	}

	t.Run("single solution", func(t *testing.T) {
		ok, err := vm.CurrentOp(Integer(1100), Atom("xfx"), Atom("+"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("multiple solutions", func(t *testing.T) {
		var (
			priority, specifier, operator = Variable("Priority"), Variable("Specifier"), Variable("Operator")
			c                             int
		)
		ok, err := vm.CurrentOp(priority, specifier, operator, func(env *Env) *Promise {
			switch c {
			case 0:
				assert.Equal(t, Integer(900), env.Resolve(priority))
				assert.Equal(t, Atom("xfx"), env.Resolve(specifier))
				assert.Equal(t, Atom("+++"), env.Resolve(operator))
			case 1:
				assert.Equal(t, Integer(1000), env.Resolve(priority))
				assert.Equal(t, Atom("xfx"), env.Resolve(specifier))
				assert.Equal(t, Atom("++"), env.Resolve(operator))
			case 2:
				assert.Equal(t, Integer(1100), env.Resolve(priority))
				assert.Equal(t, Atom("xfx"), env.Resolve(specifier))
				assert.Equal(t, Atom("+"), env.Resolve(operator))
			default:
				assert.Fail(t, "unreachable")
			}
			c++
			return Bool(false)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("priority is not an operator priority", func(t *testing.T) {
		t.Run("priority is not an integer", func(t *testing.T) {
			ok, err := vm.CurrentOp(Atom("foo"), Atom("xfx"), Atom("+"), Success, nil).Force(context.Background())
			assert.Equal(t, domainErrorOperatorPriority(Atom("foo")), err)
			assert.False(t, ok)
		})

		t.Run("priority is negative", func(t *testing.T) {
			ok, err := vm.CurrentOp(Integer(-1), Atom("xfx"), Atom("+"), Success, nil).Force(context.Background())
			assert.Equal(t, domainErrorOperatorPriority(Integer(-1)), err)
			assert.False(t, ok)
		})
	})

	t.Run("specifier is not an operator specifier", func(t *testing.T) {
		t.Run("specifier is not an atom", func(t *testing.T) {
			ok, err := vm.CurrentOp(Integer(1100), Integer(0), Atom("+"), Success, nil).Force(context.Background())
			assert.Equal(t, domainErrorOperatorSpecifier(Integer(0)), err)
			assert.False(t, ok)
		})

		t.Run("specifier is a non-specifier atom", func(t *testing.T) {
			ok, err := vm.CurrentOp(Integer(1100), Atom("foo"), Atom("+"), Success, nil).Force(context.Background())
			assert.Equal(t, domainErrorOperatorSpecifier(Atom("foo")), err)
			assert.False(t, ok)
		})
	})

	t.Run("operator is not an atom", func(t *testing.T) {
		ok, err := vm.CurrentOp(Integer(1100), Atom("xfx"), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorAtom(Integer(0)), err)
		assert.False(t, ok)
	})
}

func TestVM_BagOf(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		vm := VM{
			procedures: map[ProcedureIndicator]procedure{
				{Name: "foo", Arity: 3}: clauses{
					{xrTable: []Term{Atom("a"), Atom("b"), Atom("c")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opConst, operand: 1},
						{opcode: opConst, operand: 2},
						{opcode: opExit},
					}},
					{xrTable: []Term{Atom("a"), Atom("b"), Atom("d")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opConst, operand: 1},
						{opcode: opConst, operand: 2},
						{opcode: opExit},
					}},
					{xrTable: []Term{Atom("b"), Atom("c"), Atom("e")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opConst, operand: 1},
						{opcode: opConst, operand: 2},
						{opcode: opExit},
					}},
					{xrTable: []Term{Atom("b"), Atom("c"), Atom("f")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opConst, operand: 1},
						{opcode: opConst, operand: 2},
						{opcode: opExit},
					}},
					{xrTable: []Term{Atom("c"), Atom("c"), Atom("g")}, bytecode: bytecode{
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
				count       int
				a, b, c, cs = Variable("A"), Variable("B"), Variable("C"), Variable("Cs")
			)
			ok, err := vm.BagOf(c, &Compound{
				Functor: "foo",
				Args:    []Term{a, b, c},
			}, cs, func(env *Env) *Promise {
				switch count {
				case 0:
					assert.Equal(t, Atom("a"), env.Resolve(a))
					assert.Equal(t, Atom("b"), env.Resolve(b))
					assert.Equal(t, List(Atom("c"), Atom("d")), env.Resolve(cs))
				case 1:
					assert.Equal(t, Atom("b"), env.Resolve(a))
					assert.Equal(t, Atom("c"), env.Resolve(b))
					assert.Equal(t, List(Atom("e"), Atom("f")), env.Resolve(cs))
				case 2:
					assert.Equal(t, Atom("c"), env.Resolve(a))
					assert.Equal(t, Atom("c"), env.Resolve(b))
					assert.Equal(t, List(Atom("g")), env.Resolve(cs))
				default:
					assert.Fail(t, "unreachable")
				}
				count++
				return Bool(false)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("with qualifier", func(t *testing.T) {
			var (
				count       int
				a, b, c, cs = Variable("A"), Variable("B"), Variable("C"), Variable("Cs")
			)
			ok, err := vm.BagOf(c, &Compound{
				Functor: "^",
				Args: []Term{a, &Compound{
					Functor: "foo",
					Args:    []Term{a, b, c},
				}},
			}, cs, func(env *Env) *Promise {
				switch count {
				case 0:
					assert.Equal(t, Atom("b"), env.Resolve(b))
					assert.Equal(t, List(Atom("c"), Atom("d")), env.Resolve(cs))
				case 1:
					assert.Equal(t, Atom("c"), env.Resolve(b))
					assert.Equal(t, List(Atom("e"), Atom("f"), Atom("g")), env.Resolve(cs))
				default:
					assert.Fail(t, "unreachable")
				}
				count++
				return Bool(false)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("with multiple qualifiers", func(t *testing.T) {
			var (
				count       int
				a, b, c, cs = Variable("A"), Variable("B"), Variable("C"), Variable("Cs")
			)
			ok, err := vm.BagOf(c, &Compound{
				Functor: "^",
				Args: []Term{
					&Compound{
						Functor: ",",
						Args:    []Term{a, b},
					},
					&Compound{
						Functor: "foo",
						Args:    []Term{a, b, c},
					},
				},
			}, cs, func(env *Env) *Promise {
				switch count {
				case 0:
					assert.Equal(t, List(Atom("c"), Atom("d"), Atom("e"), Atom("f"), Atom("g")), env.Resolve(cs))
				default:
					assert.Fail(t, "unreachable")
				}
				count++
				return Bool(false)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("goal is a variable", func(t *testing.T) {
		goal := Variable("Goal")

		var vm VM
		ok, err := vm.BagOf(NewVariable(), goal, NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(&goal), err)
		assert.False(t, ok)
	})

	t.Run("goal is neither a variable nor a callable term", func(t *testing.T) {
		var vm VM
		ok, err := vm.BagOf(NewVariable(), Integer(0), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorCallable(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("disjunction", func(t *testing.T) {
		vm := VM{
			procedures: map[ProcedureIndicator]procedure{
				{Name: "foo", Arity: 1}: predicate1(func(t Term, k func(*Env) *Promise, env *Env) *Promise {
					return Delay(func(ctx context.Context) *Promise {
						return Unify(t, Atom("a"), k, env)
					}, func(ctx context.Context) *Promise {
						return Unify(t, Atom("b"), k, env)
					})
				}),
			},
		}

		t.Run("variable", func(t *testing.T) {
			x, xs := Variable("X"), Variable("Xs")
			ok, err := vm.BagOf(x, &Compound{
				Functor: "foo",
				Args:    []Term{x},
			}, xs, func(env *Env) *Promise {
				assert.Equal(t, List(Atom("a"), Atom("b")), env.Resolve(xs))
				return Bool(false)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("not variable", func(t *testing.T) {
			count := 0
			x, xs := Variable("X"), Variable("Xs")
			ok, err := vm.BagOf(Atom("c"), &Compound{
				Functor: "foo",
				Args:    []Term{x},
			}, xs, func(env *Env) *Promise {
				switch count {
				case 0:
					assert.Equal(t, Atom("a"), env.Resolve(x))
					assert.Equal(t, List(Atom("c")), env.Resolve(xs))
				case 1:
					assert.Equal(t, Atom("b"), env.Resolve(x))
					assert.Equal(t, List(Atom("c")), env.Resolve(xs))
				default:
					assert.Fail(t, "unreachable")
				}
				count++
				return Bool(false)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("complex", func(t *testing.T) {
			// bagof(X, X = Y; X = Z; Y = a, Xs).
			count := 0
			x, y, z, xs := Variable("X"), Variable("Y"), Variable("Z"), Variable("Xs")
			var vm VM
			vm.Register2("=", Unify)
			ok, err := vm.BagOf(x, &Compound{
				Functor: ";",
				Args: []Term{
					&Compound{Functor: "=", Args: []Term{x, y}},
					&Compound{
						Functor: ";",
						Args: []Term{
							&Compound{Functor: "=", Args: []Term{x, z}},
							&Compound{Functor: "=", Args: []Term{y, Atom("a")}},
						},
					},
				},
			}, xs, func(env *Env) *Promise {
				switch count {
				case 0:
					assert.Equal(t, List(y, z), env.Resolve(xs))
				case 1:
					assert.Equal(t, Atom("a"), env.Resolve(y))
					v := NewVariable()
					_, ok := List(v).Unify(xs, false, env)
					assert.True(t, ok)
				default:
					assert.Fail(t, "unreachable")
				}
				count++
				return Bool(false)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})
	})
}

func TestVM_SetOf(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		vm := VM{
			procedures: map[ProcedureIndicator]procedure{
				{Name: "foo", Arity: 3}: clauses{
					{xrTable: []Term{Atom("a"), Atom("b"), Atom("c")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opConst, operand: 1},
						{opcode: opConst, operand: 2},
						{opcode: opExit},
					}},
					{xrTable: []Term{Atom("a"), Atom("b"), Atom("d")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opConst, operand: 1},
						{opcode: opConst, operand: 2},
						{opcode: opExit},
					}},
					{xrTable: []Term{Atom("a"), Atom("b"), Atom("c")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opConst, operand: 1},
						{opcode: opConst, operand: 2},
						{opcode: opExit},
					}},
					{xrTable: []Term{Atom("b"), Atom("c"), Atom("e")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opConst, operand: 1},
						{opcode: opConst, operand: 2},
						{opcode: opExit},
					}},
					{xrTable: []Term{Atom("b"), Atom("c"), Atom("f")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opConst, operand: 1},
						{opcode: opConst, operand: 2},
						{opcode: opExit},
					}},
					{xrTable: []Term{Atom("b"), Atom("c"), Atom("e")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opConst, operand: 1},
						{opcode: opConst, operand: 2},
						{opcode: opExit},
					}},
					{xrTable: []Term{Atom("c"), Atom("c"), Atom("g")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opConst, operand: 1},
						{opcode: opConst, operand: 2},
						{opcode: opExit},
					}},
					{xrTable: []Term{Atom("c"), Atom("c"), Atom("g")}, bytecode: bytecode{
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
				count       int
				a, b, c, cs = Variable("A"), Variable("B"), Variable("C"), Variable("Cs")
			)
			ok, err := vm.SetOf(c, &Compound{
				Functor: "foo",
				Args:    []Term{a, b, c},
			}, cs, func(env *Env) *Promise {
				switch count {
				case 0:
					assert.Equal(t, Atom("a"), env.Resolve(a))
					assert.Equal(t, Atom("b"), env.Resolve(b))
					assert.Equal(t, List(Atom("c"), Atom("d")), env.Resolve(cs))
				case 1:
					assert.Equal(t, Atom("b"), env.Resolve(a))
					assert.Equal(t, Atom("c"), env.Resolve(b))
					assert.Equal(t, List(Atom("e"), Atom("f")), env.Resolve(cs))
				case 2:
					assert.Equal(t, Atom("c"), env.Resolve(a))
					assert.Equal(t, Atom("c"), env.Resolve(b))
					assert.Equal(t, List(Atom("g")), env.Resolve(cs))
				default:
					assert.Fail(t, "unreachable")
				}
				count++
				return Bool(false)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("with qualifier", func(t *testing.T) {
			var (
				count       int
				a, b, c, cs = Variable("A"), Variable("B"), Variable("C"), Variable("Cs")
			)
			ok, err := vm.SetOf(c, &Compound{
				Functor: "^",
				Args: []Term{a, &Compound{
					Functor: "foo",
					Args:    []Term{a, b, c},
				}},
			}, cs, func(env *Env) *Promise {
				switch count {
				case 0:
					assert.Equal(t, Atom("b"), env.Resolve(b))
					assert.Equal(t, List(Atom("c"), Atom("d")), env.Resolve(cs))
				case 1:
					assert.Equal(t, Atom("c"), env.Resolve(b))
					assert.Equal(t, List(Atom("e"), Atom("f"), Atom("g")), env.Resolve(cs))
				default:
					assert.Fail(t, "unreachable")
				}
				count++
				return Bool(false)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("with multiple qualifiers", func(t *testing.T) {
			var (
				count       int
				a, b, c, cs = Variable("A"), Variable("B"), Variable("C"), Variable("Cs")
			)
			ok, err := vm.SetOf(c, &Compound{
				Functor: "^",
				Args: []Term{
					&Compound{
						Functor: ",",
						Args:    []Term{a, b},
					},
					&Compound{
						Functor: "foo",
						Args:    []Term{a, b, c},
					},
				},
			}, cs, func(env *Env) *Promise {
				switch count {
				case 0:
					assert.Equal(t, List(Atom("c"), Atom("d"), Atom("e"), Atom("f"), Atom("g")), env.Resolve(cs))
				default:
					assert.Fail(t, "unreachable")
				}
				count++
				return Bool(false)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("goal is a variable", func(t *testing.T) {
		goal := Variable("Goal")

		var vm VM
		ok, err := vm.SetOf(NewVariable(), goal, NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(goal), err)
		assert.False(t, ok)
	})

	t.Run("goal is neither a variable nor a callable term", func(t *testing.T) {
		var vm VM
		ok, err := vm.SetOf(NewVariable(), Integer(0), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorCallable(Integer(0)), err)
		assert.False(t, ok)
	})
}

func TestVM_FindAll(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		vm := VM{
			procedures: map[ProcedureIndicator]procedure{
				{Name: "foo", Arity: 3}: clauses{
					{xrTable: []Term{Atom("a"), Atom("b"), Atom("c")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opConst, operand: 1},
						{opcode: opConst, operand: 2},
						{opcode: opExit},
					}},
					{xrTable: []Term{Atom("a"), Atom("b"), Atom("d")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opConst, operand: 1},
						{opcode: opConst, operand: 2},
						{opcode: opExit},
					}},
					{xrTable: []Term{Atom("b"), Atom("c"), Atom("e")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opConst, operand: 1},
						{opcode: opConst, operand: 2},
						{opcode: opExit},
					}},
					{xrTable: []Term{Atom("b"), Atom("c"), Atom("f")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opConst, operand: 1},
						{opcode: opConst, operand: 2},
						{opcode: opExit},
					}},
					{xrTable: []Term{Atom("c"), Atom("c"), Atom("g")}, bytecode: bytecode{
						{opcode: opConst, operand: 0},
						{opcode: opConst, operand: 1},
						{opcode: opConst, operand: 2},
						{opcode: opExit},
					}},
				},
			},
		}

		var (
			count       int
			a, b, c, cs = Variable("A"), Variable("B"), Variable("C"), Variable("Cs")
		)
		ok, err := vm.FindAll(c, &Compound{
			Functor: "foo",
			Args:    []Term{a, b, c},
		}, cs, func(env *Env) *Promise {
			switch count {
			case 0:
				assert.Equal(t, List(Atom("c"), Atom("d"), Atom("e"), Atom("f"), Atom("g")), env.Resolve(cs))
			default:
				assert.Fail(t, "unreachable")
			}
			count++
			return Bool(false)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("goal is a variable", func(t *testing.T) {
		goal := Variable("Goal")

		var vm VM
		ok, err := vm.FindAll(NewVariable(), goal, NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(&goal), err)
		assert.False(t, ok)
	})

	t.Run("goal is neither a variable nor a callable term", func(t *testing.T) {
		var vm VM
		ok, err := vm.FindAll(NewVariable(), Integer(0), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorCallable(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("goal fails", func(t *testing.T) {
		instances := Variable("instances")

		vm := VM{
			procedures: map[ProcedureIndicator]procedure{
				{Name: "fail", Arity: 0}: predicate0(func(f func(*Env) *Promise, env *Env) *Promise {
					return Bool(false)
				}),
			},
		}
		ok, err := vm.FindAll(NewVariable(), Atom("fail"), instances, func(env *Env) *Promise {
			assert.Equal(t, List(), env.Resolve(instances))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})
}

func TestCompare(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		x, y := Variable("X"), Variable("Y")
		ok, err := Compare(Atom("<"), x, y, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(Atom("="), x, x, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(Atom(">"), y, x, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		env := NewEnv().
			Bind(x, Atom("b")).
			Bind(y, Atom("a"))
		ok, err = Compare(Atom(">"), x, y, Success, env).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(Atom("<"), NewVariable(), Integer(0), Success, env).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(Atom("<"), NewVariable(), Atom(""), Success, env).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(Atom("<"), NewVariable(), &Compound{}, Success, env).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(Atom(">"), Integer(0), NewVariable(), Success, env).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(Atom("<"), Integer(0), Integer(1), Success, env).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(Atom("="), Integer(0), Integer(0), Success, env).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(Atom(">"), Integer(1), Integer(0), Success, env).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(Atom("<"), Integer(0), Atom(""), Success, env).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(Atom("<"), Integer(0), &Compound{}, Success, env).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(Atom(">"), Atom(""), NewVariable(), Success, env).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(Atom(">"), Atom(""), Integer(0), Success, env).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(Atom("<"), Atom("a"), Atom("b"), Success, env).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(Atom("="), Atom("a"), Atom("a"), Success, env).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(Atom(">"), Atom("b"), Atom("a"), Success, env).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(Atom("<"), Atom(""), &Compound{}, Success, env).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(Atom(">"), &Compound{}, NewVariable(), Success, env).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(Atom(">"), &Compound{}, Integer(0), Success, env).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(Atom(">"), &Compound{}, Atom(""), Success, env).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(Atom("<"), &Compound{Functor: "a"}, &Compound{Functor: "b"}, Success, env).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(Atom("="), &Compound{Functor: "a"}, &Compound{Functor: "a"}, Success, env).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(Atom(">"), &Compound{Functor: "b"}, &Compound{Functor: "a"}, Success, env).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(Atom(">"), &Compound{Functor: "f", Args: []Term{Atom("a")}}, &Compound{Functor: "f"}, Success, env).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(Atom("="), &Compound{Functor: "f", Args: []Term{Atom("a")}}, &Compound{Functor: "f", Args: []Term{Atom("a")}}, Success, env).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(Atom("<"), &Compound{Functor: "f"}, &Compound{Functor: "f", Args: []Term{Atom("a")}}, Success, env).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(Atom(">"), &Compound{Functor: "f", Args: []Term{Atom("b")}}, &Compound{Functor: "f", Args: []Term{Atom("a")}}, Success, env).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Compare(Atom("<"), &Compound{Functor: "f", Args: []Term{Atom("a")}}, &Compound{Functor: "f", Args: []Term{Atom("b")}}, Success, env).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("detect order", func(t *testing.T) {
		order := Variable("Order")
		ok, err := Compare(order, Atom("a"), Atom("b"), func(env *Env) *Promise {
			assert.Equal(t, Atom("<"), env.Resolve(order))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("order is neither a variable nor an atom", func(t *testing.T) {
		ok, err := Compare(Integer(0), NewVariable(), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorAtom(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("order is an atom but not <, =, or >", func(t *testing.T) {
		ok, err := Compare(Atom("foo"), NewVariable(), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorOrder(Atom("foo")), err)
		assert.False(t, ok)
	})
}

func TestThrow(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		ok, err := Throw(Atom("a"), Success, nil).Force(context.Background())
		assert.Equal(t, &Exception{Term: Atom("a")}, err)
		assert.False(t, ok)
	})

	t.Run("ball is a variable", func(t *testing.T) {
		ball := Variable("Ball")

		ok, err := Throw(ball, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(&ball), err)
		assert.False(t, ok)
	})
}

func TestVM_Catch(t *testing.T) {
	var vm VM
	vm.Register2("=", Unify)
	vm.Register1("throw", Throw)
	vm.Register0("true", func(k func(*Env) *Promise, env *Env) *Promise {
		return k(env)
	})
	vm.Register0("fail", func(_ func(*Env) *Promise, _ *Env) *Promise {
		return Bool(false)
	})

	t.Run("match", func(t *testing.T) {
		v := NewVariable()
		ok, err := vm.Catch(&Compound{
			Functor: "throw",
			Args:    []Term{Atom("a")},
		}, v, &Compound{
			Functor: "=",
			Args:    []Term{v, Atom("a")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not match", func(t *testing.T) {
		ok, err := vm.Catch(&Compound{
			Functor: "throw",
			Args:    []Term{Atom("a")},
		}, Atom("b"), Atom("fail"), Success, nil).Force(context.Background())
		assert.False(t, ok)
		ex, ok := err.(*Exception)
		assert.True(t, ok)
		assert.Equal(t, Atom("a"), ex.Term)
	})

	t.Run("true", func(t *testing.T) {
		ok, err := vm.Catch(Atom("true"), Atom("b"), Atom("fail"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("false", func(t *testing.T) {
		ok, err := vm.Catch(Atom("fail"), Atom("b"), Atom("fail"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("non-exception error", func(t *testing.T) {
		ok, err := vm.Catch(Atom("true"), NewVariable(), Atom("true"), func(env *Env) *Promise {
			return Error(errors.New("failed"))
		}, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})
}

func TestVM_CurrentPredicate(t *testing.T) {
	t.Run("user defined predicate", func(t *testing.T) {
		vm := VM{procedures: map[ProcedureIndicator]procedure{
			{Name: "foo", Arity: 1}: clauses{},
		}}
		ok, err := vm.CurrentPredicate(&Compound{
			Functor: "/",
			Args: []Term{
				Atom("foo"),
				Integer(1),
			},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("variable", func(t *testing.T) {
		var foo, bar, baz bool

		v := Variable("V")

		vm := VM{procedures: map[ProcedureIndicator]procedure{
			{Name: "foo", Arity: 1}: clauses{},
			{Name: "bar", Arity: 1}: clauses{},
			{Name: "baz", Arity: 1}: clauses{},
		}}
		ok, err := vm.CurrentPredicate(v, func(env *Env) *Promise {
			c, ok := env.Resolve(v).(*Compound)
			assert.True(t, ok)
			assert.Equal(t, Atom("/"), c.Functor)
			assert.Len(t, c.Args, 2)
			assert.Equal(t, Integer(1), c.Args[1])
			switch c.Args[0] {
			case Atom("foo"):
				foo = true
			case Atom("bar"):
				bar = true
			case Atom("baz"):
				baz = true
			default:
				assert.Fail(t, "unreachable")
			}
			return Bool(false)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)

		assert.True(t, foo)
		assert.True(t, bar)
		assert.True(t, baz)
	})

	t.Run("builtin predicate", func(t *testing.T) {
		vm := VM{procedures: map[ProcedureIndicator]procedure{
			{Name: "=", Arity: 2}: predicate2(Unify),
		}}
		ok, err := vm.CurrentPredicate(&Compound{
			Functor: "/",
			Args: []Term{
				Atom("="),
				Integer(2),
			},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("pi is neither a variable nor a predicate indicator", func(t *testing.T) {
		t.Run("atom", func(t *testing.T) {
			var vm VM
			ok, err := vm.CurrentPredicate(Atom("foo"), Success, nil).Force(context.Background())
			assert.Equal(t, typeErrorPredicateIndicator(Atom("foo")), err)
			assert.False(t, ok)
		})

		t.Run("compound", func(t *testing.T) {
			t.Run("non slash", func(t *testing.T) {
				var vm VM
				ok, err := vm.CurrentPredicate(&Compound{
					Functor: "f",
					Args:    []Term{Atom("a")},
				}, Success, nil).Force(context.Background())
				assert.Equal(t, typeErrorPredicateIndicator(&Compound{
					Functor: "f",
					Args:    []Term{Atom("a")},
				}), err)
				assert.False(t, ok)
			})

			t.Run("slash but number", func(t *testing.T) {
				var vm VM
				ok, err := vm.CurrentPredicate(&Compound{
					Functor: "/",
					Args:    []Term{Integer(0), Integer(0)},
				}, Success, nil).Force(context.Background())
				assert.Equal(t, typeErrorPredicateIndicator(&Compound{
					Functor: "/",
					Args:    []Term{Integer(0), Integer(0)},
				}), err)
				assert.False(t, ok)
			})

			t.Run("slash but path", func(t *testing.T) {
				var vm VM
				ok, err := vm.CurrentPredicate(&Compound{
					Functor: "/",
					Args:    []Term{Atom("foo"), Atom("bar")},
				}, Success, nil).Force(context.Background())
				assert.Equal(t, typeErrorPredicateIndicator(&Compound{
					Functor: "/",
					Args:    []Term{Atom("foo"), Atom("bar")},
				}), err)
				assert.False(t, ok)
			})
		})
	})
}

func TestVM_Assertz(t *testing.T) {
	t.Run("append", func(t *testing.T) {
		var vm VM

		ok, err := vm.Assertz(&Compound{
			Functor: "foo",
			Args:    []Term{Atom("a")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = vm.Assertz(&Compound{
			Functor: "foo",
			Args:    []Term{Atom("b")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, clauses{
			{
				pi: ProcedureIndicator{
					Name:  "foo",
					Arity: 1,
				},
				raw: &Compound{
					Functor: "foo",
					Args:    []Term{Atom("a")},
				},
				xrTable: []Term{Atom("a")},
				bytecode: bytecode{
					{opcode: opConst, operand: 0},
					{opcode: opExit},
				},
			},
			{
				pi: ProcedureIndicator{
					Name:  "foo",
					Arity: 1,
				},
				raw: &Compound{
					Functor: "foo",
					Args:    []Term{Atom("b")},
				},
				xrTable: []Term{Atom("b")},
				bytecode: bytecode{
					{opcode: opConst, operand: 0},
					{opcode: opExit},
				},
			},
		}, vm.procedures[ProcedureIndicator{
			Name:  "foo",
			Arity: 1,
		}])
	})

	t.Run("directive", func(t *testing.T) {
		var called bool
		vm := VM{
			procedures: map[ProcedureIndicator]procedure{
				{Name: "directive", Arity: 0}: predicate0(func(k func(*Env) *Promise, env *Env) *Promise {
					called = true
					return k(env)
				}),
			},
		}

		ok, err := vm.Assertz(&Compound{
			Functor: ":-",
			Args:    []Term{Atom("directive")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.True(t, called)
	})

	t.Run("clause is a variable", func(t *testing.T) {
		clause := Variable("Term")

		var vm VM
		ok, err := vm.Assertz(clause, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(&clause), err)
		assert.False(t, ok)
	})

	t.Run("clause is neither a variable, nor callable", func(t *testing.T) {
		var vm VM
		ok, err := vm.Assertz(Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorCallable(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("head is a variable", func(t *testing.T) {
		head := Variable("Head")

		var vm VM
		ok, err := vm.Assertz(&Compound{
			Functor: ":-",
			Args:    []Term{head, Atom("true")},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(&head), err)
		assert.False(t, ok)
	})

	t.Run("head is neither a variable, nor callable", func(t *testing.T) {
		var vm VM
		ok, err := vm.Assertz(&Compound{
			Functor: ":-",
			Args:    []Term{Integer(0), Atom("true")},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorCallable(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("directive is a variable", func(t *testing.T) {
		directive := Variable("Directive")

		var vm VM
		ok, err := vm.Assertz(&Compound{
			Functor: ":-",
			Args:    []Term{directive},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(directive), err)
		assert.False(t, ok)
	})

	t.Run("directive is neither a variable, nor callable", func(t *testing.T) {
		var vm VM
		ok, err := vm.Assertz(&Compound{
			Functor: ":-",
			Args:    []Term{Integer(0)},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorCallable(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("body contains a term which is not callable", func(t *testing.T) {
		var vm VM
		ok, err := vm.Assertz(&Compound{
			Functor: ":-",
			Args: []Term{
				Atom("foo"),
				&Compound{
					Functor: ",",
					Args: []Term{
						Atom("true"),
						Integer(0),
					},
				},
			},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorCallable(&Compound{
			Functor: ",",
			Args: []Term{
				Atom("true"),
				Integer(0),
			},
		}), err)
		assert.False(t, ok)
	})

	t.Run("static", func(t *testing.T) {
		vm := VM{
			procedures: map[ProcedureIndicator]procedure{
				{Name: "static", Arity: 0}: predicate0(func(k func(*Env) *Promise, env *Env) *Promise {
					return k(env)
				}),
			},
		}

		ok, err := vm.Assertz(Atom("static"), Success, nil).Force(context.Background())
		assert.Equal(t, permissionErrorModifyStaticProcedure(&Compound{
			Functor: "/",
			Args: []Term{
				Atom("static"),
				Integer(0),
			},
		}), err)
		assert.False(t, ok)
	})
}

func TestVM_Asserta(t *testing.T) {
	t.Run("prepend", func(t *testing.T) {
		var vm VM
		ok, err := vm.Asserta(&Compound{
			Functor: "foo",
			Args:    []Term{Atom("a")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = vm.Asserta(&Compound{
			Functor: "foo",
			Args:    []Term{Atom("b")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, clauses{
			{
				pi: ProcedureIndicator{Name: "foo", Arity: 1},
				raw: &Compound{
					Functor: "foo",
					Args:    []Term{Atom("b")},
				},
				xrTable: []Term{Atom("b")},
				bytecode: bytecode{
					{opcode: opConst, operand: 0},
					{opcode: opExit},
				},
			},
			{
				pi: ProcedureIndicator{Name: "foo", Arity: 1},
				raw: &Compound{
					Functor: "foo",
					Args:    []Term{Atom("a")},
				},
				xrTable: []Term{Atom("a")},
				bytecode: bytecode{
					{opcode: opConst, operand: 0},
					{opcode: opExit},
				},
			},
		}, vm.procedures[ProcedureIndicator{Name: "foo", Arity: 1}])
	})

	t.Run("directive", func(t *testing.T) {
		var called bool
		vm := VM{
			procedures: map[ProcedureIndicator]procedure{
				{Name: "directive", Arity: 0}: predicate0(func(k func(*Env) *Promise, env *Env) *Promise {
					called = true
					return k(env)
				}),
			},
		}

		ok, err := vm.Asserta(&Compound{
			Functor: ":-",
			Args:    []Term{Atom("directive")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.True(t, called)
	})

	t.Run("clause is a variable", func(t *testing.T) {
		clause := Variable("Term")

		var vm VM
		ok, err := vm.Asserta(clause, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(&clause), err)
		assert.False(t, ok)
	})

	t.Run("clause is neither a variable, nor callable", func(t *testing.T) {
		var vm VM
		ok, err := vm.Asserta(Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorCallable(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("head is a variable", func(t *testing.T) {
		head := Variable("Head")

		var vm VM
		ok, err := vm.Asserta(&Compound{
			Functor: ":-",
			Args:    []Term{head, Atom("true")},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(head), err)
		assert.False(t, ok)
	})

	t.Run("head is neither a variable, nor callable", func(t *testing.T) {
		var vm VM
		ok, err := vm.Asserta(&Compound{
			Functor: ":-",
			Args:    []Term{Integer(0), Atom("true")},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorCallable(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("directive is a variable", func(t *testing.T) {
		directive := Variable("Directive")

		var vm VM
		ok, err := vm.Asserta(&Compound{
			Functor: ":-",
			Args:    []Term{directive},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(directive), err)
		assert.False(t, ok)
	})

	t.Run("directive is neither a variable, nor callable", func(t *testing.T) {
		var vm VM
		ok, err := vm.Asserta(&Compound{
			Functor: ":-",
			Args:    []Term{Integer(0)},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorCallable(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("body contains a term which is not callable", func(t *testing.T) {
		var vm VM
		ok, err := vm.Asserta(&Compound{
			Functor: ":-",
			Args: []Term{
				Atom("foo"),
				&Compound{
					Functor: ",",
					Args: []Term{
						Atom("true"),
						Integer(0)},
				},
			},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorCallable(&Compound{
			Functor: ",",
			Args: []Term{
				Atom("true"),
				Integer(0)},
		}), err)
		assert.False(t, ok)
	})

	t.Run("static", func(t *testing.T) {
		vm := VM{
			procedures: map[ProcedureIndicator]procedure{
				{Name: "static", Arity: 0}: predicate0(func(k func(*Env) *Promise, env *Env) *Promise {
					return k(env)
				}),
			},
		}

		ok, err := vm.Asserta(Atom("static"), Success, nil).Force(context.Background())
		assert.Equal(t, permissionErrorModifyStaticProcedure(&Compound{
			Functor: "/",
			Args: []Term{
				Atom("static"),
				Integer(0),
			},
		}), err)
		assert.False(t, ok)
	})
}

func TestVM_Retract(t *testing.T) {
	t.Run("retract the first one", func(t *testing.T) {
		vm := VM{
			procedures: map[ProcedureIndicator]procedure{
				{Name: "foo", Arity: 1}: clauses{
					{raw: &Compound{Functor: "foo", Args: []Term{Atom("a")}}},
					{raw: &Compound{Functor: "foo", Args: []Term{Atom("b")}}},
					{raw: &Compound{Functor: "foo", Args: []Term{Atom("c")}}},
				},
			},
		}

		ok, err := vm.Retract(&Compound{
			Functor: "foo",
			Args:    []Term{Variable("X")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, clauses{
			{raw: &Compound{Functor: "foo", Args: []Term{Atom("b")}}},
			{raw: &Compound{Functor: "foo", Args: []Term{Atom("c")}}},
		}, vm.procedures[ProcedureIndicator{Name: "foo", Arity: 1}])
	})

	t.Run("retract the specific one", func(t *testing.T) {
		vm := VM{
			procedures: map[ProcedureIndicator]procedure{
				{Name: "foo", Arity: 1}: clauses{
					{raw: &Compound{Functor: "foo", Args: []Term{Atom("a")}}},
					{raw: &Compound{Functor: "foo", Args: []Term{Atom("b")}}},
					{raw: &Compound{Functor: "foo", Args: []Term{Atom("c")}}},
				},
			},
		}

		ok, err := vm.Retract(&Compound{
			Functor: "foo",
			Args:    []Term{Atom("b")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, clauses{
			{raw: &Compound{Functor: "foo", Args: []Term{Atom("a")}}},
			{raw: &Compound{Functor: "foo", Args: []Term{Atom("c")}}},
		}, vm.procedures[ProcedureIndicator{Name: "foo", Arity: 1}])
	})

	t.Run("retract all", func(t *testing.T) {
		vm := VM{
			procedures: map[ProcedureIndicator]procedure{
				{Name: "foo", Arity: 1}: clauses{
					{raw: &Compound{Functor: "foo", Args: []Term{Atom("a")}}},
					{raw: &Compound{Functor: "foo", Args: []Term{Atom("b")}}},
					{raw: &Compound{Functor: "foo", Args: []Term{Atom("c")}}},
				},
			},
		}

		ok, err := vm.Retract(&Compound{
			Functor: "foo",
			Args:    []Term{Variable("X")},
		}, Failure, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
		assert.Empty(t, vm.procedures[ProcedureIndicator{Name: "foo", Arity: 1}])
	})

	t.Run("variable", func(t *testing.T) {
		x := Variable("X")

		var vm VM
		ok, err := vm.Retract(x, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(x), err)
		assert.False(t, ok)
	})

	t.Run("not callable", func(t *testing.T) {
		var vm VM
		ok, err := vm.Retract(Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorCallable(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("no clause matches", func(t *testing.T) {
		var vm VM

		ok, err := vm.Retract(&Compound{
			Functor: "foo",
			Args:    []Term{Variable("X")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("static", func(t *testing.T) {
		vm := VM{
			procedures: map[ProcedureIndicator]procedure{
				{Name: "foo", Arity: 0}: predicate0(nil),
			},
		}

		ok, err := vm.Retract(Atom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, permissionErrorModifyStaticProcedure(&Compound{
			Functor: "/",
			Args:    []Term{Atom("foo"), Integer(0)},
		}), err)
		assert.False(t, ok)
	})

	t.Run("exception in continuation", func(t *testing.T) {
		vm := VM{
			procedures: map[ProcedureIndicator]procedure{
				{Name: "foo", Arity: 1}: clauses{
					{raw: &Compound{Functor: "foo", Args: []Term{Atom("a")}}},
				},
			},
		}

		ok, err := vm.Retract(&Compound{
			Functor: "foo",
			Args:    []Term{Variable("X")},
		}, func(_ *Env) *Promise {
			return Error(errors.New("failed"))
		}, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)

		// removed
		assert.Empty(t, vm.procedures[ProcedureIndicator{Name: "foo", Arity: 1}])
	})
}

func TestVM_Abolish(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		vm := VM{
			procedures: map[ProcedureIndicator]procedure{
				{Name: "foo", Arity: 1}: clauses{
					{raw: &Compound{Functor: "foo", Args: []Term{Atom("a")}}},
					{raw: &Compound{Functor: "foo", Args: []Term{Atom("b")}}},
					{raw: &Compound{Functor: "foo", Args: []Term{Atom("c")}}},
				},
			},
		}

		ok, err := vm.Abolish(&Compound{
			Functor: "/",
			Args:    []Term{Atom("foo"), Integer(1)},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		_, ok = vm.procedures[ProcedureIndicator{Name: "foo", Arity: 1}]
		assert.False(t, ok)
	})

	t.Run("pi is a variable", func(t *testing.T) {
		pi := Variable("PI")

		var vm VM
		ok, err := vm.Abolish(pi, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(&pi), err)
		assert.False(t, ok)
	})

	t.Run("pi is a term Name/Arity and either Name or Arity is a variable", func(t *testing.T) {
		t.Run("Name is a variable", func(t *testing.T) {
			name := Variable("Name")

			var vm VM
			ok, err := vm.Abolish(&Compound{
				Functor: "/",
				Args:    []Term{name, Integer(2)},
			}, Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(name), err)
			assert.False(t, ok)
		})

		t.Run("Arity is a variable", func(t *testing.T) {
			arity := Variable("Arity")

			var vm VM
			ok, err := vm.Abolish(&Compound{
				Functor: "/",
				Args:    []Term{Atom("foo"), arity},
			}, Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(arity), err)
			assert.False(t, ok)
		})
	})

	t.Run("pi is neither a variable nor a predicate indicator", func(t *testing.T) {
		var vm VM
		ok, err := vm.Abolish(Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorPredicateIndicator(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("pi is a term Name/Arity and Name is neither a variable nor an atom", func(t *testing.T) {
		var vm VM
		ok, err := vm.Abolish(&Compound{
			Functor: "/",
			Args:    []Term{Integer(0), Integer(2)},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorAtom(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("pi is a term Name/Arity and Arity is neither a variable nor an integer", func(t *testing.T) {
		var vm VM
		ok, err := vm.Abolish(&Compound{
			Functor: "/",
			Args:    []Term{Atom("foo"), Atom("bar")},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Atom("bar")), err)
		assert.False(t, ok)
	})

	t.Run("pi is a term Name/Arity and Arity is an integer less than zero", func(t *testing.T) {
		var vm VM
		ok, err := vm.Abolish(&Compound{
			Functor: "/",
			Args:    []Term{Atom("foo"), Integer(-2)},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorNotLessThanZero(Integer(-2)), err)
		assert.False(t, ok)
	})

	t.Run("The predicate indicator pi is that of a static procedure", func(t *testing.T) {
		vm := VM{
			procedures: map[ProcedureIndicator]procedure{
				{Name: "foo", Arity: 0}: predicate0(nil),
			},
		}
		ok, err := vm.Abolish(&Compound{
			Functor: "/",
			Args:    []Term{Atom("foo"), Integer(0)},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, permissionErrorModifyStaticProcedure(&Compound{
			Functor: "/",
			Args:    []Term{Atom("foo"), Integer(0)},
		}), err)
		assert.False(t, ok)
	})
}

func TestVM_CurrentInput(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		var s Stream
		vm := VM{
			input: &s,
		}

		ok, err := vm.CurrentInput(&s, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("stream is neither a variable nor a stream", func(t *testing.T) {
		var vm VM
		ok, err := vm.CurrentInput(Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorStream(Integer(0)), err)
		assert.False(t, ok)
	})
}

func TestVM_CurrentOutput(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		var s Stream
		vm := VM{
			output: &s,
		}

		ok, err := vm.CurrentOutput(&s, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("stream is neither a variable nor a stream", func(t *testing.T) {
		var vm VM
		ok, err := vm.CurrentOutput(Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorStream(Integer(0)), err)
		assert.False(t, ok)
	})
}

func TestVM_SetInput(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		v := Variable("Stream")
		s := Stream{Source: os.Stdin}
		env := NewEnv().
			Bind(v, &s)
		var vm VM
		ok, err := vm.SetInput(v, Success, env).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
		assert.Equal(t, &s, vm.input)
	})

	t.Run("alias", func(t *testing.T) {
		v := Variable("Stream")
		s := Stream{Source: os.Stdin}
		env := NewEnv().
			Bind(v, &s)
		vm := VM{
			streams: map[Term]*Stream{
				Atom("x"): &s,
			},
		}
		ok, err := vm.SetInput(v, Success, env).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
		assert.Equal(t, &s, vm.input)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		streamOrAlias := Variable("Stream")

		var vm VM
		ok, err := vm.SetInput(streamOrAlias, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable, nor a stream term or alias", func(t *testing.T) {
		var vm VM
		ok, err := vm.SetInput(Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorStreamOrAlias(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var vm VM
		ok, err := vm.SetInput(Atom("x"), Success, nil).Force(context.Background())
		assert.Equal(t, existenceErrorStream(Atom("x")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		v := Variable("Stream")
		env := NewEnv().
			Bind(v, &Stream{Sink: os.Stdout})
		var vm VM
		ok, err := vm.SetInput(v, Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorInputStream(v), err)
		assert.False(t, ok)
	})
}

func TestVM_SetOutput(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		v := Variable("Stream")
		s := Stream{Sink: os.Stdout}
		env := NewEnv().
			Bind(v, &s)
		var vm VM
		ok, err := vm.SetOutput(v, Success, env).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
		assert.Equal(t, &s, vm.output)
	})

	t.Run("alias", func(t *testing.T) {
		s := Stream{Sink: os.Stdout}
		vm := VM{
			streams: map[Term]*Stream{
				Atom("x"): &s,
			},
		}
		ok, err := vm.SetOutput(Atom("x"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
		assert.Equal(t, &s, vm.output)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		streamOrAlias := Variable("Stream")

		var vm VM
		ok, err := vm.SetOutput(streamOrAlias, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable, nor a stream term or alias", func(t *testing.T) {
		var vm VM
		ok, err := vm.SetOutput(Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorStreamOrAlias(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var vm VM
		ok, err := vm.SetOutput(Atom("x"), Success, nil).Force(context.Background())
		assert.Equal(t, existenceErrorStream(Atom("x")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an input stream", func(t *testing.T) {
		s := Variable("Stream")
		env := NewEnv().
			Bind(s, &Stream{Source: os.Stdin})

		var vm VM
		ok, err := vm.SetOutput(s, Success, env).Force(context.Background())
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

		v := Variable("Stream")

		ok, err := vm.Open(Atom(f.Name()), Atom("read"), v, List(&Compound{
			Functor: "alias",
			Args:    []Term{Atom("input")},
		}), func(env *Env) *Promise {
			ref, ok := env.Lookup(v)
			assert.True(t, ok)
			s, ok := ref.(*Stream)
			assert.True(t, ok)

			assert.Equal(t, vm.streams[Atom("input")], s)

			b, err := ioutil.ReadAll(s.Source)
			assert.NoError(t, err)
			assert.Equal(t, "test\n", string(b))

			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("write", func(t *testing.T) {
		n := filepath.Join(os.TempDir(), "open_test_write")
		defer func() {
			assert.NoError(t, os.Remove(n))
		}()

		v := Variable("Stream")

		ok, err := vm.Open(Atom(n), Atom("write"), v, List(&Compound{
			Functor: "alias",
			Args:    []Term{Atom("output")},
		}), func(env *Env) *Promise {
			ref, ok := env.Lookup(v)
			assert.True(t, ok)
			s, ok := ref.(*Stream)
			assert.True(t, ok)

			assert.Equal(t, vm.streams[Atom("output")], s)

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

			return Bool(true)
		}, nil).Force(context.Background())
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

		v := Variable("Stream")

		ok, err := vm.Open(Atom(f.Name()), Atom("append"), v, List(&Compound{
			Functor: "alias",
			Args:    []Term{Atom("append")},
		}), func(env *Env) *Promise {
			ref, ok := env.Lookup(v)
			assert.True(t, ok)
			s, ok := ref.(*Stream)
			assert.True(t, ok)

			assert.Equal(t, vm.streams[Atom("append")], s)

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

			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("sourceSink is a variable", func(t *testing.T) {
		sourceSink := Variable("Source_Sink")

		var vm VM
		ok, err := vm.Open(sourceSink, Atom("read"), Variable("Stream"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(sourceSink), err)
		assert.False(t, ok)
	})

	t.Run("mode is a variable", func(t *testing.T) {
		mode := Variable("Mode")

		var vm VM
		ok, err := vm.Open(Atom("/dev/null"), mode, Variable("Stream"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(mode), err)
		assert.False(t, ok)
	})

	t.Run("options is a partial list or a list with an element E which is a variable", func(t *testing.T) {
		t.Run("partial list", func(t *testing.T) {
			options := ListRest(Variable("Rest"),
				&Compound{Functor: "type", Args: []Term{Atom("text")}},
				&Compound{Functor: "alias", Args: []Term{Atom("foo")}},
			)

			var vm VM
			ok, err := vm.Open(Atom("/dev/null"), Atom("read"), Variable("Stream"), options, Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(options), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			option := Variable("Option")

			var vm VM
			ok, err := vm.Open(Atom("/dev/null"), Atom("read"), Variable("Stream"), List(
				option,
				&Compound{Functor: "type", Args: []Term{Atom("text")}},
				&Compound{Functor: "alias", Args: []Term{Atom("foo")}},
			), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(option), err)
			assert.False(t, ok)
		})
	})

	t.Run("mode is neither a variable nor an atom", func(t *testing.T) {
		var vm VM
		ok, err := vm.Open(Atom("/dev/null"), Integer(0), Variable("Stream"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorAtom(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("options is neither a partial list nor a list", func(t *testing.T) {
		var vm VM
		ok, err := vm.Open(Atom("/dev/null"), Atom("read"), Variable("Stream"), Atom("list"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorList(Atom("list")), err)
		assert.False(t, ok)
	})

	t.Run("stream is not a variable", func(t *testing.T) {
		var vm VM
		ok, err := vm.Open(Atom("/dev/null"), Atom("read"), Atom("stream"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorVariable(Atom("stream")), err)
		assert.False(t, ok)
	})

	t.Run("sourceSink is neither a variable nor a source/sink", func(t *testing.T) {
		var vm VM
		ok, err := vm.Open(Integer(0), Atom("read"), Variable("Stream"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorSourceSink(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("mode is an atom but not an input/output mode", func(t *testing.T) {
		var vm VM
		ok, err := vm.Open(Atom("/dev/null"), Atom("foo"), Variable("Stream"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorIOMode(Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the options list is neither a variable nor a stream-option", func(t *testing.T) {
		var vm VM
		ok, err := vm.Open(Atom("/dev/null"), Atom("read"), Variable("Stream"), List(Atom("foo")), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorStreamOption(Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("the source/sink specified by sourceSink does not exist", func(t *testing.T) {
		f, err := ioutil.TempFile("", "open_test_existence")
		assert.NoError(t, err)
		assert.NoError(t, os.Remove(f.Name()))

		var vm VM
		ok, err := vm.Open(Atom(f.Name()), Atom("read"), Variable("Stream"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, existenceErrorSourceSink(Atom(f.Name())), err)
		assert.False(t, ok)
	})

	t.Run("the source/sink specified by sourceSink cannot be opened", func(t *testing.T) {
		f, err := ioutil.TempFile("", "open_test_permission")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, os.Remove(f.Name()))
		}()

		assert.NoError(t, f.Chmod(0200))

		var vm VM
		ok, err := vm.Open(Atom(f.Name()), Atom("read"), Variable("Stream"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError("open", "source_sink", Atom(f.Name()), "'%s' cannot be opened.", f.Name()), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the options list is alias and A is already associated with an open stream", func(t *testing.T) {
		f, err := ioutil.TempFile("", "open_test_dup_alias")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, os.Remove(f.Name()))
		}()

		vm := VM{
			streams: map[Term]*Stream{
				Atom("foo"): nil,
			},
		}
		ok, err := vm.Open(Atom(f.Name()), Atom("read"), Variable("Stream"), List(&Compound{
			Functor: "alias",
			Args:    []Term{Atom("foo")},
		}), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError("open", "source_sink", &Compound{
			Functor: "alias",
			Args:    []Term{Atom("foo")},
		}, "foo is already defined as an alias."), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the options list is reposition(true) and it is not possible to reposition", func(t *testing.T) {
		// TODO:
	})
}

func TestVM_Close(t *testing.T) {
	t.Run("without options", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			var m mockCloser
			m.On("Close").Return(nil).Once()
			defer m.AssertExpectations(t)

			var vm VM
			ok, err := vm.Close(&Stream{Closer: &m}, List(), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("ng", func(t *testing.T) {
			var m mockCloser
			m.On("Close").Return(errors.New("")).Once()
			defer m.AssertExpectations(t)

			var vm VM
			_, err := vm.Close(&Stream{Closer: &m}, List(), Success, nil).Force(context.Background())
			assert.Error(t, err)
		})
	})

	t.Run("force false", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			var m mockCloser
			m.On("Close").Return(nil).Once()
			defer m.AssertExpectations(t)

			var vm VM
			ok, err := vm.Close(&Stream{Closer: &m}, List(&Compound{
				Functor: "force",
				Args:    []Term{Atom("false")},
			}), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("ng", func(t *testing.T) {
			var m mockCloser
			m.On("Close").Return(errors.New("something happened")).Once()
			defer m.AssertExpectations(t)

			s := Stream{Closer: &m}

			var vm VM
			ok, err := vm.Close(&s, List(&Compound{
				Functor: "force",
				Args:    []Term{Atom("false")},
			}), Success, nil).Force(context.Background())
			assert.Equal(t, resourceError(&s, Atom("something happened")), err)
			assert.False(t, ok)
		})
	})

	t.Run("force true", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			var m mockCloser
			m.On("Close").Return(nil).Once()
			defer m.AssertExpectations(t)

			var vm VM
			ok, err := vm.Close(&Stream{Closer: &m}, List(&Compound{
				Functor: "force",
				Args:    []Term{Atom("true")},
			}), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("ng", func(t *testing.T) {
			var m mockCloser
			m.On("Close").Return(errors.New("")).Once()
			defer m.AssertExpectations(t)

			var vm VM
			ok, err := vm.Close(&Stream{Closer: &m}, List(&Compound{
				Functor: "force",
				Args:    []Term{Atom("true")},
			}), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("valid stream alias", func(t *testing.T) {
		var m mockCloser
		m.On("Close").Return(nil).Once()
		defer m.AssertExpectations(t)

		vm := VM{
			streams: map[Term]*Stream{
				Atom("foo"): {Closer: &m},
			},
		}
		ok, err := vm.Close(Atom("foo"), List(), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("streamOrAlias ia a variable", func(t *testing.T) {
		streamOrAlias := Variable("Stream")

		var vm VM
		ok, err := vm.Close(streamOrAlias, List(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("options is a partial list or a list with an element E which is a variable", func(t *testing.T) {
		t.Run("partial list", func(t *testing.T) {
			options := ListRest(Variable("Rest"),
				&Compound{Functor: "force", Args: []Term{Atom("true")}},
			)

			var vm VM
			ok, err := vm.Close(&Stream{}, options, Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(options), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			option := Variable("Option")

			var vm VM
			ok, err := vm.Close(&Stream{}, List(option, &Compound{Functor: "force", Args: []Term{Atom("true")}}), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(option), err)
			assert.False(t, ok)
		})
	})

	t.Run("options is neither a partial list nor a list", func(t *testing.T) {
		var vm VM
		ok, err := vm.Close(&Stream{}, Atom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorList(Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream-term or alias", func(t *testing.T) {
		var vm VM
		ok, err := vm.Close(Integer(0), List(), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorStreamOrAlias(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the Options list is neither a variable nor a stream-option", func(t *testing.T) {
		var vm VM
		ok, err := vm.Close(&Stream{}, List(Atom("foo")), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorStreamOption(Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var vm VM
		ok, err := vm.Close(Atom("foo"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, existenceErrorStream(Atom("foo")), err)
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
		var m mockWriter
		defer m.AssertExpectations(t)

		var vm VM
		ok, err := vm.FlushOutput(&Stream{Sink: &m}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("flusher", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			var m struct {
				mockWriter
				mockFlusher
			}
			m.mockFlusher.On("Flush").Return(nil).Once()
			defer m.mockWriter.AssertExpectations(t)
			defer m.mockFlusher.AssertExpectations(t)

			var vm VM
			ok, err := vm.FlushOutput(&Stream{Sink: &m}, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("ng", func(t *testing.T) {
			var m struct {
				mockWriter
				mockFlusher
			}
			m.mockFlusher.On("Flush").Return(errors.New("")).Once()
			defer m.mockWriter.AssertExpectations(t)
			defer m.mockFlusher.AssertExpectations(t)

			var vm VM
			_, err := vm.FlushOutput(&Stream{Sink: &m}, Success, nil).Force(context.Background())
			assert.Error(t, err)
		})
	})

	t.Run("valid stream alias", func(t *testing.T) {
		var m mockWriter
		defer m.AssertExpectations(t)

		vm := VM{
			streams: map[Term]*Stream{
				Atom("foo"): {Sink: &m},
			},
		}
		ok, err := vm.FlushOutput(Atom("foo"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		streamOrAlias := Variable("Stream")

		var vm VM
		ok, err := vm.FlushOutput(streamOrAlias, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream-term or alias", func(t *testing.T) {
		var vm VM
		ok, err := vm.FlushOutput(Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorStreamOrAlias(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var vm VM
		ok, err := vm.FlushOutput(Atom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, existenceErrorStream(Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("SorA is an input stream", func(t *testing.T) {
		s := Stream{Source: &mockReader{}}

		var vm VM
		ok, err := vm.FlushOutput(&s, Success, nil).Force(context.Background())
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

	s := Stream{Sink: &w}

	ops := Operators{
		{Priority: 500, Specifier: OperatorSpecifierYFX, Name: "+"},
		{Priority: 200, Specifier: OperatorSpecifierFY, Name: "-"},
	}

	vm := VM{
		operators: ops,
		streams: map[Term]*Stream{
			Atom("foo"): &s,
		},
	}

	t.Run("without options", func(t *testing.T) {
		var m mockTerm
		m.On("Unparse", mock.Anything, WriteTermOptions{Ops: ops, Priority: 1200}, (*Env)(nil)).Once()
		defer m.AssertExpectations(t)

		ok, err := vm.WriteTerm(&s, &m, List(), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("quoted", func(t *testing.T) {
		t.Run("false", func(t *testing.T) {
			var m mockTerm
			m.On("Unparse", mock.Anything, WriteTermOptions{Quoted: false, Ops: ops, Priority: 1200}, (*Env)(nil)).Once()
			defer m.AssertExpectations(t)

			ok, err := vm.WriteTerm(&s, &m, List(&Compound{
				Functor: "quoted",
				Args:    []Term{Atom("false")},
			}), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("true", func(t *testing.T) {
			var m mockTerm
			m.On("Unparse", mock.Anything, WriteTermOptions{Quoted: true, Ops: ops, Priority: 1200}, (*Env)(nil)).Once()
			defer m.AssertExpectations(t)

			ok, err := vm.WriteTerm(&s, &m, List(&Compound{
				Functor: "quoted",
				Args:    []Term{Atom("true")},
			}), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("ignore_ops", func(t *testing.T) {
		t.Run("false", func(t *testing.T) {
			var m mockTerm
			m.On("Unparse", mock.Anything, WriteTermOptions{Ops: ops, Priority: 1200}, (*Env)(nil)).Once()
			defer m.AssertExpectations(t)

			ok, err := vm.WriteTerm(&s, &m, List(&Compound{
				Functor: "ignore_ops",
				Args:    []Term{Atom("false")},
			}), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("true", func(t *testing.T) {
			var m mockTerm
			m.On("Unparse", mock.Anything, WriteTermOptions{Ops: nil, Priority: 1200}, (*Env)(nil)).Once()
			defer m.AssertExpectations(t)

			ok, err := vm.WriteTerm(&s, &m, List(&Compound{
				Functor: "ignore_ops",
				Args:    []Term{Atom("true")},
			}), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("numbervars", func(t *testing.T) {
		t.Run("false", func(t *testing.T) {
			var m mockTerm
			m.On("Unparse", mock.Anything, WriteTermOptions{Ops: ops, NumberVars: false, Priority: 1200}, (*Env)(nil)).Once()
			defer m.AssertExpectations(t)

			ok, err := vm.WriteTerm(&s, &m, List(&Compound{
				Functor: "numbervars",
				Args:    []Term{Atom("false")},
			}), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("true", func(t *testing.T) {
			var m mockTerm
			m.On("Unparse", mock.Anything, WriteTermOptions{Ops: ops, NumberVars: true, Priority: 1200}, (*Env)(nil)).Once()
			defer m.AssertExpectations(t)

			ok, err := vm.WriteTerm(&s, &m, List(&Compound{
				Functor: "numbervars",
				Args:    []Term{Atom("true")},
			}), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		streamOrAlias := Variable("Stream")

		var vm VM
		ok, err := vm.WriteTerm(streamOrAlias, Atom("foo"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("options is a partial list or a list with an element which is a variable", func(t *testing.T) {
		t.Run("partial list", func(t *testing.T) {
			options := ListRest(Variable("Rest"),
				&Compound{Functor: "quoted", Args: []Term{Atom("true")}},
			)

			var vm VM
			ok, err := vm.WriteTerm(&Stream{Sink: &mockWriter{}}, Atom("foo"), options, Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(options), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			option := Variable("Option")

			var vm VM
			ok, err := vm.WriteTerm(&Stream{Sink: &mockWriter{}}, Atom("foo"), List(option, &Compound{Functor: "quoted", Args: []Term{Atom("true")}}), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(option), err)
			assert.False(t, ok)
		})
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		var vm VM
		ok, err := vm.WriteTerm(Integer(0), Atom("foo"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorStreamOrAlias(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("options is neither a partial list nor a list", func(t *testing.T) {
		var vm VM
		ok, err := vm.WriteTerm(&Stream{Sink: &mockWriter{}}, Atom("foo"), Atom("options"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorList(Atom("options")), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the Options list is neither a variable nor a valid write-option", func(t *testing.T) {
		var vm VM
		ok, err := vm.WriteTerm(&Stream{Sink: &mockWriter{}}, Atom("foo"), List(&Compound{
			Functor: "unknown",
			Args:    []Term{Atom("option")},
		}), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorWriteOption(&Compound{
			Functor: "unknown",
			Args:    []Term{Atom("option")},
		}), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var vm VM
		ok, err := vm.WriteTerm(Atom("stream"), Atom("foo"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, existenceErrorStream(Atom("stream")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an input stream", func(t *testing.T) {
		s := Stream{Source: &mockReader{}}

		var vm VM
		ok, err := vm.WriteTerm(&s, Atom("foo"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, permissionErrorOutputStream(&s), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a binary stream", func(t *testing.T) {
		s := Stream{Sink: &mockWriter{}, StreamType: StreamTypeBinary}

		var vm VM
		ok, err := vm.WriteTerm(&s, Atom("foo"), List(), Success, nil).Force(context.Background())
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

func (m *mockTerm) Unify(t Term, occursCheck bool, env *Env) (*Env, bool) {
	args := m.Called(t, occursCheck, env)
	return args.Get(0).(*Env), args.Bool(1)
}

func (m *mockTerm) Unparse(emit func(Token), opts WriteTermOptions, env *Env) {
	_ = m.Called(emit, opts, env)
}

func TestCharCode(t *testing.T) {
	t.Run("ascii", func(t *testing.T) {
		ok, err := CharCode(Atom("a"), Integer(97), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("emoji", func(t *testing.T) {
		ok, err := CharCode(Atom("😀"), Integer(128512), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("query char", func(t *testing.T) {
		v := Variable("Char")

		ok, err := CharCode(v, Integer(128512), func(env *Env) *Promise {
			assert.Equal(t, Atom("😀"), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("query code", func(t *testing.T) {
		v := Variable("Code")
		ok, err := CharCode(Atom("😀"), v, func(env *Env) *Promise {
			assert.Equal(t, Integer(128512), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("char and code are variables", func(t *testing.T) {
		char, code := Variable("Char"), Variable("Code")

		ok, err := CharCode(char, code, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(&Compound{
			Functor: ",",
			Args:    []Term{char, code},
		}), err)
		assert.False(t, ok)
	})

	t.Run("char is neither a variable nor a one character atom", func(t *testing.T) {
		t.Run("atom", func(t *testing.T) {
			ok, err := CharCode(Atom("foo"), NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, typeErrorCharacter(Atom("foo")), err)
			assert.False(t, ok)
		})

		t.Run("non-atom", func(t *testing.T) {
			ok, err := CharCode(Integer(0), NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, typeErrorCharacter(Integer(0)), err)
			assert.False(t, ok)
		})
	})

	t.Run("code is neither a variable nor an integer", func(t *testing.T) {
		t.Run("char is variable", func(t *testing.T) {
			ok, err := CharCode(NewVariable(), Atom("foo"), Success, nil).Force(context.Background())
			assert.Equal(t, typeErrorInteger(Atom("foo")), err)
			assert.False(t, ok)
		})

		t.Run("char is a character", func(t *testing.T) {
			ok, err := CharCode(Atom("a"), Atom("x"), Success, nil).Force(context.Background())
			assert.Equal(t, typeErrorInteger(Atom("x")), err)
			assert.False(t, ok)
		})
	})

	t.Run("code is neither a variable nor a character-code", func(t *testing.T) {
		ok, err := CharCode(NewVariable(), Integer(-1), Success, nil).Force(context.Background())
		assert.Equal(t, representationError(Atom("character_code"), Atom("-1 is not a valid unicode code point.")), err)
		assert.False(t, ok)
	})
}

func TestVM_PutByte(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		var w mockWriter
		w.On("Write", []byte{97}).Return(1, nil).Once()
		defer w.AssertExpectations(t)

		s := Stream{Sink: &w, StreamType: StreamTypeBinary}

		var vm VM
		ok, err := vm.PutByte(&s, Integer(97), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("ng", func(t *testing.T) {
		var w mockWriter
		w.On("Write", []byte{97}).Return(0, errors.New("")).Once()
		defer w.AssertExpectations(t)

		s := Stream{Sink: &w, StreamType: StreamTypeBinary}

		var vm VM
		_, err := vm.PutByte(&s, Integer(97), Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("valid stream alias", func(t *testing.T) {
		var w mockWriter
		w.On("Write", []byte{97}).Return(1, nil).Once()
		defer w.AssertExpectations(t)

		s := Stream{Sink: &w, StreamType: StreamTypeBinary}

		vm := VM{
			streams: map[Term]*Stream{
				Atom("foo"): &s,
			},
		}
		ok, err := vm.PutByte(Atom("foo"), Integer(97), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		streamOrAlias := Variable("Stream")

		var vm VM
		ok, err := vm.PutByte(streamOrAlias, Integer(97), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("byt is a variable", func(t *testing.T) {
		byt := Variable("Byte")

		var vm VM
		ok, err := vm.PutByte(&Stream{Sink: &mockWriter{}, StreamType: StreamTypeBinary}, byt, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(byt), err)
		assert.False(t, ok)
	})

	t.Run("byt is neither a variable nor an byte", func(t *testing.T) {
		var vm VM
		ok, err := vm.PutByte(&Stream{Sink: &mockWriter{}, StreamType: StreamTypeBinary}, Atom("byte"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorByte(Atom("byte")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		var vm VM
		ok, err := vm.PutByte(Integer(0), Integer(97), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorStreamOrAlias(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an input stream", func(t *testing.T) {
		s := Variable("Stream")
		env := NewEnv().
			Bind(s, &Stream{Source: &mockReader{}})

		var vm VM
		ok, err := vm.PutByte(s, Integer(97), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorOutputStream(s), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a text stream", func(t *testing.T) {
		s := Variable("Stream")
		env := NewEnv().
			Bind(s, &Stream{
				Sink:       &mockWriter{},
				StreamType: StreamTypeText,
			})

		var vm VM
		ok, err := vm.PutByte(s, Integer(97), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorOutputTextStream(s), err)
		assert.False(t, ok)
	})
}

func TestVM_PutCode(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		var w mockWriter
		w.On("Write", []byte{0xf0, 0x9f, 0x98, 0x80}).Return(1, nil).Once()
		defer w.AssertExpectations(t)

		s := Stream{Sink: &w}

		var vm VM
		ok, err := vm.PutCode(&s, Integer('😀'), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("ng", func(t *testing.T) {
		var w mockWriter
		w.On("Write", []byte{0xf0, 0x9f, 0x98, 0x80}).Return(0, errors.New("")).Once()
		defer w.AssertExpectations(t)

		s := Stream{Sink: &w}

		var vm VM
		_, err := vm.PutCode(&s, Integer('😀'), Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("valid stream alias", func(t *testing.T) {
		var w mockWriter
		w.On("Write", []byte{0xf0, 0x9f, 0x98, 0x80}).Return(1, nil).Once()
		defer w.AssertExpectations(t)

		s := Stream{Sink: &w}

		vm := VM{
			streams: map[Term]*Stream{
				Atom("foo"): &s,
			},
		}
		ok, err := vm.PutCode(Atom("foo"), Integer('😀'), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		streamOrAlias := Variable("Stream")

		var vm VM
		ok, err := vm.PutCode(streamOrAlias, Integer(97), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("code is a variable", func(t *testing.T) {
		code := Variable("Code")

		var vm VM
		ok, err := vm.PutCode(&Stream{Sink: &mockWriter{}}, code, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(code), err)
		assert.False(t, ok)
	})

	t.Run("code is neither a variable nor an integer", func(t *testing.T) {
		var vm VM
		ok, err := vm.PutCode(&Stream{Sink: &mockWriter{}}, Atom("code"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Atom("code")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		var vm VM
		ok, err := vm.PutCode(Integer(0), Integer(97), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorStreamOrAlias(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var vm VM
		ok, err := vm.PutCode(Atom("foo"), Integer(97), Success, nil).Force(context.Background())
		assert.Equal(t, existenceErrorStream(Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an input stream", func(t *testing.T) {
		s := Variable("Stream")
		env := NewEnv().
			Bind(s, &Stream{Source: &mockReader{}})

		var vm VM
		ok, err := vm.PutCode(s, Integer(97), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorOutputStream(s), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a binary stream", func(t *testing.T) {
		s := Variable("Stream")
		env := NewEnv().
			Bind(s, &Stream{
				Sink:       &mockWriter{},
				StreamType: StreamTypeBinary,
			})

		var vm VM
		ok, err := vm.PutCode(s, Integer(97), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorOutputBinaryStream(s), err)
		assert.False(t, ok)
	})

	t.Run("code is an integer but not an character code", func(t *testing.T) {
		var vm VM
		ok, err := vm.PutCode(&Stream{Sink: &mockWriter{}}, Integer(-1), Success, nil).Force(context.Background())
		assert.Equal(t, representationError(Atom("character_code"), Atom("-1 is not a valid unicode code point.")), err)
		assert.False(t, ok)
	})

	t.Run("unknown stream alias", func(t *testing.T) {
		var vm VM
		_, err := vm.PutCode(Atom("foo"), Integer('😀'), Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("not a stream", func(t *testing.T) {
		var vm VM
		_, err := vm.PutCode(NewVariable(), Integer('😀'), Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("not a code", func(t *testing.T) {
		var w mockWriter
		defer w.AssertExpectations(t)

		s := Stream{Sink: &w}

		t.Run("not an integer", func(t *testing.T) {
			var vm VM
			_, err := vm.PutCode(&s, Atom("a"), Success, nil).Force(context.Background())
			assert.Error(t, err)
		})
	})
}

func TestVM_ReadTerm(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		v := Variable("Term")

		var vm VM
		ok, err := vm.ReadTerm(&Stream{Source: bufio.NewReader(strings.NewReader("foo."))}, v, List(), func(env *Env) *Promise {
			assert.Equal(t, Atom("foo"), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("valid stream alias", func(t *testing.T) {
		v := Variable("Term")

		s := Stream{Source: bufio.NewReader(strings.NewReader("foo."))}

		vm := VM{
			streams: map[Term]*Stream{
				Atom("foo"): &s,
			},
		}
		ok, err := vm.ReadTerm(Atom("foo"), v, List(), func(env *Env) *Promise {
			assert.Equal(t, Atom("foo"), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("singletons", func(t *testing.T) {
		v, singletons := Variable("Term"), Variable("Singletons")

		var vm VM
		ok, err := vm.ReadTerm(&Stream{Source: bufio.NewReader(strings.NewReader("f(X, X, Y)."))}, v, List(&Compound{
			Functor: "singletons",
			Args:    []Term{singletons},
		}), func(env *Env) *Promise {
			c, ok := env.Resolve(v).(*Compound)
			assert.True(t, ok)
			assert.Equal(t, Atom("f"), c.Functor)
			assert.Len(t, c.Args, 3)

			x, ok := c.Args[0].(Variable)
			assert.True(t, ok)
			assert.Equal(t, x, c.Args[1])

			y, ok := c.Args[2].(Variable)
			assert.True(t, ok)

			assert.Equal(t, List(y), env.Resolve(singletons))

			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("variables", func(t *testing.T) {
		v, variables := Variable("Term"), Variable("Variables")

		var vm VM
		ok, err := vm.ReadTerm(&Stream{Source: bufio.NewReader(strings.NewReader("f(X, X, Y)."))}, v, List(&Compound{
			Functor: "variables",
			Args:    []Term{variables},
		}), func(env *Env) *Promise {
			c, ok := env.Resolve(v).(*Compound)
			assert.True(t, ok)
			assert.Equal(t, Atom("f"), c.Functor)
			assert.Len(t, c.Args, 3)

			x, ok := c.Args[0].(Variable)
			assert.True(t, ok)
			assert.Equal(t, x, c.Args[1])

			y, ok := c.Args[2].(Variable)
			assert.True(t, ok)

			assert.Equal(t, List(x, y), env.Resolve(variables))

			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("variable_names", func(t *testing.T) {
		v, variableNames := Variable("Term"), Variable("VariableNames")

		var vm VM
		ok, err := vm.ReadTerm(&Stream{Source: bufio.NewReader(strings.NewReader("f(X, X, Y)."))}, v, List(&Compound{
			Functor: "variable_names",
			Args:    []Term{variableNames},
		}), func(env *Env) *Promise {
			c, ok := env.Resolve(v).(*Compound)
			assert.True(t, ok)
			assert.Equal(t, Atom("f"), c.Functor)
			assert.Len(t, c.Args, 3)

			x, ok := c.Args[0].(Variable)
			assert.True(t, ok)
			assert.Equal(t, x, c.Args[1])

			y, ok := c.Args[2].(Variable)
			assert.True(t, ok)

			assert.Equal(t, List(
				&Compound{
					Functor: "=",
					Args:    []Term{Atom("X"), x},
				},
				&Compound{
					Functor: "=",
					Args:    []Term{Atom("Y"), y},
				},
			), env.Resolve(variableNames))

			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("multiple reads", func(t *testing.T) {
		s := Stream{Source: bufio.NewReader(strings.NewReader(`
foo(a).
foo(b).
foo(c).
`))}

		v := Variable("Term")

		var vm VM

		ok, err := vm.ReadTerm(&s, v, List(), func(env *Env) *Promise {
			assert.Equal(t, &Compound{Functor: "foo", Args: []Term{Atom("a")}}, env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = vm.ReadTerm(&s, v, List(), func(env *Env) *Promise {
			assert.Equal(t, &Compound{Functor: "foo", Args: []Term{Atom("b")}}, env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = vm.ReadTerm(&s, &v, List(), func(env *Env) *Promise {
			assert.Equal(t, &Compound{Functor: "foo", Args: []Term{Atom("c")}}, env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		streamOrAlias := Variable("Stream")

		var vm VM
		ok, err := vm.ReadTerm(streamOrAlias, NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("options is a partial list or a list with an element which is a variable", func(t *testing.T) {
		t.Run("partial list", func(t *testing.T) {
			options := ListRest(Variable("Rest"),
				&Compound{Functor: "variables", Args: []Term{Variable("VL")}},
			)

			var vm VM
			ok, err := vm.ReadTerm(&Stream{Source: &mockReader{}}, NewVariable(), options, Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(options), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			option := Variable("Option")

			var vm VM
			ok, err := vm.ReadTerm(&Stream{Source: &mockReader{}}, NewVariable(), List(option, &Compound{Functor: "variables", Args: []Term{Variable("VL")}}), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(option), err)
			assert.False(t, ok)
		})
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		var vm VM
		ok, err := vm.ReadTerm(Integer(0), NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorStreamOrAlias(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("options is neither a partial list nor a list", func(t *testing.T) {
		var vm VM
		ok, err := vm.ReadTerm(&Stream{Source: &mockReader{}}, NewVariable(), Atom("options"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorList(Atom("options")), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the Options list is neither a variable nor a valid read-option", func(t *testing.T) {
		var vm VM
		ok, err := vm.ReadTerm(&Stream{Source: &mockReader{}}, NewVariable(), List(&Compound{
			Functor: "unknown",
			Args:    []Term{Atom("option")},
		}), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorReadOption(&Compound{
			Functor: "unknown",
			Args:    []Term{Atom("option")},
		}), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var vm VM
		ok, err := vm.ReadTerm(Atom("foo"), NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, existenceErrorStream(Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		s := Variable("Stream")
		env := NewEnv().
			Bind(s, &Stream{Sink: &mockWriter{}})

		var vm VM
		ok, err := vm.ReadTerm(s, NewVariable(), List(), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorInputStream(s), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a binary stream", func(t *testing.T) {
		s := Variable("Stream")
		env := NewEnv().
			Bind(s, &Stream{
				Source:     bufio.NewReader(&mockReader{}),
				StreamType: StreamTypeBinary,
			})

		var vm VM
		ok, err := vm.ReadTerm(s, NewVariable(), List(), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorInputBinaryStream(s), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias has stream properties end_of_stream(past) and eof_action(error)", func(t *testing.T) {
		var r mockReader
		r.On("Read", mock.Anything).Return(0, io.EOF)

		s := Variable("Stream")
		env := NewEnv().
			Bind(s, &Stream{
				Source:    bufio.NewReader(&r),
				EofAction: EofActionError,
			})

		var vm VM
		ok, err := vm.ReadTerm(s, NewVariable(), List(), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorInputPastEndOfStream(s), err)
		assert.False(t, ok)
	})

	t.Run("one or more characters were input, but they cannot be parsed as a sequence of tokens", func(t *testing.T) {
		var vm VM
		ok, err := vm.ReadTerm(&Stream{Source: bufio.NewReader(strings.NewReader("foo bar baz."))}, NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, syntaxErrorUnexpectedToken(Atom("unexpected token: <ident bar>")), err)
		assert.False(t, ok)
	})

	t.Run("the sequence of tokens cannot be parsed as a term using the current set of operator definitions", func(t *testing.T) {
		var vm VM
		ok, err := vm.ReadTerm(&Stream{Source: bufio.NewReader(strings.NewReader("X = a."))}, NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, syntaxErrorUnexpectedToken(Atom("unexpected token: <graphical =>")), err)
		assert.False(t, ok)
	})
}

func TestVM_GetByte(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		s := Stream{Source: strings.NewReader("a"), StreamType: StreamTypeBinary}
		v := Variable("Byte")

		var vm VM
		ok, err := vm.GetByte(&s, v, func(env *Env) *Promise {
			assert.Equal(t, Integer(97), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("valid stream alias", func(t *testing.T) {
		s := Stream{Source: strings.NewReader("a"), StreamType: StreamTypeBinary}
		v := Variable("Byte")

		vm := VM{
			streams: map[Term]*Stream{
				Atom("foo"): &s,
			},
		}
		ok, err := vm.GetByte(Atom("foo"), v, func(env *Env) *Promise {
			assert.Equal(t, Integer(97), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("eof", func(t *testing.T) {
		s := Stream{Source: strings.NewReader(""), StreamType: StreamTypeBinary}
		v := Variable("Byte")

		var vm VM
		ok, err := vm.GetByte(&s, v, func(env *Env) *Promise {
			assert.Equal(t, Integer(-1), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("error", func(t *testing.T) {
		var m mockReader
		m.On("Read", make([]byte, 1)).Return(0, errors.New("failed")).Once()
		defer m.AssertExpectations(t)

		s := Stream{Source: &m, StreamType: StreamTypeBinary}

		var vm VM

		v := Variable("V")
		_, err := vm.GetByte(&s, v, Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		streamOrAlias := Variable("Stream")
		var vm VM
		ok, err := vm.GetByte(streamOrAlias, Variable("InByte"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("inByte is neither a variable nor an in-byte", func(t *testing.T) {
		var vm VM
		ok, err := vm.GetByte(&Stream{Source: &mockReader{}, StreamType: StreamTypeBinary}, Atom("inByte"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInByte(Atom("inByte")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream-term or alias", func(t *testing.T) {
		var vm VM
		ok, err := vm.GetByte(Integer(0), Variable("InByte"), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorStreamOrAlias(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var vm VM
		ok, err := vm.GetByte(Atom("foo"), Variable("InByte"), Success, nil).Force(context.Background())
		assert.Equal(t, existenceErrorStream(Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		streamOrAlias := Variable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, &Stream{Sink: &mockWriter{}})

		var vm VM
		ok, err := vm.GetByte(streamOrAlias, Variable("InByte"), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorInputStream(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a text stream", func(t *testing.T) {
		streamOrAlias := Variable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, &Stream{Source: &mockReader{}})

		var vm VM
		ok, err := vm.GetByte(streamOrAlias, Variable("InByte"), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorInputTextStream(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias has stream properties end_of_stream(past) and eof_action(error)", func(t *testing.T) {
		var r mockReader
		r.On("Read", mock.Anything).Return(0, io.EOF)

		streamOrAlias := Variable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, &Stream{
				Source:     &r,
				StreamType: StreamTypeBinary,
				EofAction:  EofActionError,
			})

		var vm VM
		ok, err := vm.GetByte(streamOrAlias, Variable("InByte"), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorInputPastEndOfStream(streamOrAlias), err)
		assert.False(t, ok)
	})
}

func TestVM_GetChar(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		s := Stream{Source: bufio.NewReader(strings.NewReader("😀"))}
		v := Variable("Char")

		var vm VM
		ok, err := vm.GetChar(&s, v, func(env *Env) *Promise {
			assert.Equal(t, Atom("😀"), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("valid stream alias", func(t *testing.T) {
		s := Stream{Source: bufio.NewReader(strings.NewReader("😀"))}
		v := Variable("Char")

		vm := VM{
			streams: map[Term]*Stream{
				Atom("foo"): &s,
			},
		}
		ok, err := vm.GetChar(Atom("foo"), v, func(env *Env) *Promise {
			assert.Equal(t, Atom("😀"), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("non buffered stream", func(t *testing.T) {
		s := Variable("Stream")
		env := NewEnv().
			Bind(s, &Stream{Source: strings.NewReader("")})

		var vm VM
		ok, err := vm.GetChar(s, NewVariable(), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorInputBufferedStream(s), err)
		assert.False(t, ok)
	})

	t.Run("eof", func(t *testing.T) {
		s := Stream{Source: bufio.NewReader(strings.NewReader(""))}
		v := Variable("Char")

		var vm VM
		ok, err := vm.GetChar(&s, v, func(env *Env) *Promise {
			assert.Equal(t, Atom("end_of_file"), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("error", func(t *testing.T) {
		var m mockReader
		m.On("Read", mock.Anything).Return(0, errors.New("failed")).Once()
		defer m.AssertExpectations(t)

		s := Stream{Source: bufio.NewReader(&m)}

		v := Variable("V")

		var vm VM
		ok, err := vm.GetChar(&s, v, Success, nil).Force(context.Background())
		assert.Equal(t, SystemError(errors.New("failed")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		streamOrAlias := Variable("Stream")

		var vm VM
		ok, err := vm.GetChar(streamOrAlias, Variable("Char"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("char is neither a variable nor an in-character", func(t *testing.T) {
		var vm VM
		ok, err := vm.GetChar(&Stream{Source: bufio.NewReader(&mockReader{})}, Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInCharacter(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		var vm VM
		ok, err := vm.GetChar(Integer(0), Variable("Char"), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorStreamOrAlias(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		streamOrAlias := Variable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, &Stream{Sink: &mockWriter{}})

		var vm VM
		ok, err := vm.GetChar(streamOrAlias, Variable("Char"), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorInputStream(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a binary stream", func(t *testing.T) {
		streamOrAlias := Variable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, &Stream{
				Source:     bufio.NewReader(&mockReader{}),
				StreamType: StreamTypeBinary,
			})

		var vm VM
		ok, err := vm.GetChar(streamOrAlias, Variable("Char"), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorInputBinaryStream(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias has stream properties end_of_stream(past) and eof_action(error)", func(t *testing.T) {
		var r mockReader
		r.On("Read", mock.Anything).Return(0, io.EOF)

		streamOrAlias := Variable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, &Stream{
				Source:    bufio.NewReader(&r),
				EofAction: EofActionError,
			})

		var vm VM
		ok, err := vm.GetChar(streamOrAlias, Variable("Char"), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorInputPastEndOfStream(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("the entity input from the stream is not a character", func(t *testing.T) {
		streamOrAlias := Variable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, &Stream{
				Source: bufio.NewReader(bytes.NewBufferString(string(unicode.ReplacementChar))),
			})

		var vm VM
		ok, err := vm.GetChar(streamOrAlias, Variable("Char"), Success, env).Force(context.Background())
		assert.Equal(t, representationError(Atom("character"), Atom("invalid character.")), err)
		assert.False(t, ok)
	})
}

func TestVM_PeekByte(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		s := Stream{Source: bufio.NewReader(strings.NewReader("abc")), StreamType: StreamTypeBinary}
		v := Variable("Byte")

		var vm VM
		ok, err := vm.PeekByte(&s, v, func(env *Env) *Promise {
			assert.Equal(t, Integer(97), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = vm.PeekByte(&s, v, Success, nil).Force(context.Background()) // 'a' again
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("valid stream alias", func(t *testing.T) {
		s := Stream{Source: bufio.NewReader(strings.NewReader("abc")), StreamType: StreamTypeBinary}
		v := Variable("Byte")

		vm := VM{
			streams: map[Term]*Stream{
				Atom("foo"): &s,
			},
		}
		ok, err := vm.PeekByte(Atom("foo"), v, func(env *Env) *Promise {
			assert.Equal(t, Integer(97), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("non buffered stream", func(t *testing.T) {
		s := Variable("Stream")
		env := NewEnv().
			Bind(s, &Stream{
				Source:     strings.NewReader(""),
				StreamType: StreamTypeBinary,
			})

		var vm VM
		ok, err := vm.PeekByte(s, NewVariable(), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorInputBufferedStream(s), err)
		assert.False(t, ok)
	})

	t.Run("eof", func(t *testing.T) {
		s := Stream{Source: bufio.NewReader(strings.NewReader("")), StreamType: StreamTypeBinary}
		v := Variable("Byte")

		var vm VM
		ok, err := vm.PeekByte(&s, v, func(env *Env) *Promise {
			assert.Equal(t, Integer(-1), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("error", func(t *testing.T) {
		var m mockReader
		m.On("Read", mock.Anything).Return(0, errors.New("failed")).Once()
		defer m.AssertExpectations(t)

		s := Stream{Source: bufio.NewReader(&m), StreamType: StreamTypeBinary}

		v := Variable("V")

		var vm VM
		ok, err := vm.PeekByte(&s, v, Success, nil).Force(context.Background())
		assert.Equal(t, SystemError(errors.New("failed")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		streamOrAlias := Variable("Stream")

		var vm VM
		ok, err := vm.PeekByte(streamOrAlias, Variable("Byte"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("inByte is neither a variable nor an in-byte", func(t *testing.T) {
		var vm VM
		ok, err := vm.PeekByte(&Stream{Source: bufio.NewReader(&mockReader{}), StreamType: StreamTypeBinary}, Atom("byte"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInByte(Atom("byte")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		var vm VM
		ok, err := vm.PeekByte(Integer(0), Variable("Byte"), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorStreamOrAlias(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		streamOrAlias := Variable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, &Stream{Sink: &mockWriter{}})

		var vm VM
		ok, err := vm.PeekByte(streamOrAlias, Variable("Byte"), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorInputStream(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a text stream", func(t *testing.T) {
		streamOrAlias := Variable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, &Stream{
				Source:     bufio.NewReader(&mockReader{}),
				StreamType: StreamTypeText,
			})

		var vm VM
		ok, err := vm.PeekByte(streamOrAlias, Variable("Byte"), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorInputTextStream(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias has stream properties end_of_stream(past) and eof_action(error)", func(t *testing.T) {
		var r mockReader
		r.On("Read", mock.Anything).Return(0, io.EOF)

		streamOrAlias := Variable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, &Stream{
				Source:     bufio.NewReader(&r),
				StreamType: StreamTypeBinary,
				EofAction:  EofActionError,
			})

		var vm VM
		ok, err := vm.PeekByte(streamOrAlias, Variable("Byte"), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorInputPastEndOfStream(streamOrAlias), err)
		assert.False(t, ok)
	})
}

func TestVM_PeekChar(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		s := Stream{Source: bufio.NewReader(strings.NewReader("😀❗"))}
		v := Variable("Char")

		var vm VM
		ok, err := vm.PeekChar(&s, v, func(env *Env) *Promise {
			assert.Equal(t, Atom("😀"), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = vm.PeekChar(&s, v, func(env *Env) *Promise {
			assert.Equal(t, Atom("😀"), env.Resolve(v)) // '😀' again
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("valid stream alias", func(t *testing.T) {
		s := Stream{Source: bufio.NewReader(strings.NewReader("😀❗"))}
		v := Variable("Char")

		vm := VM{
			streams: map[Term]*Stream{
				Atom("foo"): &s,
			},
		}
		ok, err := vm.PeekChar(Atom("foo"), v, func(env *Env) *Promise {
			assert.Equal(t, Atom("😀"), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("non buffered stream", func(t *testing.T) {
		s := Variable("Stream")
		env := NewEnv().
			Bind(s, &Stream{Source: strings.NewReader("")})

		var vm VM
		ok, err := vm.PeekChar(s, NewVariable(), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorInputBufferedStream(s), err)
		assert.False(t, ok)
	})

	t.Run("eof", func(t *testing.T) {
		s := Stream{Source: bufio.NewReader(strings.NewReader(""))}
		v := Variable("Char")

		var vm VM
		ok, err := vm.PeekChar(&s, v, func(env *Env) *Promise {
			assert.Equal(t, Atom("end_of_file"), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("error", func(t *testing.T) {
		var m mockReader
		m.On("Read", mock.Anything).Return(0, errors.New("failed")).Once()
		defer m.AssertExpectations(t)

		s := Stream{Source: bufio.NewReader(&m)}
		v := Variable("V")

		var vm VM
		ok, err := vm.PeekChar(&s, v, Success, nil).Force(context.Background())
		assert.Equal(t, SystemError(errors.New("failed")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		streamOrAlias := Variable("Stream")

		var vm VM
		ok, err := vm.PeekChar(streamOrAlias, Variable("Char"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("char is neither a variable nor an in-character", func(t *testing.T) {
		var vm VM
		ok, err := vm.PeekChar(&Stream{Source: bufio.NewReader(&mockReader{})}, Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInCharacter(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		var vm VM
		ok, err := vm.PeekChar(Integer(0), Variable("Char"), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorStreamOrAlias(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		streamOrAlias := Variable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, &Stream{Sink: &mockWriter{}})

		var vm VM
		ok, err := vm.PeekChar(streamOrAlias, Variable("Char"), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorInputStream(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a binary stream", func(t *testing.T) {
		streamOrAlias := Variable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, &Stream{
				Source:     bufio.NewReader(&mockReader{}),
				StreamType: StreamTypeBinary,
			})

		var vm VM
		ok, err := vm.PeekChar(streamOrAlias, Variable("Char"), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorInputBinaryStream(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias has stream properties end_of_stream(past) and eof_action(error)", func(t *testing.T) {
		var r mockReader
		r.On("Read", mock.Anything).Return(0, io.EOF)

		streamOrAlias := Variable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, &Stream{
				Source:    bufio.NewReader(&r),
				EofAction: EofActionError,
			})

		var vm VM
		ok, err := vm.PeekChar(streamOrAlias, Variable("Char"), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorInputPastEndOfStream(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("the entity input from the stream is not a character", func(t *testing.T) {
		streamOrAlias := Variable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, &Stream{
				Source: bufio.NewReader(bytes.NewBufferString(string(unicode.ReplacementChar))),
			})

		var vm VM
		ok, err := vm.PeekChar(streamOrAlias, Variable("Char"), Success, env).Force(context.Background())
		assert.Equal(t, representationError(Atom("character"), Atom("invalid character.")), err)
		assert.False(t, ok)
	})
}

func Test_Halt(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		var exitCalled bool
		osExit = func(code int) {
			assert.Equal(t, 2, code)
			exitCalled = true
		}
		defer func() {
			osExit = os.Exit
		}()

		ok, err := Halt(Integer(2), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.True(t, exitCalled)
	})

	t.Run("n is a variable", func(t *testing.T) {
		n := Variable("N")

		ok, err := Halt(n, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(n), err)
		assert.False(t, ok)
	})

	t.Run("n is neither a variable nor an integer", func(t *testing.T) {
		ok, err := Halt(Atom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Atom("foo")), err)
		assert.False(t, ok)
	})
}

func TestVM_Clause(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		x := Variable("X")
		what, body := Variable("What"), Variable("Body")

		var c int

		vm := VM{
			procedures: map[ProcedureIndicator]procedure{
				{Name: "green", Arity: 1}: clauses{
					{raw: &Compound{
						Functor: ":-", Args: []Term{
							&Compound{Functor: "green", Args: []Term{x}},
							&Compound{Functor: "moldy", Args: []Term{x}},
						},
					}},
					{raw: &Compound{Functor: "green", Args: []Term{Atom("kermit")}}},
				},
			},
		}
		ok, err := vm.Clause(&Compound{
			Functor: "green",
			Args:    []Term{what},
		}, body, func(env *Env) *Promise {
			switch c {
			case 0:
				assert.True(t, env.Resolve(what).(Variable).Generated())
				b, ok := env.Resolve(body).(*Compound)
				assert.True(t, ok)
				assert.Equal(t, Atom("moldy"), b.Functor)
				assert.Len(t, b.Args, 1)
				assert.True(t, b.Args[0].(Variable).Generated())
			case 1:
				assert.Equal(t, Atom("kermit"), env.Resolve(what))
				assert.Equal(t, Atom("true"), env.Resolve(body))
			default:
				assert.Fail(t, "unreachable")
			}
			c++
			return Bool(false)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("head is a variable", func(t *testing.T) {
		head := Variable("Head")

		var vm VM
		ok, err := vm.Clause(head, Atom("true"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(head), err)
		assert.False(t, ok)
	})

	t.Run("head is neither a variable nor a predication", func(t *testing.T) {
		var vm VM
		ok, err := vm.Clause(Integer(0), Atom("true"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorCallable(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("the predicate indicator Pred of Head is that of a private (ie. Not public) procedure", func(t *testing.T) {
		what, body := Variable("What"), Variable("Body")

		vm := VM{
			procedures: map[ProcedureIndicator]procedure{
				{Name: "green", Arity: 1}: predicate1(func(t Term, f func(*Env) *Promise, env *Env) *Promise {
					return Bool(true)
				}),
			},
		}
		ok, err := vm.Clause(&Compound{
			Functor: "green",
			Args:    []Term{what},
		}, body, Success, nil).Force(context.Background())
		assert.Equal(t, permissionErrorAccessPrivateProcedure(&Compound{
			Functor: "/",
			Args:    []Term{Atom("green"), Integer(1)},
		}), err)
		assert.False(t, ok)
	})

	t.Run("body is neither a variable nor a callable term", func(t *testing.T) {
		var vm VM
		ok, err := vm.Clause(Atom("foo"), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorCallable(Integer(0)), err)
		assert.False(t, ok)
	})
}

func TestAtomLength(t *testing.T) {
	t.Run("ascii", func(t *testing.T) {
		ok, err := AtomLength(Atom("abc"), Integer(3), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("emoji", func(t *testing.T) {
		ok, err := AtomLength(Atom("😀"), Integer(1), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("atom is a variable", func(t *testing.T) {
		atom := Variable("Atom")
		ok, err := AtomLength(atom, Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(atom), err)
		assert.False(t, ok)
	})

	t.Run("atom is neither a variable nor an atom", func(t *testing.T) {
		ok, err := AtomLength(Integer(2), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorAtom(Integer(2)), err)
		assert.False(t, ok)
	})

	t.Run("length is neither a variable nor an integer", func(t *testing.T) {
		ok, err := AtomLength(Atom("😀"), Atom("1"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Atom("1")), err)
		assert.False(t, ok)
	})

	t.Run("length is an integer less than zero", func(t *testing.T) {
		ok, err := AtomLength(Atom("😀"), Integer(-1), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorNotLessThanZero(Integer(-1)), err)
		assert.False(t, ok)
	})
}

func TestAtomConcat(t *testing.T) {
	t.Run("atom3 is a variable", func(t *testing.T) {
		atom3 := Variable("Atom3")

		ok, err := AtomConcat(Atom("foo"), Atom("bar"), atom3, func(env *Env) *Promise {
			assert.Equal(t, Atom("foobar"), env.Resolve(atom3))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("atom3 is an atom", func(t *testing.T) {
		var c int
		v1, v2 := Variable("V1"), Variable("V2")
		ok, err := AtomConcat(v1, v2, Atom("foo"), func(env *Env) *Promise {
			switch c {
			case 0:
				assert.Equal(t, Atom(""), env.Resolve(v1))
				assert.Equal(t, Atom("foo"), env.Resolve(v2))
			case 1:
				assert.Equal(t, Atom("f"), env.Resolve(v1))
				assert.Equal(t, Atom("oo"), env.Resolve(v2))
			case 2:
				assert.Equal(t, Atom("fo"), env.Resolve(v1))
				assert.Equal(t, Atom("o"), env.Resolve(v2))
			case 3:
				assert.Equal(t, Atom("foo"), env.Resolve(v1))
				assert.Equal(t, Atom(""), env.Resolve(v2))
			default:
				assert.Fail(t, "unreachable")
			}
			c++
			return Bool(false)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("atom1 and atom3 are variables", func(t *testing.T) {
		atom1, atom3 := Variable("Atom1"), Variable("Atom3")

		ok, err := AtomConcat(atom1, Atom("bar"), atom3, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(&Compound{
			Functor: ",",
			Args:    []Term{atom1, atom3},
		}), err)
		assert.False(t, ok)
	})

	t.Run("atom2 and atom3 are variables", func(t *testing.T) {
		atom2, atom3 := Variable("Atom2"), Variable("Atom3")

		ok, err := AtomConcat(Atom("foo"), atom2, atom3, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(&Compound{
			Functor: ",",
			Args:    []Term{atom2, atom3},
		}), err)
		assert.False(t, ok)
	})

	t.Run("atom1 is neither a variable nor an atom", func(t *testing.T) {
		t.Run("atom3 is a variable", func(t *testing.T) {
			ok, err := AtomConcat(Integer(1), Atom("bar"), Variable("Atom3"), Success, nil).Force(context.Background())
			assert.Equal(t, typeErrorAtom(Integer(1)), err)
			assert.False(t, ok)
		})

		t.Run("atom3 is an atom", func(t *testing.T) {
			ok, err := AtomConcat(Integer(1), Atom("bar"), Atom("foobar"), Success, nil).Force(context.Background())
			assert.Equal(t, typeErrorAtom(Integer(1)), err)
			assert.False(t, ok)
		})
	})

	t.Run("atom2 is neither a variable nor an atom", func(t *testing.T) {
		t.Run("atom3 is a variable", func(t *testing.T) {
			ok, err := AtomConcat(Atom("foo"), Integer(2), Variable("Atom3"), Success, nil).Force(context.Background())
			assert.Equal(t, typeErrorAtom(Integer(2)), err)
			assert.False(t, ok)
		})

		t.Run("atom3 is an atom", func(t *testing.T) {
			ok, err := AtomConcat(Atom("foo"), Integer(2), Atom("foobar"), Success, nil).Force(context.Background())
			assert.Equal(t, typeErrorAtom(Integer(2)), err)
			assert.False(t, ok)
		})
	})

	t.Run("atom3 is neither a variable nor an atom", func(t *testing.T) {
		ok, err := AtomConcat(Atom("foo"), Atom("bar"), Integer(3), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorAtom(Integer(3)), err)
		assert.False(t, ok)
	})
}

func TestSubAtom(t *testing.T) {
	t.Run("multiple solutions", func(t *testing.T) {
		before, length, after := Variable("Before"), Variable("Length"), Variable("After")
		var c int
		ok, err := SubAtom(Atom("xATGATGAxATGAxATGAx"), before, length, after, Atom("ATGA"), func(env *Env) *Promise {
			switch c {
			case 0:
				assert.Equal(t, Integer(1), env.Resolve(before))
				assert.Equal(t, Integer(4), env.Resolve(length))
				assert.Equal(t, Integer(14), env.Resolve(after))
			case 1:
				assert.Equal(t, Integer(4), env.Resolve(before))
				assert.Equal(t, Integer(4), env.Resolve(length))
				assert.Equal(t, Integer(11), env.Resolve(after))
			case 2:
				assert.Equal(t, Integer(9), env.Resolve(before))
				assert.Equal(t, Integer(4), env.Resolve(length))
				assert.Equal(t, Integer(6), env.Resolve(after))
			case 3:
				assert.Equal(t, Integer(14), env.Resolve(before))
				assert.Equal(t, Integer(4), env.Resolve(length))
				assert.Equal(t, Integer(1), env.Resolve(after))
			default:
				assert.Fail(t, "unreachable")
			}
			c++
			return Bool(false)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("get the first char", func(t *testing.T) {
		char := Variable("Char")
		ok, err := SubAtom(Atom("a"), Integer(0), Integer(1), Integer(0), char, func(env *Env) *Promise {
			assert.Equal(t, Atom("a"), env.Resolve(char))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("atom is a variable", func(t *testing.T) {
		atom := Variable("Atom")
		ok, err := SubAtom(atom, Variable("Before"), Variable("Length"), Variable("After"), Variable("SubAtom"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(atom), err)
		assert.False(t, ok)
	})

	t.Run("atom is neither a variable nor an atom", func(t *testing.T) {
		ok, err := SubAtom(Integer(0), Variable("Before"), Variable("Length"), Variable("After"), Variable("SubAtom"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorAtom(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("subAtom is neither a variable nor an atom", func(t *testing.T) {
		ok, err := SubAtom(Atom("foo"), Variable("Before"), Variable("Length"), Variable("After"), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorAtom(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("before is neither a variable nor an integer", func(t *testing.T) {
		ok, err := SubAtom(Atom("foo"), Atom("before"), Variable("Length"), Variable("After"), Variable("SubAtom"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Atom("before")), err)
		assert.False(t, ok)
	})

	t.Run("length is neither a variable nor an integer", func(t *testing.T) {
		ok, err := SubAtom(Atom("foo"), Variable("Before"), Atom("length"), Variable("After"), Variable("SubAtom"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Atom("length")), err)
		assert.False(t, ok)
	})

	t.Run("after is neither a variable nor an integer", func(t *testing.T) {
		ok, err := SubAtom(Atom("foo"), Variable("Before"), Variable("Length"), Atom("after"), Variable("SubAtom"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Atom("after")), err)
		assert.False(t, ok)
	})

	t.Run("before is an integer less than zero", func(t *testing.T) {
		ok, err := SubAtom(Atom("foo"), Integer(-1), Variable("Length"), Variable("After"), Variable("SubAtom"), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorNotLessThanZero(Integer(-1)), err)
		assert.False(t, ok)
	})

	t.Run("length is an integer less than zero", func(t *testing.T) {
		ok, err := SubAtom(Atom("foo"), Variable("Before"), Integer(-1), Variable("After"), Variable("SubAtom"), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorNotLessThanZero(Integer(-1)), err)
		assert.False(t, ok)
	})

	t.Run("after is an integer less than zero", func(t *testing.T) {
		ok, err := SubAtom(Atom("foo"), Variable("Before"), Variable("Length"), Integer(-1), Variable("SubAtom"), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorNotLessThanZero(Integer(-1)), err)
		assert.False(t, ok)
	})
}

func TestAtomChars(t *testing.T) {
	t.Run("break down", func(t *testing.T) {
		chars := Variable("Char")

		ok, err := AtomChars(Atom("foo"), chars, func(env *Env) *Promise {
			assert.Equal(t, List(Atom("f"), Atom("o"), Atom("o")), env.Resolve(chars))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("construct", func(t *testing.T) {
		atom := Variable("Atom")

		ok, err := AtomChars(atom, List(Atom("f"), Atom("o"), Atom("o")), func(env *Env) *Promise {
			assert.Equal(t, Atom("foo"), env.Resolve(atom))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		_, err = AtomChars(NewVariable(), List(Integer(102), Integer(111), Integer(111)), Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("atom is a variable and List is a partial list or list with an element which is a variable", func(t *testing.T) {
		t.Run("partial list", func(t *testing.T) {
			chars := ListRest(Variable("Rest"),
				Atom("0"),
				Atom("0"),
			)

			ok, err := AtomChars(NewVariable(), chars, Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(chars), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			char := Variable("Char")
			ok, err := AtomChars(NewVariable(), List(char, Atom("o"), Atom("o")), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(char), err)
			assert.False(t, ok)
		})
	})

	t.Run("atom is neither a variable nor an atom", func(t *testing.T) {
		ok, err := AtomChars(Integer(0), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorAtom(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("atom is a variable and List is neither a list nor a partial list", func(t *testing.T) {
		ok, err := AtomChars(NewVariable(), Atom("chars"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorList(Atom("chars")), err)
		assert.False(t, ok)
	})

	t.Run("atom is a variable and an element E of the list List is neither a variable nor a one-character atom", func(t *testing.T) {
		t.Run("not a one-character atom", func(t *testing.T) {
			ok, err := AtomChars(NewVariable(), List(Atom("chars")), Success, nil).Force(context.Background())
			assert.Equal(t, typeErrorCharacter(Atom("chars")), err)
			assert.False(t, ok)
		})

		t.Run("not an atom", func(t *testing.T) {
			ok, err := AtomChars(NewVariable(), List(Integer(0)), Success, nil).Force(context.Background())
			assert.Equal(t, typeErrorCharacter(Integer(0)), err)
			assert.False(t, ok)
		})
	})
}

func TestAtomCodes(t *testing.T) {
	t.Run("break up", func(t *testing.T) {
		codes := Variable("Codes")

		ok, err := AtomCodes(Atom("foo"), codes, func(env *Env) *Promise {
			assert.Equal(t, List(Integer(102), Integer(111), Integer(111)), env.Resolve(codes))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("construct", func(t *testing.T) {
		atom := Variable("Atom")

		ok, err := AtomCodes(atom, List(Integer(102), Integer(111), Integer(111)), func(env *Env) *Promise {
			assert.Equal(t, Atom("foo"), env.Resolve(atom))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("atom is a variable and List is a partial list or list with an element which is a variable", func(t *testing.T) {
		t.Run("partial list", func(t *testing.T) {
			codes := ListRest(Variable("Rest"),
				Integer(111),
				Integer(111),
			)
			ok, err := AtomCodes(NewVariable(), codes, Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(codes), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			code := Variable("Code")

			ok, err := AtomCodes(NewVariable(), List(code, Integer(111), Integer(111)), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(code), err)
			assert.False(t, ok)
		})
	})

	t.Run("atom is neither a variable nor an atom", func(t *testing.T) {
		ok, err := AtomCodes(Integer(0), List(Integer(102), Integer(111), Integer(111)), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorAtom(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("atom is a variable and List is neither a list nor a partial list", func(t *testing.T) {
		ok, err := AtomCodes(NewVariable(), Atom("codes"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorList(Atom("codes")), err)
		assert.False(t, ok)
	})

	t.Run("atom is a variable and an element E of the list List is neither a variable nor a character-code", func(t *testing.T) {
		ok, err := AtomCodes(NewVariable(), List(Atom("f"), Integer(111), Integer(111)), Success, nil).Force(context.Background())
		assert.Equal(t, representationError(Atom("character_code"), Atom("invalid character code.")), err)
		assert.False(t, ok)
	})
}

func TestNumberChars(t *testing.T) {
	t.Run("number to chars", func(t *testing.T) {
		chars := Variable("Chars")

		ok, err := NumberChars(Float(23.4), chars, func(env *Env) *Promise {
			assert.Equal(t, List(Atom("2"), Atom("3"), Atom("."), Atom("4")), env.Resolve(chars))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("chars to number", func(t *testing.T) {
		num := Variable("Num")

		ok, err := NumberChars(num, List(Atom("2"), Atom("3"), Atom("."), Atom("4")), func(env *Env) *Promise {
			assert.Equal(t, Float(23.4), env.Resolve(num))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("both provided", func(t *testing.T) {
		t.Run("3.3", func(t *testing.T) {
			ok, err := NumberChars(Float(3.3), List(Atom("3"), Atom("."), Atom("3")), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("3.3E+0", func(t *testing.T) {
			ok, err := NumberChars(Float(3.3), List(Atom("3"), Atom("."), Atom("3"), Atom("E"), Atom("+"), Atom("0")), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("num is a variable and chars is a partial list or list with an element which is a variable", func(t *testing.T) {
		t.Run("partial list", func(t *testing.T) {
			codes := ListRest(Variable("Rest"),
				Atom("2"), Atom("3"), Atom("."), Atom("4"),
			)

			ok, err := NumberChars(NewVariable(), codes, Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(codes), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			code := Variable("Code")

			ok, err := NumberChars(NewVariable(), List(code, Atom("3"), Atom("."), Atom("4")), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(code), err)
			assert.False(t, ok)
		})
	})

	t.Run("num is neither a variable nor a number", func(t *testing.T) {
		ok, err := NumberChars(Atom("23.4"), List(Atom("2"), Atom("3"), Atom("."), Atom("4")), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorNumber(Atom("23.4")), err)
		assert.False(t, ok)
	})

	t.Run("num is a variable and chars is neither a list nor partial list", func(t *testing.T) {
		ok, err := NumberChars(NewVariable(), Atom("23.4"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorList(Atom("23.4")), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the list chars is neither a variable nor a one-character atom", func(t *testing.T) {
		ok, err := NumberChars(NewVariable(), List(Integer(2), Atom("3"), Atom("."), Atom("4")), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorCharacter(Integer(2)), err)
		assert.False(t, ok)
	})

	t.Run("chars is a list of one-char atoms but is not parsable as a number", func(t *testing.T) {
		ok, err := NumberChars(NewVariable(), List(Atom("f"), Atom("o"), Atom("o")), Success, nil).Force(context.Background())
		assert.Equal(t, syntaxErrorNotANumber(), err)
		assert.False(t, ok)
	})
}

func TestNumberCodes(t *testing.T) {
	t.Run("number to codes", func(t *testing.T) {
		codes := Variable("Codes")

		ok, err := NumberCodes(Float(23.4), codes, func(env *Env) *Promise {
			assert.Equal(t, List(Integer(50), Integer(51), Integer(46), Integer(52)), env.Resolve(codes))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("codes to number", func(t *testing.T) {
		num := Variable("Num")

		ok, err := NumberCodes(num, List(Integer(50), Integer(51), Integer(46), Integer(52)), func(env *Env) *Promise {
			assert.Equal(t, Float(23.4), env.Resolve(num))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("both provided", func(t *testing.T) {
		t.Run("33.0", func(t *testing.T) {
			ok, err := NumberCodes(Float(33.0), List(Integer(51), Integer(51), Integer(46), Integer(48)), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("33", func(t *testing.T) {
			ok, err := NumberCodes(Float(33.0), List(Integer(51), Integer(51)), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("3.3E+01", func(t *testing.T) {
			ok, err := NumberCodes(Float(33.0), List(Integer(51), Integer(46), Integer(51), Integer(69), Integer(43), Integer(48), Integer(49)), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("num is a variable and codes is a partial list or list with an element which is a variable", func(t *testing.T) {
		t.Run("partial list", func(t *testing.T) {
			codes := ListRest(Variable("Rest"),
				Integer(50), Integer(51), Integer(46), Integer(52),
			)

			ok, err := NumberCodes(NewVariable(), codes, Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(codes), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			code := Variable("Code")

			ok, err := NumberCodes(NewVariable(), List(code, Integer(50), Integer(51), Integer(46), Integer(52)), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(code), err)
			assert.False(t, ok)
		})
	})

	t.Run("num is neither a variable nor a number", func(t *testing.T) {
		ok, err := NumberCodes(Atom("23.4"), List(Integer(50), Integer(51), Integer(46), Integer(52)), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorNumber(Atom("23.4")), err)
		assert.False(t, ok)
	})

	t.Run("num is a variable and codes is neither a list nor partial list", func(t *testing.T) {
		ok, err := NumberCodes(NewVariable(), Atom("23.4"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorList(Atom("23.4")), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the list codes is neither a variable nor a one-character atom", func(t *testing.T) {
		ok, err := NumberCodes(NewVariable(), List(Atom("2"), Integer(51), Integer(46), Integer(52)), Success, nil).Force(context.Background())
		assert.Equal(t, representationError(Atom("character_code"), Atom("'2' is not a valid character code.")), err)
		assert.False(t, ok)
	})

	t.Run("codes is a list of one-char atoms but is not parsable as a number", func(t *testing.T) {
		ok, err := NumberCodes(NewVariable(), List(Integer(102), Integer(111), Integer(111)), Success, nil).Force(context.Background())
		assert.Equal(t, syntaxErrorNotANumber(), err)
		assert.False(t, ok)
	})
}

func TestFunctionSet_Is(t *testing.T) {
	t.Run("addition", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(3), &Compound{Functor: "+", Args: []Term{Integer(1), Integer(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(3), &Compound{Functor: "+", Args: []Term{Integer(1), Float(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(3), &Compound{Functor: "+", Args: []Term{Float(1), Integer(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(3), &Compound{Functor: "+", Args: []Term{Float(1), Float(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("subtraction", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(1), &Compound{Functor: "-", Args: []Term{Integer(3), Integer(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1), &Compound{Functor: "-", Args: []Term{Integer(3), Float(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1), &Compound{Functor: "-", Args: []Term{Float(3), Integer(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1), &Compound{Functor: "-", Args: []Term{Float(3), Float(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("multiplication", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(6), &Compound{Functor: "*", Args: []Term{Integer(3), Integer(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(6), &Compound{Functor: "*", Args: []Term{Integer(3), Float(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(6), &Compound{Functor: "*", Args: []Term{Float(3), Integer(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(6), &Compound{Functor: "*", Args: []Term{Float(3), Float(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("floating-point division", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(2), &Compound{Functor: "/", Args: []Term{Integer(4), Integer(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(2), &Compound{Functor: "/", Args: []Term{Integer(4), Float(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(2), &Compound{Functor: "/", Args: []Term{Float(4), Integer(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(2), &Compound{Functor: "/", Args: []Term{Float(4), Float(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("integer division", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(2), &Compound{Functor: "//", Args: []Term{Integer(4), Integer(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: "//", Args: []Term{Integer(4), Float(2)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(2)), err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: "//", Args: []Term{Float(4), Integer(2)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(4)), err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: "//", Args: []Term{Integer(4), Integer(0)}}, Success, nil).Force(context.Background())
		assert.Equal(t, evaluationErrorZeroDivisor(), err)
		assert.False(t, ok)
	})

	t.Run("remainder", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(-1), &Compound{Functor: "rem", Args: []Term{Integer(-21), Integer(4)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: "rem", Args: []Term{Integer(-21), Float(4)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(4)), err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: "rem", Args: []Term{Float(-21), Integer(4)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(-21)), err)
		assert.False(t, ok)
	})

	t.Run("mod", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(3), &Compound{Functor: "mod", Args: []Term{Integer(-21), Integer(4)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: "mod", Args: []Term{Integer(-21), Float(4)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(4)), err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: "mod", Args: []Term{Float(-21), Integer(4)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(-21)), err)
		assert.False(t, ok)
	})

	t.Run("exponential", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(16), &Compound{Functor: "**", Args: []Term{Integer(4), Integer(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(16), &Compound{Functor: "**", Args: []Term{Integer(4), Float(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(16), &Compound{Functor: "**", Args: []Term{Float(4), Integer(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(16), &Compound{Functor: "**", Args: []Term{Float(4), Float(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("sign reversal", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(-2), &Compound{Functor: "-", Args: []Term{Integer(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(-2), &Compound{Functor: "-", Args: []Term{Float(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("absolute value", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(2), &Compound{Functor: "abs", Args: []Term{Integer(-2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(2), &Compound{Functor: "abs", Args: []Term{Float(-2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("arctangent", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(0), &Compound{Functor: "atan", Args: []Term{Integer(0)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(0), &Compound{Functor: "atan", Args: []Term{Float(0)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("ceiling", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(1), &Compound{Functor: "ceiling", Args: []Term{Integer(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1), &Compound{Functor: "ceiling", Args: []Term{Float(0.9)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("cosine", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(1.0), &Compound{Functor: "cos", Args: []Term{Integer(0)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1.0), &Compound{Functor: "cos", Args: []Term{Float(0)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("natural antilogarithm", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(1.0), &Compound{Functor: "exp", Args: []Term{Integer(0)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1.0), &Compound{Functor: "exp", Args: []Term{Float(0)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("square root", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(1.0), &Compound{Functor: "sqrt", Args: []Term{Integer(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1.0), &Compound{Functor: "sqrt", Args: []Term{Float(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("sign", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(1), &Compound{Functor: "sign", Args: []Term{Integer(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Integer(1), &Compound{Functor: "sign", Args: []Term{Integer(math.MaxInt64)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Integer(0), &Compound{Functor: "sign", Args: []Term{Integer(0)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Integer(-1), &Compound{Functor: "sign", Args: []Term{Integer(-1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Integer(-1), &Compound{Functor: "sign", Args: []Term{Integer(math.MinInt64)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1), &Compound{Functor: "sign", Args: []Term{Float(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1), &Compound{Functor: "sign", Args: []Term{Float(math.MaxFloat64)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(0), &Compound{Functor: "sign", Args: []Term{Float(0)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(-1), &Compound{Functor: "sign", Args: []Term{Float(-1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(-1), &Compound{Functor: "sign", Args: []Term{Float(-math.MaxFloat64)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		v := Variable("N")
		ok, err = DefaultFunctionSet.Is(v, &Compound{Functor: "sign", Args: []Term{Float(math.NaN())}}, func(env *Env) *Promise {
			assert.True(t, math.IsNaN(float64(env.Resolve(v).(Float))))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("float", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(1.0), &Compound{Functor: "float", Args: []Term{Integer(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1.0), &Compound{Functor: "float", Args: []Term{Float(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("floor", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(1), &Compound{Functor: "floor", Args: []Term{Integer(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1), &Compound{Functor: "floor", Args: []Term{Float(1.1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("natural logarithm", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(0), &Compound{Functor: "log", Args: []Term{Integer(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(0), &Compound{Functor: "log", Args: []Term{Float(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("sine", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(0), &Compound{Functor: "sin", Args: []Term{Integer(0)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(0), &Compound{Functor: "sin", Args: []Term{Float(0)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("truncate", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(1), &Compound{Functor: "truncate", Args: []Term{Integer(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1), &Compound{Functor: "truncate", Args: []Term{Float(1.1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("round", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(1), &Compound{Functor: "round", Args: []Term{Integer(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1), &Compound{Functor: "round", Args: []Term{Float(1.1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("bit-shift right", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(2), &Compound{Functor: ">>", Args: []Term{Integer(4), Integer(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: ">>", Args: []Term{Float(4), Integer(1)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(4)), err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: ">>", Args: []Term{Integer(4), Float(1)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(1)), err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: ">>", Args: []Term{Float(4), Float(1)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(4)), err)
		assert.False(t, ok)
	})

	t.Run("bit-shift left", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(8), &Compound{Functor: "<<", Args: []Term{Integer(4), Integer(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: "<<", Args: []Term{Float(4), Integer(1)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(4)), err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: "<<", Args: []Term{Integer(4), Float(1)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(1)), err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: "<<", Args: []Term{Float(4), Float(1)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(4)), err)
		assert.False(t, ok)
	})

	t.Run("bitwise and", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(1), &Compound{Functor: "/\\", Args: []Term{Integer(5), Integer(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: "/\\", Args: []Term{Float(5), Integer(1)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(5)), err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: "/\\", Args: []Term{Integer(5), Float(1)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(1)), err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: "/\\", Args: []Term{Float(5), Float(1)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(5)), err)
		assert.False(t, ok)
	})

	t.Run("bitwise or", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(5), &Compound{Functor: "\\/", Args: []Term{Integer(4), Integer(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: "\\/", Args: []Term{Float(4), Integer(1)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(4)), err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: "\\/", Args: []Term{Integer(4), Float(1)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(1)), err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: "\\/", Args: []Term{Float(4), Float(1)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(4)), err)
		assert.False(t, ok)
	})

	t.Run("bitwise complement", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(-1), &Compound{Functor: "\\", Args: []Term{Integer(0)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: "\\", Args: []Term{Float(0)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(0)), err)
		assert.False(t, ok)
	})

	t.Run("expression is a variable", func(t *testing.T) {
		expression := Variable("Exp")

		ok, err := DefaultFunctionSet.Is(Integer(0), expression, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(expression), err)
		assert.False(t, ok)
	})

	t.Run("unknown constant", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(0), Atom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorEvaluable(&Compound{
			Functor: "/",
			Args:    []Term{Atom("foo"), Integer(0)},
		}), err)
		assert.False(t, ok)
	})
}

func TestFunctionSet_Equal(t *testing.T) {
	t.Run("same", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Equal(Integer(1), Integer(1), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Equal(Float(1), Integer(1), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Equal(Integer(1), Float(1), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Equal(Float(1), Float(1), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("different", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Equal(Integer(1), Integer(2), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Equal(Float(1), Integer(2), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Equal(Integer(1), Float(2), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Equal(Float(1), Float(2), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("lhs is a variable", func(t *testing.T) {
		lhs := Variable("LHS")

		ok, err := DefaultFunctionSet.Equal(lhs, Integer(1), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(lhs), err)
		assert.False(t, ok)
	})

	t.Run("rhs is a variable", func(t *testing.T) {
		rhs := Variable("RHS")

		ok, err := DefaultFunctionSet.Equal(Integer(1), rhs, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(rhs), err)
		assert.False(t, ok)
	})
}

func TestFunctionSet_NotEqual(t *testing.T) {
	t.Run("same", func(t *testing.T) {
		ok, err := DefaultFunctionSet.NotEqual(Integer(1), Integer(1), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.NotEqual(Float(1), Integer(1), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.NotEqual(Integer(1), Float(1), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.NotEqual(Float(1), Float(1), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("different", func(t *testing.T) {
		ok, err := DefaultFunctionSet.NotEqual(Integer(1), Integer(2), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.NotEqual(Float(1), Integer(2), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.NotEqual(Integer(1), Float(2), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.NotEqual(Float(1), Float(2), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("lhs is a variable", func(t *testing.T) {
		lhs := Variable("LHS")

		ok, err := DefaultFunctionSet.NotEqual(lhs, Integer(1), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(lhs), err)
		assert.False(t, ok)
	})

	t.Run("rhs is a variable", func(t *testing.T) {
		rhs := Variable("RHS")

		ok, err := DefaultFunctionSet.NotEqual(Integer(1), rhs, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(rhs), err)
		assert.False(t, ok)
	})
}

func TestFunctionSet_LessThan(t *testing.T) {
	ok, err := DefaultFunctionSet.LessThan(Integer(1), Integer(2), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.LessThan(Float(1), Integer(2), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.LessThan(Integer(1), Float(2), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.LessThan(Float(1), Float(2), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)
}

func TestFunctionSet_GreaterThan(t *testing.T) {
	ok, err := DefaultFunctionSet.GreaterThan(Integer(2), Integer(1), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.GreaterThan(Float(2), Integer(1), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.GreaterThan(Integer(2), Float(1), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.GreaterThan(Float(2), Float(1), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)
}

func TestFunctionSet_LessThanOrEqual(t *testing.T) {
	ok, err := DefaultFunctionSet.LessThanOrEqual(Integer(1), Integer(2), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.LessThanOrEqual(Float(1), Integer(2), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.LessThanOrEqual(Integer(1), Float(2), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.LessThanOrEqual(Float(1), Float(2), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)
}

func TestFunctionSet_GreaterThanOrEqual(t *testing.T) {
	ok, err := DefaultFunctionSet.GreaterThanOrEqual(Integer(2), Integer(1), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.GreaterThanOrEqual(Float(2), Integer(1), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.GreaterThanOrEqual(Integer(2), Float(1), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.GreaterThanOrEqual(Float(2), Float(1), Success, nil).Force(context.Background())
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
		expected := []Term{
			&Compound{Functor: "mode", Args: []Term{Atom("read")}},
			&Compound{Functor: "alias", Args: []Term{Atom("null")}},
			&Compound{Functor: "eof_action", Args: []Term{Atom("eof_code")}},
			Atom("input"),
			&Compound{Functor: "buffer", Args: []Term{Atom("true")}},
			&Compound{Functor: "file_name", Args: []Term{Atom(f.Name())}},
			&Compound{Functor: "position", Args: []Term{Integer(0)}},
			&Compound{Functor: "end_of_stream", Args: []Term{Atom("at")}},
			&Compound{Functor: "reposition", Args: []Term{Atom("false")}},
			&Compound{Functor: "type", Args: []Term{Atom("text")}},
		}

		v := Variable("V")
		c := 0
		var vm VM
		ok, err := vm.StreamProperty(&Stream{
			Source: bufio.NewReader(f),
			Closer: f,
			Mode:   StreamModeRead,
			Alias:  "null",
		}, v, func(env *Env) *Promise {
			assert.Equal(t, expected[c], env.Resolve(v))
			c++
			return Bool(false)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("stream alias", func(t *testing.T) {
		expected := []Term{
			&Compound{Functor: "mode", Args: []Term{Atom("write")}},
			&Compound{Functor: "alias", Args: []Term{Atom("null")}},
			&Compound{Functor: "eof_action", Args: []Term{Atom("eof_code")}},
			Atom("output"),
			&Compound{Functor: "buffer", Args: []Term{Atom("true")}},
			&Compound{Functor: "file_name", Args: []Term{Atom(f.Name())}},
			&Compound{Functor: "position", Args: []Term{Integer(0)}},
			&Compound{Functor: "end_of_stream", Args: []Term{Atom("at")}},
			&Compound{Functor: "reposition", Args: []Term{Atom("false")}},
			&Compound{Functor: "type", Args: []Term{Atom("text")}},
		}

		vm := VM{
			streams: map[Term]*Stream{
				Atom("null"): {
					Sink:   bufio.NewWriter(f),
					Closer: f,
					Mode:   StreamModeWrite,
					Alias:  "null",
				},
			},
		}
		v := Variable("V")
		c := 0
		ok, err := vm.StreamProperty(Atom("null"), v, func(env *Env) *Promise {
			assert.Equal(t, expected[c], env.Resolve(v))
			c++
			return Bool(false)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("correct property value", func(t *testing.T) {
		var vm VM
		ok, err := vm.StreamProperty(&Stream{Mode: StreamModeRead}, &Compound{
			Functor: "mode",
			Args:    []Term{Atom("read")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("streamOrAlias is neither a variable, a stream-term, nor an alias", func(t *testing.T) {
		var vm VM
		ok, err := vm.StreamProperty(Integer(0), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorStreamOrAlias(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("property is neither a variable nor a stream property", func(t *testing.T) {
		var vm VM
		ok, err := vm.StreamProperty(NewVariable(), Atom("property"), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorStreamProperty(Atom("property")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var vm VM
		ok, err := vm.StreamProperty(Atom("foo"), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, existenceErrorStream(Atom("foo")), err)
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
		s := Stream{
			Source: f,
			Closer: f,
			Mode:   StreamModeRead,
		}

		var vm VM
		ok, err := vm.SetStreamPosition(&s, Integer(0), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		streamOrAlias := Variable("Stream")

		var vm VM
		ok, err := vm.SetStreamPosition(streamOrAlias, Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("position is a variable", func(t *testing.T) {
		s := Stream{
			Source: f,
			Closer: f,
			Mode:   StreamModeRead,
		}
		position := Variable("Pos")

		var vm VM
		ok, err := vm.SetStreamPosition(&s, position, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(position), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		var vm VM
		ok, err := vm.SetStreamPosition(Integer(2), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorStreamOrAlias(Integer(2)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var vm VM
		ok, err := vm.SetStreamPosition(Atom("foo"), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, existenceErrorStream(Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias has stream property reposition(false)", func(t *testing.T) {
		s := Variable("Stream")
		env := NewEnv().
			Bind(s, &Stream{
				Source: bytes.NewReader(nil),
				Mode:   StreamModeRead,
			})

		var vm VM
		ok, err := vm.SetStreamPosition(s, Integer(0), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError("reposition", "stream", s, "Stream is not a file."), err)
		assert.False(t, ok)
	})
}

func TestVM_CharConversion(t *testing.T) {
	t.Run("register", func(t *testing.T) {
		var vm VM
		ok, err := vm.CharConversion(Atom("a"), Atom("b"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, 'b', vm.charConversions['a'])
	})

	t.Run("remove", func(t *testing.T) {
		vm := VM{
			charConversions: map[rune]rune{
				'a': 'b',
			},
		}
		ok, err := vm.CharConversion(Atom("a"), Atom("a"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		_, ok = vm.charConversions['a']
		assert.False(t, ok)
	})

	t.Run("inChar is a variable", func(t *testing.T) {
		inChar := Variable("In")

		var vm VM
		ok, err := vm.CharConversion(inChar, Atom("a"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(inChar), err)
		assert.False(t, ok)
	})

	t.Run("outChar is a variable", func(t *testing.T) {
		outChar := Variable("Out")

		var vm VM
		ok, err := vm.CharConversion(Atom("a"), outChar, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(outChar), err)
		assert.False(t, ok)
	})

	t.Run("inChar is neither a variable nor a one character atom", func(t *testing.T) {
		t.Run("not even an atom", func(t *testing.T) {
			var vm VM
			ok, err := vm.CharConversion(Integer(0), Atom("a"), Success, nil).Force(context.Background())
			assert.Equal(t, representationError(Atom("character"), Atom("0 is not a character.")), err)
			assert.False(t, ok)
		})

		t.Run("multi-character atom", func(t *testing.T) {
			var vm VM
			ok, err := vm.CharConversion(Atom("foo"), Atom("a"), Success, nil).Force(context.Background())
			assert.Equal(t, representationError(Atom("character"), Atom("foo is not a character.")), err)
			assert.False(t, ok)
		})
	})

	t.Run("outChar is neither a variable nor a one character atom", func(t *testing.T) {
		t.Run("not even an atom", func(t *testing.T) {
			var vm VM
			ok, err := vm.CharConversion(Atom("a"), Integer(0), Success, nil).Force(context.Background())
			assert.Equal(t, representationError(Atom("character"), Atom("0 is not a character.")), err)
			assert.False(t, ok)
		})

		t.Run("multi-character atom", func(t *testing.T) {
			var vm VM
			ok, err := vm.CharConversion(Atom("a"), Atom("foo"), Success, nil).Force(context.Background())
			assert.Equal(t, representationError(Atom("character"), Atom("foo is not a character.")), err)
			assert.False(t, ok)
		})
	})
}

func TestVM_CurrentCharConversion(t *testing.T) {
	t.Run("specified", func(t *testing.T) {
		t.Run("as is", func(t *testing.T) {
			var vm VM
			ok, err := vm.CurrentCharConversion(Atom("a"), Atom("a"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("converted", func(t *testing.T) {
			vm := VM{
				charConversions: map[rune]rune{
					'a': 'b',
				},
			}
			ok, err := vm.CurrentCharConversion(Atom("a"), Atom("b"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("not specified", func(t *testing.T) {
		x, y := Variable("X"), Variable("Y")

		var r rune
		var vm VM
		ok, err := vm.CurrentCharConversion(x, y, func(env *Env) *Promise {
			ref, ok := env.Lookup(x)
			assert.True(t, ok)
			x, ok := ref.(Atom)
			assert.True(t, ok)
			assert.Len(t, []rune(x), 1)

			ref, ok = env.Lookup(y)
			assert.True(t, ok)
			y, ok := ref.(Atom)
			assert.True(t, ok)
			assert.Len(t, []rune(y), 1)

			assert.Equal(t, r, []rune(x)[0])
			assert.Equal(t, r, []rune(y)[0])
			r++
			return Bool(false)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
		assert.Equal(t, rune(256), r)
	})

	t.Run("inChar is neither a variable nor a one character atom", func(t *testing.T) {
		t.Run("not even an atom", func(t *testing.T) {
			var vm VM
			ok, err := vm.CurrentCharConversion(Integer(0), Atom("b"), Success, nil).Force(context.Background())
			assert.Equal(t, representationError(Atom("character"), Atom("0 is not a character.")), err)
			assert.False(t, ok)
		})

		t.Run("multi-character atom", func(t *testing.T) {
			var vm VM
			ok, err := vm.CurrentCharConversion(Atom("foo"), Atom("b"), Success, nil).Force(context.Background())
			assert.Equal(t, representationError(Atom("character"), Atom("foo is not a character.")), err)
			assert.False(t, ok)
		})
	})

	t.Run("outChar is neither a variable nor a one character atom", func(t *testing.T) {
		t.Run("not even an atom", func(t *testing.T) {
			var vm VM
			ok, err := vm.CurrentCharConversion(Atom("a"), Integer(0), Success, nil).Force(context.Background())
			assert.Equal(t, representationError(Atom("character"), Atom("0 is not a character.")), err)
			assert.False(t, ok)
		})

		t.Run("multi-character atom", func(t *testing.T) {
			var vm VM
			ok, err := vm.CurrentCharConversion(Atom("a"), Atom("bar"), Success, nil).Force(context.Background())
			assert.Equal(t, representationError(Atom("character"), Atom("bar is not a character.")), err)
			assert.False(t, ok)
		})
	})
}

func TestVM_SetPrologFlag(t *testing.T) {
	t.Run("bounded", func(t *testing.T) {
		var vm VM
		ok, err := vm.SetPrologFlag(Atom("bounded"), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError("modify", "flag", Atom("bounded"), "bounded is not modifiable."), err)
		assert.False(t, ok)
	})

	t.Run("max_integer", func(t *testing.T) {
		var vm VM
		ok, err := vm.SetPrologFlag(Atom("max_integer"), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError("modify", "flag", Atom("max_integer"), "max_integer is not modifiable."), err)
		assert.False(t, ok)
	})

	t.Run("min_integer", func(t *testing.T) {
		var vm VM
		ok, err := vm.SetPrologFlag(Atom("min_integer"), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError("modify", "flag", Atom("min_integer"), "min_integer is not modifiable."), err)
		assert.False(t, ok)
	})

	t.Run("integer_rounding_function", func(t *testing.T) {
		var vm VM
		ok, err := vm.SetPrologFlag(Atom("integer_rounding_function"), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError("modify", "flag", Atom("integer_rounding_function"), "integer_rounding_function is not modifiable."), err)
		assert.False(t, ok)
	})

	t.Run("char_conversion", func(t *testing.T) {
		t.Run("on", func(t *testing.T) {
			var vm VM
			ok, err := vm.SetPrologFlag(Atom("char_conversion"), Atom("on"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.True(t, vm.charConvEnabled)
		})

		t.Run("off", func(t *testing.T) {
			vm := VM{charConvEnabled: true}
			ok, err := vm.SetPrologFlag(Atom("char_conversion"), Atom("off"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.False(t, vm.charConvEnabled)
		})
	})

	t.Run("debug", func(t *testing.T) {
		t.Run("on", func(t *testing.T) {
			var vm VM
			ok, err := vm.SetPrologFlag(Atom("debug"), Atom("on"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.True(t, vm.debug)
		})

		t.Run("off", func(t *testing.T) {
			vm := VM{debug: true}
			ok, err := vm.SetPrologFlag(Atom("debug"), Atom("off"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.False(t, vm.debug)
		})
	})

	t.Run("max_arity", func(t *testing.T) {
		var vm VM
		ok, err := vm.SetPrologFlag(Atom("max_arity"), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError("modify", "flag", Atom("max_arity"), "max_arity is not modifiable."), err)
		assert.False(t, ok)
	})

	t.Run("unknown", func(t *testing.T) {
		t.Run("error", func(t *testing.T) {
			vm := VM{unknown: unknownFail}
			ok, err := vm.SetPrologFlag(Atom("unknown"), Atom("error"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.Equal(t, unknownError, vm.unknown)
		})

		t.Run("warning", func(t *testing.T) {
			var vm VM
			ok, err := vm.SetPrologFlag(Atom("unknown"), Atom("warning"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.Equal(t, unknownWarning, vm.unknown)
		})

		t.Run("fail", func(t *testing.T) {
			var vm VM
			ok, err := vm.SetPrologFlag(Atom("unknown"), Atom("fail"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.Equal(t, unknownFail, vm.unknown)
		})
	})

	t.Run("flag is a variable", func(t *testing.T) {
		flag := Variable("Flag")

		var vm VM
		ok, err := vm.SetPrologFlag(flag, Atom("fail"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(flag), err)
		assert.False(t, ok)
	})

	t.Run("value is a variable", func(t *testing.T) {
		value := Variable("Value")

		var vm VM
		ok, err := vm.SetPrologFlag(Atom("unknown"), value, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(value), err)
		assert.False(t, ok)
	})

	t.Run("flag is neither a variable nor an atom", func(t *testing.T) {
		var vm VM
		ok, err := vm.SetPrologFlag(Integer(0), Atom("fail"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorAtom(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("flag is an atom but an invalid flag for the processor", func(t *testing.T) {
		var vm VM
		ok, err := vm.SetPrologFlag(Atom("foo"), Atom("fail"), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorPrologFlag(Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("value is inadmissible for flag", func(t *testing.T) {
		var vm VM
		ok, err := vm.SetPrologFlag(Atom("unknown"), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorFlagValue(&Compound{
			Functor: "+",
			Args:    []Term{Atom("unknown"), Integer(0)},
		}), err)
		assert.False(t, ok)
	})

	t.Run("value is admissible for flag but the flag is not modifiable", func(t *testing.T) {
		var vm VM
		ok, err := vm.SetPrologFlag(Atom("bounded"), Atom("true"), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError("modify", "flag", Atom("bounded"), "bounded is not modifiable."), err)
		assert.False(t, ok)
	})
}

func TestVM_CurrentPrologFlag(t *testing.T) {
	var vm VM

	t.Run("specified", func(t *testing.T) {
		ok, err := vm.CurrentPrologFlag(Atom("bounded"), Atom("true"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = vm.CurrentPrologFlag(Atom("max_integer"), Integer(math.MaxInt64), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = vm.CurrentPrologFlag(Atom("min_integer"), Integer(math.MinInt64), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = vm.CurrentPrologFlag(Atom("integer_rounding_function"), Atom("toward_zero"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = vm.CurrentPrologFlag(Atom("char_conversion"), Atom("off"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = vm.CurrentPrologFlag(Atom("debug"), Atom("off"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = vm.CurrentPrologFlag(Atom("max_arity"), Atom("unbounded"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = vm.CurrentPrologFlag(Atom("unknown"), Atom("error"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not specified", func(t *testing.T) {
		flag, value := Variable("Flag"), Variable("Value")
		var c int
		ok, err := vm.CurrentPrologFlag(flag, value, func(env *Env) *Promise {
			switch c {
			case 0:
				assert.Equal(t, Atom("bounded"), env.Resolve(flag))
				assert.Equal(t, Atom("true"), env.Resolve(value))
			case 1:
				assert.Equal(t, Atom("max_integer"), env.Resolve(flag))
				assert.Equal(t, Integer(math.MaxInt64), env.Resolve(value))
			case 2:
				assert.Equal(t, Atom("min_integer"), env.Resolve(flag))
				assert.Equal(t, Integer(math.MinInt64), env.Resolve(value))
			case 3:
				assert.Equal(t, Atom("integer_rounding_function"), env.Resolve(flag))
				assert.Equal(t, Atom("toward_zero"), env.Resolve(value))
			case 4:
				assert.Equal(t, Atom("char_conversion"), env.Resolve(flag))
				assert.Equal(t, Atom("off"), env.Resolve(value))
			case 5:
				assert.Equal(t, Atom("debug"), env.Resolve(flag))
				assert.Equal(t, Atom("off"), env.Resolve(value))
			case 6:
				assert.Equal(t, Atom("max_arity"), env.Resolve(flag))
				assert.Equal(t, Atom("unbounded"), env.Resolve(value))
			case 7:
				assert.Equal(t, Atom("unknown"), env.Resolve(flag))
				assert.Equal(t, Atom(vm.unknown.String()), env.Resolve(value))
			case 8:
				assert.Equal(t, Atom("double_quotes"), env.Resolve(flag))
				assert.Equal(t, Atom(vm.doubleQuotes.String()), env.Resolve(value))
			default:
				assert.Fail(t, "unreachable")
			}
			c++
			return Bool(false)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
		assert.Equal(t, 9, c)
	})

	t.Run("flag is neither a variable nor an atom", func(t *testing.T) {
		var vm VM
		ok, err := vm.CurrentPrologFlag(Integer(0), Atom("error"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorAtom(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("flag is an atom but an invalid flag for the processor", func(t *testing.T) {
		var vm VM
		ok, err := vm.CurrentPrologFlag(Atom("foo"), Atom("error"), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorPrologFlag(Atom("foo")), err)
		assert.False(t, ok)
	})
}

func TestVM_ExpandTerm(t *testing.T) {
	t.Run("term_expansion/2 is undefined", func(t *testing.T) {
		var vm VM
		ok, err := vm.ExpandTerm(&Compound{
			Functor: "f",
			Args:    []Term{Atom("a")},
		}, &Compound{
			Functor: "f",
			Args:    []Term{Atom("a")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("term_expansion/2 is defined", func(t *testing.T) {
		t.Run("not applicable", func(t *testing.T) {
			vm := VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "term_expansion", Arity: 2}: predicate2(func(Term, Term, func(*Env) *Promise, *Env) *Promise {
						return Bool(false)
					}),
				},
			}
			ok, err := vm.ExpandTerm(&Compound{
				Functor: "f",
				Args:    []Term{Atom("a")},
			}, &Compound{
				Functor: "f",
				Args:    []Term{Atom("a")},
			}, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("applicable", func(t *testing.T) {
			vm := VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "term_expansion", Arity: 2}: predicate2(func(t1, t2 Term, k func(*Env) *Promise, env *Env) *Promise {
						return Unify(t2, &Compound{
							Functor: "g",
							Args:    []Term{Atom("b")},
						}, k, env)
					}),
				},
			}
			ok, err := vm.ExpandTerm(&Compound{
				Functor: "f",
				Args:    []Term{Atom("a")},
			}, &Compound{
				Functor: "g",
				Args:    []Term{Atom("b")},
			}, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})
}
