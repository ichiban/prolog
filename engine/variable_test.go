package engine

import (
	"bytes"
	"testing"

	"github.com/stretchr/testify/mock"

	"github.com/stretchr/testify/assert"
)

func TestVariable_Unify(t *testing.T) {
	v1, v2 := Variable("V1"), Variable("V2")
	env, ok := v1.Unify(v2, false, nil)
	assert.True(t, ok)
	env, ok = v1.Unify(Atom("foo"), false, env)
	assert.True(t, ok)
	assert.Equal(t, Atom("foo"), env.Resolve(v1))
	assert.Equal(t, Atom("foo"), env.Resolve(v2))

	v3, v4 := Variable("V3"), Variable("V4")
	env, ok = v3.Unify(v4, false, env)
	assert.True(t, ok)
	env, ok = v4.Unify(Atom("bar"), false, env)
	assert.True(t, ok)
	assert.Equal(t, Atom("bar"), env.Resolve(v3))
	assert.Equal(t, Atom("bar"), env.Resolve(v4))
}

func TestVariable_WriteTerm(t *testing.T) {
	var m mockTerm
	m.On("WriteTerm", mock.Anything, mock.Anything, mock.Anything).Return(nil).Once()
	defer m.AssertExpectations(t)

	mock := Variable("Mock")
	env := NewEnv().Bind(mock, &m)

	tests := []struct {
		title         string
		variableNames map[Variable]Atom
		variable      Variable
		output        string
	}{
		{title: "named", variable: `X`, output: `X`},
		{title: "unnamed", variable: `` /* NewVariable() */, output: `_1`},
		{title: "variable_names", variableNames: map[Variable]Atom{"X": "Foo"}, variable: `X`, output: `Foo`},
		{title: "not a variable", variable: mock},
	}

	var buf bytes.Buffer
	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			varCounter = 0
			if tt.variable == "" {
				tt.variable = NewVariable()
			}
			buf.Reset()
			assert.NoError(t, tt.variable.WriteTerm(&buf, &WriteOptions{VariableNames: tt.variableNames}, env))
			assert.Equal(t, tt.output, buf.String())
		})
	}
}

func TestVariable_Compare(t *testing.T) {
	t.Run("free", func(t *testing.T) {
		var m mockTerm
		defer m.AssertExpectations(t)

		assert.Equal(t, int64(-1), Variable("Y").Compare(&m, nil))
		assert.Equal(t, int64(-1), Variable("X").Compare(Variable("Y"), nil))
		assert.Equal(t, int64(0), Variable("X").Compare(Variable("X"), nil))
		assert.Equal(t, int64(1), Variable("Y").Compare(Variable("X"), nil))
	})

	t.Run("bound", func(t *testing.T) {
		var m mockTerm
		m.On("Compare", mock.Anything, mock.Anything).Return(int64(123))
		defer m.AssertExpectations(t)

		env := NewEnv().Bind("X", &m)
		assert.Equal(t, int64(123), Variable("X").Compare(Variable("Y"), env))
	})
}

func Test_variableSet(t *testing.T) {
	tests := []struct {
		term Term
		s    variableSet
	}{
		{term: &Compound{
			Functor: "f",
			Args:    []Term{Variable("X"), Variable("Y")},
		}, s: map[Variable]int{
			"X": 1,
			"Y": 1,
		}},
		{term: &Compound{
			Functor: "f",
			Args:    []Term{Variable("Y"), Variable("X")},
		}, s: map[Variable]int{
			"X": 1,
			"Y": 1,
		}},
		{term: &Compound{
			Functor: "+",
			Args:    []Term{Variable("X"), Variable("Y")},
		}, s: map[Variable]int{
			"X": 1,
			"Y": 1,
		}},
		{term: &Compound{
			Functor: "-",
			Args: []Term{
				Variable("Y"),
				&Compound{
					Functor: "-",
					Args:    []Term{Variable("X"), Variable("X")},
				},
			},
		}, s: map[Variable]int{
			"X": 2,
			"Y": 1,
		}},
	}

	for _, tt := range tests {
		assert.Equal(t, tt.s, newVariableSet(tt.term, nil))
	}
}

func Test_existentialVariableSet(t *testing.T) {
	tests := []struct {
		term Term
		ev   variableSet
	}{
		{term: &Compound{
			Functor: "^",
			Args: []Term{
				Variable("X"),
				&Compound{
					Functor: "^",
					Args: []Term{
						Variable("Y"),
						&Compound{
							Functor: "f",
							Args:    []Term{Variable("X"), Variable("Y"), Variable("Z")},
						},
					},
				},
			},
		}, ev: variableSet{
			"X": 1,
			"Y": 1,
		}},
		{term: &Compound{
			Functor: "^",
			Args: []Term{
				&Compound{
					Functor: ",",
					Args:    []Term{Variable("X"), Variable("Y")},
				},
				&Compound{
					Functor: "f",
					Args:    []Term{Variable("Z"), Variable("Y"), Variable("X")},
				},
			},
		}, ev: variableSet{
			"X": 1,
			"Y": 1,
		}},
		{term: &Compound{
			Functor: "^",
			Args: []Term{
				&Compound{
					Functor: "+",
					Args:    []Term{Variable("X"), Variable("Y")},
				},
				Integer(3),
			},
		}, ev: variableSet{
			"X": 1,
			"Y": 1,
		}},
	}

	for _, tt := range tests {
		assert.Equal(t, tt.ev, newExistentialVariablesSet(tt.term, nil))
	}
}

func Test_freeVariablesSet(t *testing.T) {
	tests := []struct {
		t, v Term
		fv   variableSet
	}{
		{t: &Compound{
			Functor: "+",
			Args: []Term{
				Variable("X"),
				&Compound{
					Functor: "+",
					Args:    []Term{Variable("Y"), Variable("Z")},
				},
			},
		}, v: &Compound{
			Functor: "f",
			Args:    []Term{Variable("Z")},
		}, fv: variableSet{
			"X": 1,
			"Y": 1,
		}},
		{t: &Compound{
			Functor: "^",
			Args: []Term{
				Variable("Z"),
				&Compound{
					Functor: "+",
					Args: []Term{
						Variable("A"),
						&Compound{
							Functor: "+",
							Args: []Term{
								Variable("X"),
								&Compound{
									Functor: "+",
									Args: []Term{
										Variable("Y"),
										Variable("Z"),
									},
								},
							},
						},
					},
				},
			},
		}, v: Variable("A"), fv: variableSet{
			"X": 1,
			"Y": 1,
		}},
	}

	for _, tt := range tests {
		assert.Equal(t, tt.fv, newFreeVariablesSet(tt.t, tt.v, nil))
	}
}
