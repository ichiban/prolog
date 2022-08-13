package engine

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

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
