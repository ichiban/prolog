package prolog

import (
	"errors"
	"fmt"
	"testing"

	"github.com/ichiban/prolog/engine"

	"github.com/stretchr/testify/assert"
)

func TestSolutions_Close(t *testing.T) {
	ch := make(chan bool)
	sols := Solutions{more: ch}
	assert.NoError(t, sols.Close())
	assert.Error(t, sols.Close())
}

func TestSolutions_Next(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		v := engine.NewVariable()
		env, _ := engine.NewEnv().Unify(v, engine.NewAtom("foo"), false)
		more := make(chan bool, 1)
		defer close(more)
		next := make(chan *engine.Env, 1)
		defer close(next)
		next <- env
		sols := Solutions{more: more, next: next}
		assert.True(t, sols.Next())
		assert.Equal(t, engine.NewAtom("foo"), sols.env.Resolve(v))
	})

	t.Run("closed", func(t *testing.T) {
		sols := Solutions{closed: true}
		assert.False(t, sols.Next())
	})
}

func TestSolutions_Scan(t *testing.T) {
	var (
		varFloat32 = engine.NewVariable()
		varFloat64 = engine.NewVariable()
		varInt     = engine.NewVariable()
		varInt8    = engine.NewVariable()
		varInt16   = engine.NewVariable()
		varInt32   = engine.NewVariable()
		varInt64   = engine.NewVariable()
		varString  = engine.NewVariable()
		varSlice   = engine.NewVariable()
		varFoo     = engine.NewVariable()
		varBar     = engine.NewVariable()
		varBaz     = engine.NewVariable()
	)

	env := engine.NewEnv()
	for k, v := range map[engine.Variable]engine.Term{
		varFloat32: engine.Float(32),
		varFloat64: engine.Float(64),
		varInt:     engine.Integer(1),
		varInt8:    engine.Integer(8),
		varInt16:   engine.Integer(16),
		varInt32:   engine.Integer(32),
		varInt64:   engine.Integer(64),
		varString:  engine.NewAtom("string"),
		varSlice:   engine.List(engine.NewAtom("a"), engine.NewAtom("b"), engine.NewAtom("c")),
		varFoo:     engine.NewAtom("foo"),
		varBar:     engine.NewAtom("bar"),
		varBaz:     engine.NewAtom("baz"),
	} {
		env, _ = env.Unify(k, v, false)
	}

	sols := Solutions{
		env: env,
		vars: []engine.ParsedVariable{
			{Name: engine.NewAtom("Float32"), Variable: varFloat32},
			{Name: engine.NewAtom("Float64"), Variable: varFloat64},
			{Name: engine.NewAtom("Int"), Variable: varInt},
			{Name: engine.NewAtom("Int8"), Variable: varInt8},
			{Name: engine.NewAtom("Int16"), Variable: varInt16},
			{Name: engine.NewAtom("Int32"), Variable: varInt32},
			{Name: engine.NewAtom("Int64"), Variable: varInt64},
			{Name: engine.NewAtom("String"), Variable: varString},
			{Name: engine.NewAtom("Slice"), Variable: varSlice},
			{Name: engine.NewAtom("Foo"), Variable: varFoo},
			{Name: engine.NewAtom("Bar"), Variable: varBar},
			{Name: engine.NewAtom("Baz"), Variable: varBaz},
		},
	}

	t.Run("struct", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			var s struct {
				Float32 float32
				Float64 float64
				Int     int
				Int8    int8
				Int16   int16
				Int32   int32
				Int64   int64
				String  string
				Slice   []string
				Tagged  string `prolog:"Foo"`
				Bar     engine.Term
			}
			assert.NoError(t, sols.Scan(&s))
			assert.Equal(t, float32(32), s.Float32)
			assert.Equal(t, float64(64), s.Float64)
			assert.Equal(t, 1, s.Int)
			assert.Equal(t, int8(8), s.Int8)
			assert.Equal(t, int16(16), s.Int16)
			assert.Equal(t, int32(32), s.Int32)
			assert.Equal(t, int64(64), s.Int64)
			assert.Equal(t, "string", s.String)
			assert.Equal(t, []string{"a", "b", "c"}, s.Slice)
			assert.Equal(t, "foo", s.Tagged)
			assert.Equal(t, engine.NewAtom("bar"), s.Bar)
		})

		t.Run("ng", func(t *testing.T) {
			t.Run("string", func(t *testing.T) {
				var s struct {
					Int string
				}
				assert.Error(t, sols.Scan(&s))
			})

			t.Run("float", func(t *testing.T) {
				var s struct {
					String float64
				}
				assert.Error(t, sols.Scan(&s))
			})

			t.Run("slice", func(t *testing.T) {
				var s struct {
					Slice []int
				}
				assert.Error(t, sols.Scan(&s))
			})

			t.Run("unsupported", func(t *testing.T) {
				var s struct {
					Int complex64
				}
				assert.Error(t, sols.Scan(&s))
			})
		})
	})

	t.Run("map", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			m := map[string]engine.Term{}
			assert.NoError(t, sols.Scan(m))
			assert.Equal(t, engine.Float(32), m["Float32"])
			assert.Equal(t, engine.Float(64), m["Float64"])
			assert.Equal(t, engine.Integer(1), m["Int"])
			assert.Equal(t, engine.Integer(8), m["Int8"])
			assert.Equal(t, engine.Integer(16), m["Int16"])
			assert.Equal(t, engine.Integer(32), m["Int32"])
			assert.Equal(t, engine.Integer(64), m["Int64"])
			assert.Equal(t, engine.NewAtom("string"), m["String"])
			assert.Equal(t, engine.List(engine.NewAtom("a"), engine.NewAtom("b"), engine.NewAtom("c")), m["Slice"])
			assert.Equal(t, engine.NewAtom("foo"), m["Foo"])
			assert.Equal(t, engine.NewAtom("bar"), m["Bar"])
		})

		t.Run("ng", func(t *testing.T) {
			t.Run("key", func(t *testing.T) {
				m := map[int]engine.Term{}
				assert.Error(t, sols.Scan(m))
			})

			t.Run("value", func(t *testing.T) {
				m := map[string]int{}
				assert.Error(t, sols.Scan(m))
			})
		})
	})

	t.Run("other", func(t *testing.T) {
		assert.Error(t, sols.Scan(1))
	})
}

func TestSolutions_Err(t *testing.T) {
	err := errors.New("ng")
	sols := Solutions{err: err}
	assert.Equal(t, err, sols.Err())
}

func TestSolutions_Vars(t *testing.T) {
	sols := Solutions{
		vars: []engine.ParsedVariable{
			{Name: engine.NewAtom("A")},
			{Name: engine.NewAtom("B")},
			{Name: engine.NewAtom("C")},
		},
	}

	assert.Equal(t, []string{"A", "B", "C"}, sols.Vars())
}

func ExampleSolutions_Scan() {
	p := New(nil, nil)
	sols, _ := p.Query(`A = foo, I = 42, F = 3.14.`)
	for sols.Next() {
		var s struct {
			A string
			I int
			F float64
		}
		_ = sols.Scan(&s)
		fmt.Printf("A = %s\n", s.A)
		fmt.Printf("I = %d\n", s.I)
		fmt.Printf("F = %.2f\n", s.F)
	}

	// Output:
	// A = foo
	// I = 42
	// F = 3.14
}

func ExampleSolutions_Scan_tag() {
	p := New(nil, nil)
	sols, _ := p.Query(`A = foo, I = 42, F = 3.14.`)
	for sols.Next() {
		var s struct {
			Atom    string  `prolog:"A"`
			Integer int     `prolog:"I"`
			Float   float64 `prolog:"F"`
		}
		_ = sols.Scan(&s)
		fmt.Printf("Atom = %s\n", s.Atom)
		fmt.Printf("Integer = %d\n", s.Integer)
		fmt.Printf("Float = %.2f\n", s.Float)
	}

	// Output:
	// Atom = foo
	// Integer = 42
	// Float = 3.14
}

func ExampleSolutions_Scan_list() {
	p := New(nil, nil)
	sols, _ := p.Query(`Atoms = [foo, bar], Integers = [1, 2], Floats = [1.1, 2.1], Mixed = [foo, 1, 1.1].`)
	for sols.Next() {
		var s struct {
			Atoms    []string
			Integers []int64
			Floats   []float64
			Mixed    []interface{}
		}
		_ = sols.Scan(&s)

		fmt.Printf("Atoms = %s\n", s.Atoms)
		fmt.Printf("Integers = %d\n", s.Integers)
		fmt.Printf("Floats = %.1f\n", s.Floats)
		fmt.Printf("Mixed = %v\n", s.Mixed)
	}

	// Output:
	// Atoms = [foo bar]
	// Integers = [1 2]
	// Floats = [1.1 2.1]
	// Mixed = [foo 1 1.1]
}
