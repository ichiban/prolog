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
		env := engine.NewEnv().Bind("Foo", engine.Atom("bar"))
		more := make(chan bool, 1)
		defer close(more)
		next := make(chan *engine.Env, 1)
		defer close(next)
		next <- env
		sols := Solutions{more: more, next: next}
		assert.True(t, sols.Next())
		assert.Equal(t, engine.Atom("bar"), sols.env.Resolve(engine.Variable("Foo")))
	})

	t.Run("closed", func(t *testing.T) {
		sols := Solutions{closed: true}
		assert.False(t, sols.Next())
	})
}

func TestSolutions_Scan(t *testing.T) {
	env := engine.NewEnv()
	for k, v := range map[engine.Variable]engine.Term{
		"Float32": engine.Float(32),
		"Float64": engine.Float(64),
		"Int":     engine.Integer(1),
		"Int8":    engine.Integer(8),
		"Int16":   engine.Integer(16),
		"Int32":   engine.Integer(32),
		"Int64":   engine.Integer(64),
		"String":  engine.Atom("string"),
		"Slice":   engine.List(engine.Atom("a"), engine.Atom("b"), engine.Atom("c")),
		"Foo":     engine.Atom("foo"),
		"Bar":     engine.Atom("bar"),
		"Baz":     engine.Atom("baz"),
	} {
		env = env.Bind(k, v)
	}

	sols := Solutions{
		env: env,
		vars: []engine.Variable{
			"Float32", "Float64",
			"Int", "Int8", "Int16", "Int32", "Int64",
			"String",
			"Slice",
			"Foo", "Bar", "Baz",
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
			assert.Equal(t, engine.Atom("bar"), s.Bar)
		})

		t.Run("ng", func(t *testing.T) {
			t.Run("string", func(t *testing.T) {
				var s struct {
					Int string
				}
				assert.Error(t, sols.Scan(&s))
			})

			t.Run("slice", func(t *testing.T) {
				var s struct {
					Slice []int
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
			assert.Equal(t, engine.Atom("string"), m["String"])
			assert.Equal(t, engine.List(engine.Atom("a"), engine.Atom("b"), engine.Atom("c")), m["Slice"])
			assert.Equal(t, engine.Atom("foo"), m["Foo"])
			assert.Equal(t, engine.Atom("bar"), m["Bar"])
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
		vars: []engine.Variable{"A", "B", "_1", "C"},
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
