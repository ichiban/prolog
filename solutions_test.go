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
		env, _ := engine.NewEnv().Unify(v, engine.NewAtom("foo"))
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
	sols := func(m map[string]engine.Term) Solutions {
		env := engine.NewEnv()
		var vars []engine.ParsedVariable
		for n, t := range m {
			v := engine.NewVariable()
			env, _ = env.Unify(v, t)
			vars = append(vars, engine.ParsedVariable{Name: engine.NewAtom(n), Variable: v})
		}
		return Solutions{
			env:  env,
			vars: vars,
		}
	}

	tests := []struct {
		title  string
		sols   Solutions
		dest   interface{}
		err    error
		result interface{}
	}{
		{title: "struct: empty", sols: Solutions{}, dest: &struct{}{}, result: &struct{}{}},

		{title: "struct: interface, variable", sols: sols(map[string]engine.Term{
			"X": engine.NewVariable(),
		}), dest: &struct{ X interface{} }{}, result: &struct{ X interface{} }{
			X: nil,
		}},
		{title: "struct: interface, atom", sols: sols(map[string]engine.Term{
			"X": engine.NewAtom("foo"),
		}), dest: &struct{ X interface{} }{}, result: &struct{ X interface{} }{
			X: "foo",
		}},
		{title: "struct: interface, empty list", sols: sols(map[string]engine.Term{
			"X": engine.NewAtom("[]"),
		}), dest: &struct{ X interface{} }{}, result: &struct{ X interface{} }{
			X: []interface{}{},
		}},
		{title: "struct: interface, integer", sols: sols(map[string]engine.Term{
			"X": engine.Integer(1),
		}), dest: &struct{ X interface{} }{}, result: &struct{ X interface{} }{
			X: 1,
		}},
		{title: "struct: interface, float", sols: sols(map[string]engine.Term{
			"X": engine.Float(1),
		}), dest: &struct{ X interface{} }{}, result: &struct{ X interface{} }{
			X: 1.0,
		}},
		{title: "struct: interface, list", sols: sols(map[string]engine.Term{
			"X": engine.List(engine.Integer(1), engine.Integer(2), engine.Integer(3)),
		}), dest: &struct{ X interface{} }{}, result: &struct{ X interface{} }{
			X: []interface{}{1, 2, 3},
		}},
		{title: "struct: interface, list with unknown", sols: sols(map[string]engine.Term{
			"X": engine.List(engine.Integer(1), nil, engine.Integer(3)),
		}), dest: &struct{ X interface{} }{}, err: errConversion},
		{title: "struct: interface, not list", sols: sols(map[string]engine.Term{
			"X": engine.PartialList(engine.NewVariable(), engine.Integer(1), engine.Integer(2), engine.Integer(3)),
		}), dest: &struct{ X interface{} }{}, err: errConversion},
		{title: "struct: interface, unknown", sols: sols(map[string]engine.Term{
			"X": nil,
		}), dest: &struct{ X interface{} }{}, err: errConversion},

		{title: "struct: string, atom", sols: sols(map[string]engine.Term{
			"X": engine.NewAtom("foo"),
		}), dest: &struct{ X string }{}, result: &struct{ X string }{X: "foo"}},
		{title: "struct: string, character list", sols: sols(map[string]engine.Term{
			"X": engine.CharList("foo"),
		}), dest: &struct{ X string }{}, result: &struct{ X string }{X: "foo"}},
		{title: "struct: string, code list", sols: sols(map[string]engine.Term{
			"X": engine.CodeList("foo"),
		}), dest: &struct{ X string }{}, result: &struct{ X string }{X: "foo"}},
		{title: "struct: string, non-atom", sols: sols(map[string]engine.Term{
			"X": engine.Integer(1),
		}), dest: &struct{ X string }{}, err: errConversion},

		{title: "struct: int, integer", sols: sols(map[string]engine.Term{
			"X": engine.Integer(1),
		}), dest: &struct{ X int }{}, result: &struct{ X int }{X: 1}},
		{title: "struct: int, non-integer", sols: sols(map[string]engine.Term{
			"X": engine.NewAtom("foo"),
		}), dest: &struct{ X int }{}, err: errConversion},

		{title: "struct: int8, integer", sols: sols(map[string]engine.Term{
			"X": engine.Integer(1),
		}), dest: &struct{ X int8 }{}, result: &struct{ X int8 }{X: 1}},
		{title: "struct: int8, non-integer", sols: sols(map[string]engine.Term{
			"X": engine.NewAtom("foo"),
		}), dest: &struct{ X int8 }{}, err: errConversion},

		{title: "struct: int16, integer", sols: sols(map[string]engine.Term{
			"X": engine.Integer(1),
		}), dest: &struct{ X int16 }{}, result: &struct{ X int16 }{X: 1}},
		{title: "struct: int16, non-integer", sols: sols(map[string]engine.Term{
			"X": engine.NewAtom("foo"),
		}), dest: &struct{ X int16 }{}, err: errConversion},

		{title: "struct: int32, integer", sols: sols(map[string]engine.Term{
			"X": engine.Integer(1),
		}), dest: &struct{ X int32 }{}, result: &struct{ X int32 }{X: 1}},
		{title: "struct: int32, non-integer", sols: sols(map[string]engine.Term{
			"X": engine.NewAtom("foo"),
		}), dest: &struct{ X int32 }{}, err: errConversion},

		{title: "struct: int64, integer", sols: sols(map[string]engine.Term{
			"X": engine.Integer(1),
		}), dest: &struct{ X int64 }{}, result: &struct{ X int64 }{X: 1}},
		{title: "struct: int64, non-integer", sols: sols(map[string]engine.Term{
			"X": engine.NewAtom("foo"),
		}), dest: &struct{ X int64 }{}, err: errConversion},

		{title: "struct: float32, float", sols: sols(map[string]engine.Term{
			"X": engine.Float(1),
		}), dest: &struct{ X float32 }{}, result: &struct{ X float32 }{X: 1}},
		{title: "struct: float32, non-float", sols: sols(map[string]engine.Term{
			"X": engine.NewAtom("foo"),
		}), dest: &struct{ X float32 }{}, err: errConversion},

		{title: "struct: float64, float", sols: sols(map[string]engine.Term{
			"X": engine.Float(1),
		}), dest: &struct{ X float64 }{}, result: &struct{ X float64 }{X: 1}},
		{title: "struct: float64, non-float", sols: sols(map[string]engine.Term{
			"X": engine.NewAtom("foo"),
		}), dest: &struct{ X float64 }{}, err: errConversion},

		{title: "struct: slice, list", sols: sols(map[string]engine.Term{
			"X": engine.List(engine.Integer(1), engine.Integer(2), engine.Integer(3)),
		}), dest: &struct{ X []int }{}, result: &struct{ X []int }{X: []int{1, 2, 3}}},
		{title: "struct: slice, list with unknown", sols: sols(map[string]engine.Term{
			"X": engine.List(engine.Integer(1), nil, engine.Integer(3)),
		}), dest: &struct{ X []int }{}, err: errConversion},
		{title: "struct: slice, non-list", sols: sols(map[string]engine.Term{
			"X": engine.PartialList(engine.NewVariable(), engine.Integer(1), engine.Integer(2), engine.Integer(3)),
		}), dest: &struct{ X []int }{}, err: errConversion},

		{title: "struct: unsupported field type", sols: sols(map[string]engine.Term{
			"X": engine.Integer(1),
		}), dest: &struct{ X bool }{}, err: errConversion},

		{title: "struct: alias", sols: sols(map[string]engine.Term{
			"Y": engine.Integer(1),
		}), dest: &struct {
			X int `prolog:"Y"`
		}{}, result: &struct {
			X int `prolog:"Y"`
		}{X: 1}},

		{title: "struct: ignored variable", sols: sols(map[string]engine.Term{
			"X": engine.Integer(1),
			"Y": engine.Integer(2), // Y is not a field of the struct. Ignored.
		}), dest: &struct {
			X int
		}{}, result: &struct {
			X int
		}{X: 1}},

		{title: "map: empty", sols: Solutions{}, dest: map[string]interface{}{}, result: map[string]interface{}{}},
		{title: "map: interface, integer", sols: sols(map[string]engine.Term{
			"X": engine.Integer(1),
		}), dest: map[string]interface{}{}, result: map[string]interface{}{
			"X": 1,
		}},
		{title: "map: non-string key", sols: Solutions{}, dest: map[int]interface{}{}, err: errors.New("map key is not string")},
		{title: "map: interface, unknown", sols: sols(map[string]engine.Term{
			"X": nil,
		}), dest: map[string]interface{}{}, err: errConversion},

		{title: "invalid", sols: Solutions{}, dest: nil, err: errors.New("invalid kind: invalid")},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			assert.Equal(t, tt.err, tt.sols.Scan(tt.dest))
			if tt.err == nil {
				assert.Equal(t, tt.result, tt.dest)
			}
		})
	}
}

func TestSolutions_Err(t *testing.T) {
	err := errors.New("ng")
	sols := Solutions{err: err}
	assert.Equal(t, err, sols.Err())
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
