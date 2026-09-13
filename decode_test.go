package prolog

import (
	"errors"
	"maps"
	"slices"
	"testing"
)

// decode runs query and returns its first solution decoded into T.
func decode[T any](t *testing.T, query string) (T, error) {
	t.Helper()

	i := New()
	for r, err := range i.Query[T](t.Context(), query) {
		return r, err
	}

	var zero T
	t.Fatalf("%s: no solution", query)
	return zero, nil
}

func TestInterpreter_Query_mapOfAny(t *testing.T) {
	tests := []struct {
		title string
		query string
		want  map[string]any
	}{
		{title: "atom", query: `X = foo.`, want: map[string]any{"X": Atom("foo")}},
		{title: "integer", query: `X = 42.`, want: map[string]any{"X": int64(42)}},
		{title: "negative integer", query: `X = -1.`, want: map[string]any{"X": int64(-1)}},
		{title: "float", query: `X = 1.5.`, want: map[string]any{"X": 1.5}},
		// "abc" and [a,b,c] are the same term, so both read as a string.
		{title: "double quoted string", query: `X = "abc".`, want: map[string]any{"X": "abc"}},
		{title: "list of one-character atoms", query: `X = [a,b,c].`, want: map[string]any{"X": "abc"}},
		{title: "empty list", query: `X = [].`, want: map[string]any{"X": ""}},
		{title: "list of integers", query: `X = [1,2,3].`, want: map[string]any{"X": []any{int64(1), int64(2), int64(3)}}},
		{title: "list of atoms", query: `X = [foo,bar].`, want: map[string]any{"X": []any{Atom("foo"), Atom("bar")}}},
		{title: "nested list", query: `X = [[1],[2]].`, want: map[string]any{"X": []any{[]any{int64(1)}, []any{int64(2)}}}},
		{title: "compound", query: `X = f(y).`, want: map[string]any{"X": Raw("f(y)")}},
		{title: "several variables", query: `X = foo, Y = 1.`, want: map[string]any{"X": Atom("foo"), "Y": int64(1)}},
		// Unbound variables have no binding, so they're left out.
		{title: "unbound variable", query: `X = 1, var(Y).`, want: map[string]any{"X": int64(1)}},
		{title: "anonymous variable", query: `_ = 1, X = 2.`, want: map[string]any{"X": int64(2)}},
		{title: "no variables", query: `true.`, want: map[string]any{}},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			got, err := decode[map[string]any](t, test.query)
			if err != nil {
				t.Fatal(err)
			}

			if want := test.want; !maps.EqualFunc(got, want, equalAny) {
				t.Errorf("got: %v, want: %v", got, want)
			}
		})
	}
}

func TestInterpreter_Query_mapOfTyped(t *testing.T) {
	t.Run("Atom", func(t *testing.T) {
		got, err := decode[map[string]Atom](t, `X = foo, Y = bar.`)
		if err != nil {
			t.Fatal(err)
		}
		if want := (map[string]Atom{"X": "foo", "Y": "bar"}); !maps.Equal(got, want) {
			t.Errorf("got: %v, want: %v", got, want)
		}
	})

	t.Run("int64", func(t *testing.T) {
		got, err := decode[map[string]int64](t, `X = 1, Y = 2.`)
		if err != nil {
			t.Fatal(err)
		}
		if want := (map[string]int64{"X": 1, "Y": 2}); !maps.Equal(got, want) {
			t.Errorf("got: %v, want: %v", got, want)
		}
	})

	t.Run("float64", func(t *testing.T) {
		got, err := decode[map[string]float64](t, `X = 1.5.`)
		if err != nil {
			t.Fatal(err)
		}
		if want := (map[string]float64{"X": 1.5}); !maps.Equal(got, want) {
			t.Errorf("got: %v, want: %v", got, want)
		}
	})

	t.Run("string", func(t *testing.T) {
		got, err := decode[map[string]string](t, `X = "hi".`)
		if err != nil {
			t.Fatal(err)
		}
		if want := (map[string]string{"X": "hi"}); !maps.Equal(got, want) {
			t.Errorf("got: %v, want: %v", got, want)
		}
	})

	t.Run("Raw", func(t *testing.T) {
		got, err := decode[map[string]Raw](t, `X = f(y), Y = foo.`)
		if err != nil {
			t.Fatal(err)
		}
		if want := (map[string]Raw{"X": "f(y)", "Y": "foo"}); !maps.Equal(got, want) {
			t.Errorf("got: %v, want: %v", got, want)
		}
	})

	t.Run("slice", func(t *testing.T) {
		got, err := decode[map[string][]int64](t, `X = [1,2,3].`)
		if err != nil {
			t.Fatal(err)
		}
		if want := []int64{1, 2, 3}; !slices.Equal(got["X"], want) {
			t.Errorf("got: %v, want: %v", got["X"], want)
		}
	})

	t.Run("named string type", func(t *testing.T) {
		type name string
		got, err := decode[map[string]name](t, `X = "hi".`)
		if err != nil {
			t.Fatal(err)
		}
		if want := (map[string]name{"X": "hi"}); !maps.Equal(got, want) {
			t.Errorf("got: %v, want: %v", got, want)
		}
	})
}

// map[string]Raw gives every binding as its text, whatever the term is.
func TestInterpreter_Query_raw(t *testing.T) {
	got, err := decode[map[string]Raw](t, `X = f(y).`)
	if err != nil {
		t.Fatal(err)
	}

	if want := map[string]Raw{"X": "f(y)"}; !maps.Equal(got, want) {
		t.Errorf("got: %v, want: %v", got, want)
	}
}

func TestInterpreter_Query_struct(t *testing.T) {
	t.Run("by field name", func(t *testing.T) {
		type sol struct {
			X Atom
			Y int64
		}

		got, err := decode[sol](t, `X = foo, Y = 42.`)
		if err != nil {
			t.Fatal(err)
		}
		if want := (sol{X: "foo", Y: 42}); got != want {
			t.Errorf("got: %v, want: %v", got, want)
		}
	})

	t.Run("by tag", func(t *testing.T) {
		type sol struct {
			Who  Atom `prolog:"Who"`
			Age  int64
			Skip Atom `prolog:"-"`
		}

		got, err := decode[sol](t, `Who = socrates, Age = 70, Skip = ignored.`)
		if err != nil {
			t.Fatal(err)
		}
		if want := (sol{Who: "socrates", Age: 70}); got != want {
			t.Errorf("got: %v, want: %v", got, want)
		}
	})

	t.Run("unmatched field keeps its zero value", func(t *testing.T) {
		type sol struct {
			X Atom
			Z Atom
		}

		got, err := decode[sol](t, `X = foo.`)
		if err != nil {
			t.Fatal(err)
		}
		if want := (sol{X: "foo"}); got != want {
			t.Errorf("got: %v, want: %v", got, want)
		}
	})

	t.Run("unbound variable keeps its zero value", func(t *testing.T) {
		type sol struct {
			X Atom
			Y Atom
		}

		got, err := decode[sol](t, `X = foo, var(Y).`)
		if err != nil {
			t.Fatal(err)
		}
		if want := (sol{X: "foo"}); got != want {
			t.Errorf("got: %v, want: %v", got, want)
		}
	})

	t.Run("unexported field is ignored", func(t *testing.T) {
		type sol struct {
			X Atom
			y Atom //nolint:unused
		}

		got, err := decode[sol](t, `X = foo.`)
		if err != nil {
			t.Fatal(err)
		}
		if want := (sol{X: "foo"}); got != want {
			t.Errorf("got: %v, want: %v", got, want)
		}
	})
}

func TestInterpreter_Query_conversionError(t *testing.T) {
	tests := []struct {
		title  string
		decode func(*testing.T) error
	}{
		{title: "atom into int64", decode: func(t *testing.T) error {
			_, err := decode[map[string]int64](t, `X = foo.`)
			return err
		}},
		{title: "integer into Atom", decode: func(t *testing.T) error {
			_, err := decode[map[string]Atom](t, `X = 1.`)
			return err
		}},
		{title: "integer into float64", decode: func(t *testing.T) error {
			_, err := decode[map[string]float64](t, `X = 1.`)
			return err
		}},
		{title: "compound into string", decode: func(t *testing.T) error {
			_, err := decode[map[string]string](t, `X = f(y).`)
			return err
		}},
		{title: "atom into slice", decode: func(t *testing.T) error {
			_, err := decode[map[string][]int64](t, `X = foo.`)
			return err
		}},
		{title: "list element of the wrong type", decode: func(t *testing.T) error {
			_, err := decode[map[string][]int64](t, `X = [1,foo].`)
			return err
		}},
		{title: "struct field of the wrong type", decode: func(t *testing.T) error {
			type sol struct{ X int64 }
			_, err := decode[sol](t, `X = foo.`)
			return err
		}},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			if err := test.decode(t); !errors.Is(err, errConversion) {
				t.Errorf("got: %v, want: %v", err, errConversion)
			}
		})
	}
}

func TestInterpreter_Query_overflow(t *testing.T) {
	if _, err := decode[map[string]int8](t, `X = 1000.`); err == nil {
		t.Errorf("got no error, want an overflow error")
	}
}

func TestInterpreter_Query_unsupportedResultType(t *testing.T) {
	tests := []struct {
		title  string
		decode func(*testing.T) error
	}{
		{title: "int", decode: func(t *testing.T) error {
			_, err := decode[int](t, `X = 1.`)
			return err
		}},
		{title: "slice", decode: func(t *testing.T) error {
			_, err := decode[[]Atom](t, `X = foo.`)
			return err
		}},
		{title: "map with a non-string key", decode: func(t *testing.T) error {
			_, err := decode[map[int]Atom](t, `X = foo.`)
			return err
		}},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			if err := test.decode(t); err == nil {
				t.Errorf("got no error, want an unsupported result type error")
			}
		})
	}
}

func equalAny(a, b any) bool {
	as, aok := a.([]any)
	bs, bok := b.([]any)
	if aok || bok {
		return aok && bok && slices.EqualFunc(as, bs, equalAny)
	}
	return a == b
}
