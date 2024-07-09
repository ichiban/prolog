package internal

import (
	"math/rand"
	"testing"
)

func TestEnv_Lookup(t *testing.T) {
	tests := []struct {
		title string
		env   func() *Env
		v     Variable
		id    TermID
		ok    bool
	}{
		{title: "empty", env: func() *Env {
			var env Env
			return &env
		}, v: 1, ok: false},
		{title: "ok (odd)", env: func() *Env {
			env := Env{bindings: make([]binding, 0, 10)}
			if err := env.Bind(1, 1); err != nil {
				t.Fatalf("bind failed: %v", err)
			}
			if err := env.Bind(2, 2); err != nil {
				t.Fatalf("bind failed: %v", err)
			}
			if err := env.Bind(3, 3); err != nil {
				t.Fatalf("bind failed: %v", err)
			}
			return &env
		}, v: 1, id: 1, ok: true},
		{title: "ok (even)", env: func() *Env {
			env := Env{bindings: make([]binding, 0, 10)}
			if err := env.Bind(1, 1); err != nil {
				t.Fatalf("bind failed: %v", err)
			}
			if err := env.Bind(2, 2); err != nil {
				t.Fatalf("bind failed: %v", err)
			}
			if err := env.Bind(3, 3); err != nil {
				t.Fatalf("bind failed: %v", err)
			}
			return &env
		}, v: 2, id: 2, ok: true},
	}
	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			id, ok := tt.env().Lookup(tt.v)
			if tt.ok != ok {
				t.Errorf("unexpected lookup result: expected=%t, got=%t", tt.ok, ok)
				return
			}
			if tt.id != id {
				t.Errorf("unexpected lookup value: expected=%d, got=%d", tt.id, id)
			}
		})
	}
}

func BenchmarkEnv_Lookup(b *testing.B) {
	vars := make([]Variable, 1000)
	for i := range vars {
		vars[i] = Variable(i)
	}

	rand.Shuffle(len(vars), func(i, j int) {
		vars[i], vars[j] = vars[j], vars[i]
	})

	env := Env{bindings: make([]binding, 0, 20*1024)}
	for _, v := range vars {
		if err := env.Bind(v, TermID(v)); err != nil {
			b.Fatalf("failed to bind: %v", err)
		}
	}

	rand.Shuffle(len(vars), func(i, j int) {
		vars[i], vars[j] = vars[j], vars[i]
	})

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		v := vars[i%len(vars)]
		id, ok := env.Lookup(v)
		if !ok {
			b.Errorf("failed to lookup: v=%s", v)
		}
		if TermID(v) != id {
			b.Errorf("expected=%d, got=%d", v, id)
		}
	}
}
