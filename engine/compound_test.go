package engine

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestList(t *testing.T) {
	tests := []struct {
		title string
		elems []Term
		list  Term
	}{
		{title: "empty", elems: nil, list: Atom("[]")},
		{title: "non-empty", elems: []Term{Atom("a"), Atom("b"), Atom("c")}, list: list{Atom("a"), Atom("b"), Atom("c")}},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			assert.Equal(t, tt.list, List(tt.elems...))
		})
	}
}

func TestListRest(t *testing.T) {
	tests := []struct {
		title string
		rest  Term
		elems []Term
		list  Term
	}{
		{title: "empty", rest: Variable("X"), elems: nil, list: Variable("X")},
		{title: "non-empty", rest: Variable("X"), elems: []Term{Atom("a"), Atom("b")}, list: partial{Compound: list{Atom("a"), Atom("b")}, tail: Variable("X")}},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			assert.Equal(t, tt.list, ListRest(tt.rest, tt.elems...))
		})
	}
}

func TestEnv_Set(t *testing.T) {
	env := NewEnv()
	assert.Equal(t, List(), env.Set())
	assert.Equal(t, List(Atom("a")), env.Set(Atom("a")))
	assert.Equal(t, List(Atom("a")), env.Set(Atom("a"), Atom("a"), Atom("a")))
	assert.Equal(t, List(Atom("a"), Atom("b"), Atom("c")), env.Set(Atom("c"), Atom("b"), Atom("a")))
}

func TestSeq(t *testing.T) {
	assert.Equal(t, Atom("a"), Seq(",", Atom("a")))
	assert.Equal(t, &compound{
		functor: ",",
		args: []Term{
			Atom("a"),
			Atom("b"),
		},
	}, Seq(",", Atom("a"), Atom("b")))
	assert.Equal(t, &compound{
		functor: ",",
		args: []Term{
			Atom("a"),
			&compound{
				functor: ",",
				args: []Term{
					Atom("b"),
					Atom("c"),
				},
			},
		},
	}, Seq(",", Atom("a"), Atom("b"), Atom("c")))
}

func TestCharList(t *testing.T) {
	assert.Equal(t, Atom("[]"), CharList(""))
	assert.Equal(t, charList("abc"), CharList("abc"))
}

func TestCodeList(t *testing.T) {
	assert.Equal(t, Atom("[]"), CodeList(""))
	assert.Equal(t, codeList("abc"), CodeList("abc"))
}
