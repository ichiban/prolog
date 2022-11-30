package engine

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestCompound_GoString(t *testing.T) {
	tests := []struct {
		term   Term
		output string
	}{
		{term: NewAtom("f").Apply(NewAtom("a")), output: `&engine.compound{functor:"f", args:[]engine.Term{"a"}}`},
		{term: List(NewAtom("a"), NewAtom("b"), NewAtom("c")), output: `engine.list{"a", "b", "c"}`},
		{term: PartialList(NewAtom("c"), NewAtom("a"), NewAtom("b")), output: `engine.partial{Compound:engine.list{"a", "b"}, tail:"c"}`},
	}

	for _, tt := range tests {
		t.Run(tt.output, func(t *testing.T) {
			assert.Equal(t, tt.output, tt.term.(fmt.GoStringer).GoString())
		})
	}
}

func TestList(t *testing.T) {
	tests := []struct {
		title string
		elems []Term
		list  Term
	}{
		{title: "empty", elems: nil, list: atomEmptyList},
		{title: "non-empty", elems: []Term{NewAtom("a"), NewAtom("b"), NewAtom("c")}, list: list{NewAtom("a"), NewAtom("b"), NewAtom("c")}},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			assert.Equal(t, tt.list, List(tt.elems...))
		})
	}
}

func TestPartialList(t *testing.T) {
	x := Term(NewVariable())

	tests := []struct {
		title string
		rest  Term
		elems []Term
		list  Term
	}{
		{title: "empty", rest: x, elems: nil, list: x},
		{title: "non-empty", rest: x, elems: []Term{NewAtom("a"), NewAtom("b")}, list: &partial{Compound: list{NewAtom("a"), NewAtom("b")}, tail: &x}},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			assert.Equal(t, tt.list, PartialList(tt.rest, tt.elems...))
		})
	}
}

func TestEnv_Set(t *testing.T) {
	env := NewEnv()
	assert.Equal(t, List(), env.set())
	assert.Equal(t, List(NewAtom("a")), env.set(NewAtom("a")))
	assert.Equal(t, List(NewAtom("a")), env.set(NewAtom("a"), NewAtom("a"), NewAtom("a")))
	assert.Equal(t, List(NewAtom("a"), NewAtom("b"), NewAtom("c")), env.set(NewAtom("c"), NewAtom("b"), NewAtom("a")))
}

func TestSeq(t *testing.T) {
	assert.Equal(t, NewAtom("a"), seq(atomComma, NewAtom("a")))
	assert.Equal(t, &compound{
		functor: atomComma,
		args: []Term{
			NewAtom("a"),
			NewAtom("b"),
		},
	}, seq(atomComma, NewAtom("a"), NewAtom("b")))
	assert.Equal(t, &compound{
		functor: atomComma,
		args: []Term{
			NewAtom("a"),
			&compound{
				functor: atomComma,
				args: []Term{
					NewAtom("b"),
					NewAtom("c"),
				},
			},
		},
	}, seq(atomComma, NewAtom("a"), NewAtom("b"), NewAtom("c")))
}

func TestCharList(t *testing.T) {
	assert.Equal(t, atomEmptyList, CharList(""))
	assert.Equal(t, charList("abc"), CharList("abc"))
}

func TestCodeList(t *testing.T) {
	assert.Equal(t, atomEmptyList, CodeList(""))
	assert.Equal(t, codeList("abc"), CodeList("abc"))
}
