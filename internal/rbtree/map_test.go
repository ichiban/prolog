package rbtree

import (
	"fmt"
	"reflect"
	"slices"
	"testing"
)

func ExampleMap_Set() {
	m := Map[string, int]{
		Nodes: make([]Node[string, int], 0, 14),
	}
	if err := m.Set("foo", 1); err != nil {
		panic(err)
	}
	if err := m.Set("bar", 2); err != nil {
		panic(err)
	}
	snapshot := m
	if err := m.Set("baz", 3); err != nil {
		panic(err)
	}
	if err := m.Set("foo", 4); err != nil {
		panic(err)
	}

	e, ok := m.Get("foo")
	fmt.Println("foo:", e, ok)
	e, ok = m.Get("bar")
	fmt.Println("bar:", e, ok)
	e, ok = m.Get("baz")
	fmt.Println("baz:", e, ok)

	fmt.Println("rollback to snapshot")
	m = snapshot

	e, ok = m.Get("foo")
	fmt.Println("foo:", e, ok)
	e, ok = m.Get("bar")
	fmt.Println("bar:", e, ok)
	e, ok = m.Get("baz")
	fmt.Println("baz:", e, ok)

	// Output:
	// foo: 4 true
	// bar: 2 true
	// baz: 3 true
	// rollback to snapshot
	// foo: 1 true
	// bar: 2 true
	// baz: 0 false
}

func TestMap_Set(t *testing.T) {
	tests := []struct {
		title  string
		before Map[string, int]
		key    string
		value  int
		err    error
		after  Map[string, int]
	}{
		{
			title: "initial",
			before: Map[string, int]{
				Nodes: make([]Node[string, int], 0, 2),
			},
			key:   "foo",
			value: 1,
			after: Map[string, int]{
				root: 1,
				Nodes: []Node[string, int]{
					{color: red, left: -1, right: -1, elem: elem[string, int]{key: "foo", value: 1}}, // TODO: Can we remove this node?
					{color: black, left: -1, right: -1, elem: elem[string, int]{key: "foo", value: 1}},
				},
			},
		},
		{
			title: "insert left",
			before: Map[string, int]{
				root: 1,
				Nodes: []Node[string, int]{
					{color: red, left: -1, right: -1, elem: elem[string, int]{key: "foo", value: 1}},
					{color: black, left: -1, right: -1, elem: elem[string, int]{key: "foo", value: 1}},
					{},
					{},
				}[:2],
			},
			key:   "bar",
			value: 2,
			after: Map[string, int]{
				root: 3,
				Nodes: []Node[string, int]{
					{color: red, left: -1, right: -1, elem: elem[string, int]{key: "foo", value: 1}},
					{color: black, left: -1, right: -1, elem: elem[string, int]{key: "foo", value: 1}},
					{color: red, left: -1, right: -1, elem: elem[string, int]{key: "bar", value: 2}},
					{color: black, left: 2, right: -1, elem: elem[string, int]{key: "foo", value: 1}},
				},
			},
		},
		{
			title: "failure on inserting left",
			before: Map[string, int]{
				root: 1,
				Nodes: []Node[string, int]{
					{color: red, left: -1, right: -1, elem: elem[string, int]{key: "foo", value: 1}},
					{color: black, left: -1, right: -1, elem: elem[string, int]{key: "foo", value: 1}},
				},
			},
			key:   "bar",
			value: 2,
			err:   ErrTooManyNodes,
			after: Map[string, int]{
				root: 1,
				Nodes: []Node[string, int]{
					{color: red, left: -1, right: -1, elem: elem[string, int]{key: "foo", value: 1}},
					{color: black, left: -1, right: -1, elem: elem[string, int]{key: "foo", value: 1}},
				},
			},
		},
		{
			title: "failure after inserting left",
			before: Map[string, int]{
				root: 1,
				Nodes: []Node[string, int]{
					{color: red, left: -1, right: -1, elem: elem[string, int]{key: "foo", value: 1}},
					{color: black, left: -1, right: -1, elem: elem[string, int]{key: "foo", value: 1}},
					{},
				}[:2],
			},
			key:   "bar",
			value: 2,
			err:   ErrTooManyNodes,
			after: Map[string, int]{
				root: 1,
				Nodes: []Node[string, int]{
					{color: red, left: -1, right: -1, elem: elem[string, int]{key: "foo", value: 1}},
					{color: black, left: -1, right: -1, elem: elem[string, int]{key: "foo", value: 1}},
				},
			},
		},
		{
			title: "insert right",
			before: Map[string, int]{
				root: 1,
				Nodes: []Node[string, int]{
					{color: red, left: -1, right: -1, elem: elem[string, int]{key: "bar", value: 2}},
					{color: black, left: -1, right: -1, elem: elem[string, int]{key: "bar", value: 2}},
					{},
					{},
				}[:2],
			},
			key:   "foo",
			value: 1,
			after: Map[string, int]{
				root: 3,
				Nodes: []Node[string, int]{
					{color: red, left: -1, right: -1, elem: elem[string, int]{key: "bar", value: 2}},
					{color: black, left: -1, right: -1, elem: elem[string, int]{key: "bar", value: 2}},
					{color: red, left: -1, right: -1, elem: elem[string, int]{key: "foo", value: 1}},
					{color: black, left: -1, right: 2, elem: elem[string, int]{key: "bar", value: 2}},
				},
			},
		},
		{
			title: "failure on inserting right",
			before: Map[string, int]{
				root: 1,
				Nodes: []Node[string, int]{
					{color: red, left: -1, right: -1, elem: elem[string, int]{key: "bar", value: 2}},
					{color: black, left: -1, right: -1, elem: elem[string, int]{key: "bar", value: 2}},
				},
			},
			key:   "foo",
			value: 1,
			err:   ErrTooManyNodes,
			after: Map[string, int]{
				root: 1,
				Nodes: []Node[string, int]{
					{color: red, left: -1, right: -1, elem: elem[string, int]{key: "bar", value: 2}},
					{color: black, left: -1, right: -1, elem: elem[string, int]{key: "bar", value: 2}},
				},
			},
		},
		{
			title: "failure after inserting right",
			before: Map[string, int]{
				root: 1,
				Nodes: []Node[string, int]{
					{color: red, left: -1, right: -1, elem: elem[string, int]{key: "bar", value: 2}},
					{color: black, left: -1, right: -1, elem: elem[string, int]{key: "bar", value: 2}},
					{},
				}[:2],
			},
			key:   "foo",
			value: 1,
			err:   ErrTooManyNodes,
			after: Map[string, int]{
				root: 1,
				Nodes: []Node[string, int]{
					{color: red, left: -1, right: -1, elem: elem[string, int]{key: "bar", value: 2}},
					{color: black, left: -1, right: -1, elem: elem[string, int]{key: "bar", value: 2}},
				},
			},
		},
		{
			title: "update",
			before: Map[string, int]{
				root: 1,
				Nodes: []Node[string, int]{
					{color: red, left: -1, right: -1, elem: elem[string, int]{key: "foo", value: 1}},
					{color: black, left: -1, right: -1, elem: elem[string, int]{key: "foo", value: 1}},
					{},
					{},
				}[:2],
			},
			key:   "foo",
			value: 2,
			after: Map[string, int]{
				root: 2,
				Nodes: []Node[string, int]{
					{color: red, left: -1, right: -1, elem: elem[string, int]{key: "foo", value: 1}},
					{color: black, left: -1, right: -1, elem: elem[string, int]{key: "foo", value: 1}},
					{color: black, left: -1, right: -1, elem: elem[string, int]{key: "foo", value: 2}},
				},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			m := tt.before
			err := m.Set(tt.key, tt.value)
			if !reflect.DeepEqual(tt.err, err) {
				t.Errorf("got %v, want %v", err, tt.err)
			}
			if !reflect.DeepEqual(tt.after, m) {
				t.Errorf("got %+v, want %+v", m, tt.after)
			}
		})
	}
}

func TestMap_Get(t *testing.T) {
	fbb := Map[string, int]{
		Nodes: make([]Node[string, int], 0, 11),
	}
	if err := fbb.Set("foo", 1); err != nil {
		t.Fatal(err)
	}
	if err := fbb.Set("bar", 2); err != nil {
		t.Fatal(err)
	}
	if err := fbb.Set("baz", 3); err != nil {
		t.Fatal(err)
	}

	tests := []struct {
		title string
		m     *Map[string, int]
		key   string
		value int
		ok    bool
	}{
		{title: "nil", m: nil, key: "foo", ok: false},
		{title: "empty", m: &Map[string, int]{}, key: "foo", ok: false},
		{title: "foo", m: &fbb, key: "foo", value: 1, ok: true},
		{title: "bar", m: &fbb, key: "bar", value: 2, ok: true},
		{title: "baz", m: &fbb, key: "baz", value: 3, ok: true},
		{title: "unknown", m: &fbb, key: "unknown", ok: false},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			val, ok := tt.m.Get(tt.key)
			if ok != tt.ok {
				t.Errorf("got %v, want %v", ok, tt.ok)
			}
			if val != tt.value {
				t.Errorf("got %v, want %v", val, tt.value)
			}
		})
	}
}

func TestMap_All(t *testing.T) {
	type result struct {
		key   string
		value int
	}

	fbb := Map[string, int]{
		Nodes: make([]Node[string, int], 0, 11),
	}
	if err := fbb.Set("foo", 1); err != nil {
		t.Fatal(err)
	}
	if err := fbb.Set("bar", 2); err != nil {
		t.Fatal(err)
	}
	if err := fbb.Set("baz", 3); err != nil {
		t.Fatal(err)
	}

	tests := []struct {
		title   string
		m       *Map[string, int]
		count   int
		results []result
	}{
		{title: "nil", m: &Map[string, int]{}, results: []result{}},
		{title: "empty", m: &Map[string, int]{}, results: []result{}},
		{title: "{foo: 1, bar: 2, baz: 3}", m: &fbb, results: []result{
			{key: "baz", value: 3},
			{key: "bar", value: 2},
			{key: "foo", value: 1},
		}},
		{title: "{foo: 1, bar: 2, baz: 3} but break after the first pair", m: &fbb, count: 1, results: []result{
			{key: "baz", value: 3},
		}},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			var (
				results []result
				count   int
			)
			for k, v := range tt.m.All() {
				if tt.count > 0 && tt.count == count {
					break
				}
				results = append(results, result{key: k, value: v})
				count++
			}
			if !slices.Equal(results, tt.results) {
				t.Errorf("got %v, want %v", results, tt.results)
			}
		})
	}
}
