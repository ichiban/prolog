package rbtree

import (
	"fmt"
)

func ExampleMap_Set() {
	var m Map[string, int]
	m.Set("foo", 1)
	m.Set("bar", 2)
	snapshot := m
	m.Set("baz", 3)
	m.Set("foo", 4)

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
