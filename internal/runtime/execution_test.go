package runtime

import (
	"slices"
	"testing"
	"weak"

	"github.com/ichiban/prolog/v2/internal/term"
)

// execution returns an execution over an engine with a heap of the given size.
func execution(size int) *Execution {
	return &Execution{
		Engine: &Engine{Arena: term.NewArena(size)},
	}
}

// fill allocates n distinct variables and returns them.
func fill(e *Execution, n int) []term.Cell {
	cs := make([]term.Cell, n)
	for i := range cs {
		cs[i] = must(e.PutVariable())
	}
	return cs
}

func TestExecution_enter(t *testing.T) {
	tests := []struct {
		title string
		args  int
	}{
		{title: "no arguments", args: 0},
		{title: "one argument", args: 1},
		{title: "several arguments", args: 4},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			e := execution(64)

			// A register the previous goal left behind. Anything enter doesn't
			// overwrite stays put, which is why liveRegs has to come down to
			// match: it's how GC tells the two apart.
			stale := must(e.PutAtom(term.NewAtom("stale")))
			for i := range e.tempVars[:8] {
				e.tempVars[i] = stale
			}
			e.liveRegs = 7

			args := fill(e, test.args)
			e.enter(42, slices.Values(args))

			if e.programPointer != 42 {
				t.Errorf("expected: %d, got: %d", 42, e.programPointer)
			}
			if e.liveRegs != test.args {
				t.Errorf("expected: %d, got: %d", test.args, e.liveRegs)
			}
			for i, arg := range args {
				if got := e.tempVars[i+1]; got != arg {
					t.Errorf("X%d: expected: %v, got: %v", i+1, arg, got)
				}
			}
			if got := e.tempVars[test.args+1]; got != stale {
				t.Errorf("enter wrote past its arguments: %v", got)
			}
		})
	}
}

func TestExecution_roots_argumentRegisters(t *testing.T) {
	tests := []struct {
		title    string
		liveRegs int
	}{
		{title: "no live registers", liveRegs: 0},
		{title: "one live register", liveRegs: 1},
		{title: "several live registers", liveRegs: 3},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			e := execution(64)
			for i := range e.tempVars[:8] {
				e.tempVars[i] = must(e.PutVariable())
			}
			e.liveRegs = test.liveRegs

			// X0 is scratch for inline built-ins, and everything above
			// liveRegs belongs to a goal that has already been abandoned; the
			// heap they name may well have been cut back. Following either is
			// how GC ends up reading past the end of the heap.
			want := []*term.Cell{&e.structurePointer.term}
			for i := 1; i <= test.liveRegs; i++ {
				want = append(want, &e.tempVars[i])
			}

			if got := slices.Collect(e.roots()); !slices.Equal(got, want) {
				t.Errorf("expected: %v, got: %v", want, got)
			}
		})
	}
}

func TestExecution_roots(t *testing.T) {
	e := execution(64)

	e.tempVars[1] = must(e.PutVariable())
	e.liveRegs = 1

	e.structurePointer.term = must(e.PutCompound(term.NewAtom("f"), must(e.PutVariable())))

	e.trail = fill(e, 2)

	// A plain choice point, then one a built-in is suspended on. The built-in's
	// terms live in a Go frame, reachable only through the weak pointers it
	// registered when it took them.
	held := new(must(e.PutVariable()))
	captured := []weak.Pointer[term.Cell]{weak.Make(held)}
	e.stack = []stackFrame{
		{tempVars: must(e.PutCompound(term.NewAtom("$temp_vars"), must(e.PutVariable())))},
		{tempVars: must(e.PutAtom(term.NewAtom("$temp_vars"))), captured: &captured},
	}

	want := []*term.Cell{
		&e.structurePointer.term,
		&e.tempVars[1],
		&e.trail[0], &e.trail[1],
		&e.stack[0].tempVars,
		&e.stack[1].tempVars,
		held,
	}
	if got := slices.Collect(e.roots()); !slices.Equal(got, want) {
		t.Errorf("expected: %v, got: %v", want, got)
	}
}

func TestExecution_roots_droppedRef(t *testing.T) {
	e := execution(64)

	// A built-in that has let go of a Ref leaves a cleared weak pointer behind.
	// Skipping it is the point of the weak pointer; dereferencing it is a nil
	// panic.
	var captured []weak.Pointer[term.Cell]
	captured = append(captured, weak.Pointer[term.Cell]{})
	e.stack = []stackFrame{{tempVars: must(e.PutAtom(term.NewAtom("$temp_vars"))), captured: &captured}}

	want := []*term.Cell{&e.structurePointer.term, &e.stack[0].tempVars}
	if got := slices.Collect(e.roots()); !slices.Equal(got, want) {
		t.Errorf("expected: %v, got: %v", want, got)
	}
}

func TestExecution_heapTops(t *testing.T) {
	e := execution(64)
	e.heapBacktrackPoint = 7
	e.stack = []stackFrame{{heapTop: 3}, {heapTop: 5}}

	want := []*int{&e.heapBacktrackPoint, &e.stack[0].heapTop, &e.stack[1].heapTop}
	if got := slices.Collect(e.heapTops()); !slices.Equal(got, want) {
		t.Errorf("expected: %v, got: %v", want, got)
	}
}

func TestExecution_restoreState(t *testing.T) {
	e := execution(64)

	a, b := must(e.PutAtom(term.NewAtom("a"))), must(e.PutAtom(term.NewAtom("b")))
	tvs := must(e.PutCompound(term.NewAtom("$temp_vars"), a, b))

	v := must(e.PutVariable())
	heapTop := len(e.Heap)
	e.stack = []stackFrame{{tempVars: tvs, heapTop: heapTop, trailTop: 0, cutB: 4}}

	// State the abandoned branch built: a binding to undo, heap above the
	// choice point to drop, registers to overwrite, and a structure pointer
	// naming a cell that is about to go away.
	if err := e.Bind(v, must(e.PutAtom(term.NewAtom("c")))); err != nil {
		t.Fatal(err)
	}
	e.trail = []term.Cell{v}
	e.tempVars[1] = must(e.PutAtom(term.NewAtom("x")))
	e.tempVars[3] = must(e.PutAtom(term.NewAtom("y")))
	e.liveRegs = 3
	e.structurePointer = structurePointer{term: must(e.PutCompound(term.NewAtom("f"), v)), argNo: 1}

	if err := e.restoreState(); err != nil {
		t.Fatal(err)
	}

	if len(e.stack) != 0 {
		t.Errorf("expected the frame to be popped, got %d", len(e.stack))
	}
	if e.tempVars[1] != a || e.tempVars[2] != b {
		t.Errorf("expected: %v, %v, got: %v, %v", a, b, e.tempVars[1], e.tempVars[2])
	}
	// The saved registers say how many are live again. X3 still holds the
	// abandoned branch's value, and the heap it named is gone.
	if e.liveRegs != 2 {
		t.Errorf("expected: %d, got: %d", 2, e.liveRegs)
	}
	if len(e.Heap) != heapTop {
		t.Errorf("expected: %d, got: %d", heapTop, len(e.Heap))
	}
	if _, ok := e.Variable(e.Deref(v)); !ok {
		t.Errorf("expected the binding to be undone, got: %v", e.Deref(v))
	}
	if len(e.trail) != 0 {
		t.Errorf("expected: %d, got: %d", 0, len(e.trail))
	}
	if e.cutB != 4 {
		t.Errorf("expected: %d, got: %d", 4, e.cutB)
	}
	// S named a cell the truncation just discarded. The clause we resume in
	// sets it again before any unify_*, so dropping it costs nothing and keeps
	// GC from following it.
	if e.structurePointer != (structurePointer{}) {
		t.Errorf("expected the structure pointer to be cleared, got: %v", e.structurePointer)
	}
}

func TestExecution_setNextGCThreshold(t *testing.T) {
	const capacity = 1024

	tests := []struct {
		title string
		live  int
		want  int
	}{
		// Half the arena is the floor: with a heap that can't grow, collecting
		// while most of it is free is cost without a benefit.
		{title: "an empty heap waits for the floor", live: 0, want: capacity / 2},
		{title: "a nearly empty heap waits for the floor", live: 100, want: capacity / 2},
		// Past the floor it's the usual rule: let the heap grow to twice what
		// survived, so collecting amortizes against allocating.
		{title: "twice the live set once that clears the floor", live: 400, want: 800},
		// Once twice the live set overshoots, the threshold pins to the whole
		// arena, which in practice means no further collection: put fails
		// first, and collection only happens at an execute.
		{title: "clamped to the arena when the live set is over half", live: 600, want: capacity},
		{title: "clamped to the arena when the heap is full", live: capacity, want: capacity},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			e := execution(capacity)
			fill(e, test.live)

			e.setNextGCThreshold()

			if e.gcThreshold != test.want {
				t.Errorf("expected: %d, got: %d", test.want, e.gcThreshold)
			}
		})
	}
}

func TestExecution_pin(t *testing.T) {
	e := execution(64)
	c := must(e.PutVariable())

	unpin := e.pin(&c)
	if got, want := slices.Collect(e.Engine.roots()), &c; !slices.Contains(got, want) {
		t.Errorf("expected %v to be rooted, got: %v", want, got)
	}

	unpin()
	if got, want := slices.Collect(e.Engine.roots()), &c; slices.Contains(got, want) {
		t.Errorf("expected %v not to be rooted, got: %v", want, got)
	}
}
