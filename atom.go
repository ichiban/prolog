package prolog

type Atom string

type AtomTable struct {
	ids     map[Atom]int32
	entries []atomTableEntry
}

type atomTableEntry struct {
	name Atom
	// TODO: GC
}

func (a *AtomTable) Put(name Atom) (int32, error) {
	if id, ok := a.ids[name]; ok {
		return id, nil
	}

	if len(a.entries) == cap(a.entries) {
		return 0, &ResourceError{Resource: "atom"}
	}

	id := int32(len(a.entries))
	a.entries = append(a.entries, atomTableEntry{
		name: name,
	})
	if a.ids == nil {
		a.ids = make(map[Atom]int32, cap(a.entries))
	}
	a.ids[name] = id
	return id, nil
}

func (a *AtomTable) Get(id int32) Atom {
	e := a.entries[id]
	return e.name
}
