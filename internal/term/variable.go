package term

type Variable struct {
	term Handle
}

// Bind binds a variable term to another term.
func (v Variable) Bind(t Handle) error {
	h := v.term.heap
	if t.heap != nil && t.heap != h {
		return ErrIncompatibleHandle
	}

	(*h)[v.term.cell.value] = pack(t.cell)
	return nil
}
