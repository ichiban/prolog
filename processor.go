package prolog

import (
	"errors"
	"io/fs"
	"strings"
)

type Processor struct {
	Heap
	ModuleSystem
	FS   map[string]fs.FS
	Warn func(Term) error // TODO: Not sure what warning should look like.
}

func (p *Processor) Load(filename string) error {
	return nil // TODO:
}

func (p *Processor) Compile(text string) error {
	// - text
	// - sequence of terms
	// - mappings from PI to terms
	// - labeled instructions
	// - instructions

	m := p.typeIn
	defer func() {
		p.typeIn = m
	}()

	var (
		ps      = NewParser(strings.NewReader(text), p.typeIn)
		clauses = map[Functor][]headBody{}
	)
	for ps.More() {
		t, _, err := ps.Term(&p.Heap)
		if err != nil {
			return err
		}
		// TODO: Transformations (especially binarization)
		f, err := p.Heap.Functor(t)
		if err != nil {
			return err
		}
		switch f {
		case Functor{Name: "::-", Arity: 1}: // Directive
			// TODO: Invoke directives.
			continue
		case Functor{Name: "::-", Arity: 2}: // Rule
			head, body := p.Heap.Arg(t, 0), p.Heap.Arg(t, 1)
			f, err := p.Heap.Functor(head)
			if err != nil {
				return err
			}
			cs, _ := clauses[f] // TODO: Discontiguous check.
			clauses[f] = append(cs, headBody{head, body})
		default:
			return errors.New("unknown functor")
		}
	}
	for pi, hbs := range clauses {
		cp, err := compileClauses(&p.Heap, p.typeIn, pi, hbs)
		if err != nil {
			return err
		}
		e, _ := m.procedures[pi]
		e.procedure = cp
		m.procedures[pi] = e
	}
	return nil
}
