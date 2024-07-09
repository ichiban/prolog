package internal

import (
	"errors"
	"unicode/utf8"
)

var (
	errTooManyTerms    = errors.New("too many terms")
	errTooManyAtoms    = errors.New("too many atoms")
	errTooManyIntegers = errors.New("too many integers")
	errTooManyFloats   = errors.New("too many floats")
	errTooManyStrings  = errors.New("too many strings")

	errWrongType     = errors.New("wrong type")
	errWrongArgument = errors.New("wrong argument")
)

type Type int8

const (
	TypeInvalid Type = iota
	TypeVariable
	TypeAtom
	TypeInteger
	TypeFloat
	TypeCompound
)

type termTag int8

const (
	termTagVariable termTag = iota
	termTagAtom
	termTagInteger
	termTagFloat
	termTagFunctor
	termTagArgument
	termTagReference
	termTagList
	termTagString
)

// TermID refers to a term in a TermPool.
type TermID int32

// TermPool is a memory region to store terms.
type TermPool struct {
	tags     []termTag
	payloads []int32

	atoms    []Atom
	integers []int
	floats   []float64
	strings  []string
	env      Env
}

func (t *TermPool) Type(id TermID) Type {
	id = t.resolve(id)
	switch tag := t.tags[id]; tag {
	case termTagVariable:
		return TypeVariable
	case termTagAtom:
		return TypeAtom
	case termTagInteger:
		return TypeInteger
	case termTagFloat:
		return TypeFloat
	case termTagFunctor, termTagList, termTagString:
		return TypeCompound
	default:
		return TypeInvalid
	}
}

func (t *TermPool) PutVariable(v Variable) (TermID, error) {
	return t.putTagPayload(termTagVariable, int32(v))
}

func (t *TermPool) Variable(id TermID) (Variable, bool) {
	id = t.resolve(id)
	if t.tags[id] != termTagVariable {
		return 0, false
	}
	return Variable(t.payloads[id]), true
}

func (t *TermPool) PutAtom(atom Atom) (TermID, error) {
	var ok bool
	t.atoms, ok = cappend(t.atoms, atom)
	if !ok {
		return 0, errTooManyAtoms
	}

	return t.putTagPayload(termTagAtom, int32(len(t.atoms)-1))
}

func (t *TermPool) Atom(id TermID) (Atom, bool) {
	id = t.resolve(id)
	if t.tags[id] != termTagAtom {
		return 0, false
	}
	return t.atoms[t.payloads[id]], true
}

func (t *TermPool) PutInteger(n int) (TermID, error) {
	var ok bool
	t.integers, ok = cappend(t.integers, n)
	if !ok {
		return 0, errTooManyIntegers
	}

	return t.putTagPayload(termTagInteger, int32(len(t.integers)-1))
}

func (t *TermPool) Integer(id TermID) (int, bool) {
	id = t.resolve(id)
	if t.tags[id] != termTagInteger {
		return 0, false
	}
	return t.integers[t.payloads[id]], true
}

func (t *TermPool) PutFloat(f float64) (TermID, error) {
	var ok bool
	t.floats, ok = cappend(t.floats, f)
	if !ok {
		return 0, errTooManyFloats
	}

	return t.putTagPayload(termTagFloat, int32(len(t.floats)-1))
}

func (t *TermPool) Float(id TermID) (float64, bool) {
	id = t.resolve(id)
	if t.tags[id] != termTagFloat {
		return 0, false
	}
	return t.floats[t.payloads[id]], true
}

func (t *TermPool) Atomic(id TermID) bool {
	id = t.resolve(id)
	tag := t.tags[id]
	return termTagAtom <= tag && tag <= termTagFloat
}

func (t *TermPool) PutCompound(name Atom, args ...TermID) (TermID, error) {
	if len(args) == 0 {
		return t.PutAtom(name)
	}

	id, err := t.putTagPayload(termTagFunctor, int32(name))
	if err != nil {
		return 0, err
	}

	// A compound term doesn't store its arity explicitly so that `current_prolog_flag(max_arity, unbounded)`.
	for _, arg := range args {
		if _, err := t.putTagPayload(termTagArgument, int32(arg)); err != nil {
			return 0, err
		}
	}

	return id, nil
}

func (t *TermPool) PutList(args ...TermID) (TermID, error) {
	if len(args) == 0 {
		return t.PutAtom(NewAtom("[]"))
	}

	id, err := t.putTagPayload(termTagList, int32(args[0]))
	if err != nil {
		return 0, err
	}

	for _, arg := range args[1:] {
		if _, err := t.putTagPayload(termTagList, int32(arg)); err != nil {
			return 0, err
		}
	}

	if _, err := t.PutAtom(NewAtom("[]")); err != nil {
		return 0, err
	}

	return id, nil
}

func (t *TermPool) PutPartial(rest TermID, args ...TermID) (TermID, error) {
	if len(args) == 0 {
		return t.putTagPayload(termTagReference, int32(rest))
	}

	id, err := t.putTagPayload(termTagList, int32(args[0]))
	if err != nil {
		return 0, err
	}

	for _, arg := range args[1:] {
		if _, err := t.putTagPayload(termTagList, int32(arg)); err != nil {
			return 0, err
		}
	}

	if _, err := t.putTagPayload(termTagReference, int32(rest)); err != nil {
		return 0, err
	}

	return id, nil
}

func (t *TermPool) PutString(str string) (TermID, error) {
	var ok bool
	t.strings, ok = cappend(t.strings, str)
	if !ok {
		return 0, errTooManyStrings
	}

	return t.putTagPayload(termTagString, int32(len(t.strings)-1))
}

func (t *TermPool) Compound(id TermID) bool {
	id = t.resolve(id)
	switch tag := t.tags[id]; tag {
	case termTagFunctor, termTagList, termTagString:
		return true
	default:
		return false
	}
}

func (t *TermPool) Functor(id TermID) (Atom, error) {
	id = t.resolve(id)
	switch tag := t.tags[id]; tag {
	case termTagFunctor:
		return t.atoms[t.payloads[id]], nil
	case termTagList, termTagString:
		return Atom('.'), nil
	default:
		return 0, errWrongType
	}
}

func (t *TermPool) Arity(id TermID) (int, error) {
	id = t.resolve(id)
	switch tag := t.tags[id]; tag {
	case termTagFunctor:
		i := 1
		for t.tags[int(id)+i] == termTagArgument {
			i++
		}
		return i, nil
	case termTagList, termTagString:
		return 2, nil
	default:
		return 0, errWrongType
	}
}

func (t *TermPool) Arg(id TermID, n int) (TermID, error) {
	id = t.resolve(id)
	switch tag := t.tags[id]; tag {
	case termTagFunctor:
		return TermID(t.payloads[int(id)+1+n]), nil // No boundary check.
	case termTagList:
		switch n {
		case 0:
			return TermID(t.payloads[id]), nil
		case 1:
			return id + 1, nil
		default:
			return 0, errWrongArgument
		}
	case termTagString:
		str := t.strings[t.payloads[id]]
		r, i := utf8.DecodeRuneInString(str)
		switch n {
		case 0:
			return t.PutAtom(Atom(r))
		case 1:
			return t.PutAtom(NewAtom(str[i:]))
		default:
			return 0, errWrongArgument
		}
	default:
		return 0, errWrongType
	}
}

func (t *TermPool) putTagPayload(tag termTag, payload int32) (TermID, error) {
	var ok bool
	t.tags, ok = cappend(t.tags, tag)
	if !ok {
		return 0, errTooManyTerms
	}
	t.payloads, _ = cappend(t.payloads, payload)
	return TermID(len(t.tags) - 1), nil
}

func (t *TermPool) resolve(id TermID) TermID {
	switch tag := t.tags[id]; tag {
	case termTagVariable:
		ret, ok := t.env.Lookup(Variable(t.payloads[id]))
		if !ok {
			return id
		}
		return t.resolve(ret)
	case termTagReference:
		return t.resolve(TermID(t.payloads[id]))
	default:
		return id
	}
}

// cappend means capped append.
func cappend[S ~[]E, E any](slice S, elems ...E) (S, bool) {
	if len(slice)+len(elems) > cap(slice) {
		return slice, false
	}
	return append(slice, elems...), true
}
