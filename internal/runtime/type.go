package runtime

import (
	"unicode/utf8"

	"github.com/ichiban/prolog/v2/internal/syntax"
	"github.com/ichiban/prolog/v2/internal/term"
)

func (e *Engine) canBeAtom(t term.Handle) (term.Atom, bool, error) {
	if _, ok := e.Variable(t); ok {
		return term.Atom{}, false, nil
	}
	a, ok := e.Atom(t)
	if !ok {
		return term.Atom{}, false, &TypeError{
			ValidType: term.NewAtom("atom"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}
	return a, true, nil
}

func (e *Engine) mustBeAtom(t term.Handle) (term.Atom, error) {
	if _, ok := e.Variable(t); ok {
		return term.Atom{}, &InstantiationError{
			Location: e.location,
		}
	}
	a, ok := e.Atom(t)
	if !ok {
		return term.Atom{}, &TypeError{
			ValidType: term.NewAtom("atom"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}
	return a, nil
}

func (e *Engine) canBeChar(t term.Handle) (rune, bool, error) {
	if _, ok := e.Variable(t); ok {
		return 0, false, nil
	}
	a, ok := e.Atom(t)
	r := a.Rune()
	if !ok || r == utf8.RuneError {
		return 0, false, &TypeError{
			ValidType: term.NewAtom("character"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}
	return r, true, nil
}

func (e *Engine) mustBeChar(t term.Handle) (rune, error) {
	r, ok, err := e.canBeChar(t)
	if err != nil {
		return 0, err
	}
	if !ok {
		return 0, &InstantiationError{
			Location: e.location,
		}
	}
	return r, nil
}

func (e *Engine) canBeInteger(t term.Handle) (int64, bool, error) {
	if _, ok := e.Variable(t); ok {
		return 0, false, nil
	}
	n, ok := e.Integer(t)
	if !ok {
		return 0, false, &TypeError{
			ValidType: term.NewAtom("integer"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}
	return n, true, nil
}

func (e *Engine) mustBeInteger(t term.Handle) (int64, error) {
	n, ok, err := e.canBeInteger(t)
	if err != nil {
		return 0, err
	}

	if !ok {
		return 0, &InstantiationError{
			Location: e.location,
		}
	}

	return n, nil
}

func (e *Engine) canBeNotLessThanZero(t term.Handle) (int64, bool, error) {
	i, ok, err := e.canBeInteger(t)
	if err != nil {
		return 0, false, err
	}
	if !ok {
		return 0, false, nil
	}
	if i < 0 {
		return 0, false, &DomainError{
			ValidDomain: term.NewAtom("not_less_than_zero"),
			Culprit:     syntax.Serialize(e.Arena, t),
			Location:    e.location,
		}
	}
	return i, ok, nil
}

func (e *Engine) canBeCharCode(t term.Handle) (rune, bool, error) {
	if _, ok := e.Variable(t); ok {
		return 0, false, nil
	}
	i, ok := e.Integer(t)
	r := rune(i)
	if !ok || !utf8.ValidRune(r) {
		return 0, false, &TypeError{
			ValidType: term.NewAtom("character_code"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}
	return r, true, nil
}

func (e *Engine) mustBeCharCode(t term.Handle) (rune, error) {
	r, ok, err := e.canBeCharCode(t)
	if err != nil {
		return 0, err
	}
	if !ok {
		return 0, &InstantiationError{
			Location: e.location,
		}
	}
	return r, nil
}

func (e *Engine) canBeFloat(t term.Handle) (float64, bool, error) {
	if _, ok := e.Variable(t); ok {
		return 0, false, nil
	}
	f, ok := e.Float(t)
	if !ok {
		return 0, false, &TypeError{
			ValidType: term.NewAtom("float"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}
	return f, true, nil
}

func (e *Engine) mustBeFloat(t term.Handle) (float64, error) {
	f, ok, err := e.canBeFloat(t)
	if err != nil {
		return 0, err
	}

	if !ok {
		return 0, &InstantiationError{
			Location: e.location,
		}
	}

	return f, nil
}

func (e *Engine) mustBeAtomic(t term.Handle) error {
	if _, ok := e.Variable(t); ok {
		return &InstantiationError{
			Location: e.location,
		}
	}
	if _, ok := e.Functor(t); ok {
		return &TypeError{
			ValidType: term.NewAtom("atomic"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}
	return nil
}

func (e *Engine) mustBeNumber(t term.Handle) (int64, bool, float64, bool, error) {
	t = e.Deref(t)

	if _, ok := e.Variable(t); ok {
		return 0, false, 0, false, &InstantiationError{
			Location: e.location,
		}
	}

	if i, ok := e.Integer(t); ok {
		return i, true, 0, false, nil
	}

	if f, ok := e.Float(t); ok {
		return 0, false, f, true, nil
	}

	return 0, false, 0, false, &TypeError{
		ValidType: term.NewAtom("number"),
		Culprit:   syntax.Serialize(e.Arena, t),
		Location:  e.location,
	}
}

func (e *Engine) canBeCallable(t term.Handle) (term.Functor, bool, error) {
	if _, ok := e.Variable(t); ok {
		return 0, false, nil
	}

	f, ok := e.Functor(t, term.AllowAtom(true))
	if !ok {
		return 0, false, &TypeError{
			ValidType: term.NewAtom("callable"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}

	return f, true, nil
}

func (e *Engine) mustBeCallable(t term.Handle) (term.Functor, error) {
	f, ok, err := e.canBeCallable(t)
	if err != nil {
		return 0, err
	}
	if !ok {
		return 0, &InstantiationError{
			Location: e.location,
		}
	}
	return f, nil
}

func (e *Engine) canBePredicateIndicator(t term.Handle) (term.Functor, bool, error) {
	if _, ok := e.Variable(t); ok {
		return 0, false, nil
	}

	if f, ok := e.Functor(t, term.AllowAtom(true)); !ok || f != term.NewFunctor(term.NewAtomRune('/'), 2) {
		return 0, false, &TypeError{
			ValidType: term.NewAtom("predicate_indicator"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}

	name, arity := e.Deref(e.Arg(t, 0)), e.Deref(e.Arg(t, 1))

	n, nok, err := e.canBeAtom(name)
	if err != nil {
		return 0, false, &TypeError{
			ValidType: term.NewAtom("predicate_indicator"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}

	a, aok, err := e.canBeInteger(arity)
	if err != nil {
		return 0, false, &TypeError{
			ValidType: term.NewAtom("predicate_indicator"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}

	if !nok || !aok {
		return 0, false, nil
	}

	pi := term.NewFunctor(n, int(a))
	return pi, true, nil
}

func (e *Engine) mustBePredicateIndicator(t term.Handle) (term.Functor, error) {
	pi, ok, err := e.canBePredicateIndicator(t)
	if err != nil {
		return 0, err
	}
	if !ok {
		return 0, &InstantiationError{
			Location: e.location,
		}
	}
	return pi, nil
}

func (e *Engine) canBeStream(t term.Handle) (*term.Stream, error) {
	t = e.Deref(t)
	if _, ok := e.Variable(t); ok {
		return nil, nil
	}
	s, ok := e.Stream(t)
	if !ok {
		return nil, &TypeError{
			ValidType: term.NewAtom("stream"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}
	return s, nil
}

func (e *Engine) mustBeStream(t term.Handle) (*term.Stream, error) {
	s, err := e.canBeStream(t)
	if err != nil {
		return nil, err
	}
	if s == nil {
		return nil, &InstantiationError{
			Location: e.location,
		}
	}
	return s, nil
}

func (e *Engine) canBeSourceSink(t term.Handle) (term.Atom, string, error) {
	t = e.Deref(t)

	if _, ok := e.Variable(t); ok {
		return term.Atom{}, "", nil
	}

	var fsID term.Atom
	if f, ok := e.Functor(t); ok && f.Arity() == 1 {
		fsID = f.Name()
		t = e.Deref(e.Arg(t, 0))
	}

	if a, ok := e.Atom(t); ok {
		return fsID, a.String(), nil
	}

	if s, ok := e.CharList(t); ok {
		return fsID, s, nil
	}

	return term.Atom{}, "", &DomainError{
		ValidDomain: term.NewAtom("source_sink"),
		Culprit:     syntax.Serialize(e.Arena, t),
		Location:    e.location,
	}
}

func (e *Engine) mustBeSourceSink(t term.Handle) (term.Atom, string, error) {
	fsID, s, err := e.canBeSourceSink(t)
	if err != nil {
		return term.Atom{}, "", err
	}
	if s == "" {
		return term.Atom{}, "", &InstantiationError{
			Location: e.location,
		}
	}
	return fsID, s, nil
}

func (e *Engine) canBeMode(t term.Handle) (term.Mode, bool, error) {
	t = e.Deref(t)

	a, ok, err := e.canBeAtom(t)
	if err != nil || !ok {
		return 0, false, err
	}

	switch a {
	case term.NewAtom("read"):
		return term.Read, true, nil
	case term.NewAtom("write"):
		return term.Write, true, nil
	case term.NewAtom("append"):
		return term.Append, true, nil
	default:
		return 0, false, &DomainError{
			ValidDomain: term.NewAtom("mode"),
			Culprit:     syntax.Serialize(e.Arena, t),
			Location:    e.location,
		}
	}
}

func (e *Engine) mustBeMode(t term.Handle) (term.Mode, error) {
	m, ok, err := e.canBeMode(t)
	if err != nil {
		return 0, err
	}
	if !ok {
		return 0, &InstantiationError{
			Location: e.location,
		}
	}
	return m, nil
}

func (e *Engine) canBeStreamOrAlias(t term.Handle) (*term.Stream, error) {
	t = e.Deref(t)

	if _, ok := e.Variable(t); ok {
		return nil, nil
	}

	if a, ok := e.Atom(t); ok {
		for t := range e.OpenStreams() {
			s, _ := e.Stream(t)
			if s.Alias == a {
				return s, nil
			}
		}
		return nil, &ExistenceError{
			ObjectType: term.NewAtom("stream"),
			Culprit:    syntax.Serialize(e.Arena, t),
			Location:   e.location,
		}
	}

	s, ok := e.Stream(t)
	if !ok {
		return nil, &DomainError{
			ValidDomain: term.NewAtom("stream_or_alias"),
			Culprit:     syntax.Serialize(e.Arena, t),
			Location:    e.location,
		}
	}
	if s.Closed {
		return nil, &ExistenceError{
			ObjectType: term.NewAtom("stream"),
			Culprit:    syntax.Serialize(e.Arena, t),
			Location:   e.location,
		}
	}

	return s, nil
}

func (e *Engine) mustBeStreamOrAlias(t term.Handle) (*term.Stream, error) {
	s, err := e.canBeStreamOrAlias(t)
	if err != nil {
		return nil, err
	}
	if s == nil {
		return nil, &InstantiationError{
			Location: e.location,
		}
	}
	return s, nil
}

func (e *Engine) canBeStreamProperty(t term.Handle) error {
	t = e.Deref(t)

	if _, ok := e.Variable(t); ok {
		return nil
	}

	switch f, _ := e.Functor(t, term.AllowAtom(true)); f {
	case term.NewFunctor(term.NewAtom("input"), 0),
		term.NewFunctor(term.NewAtom("output"), 0):
		return nil
	case term.NewFunctor(term.NewAtom("file_name"), 1):
		arg := e.Arg(t, 0)
		arg = e.Deref(arg)
		if _, _, err := e.canBeAtom(arg); err == nil {
			return nil
		}
		break
	case term.NewFunctor(term.NewAtom("mode"), 1),
		term.NewFunctor(term.NewAtom("alias"), 1),
		term.NewFunctor(term.NewAtom("end_of_stream"), 1),
		term.NewFunctor(term.NewAtom("eof_action"), 1),
		term.NewFunctor(term.NewAtom("reposition"), 1):
		arg := e.Arg(t, 0)
		arg = e.Deref(arg)
		if _, _, err := e.canBeAtom(arg); err == nil {
			return nil
		}
		break
	case term.NewFunctor(term.NewAtom("position"), 1):
		arg := e.Arg(t, 0)
		arg = e.Deref(arg)
		if _, _, err := e.canBeInteger(arg); err == nil {
			return nil
		}
		break
	}
	return &DomainError{
		ValidDomain: term.NewAtom("stream_property"),
		Culprit:     syntax.Serialize(e.Arena, t),
		Location:    e.location,
	}
}

func (e *Engine) canBeInByte(t term.Handle) (byte, bool, error) {
	t = e.Deref(t)

	if _, ok := e.Variable(t); ok {
		return 0, false, nil
	}

	b, ok := e.Integer(t)
	if !ok || b < 0 || b > 255 {
		return 0, false, &TypeError{
			ValidType: term.NewAtom("in_byte"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}

	return byte(b), true, nil
}

func (e *Engine) canBeByte(t term.Handle) (byte, bool, error) {
	t = e.Deref(t)

	if _, ok := e.Variable(t); ok {
		return 0, false, nil
	}

	b, ok := e.Integer(t)
	if !ok || b < 0 || b > 255 {
		return 0, false, &TypeError{
			ValidType: term.NewAtom("byte"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}

	return byte(b), true, nil
}

func (e *Engine) mustBeByte(t term.Handle) (byte, error) {
	b, ok, err := e.canBeByte(t)
	if err != nil {
		return 0, err
	}
	if !ok {
		return 0, &InstantiationError{
			Location: e.location,
		}
	}
	return b, nil
}

func (e *Engine) canBeInChar(t term.Handle) (rune, bool, error) {
	if _, ok := e.Variable(t); ok {
		return 0, false, nil
	}
	a, ok := e.Atom(t)
	if ok && a == term.NewAtom("end_of_file") {
		return -1, true, nil
	}
	r := a.Rune()
	if !ok || r == utf8.RuneError {
		return 0, false, &TypeError{
			ValidType: term.NewAtom("in_character"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}
	return r, true, nil
}

func (e *Engine) canBeInCharCode(t term.Handle) (rune, bool, error) {
	if _, ok := e.Variable(t); ok {
		return 0, false, nil
	}
	i, ok := e.Integer(t)
	if ok && i == -1 {
		return -1, true, nil
	}
	r := rune(i)
	if !ok || !utf8.ValidRune(r) {
		return 0, false, &TypeError{
			ValidType: term.NewAtom("in_character_code"),
			Culprit:   syntax.Serialize(e.Arena, t),
			Location:  e.location,
		}
	}
	return r, true, nil
}

func (e *Engine) canBeList(list term.Handle, fn func(elem term.Handle) error) (bool, error) {
	if fn == nil {
		fn = func(term.Handle) error {
			return nil
		}
	}
	for elem, ok := range e.List(list) {
		if !ok {
			if _, ok := e.Variable(elem); ok {
				return false, nil
			}

			return false, &TypeError{
				ValidType: term.NewAtom("list"),
				Culprit:   syntax.Serialize(e.Arena, list),
				Location:  e.location,
			}
		}

		if err := fn(elem); err != nil {
			return false, err
		}
	}
	return true, nil
}

func (e *Engine) mustBeList(list term.Handle, fn func(elem term.Handle) error) error {
	if fn == nil {
		fn = func(term.Handle) error {
			return nil
		}
	}
	for elem, ok := range e.List(list) {
		if !ok {
			elem = e.Deref(elem)
			if _, ok := e.Variable(elem); ok {
				return &InstantiationError{
					Location: e.location,
				}
			}
			return &TypeError{
				ValidType: term.NewAtom("list"),
				Culprit:   syntax.Serialize(e.Arena, list),
				Location:  e.location,
			}
		}

		if err := fn(elem); err != nil {
			return err
		}
	}
	return nil
}

func (e *Engine) mustBeNonEmptyList(list term.Handle, fn func(elem term.Handle) error) error {
	var ok bool
	if err := e.mustBeList(list, func(elem term.Handle) error {
		ok = true
		return fn(elem)
	}); err != nil {
		return err
	}
	if !ok {
		return &DomainError{
			ValidDomain: term.NewAtom("non_empty_list"),
			Culprit:     syntax.Serialize(e.Arena, list),
			Location:    e.location,
		}
	}
	return nil
}
