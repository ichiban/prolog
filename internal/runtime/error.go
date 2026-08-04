package runtime

import (
	"errors"
	"fmt"

	"github.com/ichiban/prolog/v2/internal/syntax"
	"github.com/ichiban/prolog/v2/internal/term"
)

type Termer interface {
	Term(arena *term.Arena) (term.Handle, error)
}

func ErrorTerm(arena *term.Arena, err error) (term.Handle, error) {
	err = cause(err)
	if err == nil {
		return term.Handle{}, errors.New("nil error")
	}

	if err, ok := err.(Termer); ok {
		return err.Term(arena)
	}

	origErr := err
	t, err := arena.PutAtom(term.NewAtom("system_error"))
	if err != nil {
		return term.Handle{}, err
	}
	s, err := arena.PutCharList(origErr.Error())
	if err != nil {
		return term.Handle{}, err
	}
	return arena.PutCompound(term.NewAtom("error"), t, s)
}

func cause(err error) error {
	unwrapped := errors.Unwrap(err)
	if unwrapped == nil {
		return err
	}
	return cause(unwrapped)
}

// InstantiationError is an error that signifies a term is variable.
type InstantiationError struct {
	Location term.Functor
}

func (e *InstantiationError) Error() string {
	return fmt.Sprintf("instantiation error: location = %s", e.Location)
}

func (e *InstantiationError) Term(arena *term.Arena) (term.Handle, error) {
	t, err := arena.PutAtom(term.NewAtom("instantiation_error"))
	if err != nil {
		return term.Handle{}, err
	}
	l, err := arena.PutFunctor(e.Location)
	if err != nil {
		return term.Handle{}, err
	}
	return arena.PutCompound(term.NewAtom("error"), t, l)
}

// TypeError is an error that signifies an incorrect type.
type TypeError struct {
	ValidType term.Atom
	Culprit   syntax.Serialized
	Location  term.Functor
}

func (e *TypeError) Error() string {
	return fmt.Sprintf("invalid type: valid type = %s, culprit = %s, location = %s", e.ValidType, e.Culprit, e.Location)
}

func (e *TypeError) Term(arena *term.Arena) (term.Handle, error) {
	v, err := arena.PutAtom(e.ValidType)
	if err != nil {
		return term.Handle{}, err
	}
	c, err := syntax.Deserialize(arena, e.Culprit)
	if err != nil {
		return term.Handle{}, err
	}
	t, err := arena.PutCompound(term.NewAtom("type_error"), v, c)
	if err != nil {
		return term.Handle{}, err
	}
	l, err := arena.PutFunctor(e.Location)
	if err != nil {
		return term.Handle{}, err
	}
	return arena.PutCompound(term.NewAtom("error"), t, l)
}

// DomainError is an error that signifies an incorrect value.
type DomainError struct {
	ValidDomain term.Atom
	Culprit     syntax.Serialized
	Location    term.Functor
}

func (e *DomainError) Error() string {
	return fmt.Sprintf("invalid domain: valid domain = %s, culprit = %s, location = %s", e.ValidDomain, e.Culprit, e.Location)
}

func (e *DomainError) Term(arena *term.Arena) (term.Handle, error) {
	v, err := arena.PutAtom(e.ValidDomain)
	if err != nil {
		return term.Handle{}, err
	}
	c, err := syntax.Deserialize(arena, e.Culprit)
	if err != nil {
		return term.Handle{}, err
	}
	t, err := arena.PutCompound(term.NewAtom("domain_error"), v, c)
	if err != nil {
		return term.Handle{}, err
	}
	l, err := arena.PutFunctor(e.Location)
	if err != nil {
		return term.Handle{}, err
	}
	return arena.PutCompound(term.NewAtom("error"), t, l)
}

// ExistenceError is an error that signifies nonexistence of an object.
type ExistenceError struct {
	ObjectType term.Atom
	Culprit    syntax.Serialized
	Location   term.Functor
}

func (e *ExistenceError) Error() string {
	return fmt.Sprintf("%s does not exist: culprit = %s, location = %s", e.ObjectType, e.Culprit, e.Location)
}

func (e *ExistenceError) Term(arena *term.Arena) (term.Handle, error) {
	o, err := arena.PutAtom(e.ObjectType)
	if err != nil {
		return term.Handle{}, err
	}
	c, err := syntax.Deserialize(arena, e.Culprit)
	if err != nil {
		return term.Handle{}, err
	}
	t, err := arena.PutCompound(term.NewAtom("existence_error"), o, c)
	if err != nil {
		return term.Handle{}, err
	}
	l, err := arena.PutFunctor(e.Location)
	if err != nil {
		return term.Handle{}, err
	}
	return arena.PutCompound(term.NewAtom("error"), t, l)
}

// PermissionError is an error that signifies a disallowed operation.
type PermissionError struct {
	Operation      term.Atom
	PermissionType term.Atom
	Culprit        syntax.Serialized
	Location       term.Functor
}

func (e *PermissionError) Error() string {
	return fmt.Sprintf("disallowed operation %s on %s: culprit = %s, location = %s", e.Operation, e.PermissionType, e.Culprit, e.Location)
}

func (e *PermissionError) Term(arena *term.Arena) (term.Handle, error) {
	o, err := arena.PutAtom(e.Operation)
	if err != nil {
		return term.Handle{}, err
	}
	p, err := arena.PutAtom(e.PermissionType)
	if err != nil {
		return term.Handle{}, err
	}
	c, err := syntax.Deserialize(arena, e.Culprit)
	if err != nil {
		return term.Handle{}, err
	}
	t, err := arena.PutCompound(term.NewAtom("permission_error"), o, p, c)
	if err != nil {
		return term.Handle{}, err
	}
	l, err := arena.PutFunctor(e.Location)
	if err != nil {
		return term.Handle{}, err
	}
	return arena.PutCompound(term.NewAtom("error"), t, l)
}

// RepresentationError is an error that signifies one of the implementation limits exceeded.
type RepresentationError struct {
	Flag     term.Atom
	Location term.Functor
}

func (e *RepresentationError) Error() string {
	return fmt.Sprintf("implementation limit exceeded: %s, location = %s", e.Flag, e.Location)
}

func (e *RepresentationError) Term(arena *term.Arena) (term.Handle, error) {
	f, err := arena.PutAtom(e.Flag)
	if err != nil {
		return term.Handle{}, err
	}
	t, err := arena.PutCompound(term.NewAtom("representation_error"), f)
	if err != nil {
		return term.Handle{}, err
	}
	l, err := arena.PutFunctor(e.Location)
	if err != nil {
		return term.Handle{}, err
	}
	return arena.PutCompound(term.NewAtom("error"), t, l)
}

// ResourceError is an error that signifies lack of a resource.
type ResourceError struct {
	Resource term.Atom
	Location term.Functor
}

func (e *ResourceError) Error() string {
	return fmt.Sprintf("insufficient resource: %s", e.Resource)
}

func (e *ResourceError) Term(arena *term.Arena) (term.Handle, error) {
	r, err := arena.PutAtom(e.Resource)
	if err != nil {
		return term.Handle{}, err
	}
	t, err := arena.PutCompound(term.NewAtom("resource_error"), r)
	if err != nil {
		return term.Handle{}, err
	}
	l, err := arena.PutFunctor(e.Location)
	if err != nil {
		return term.Handle{}, err
	}
	return arena.PutCompound(term.NewAtom("error"), t, l)
}

// SyntaxError is an error that signifies a syntax error.
type SyntaxError struct {
	ImpDepAtom term.Atom
	Location   term.Functor
}

func (e *SyntaxError) Error() string {
	return fmt.Sprintf("syntax error: %s", e.ImpDepAtom)
}

func (e *SyntaxError) Term(arena *term.Arena) (term.Handle, error) {
	i, err := arena.PutAtom(e.ImpDepAtom)
	if err != nil {
		return term.Handle{}, err
	}
	t, err := arena.PutCompound(term.NewAtom("syntax_error"), i)
	if err != nil {
		return term.Handle{}, err
	}
	l, err := arena.PutFunctor(e.Location)
	if err != nil {
		return term.Handle{}, err
	}
	return arena.PutCompound(term.NewAtom("error"), t, l)
}

// UninstantiationError is an error that signifies a term is non-variable.
type UninstantiationError struct {
	Culprit  syntax.Serialized
	Location term.Functor
}

func (e *UninstantiationError) Error() string {
	return "uninstantiation error"
}

func (e *UninstantiationError) Term(arena *term.Arena) (term.Handle, error) {
	c, err := syntax.Deserialize(arena, e.Culprit)
	if err != nil {
		return term.Handle{}, err
	}
	t, err := arena.PutCompound(term.NewAtom("uninstantiation_error"), c)
	if err != nil {
		return term.Handle{}, err
	}
	l, err := arena.PutFunctor(e.Location)
	if err != nil {
		return term.Handle{}, err
	}
	return arena.PutCompound(term.NewAtom("error"), t, l)
}

type EvaluationError struct {
	Cause    error
	Location term.Functor
}

func (e *EvaluationError) Error() string {
	return fmt.Sprintf("evaluation error: %s, location = %s", e.Cause, e.Location)
}

func (e *EvaluationError) Term(arena *term.Arena) (term.Handle, error) {
	i, err := arena.PutAtom(term.NewAtom(e.Cause.Error()))
	if err != nil {
		return term.Handle{}, err
	}
	t, err := arena.PutCompound(term.NewAtom("evaluation_error"), i)
	if err != nil {
		return term.Handle{}, err
	}
	l, err := arena.PutFunctor(e.Location)
	if err != nil {
		return term.Handle{}, err
	}
	return arena.PutCompound(term.NewAtom("error"), t, l)
}
