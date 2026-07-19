package runtime

import (
	"errors"
	"fmt"

	"github.com/ichiban/prolog/v2/internal/term"
)

type Termer interface {
	Term(arena *term.Arena) (term.Handle, error)
}

type ErrorContext struct {
	Location term.Functor
	Message  string
}

func (c *ErrorContext) Term(arena *term.Arena) (term.Handle, error) {
	var (
		l, m term.Handle
		err  error
	)
	if c.Location == 0 {
		l, err = arena.PutVariable()
		if err != nil {
			return term.Handle{}, err
		}
	} else {
		n, err := arena.PutAtom(c.Location.Name())
		if err != nil {
			return term.Handle{}, err
		}
		a, err := arena.PutInteger(int64(c.Location.Arity()))
		if err != nil {
			return term.Handle{}, err
		}
		l, err = arena.PutCompound(term.NewAtomRune('/'), n, a)
		if err != nil {
			return term.Handle{}, err
		}
	}
	if c.Message == "" {
		return l, nil
	} else {
		m, err = arena.PutCharList(c.Message)
		if err != nil {
			return term.Handle{}, err
		}
	}
	return arena.PutCompound(term.NewAtom("context"), l, m)
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
	ErrorContext
}

func (e *InstantiationError) Error() string {
	return fmt.Sprintf("instantiation error")
}

func (e *InstantiationError) Term(arena *term.Arena) (term.Handle, error) {
	t, err := arena.PutAtom(term.NewAtom("instantiation_error"))
	if err != nil {
		return term.Handle{}, err
	}
	c, err := e.ErrorContext.Term(arena)
	if err != nil {
		return term.Handle{}, err
	}
	return arena.PutCompound(term.NewAtom("error"), t, c)
}

// TypeError is an error that signifies an incorrect type.
type TypeError struct {
	ErrorContext
	ValidType string
	Culprit   term.Handle
}

func (e *TypeError) Error() string {
	return fmt.Sprintf("invalid type: expected %s", e.ValidType)
}

func (e *TypeError) Term(arena *term.Arena) (term.Handle, error) {
	v, err := arena.PutAtom(term.NewAtom(e.ValidType))
	if err != nil {
		return term.Handle{}, err
	}
	t, err := arena.PutCompound(term.NewAtom("type_error"), v, e.Culprit)
	if err != nil {
		return term.Handle{}, err
	}
	c, err := e.ErrorContext.Term(arena)
	if err != nil {
		return term.Handle{}, err
	}
	return arena.PutCompound(term.NewAtom("error"), t, c)
}

// DomainError is an error that signifies an incorrect value.
type DomainError struct {
	ErrorContext
	Arena       *term.Arena
	ValidDomain string
	Culprit     term.Handle
}

func (e *DomainError) Error() string {
	return fmt.Sprintf("invalid domain: expected %s", e.ValidDomain)
}

func (e *DomainError) Term(arena *term.Arena) (term.Handle, error) {
	v, err := arena.PutAtom(term.NewAtom(e.ValidDomain))
	if err != nil {
		return term.Handle{}, err
	}
	t, err := arena.PutCompound(term.NewAtom("domain_error"), v, e.Culprit)
	if err != nil {
		return term.Handle{}, err
	}
	c, err := e.ErrorContext.Term(arena)
	if err != nil {
		return term.Handle{}, err
	}
	return arena.PutCompound(term.NewAtom("error"), t, c)
}

// ExistenceError is an error that signifies nonexistence of an object.
type ExistenceError struct {
	ErrorContext
	ObjectType string
	Culprit    term.Handle
}

func (e *ExistenceError) Error() string {
	return fmt.Sprintf("%s does not exist", e.ObjectType)
}

func (e *ExistenceError) Term(arena *term.Arena) (term.Handle, error) {
	o, err := arena.PutAtom(term.NewAtom(e.ObjectType))
	if err != nil {
		return term.Handle{}, err
	}
	t, err := arena.PutCompound(term.NewAtom("existence_error"), o, e.Culprit)
	if err != nil {
		return term.Handle{}, err
	}
	c, err := e.ErrorContext.Term(arena)
	if err != nil {
		return term.Handle{}, err
	}
	return arena.PutCompound(term.NewAtom("error"), t, c)
}

// PermissionError is an error that signifies a disallowed operation.
type PermissionError struct {
	ErrorContext
	Operation      string
	PermissionType string
	Culprit        term.Handle
}

func (e *PermissionError) Error() string {
	return fmt.Sprintf("disallowed operation %s on %s", e.Operation, e.PermissionType)
}

func (e *PermissionError) Term(arena *term.Arena) (term.Handle, error) {
	o, err := arena.PutAtom(term.NewAtom(e.Operation))
	if err != nil {
		return term.Handle{}, err
	}
	p, err := arena.PutAtom(term.NewAtom(e.PermissionType))
	if err != nil {
		return term.Handle{}, err
	}
	t, err := arena.PutCompound(term.NewAtom("permission_error"), o, p, e.Culprit)
	if err != nil {
		return term.Handle{}, err
	}
	c, err := e.ErrorContext.Term(arena)
	if err != nil {
		return term.Handle{}, err
	}
	return arena.PutCompound(term.NewAtom("error"), t, c)
}

// RepresentationError is an error that signifies one of the implementation limits exceeded.
type RepresentationError struct {
	ErrorContext
	flag string
}

func (e *RepresentationError) Error() string {
	return fmt.Sprintf("implementation limit exceeded: %s", e.flag)
}

func (e *RepresentationError) Term(arena *term.Arena) (term.Handle, error) {
	f, err := arena.PutAtom(term.NewAtom(e.flag))
	if err != nil {
		return term.Handle{}, err
	}
	t, err := arena.PutCompound(term.NewAtom("representation_error"), f)
	if err != nil {
		return term.Handle{}, err
	}
	c, err := e.ErrorContext.Term(arena)
	if err != nil {
		return term.Handle{}, err
	}
	return arena.PutCompound(term.NewAtom("error"), t, c)
}

// TODO: evaluation_error

// ResourceError is an error that signifies lack of a resource.
type ResourceError struct {
	ErrorContext
	Resource string
}

func (e *ResourceError) Error() string {
	return fmt.Sprintf("insufficient resource: %s", e.Resource)
}

func (e *ResourceError) Term(arena *term.Arena) (term.Handle, error) {
	r, err := arena.PutAtom(term.NewAtom(e.Resource))
	if err != nil {
		return term.Handle{}, err
	}
	t, err := arena.PutCompound(term.NewAtom("resource_error"), r)
	if err != nil {
		return term.Handle{}, err
	}
	c, err := e.ErrorContext.Term(arena)
	if err != nil {
		return term.Handle{}, err
	}
	return arena.PutCompound(term.NewAtom("error"), t, c)
}

// SyntaxError is an error that signifies a syntax error.
type SyntaxError struct {
	ErrorContext
	impDepAtom string
}

func (e *SyntaxError) Error() string {
	return fmt.Sprintf("syntax error: %s", e.impDepAtom)
}

func (e *SyntaxError) Term(arena *term.Arena) (term.Handle, error) {
	i, err := arena.PutAtom(term.NewAtom(e.impDepAtom))
	if err != nil {
		return term.Handle{}, err
	}
	t, err := arena.PutCompound(term.NewAtom("syntax_error"), i)
	if err != nil {
		return term.Handle{}, err
	}
	c, err := e.ErrorContext.Term(arena)
	if err != nil {

	}
	return arena.PutCompound(term.NewAtom("error"), t, c)
}

// UninstantiationError is an error that signifies a term is non-variable.
type UninstantiationError struct {
	ErrorContext
	Culprit term.Handle
}

func (e *UninstantiationError) Error() string {
	return "uninstantiation error"
}

func (e *UninstantiationError) Term(arena *term.Arena) (term.Handle, error) {
	t, err := arena.PutCompound(term.NewAtom("uninstantiation_error"), e.Culprit)
	if err != nil {
		return term.Handle{}, err
	}
	c, err := e.ErrorContext.Term(arena)
	if err != nil {
		return term.Handle{}, err
	}
	return arena.PutCompound(term.NewAtom("error"), t, c)
}
