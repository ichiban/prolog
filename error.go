package prolog

import (
	"context"
	"errors"
	"fmt"
)

var (
	// ErrInstantiation is an error that signifies a term is variable.
	ErrInstantiation = errors.New("instantiation error")
)

// ResourceError is an error that signifies lack of a resource.
type ResourceError struct {
	Resource string
}

func (e *ResourceError) Error() string {
	return fmt.Sprintf("insufficient resource: %s", e.Resource)
}

// RepresentationError is an error that signifies one of the implementation limits exceeded.
type RepresentationError struct {
	flag string
}

func (e *RepresentationError) Error() string {
	return fmt.Sprintf("implementation limit exceeded: %s", e.flag)
}

// SyntaxError is an error that signifies a syntax error.
type SyntaxError struct {
	impDepAtom string
}

func (e *SyntaxError) Error() string {
	return fmt.Sprintf("syntax error: %s", e.impDepAtom)
}

// TypeError is an error that signifies an incorrect type.
type TypeError struct {
	ValidType string
	Culprit   Term
}

func (e *TypeError) Error() string {
	return fmt.Sprintf("invalid type: expected %s, got %s", e.ValidType, e.Culprit)
}

// DomainError is an error that signifies an incorrect value.
type DomainError struct {
	ValidDomain string
	Culprit     Term
}

func (e *DomainError) Error() string {
	return fmt.Sprintf("invalid domain: expected %s, got %s", e.ValidDomain, e.Culprit)
}

// PermissionError is an error that signifies a disallowed operation.
type PermissionError struct {
	Operation      string
	PermissionType string
	Culprit        Term
}

func (e *PermissionError) Error() string {
	return fmt.Sprintf("disallowed operation %s on %s: %s", e.PermissionType, e.Operation, e.Culprit)
}

// ExistenceError is an error that signifies nonexistence of an object.
type ExistenceError struct {
	ObjectType string
	Culprit    Term
}

func (e *ExistenceError) Error() string {
	return fmt.Sprintf("%s does not exist: %s", e.ObjectType, e.Culprit)
}

// UninstantiationError is an error that signifies a term is non-variable.
type UninstantiationError struct {
	Culprit Term
}

func (u *UninstantiationError) Error() string {
	return fmt.Sprintf("uninstantiation error: %v", u.Culprit)
}

func Catch(ctx context.Context, proc *Processor, goal, catcher, recovery Term, k Promise) Promise {
	// TODO: Call goal and catch errors.
	return Eager(false, nil)
}
