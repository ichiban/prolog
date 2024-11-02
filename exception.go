package prolog

import (
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

// TypeError is an error that signifies an incorrect type.
type TypeError struct {
	ValidType string
	Culprit   Term
}

func (e *TypeError) Error() string {
	return fmt.Sprintf("invalid type: expected %s, got %s", e.ValidType, e.Culprit)
}

// UninstantiationError is an error that signifies a term is non-variable.
type UninstantiationError struct {
	Culprit Term
}

func (u *UninstantiationError) Error() string {
	return fmt.Sprintf("uninstantiation error: %v", u.Culprit)
}
