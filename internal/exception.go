package internal

import (
	"errors"
	"fmt"
)

var (
	ErrInstantiation = errors.New("instantiate error")
)

// Exception is a general error backed by a term, most likely generated with throw/1.
type Exception Term

func (e Exception) Error() string {
	return fmt.Sprintf("term error: %d", e)
}

type TypeError struct {
	Type    Atom
	Culprit Term
}

func (e *TypeError) Error() string {
	return fmt.Sprintf("type error: type=%d, culprit=%d", e.Type, e.Culprit)
}

type DomainError struct {
	Domain  Atom
	Culprit Term
}

func (e *DomainError) Error() string {
	return fmt.Sprintf("domain error: domain=%d, culprit=%d", e.Domain, e.Culprit)
}

type ExistenceError struct {
	ObjectType Atom
	Culprit    Term
}

func (e *ExistenceError) Error() string {
	return fmt.Sprintf("existence error: objectType=%d, culprit=%d", e.ObjectType, e.Culprit)
}

type PermissionError struct {
	Operation      Atom
	PermissionType Atom
	Culprit        Term
}

func (p *PermissionError) Error() string {
	return fmt.Sprintf("permission error: operation=%d, permission_type=%d, culprit=%d", p.Operation, p.PermissionType, p.Culprit)
}

type RepresentationError struct {
	Limit Atom
}

type ResourceError struct {
	Resource Atom
}

type EvaluationError struct {
	Error Term // Not sure what comes here.
}
