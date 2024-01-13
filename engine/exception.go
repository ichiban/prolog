package engine

import (
	"bytes"
)

// Exception is an error represented by a prolog term.
type Exception struct {
	term Term
}

// NewException creates an Exception from a copy of the given Term.
func NewException(term Term, env *Env) Exception {
	c, err := renamedCopy(term, nil, env)
	if err != nil {
		return err.(Exception) // Must be error(resource_error(memory), _).
	}
	return Exception{term: c}
}

// Term returns the underlying Term of the Exception.
func (e Exception) Term() Term {
	return e.term
}

func (e Exception) Error() string {
	var buf bytes.Buffer
	_ = e.term.WriteTerm(&buf, &defaultWriteOptions, nil)
	return buf.String()
}

// InstantiationError returns an instantiation error exception.
func InstantiationError(env *Env) Exception {
	return NewException(atomError.Apply(atomInstantiationError, varContext), env)
}

// validType is the correct type for an argument or one of its components.
type validType uint8

const (
	validTypeAtom validType = iota
	validTypeAtomic
	validTypeByte
	validTypeCallable
	validTypeCharacter
	validTypeCompound
	validTypeEvaluable
	validTypeInByte
	validTypeInCharacter
	validTypeInteger
	validTypeList
	validTypeNumber
	validTypePredicateIndicator
	validTypePair
	validTypeFloat
)

var validTypeAtoms = [...]Atom{
	validTypeAtom:               atomAtom,
	validTypeAtomic:             atomAtomic,
	validTypeByte:               atomByte,
	validTypeCallable:           atomCallable,
	validTypeCharacter:          atomCharacter,
	validTypeCompound:           atomCompound,
	validTypeEvaluable:          atomEvaluable,
	validTypeInByte:             atomInByte,
	validTypeInCharacter:        atomInCharacter,
	validTypeInteger:            atomInteger,
	validTypeList:               atomList,
	validTypeNumber:             atomNumber,
	validTypePredicateIndicator: atomPredicateIndicator,
	validTypePair:               atomPair,
	validTypeFloat:              atomFloat,
}

// Term returns an Atom for the validType.
func (t validType) Term() Term {
	return validTypeAtoms[t]
}

// TypeError creates a new type error exception.
func TypeError(typ, culprit Term, env *Env) Exception {
	return NewException(atomError.Apply(atomTypeError.Apply(typ, culprit), varContext), env)
}

// typeError creates a new type error exception.
func typeError(validType validType, culprit Term, env *Env) Exception {
	return TypeError(validType.Term(), culprit, env)
}

// validDomain is the domain which the procedure defines.
type validDomain uint8

const (
	validDomainCharacterCodeList validDomain = iota
	validDomainCloseOption
	validDomainFlagValue
	validDomainIOMode
	validDomainNonEmptyList
	validDomainNotLessThanZero
	validDomainOperatorPriority
	validDomainOperatorSpecifier
	validDomainPrologFlag
	validDomainReadOption
	validDomainSourceSink
	validDomainStream
	validDomainStreamOption
	validDomainStreamOrAlias
	validDomainStreamPosition
	validDomainStreamProperty
	validDomainWriteOption

	validDomainOrder
	validDomainMetaArgumentSpecifier
)

var validDomainAtoms = [...]Atom{
	validDomainCharacterCodeList:     atomCharacterCodeList,
	validDomainCloseOption:           atomCloseOption,
	validDomainFlagValue:             atomFlagValue,
	validDomainIOMode:                atomIOMode,
	validDomainNonEmptyList:          atomNonEmptyList,
	validDomainNotLessThanZero:       atomNotLessThanZero,
	validDomainOperatorPriority:      atomOperatorPriority,
	validDomainOperatorSpecifier:     atomOperatorSpecifier,
	validDomainPrologFlag:            atomPrologFlag,
	validDomainReadOption:            atomReadOption,
	validDomainSourceSink:            atomSourceSink,
	validDomainStream:                atomStream,
	validDomainStreamOption:          atomStreamOption,
	validDomainStreamOrAlias:         atomStreamOrAlias,
	validDomainStreamPosition:        atomStreamPosition,
	validDomainStreamProperty:        atomStreamProperty,
	validDomainWriteOption:           atomWriteOption,
	validDomainOrder:                 atomOrder,
	validDomainMetaArgumentSpecifier: atomMetaArgumentSpecifier,
}

// Term returns an Atom for the validDomain.
func (vd validDomain) Term() Term {
	return validDomainAtoms[vd]
}

// DomainError creates a new domain error exception.
func DomainError(domain, culprit Term, env *Env) Exception {
	return NewException(atomError.Apply(atomDomainError.Apply(domain, culprit), varContext), env)
}

// domainError creates a new domain error exception.
func domainError(validDomain validDomain, culprit Term, env *Env) Exception {
	return DomainError(validDomain.Term(), culprit, env)
}

// objectType is the object on which an operation is to be performed.
type objectType uint8

const (
	objectTypeProcedure objectType = iota
	objectTypeSourceSink
	objectTypeStream
)

var objectTypeAtoms = [...]Atom{
	objectTypeProcedure:  atomProcedure,
	objectTypeSourceSink: atomSourceSink,
	objectTypeStream:     atomStream,
}

// Term returns an Atom for the objectType.
func (ot objectType) Term() Term {
	return objectTypeAtoms[ot]
}

// existenceError creates a new existence error exception.
func existenceError(objectType objectType, culprit Term, env *Env) Exception {
	return NewException(atomError.Apply(atomExistenceError.Apply(objectType.Term(), culprit), varContext), env)
}

// operation is the operation to be performed.
type operation uint8

const (
	operationAccess operation = iota
	operationCreate
	operationInput
	operationModify
	operationOpen
	operationOutput
	operationReposition
)

var operationAtoms = [...]Atom{
	operationAccess:     atomAccess,
	operationCreate:     atomCreate,
	operationInput:      atomInput,
	operationModify:     atomModify,
	operationOpen:       atomOpen,
	operationOutput:     atomOutput,
	operationReposition: atomReposition,
}

// Term returns an Atom for the operation.
func (o operation) Term() Term {
	return operationAtoms[o]
}

// permissionType is the type to which the operation is not permitted to perform.
type permissionType uint8

const (
	permissionTypeBinaryStream permissionType = iota
	permissionTypeFlag
	permissionTypeOperator
	permissionTypePastEndOfStream
	permissionTypePrivateProcedure
	permissionTypeStaticProcedure
	permissionTypeSourceSink
	permissionTypeStream
	permissionTypeTextStream
)

var permissionTypeAtoms = [...]Atom{
	permissionTypeBinaryStream:     atomBinaryStream,
	permissionTypeFlag:             atomFlag,
	permissionTypeOperator:         atomOperator,
	permissionTypePastEndOfStream:  atomPastEndOfStream,
	permissionTypePrivateProcedure: atomPrivateProcedure,
	permissionTypeStaticProcedure:  atomStaticProcedure,
	permissionTypeSourceSink:       atomSourceSink,
	permissionTypeStream:           atomStream,
	permissionTypeTextStream:       atomTextStream,
}

// Term returns an Atom for the permissionType.
func (pt permissionType) Term() Term {
	return permissionTypeAtoms[pt]
}

// permissionError creates a new permission error exception.
func permissionError(operation operation, permissionType permissionType, culprit Term, env *Env) Exception {
	return NewException(atomError.Apply(atomPermissionError.Apply(operation.Term(), permissionType.Term(), culprit), varContext), env)
}

// flag is an implementation defined limit.
type flag uint8

const (
	flagCharacter flag = iota
	flagCharacterCode
	flagInCharacterCode
	flagMaxArity
	flagMaxInteger
	flagMinInteger
)

var flagAtoms = [...]Atom{
	flagCharacter:       atomCharacter,
	flagCharacterCode:   atomCharacterCode,
	flagInCharacterCode: atomInCharacterCode,
	flagMaxArity:        atomMaxArity,
	flagMaxInteger:      atomMaxInteger,
	flagMinInteger:      atomMinInteger,
}

// Term returns an Atom for the flag.
func (f flag) Term() Term {
	return flagAtoms[f]
}

// representationError creates a new representation error exception.
func representationError(limit flag, env *Env) Exception {
	return NewException(atomError.Apply(atomRepresentationError.Apply(limit.Term()), varContext), env)
}

// resource is a resource required to complete execution.
type resource uint8

// resource is one of these values.
const (
	resourceFiniteMemory resource = iota

	resourceMemory
)

var resourceAtoms = [...]Atom{
	resourceFiniteMemory: atomFiniteMemory,
	resourceMemory:       atomMemory,
}

// Term returns an Atom for the resource.
func (r resource) Term() Term {
	return resourceAtoms[r]
}

// resourceError creates a new resource error exception.
func resourceError(resource resource, env *Env) Exception {
	// We can't call renamedCopy() since it can lead th resource_error(memory).
	return Exception{term: atomError.Apply(atomResourceError.Apply(resource.Term()), env.Resolve(varContext))}
}

// syntaxError creates a new syntax error exception.
func syntaxError(err error, env *Env) Exception {
	return NewException(atomError.Apply(atomSyntaxError.Apply(NewAtom(err.Error())), varContext), env)
}

// exceptionalValue is an evaluable functor's result which is not a number.
type exceptionalValue uint8

const (
	exceptionalValueFloatOverflow exceptionalValue = iota
	exceptionalValueIntOverflow
	exceptionalValueUnderflow
	exceptionalValueZeroDivisor
	exceptionalValueUndefined
)

func (ev exceptionalValue) Error() string {
	return ev.Term().(Atom).String()
}

var exceptionalValueAtoms = [...]Atom{
	exceptionalValueFloatOverflow: atomFloatOverflow,
	exceptionalValueIntOverflow:   atomIntOverflow,
	exceptionalValueUnderflow:     atomUnderflow,
	exceptionalValueZeroDivisor:   atomZeroDivisor,
	exceptionalValueUndefined:     atomUndefined,
}

// Term returns an Atom for the exceptionalValue.
func (ev exceptionalValue) Term() Term {
	return exceptionalValueAtoms[ev]
}

// evaluationError creates a new evaluation error exception.
func evaluationError(ev exceptionalValue, env *Env) Exception {
	return NewException(atomError.Apply(atomEvaluationError.Apply(ev.Term()), varContext), env)
}

func mustBeAtom(t Term, env *Env) (Atom, error) {
	switch t := t.(type) {
	case Variable:
		return 0, InstantiationError(env)
	case Atom:
		return t, nil
	default:
		return 0, typeError(validTypeAtom, t, env)
	}
}

func mustBePI(t Term, env *Env) (procedureIndicator, error) {
	var pi procedureIndicator
	switch t := t.(type) {
	case Variable:
		return pi, InstantiationError(env)
	case Compound:
		if t.Functor() != atomSlash || t.Arity() != 2 {
			return pi, typeError(validTypePredicateIndicator, t, env)
		}
		switch n := t.Arg(0).(type) {
		case Variable:
			return pi, InstantiationError(env)
		case Atom:
			pi.name = n
		default:
			return pi, typeError(validTypePredicateIndicator, t, env)
		}

		switch a := t.Arg(1).(type) {
		case Variable:
			return pi, InstantiationError(env)
		case Integer:
			pi.arity = a
		default:
			return pi, typeError(validTypePredicateIndicator, t, env)
		}
	default:
		return pi, typeError(validTypePredicateIndicator, t, env)
	}
	return pi, nil
}
