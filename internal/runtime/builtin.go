package runtime

type BuiltinType int8

const (
	BuiltinTypeInHead BuiltinType = iota
	BuiltinTypeInline
	BuiltinTypeArithmetic0
	BuiltinTypeArithmetic1
)

type Builtin struct {
	Type BuiltinType
	// TODO: Implement the rest.
}
