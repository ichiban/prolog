package engine

import (
	"io"
)

// Term is a prolog term.
type Term interface {
	WriteTerm(w io.Writer, opts *WriteOptions, env *Env) error
	Compare(t Term, env *Env) int
}

// WriteOptions specify how the Term writes itself.
type WriteOptions struct {
	ignoreOps     bool
	quoted        bool
	variableNames map[Variable]Atom
	numberVars    bool

	ops         operators
	priority    Integer
	visited     map[termID]struct{}
	prefixMinus bool
	left, right operator
}

func (o WriteOptions) withQuoted(quoted bool) *WriteOptions {
	o.quoted = quoted
	return &o
}

func (o WriteOptions) withFreshVisited() *WriteOptions {
	visited := make(map[termID]struct{}, len(o.visited))
	for k, v := range o.visited {
		visited[k] = v
	}
	o.visited = visited
	return &o
}

func (o WriteOptions) withPriority(priority Integer) *WriteOptions {
	o.priority = priority
	return &o
}

func (o WriteOptions) withLeft(op operator) *WriteOptions {
	o.left = op
	return &o
}

func (o WriteOptions) withRight(op operator) *WriteOptions {
	o.right = op
	return &o
}

var defaultWriteOptions = WriteOptions{
	ops: operators{
		atomPlus: [_operatorClassLen]operator{
			operatorClassInfix: {priority: 500, specifier: operatorSpecifierYFX, name: atomPlus}, // for flag+value
		},
		atomSlash: [_operatorClassLen]operator{
			operatorClassInfix: {priority: 400, specifier: operatorSpecifierYFX, name: atomSlash}, // for principal functors
		},
	},
	variableNames: map[Variable]Atom{},
	priority:      1200,
}

// termIDer lets a Term which is not comparable per se return its termID for comparison.
type termIDer interface {
	termID() termID
}

// termID is an identifier for a Term.
type termID interface{}

// id returns a termID for the Term.
func id(t Term) termID {
	switch t := t.(type) {
	case termIDer:
		return t.termID()
	default:
		return t // Assuming it's comparable.
	}
}
