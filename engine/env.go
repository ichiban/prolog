package engine

import (
	"reflect"
	"strings"
)

const (
	varContext  = Variable("$context")
	rootContext = Atom("root")
)

type color uint8

const (
	red color = iota
	black
)

// Env is a mapping from variables to terms.
type Env struct {
	// basically, this is Red-Black tree from Purely Functional Data Structures by Okazaki.
	color       color
	left, right *Env
	binding
}

type binding struct {
	variable Variable
	value    Term
	// attributes?
}

var rootEnv = &Env{
	binding: binding{
		variable: varContext,
		value:    rootContext,
	},
}

// NewEnv creates an empty environment.
func NewEnv() *Env {
	return nil
}

// Lookup returns a term that the given variable is bound to.
func (e *Env) Lookup(k Variable) (Term, bool) {
	node := e
	if node == nil {
		node = rootEnv
	}
	for {
		if node == nil {
			return nil, false
		}
		switch {
		case k < node.variable:
			node = node.left
		case k > node.variable:
			node = node.right
		default:
			return node.value, true
		}
	}
}

// Bind adds a new entry to the environment.
func (e *Env) Bind(k Variable, v Term) *Env {
	node := e
	if node == nil {
		node = rootEnv
	}
	ret := *node.insert(k, v)
	ret.color = black
	return &ret
}

func (e *Env) insert(k Variable, v Term) *Env {
	if e == nil {
		return &Env{color: red, binding: binding{variable: k, value: v}}
	}
	switch {
	case k < e.variable:
		ret := *e
		ret.left = e.left.insert(k, v)
		ret.balance()
		return &ret
	case k > e.variable:
		ret := *e
		ret.right = e.right.insert(k, v)
		ret.balance()
		return &ret
	default:
		ret := *e
		ret.value = v
		return &ret
	}
}

func (e *Env) balance() {
	var (
		a, b, c, d *Env
		x, y, z    binding
	)
	switch {
	case e.left != nil && e.left.color == red:
		switch {
		case e.left.left != nil && e.left.left.color == red:
			a = e.left.left.left
			b = e.left.left.right
			c = e.left.right
			d = e.right
			x = e.left.left.binding
			y = e.left.binding
			z = e.binding
		case e.left.right != nil && e.left.right.color == red:
			a = e.left.left
			b = e.left.right.left
			c = e.left.right.right
			d = e.right
			x = e.left.binding
			y = e.left.right.binding
			z = e.binding
		default:
			return
		}
	case e.right != nil && e.right.color == red:
		switch {
		case e.right.left != nil && e.right.left.color == red:
			a = e.left
			b = e.right.left.left
			c = e.right.left.right
			d = e.right.right
			x = e.binding
			y = e.right.left.binding
			z = e.right.binding
		case e.right.right != nil && e.right.right.color == red:
			a = e.left
			b = e.right.left
			c = e.right.right.left
			d = e.right.right.right
			x = e.binding
			y = e.right.binding
			z = e.right.right.binding
		default:
			return
		}
	default:
		return
	}
	*e = Env{
		color:   red,
		left:    &Env{color: black, left: a, right: b, binding: x},
		right:   &Env{color: black, left: c, right: d, binding: z},
		binding: y,
	}
}

// Resolve follows the variable chain and returns the first non-variable term or the last free variable.
func (e *Env) Resolve(t Term) Term {
	var stop []Variable
	for t != nil {
		switch v := t.(type) {
		case Variable:
			for _, s := range stop {
				if v == s {
					return v
				}
			}
			ref, ok := e.Lookup(v)
			if !ok {
				return v
			}
			stop = append(stop, v)
			t = ref
		default:
			return v
		}
	}
	return nil
}

// Simplify trys to remove as many variables as possible from term t.
func (e *Env) Simplify(t Term) Term {
	return simplify(t, nil, e)
}

func simplify(t Term, simplified map[termID]Compound, env *Env) Term {
	if simplified == nil {
		simplified = map[termID]Compound{}
	}
	switch t := env.Resolve(t).(type) {
	case Compound:
		if c, ok := simplified[identifier(t)]; ok {
			return c
		}
		c := compound{
			functor: t.Functor(),
			args:    make([]Term, t.Arity()),
		}
		simplified[identifier(t)] = &c
		for i := 0; i < t.Arity(); i++ {
			c.args[i] = simplify(t.Arg(i), simplified, env)
		}
		return &c
	default:
		return t
	}
}

type variables []Variable

// FreeVariables extracts variables in the given terms.
func (e *Env) FreeVariables(ts ...Term) []Variable {
	var fvs variables
	for _, t := range ts {
		fvs = e.appendFreeVariables(fvs, t)
	}
	return fvs
}

func (e *Env) appendFreeVariables(fvs variables, t Term) variables {
	switch t := e.Resolve(t).(type) {
	case Variable:
		for _, v := range fvs {
			if v == t {
				return fvs
			}
		}
		return append(fvs, t)
	case Compound:
		for i := 0; i < t.Arity(); i++ {
			fvs = e.appendFreeVariables(fvs, t.Arg(i))
		}
	}
	return fvs
}

func (e *Env) Unify(x, y Term, occursCheck bool) (*Env, bool) {
	x, y = e.Resolve(x), e.Resolve(y)
	switch x := x.(type) {
	case Variable:
		switch {
		case x == y:
			return e, true
		case occursCheck && Contains(y, x, e):
			return e, false
		default:
			return e.Bind(x, y), true
		}
	case Compound:
		switch y := y.(type) {
		case Variable:
			return e.Unify(y, x, occursCheck)
		case Compound:
			if x.Functor() != y.Functor() {
				return e, false
			}
			if x.Arity() != y.Arity() {
				return e, false
			}
			var ok bool
			for i := 0; i < x.Arity(); i++ {
				e, ok = e.Unify(x.Arg(i), y.Arg(i), occursCheck)
				if !ok {
					return e, false
				}
			}
			return e, true
		default:
			return e, false
		}
	default: // atomic
		switch y := y.(type) {
		case Variable:
			return e.Unify(y, x, occursCheck)
		default:
			return e, x == y
		}
	}
}

// Order indicates ordering of two terms.
type Order int8

// Order is either =, <, or >.
const (
	OrderEqual Order = iota
	OrderLess
	OrderGreater
)

func (o Order) GoString() string {
	return string(o.Term().(Atom))
}

// Term returns the Atom representation of Order.
func (o Order) Term() Term {
	return [...]Term{
		OrderEqual:   Atom("="),
		OrderLess:    Atom("<"),
		OrderGreater: Atom(">"),
	}[o]
}

// Compare compares two terms.
func (e *Env) Compare(x, y Term) Order {
	x, y = e.Resolve(x), e.Resolve(y)
	switch x := x.(type) {
	case Variable:
		return e.compareVariable(x, y)
	case Float:
		return e.compareFloat(x, y)
	case Integer:
		return e.compareInteger(x, y)
	case Atom:
		return e.compareAtom(x, y)
	case Compound:
		return e.compareCompound(x, y)
	default:
		return e.compareAny(x, y)
	}
}

func (e *Env) compareVariable(x Variable, y Term) Order {
	switch y := y.(type) {
	case Variable:
		switch d := strings.Compare(string(x), string(y)); {
		case d > 0:
			return OrderGreater
		case d < 0:
			return OrderLess
		default:
			return OrderEqual
		}
	default:
		return OrderLess
	}
}

func (e *Env) compareFloat(x Float, y Term) Order {
	switch y := y.(type) {
	case Variable:
		return OrderGreater
	case Float:
		switch {
		case x > y:
			return OrderGreater
		case x < y:
			return OrderLess
		default:
			return OrderEqual
		}
	default:
		return OrderLess
	}
}

func (e *Env) compareInteger(x Integer, y Term) Order {
	switch y := y.(type) {
	case Variable, Float:
		return OrderGreater
	case Integer:
		switch {
		case x > y:
			return OrderGreater
		case x < y:
			return OrderLess
		default:
			return OrderEqual
		}
	default:
		return OrderLess
	}
}

func (e *Env) compareAtom(x Atom, y Term) Order {
	switch y := y.(type) {
	case Variable, Float, Integer:
		return OrderGreater
	case Atom:
		switch d := strings.Compare(string(x), string(y)); {
		case d > 0:
			return OrderGreater
		case d < 0:
			return OrderLess
		default:
			return OrderEqual
		}
	default:
		return OrderLess
	}
}

func (e *Env) compareCompound(x Compound, y Term) Order {
	switch y := y.(type) {
	case Variable, Float, Integer, Atom:
		return OrderGreater
	case Compound:
		switch x, y := x.Arity(), y.Arity(); {
		case x > y:
			return OrderGreater
		case x < y:
			return OrderLess
		}

		if o := e.Compare(x.Functor(), y.Functor()); o != OrderEqual {
			return o
		}

		for i := 0; i < x.Arity(); i++ {
			if o := e.Compare(x.Arg(i), y.Arg(i)); o != OrderEqual {
				return o
			}
		}
		return OrderEqual
	default:
		return OrderLess
	}
}

func (e *Env) compareAny(x Term, y Term) Order {
	switch y := y.(type) {
	case Variable, Float, Integer, Atom, *compound:
		return OrderGreater
	default:
		return deepCompare(reflect.ValueOf(x), reflect.ValueOf(y))
	}
}

var deepCompareFn []func(x, y reflect.Value) Order

func deepCompare(x, y reflect.Value) Order {
	var tx, ty string
	if x != (reflect.Value{}) {
		tx = x.Type().String()
	}
	if y != (reflect.Value{}) {
		ty = y.Type().String()
	}

	switch {
	case tx > ty:
		return OrderGreater
	case tx < ty:
		return OrderLess
	}

	if deepCompareFn == nil {
		deepCompareFn = []func(x, y reflect.Value) Order{
			reflect.Bool:          deepCompareBool,
			reflect.Int:           deepCompareInt,
			reflect.Int8:          deepCompareInt,
			reflect.Int16:         deepCompareInt,
			reflect.Int32:         deepCompareInt,
			reflect.Int64:         deepCompareInt,
			reflect.Uint:          deepCompareUint,
			reflect.Uint8:         deepCompareUint,
			reflect.Uint16:        deepCompareUint,
			reflect.Uint32:        deepCompareUint,
			reflect.Uint64:        deepCompareUint,
			reflect.Uintptr:       deepCompareUint,
			reflect.Float32:       deepCompareFloat,
			reflect.Float64:       deepCompareFloat,
			reflect.Complex64:     deepCompareComplex,
			reflect.Complex128:    deepCompareComplex,
			reflect.Array:         deepCompareSlice,
			reflect.Chan:          deepComparePointer,
			reflect.Func:          deepComparePointer,
			reflect.Interface:     deepCompareInterface,
			reflect.Map:           deepCompareMap,
			reflect.Pointer:       deepComparePointer,
			reflect.Slice:         deepCompareSlice,
			reflect.String:        deepCompareString,
			reflect.Struct:        deepCompareStruct,
			reflect.UnsafePointer: deepComparePointer,
		}
	}

	if k := x.Kind(); int(k) < len(deepCompareFn) {
		if f := deepCompareFn[k]; f != nil {
			return f(x, y)
		}
	}
	return OrderEqual
}

func deepCompareBool(x, y reflect.Value) Order {
	switch x, y := x.Bool(), y.Bool(); {
	case x && !y:
		return OrderGreater
	case !x && y:
		return OrderLess
	default:
		return OrderEqual
	}
}

func deepCompareInt(x, y reflect.Value) Order {
	switch x, y := x.Int(), y.Int(); {
	case x > y:
		return OrderGreater
	case x < y:
		return OrderLess
	default:
		return OrderEqual
	}
}

func deepCompareUint(x, y reflect.Value) Order {
	switch x, y := x.Uint(), y.Uint(); {
	case x > y:
		return OrderGreater
	case x < y:
		return OrderLess
	default:
		return OrderEqual
	}
}

func deepCompareFloat(x, y reflect.Value) Order {
	switch x, y := x.Float(), y.Float(); {
	case x > y:
		return OrderGreater
	case x < y:
		return OrderLess
	default:
		return OrderEqual
	}
}

func deepCompareComplex(vx, vy reflect.Value) Order {
	x, y := vx.Complex(), vy.Complex()
	switch x, y := real(x), real(y); {
	case x > y:
		return OrderGreater
	case x < y:
		return OrderLess
	}
	switch x, y := imag(x), imag(y); {
	case x > y:
		return OrderGreater
	case x < y:
		return OrderLess
	default:
		return OrderEqual
	}
}

func deepCompareSlice(x, y reflect.Value) Order {
	lx, ly := x.Len(), y.Len()
	l := lx
	if l > ly {
		l = ly
	}
	for i := 0; i < l; i++ {
		if d := deepCompare(x.Index(i), y.Index(i)); d != 0 {
			return d
		}
	}
	switch {
	case lx > ly:
		return OrderGreater
	case lx < ly:
		return OrderLess
	default:
		return OrderEqual
	}
}

func deepComparePointer(x, y reflect.Value) Order {
	switch x, y := x.Pointer(), y.Pointer(); {
	case x > y:
		return OrderGreater
	case x < y:
		return OrderLess
	default:
		return OrderEqual
	}
}

func deepCompareInterface(x, y reflect.Value) Order {
	return deepCompare(x.Elem(), y.Elem())
}

func deepCompareMap(x, y reflect.Value) Order {
	iter := x.MapRange()
	for iter.Next() {
		if d := deepCompare(iter.Value(), y.MapIndex(iter.Key())); d != 0 {
			return d
		}
	}
	if x.Len() < y.Len() {
		return OrderLess
	}
	return OrderEqual
}

func deepCompareString(x, y reflect.Value) Order {
	switch x, y := x.String(), y.String(); {
	case x > y:
		return OrderGreater
	case x < y:
		return OrderLess
	default:
		return OrderEqual
	}
}

func deepCompareStruct(x, y reflect.Value) Order {
	for i := 0; i < x.NumField(); i++ {
		if d := deepCompare(x.Field(i), y.Field(i)); d != 0 {
			return d
		}
	}
	return OrderEqual
}
