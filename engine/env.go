package engine

import (
	"reflect"
	"strings"
)

var varContext = NewNamedVariable("$context")

var rootContext = NewAtom("root")

type envKey int64

func newEnvKey(v Variable) envKey {
	// A new Variable is always bigger than the previous ones.
	// So, if we used the Variable itself as the key, insertions to the Env tree would be skewed to the right.
	k := envKey(v)
	if k/2 != 0 {
		k *= -1
	}
	return k
}

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
	key   envKey
	value Term
	// attributes?
}

var rootEnv = &Env{
	binding: binding{
		key:   newEnvKey(varContext),
		value: rootContext,
	},
}

// NewEnv creates an empty environment.
func NewEnv() *Env {
	return nil
}

// Lookup returns a term that the given variable is bound to.
func (e *Env) Lookup(v Variable) (Term, bool) {
	k := newEnvKey(v)

	node := e
	if node == nil {
		node = rootEnv
	}
	for {
		if node == nil {
			return nil, false
		}
		switch {
		case k < node.key:
			node = node.left
		case k > node.key:
			node = node.right
		default:
			return node.value, true
		}
	}
}

// Bind adds a new entry to the environment.
func (e *Env) Bind(v Variable, t Term) *Env {
	k := newEnvKey(v)

	node := e
	if node == nil {
		node = rootEnv
	}
	ret := *node.insert(k, t)
	ret.color = black
	return &ret
}

func (e *Env) insert(k envKey, v Term) *Env {
	if e == nil {
		return &Env{color: red, binding: binding{key: k, value: v}}
	}
	switch {
	case k < e.key:
		ret := *e
		ret.left = e.left.insert(k, v)
		ret.balance()
		return &ret
	case k > e.key:
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
	case charList, codeList:
		return t
	case list:
		if c, ok := simplified[id(t)]; ok {
			return c
		}
		l := make(list, len(t))
		simplified[id(t)] = l
		for i, e := range t {
			l[i] = simplify(e, simplified, env)
		}
		return l
	case Compound:
		if c, ok := simplified[id(t)]; ok {
			return c
		}
		c := compound{
			functor: t.Functor(),
			args:    make([]Term, t.Arity()),
		}
		simplified[id(t)] = &c
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
		case occursCheck && contains(y, x, e):
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

func contains(t, s Term, env *Env) bool {
	switch t := t.(type) {
	case Variable:
		if t == s {
			return true
		}
		ref, ok := env.Lookup(t)
		if !ok {
			return false
		}
		return contains(ref, s, env)
	case Compound:
		if s, ok := s.(Atom); ok && t.Functor() == s {
			return true
		}
		for i := 0; i < t.Arity(); i++ {
			if contains(t.Arg(i), s, env) {
				return true
			}
		}
		return false
	default:
		return t == s
	}
}

// Compare compares two terms and returns -1, 0, or +1.
func (e *Env) Compare(x, y Term) int {
	x, y = e.Resolve(x), e.Resolve(y)
	switch x := x.(type) {
	case Variable:
		return e.compareVariable(x, y)
	case Float:
		return e.compareFloat(x, y)
	case Integer:
		return e.compareInteger(x, y)
	case Atom:
		return e.compareNewAtom(x, y)
	case Compound:
		return e.compareCompound(x, y)
	default:
		return e.compareAny(x, y)
	}
}

func (e *Env) compareVariable(x Variable, y Term) int {
	switch y := y.(type) {
	case Variable:
		return strings.Compare(x.String(), y.String())
	default:
		return -1
	}
}

func (e *Env) compareFloat(x Float, y Term) int {
	switch y := y.(type) {
	case Variable:
		return 1
	case Float:
		switch {
		case x > y:
			return 1
		case x < y:
			return -1
		default:
			return 0
		}
	default:
		return -1
	}
}

func (e *Env) compareInteger(x Integer, y Term) int {
	switch y := y.(type) {
	case Variable, Float:
		return 1
	case Integer:
		switch {
		case x > y:
			return 1
		case x < y:
			return -1
		default:
			return 0
		}
	default:
		return -1
	}
}

func (e *Env) compareNewAtom(x Atom, y Term) int {
	switch y := y.(type) {
	case Variable, Float, Integer:
		return 1
	case Atom:
		switch d := strings.Compare(x.String(), y.String()); {
		case d > 0:
			return 1
		case d < 0:
			return -1
		default:
			return 0
		}
	default:
		return -1
	}
}

func (e *Env) compareCompound(x Compound, y Term) int {
	switch y := y.(type) {
	case Variable, Float, Integer, Atom:
		return 1
	case Compound:
		switch x, y := x.Arity(), y.Arity(); {
		case x > y:
			return 1
		case x < y:
			return -1
		}

		if o := e.Compare(x.Functor(), y.Functor()); o != 0 {
			return o
		}

		for i := 0; i < x.Arity(); i++ {
			if o := e.Compare(x.Arg(i), y.Arg(i)); o != 0 {
				return o
			}
		}
		return 0
	default:
		return -1
	}
}

func (e *Env) compareAny(x Term, y Term) int {
	switch y := y.(type) {
	case Variable, Float, Integer, Atom, *compound:
		return 1
	default:
		return deepCompare(reflect.ValueOf(x), reflect.ValueOf(y))
	}
}

var deepCompareFn []func(x, y reflect.Value) int

func deepCompare(x, y reflect.Value) int {
	var tx, ty string
	if x != (reflect.Value{}) {
		tx = x.Type().String()
	}
	if y != (reflect.Value{}) {
		ty = y.Type().String()
	}

	switch {
	case tx > ty:
		return 1
	case tx < ty:
		return -1
	}

	if deepCompareFn == nil {
		deepCompareFn = []func(x, y reflect.Value) int{
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
	return 0
}

func deepCompareBool(x, y reflect.Value) int {
	switch x, y := x.Bool(), y.Bool(); {
	case x && !y:
		return 1
	case !x && y:
		return -1
	default:
		return 0
	}
}

func deepCompareInt(x, y reflect.Value) int {
	switch x, y := x.Int(), y.Int(); {
	case x > y:
		return 1
	case x < y:
		return -1
	default:
		return 0
	}
}

func deepCompareUint(x, y reflect.Value) int {
	switch x, y := x.Uint(), y.Uint(); {
	case x > y:
		return 1
	case x < y:
		return -1
	default:
		return 0
	}
}

func deepCompareFloat(x, y reflect.Value) int {
	switch x, y := x.Float(), y.Float(); {
	case x > y:
		return 1
	case x < y:
		return -1
	default:
		return 0
	}
}

func deepCompareComplex(vx, vy reflect.Value) int {
	x, y := vx.Complex(), vy.Complex()
	switch x, y := real(x), real(y); {
	case x > y:
		return 1
	case x < y:
		return -1
	}
	switch x, y := imag(x), imag(y); {
	case x > y:
		return 1
	case x < y:
		return -1
	default:
		return 0
	}
}

func deepCompareSlice(x, y reflect.Value) int {
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
		return 1
	case lx < ly:
		return -1
	default:
		return 0
	}
}

func deepComparePointer(x, y reflect.Value) int {
	switch x, y := x.Pointer(), y.Pointer(); {
	case x > y:
		return 1
	case x < y:
		return -1
	default:
		return 0
	}
}

func deepCompareInterface(x, y reflect.Value) int {
	return deepCompare(x.Elem(), y.Elem())
}

func deepCompareMap(x, y reflect.Value) int {
	iter := x.MapRange()
	for iter.Next() {
		if d := deepCompare(iter.Value(), y.MapIndex(iter.Key())); d != 0 {
			return d
		}
	}
	if x.Len() < y.Len() {
		return -1
	}
	return 0
}

func deepCompareString(x, y reflect.Value) int {
	return strings.Compare(x.String(), y.String())
}

func deepCompareStruct(x, y reflect.Value) int {
	for i := 0; i < x.NumField(); i++ {
		if d := deepCompare(x.Field(i), y.Field(i)); d != 0 {
			return d
		}
	}
	return 0
}
