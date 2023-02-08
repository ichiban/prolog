package engine

var varContext = NewVariable()

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

// lookup returns a term that the given variable is bound to.
func (e *Env) lookup(v Variable) (Term, bool) {
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

// bind adds a new entry to the environment.
func (e *Env) bind(v Variable, t Term) *Env {
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
			ref, ok := e.lookup(v)
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

// simplify trys to remove as many variables as possible from term t.
func (e *Env) simplify(t Term) Term {
	return simplify(t, nil, e)
}

func simplify(t Term, simplified map[termID]Compound, env *Env) Term {
	if simplified == nil {
		simplified = map[termID]Compound{}
	}
	t = env.Resolve(t)
	if c, ok := simplified[id(t)]; ok {
		return c
	}
	switch t := t.(type) {
	case charList, codeList:
		return t
	case list:
		l := make(list, len(t))
		simplified[id(t)] = l
		for i, e := range t {
			l[i] = simplify(e, simplified, env)
		}
		return l
	case *partial:
		var p partial
		simplified[id(t)] = &p
		p.Compound = simplify(t.Compound, simplified, env).(Compound)
		tail := simplify(*t.tail, simplified, env)
		p.tail = &tail
		return &p
	case Compound:
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

// freeVariables extracts variables in the given Term.
func (e *Env) freeVariables(t Term) []Variable {
	return e.appendFreeVariables(nil, t)
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

// Unify unifies 2 terms.
func (e *Env) Unify(x, y Term) (*Env, bool) {
	return e.unify(x, y, false)
}

func (e *Env) unifyWithOccursCheck(x, y Term) (*Env, bool) {
	return e.unify(x, y, true)
}

func (e *Env) unify(x, y Term, occursCheck bool) (*Env, bool) {
	x, y = e.Resolve(x), e.Resolve(y)
	switch x := x.(type) {
	case Variable:
		switch {
		case x == y:
			return e, true
		case occursCheck && contains(y, x, e):
			return e, false
		default:
			return e.bind(x, y), true
		}
	case Compound:
		switch y := y.(type) {
		case Variable:
			return e.unify(y, x, occursCheck)
		case Compound:
			if x.Functor() != y.Functor() {
				return e, false
			}
			if x.Arity() != y.Arity() {
				return e, false
			}
			var ok bool
			for i := 0; i < x.Arity(); i++ {
				e, ok = e.unify(x.Arg(i), y.Arg(i), occursCheck)
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
			return e.unify(y, x, occursCheck)
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
		ref, ok := env.lookup(t)
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
