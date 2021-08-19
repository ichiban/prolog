package term

import (
	"bytes"
	"fmt"
	"io"
	"strings"
	"sync"
)

// Variable is a prolog variable.
type Variable string

var (
	varCounter = 0
	varMutex   sync.Mutex
)

const anonVarPrefix = "_\u200b"

func NewVariable() Variable {
	varMutex.Lock()
	defer varMutex.Unlock()

	varCounter++
	return Variable(fmt.Sprintf("%s%d", anonVarPrefix, varCounter))
}

func (v Variable) Anonymous() bool {
	return strings.HasPrefix(string(v), anonVarPrefix)
}

func (v Variable) String() string {
	var buf bytes.Buffer
	_ = v.WriteTerm(&buf, DefaultWriteTermOptions, nil)
	return buf.String()
}

// WriteTerm writes the variable into w.
func (v Variable) WriteTerm(w io.Writer, opts WriteTermOptions, env Env) error {
	ref, ok := env.Lookup(v)
	if !ok && opts.Descriptive {
		if v != "" {
			if _, err := fmt.Fprintf(w, "%s = ", v); err != nil {
				return err
			}
		}
		return ref.WriteTerm(w, opts, env)
	}
	_, err := fmt.Fprint(w, string(v))
	return err
}

// Unify unifies the variable with t.
func (v Variable) Unify(t Interface, occursCheck bool, env *Env) bool {
	t = env.Resolve(t)
	if occursCheck && Contains(t, v, env) {
		return false
	}
	if ref, ok := env.Lookup(v); ok {
		return ref.Unify(t, occursCheck, env)
	}
	if w, ok := t.(Variable); ok {
		if _, ok := env.Lookup(w); !ok {
			t = NewVariable()
			*env = append(*env, Binding{
				Variable: w,
				Value:    t,
			})
		}
	}
	*env = append(*env, Binding{
		Variable: v,
		Value:    t,
	})
	return true
}
