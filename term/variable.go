package term

import (
	"bytes"
	"fmt"
	"io"
	"regexp"
)

// Variable is a prolog variable.
type Variable string

var varCounter = 0

func NewVariable() Variable {
	varCounter++
	return Variable(fmt.Sprintf("_%d", varCounter))
}

var anonVarPattern = regexp.MustCompile(`\A_\d+\z`)

func (v Variable) Anonymous() bool {
	return anonVarPattern.MatchString(string(v))
}

func (v Variable) String() string {
	var buf bytes.Buffer
	_ = v.WriteTerm(&buf, DefaultWriteTermOptions, nil)
	return buf.String()
}

// WriteTerm writes the variable into w.
func (v Variable) WriteTerm(w io.Writer, opts WriteTermOptions, env Env) error {
	ref, ok := env.Lookup(v)
	if ok && opts.Descriptive {
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
