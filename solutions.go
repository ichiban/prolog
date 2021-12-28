package prolog

import (
	"errors"
	"fmt"
	"reflect"

	"github.com/ichiban/prolog/engine"
)

// Solutions is the result of a query. Everytime the Next method is called, it searches for the next solution.
// By calling the Scan method, you can retrieve the content of the solution.
type Solutions struct {
	env    *engine.Env
	vars   []engine.Variable
	more   chan<- bool
	next   <-chan *engine.Env
	err    error
	closed bool
}

// ErrClosed indicates the Solutions are already closed and unable to perform the operation.
var ErrClosed = errors.New("closed")

// Close closes the Solutions and terminates the search for other solutions.
func (s *Solutions) Close() error {
	if s.closed {
		return ErrClosed
	}
	close(s.more)
	s.closed = true
	return nil
}

// Next prepares the next solution for reading with the Scan method. It returns true if it finds another solution,
// or false if there's no further solutions or if there's an error.
func (s *Solutions) Next() bool {
	if s.closed {
		return false
	}
	s.more <- true
	var ok bool
	s.env, ok = <-s.next
	return ok
}

// Scan copies the variable values of the current solution into the specified struct/map.
func (s *Solutions) Scan(dest interface{}) error {
	o := reflect.ValueOf(dest)
	switch o.Kind() {
	case reflect.Ptr:
		o = o.Elem()
		switch o.Kind() {
		case reflect.Struct:
			t := o.Type()

			fields := map[string]reflect.Value{}
			for i := 0; i < t.NumField(); i++ {
				f := t.Field(i)
				name := f.Name
				if alias, ok := f.Tag.Lookup("prolog"); ok {
					name = alias
				}
				fields[name] = o.Field(i)
			}

			for _, v := range s.vars {
				f, ok := fields[string(v)]
				if !ok {
					continue
				}

				val, err := convert(s.env.Simplify(v), f.Type(), s.env)
				if err != nil {
					return err
				}
				fields[string(v)].Set(val)
			}
		}
		return nil
	case reflect.Map:
		t := o.Type()
		if t.Key() != reflect.TypeOf("") {
			return errors.New("map key is not string")
		}

		for _, v := range s.vars {
			val, err := convert(s.env.Simplify(v), t.Elem(), s.env)
			if err != nil {
				return err
			}
			o.SetMapIndex(reflect.ValueOf(string(v)), val)
		}
		return nil
	default:
		return fmt.Errorf("invalid kind: %s", o.Kind())
	}
}

func convert(t engine.Term, typ reflect.Type, env *engine.Env) (reflect.Value, error) {
	switch typ {
	case reflect.TypeOf((*interface{})(nil)).Elem(), reflect.TypeOf((*engine.Term)(nil)).Elem():
		return reflect.ValueOf(t), nil
	}

	switch typ.Kind() {
	case reflect.Float32, reflect.Float64:
		if f, ok := t.(engine.Float); ok {
			return reflect.ValueOf(f).Convert(typ), nil
		}
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		if i, ok := t.(engine.Integer); ok {
			return reflect.ValueOf(i).Convert(typ), nil
		}
	case reflect.String:
		if a, ok := t.(engine.Atom); ok {
			return reflect.ValueOf(string(a)), nil
		}
	case reflect.Slice:
		r := reflect.MakeSlice(reflect.SliceOf(typ.Elem()), 0, 0)
		err := engine.EachList(t, func(elem engine.Term) error {
			e, err := convert(elem, typ.Elem(), env)
			r = reflect.Append(r, e)
			return err
		}, env)
		return r, err
	}
	return reflect.Value{}, fmt.Errorf("failed to convert: %s", typ)
}

// Err returns the error if exists.
func (s *Solutions) Err() error {
	return s.err
}

// Vars returns variable names.
func (s *Solutions) Vars() []string {
	ns := make([]string, 0, len(s.vars))
	for _, v := range s.vars {
		if v.Generated() {
			continue
		}
		ns = append(ns, string(v))
	}
	return ns
}

// Solution is the single result of a query.
type Solution struct {
	sols *Solutions
	err  error
}

// Scan copies the variable values of the solution into the specified struct/map.
func (s *Solution) Scan(dest interface{}) error {
	if err := s.err; err != nil {
		return err
	}
	return s.sols.Scan(dest)
}

// Err returns an error that occurred while querying for the Solution, if any.
func (s *Solution) Err() error {
	return s.err
}

// Vars returns variable names.
func (s *Solution) Vars() []string {
	if s.sols == nil {
		return nil
	}
	return s.sols.Vars()
}
