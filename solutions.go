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
	vars []engine.Variable
	env  *engine.Env
	more chan<- bool
	next <-chan bool
	err  error
}

// Close closes the Solutions and terminates the search for other solutions.
func (s *Solutions) Close() error {
	close(s.more)
	return nil
}

// Next prepares the next solution for reading with the Scan method. It returns true if it finds another solution,
// or false if there's no further solutions or if there's an error.
func (s *Solutions) Next() bool {
	s.more <- true
	return <-s.next
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

				val, err := convert(s.env.Resolve(v), f.Type(), s.env)
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
			if v.Anonymous() {
				continue
			}

			val, err := convert(s.env.Resolve(v), t.Elem(), s.env)
			if err != nil {
				return err
			}
			o.SetMapIndex(reflect.ValueOf(v), val)
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
		if err := engine.Each(t, func(elem engine.Term) error {
			e, err := convert(env.Resolve(elem), typ.Elem(), env)
			if err != nil {
				return err
			}
			r = reflect.Append(r, e)
			return nil
		}, env); err != nil {
			return reflect.Value{}, err
		}
		return r, nil
	}
	return reflect.Value{}, fmt.Errorf("failed to convert: %s", typ)
}

// Err returns the error if exists.
func (s *Solutions) Err() error {
	return s.err
}

// Vars returns variable names.
func (s *Solutions) Vars() []string {
	ns := make([]string, len(s.vars))
	for i, v := range s.vars {
		ns[i] = string(v)
	}
	return ns
}
