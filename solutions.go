package prolog

import (
	"context"
	"errors"
	"fmt"
	"reflect"
	"strings"

	"github.com/ichiban/prolog/engine"
)

// ErrClosed indicates the Solutions are already closed and unable to perform the operation.
var ErrClosed = errors.New("closed")

var errConversion = errors.New("conversion failed")

// Solutions is the result of a query. Everytime the Next method is called, it searches for the next solution.
// By calling the Scan method, you can retrieve the content of the solution.
type Solutions struct {
	vm       *engine.VM
	env      *engine.Env
	vars     []engine.ParsedVariable
	more     chan<- bool
	panicked any
	next     <-chan *engine.Env
	err      error
	closed   bool
}

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

	if s.panicked != nil {
		panic(s.panicked)
	}

	return ok
}

// Scan copies the variable values of the current solution into the specified struct/map.
func (s *Solutions) Scan(dest interface{}) error {
	o := reflect.ValueOf(dest)
	for o.Kind() == reflect.Ptr {
		o = o.Elem()
	}
	switch o.Kind() {
	case reflect.Struct:
		t := o.Type()

		fields := make(map[string]interface{}, t.NumField())
		for i := 0; i < t.NumField(); i++ {
			f := t.Field(i)
			name := f.Name
			if alias, ok := f.Tag.Lookup("prolog"); ok {
				name = alias
			}
			fields[name] = o.Field(i).Addr().Interface()
		}

		for _, v := range s.vars {
			n := v.Name.String()
			f, ok := fields[n]
			if !ok {
				continue
			}

			if err := convertAssign(f, s.vm, v.Variable, s.env); err != nil {
				return err
			}
		}
		return nil
	case reflect.Map:
		t := o.Type()
		if t.Key() != reflect.TypeOf("") {
			return errors.New("map key is not string")
		}

		for _, v := range s.vars {
			dest := reflect.New(t.Elem())
			if err := convertAssign(dest.Interface(), s.vm, v.Variable, s.env); err != nil {
				return err
			}
			o.SetMapIndex(reflect.ValueOf(v.Name.String()), dest.Elem())
		}
		return nil
	default:
		return fmt.Errorf("invalid kind: %s", o.Kind())
	}
}

var atomEmptyList = engine.NewAtom("[]")

func convertAssign(dest interface{}, vm *engine.VM, t engine.Term, env *engine.Env) error {
	switch d := dest.(type) {
	case *interface{}:
		return convertAssignAny(d, vm, t, env)
	case *string:
		return convertAssignString(d, t, env)
	case *int:
		return convertAssignInt(d, t, env)
	case *int8:
		return convertAssignInt8(d, t, env)
	case *int16:
		return convertAssignInt16(d, t, env)
	case *int32:
		return convertAssignInt32(d, t, env)
	case *int64:
		return convertAssignInt64(d, t, env)
	case *float32:
		return convertAssignFloat32(d, t, env)
	case *float64:
		return convertAssignFloat64(d, t, env)
	case Scanner:
		return d.Scan(vm, t, env)
	default:
		return convertAssignSlice(d, vm, t, env)
	}
}

func convertAssignAny(d *interface{}, vm *engine.VM, t engine.Term, env *engine.Env) error {
	switch t := env.Resolve(t).(type) {
	case engine.Variable:
		*d = nil
		return nil
	case engine.Atom:
		if t == atomEmptyList {
			*d = []interface{}{}
		} else {
			*d = t.String()
		}
		return nil
	case engine.Integer:
		*d = int(t)
		return nil
	case engine.Float:
		*d = float64(t)
		return nil
	case engine.Compound:
		var s []interface{}
		iter := engine.ListIterator{List: t, Env: env}
		for iter.Next() {
			s = append(s, nil)
			if err := convertAssign(&s[len(s)-1], vm, iter.Current(), env); err != nil {
				return err
			}
		}
		if err := iter.Err(); err != nil {
			return errConversion
		}
		*d = s
		return nil
	default:
		return errConversion
	}
}

func convertAssignString(d *string, t engine.Term, env *engine.Env) error {
	switch t := env.Resolve(t).(type) {
	case engine.Atom:
		*d = t.String()
		return nil
	default:
		return errConversion
	}
}

func convertAssignInt(d *int, t engine.Term, env *engine.Env) error {
	switch t := env.Resolve(t).(type) {
	case engine.Integer:
		*d = int(t)
		return nil
	default:
		return errConversion
	}
}

func convertAssignInt8(d *int8, t engine.Term, env *engine.Env) error {
	switch t := env.Resolve(t).(type) {
	case engine.Integer:
		*d = int8(t)
		return nil
	default:
		return errConversion
	}
}

func convertAssignInt16(d *int16, t engine.Term, env *engine.Env) error {
	switch t := env.Resolve(t).(type) {
	case engine.Integer:
		*d = int16(t)
		return nil
	default:
		return errConversion
	}
}

func convertAssignInt32(d *int32, t engine.Term, env *engine.Env) error {
	switch t := env.Resolve(t).(type) {
	case engine.Integer:
		*d = int32(t)
		return nil
	default:
		return errConversion
	}
}

func convertAssignInt64(d *int64, t engine.Term, env *engine.Env) error {
	switch t := env.Resolve(t).(type) {
	case engine.Integer:
		*d = int64(t)
		return nil
	default:
		return errConversion
	}
}

func convertAssignFloat32(d *float32, t engine.Term, env *engine.Env) error {
	switch t := env.Resolve(t).(type) {
	case engine.Float:
		*d = float32(t)
		return nil
	default:
		return errConversion
	}
}

func convertAssignFloat64(d *float64, t engine.Term, env *engine.Env) error {
	switch t := env.Resolve(t).(type) {
	case engine.Float:
		*d = float64(t)
		return nil
	default:
		return errConversion
	}
}

func convertAssignSlice(d interface{}, vm *engine.VM, t engine.Term, env *engine.Env) error {
	v := reflect.ValueOf(d).Elem()

	if k := v.Kind(); k != reflect.Slice {
		return errConversion
	}

	v.SetLen(0)
	orig := v

	iter := engine.ListIterator{List: t, Env: env}
	for iter.Next() {
		v = reflect.Append(v, reflect.Zero(v.Type().Elem()))
		dest := v.Index(v.Len() - 1).Addr().Interface()
		if err := convertAssign(dest, vm, iter.Current(), env); err != nil {
			return err
		}
	}
	if err := iter.Err(); err != nil {
		return errConversion
	}

	orig.Set(v)

	return nil
}

// Err returns the error if exists.
func (s *Solutions) Err() error {
	return s.err
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

// Scanner is an interface for custom conversion from term to Go value.
type Scanner interface {
	Scan(vm *engine.VM, term engine.Term, env *engine.Env) error
}

// TermString is a string representation of term.
type TermString string

// Scan implements Scanner interface.
func (t *TermString) Scan(vm *engine.VM, term engine.Term, env *engine.Env) error {
	var sb strings.Builder
	s := engine.NewOutputTextStream(&sb)
	_, _ = engine.WriteTerm(vm, s, term, engine.List(engine.NewAtom("quoted").Apply(engine.NewAtom("true"))), engine.Success, env).Force(context.Background())
	*t = TermString(sb.String())
	return nil
}
