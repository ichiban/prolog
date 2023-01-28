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
	ctx    context.Context
	vars   []engine.ParsedVariable
	more   chan<- bool
	next   <-chan context.Context
	err    error
	closed bool
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
	s.ctx, ok = <-s.next
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

			if err := convertAssign(s.ctx, f, v.Variable); err != nil {
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
			if err := convertAssign(s.ctx, dest.Interface(), v.Variable); err != nil {
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

func convertAssign(ctx context.Context, dest interface{}, t engine.Term) error {
	switch d := dest.(type) {
	case *interface{}:
		return convertAssignAny(ctx, d, t)
	case *string:
		return convertAssignString(ctx, d, t)
	case *int:
		return convertAssignInt(ctx, d, t)
	case *int8:
		return convertAssignInt8(ctx, d, t)
	case *int16:
		return convertAssignInt16(ctx, d, t)
	case *int32:
		return convertAssignInt32(ctx, d, t)
	case *int64:
		return convertAssignInt64(ctx, d, t)
	case *float32:
		return convertAssignFloat32(ctx, d, t)
	case *float64:
		return convertAssignFloat64(ctx, d, t)
	case Scanner:
		return d.Scan(ctx, t)
	default:
		return convertAssignSlice(ctx, d, t)
	}
}

func convertAssignAny(ctx context.Context, d *interface{}, t engine.Term) error {
	switch t := engine.Resolve(ctx, t).(type) {
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
		iter := engine.ListIterator{List: t}
		for iter.Next(context.TODO()) {
			s = append(s, nil)
			if err := convertAssign(ctx, &s[len(s)-1], iter.Current()); err != nil {
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

func convertAssignString(ctx context.Context, d *string, t engine.Term) error {
	switch t := engine.Resolve(ctx, t).(type) {
	case engine.Atom:
		*d = t.String()
		return nil
	default:
		return errConversion
	}
}

func convertAssignInt(ctx context.Context, d *int, t engine.Term) error {
	switch t := engine.Resolve(ctx, t).(type) {
	case engine.Integer:
		*d = int(t)
		return nil
	default:
		return errConversion
	}
}

func convertAssignInt8(ctx context.Context, d *int8, t engine.Term) error {
	switch t := engine.Resolve(ctx, t).(type) {
	case engine.Integer:
		*d = int8(t)
		return nil
	default:
		return errConversion
	}
}

func convertAssignInt16(ctx context.Context, d *int16, t engine.Term) error {
	switch t := engine.Resolve(ctx, t).(type) {
	case engine.Integer:
		*d = int16(t)
		return nil
	default:
		return errConversion
	}
}

func convertAssignInt32(ctx context.Context, d *int32, t engine.Term) error {
	switch t := engine.Resolve(ctx, t).(type) {
	case engine.Integer:
		*d = int32(t)
		return nil
	default:
		return errConversion
	}
}

func convertAssignInt64(ctx context.Context, d *int64, t engine.Term) error {
	switch t := engine.Resolve(ctx, t).(type) {
	case engine.Integer:
		*d = int64(t)
		return nil
	default:
		return errConversion
	}
}

func convertAssignFloat32(ctx context.Context, d *float32, t engine.Term) error {
	switch t := engine.Resolve(ctx, t).(type) {
	case engine.Float:
		*d = float32(t)
		return nil
	default:
		return errConversion
	}
}

func convertAssignFloat64(ctx context.Context, d *float64, t engine.Term) error {
	switch t := engine.Resolve(ctx, t).(type) {
	case engine.Float:
		*d = float64(t)
		return nil
	default:
		return errConversion
	}
}

func convertAssignSlice(ctx context.Context, d interface{}, t engine.Term) error {
	v := reflect.ValueOf(d).Elem()

	if k := v.Kind(); k != reflect.Slice {
		return errConversion
	}

	v.SetLen(0)
	orig := v

	iter := engine.ListIterator{List: t}
	for iter.Next(ctx) {
		v = reflect.Append(v, reflect.Zero(v.Type().Elem()))
		dest := v.Index(v.Len() - 1).Addr().Interface()
		if err := convertAssign(ctx, dest, iter.Current()); err != nil {
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
	Scan(ctx context.Context, term engine.Term) error
}

// TermString is a string representation of term.
type TermString string

// Scan implements Scanner interface.
func (t *TermString) Scan(ctx context.Context, term engine.Term) error {
	var sb strings.Builder
	s := engine.NewOutputTextStream(&sb)
	_, _ = engine.WriteTerm(ctx, s, term, engine.List(engine.NewAtom("quoted").Apply(engine.NewAtom("true")))).Force()
	*t = TermString(sb.String())
	return nil
}
