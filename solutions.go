package prolog

import (
	"fmt"
	"reflect"
)

// Solutions is the result of a query. Everytime the Next method is called, it searches for the next solution.
// By calling the Scan method, you can retrieve the content of the solution.
type Solutions struct {
	vars []*Variable
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

// Scan copies the variable values of the current solution into the specified map.
// Currently, only map[string]Term is supported.
func (s *Solutions) Scan(out interface{}) error {
	o := reflect.ValueOf(out)
	switch o.Kind() {
	case reflect.Map:
		for _, v := range s.vars {
			if v.Name == "" {
				continue
			}

			o.SetMapIndex(reflect.ValueOf(v.Name), reflect.ValueOf(Resolve(v)))
		}
		return nil
	default:
		return fmt.Errorf("invalid kind: %s", o.Kind())
	}
}

// Err returns the error if exists.
func (s *Solutions) Err() error {
	return s.err
}

// Vars returns variable names.
func (s *Solutions) Vars() []string {
	ns := make([]string, len(s.vars))
	for i, v := range s.vars {
		ns[i] = v.Name
	}
	return ns
}
