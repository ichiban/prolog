package prolog

import (
	"errors"
	"fmt"
	"reflect"
	"slices"

	"github.com/ichiban/prolog/v2/internal/syntax"
	"github.com/ichiban/prolog/v2/internal/term"
)

var (
	typeAtom = reflect.TypeFor[Atom]()
	typeRaw  = reflect.TypeFor[Raw]()
)

// decoder copies the bindings of one solution into a Go value.
type decoder struct {
	i        *Interpreter
	varNames []term.VariableName
}

// decodeResult copies the bindings of the current solution into out, which must
// point to a map keyed by a string type or to a struct.
//
// A map is keyed by variable name and its element type decides how each term is
// converted. A struct is matched by field name, or by the name in a `prolog`
// tag; a field tagged "-" is skipped. Variables without a binding are left out,
// so a map has no entry for them and a struct field keeps its zero value.
func (i *Interpreter) decodeResult(out any, varNames []term.VariableName) error {
	v := reflect.ValueOf(out)
	if v.Kind() != reflect.Pointer || v.IsNil() {
		return fmt.Errorf("unsupported result type: %T", out)
	}

	d := decoder{i: i, varNames: varNames}
	switch v = v.Elem(); v.Kind() {
	case reflect.Map:
		return d.decodeMap(v)
	case reflect.Struct:
		return d.decodeStruct(v)
	default:
		return fmt.Errorf("unsupported result type: %T", out)
	}
}

func (d decoder) decodeMap(v reflect.Value) error {
	t := v.Type()
	if t.Key().Kind() != reflect.String {
		return fmt.Errorf("unsupported result type: %s", t)
	}
	if v.IsNil() {
		v.Set(reflect.MakeMap(t))
	}

	for _, vn := range d.varNames {
		h, ok := d.binding(vn)
		if !ok {
			continue
		}

		elem := reflect.New(t.Elem()).Elem()
		if err := d.decodeTerm(elem, h); err != nil {
			return fmt.Errorf("%s: %w", vn.Name, err)
		}
		v.SetMapIndex(reflect.ValueOf(vn.Name).Convert(t.Key()), elem)
	}
	return nil
}

func (d decoder) decodeStruct(v reflect.Value) error {
	t := v.Type()

	fields := make(map[string]int, t.NumField())
	for j := range t.NumField() {
		f := t.Field(j)
		if !f.IsExported() {
			continue
		}
		name := f.Name
		if alias, ok := f.Tag.Lookup("prolog"); ok {
			if alias == "-" {
				continue
			}
			name = alias
		}
		fields[name] = j
	}

	for _, vn := range d.varNames {
		j, ok := fields[vn.Name]
		if !ok {
			continue
		}
		h, ok := d.binding(vn)
		if !ok {
			continue
		}
		if err := d.decodeTerm(v.Field(j), h); err != nil {
			return fmt.Errorf("%s: %w", vn.Name, err)
		}
	}
	return nil
}

// binding returns the term the named variable stands for. A variable that is
// still unbound has no binding, unless it was unified with another variable the
// query named, in which case that variable is the binding.
func (d decoder) binding(vn term.VariableName) (term.Handle, bool) {
	if vn.Name == "_" {
		return term.Handle{}, false
	}

	h := d.i.engine.Deref(vn.Variable)
	if _, ok := d.i.engine.Variable(h); ok {
		if h == vn.Variable {
			return term.Handle{}, false
		}
		if j := slices.IndexFunc(d.varNames, func(o term.VariableName) bool {
			return o.Variable == h
		}); j < 0 {
			return term.Handle{}, false
		}
	}
	return h, true
}

// decodeTerm converts h into dest, whose type decides which terms are
// acceptable. It's the inverse of Interpreter.encodeTerm.
func (d decoder) decodeTerm(dest reflect.Value, h term.Handle) error {
	e := &d.i.engine
	h = e.Deref(h)

	// Atom and Raw are string types, so they're matched before Kind is
	// consulted below.
	switch dest.Type() {
	case typeAtom:
		a, ok := e.Atom(h)
		if !ok {
			return d.conversionError(h, dest.Type())
		}
		dest.SetString(a.String())
		return nil
	case typeRaw:
		dest.SetString(d.format(h))
		return nil
	}

	switch dest.Kind() {
	case reflect.Interface:
		if dest.NumMethod() != 0 {
			return d.conversionError(h, dest.Type())
		}
		v, err := d.decodeAny(h)
		if err != nil {
			return err
		}
		if v == nil {
			dest.SetZero()
			return nil
		}
		dest.Set(reflect.ValueOf(v))
		return nil
	case reflect.String:
		s, ok := e.CharList(h)
		if !ok {
			return d.conversionError(h, dest.Type())
		}
		dest.SetString(s)
		return nil
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		n, ok := e.Integer(h)
		if !ok {
			return d.conversionError(h, dest.Type())
		}
		if dest.OverflowInt(n) {
			return fmt.Errorf("%d overflows %s", n, dest.Type())
		}
		dest.SetInt(n)
		return nil
	case reflect.Float32, reflect.Float64:
		f, ok := e.Float(h)
		if !ok {
			return d.conversionError(h, dest.Type())
		}
		if dest.OverflowFloat(f) {
			return fmt.Errorf("%v overflows %s", f, dest.Type())
		}
		dest.SetFloat(f)
		return nil
	case reflect.Slice:
		return d.decodeSlice(dest, h)
	default:
		return d.conversionError(h, dest.Type())
	}
}

func (d decoder) decodeSlice(dest reflect.Value, h term.Handle) error {
	elems := reflect.MakeSlice(dest.Type(), 0, 0)
	for elem, ok := range d.i.engine.List(h) {
		if !ok {
			return d.conversionError(h, dest.Type())
		}
		v := reflect.New(dest.Type().Elem()).Elem()
		if err := d.decodeTerm(v, elem); err != nil {
			return err
		}
		elems = reflect.Append(elems, v)
	}
	dest.Set(elems)
	return nil
}

// decodeAny converts h into the Value that represents it most closely. A term
// that has no such representation becomes Raw.
func (d decoder) decodeAny(h term.Handle) (any, error) {
	e := &d.i.engine

	if _, ok := e.Variable(h); ok {
		return nil, nil
	}
	if n, ok := e.Integer(h); ok {
		return n, nil
	}
	if f, ok := e.Float(h); ok {
		return f, nil
	}
	// A list of one-character atoms is a char list, the empty list included.
	// It's checked before Atom so that "abc" and [a,b,c], which are the same
	// term, both come back as a string.
	if s, ok := e.CharList(h); ok {
		return s, nil
	}
	if a, ok := e.Atom(h); ok {
		return Atom(a.String()), nil
	}
	if elems, ok := d.anyList(h); ok {
		return elems, nil
	}
	return Raw(d.format(h)), nil
}

func (d decoder) anyList(h term.Handle) ([]any, bool) {
	elems := []any{}
	for elem, ok := range d.i.engine.List(h) {
		if !ok {
			return nil, false
		}
		v, err := d.decodeAny(d.i.engine.Deref(elem))
		if err != nil {
			return nil, false
		}
		elems = append(elems, v)
	}
	return elems, true
}

func (d decoder) format(h term.Handle) string {
	return fmt.Sprintf("%s", &syntax.Formatter{
		Arena:         d.i.engine.Arena,
		Term:          h,
		VariableNames: d.varNames,
		Quoted:        true,
	})
}

func (d decoder) conversionError(h term.Handle, t reflect.Type) error {
	return fmt.Errorf("cannot convert %s into %s: %w", d.format(h), t, errConversion)
}

// errConversion is returned when a term doesn't fit the Go type it's decoded
// into.
var errConversion = errors.New("conversion failed")
