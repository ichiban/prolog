package runtime

import (
	"errors"
	"io/fs"
	"os"
	"slices"

	"github.com/ichiban/prolog/v2/internal/term"
)

type OpenFiler interface {
	OpenFile(name string, flag int, perm os.FileMode) (*os.File, error)
}

type FSSet []NamedFS

type NamedFS struct {
	Name term.Atom
	FS   fs.FS
}

func (f *FSSet) Put(name term.Atom, fs fs.FS) error {
	if _, ok := f.Get(name); ok {
		return errors.New("duplicate entry")
	}
	*f = append(*f, NamedFS{
		Name: name,
		FS:   fs,
	})
	return nil
}

func (f *FSSet) Get(name term.Atom) (fs.FS, bool) {
	i := slices.IndexFunc(*f, func(n NamedFS) bool {
		return n.Name == name
	})
	if i < 0 {
		return nil, false
	}
	return (*f)[i].FS, true
}
