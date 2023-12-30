package prolog

import (
	"errors"
	"io/fs"
	"os"
)

type RealFS struct{}

func (r RealFS) Open(name string) (fs.File, error) {
	return os.Open(name)
}

type OverlayFS []fs.FS

func (o OverlayFS) Open(name string) (fs.File, error) {
	for _, e := range o {
		switch f, err := e.Open(name); {
		case err == nil:
			return f, nil
		case errors.Is(err, fs.ErrNotExist):
			continue
		default:
			return nil, err
		}
	}
	return nil, fs.ErrNotExist
}
