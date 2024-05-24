package prolog

import (
	"errors"
	"io/fs"
	"os"
)

// RealFS is the actual file system.
type RealFS struct{}

func (r RealFS) Open(name string) (fs.File, error) {
	return os.Open(name)
}

// OverlayFS is a sequence of fs.FS.
// If the requested file doesn't exist in the first fs.FS, it falls back to the next and so on.
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
