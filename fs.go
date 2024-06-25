package prolog

import (
	"errors"
	"io/fs"
)

// OverlayFS is a sequence of fs.FS.
// If the requested file doesn't exist in the first fs.FS, it falls back to the next and so on.
type OverlayFS []fs.FS

var (
	_ fs.FS     = OverlayFS{}
	_ fs.StatFS = OverlayFS{}
)

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

func (o OverlayFS) Stat(name string) (fs.FileInfo, error) {
	for _, e := range o {
		fi, err := fs.Stat(e, name)
		if err != nil {
			continue
		}
		return fi, nil
	}
	return nil, fs.ErrNotExist
}
