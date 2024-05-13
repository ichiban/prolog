package prolog

import (
	"errors"
	"io/fs"
	"os"
	"time"
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

// FixedModTimeFS is an fs.FS which files ModTime() is always equal to ModTime.
type FixedModTimeFS struct {
	fs.FS
	ModTime time.Time
}

func (fs FixedModTimeFS) Open(name string) (fs.File, error) {
	f, err := fs.FS.Open(name)
	if err != nil {
		return nil, err
	}
	return fixedModTimeFile{File: f, modTime: fs.ModTime}, nil
}

type fixedModTimeFile struct {
	fs.File
	modTime time.Time
}

func (f fixedModTimeFile) Stat() (fs.FileInfo, error) {
	fi, err := f.File.Stat()
	if err != nil {
		return nil, err
	}
	return fixedModTimeFileInfo{FileInfo: fi, modTime: f.modTime}, nil
}

type fixedModTimeFileInfo struct {
	fs.FileInfo
	modTime time.Time
}

func (f fixedModTimeFileInfo) ModTime() time.Time {
	return f.modTime
}
