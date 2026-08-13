package runtime

import (
	"io"
	"io/fs"
	"os"

	"github.com/ichiban/prolog/v2/internal/term"
)

type File interface {
	io.Closer
	Stat() (fs.FileInfo, error)
}

type FS interface {
	Open(name string, mode term.Mode) (File, error)
}

type ReadOnly struct {
	fs.FS
}

func (r ReadOnly) Open(name string, mode term.Mode) (File, error) {
	if mode == term.Write || mode == term.Append {
		return nil, fs.ErrPermission
	}
	return r.FS.Open(name)
}

type Root struct {
	*os.Root
}

func (r Root) Open(name string, mode term.Mode) (File, error) {
	return r.OpenFile(name, int(mode), 0644)
}
