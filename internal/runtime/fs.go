package runtime

import (
	"io"
	"io/fs"
	"os"
	"path/filepath"
)

type File interface {
	io.Closer
	Stat() (fs.FileInfo, error)
}

type FS struct {
	SourceFSs []SourceFS
	Root      *os.Root
}

func (f FS) Open(name string) (File, error) {
	return f.OpenFile(name, os.O_RDONLY, 0)
}

func (f FS) OpenFile(name string, flag int, perm fs.FileMode) (File, error) {
	for _, s := range f.SourceFSs {
		rel, err := filepath.Rel(s.BasePath, name)
		if err != nil || !filepath.IsLocal(rel) {
			continue
		}
		if flag != os.O_RDONLY {
			return nil, fs.ErrPermission
		}
		return s.FS.Open(rel)
	}
	if f.Root == nil {
		return nil, fs.ErrNotExist
	}
	return f.Root.OpenFile(name, flag, perm)
}

type SourceFS struct {
	BasePath string
	FS       fs.FS
}
