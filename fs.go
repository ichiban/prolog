package prolog

import (
	"io/fs"
	"os"
)

type RootFS struct {
	*os.Root
}

func (r RootFS) Open(name string) (fs.File, error) {
	return r.Root.Open(name)
}
