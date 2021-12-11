package dcg

import (
	_ "embed"

	"github.com/ichiban/prolog"
)

//go:embed dcg.pl
var dcg string

func init() {
	prolog.Register("dcg", install)
}

func install(i *prolog.Interpreter) error {
	return i.Exec(dcg)
}
