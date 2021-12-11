package dcg

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/ichiban/prolog"
)

func Test_install(t *testing.T) {
	i := prolog.New(nil, nil)
	assert.NoError(t, i.Exec(`:- [library(dcg)].`))
}
