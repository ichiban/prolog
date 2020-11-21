package main

import (
	"fmt"
	"io"
	"io/ioutil"
	"os"

	"github.com/ichiban/prolog"

	"github.com/sirupsen/logrus"
	"github.com/spf13/pflag"
	"golang.org/x/crypto/ssh/terminal"
)

var Version = "1pl/0.1"

func main() {
	pflag.Parse()

	log := logrus.WithFields(nil)

	oldState, err := terminal.MakeRaw(0)
	if err != nil {
		log.WithField("err", err).Panic("failed to enter raw mode")
	}
	defer func() {
		_ = terminal.Restore(0, oldState)
	}()

	t := terminal.NewTerminal(os.Stdin, "?- ")
	defer fmt.Printf("\r\n")

	logrus.SetOutput(t)

	e, err := prolog.NewEngine()
	if err != nil {
		log.Panic(err)
	}
	e.Register0("true", func() (b bool, err error) {
		return true, nil
	})
	e.Register0("false", func() (b bool, err error) {
		return false, nil
	})
	e.Register1("version", func(term prolog.Term) (b bool, err error) {
		return term.Unify(prolog.Atom(Version)), nil
	})

	for _, a := range pflag.Args() {
		log := log.WithField("file", a)

		b, err := ioutil.ReadFile(a)
		if err != nil {
			log.WithField("err", err).Panic("failed to read")
		}

		if err := e.Load(string(b)); err != nil {
			log.WithField("err", err).Panic("failed to compile")
		}
	}

	for {
		line, err := t.ReadLine()
		if err != nil {
			if err == io.EOF {
				break
			}
			log.WithField("err", err).Error("failed to read line")
		}
		ok, err := e.Query(line, func(vars []*prolog.Variable) bool {
			for _, v := range vars {
				if v.Ref == nil {
					continue
				}
				fmt.Fprintf(t, "%s\n", e.StringTerm(v))
			}
			return true
		})
		if err != nil {
			log.WithField("err", err).Error("failed to query")
			continue
		}
		fmt.Fprintf(t, "%t\n", ok)
	}
}
