package main

import (
	"bufio"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"strings"

	"github.com/ichiban/prolog"

	"github.com/sirupsen/logrus"
	"github.com/spf13/pflag"
	"golang.org/x/crypto/ssh/terminal"
)

var Version = "1pl/0.1"

func main() {
	var verbose, debug bool
	pflag.BoolVarP(&verbose, "verbose", "v", false, `verbose`)
	pflag.BoolVarP(&debug, "debug", "d", false, `debug`)
	pflag.Parse()

	logrus.SetFormatter(&logrus.TextFormatter{
		ForceColors:      true,
		DisableQuote:     true,
		DisableTimestamp: true,
	})
	switch {
	case verbose:
		logrus.SetLevel(logrus.InfoLevel)
	case debug:
		logrus.SetLevel(logrus.DebugLevel)
	default:
		logrus.SetLevel(logrus.WarnLevel)
	}

	log := logrus.WithFields(nil)

	oldState, err := terminal.MakeRaw(0)
	if err != nil {
		log.WithError(err).Panic("failed to enter raw mode")
	}
	defer func() {
		_ = terminal.Restore(0, oldState)
	}()

	t := terminal.NewTerminal(os.Stdin, "?- ")
	defer fmt.Printf("\r\n")

	logrus.SetOutput(t)

	halt := false

	e, err := prolog.NewEngine()
	if err != nil {
		log.Panic(err)
	}
	e.Register1("version", func(term prolog.Term, k func() (bool, error)) (bool, error) {
		if !term.Unify(prolog.Atom(Version)) {
			return false, nil
		}
		return k()
	})
	e.Register0("halt", func(k func() (bool, error)) (bool, error) {
		halt = true
		return k()
	})

	for _, a := range pflag.Args() {
		log := log.WithField("file", a)

		b, err := ioutil.ReadFile(a)
		if err != nil {
			log.WithError(err).Panic("failed to read")
		}

		if err := e.Load(string(b)); err != nil {
			log.WithError(err).Panic("failed to compile")
		}
	}

	keys := bufio.NewReader(os.Stdin)

	for !halt {
		line, err := t.ReadLine()
		if err != nil {
			if err == io.EOF {
				break
			}
			log.WithError(err).Error("failed to read line")
		}

		ok, err := e.Query(line, func(vars []prolog.Variable) bool {
			ls := make([]string, 0, len(vars))
			for _, v := range vars {
				if _, ok := prolog.Resolve(v.Ref).(*prolog.Variable); ok {
					continue
				}
				ls = append(ls, e.StringTerm(&v))
			}
			if len(ls) == 0 {
				fmt.Fprintf(t, "%t ", true)
			} else {
				fmt.Fprint(t, fmt.Sprintf("%s ", strings.Join(ls, ",\n")))
			}

			r, _, err := keys.ReadRune()
			if err != nil {
				log.WithError(err).Error("failed to query")
				return false
			}
			if r != ';' {
				r = '.'
			}

			fmt.Fprintf(t, "%s\n", string(r))

			return r == '.'
		})
		if err != nil {
			log.WithError(err).Error("failed to query")
			continue
		}
		if !ok {
			fmt.Fprintf(t, "%t.\n", false)
		}
	}
}
