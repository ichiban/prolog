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

// Version is a version of this build.
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
	restore := func() {
		_ = terminal.Restore(0, oldState)
	}
	defer restore()

	t := terminal.NewTerminal(os.Stdin, "?- ")
	defer fmt.Printf("\r\n")

	logrus.SetOutput(t)

	e, err := prolog.NewEngine(bufio.NewReader(os.Stdin), t)
	if err != nil {
		log.Panic(err)
	}
	e.BeforeHalt = append(e.BeforeHalt, restore)
	e.Register1("version", func(term prolog.Term, k func() prolog.Promise) prolog.Promise {
		if !term.Unify(prolog.Atom(Version), false) {
			return prolog.Bool(false)
		}
		return prolog.Delay(k)
	})

	for _, a := range pflag.Args() {
		log := log.WithField("file", a)

		b, err := ioutil.ReadFile(a)
		if err != nil {
			log.WithError(err).Panic("failed to read")
		}

		if err := e.Exec(string(b)); err != nil {
			log.WithError(err).Panic("failed to compile")
		}
	}

	keys := bufio.NewReader(os.Stdin)
	for {
		line, err := t.ReadLine()
		if err != nil {
			if err == io.EOF {
				break
			}
			log.WithError(err).Error("failed to read line")
		}

		c := 0
		sols, err := e.Query(line)
		if err != nil {
			log.WithError(err).Error("failed to query")
			continue
		}
		for sols.Next() {
			c++

			m := map[string]prolog.Term{}
			if err := sols.Scan(m); err != nil {
				log.WithError(err).Error("failed to scan")
				break
			}

			vars := sols.Vars()
			ls := make([]string, 0, len(vars))
			for _, n := range vars {
				v := m[n]
				if _, ok := v.(*prolog.Variable); ok {
					continue
				}
				ls = append(ls, fmt.Sprintf("%s = %s", n, v))
			}
			if len(ls) == 0 {
				fmt.Fprintf(t, "%t.\n", true)
				break
			}

			fmt.Fprintf(t, "%s ", strings.Join(ls, ",\n"))

			r, _, err := keys.ReadRune()
			if err != nil {
				log.WithError(err).Error("failed to query")
				break
			}
			if r != ';' {
				r = '.'
			}

			fmt.Fprintf(t, "%s\n", string(r))

			if r == '.' {
				break
			}
		}
		sols.Close()

		if c == 0 {
			fmt.Fprintf(t, "%t.\n", false)
		}
	}
}
