package main

import (
	"bufio"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"strings"

	"github.com/ichiban/prolog/engine"

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

	i := prolog.New(bufio.NewReader(os.Stdin), t)
	i.OnHalt = restore
	/*
		i.OnArrive = func(goal engine.Term) {
			logrus.WithFields(logrus.Fields{"goal": i.DescribeTerm(goal)}).Debug("arrive")
		}
		i.OnExec = func(op string, arg engine.Term) {
			fs := logrus.Fields{"op": op}
			if arg != nil {
				fs["arg"] = arg
			}
			logrus.WithFields(fs).Debug("exec")
		}
	*/
	i.OnCall = func(pi string, args engine.Term, env engine.Env) {
		logrus.WithFields(logrus.Fields{
			"pi":   pi,
			"args": i.DescribeTerm(args, env),
		}).Debug("call")
	}
	i.OnExit = func(pi string, args engine.Term, env engine.Env) {
		logrus.WithFields(logrus.Fields{
			"pi":   pi,
			"args": i.DescribeTerm(args, env),
		}).Debug("exit")
	}
	i.OnFail = func(pi string, args engine.Term, env engine.Env) {
		logrus.WithFields(logrus.Fields{
			"pi":   pi,
			"args": i.DescribeTerm(args, env),
		}).Debug("fail")
	}
	i.OnRedo = func(pi string, args engine.Term, env engine.Env) {
		logrus.WithFields(logrus.Fields{
			"pi":   pi,
			"args": i.DescribeTerm(args, env),
		}).Debug("redo")
	}
	i.OnPanic = func(r interface{}) {
		logrus.WithField("value", r).Error("panicked")
	}
	i.Register1("version", func(term engine.Term, k func(engine.Env) *engine.Promise, env *engine.Env) *engine.Promise {
		if !term.Unify(engine.Atom(Version), false, env) {
			return engine.Bool(false)
		}
		return k(*env)
	})

	for _, a := range pflag.Args() {
		log := log.WithField("file", a)

		b, err := ioutil.ReadFile(a)
		if err != nil {
			log.WithError(err).Panic("failed to read")
		}

		if err := i.Exec(string(b)); err != nil {
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
		sols, err := i.Query(line)
		if err != nil {
			log.WithError(err).Error("failed to query")
			continue
		}
		for sols.Next() {
			c++

			m := map[string]engine.Term{}
			if err := sols.Scan(m); err != nil {
				log.WithError(err).Error("failed to scan")
				break
			}

			vars := sols.Vars()
			ls := make([]string, 0, len(vars))
			for _, n := range vars {
				v := m[n]
				if _, ok := v.(*engine.Variable); ok {
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

		if err := sols.Err(); err != nil {
			log.WithError(err).Error("failed")
			continue
		}

		if c == 0 {
			fmt.Fprintf(t, "%t.\n", false)
		}
	}
}
