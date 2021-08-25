package main

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"os"
	"strings"

	"github.com/ichiban/prolog/nondet"
	"github.com/ichiban/prolog/term"

	"github.com/ichiban/prolog"

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

	oldState, err := terminal.MakeRaw(0)
	if err != nil {
		log.Panicf("failed to enter raw mode: %v", err)
	}
	restore := func() {
		_ = terminal.Restore(0, oldState)
	}
	defer restore()

	t := terminal.NewTerminal(os.Stdin, "?- ")
	defer fmt.Printf("\r\n")

	log.SetOutput(t)

	i := prolog.New(bufio.NewReader(os.Stdin), t)
	i.Register1("halt", func(t term.Interface, k func(term.Env) *nondet.Promise, env *term.Env) *nondet.Promise {
		restore()
		return i.Halt(t, k, env)
	})
	i.Register1("cd", func(dir term.Interface, k func(term.Env) *nondet.Promise, env *term.Env) *nondet.Promise {
		switch dir := env.Resolve(dir).(type) {
		case term.Atom:
			if err := os.Chdir(string(dir)); err != nil {
				return nondet.Error(err)
			}
			return k(*env)
		default:
			return nondet.Error(errors.New("not a dir"))
		}
	})
	if verbose {
		i.OnCall = func(pi string, args term.Interface, env term.Env) {
			log.Printf("CALL %s %s", pi, i.DescribeTerm(env.Resolve(args), env))
		}
		i.OnExit = func(pi string, args term.Interface, env term.Env) {
			log.Printf("EXIT %s %s", pi, i.DescribeTerm(env.Resolve(args), env))
		}
		i.OnFail = func(pi string, args term.Interface, env term.Env) {
			log.Printf("FAIL %s %s", pi, i.DescribeTerm(env.Resolve(args), env))
		}
		i.OnRedo = func(pi string, args term.Interface, env term.Env) {
			log.Printf("REDO %s %s", pi, i.DescribeTerm(env.Resolve(args), env))
		}
	}
	i.UnknownWarning = func(procedure string) {
		log.Printf("unknown procedure: %s", procedure)
	}
	i.Register1("version", func(t term.Interface, k func(term.Env) *nondet.Promise, env *term.Env) *nondet.Promise {
		if !t.Unify(term.Atom(Version), false, env) {
			return nondet.Bool(false)
		}
		return k(*env)
	})

	for _, a := range pflag.Args() {
		b, err := ioutil.ReadFile(a)
		if err != nil {
			log.Panicf("failed to read %s: %v", a, err)
		}

		if err := i.Exec(string(b)); err != nil {
			log.Panicf("failed to execute %s: %v", a, err)
		}
	}

	keys := bufio.NewReader(os.Stdin)
	for {
		line, err := t.ReadLine()
		if err != nil {
			if err == io.EOF {
				break
			}
			log.Printf("failed to read line: %v", err)
		}

		c := 0
		sols, err := i.Query(line)
		if err != nil {
			log.Printf("failed to query: %v", err)
			continue
		}
		for sols.Next() {
			c++

			m := map[string]term.Interface{}
			if err := sols.Scan(m); err != nil {
				log.Printf("failed to scan: %v", err)
				break
			}

			vars := sols.Vars()
			ls := make([]string, 0, len(vars))
			for _, n := range vars {
				v := m[n]
				if _, ok := v.(*term.Variable); ok {
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
				log.Printf("failed to read rune: %v", err)
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
			log.Printf("failed: %v", err)
			continue
		}

		if c == 0 {
			fmt.Fprintf(t, "%t.\n", false)
		}
	}
}
