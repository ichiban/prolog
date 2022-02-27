package main

import (
	"bufio"
	"bytes"
	"context"
	"errors"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"os/signal"
	"runtime/debug"
	"strings"

	"golang.org/x/crypto/ssh/terminal"

	"github.com/ichiban/prolog"
	"github.com/ichiban/prolog/engine"
)

const (
	prompt     = "?- "
	contPrompt = "|- "
)

func main() {
	var verbose bool
	flag.BoolVar(&verbose, "v", false, `verbose`)
	flag.Parse()

	oldState, err := terminal.MakeRaw(0)
	if err != nil {
		log.Panicf("failed to enter raw mode: %v", err)
	}
	restore := func() {
		_ = terminal.Restore(0, oldState)
	}
	defer restore()

	t := terminal.NewTerminal(os.Stdin, prompt)
	defer fmt.Printf("\r\n")

	log.SetOutput(t)

	i := prolog.New(os.Stdin, t)
	i.Register1("halt", func(t engine.Term, k func(*engine.Env) *engine.Promise, env *engine.Env) *engine.Promise {
		restore()
		return engine.Halt(t, k, env)
	})
	i.Register1("cd", func(dir engine.Term, k func(*engine.Env) *engine.Promise, env *engine.Env) *engine.Promise {
		switch dir := env.Resolve(dir).(type) {
		case engine.Atom:
			if err := os.Chdir(string(dir)); err != nil {
				return engine.Error(err)
			}
			return k(env)
		default:
			return engine.Error(errors.New("not a dir"))
		}
	})
	if verbose {
		i.OnCall = func(pi engine.ProcedureIndicator, args []engine.Term, env *engine.Env) {
			goal, _ := pi.Apply(args...)
			log.Printf("CALL %s", env.Simplify(goal))
		}
		i.OnExit = func(pi engine.ProcedureIndicator, args []engine.Term, env *engine.Env) {
			goal, _ := pi.Apply(args...)
			log.Printf("EXIT %s", env.Simplify(goal))
		}
		i.OnFail = func(pi engine.ProcedureIndicator, args []engine.Term, env *engine.Env) {
			goal, _ := pi.Apply(args...)
			log.Printf("FAIL %s", env.Simplify(goal))
		}
		i.OnRedo = func(pi engine.ProcedureIndicator, args []engine.Term, env *engine.Env) {
			goal, _ := pi.Apply(args...)
			log.Printf("REDO %s", env.Simplify(goal))
		}
	}
	i.OnUnknown = func(pi engine.ProcedureIndicator, args []engine.Term, env *engine.Env) {
		log.Printf("UNKNOWN %s", pi)
	}
	i.Register1("version", func(t engine.Term, k func(*engine.Env) *engine.Promise, env *engine.Env) *engine.Promise {
		info, ok := debug.ReadBuildInfo()
		if !ok {
			return engine.Bool(false)
		}

		env, ok = t.Unify(engine.Atom(info.Main.Version), false, env)
		if !ok {
			return engine.Bool(false)
		}

		return k(env)
	})

	for _, a := range flag.Args() {
		b, err := ioutil.ReadFile(a)
		if err != nil {
			log.Panicf("failed to read %s: %v", a, err)
		}

		if err := i.Exec(string(b)); err != nil {
			log.Panicf("failed to execute %s: %v", a, err)
		}
	}

	ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt)
	defer stop()

	var buf strings.Builder
	keys := bufio.NewReader(os.Stdin)
	for {
		if err := handleLine(ctx, &buf, i, t, keys); err != nil {
			log.Panic(err)
		}
	}
}

func handleLine(ctx context.Context, buf *strings.Builder, p *prolog.Interpreter, t *terminal.Terminal, keys *bufio.Reader) (err error) {
	line, err := t.ReadLine()
	if err != nil {
		return err
	}
	_, _ = buf.WriteString(line)

	sols, err := p.QueryContext(ctx, buf.String())
	switch err {
	case engine.ErrInsufficient:
		_, _ = buf.WriteRune('\n')
		// Returns without resetting buf.
		t.SetPrompt(contPrompt)
		return nil
	case nil:
		buf.Reset()
		t.SetPrompt(prompt)
	default:
		log.Printf("failed to query: %v", err)
		buf.Reset()
		t.SetPrompt(prompt)
		return nil
	}
	defer func() {
		_ = sols.Close()
	}()

	var exists bool
	for sols.Next() {
		exists = true

		m := map[string]engine.Term{}
		_ = sols.Scan(m)

		var buf bytes.Buffer
		vars := sols.Vars()
		if len(vars) == 0 {
			_, _ = fmt.Fprintf(&buf, "%t", true)
		} else {
			ls := make([]string, len(vars))
			for i, v := range vars {
				var sb strings.Builder
				_, _ = fmt.Fprintf(&sb, "%s = ", v)
				if err := p.Write(&sb, m[v], nil, engine.WithQuoted(true)); err != nil {
					return err
				}
				ls[i] = sb.String()
			}
			_, _ = fmt.Fprintf(&buf, strings.Join(ls, ",\n"))
		}
		if _, err := t.Write(buf.Bytes()); err != nil {
			return err
		}

		r, _, err := keys.ReadRune()
		if err != nil {
			return err
		}
		if r != ';' {
			r = '.'
		}
		if _, err := fmt.Fprintf(t, "%s\n", string(r)); err != nil {
			return err
		}
		if r == '.' {
			break
		}
	}

	if err := sols.Err(); err != nil {
		log.Print(err)
		return nil
	}

	if !exists {
		if _, err := fmt.Fprintf(t, "%t.\n", false); err != nil {
			return err
		}
	}

	return nil
}
