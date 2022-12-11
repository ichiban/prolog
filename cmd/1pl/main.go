package main

import (
	"bufio"
	"bytes"
	"context"
	"flag"
	"fmt"
	"io"
	"log"
	"os"
	"os/signal"
	"runtime/debug"
	"sort"
	"strings"

	"golang.org/x/crypto/ssh/terminal"

	"github.com/ichiban/prolog"
	"github.com/ichiban/prolog/engine"
)

const (
	prompt          = "?- "
	contPrompt      = "|- "
	userInputPrompt = "|: "
)

var version = func() string {
	info, ok := debug.ReadBuildInfo()
	if !ok {
		return ""
	}

	return info.Main.Version
}()

func main() {
	var verbose bool
	flag.BoolVar(&verbose, "v", false, `verbose`)
	flag.Parse()

	fmt.Printf(`Top level for ichiban/prolog %s
This is for testing purposes only!
See https://github.com/ichiban/prolog for more details.
Type Ctrl-C or 'halt.' to exit.
`, version)

	halt := engine.Halt
	if terminal.IsTerminal(0) {
		oldState, err := terminal.MakeRaw(0)
		if err != nil {
			log.Panicf("failed to enter raw mode: %v", err)
		}
		restore := func() {
			_ = terminal.Restore(0, oldState)
		}
		defer restore()

		halt = func(vm *engine.VM, n engine.Term, k engine.Cont, env *engine.Env) *engine.Promise {
			restore()
			return engine.Halt(vm, n, k, env)
		}
	}

	t := terminal.NewTerminal(os.Stdin, prompt)
	defer fmt.Printf("\r\n")

	log.SetOutput(t)

	i := New(&userInput{t: t}, t)
	i.Register1(engine.NewAtom("halt"), halt)
	i.Unknown = func(name engine.Atom, args []engine.Term, env *engine.Env) {
		var sb strings.Builder
		s := engine.NewOutputTextStream(&sb)
		_, _ = engine.WriteTerm(&i.VM, s, name.Apply(args...), engine.List(engine.NewAtom("quoted").Apply(engine.NewAtom("true"))), engine.Success, env).Force(context.Background())
		log.Printf("UNKNOWN %s", &sb)
	}

	// Consult arguments.
	if err := i.QuerySolution(`consult(?).`, flag.Args()).Err(); err != nil {
		log.Panic(err)
	}

	ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt)
	defer stop()

	var buf strings.Builder
	keys := bufio.NewReader(os.Stdin)
	for {
		switch err := handleLine(ctx, &buf, i, t, keys); err {
		case nil:
			break
		case io.EOF:
			return
		default:
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
	_, _ = buf.WriteString("\n")

	sols, err := p.QueryContext(ctx, buf.String())
	switch err {
	case nil:
		buf.Reset()
		t.SetPrompt(prompt)
	case io.EOF:
		// Returns without resetting buf.
		t.SetPrompt(contPrompt)
		return nil
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

		m := map[string]prolog.TermString{}
		_ = sols.Scan(m)

		var buf bytes.Buffer
		if len(m) == 0 {
			_, _ = fmt.Fprintf(&buf, "%t", true)
		} else {
			ls := make([]string, 0, len(m))
			for v, t := range m {
				ls = append(ls, fmt.Sprintf("%s = %s", v, t))
			}
			sort.Strings(ls)
			_, _ = fmt.Fprint(&buf, strings.Join(ls, ",\n"))
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

type userInput struct {
	t   *terminal.Terminal
	buf bytes.Buffer
}

func (u *userInput) Read(p []byte) (n int, err error) {
	if u.buf.Len() == 0 {
		u.t.SetPrompt(userInputPrompt)
		defer u.t.SetPrompt(prompt)
		line, err := u.t.ReadLine()
		if err != nil {
			return 0, err
		}
		u.buf.WriteString(line + "\n")
	}

	return u.buf.Read(p)
}

func (u *userInput) Write(b []byte) (n int, err error) {
	return 0, nil
}
