package main

import (
	"bufio"
	"bytes"
	"context"
	"flag"
	"fmt"
	"github.com/ichiban/prolog/internal"
	"io"
	"log"
	"os"
	"os/signal"
	"runtime/debug"
	"sort"
	"strings"

	"golang.org/x/crypto/ssh/terminal"

	"github.com/ichiban/prolog"
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

	halt := prolog.Halt
	if terminal.IsTerminal(0) {
		oldState, err := terminal.MakeRaw(0)
		if err != nil {
			log.Panicf("failed to enter raw mode: %v", err)
		}
		restore := func() {
			_ = terminal.Restore(0, oldState)
		}
		defer restore()

		halt = func(vm *internal.VM, n internal.Term, k internal.Cont, env *internal.Env) *internal.Promise {
			restore()
			return prolog.Halt(vm, n, k, env)
		}
	}

	t := terminal.NewTerminal(os.Stdin, prompt)
	defer fmt.Printf("\r\n")

	log.SetOutput(t)

	i := prolog.New(&userInput{t: t}, t)
	if err := i.QuerySolution(`use_module(library(prologue)).`).Err(); err != nil {
		log.Panic(err)
	}
	i.SetPredicate1("cd", func(vm *internal.VM, path internal.Term, k internal.Cont, env *internal.Env) *internal.Promise {
		var p string
		switch path := env.Resolve(path).(type) {
		case internal.Variable:
			return internal.Error(internal.InstantiationError(env))
		case internal.Atom:
			p = path.String()
		default:
			return internal.Error(internal.TypeError(internal.NewAtom("atom"), path, env))
		}
		if err := os.Chdir(p); err != nil {
			return internal.Error(err)
		}
		return k(env)
	})
	i.SetPredicate4("skip_max_list", prolog.SkipMaxList)
	i.SetPredicate2("go_string", func(vm *internal.VM, term, s internal.Term, k internal.Cont, env *internal.Env) *internal.Promise {
		return prolog.Unify(vm, s, internal.NewAtom(fmt.Sprintf("%#v", term)), k, env)
	})
	i.SetPredicate1("halt", halt)
	i.Unknown = func(name internal.Atom, args []internal.Term, env *internal.Env) {
		var sb strings.Builder
		s := internal.NewOutputTextStream(&sb)
		_, _ = prolog.WriteTerm(&i.VM, s, name.Apply(args...), internal.List(internal.NewAtom("quoted").Apply(internal.NewAtom("true"))), internal.Success, env).Force(context.Background())
		log.Printf("UNKNOWN %s", &sb)
	}

	ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt)
	defer stop()

	for _, arg := range flag.Args() {
		f, err := os.Open(arg)
		if err != nil {
			log.Panicf("open file error: %v", err)
		}
		b, err := io.ReadAll(f)
		if err != nil {
			log.Panicf("read file error: %v", err)
		}
		if err := i.LoadText(ctx, string(b)); err != nil {
			log.Panicf("load text error: %v", err)
		}
	}

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
