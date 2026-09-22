// Command 1pl is a Prolog top level for exercising github.com/ichiban/prolog.
//
//	1pl [file...]
//
// Files given on the command line are consulted before the first prompt. With
// stdin redirected the banner is dropped and input is read line by line, which
// makes a session scriptable:
//
//	printf 'member(X, [a,b,c]).\n;\n;\n' | 1pl
package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"os"
	"os/signal"
	"runtime/debug"

	"golang.org/x/term"

	"github.com/ichiban/prolog/v2"
)

const banner = `1pl %s -- a top level for ichiban/prolog, for testing purposes only.
See https://github.com/ichiban/prolog for more details.
Type 'halt.' or Ctrl-D to exit, ';' for another solution, Ctrl-C to interrupt.

`

func main() {
	if err := run(); err != nil {
		fmt.Fprintf(os.Stderr, "1pl: %v\n", err)
		os.Exit(1)
	}
}

func run() error {
	flag.Parse()

	if term.IsTerminal(int(os.Stdin.Fd())) {
		fmt.Printf(banner, version())
	}

	i, err := newInterpreter()
	if err != nil {
		return err
	}

	// Ctrl-C interrupts the running query rather than the session, so the
	// signal is watched for the whole run and consumed per query.
	sigs := make(chan os.Signal, 1)
	signal.Notify(sigs, os.Interrupt)
	defer signal.Stop(sigs)

	ctx := context.Background()
	for _, file := range flag.Args() {
		if err := i.Load(ctx, "", file); err != nil {
			return fmt.Errorf("failed to load %s: %w", file, err)
		}
	}

	return (&repl{i: i, c: newConsole(), sigs: sigs}).loop(ctx)
}

func newInterpreter() (*prolog.Interpreter, error) {
	root, err := os.OpenRoot(".")
	if err != nil {
		return nil, err
	}

	i := prolog.New()
	if err := errors.Join(
		i.MountFS("", prolog.RootFS{Root: root}),
		i.SetUserInput(os.Stdin),
		i.SetUserOutput(os.Stdout),
	); err != nil {
		return nil, err
	}
	i.SetWarn(func(err error) {
		fmt.Printf("%% warning: %v\n", err)
	})
	i.SetHalt(func(code int) {
		os.Exit(code)
	})
	return i, nil
}

func version() string {
	info, ok := debug.ReadBuildInfo()
	if !ok {
		return "(unknown)"
	}
	return info.Main.Version
}
