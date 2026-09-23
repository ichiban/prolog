// Command 1pl is a Prolog top level for exercising github.com/ichiban/prolog.
//
//	1pl [file...]
//
// Files given on the command line are consulted before the first prompt. With
// stdin redirected the banner is dropped and input is read line by line, which
// makes a session scriptable:
//
//	printf 'member(X, [a,b,c]).\n;\n;\n' | 1pl
//
// A query reaches the whole file system rather than a subtree of it, and cd/1
// moves the directory that relative paths resolve against.
package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"io/fs"
	"os"
	"os/signal"
	"runtime/debug"

	"golang.org/x/term"

	"github.com/ichiban/prolog/v2"
)

const banner = `1pl %s -- a top level for ichiban/prolog, for testing purposes only.
See https://github.com/ichiban/prolog for more details.
Type 'halt.' or Ctrl-D to exit, ';' for another solution, Ctrl-C to interrupt.
Use cd(Dir) to move around, cd(Dir) with Dir unbound to see where you are.

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
	i := prolog.New()
	if err := errors.Join(
		i.MountFS("", osFS{}),
		i.SetUserInput(os.Stdin),
		i.SetUserOutput(os.Stdout),
		i.Register("cd", cd),
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

// osFS is the whole file system rather than a subtree of it. 1pl is a testing
// tool, so a query gets to read whatever the person running it can read, and a
// file to consult can sit anywhere. Relative names resolve against the process
// working directory, which cd/1 moves.
//
// Deliberately not prolog.RootFS, and not an fs.FS in good standing either:
// fs.ValidPath rejects the absolute and "../" names that make this useful.
// Nothing in the interpreter demands valid paths; it hands the name it was
// given straight to Open.
type osFS struct{}

func (osFS) Open(name string) (fs.File, error) {
	return os.Open(name)
}

// cd(Dir) changes the working directory every relative path in the session is
// resolved against, consult/1 and open/3 included. Called with Dir unbound it
// reports the current one instead, so there's no second predicate to remember.
func cd(_ context.Context, a prolog.Activation, dir prolog.Term) prolog.Outcome {
	if a.Variable(dir) {
		wd, err := os.Getwd()
		if err != nil {
			return a.Error(err)
		}
		here, err := a.NewAtom(prolog.Atom(wd))
		if err != nil {
			return a.Error(err)
		}
		return a.Unification(dir, here)
	}

	name, err := a.Atom(dir) // Raises the type error for a non-atom itself.
	if err != nil {
		return a.Error(err)
	}
	if err := os.Chdir(string(name)); err != nil {
		return a.Error(err)
	}
	return a.Success()
}
