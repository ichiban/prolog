package main

import (
	"bufio"
	"context"
	"errors"
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

	"github.com/ichiban/prolog/v2"
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

	if terminal.IsTerminal(0) {
		oldState, err := terminal.MakeRaw(0)
		if err != nil {
			log.Panicf("failed to enter raw mode: %v", err)
		}
		restore := func() {
			_ = terminal.Restore(0, oldState)
		}
		defer restore()
	}

	t := terminal.NewTerminal(os.Stdin, prompt)
	defer fmt.Printf("\r\n")

	log.SetOutput(t)

	r, err := os.OpenRoot(".")
	if err != nil {
		log.Fatalf("failed to open root: %v", err)
	}

	i := prolog.New(
		prolog.HeapSize(5*1024),
		prolog.Root(r),
	)
	if err := i.SetUserInput(os.Stdin); err != nil {
		log.Fatalf("failed to set user input: %v", err)
	}
	if err := i.SetUserOutput(os.Stdout); err != nil {
		log.Fatalf("failed to set user output: %v", err)
	}

	ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt, os.Kill)
	defer stop()

	for _, arg := range flag.Args() {
		if err := i.Load(ctx, arg); err != nil {
			log.Fatalf("failed to load %s: %v", arg, err)
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

func handleLine(ctx context.Context, buf *strings.Builder, i *prolog.Interpreter, t *terminal.Terminal, keys *bufio.Reader) (err error) {
	line, err := t.ReadLine()
	if err != nil {
		return err
	}
	_, _ = fmt.Fprintf(buf, "%s\n", line)

	var resultShown bool
	for result, err := range prolog.Query[prolog.Result](ctx, i, buf.String()) {
		switch {
		case err == nil:
			buf.Reset()
			t.SetPrompt(prompt)
		case errors.Is(err, io.EOF):
			// Returns without resetting buf.
			t.SetPrompt(contPrompt)
			return nil
		default:
			log.Printf("failed to query: %v", err)
			buf.Reset()
			t.SetPrompt(prompt)
			return nil
		}

		if len(result) == 0 {
			_, _ = fmt.Fprintf(t, "%t\n", true)
		} else {
			ls := make([]string, 0, len(result))
			for v, t := range result {
				ls = append(ls, fmt.Sprintf("%s = %s", v, t))
			}
			sort.Strings(ls)
			_, _ = fmt.Fprintf(t, "%s\n", strings.Join(ls, ",\n"))
		}
		resultShown = true
	}
	if !resultShown {
		_, _ = fmt.Fprintf(t, "%t\n", false)
	}

	return nil
}
