package main

import (
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/ichiban/prolog/v2"
)

const (
	prompt     = "?- "
	contPrompt = "|  "
)

// repl is the read-solve-print loop.
type repl struct {
	i    *prolog.Interpreter
	c    console
	sigs <-chan os.Signal

	// pending holds the term being read. A term can span several lines, so a
	// line that doesn't finish one is kept here and the next line appended.
	pending strings.Builder
}

func (r *repl) loop(ctx context.Context) error {
	for {
		p := prompt
		if r.pending.Len() > 0 {
			p = contPrompt
		}

		switch line, err := r.c.ReadLine(p); {
		case errors.Is(err, io.EOF):
			fmt.Println()
			return nil
		case err != nil:
			return err
		default:
			fmt.Fprintf(&r.pending, "%s\n", line)
		}

		if strings.TrimSpace(r.pending.String()) == "" {
			r.pending.Reset()
			continue
		}
		if r.solve(ctx, r.pending.String()) {
			r.pending.Reset()
		}
	}
}

// solve runs one query, showing each solution and asking whether to look for
// the next. It reports whether the term was complete; an unfinished one is left
// in r.pending for the next line to continue.
func (r *repl) solve(ctx context.Context, query string) (complete bool) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	select { // Forget a Ctrl-C that arrived while nothing was running.
	case <-r.sigs:
	default:
	}
	go r.interrupt(ctx, cancel)

	var (
		names []prolog.VariableName
		found bool
	)
	for solution, err := range r.i.Query[map[string]prolog.Raw](ctx, query, prolog.VariableNames(&names)) {
		switch {
		case err == nil:
		case ctx.Err() != nil:
			fmt.Println("\n% interrupted")
			return true
		case !found && errors.Is(err, io.EOF):
			return false // The term stops mid-air; read another line.
		default:
			fmt.Printf("ERROR: %v\n", err)
			return true
		}

		found = true
		if !r.show(solution, names) {
			return true
		}
	}

	// Either nothing succeeded, or the last solution was rejected and no other
	// one came. Both are a failure of the query as asked.
	fmt.Println("false.")
	return true
}

// show prints one solution and asks whether to look for another, the way a
// Prolog top level does: ';' or space for the next one, anything else to stop.
func (r *repl) show(solution map[string]prolog.Raw, names []prolog.VariableName) bool {
	var bindings []string
	for _, n := range names {
		// A variable the writer named '_' or '_Something' is theirs to ignore,
		// and one left unbound has nothing to report.
		if t, ok := solution[n.Name]; ok && !strings.HasPrefix(n.Name, "_") {
			bindings = append(bindings, fmt.Sprintf("%s = %s", n.Name, t))
		}
	}
	if len(bindings) == 0 {
		bindings = []string{"true"}
	}
	fmt.Print(strings.Join(bindings, ",\n"))

	// ponytail: every solution is offered, even one the engine already knows is
	// the last, because the engine doesn't say whether choice points remain.
	// Show a bare '.' once Interpreter.Query reports determinism.
	switch key, err := r.c.ReadKey(); {
	case err == nil && (key == ';' || key == ' '):
		fmt.Println(" ;")
		return true
	default:
		fmt.Println(".")
		return false
	}
}

// interrupt cancels the running query on Ctrl-C, leaving the loop itself alive.
func (r *repl) interrupt(ctx context.Context, cancel context.CancelFunc) {
	select {
	case <-r.sigs:
		cancel()
	case <-ctx.Done():
	}
}
