package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strings"
	"unicode"

	"golang.org/x/term"
)

// console is the input side of the top level. Output isn't part of it: answers,
// warnings and whatever the program itself writes all go straight to os.Stdout
// so that they stay in order.
type console interface {
	// ReadLine shows prompt and reads one line of input.
	ReadLine(prompt string) (string, error)

	// ReadKey reads a single answer to the "more solutions?" prompt.
	ReadKey() (rune, error)
}

func newConsole() console {
	if !term.IsTerminal(int(os.Stdin.Fd())) {
		return &pipe{in: bufio.NewReader(os.Stdin)}
	}
	return &tty{t: term.NewTerminal(struct {
		io.Reader
		io.Writer
	}{os.Stdin, os.Stdout}, "")}
}

// tty reads through golang.org/x/term, which gives line editing and history.
// Raw mode is entered only while a key is being read: while a query runs the
// terminal stays cooked, so Ctrl-C raises SIGINT and a newline the program
// writes still returns the carriage.
type tty struct {
	t *term.Terminal
}

func (c *tty) ReadLine(prompt string) (string, error) {
	c.t.SetPrompt(prompt)
	restore, err := makeRaw()
	if err != nil {
		return "", err
	}
	defer restore()
	return c.t.ReadLine()
}

func (c *tty) ReadKey() (rune, error) {
	restore, err := makeRaw()
	if err != nil {
		return 0, err
	}
	defer restore()

	// ponytail: reads os.Stdin behind the Terminal's back, so type-ahead it
	// already buffered is missed. Only bites when input is pasted, not typed.
	var b [1]byte
	if _, err := os.Stdin.Read(b[:]); err != nil {
		return 0, err
	}
	return rune(b[0]), nil
}

func makeRaw() (func(), error) {
	fd := int(os.Stdin.Fd())
	state, err := term.MakeRaw(fd)
	if err != nil {
		return nil, err
	}
	return func() { _ = term.Restore(fd, state) }, nil
}

// pipe reads a redirected stdin line by line, so a session can be scripted.
// Input is echoed, since nothing else does it, which makes a captured session
// read like an interactive one. A line answering the "more solutions?" prompt
// is read whole and only its first non-blank character counts.
type pipe struct {
	in *bufio.Reader
}

func (c *pipe) ReadLine(prompt string) (string, error) {
	fmt.Print(prompt)
	line, err := c.line()
	if line == "" {
		fmt.Println()
		return "", err
	}
	fmt.Println(line)
	return line, nil
}

func (c *pipe) ReadKey() (rune, error) {
	// Not echoed: the caller prints the answer it settled on either way.
	line, err := c.line()
	if err != nil {
		return 0, err
	}
	for _, r := range line {
		if !unicode.IsSpace(r) {
			return r, nil
		}
	}
	return '\n', nil
}

func (c *pipe) line() (string, error) {
	line, err := c.in.ReadString('\n')
	if line == "" {
		return "", err
	}
	return strings.TrimRight(line, "\r\n"), nil
}
