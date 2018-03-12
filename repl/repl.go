package repl

import (
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/bmatsuo/somelisp/lisp"
	"github.com/bmatsuo/somelisp/parser"
	"github.com/chzyer/readline"
)

// RunRepl runs a simple repl
func RunRepl(prompt string) {
	env := lisp.NewEnv(nil)
	env.AddBuiltins()

	rl, err := readline.New(prompt)
	if err != nil {
		panic(err)
	}
	contPrompt := strings.Repeat(" ", len(prompt)) // prompt had better be ascii...

	var buf []byte
	for {
		var line []byte
		line, err = rl.ReadSlice()
		if err != nil && err != readline.ErrInterrupt {
			break
		}
		if len(buf) != 0 {
			line = append(buf, line...)
			rl.SetPrompt(prompt)
		}
		complete, err := parser.Parse(env, line)
		if err != nil {
			errln(err)
			continue
		}
		if !complete {
			buf = line
			rl.SetPrompt(contPrompt)
		}
	}
	if err != io.EOF {
		errln(err)
		return
	}
	errln("done")
}

func errlnf(format string, v ...interface{}) {
	if strings.HasSuffix(format, "\n") {
		errf(format, v...)
		return
	}
	errf(format+"\n", v...)
}

func errln(v ...interface{}) {
	fmt.Fprintln(os.Stderr, v...)
}

func errf(format string, v ...interface{}) {
	fmt.Fprintf(os.Stderr, format, v...)
}
