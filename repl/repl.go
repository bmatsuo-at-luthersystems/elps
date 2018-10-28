package repl

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"strings"

	"bitbucket.org/luthersystems/elps/lisp"
	"bitbucket.org/luthersystems/elps/lisp/lisplib"
	"bitbucket.org/luthersystems/elps/parser"
	"bitbucket.org/luthersystems/elps/parser/lexer"
	"bitbucket.org/luthersystems/elps/parser/rdparser"
	"bitbucket.org/luthersystems/elps/parser/token"
	"github.com/chzyer/readline"
)

// RunRepl runs a simple repl in a vanilla elps environment.
func RunRepl(prompt string) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}
	rc := lisp.InitializeUserEnv(env)
	if !rc.IsNil() {
		errlnf("Language initialization failure: %v", rc)
		os.Exit(1)
	}
	rc = lisplib.LoadLibrary(env)
	if !rc.IsNil() {
		errlnf("Stdlib initialization failure: %v", rc)
		os.Exit(1)
	}
	rc = env.InPackage(lisp.String(lisp.DefaultUserPackage))
	if !rc.IsNil() {
		errlnf("No user package: %v", rc)
		os.Exit(1)
	}

	RunEnv(env, prompt, strings.Repeat(" ", len(prompt)))
}

// RunEnv runs a simple repl with env as a root environment.
func RunEnv(env *lisp.LEnv, prompt, cont string) {
	if env.Parent != nil {
		errlnf("REPL environment is not a root environment.")
		os.Exit(1)
	}

	p := rdparser.NewInteractive(nil)
	p.SetPrompts(prompt, cont)
	rl, err := readline.NewEx(&readline.Config{
		Prompt: p.Prompt(),
	})
	if err != nil {
		panic(err)
	}
	defer rl.Close()

	p.Read = func() []*token.Token {
		rl.SetPrompt(p.Prompt())
		for {
			var line []byte
			line, err = rl.ReadSlice()
			if err != nil && err != readline.ErrInterrupt {
				return []*token.Token{&token.Token{
					Type: token.EOF,
					Text: "",
				}}
			}
			if err == readline.ErrInterrupt {
				line = nil
				continue
			}
			line = bytes.TrimSpace(line)
			if len(line) == 0 {
				continue
			}
			var tokens []*token.Token
			scanner := token.NewScanner("stdin", bytes.NewReader(line))
			lex := lexer.New(scanner)
			for {
				tok := lex.ReadToken()
				if len(tok) != 1 {
					panic("bad tokens")
				}
				if tok[0].Type == token.EOF {
					return tokens
				}
				tokens = append(tokens, tok...)
				if tok[0].Type == token.ERROR {
					// This will work itself out eventually...
					return tokens
				}
			}
		}
	}

	for {
		expr, err := p.Parse()
		if err == io.EOF {
			break
		}
		if err != nil {
			fmt.Fprintln(env.Runtime.Stderr, err)
			continue
		}
		val := env.Eval(expr)
		if val.Type == lisp.LError {
			(*lisp.ErrorVal)(val).WriteTrace(os.Stderr)
		} else {
			fmt.Fprintln(env.Runtime.Stderr, val)
		}
	}
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
