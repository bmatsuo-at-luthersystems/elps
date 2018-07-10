package repl

import (
	"bytes"
	"fmt"
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

// RunRepl runs a simple repl
func RunRepl(prompt string) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
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

	rl, err := readline.New(prompt)
	if err != nil {
		panic(err)
	}

	var eof bool

	p := rdparser.NewInteractive(nil)
	p.Read = func() []*token.Token {
		prompt := p.Prompt()
		rl.SetPrompt(prompt)
		for {
			var line []byte
			line, err = rl.ReadSlice()
			if err != nil && err != readline.ErrInterrupt {
				// Set eof so the parser breaks out of its loop.
				eof = true
				return []*token.Token{&token.Token{
					Type: token.EOF,
					Text: "",
				}}
			}
			if err == readline.ErrInterrupt {
				line = nil
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
		expr, err := p.ParseExpression()
		if err != nil {
			fmt.Fprintln(env.Runtime.Stderr, err)
			if eof {
				break
			}
		}
		val := env.Eval(expr)
		fmt.Fprintln(env.Runtime.Stderr, val)
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
