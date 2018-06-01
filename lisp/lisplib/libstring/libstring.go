package libstring

import (
	"bytes"
	"fmt"
	"strings"

	"bitbucket.org/luthersystems/elps/lisp"
	"bitbucket.org/luthersystems/elps/lisp/lisplib/internal/libutil"
)

// DeafultPackageName is the package name used by LoadPackage.
const DefaultPackageName = "string"

// LoadPackage adds the math package to env
func LoadPackage(env *lisp.LEnv) *lisp.LVal {
	name := lisp.Symbol(DefaultPackageName)
	e := env.DefinePackage(name)
	if !e.IsNil() {
		return e
	}
	e = env.InPackage(name)
	if !e.IsNil() {
		return e
	}
	for _, fn := range builtins {
		env.AddBuiltins(true, fn)
	}
	return lisp.Nil()
}

var builtins = []*libutil.Builtin{
	libutil.Function("format", lisp.Formals("format-string", lisp.VarArgSymbol, "values"), builtinFormat),
}

func builtinFormat(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	format := args.Cells[0]
	fvals := args.Cells[1:]
	if format.Type != lisp.LString {
		return env.Errorf("first argument is not a string")
	}
	parts, err := parseFormatString(format.Str)
	if err != nil {
		return env.Error(err)
	}
	var buf bytes.Buffer
	anonIndex := 0
	for _, p := range parts {
		if strings.HasPrefix(p, "{") && strings.HasSuffix(p, "}") {
			p = strings.Join(strings.Fields(p), "")
			// TODO:  Allow non-empty formatting directives
			if p != "{}" {
				return env.Errorf("formatting direcives must be empty")
			}
			if anonIndex >= len(fvals) {
				return env.Errorf("too many formatting direcives for supplied values")
			}
			val := fvals[anonIndex]
			if val.Type == lisp.LString {
				buf.WriteString(val.Str)
			} else {
				buf.WriteString(val.String())
			}
			anonIndex++
		} else {
			buf.WriteString(p)
		}
	}

	return lisp.String(buf.String())
}

func parseFormatString(f string) ([]string, error) {
	var s []string
	tokens := tokenizeFormatString(f)
	for len(tokens) > 0 {
		tok := tokens[0]
		if tok.typ == formatText {
			s = append(s, tok.text)
			tokens = tokens[1:]
			continue
		}
		if tok.typ == formatClose {
			if len(tokens) == 0 || tokens[0].typ != formatClose {
				return nil, fmt.Errorf("unexpected closing brace '}' outside of formatting direcive")
			}
			s = append(s, "}")
			tokens = tokens[2:]
		}
		if len(tokens) < 2 {
			return nil, fmt.Errorf("unclosed formatting directive")
		}
		switch tokens[1].typ {
		case formatOpen:
			s = append(s, "{")
			tokens = tokens[2:]
			continue
		case formatClose:
			s = append(s, "{}")
			tokens = tokens[2:]
			continue
		case formatText:
			if len(tokens) < 3 {
				return nil, fmt.Errorf("unclosed formatting directive")
			}
			if tokens[2].typ != formatClose {
				return nil, fmt.Errorf("invalid formatting directive")
			}
			s = append(s, "{"+tokens[1].text+"}")
			tokens = tokens[3:]
			continue
		default:
			panic("unknown type")
		}
	}
	return s, nil
}

func tokenizeFormatString(f string) []formatToken {
	var tokens []formatToken
	for {
		i := strings.IndexAny(f, "{}")
		if i < 0 {
			tokens = append(tokens, formatToken{formatText, f})
			return tokens
		}
		if i > 0 {
			tokens = append(tokens, formatToken{formatText, f[:i]})
			f = f[i:]
		}
		if f[0] == '{' {
			tokens = append(tokens, formatToken{formatOpen, "{"})
			f = f[1:]
		} else {
			tokens = append(tokens, formatToken{formatClose, "}"})
			f = f[1:]
		}
	}
}

type formatTokenType uint

const (
	formatText formatTokenType = iota
	formatOpen
	formatClose
)

type formatToken struct {
	typ  formatTokenType
	text string
}
