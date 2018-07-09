package token

import "fmt"

// Source is an abstract stream of tokens which allows one token lookahead.
type Source interface {
	// Token returns the current token.  Token returns nil if Scan has not been
	// called.
	Token() *Token
	// Peek returns the next token in the stream.  At the end of the stream
	// Peek should return a value to indicate the lack of a token (EOF).
	Peek() *Token
	// Scan advances the token stream if possible.  If there are no tokens
	// remaining Scan returns false.
	Scan() bool
}

type Token struct {
	Type   Type
	Text   string
	Source *Location
}

type Type uint

// Type constants used for the elps lexer/parser.  These constants aren't
// necessary to use the package.
const (
	INVALID Type = iota
	ERROR
	EOF

	// Atomic expressions & literals
	SYMBOL
	INT
	FLOAT
	STRING
	STRING_RAW

	COMMENT

	// Operators
	NEGATIVE // arithmetic negation is parsed specially
	QUALIFY
	QUOTE
	UNBOUND

	// Delimiters
	PAREN_L
	PAREN_R
	BRACE_L
	BRACE_R

	numTokenTypes
)

func (typ Type) String() string {
	typeStrings := [numTokenTypes]string{
		INVALID:    "invalid",
		ERROR:      "error",
		EOF:        "EOF",
		SYMBOL:     "symbol",
		INT:        "int",
		FLOAT:      "float",
		STRING:     "string",
		STRING_RAW: "raw-string",
		COMMENT:    ";",
		NEGATIVE:   "-",
		QUALIFY:    ":",
		QUOTE:      "'",
		UNBOUND:    "#^",
		PAREN_L:    "(",
		PAREN_R:    ")",
		BRACE_L:    "[",
		BRACE_R:    "]",
	}
	if typ >= numTokenTypes {
		return typeStrings[INVALID]
	}
	return typeStrings[typ]
}

type Location struct {
	File string
	Pos  int
	Line int // line number (starting at 1 when tracked)
	Col  int // line column number (starting at 1 when tracked)
}

func (loc *Location) String() string {
	switch {
	case loc.Pos < 0:
		return fmt.Sprintf("%s", loc.File)
	case loc.Line == 0:
		return fmt.Sprintf("%s[%d]", loc.File, loc.Pos)
	case loc.Col == 0:
		return fmt.Sprintf("%s:%d", loc.File, loc.Line)
	default:
		return fmt.Sprintf("%s:%d:%d", loc.File, loc.Line, loc.Col)
	}
}

type LocationError struct {
	Err    error
	Source *Location
}

func (err *LocationError) Error() string {
	return fmt.Sprintf("%s: %s", err.Source, err.Err)
}
