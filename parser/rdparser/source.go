package rdparser

import (
	"bitbucket.org/luthersystems/elps/parser/lexer"
	"bitbucket.org/luthersystems/elps/parser/token"
)

// TokenStream is an arbitrary sequence of tokens.  Typically, a TokenStream
// will be a *lexer.Lexer but other implementations may be desirable for
// implementation a REPL or other dynamic environments.
type TokenStream interface {
	// NextToken returns the next token from an input source.  When no more
	// tokens can be generated NextToken returns a token with type token.EOF.
	NextToken() *token.Token
}

// TokenGenerator implements TokenStream.  The function will be called any time
// a TokenSource wants a token.
type TokenGenerator func() *token.Token

// NextToken implements TokenStream.
func (fn TokenGenerator) NextToken() *token.Token {
	return fn()
}

// TokenChannel returns a TokenStream that returns tokens receieved from c.
func TokenChannel(c <-chan *token.Token) TokenStream {
	pos := &token.Location{}
	return TokenGenerator(func() *token.Token {
		tok, ok := <-c
		if !ok {
			return &token.Token{
				Type:   token.EOF,
				Source: pos,
			}
		}
		pos = tok.Source
		return tok
	})
}

// TokenSource abstracts a TokenStream by adding "memory" and providing methods
// to process and branch off the stream's tokens.
type TokenSource struct {
	lex   TokenStream
	Token *token.Token
	peek  *token.Token
}

func NewTokenStreamSource(stream TokenStream) *TokenSource {
	return &TokenSource{
		lex: stream,
	}
}

// TokenSource initializes and returns a new token.Source that scans tokens
// from scanner.
func NewTokenSource(scanner *token.Scanner) *TokenSource {
	lex := lexer.New(scanner)
	return NewTokenStreamSource(lex)
}

func (s *TokenSource) Peek() *token.Token {
	if s.peek != nil {
		return s.peek
	}
	s.peek = s.lex.NextToken()
	return s.peek
}

func (s *TokenSource) Accept(fn func(*token.Token) bool) bool {
	if fn(s.Peek()) {
		s.scan()
		return true
	}
	return false
}

func (s *TokenSource) AcceptType(typ ...token.Type) bool {
	for _, typ := range typ {
		if s.Peek().Type == typ {
			s.scan()
			return true
		}
	}
	return false
}

func (s *TokenSource) Scan() bool {
	if s.IsEOF() {
		s.Token = s.peek
		return false
	}
	s.scan()
	return true
}

func (s *TokenSource) IsEOF() bool {
	return s.Peek().Type == token.EOF
}

func (s *TokenSource) scan() {
	s.Token = s.Peek()
	s.peek = nil
}
