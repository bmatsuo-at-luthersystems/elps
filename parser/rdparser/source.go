package rdparser

import (
	"bitbucket.org/luthersystems/elps/parser/lexer"
	"bitbucket.org/luthersystems/elps/parser/token"
)

type TokenSource struct {
	lex   *lexer.Lexer
	Token *token.Token
	Peek  *token.Token
}

// TokenSource initializes and returns a new token.Source that scans tokens
// from scanner.
func NewTokenSource(scanner *token.Scanner) *TokenSource {
	lex := lexer.New(scanner)
	s := &TokenSource{
		lex: lex,
	}
	s.scan()
	return s
}

func (s *TokenSource) Accept(fn func(*token.Token) bool) bool {
	if fn(s.Peek) {
		s.scan()
		return true
	}
	return false
}

func (s *TokenSource) AcceptType(typ ...token.Type) bool {
	for _, typ := range typ {
		if s.Peek.Type == typ {
			s.scan()
			return true
		}
	}
	return false
}

func (s *TokenSource) Scan() bool {
	if s.IsEOF() {
		s.Token = s.Peek
		return false
	}
	s.scan()
	return true
}

func (s *TokenSource) IsEOF() bool {
	return s.Peek.Type == token.EOF
}

func (s *TokenSource) scan() {
	s.Token = s.Peek
	s.Peek = s.lex.NextToken()
}
