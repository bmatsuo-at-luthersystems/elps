package lexer

import (
	"fmt"
	"io"
	"strings"
	"unicode"

	"bitbucket.org/luthersystems/elps/parser/token"
)

const miscWordRunes = "0123456789" + miscWordSymbols
const miscWordSymbols = "._+-*/=<>!&~%?$"

const megabyte = 1024 * 1024

type Lexer struct {
	scanner *token.Scanner
	ch      rune // current unicode rune

	// readErr is a reader from
	readErr error
}

func New(s *token.Scanner) *Lexer {
	lex := &Lexer{
		scanner: s,
	}
	return lex
}

func (lex *Lexer) NextToken() *token.Token {
	if lex.readErr != nil {
		return lex.emitError(lex.readErr, true)
	}
	lex.readErr = lex.skipWhitespace()
	if lex.readErr != nil {
		return lex.emitError(lex.readErr, true)
	}
	lex.readChar()
	if lex.readErr != nil {
		return lex.emitError(lex.readErr, true)
	}
	switch lex.ch {
	case '(':
		return lex.charToken(token.PAREN_L)
	case ')':
		return lex.charToken(token.PAREN_R)
	case '[':
		return lex.charToken(token.BRACE_L)
	case ']':
		return lex.charToken(token.BRACE_R)
	case ':':
		return lex.charToken(token.QUALIFY)
	case '\'':
		return lex.charToken(token.QUOTE)
	case ';':
		for lex.peekRune() != '\n' {
			err := lex.readChar()
			if err == io.EOF {
				return lex.scanner.EmitToken(token.COMMENT)
			}
			if err != nil {
				return lex.emitError(err, false)
			}
		}
		if lex.readChar() != nil {
			return lex.emit(token.ERROR, lex.readErr.Error())
		}
		return lex.scanner.EmitToken(token.COMMENT)
	case '#':
		if lex.readChar() != nil {
			return lex.emit(token.ERROR, lex.readErr.Error())
		}
		switch lex.ch {
		case '^':
			tok := lex.scanner.EmitToken(token.UNBOUND)
			if unicode.IsSpace(lex.peekRune()) {
				return lex.errorf("whitespace following %s", tok.Text)
			}
			return tok
		default:
			lex.scanner.Ignore()
			return lex.errorf("invalid meta character %q", lex.ch)
		}
	case '-':
		if unicode.IsSpace(lex.peekRune()) {
			return lex.scanner.EmitToken(token.SYMBOL)
		}
		return lex.scanner.EmitToken(token.NEGATIVE)
	case '"':
		n := 0
		for lex.peekRune() != '"' {
			n++
			err := lex.readChar()
			if err != nil {
				return lex.emitError(err, false)
			}
			switch lex.ch {
			case '\n':
				return lex.errorf("invalid-string", "unterminated string literal")
			case '\\':
				// Wait until parsing to check the escaped character
				err := lex.readChar()
				if err != nil {
					return lex.emitError(err, false)
				}
			}
		}
		if lex.peekRune() != '"' {
			return lex.errorf("syntax-error", "unexpected rune %q", lex.peekRune())
		}
		err := lex.readChar()
		if err != nil {
			return lex.emitError(err, false)
		}
		if n > 0 || lex.peekRune() != '"' {
			// This was a normal string (possibly empty)
			return lex.scanner.EmitToken(token.STRING)
		}
		// This is a raw string -- consume the third quote.
		err = lex.readChar()
		if err != nil {
			return lex.emitError(err, false)
		}
		for {
			if lex.peekRune() == '"' {
				err = lex.readChar()
				if err != nil {
					return lex.emitError(err, false)
				}
				if lex.peekRune() == '"' {
					err = lex.readChar()
					if err != nil {
						return lex.emitError(err, false)
					}
					if lex.peekRune() == '"' {
						err = lex.readChar()
						if err != nil {
							return lex.emitError(err, false)
						}
						return lex.scanner.EmitToken(token.STRING_RAW)
					}
				}
			}

			err = lex.readChar()
			if err != nil {
				return lex.emitError(err, false)
			}
		}
	default:
		if isDigit(lex.ch) {
			return lex.readNumber()
		}

		if isWordStart(lex.ch) {
			err := lex.readSymbol()
			if err != nil {
				return lex.emit(token.ERROR, err.Error())
			}
			return lex.scanner.EmitToken(token.SYMBOL)
		}

		lex.readErr = fmt.Errorf("unexpected text starting with %q", lex.ch)
		return lex.emit(token.INVALID, lex.readErr.Error())
	}
}

func (lex *Lexer) emit(typ token.Type, text string) *token.Token {
	tok := &token.Token{
		Type:   typ,
		Text:   text,
		Source: lex.scanner.LocStart(),
	}
	lex.scanner.Ignore()
	return tok
}

func (lex *Lexer) emitError(err error, expectEOF bool) *token.Token {
	if err == io.EOF {
		if expectEOF {
			return lex.emit(token.EOF, "")
		}
		return lex.emit(token.ERROR, "unexpected EOF")
	}
	return lex.emit(token.ERROR, err.Error())
}

func (lex *Lexer) errorf(format string, v ...interface{}) *token.Token {
	return lex.emitError(fmt.Errorf(format, v...), false)
}

func (lex *Lexer) charToken(typ token.Type) *token.Token {
	tok := lex.scanner.EmitToken(typ)
	return tok
}

func (lex *Lexer) readSymbol() error {
	for isWord(lex.peekRune()) {
		err := lex.readChar()
		if err != nil {
			return err
		}
	}
	return nil
}

func (lex *Lexer) readNumber() *token.Token {
	// TODO: support octal and hex integer literals

	for isDigit(lex.peekRune()) {
		err := lex.readChar()
		if err != nil {
			return lex.emitError(err, false)
		}
	}
	switch lex.peekRune() {
	case '.':
		err := lex.readChar()
		if err != nil {
			return lex.emitError(err, false)
		}
		return lex.readFloatFraction()
	case 'e', 'E':
		err := lex.readChar()
		if err != nil {
			return lex.emitError(err, false)
		}
		return lex.readFloatExponent()
	default:
		return lex.scanner.EmitToken(token.INT)
	}
	// the returned string may not actually be a usable number (overflow), but
	// we can find that out at parse time -- not scan time.
}

func (lex *Lexer) readFloatFraction() *token.Token {
	if !isDigit(lex.peekRune()) {
		return lex.errorf("invalid floating point literal: %v", lex.scanner.Text())
	}
	for isDigit(lex.peekRune()) {
		err := lex.readChar()
		if err != nil {
			return lex.emitError(err, false)
		}
	}
	switch lex.peekRune() {
	case 'e', 'E':
		err := lex.readChar()
		if err != nil {
			return lex.emitError(err, false)
		}
		return lex.readFloatExponent()
	default:
		return lex.scanner.EmitToken(token.FLOAT)
	}
}

func (lex *Lexer) readFloatExponent() *token.Token {
	switch lex.peekRune() {
	case '+', '-':
		err := lex.readChar()
		if err != nil {
			return lex.emitError(err, false)
		}
	}
	if !isDigit(lex.peekRune()) {
		return lex.errorf("invalid floating point literal: %v", lex.scanner.Text())
	}
	for isDigit(lex.peekRune()) {
		err := lex.readChar()
		if err != nil {
			return lex.emitError(err, false)
		}
	}
	return lex.scanner.EmitToken(token.FLOAT)
}

func (lex *Lexer) skipWhitespace() error {
	for unicode.IsSpace(lex.peekRune()) {
		err := lex.readChar()
		if err != nil {
			return err
		}
	}
	lex.scanner.Ignore()
	return nil
}

func (lex *Lexer) peekRune() rune {
	r, _ := lex.scanner.Peek()
	return r
}

func (lex *Lexer) readChar() error {
	lex.readErr = lex.scanner.ScanRune()
	if lex.readErr != nil {
		return lex.readErr
	}
	lex.ch = lex.scanner.Rune()
	return nil
}

type InvalidUTF8Error struct {
	File string
	Pos  int
	Line int // line number (starting at 1 when tracked)
}

func (err *InvalidUTF8Error) Error() string {
	if err.Line > 0 {
		return fmt.Sprintf("%s:%d: invalid utf-8 rune", err.File, err.Pos)
	}
	return fmt.Sprintf("%s: invalid utf-8 rune at byte %d", err.File, err.Pos)
}

func isLetter(c rune) bool {
	return unicode.IsLetter(c) || c == '_'
}

func isWordStart(c rune) bool {
	return unicode.IsLetter(c) || strings.ContainsRune(miscWordSymbols, c)
}

func isWord(c rune) bool {
	return unicode.IsLetter(c) || strings.ContainsRune(miscWordRunes, c)
}

func isDigit(c rune) bool {
	return '0' <= c && c <= '9'
}
