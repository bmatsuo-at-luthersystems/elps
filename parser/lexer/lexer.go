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

type Lexer struct {
	scanner *token.Scanner

	// readErr is a reader from
	readErr error
}

func New(s *token.Scanner) *Lexer {
	lex := &Lexer{
		scanner: s,
	}
	return lex
}

func (lex *Lexer) ReadToken() []*token.Token {
	lex.skipWhitespace()
	if !lex.scanner.Accept(func(c rune) bool { return true }) {
		if lex.scanner.EOF() {
			return lex.emit(token.EOF, "")
		}
		err := lex.scanner.Err()
		if err != nil {
			lex.emitError(err, false)
		}
	}
	switch lex.scanner.Rune() {
	case '(':
		return lex.charToken(token.PAREN_L)
	case ')':
		return lex.charToken(token.PAREN_R)
	case '[':
		return lex.charToken(token.BRACE_L)
	case ']':
		return lex.charToken(token.BRACE_R)
	case '\'':
		return lex.charToken(token.QUOTE)
	case ':':
		return lex.readSymbol()
	case ';':
		lex.scanner.AcceptSeq(func(c rune) bool { return c != '\n' })
		return lex.emitText(token.COMMENT)
	case '#':
		lex.readChar()
		err := lex.scanner.Err()
		if err != nil {
			return lex.emitError(err, false)
		}
		switch lex.scanner.Rune() {
		case '^':
			tok := lex.emitText(token.UNBOUND)
			if unicode.IsSpace(lex.peekRune()) {
				return lex.errorf("whitespace following %s", tok[0].Text)
			}
			return tok
		default:
			lex.scanner.Ignore()
			return lex.errorf("invalid meta character %q", lex.scanner.Rune())
		}
	case '-':
		if unicode.IsSpace(lex.peekRune()) {
			return lex.emitText(token.SYMBOL)
		}
		return lex.emitText(token.NEGATIVE)
	case '"':
		n := 0
		for lex.scanner.AcceptSeq(func(c rune) bool { return c != '"' && c != '\n' }) != 0 {
			n++
			if lex.scanner.Accept(func(c rune) bool { return c == '\n' }) {
				return lex.errorf("unterminated string literal")
			}
			if lex.scanner.Rune() == '\\' {
				// Wait until parsing to check the escaped character
				if !lex.scanner.Accept(func(c rune) bool { return true }) {
					return lex.errorf("unterminated string literal %q", lex.peekRune())
				}
			}
		}
		if !lex.scanner.AcceptRune('"') {
			if lex.scanner.EOF() {
				return lex.errorf("unexpected EOF")
			}
			err := lex.scanner.Err()
			if err != nil {
				return lex.errorf("scan failure: %v", err)
			}
			return lex.errorf("unexpected rune %q", lex.peekRune())
		}
		if n > 0 {
			// This was a normal string
			return lex.emitText(token.STRING)
		}
		if !lex.scanner.AcceptRune('"') {
			// This is just an empty string -- not raw.
			return lex.emitText(token.STRING)
		}
		// This is a raw string
		for {
			_, ok := lex.scanner.AcceptString(`"""`)
			if ok {
				return lex.emitText(token.STRING_RAW)
			}
			if !lex.scanner.Accept(func(c rune) bool { return true }) {
				return lex.errorf("unterminated raw-string literal %q", lex.peekRune())
			}
		}
	default:
		if isDigit(lex.scanner.Rune()) {
			return lex.readNumber()
		}
		if isWordStart(lex.scanner.Rune()) {
			return lex.readSymbol()
		}
		err := fmt.Errorf("unexpected text starting with %q", lex.scanner.Rune())
		return lex.emit(token.INVALID, err.Error())
	}
}

func (lex *Lexer) emit(typ token.Type, text string) []*token.Token {
	tok := []*token.Token{&token.Token{
		Type:   typ,
		Text:   text,
		Source: lex.scanner.LocStart(),
	}}
	lex.scanner.Ignore()
	return tok
}

func (lex *Lexer) emitText(typ token.Type) []*token.Token {
	return []*token.Token{lex.scanner.EmitToken(typ)}
}

func (lex *Lexer) emitError(err error, expectEOF bool) []*token.Token {
	if err == io.EOF {
		if expectEOF {
			return lex.emit(token.EOF, "")
		}
		return lex.emit(token.ERROR, "unexpected EOF")
	}
	return lex.emit(token.ERROR, err.Error())
}

func (lex *Lexer) errorf(format string, v ...interface{}) []*token.Token {
	return lex.emitError(fmt.Errorf(format, v...), false)
}

func (lex *Lexer) charToken(typ token.Type) []*token.Token {
	tok := lex.scanner.EmitToken(typ)
	return []*token.Token{tok}
}

func (lex *Lexer) readSymbol() []*token.Token {
	lex.scanner.AcceptSeq(isWord)
	if lex.scanner.AcceptRune(':') {
		// This may produce an invalid symbol that should be detected during
		// parsing.
		return lex.readSymbol()
	}
	return lex.emitText(token.SYMBOL)
}

func (lex *Lexer) readNumber() []*token.Token {
	// TODO: support octal and hex integer literals
	lex.scanner.AcceptSeqDigit() // the first digit already scanned
	switch {
	case lex.scanner.AcceptRune('.'):
		return lex.readFloatFraction()
	case lex.scanner.AcceptAny("eE"):
		if !lex.scanner.Accept(func(c rune) bool { return true }) {
			return lex.errorf("invalid floating point literal starting: %v", lex.scanner.Text())
		}
		return lex.readFloatExponent()
	default:
		return lex.emitText(token.INT)
	}
	// the returned string may not actually be a usable number (overflow), but
	// we can find that out at parse time -- not scan time.
}

func (lex *Lexer) readFloatFraction() []*token.Token {
	if lex.scanner.AcceptSeqDigit() == 0 {
		return lex.errorf("invalid floating point literal starting: %v", lex.scanner.Text())
	}
	switch {
	case lex.scanner.AcceptAny("eE"):
		if !lex.scanner.Accept(func(c rune) bool { return true }) {
			return lex.errorf("invalid floating point literal starting: %v", lex.scanner.Text())
		}
		return lex.readFloatExponent()
	default:
		return lex.emitText(token.FLOAT)
	}
}

func (lex *Lexer) readFloatExponent() []*token.Token {
	lex.scanner.AcceptAny("+-") // optional sign
	if lex.scanner.AcceptSeqDigit() == 0 {
		return lex.errorf("invalid floating point literal starting: %v", lex.scanner.Text())
	}
	return lex.emitText(token.FLOAT)
}

func (lex *Lexer) skipWhitespace() {
	if lex.scanner.AcceptSeqSpace() > 0 {
		lex.scanner.Ignore()
	}
}

func (lex *Lexer) peekRune() rune {
	r, _ := lex.scanner.Peek()
	return r
}

func (lex *Lexer) readChar() error {
	lex.scanner.ScanRune()
	return nil
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
