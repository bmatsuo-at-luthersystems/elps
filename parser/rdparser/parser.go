package rdparser

import (
	"io"
	"strconv"
	"strings"

	"bitbucket.org/luthersystems/elps/lisp"
	"bitbucket.org/luthersystems/elps/parser/token"
)

type reader struct {
}

// NewReader returns a lisp.Reader to use in a lisp.Runtime.
func NewReader() lisp.Reader {
	return &reader{}
}

// Read implements lisp.Reader.
func (_ *reader) Read(name string, r io.Reader) ([]*lisp.LVal, error) {
	s := token.NewScanner(name, r)
	p := New(s)
	return p.ParseProgram()
}

// Parser is a lisp parser.
type Parser struct {
	src *TokenSource
}

// New initializes and returns a new Parser that reads tokens from scanner.
func New(scanner *token.Scanner) *Parser {
	p := &Parser{
		src: NewTokenSource(scanner),
	}
	return p
}

func (p *Parser) ParseProgram() ([]*lisp.LVal, error) {
	var exprs []*lisp.LVal

	for {
		p.ignoreComments()
		if p.src.IsEOF() {
			break
		}
		expr := p.ParseExpression()
		if expr.Type == lisp.LError {
			return nil, lisp.GoError(expr)
		}
		exprs = append(exprs, expr)
	}

	return exprs, nil
}

func (p *Parser) ParseExpression() *lisp.LVal {
	p.ignoreComments()
	switch p.PeekType() {
	case token.INT:
		return p.ParseLiteralInt()
	case token.FLOAT:
		return p.ParseLiteralFloat()
	case token.STRING:
		return p.ParseLiteralString()
	case token.STRING_RAW:
		return p.ParseLiteralStringRaw()
	case token.NEGATIVE:
		return p.ParseNegative()
	case token.QUOTE:
		return p.ParseQuote()
	case token.UNBOUND:
		return p.ParseUnbound()
	case token.SYMBOL:
		return p.ParseSymbol()
	case token.QUALIFY:
		return p.ParseKeyword()
	case token.PAREN_L:
		return p.ParseConsExpression()
	case token.BRACE_L:
		return p.ParseList()
	case token.ERROR, token.INVALID:
		p.ReadToken()
		return p.errorf("scan-error", p.TokenText())
	default:
		p.ReadToken()
		return p.errorf("unexpected-token", "%s unexpected %s", p.Location(), p.TokenType())
	}
}

func (p *Parser) ParseLiteralInt() *lisp.LVal {
	if !p.Accept(token.INT) {
		return p.errorf("parse-error", "invalid integer literal: %v", p.PeekType())
	}
	text := p.TokenText()
	if strings.HasPrefix(text, "0") && text != "0" {
		// TODO: octal and hex integer literals
		return p.errorf("invalid-integer-literal", "integer literal starts with 0: %v", text)
	}
	x, err := strconv.Atoi(text)
	if err != nil {
		return p.errorf("integer-overflow-error", "integer literal overflows int: %v", text)
	}
	return p.Int(x)
}

func (p *Parser) ParseLiteralFloat() *lisp.LVal {
	if !p.Accept(token.FLOAT) {
		return p.errorf("parse-error", "invalid float literal: %v", p.PeekType())
	}
	x, err := strconv.ParseFloat(p.TokenText(), 64)
	if err != nil {
		return p.errorf("invalid-float", "invalid floating point literal: %v", p.TokenText())
	}
	return p.Float(x)
}

func (p *Parser) ParseLiteralString() *lisp.LVal {
	if !p.Accept(token.STRING) {
		return p.errorf("parse-error", "invalid string literal: %v", p.PeekType())
	}
	s, err := strconv.Unquote(p.TokenText())
	if err != nil {
		return p.errorf("invalid-float", "invalid string literal: %v", p.TokenText())
	}
	return p.String(s)
}

func (p *Parser) ParseLiteralStringRaw() *lisp.LVal {
	if !p.Accept(token.STRING_RAW) {
		return p.errorf("parse-error", "invalid raw string literal: %v", p.PeekType())
	}
	text := p.TokenText()
	if len(text) < 6 {
		panic("short raw string")
	}
	return p.String(text[3 : len(text)-3])
}

func (p *Parser) ParseQuote() *lisp.LVal {
	if !p.Accept(token.QUOTE) {
		return p.errorf("parse-error", "invalid quote: %v", p.PeekType())
	}
	return p.Quote(p.ParseExpression())
}

func (p *Parser) ParseUnbound() *lisp.LVal {
	if !p.Accept(token.UNBOUND) {
		return p.errorf("parse-error", "invalid quote: %v", p.PeekType())
	}
	expr := p.ParseExpression()
	if expr.Type == lisp.LError {
		return expr
	}
	sym := lisp.Symbol("lisp:expr")
	sym.Source = expr.Source
	// Ensure that the expression doesn't contain nested cons expressions.
	for _, c := range expr.Cells {
		if c.Type == lisp.LSExpr && !c.Quoted {
			return p.errorf("unbound-expression-error", "unbound expression cannot contain nested expressions")
		}
	}
	return p.SExpr([]*lisp.LVal{sym, expr})
}

func (p *Parser) ParseNegative() *lisp.LVal {
	if !p.Accept(token.NEGATIVE) {
		return p.errorf("parse-error", "invalid negative: %v", p.PeekType())
	}
	switch p.PeekType() {
	case token.INT, token.FLOAT, token.SYMBOL:
		p.src.Peek.Source = p.Location()
		p.src.Peek.Text = p.TokenText() + p.src.Peek.Text
	default:
		return p.Symbol(p.TokenText())
	}
	return p.ParseExpression()
}

func (p *Parser) ParseSymbol() *lisp.LVal {
	if !p.Accept(token.SYMBOL) {
		return p.errorf("parse-error", "invalid symbol: %v", p.PeekType())
	}
	tok := p.src.Token
	sym := p.Symbol(tok.Text)
	if p.Accept(token.QUALIFY) {
		if !p.Accept(token.SYMBOL) {
			p.ReadToken()
			return p.errorf("unexpected-token", "%s unexpected %s", p.src.Token.Source, p.src.Token.Type)
		}
		sym.Str += ":" + p.src.Token.Text
	}
	sym.Source = tok.Source
	return sym
}

func (p *Parser) ParseKeyword() *lisp.LVal {
	if !p.Accept(token.QUALIFY) {
		return p.errorf("parse-error", "invalid symbol: %v", p.PeekType())
	}
	tok := p.src.Token
	if !p.Accept(token.SYMBOL) {
		p.ReadToken()
		return p.errorf("unexpected-token", "%s unexpected %s", p.src.Token.Source, p.src.Token.Type)
	}
	sym := lisp.Symbol(":" + p.src.Token.Text)
	sym.Source = tok.Source
	return sym
}

func (p *Parser) ParseConsExpression() *lisp.LVal {
	if !p.Accept(token.PAREN_L) {
		return p.errorf("parse-error", "invalid symbol: %v", p.PeekType())
	}
	open := p.src.Token
	expr := p.SExpr(nil)
	for {
		p.ignoreComments()
		if p.src.IsEOF() {
			return p.errorf("unmatched-syntax", "unmatched %s", open.Text)
		}
		if p.Accept(token.PAREN_R) {
			break
		}
		x := p.ParseExpression()
		if x.Type == lisp.LError {
			return x
		}
		expr.Cells = append(expr.Cells, x)
	}
	return expr
}

func (p *Parser) ParseList() *lisp.LVal {
	if !p.Accept(token.BRACE_L) {
		return p.errorf("parse-error", "invalid symbol: %v", p.PeekType())
	}
	open := p.src.Token
	expr := p.QExpr(nil)
	for {
		if p.src.IsEOF() {
			return p.errorf("unmatched-syntax", "unmatched %s", open.Text)
		}
		if p.Accept(token.BRACE_R) {
			break
		}
		x := p.ParseExpression()
		if x.Type == lisp.LError {
			return x
		}
		expr.Cells = append(expr.Cells, x)
	}
	return expr
}

func (p *Parser) ignoreComments() {
	for p.Accept(token.COMMENT) {
	}
}

func (p *Parser) ReadToken() *token.Token {
	p.src.Scan()
	return p.src.Token
}

func (p *Parser) TokenText() string {
	return p.src.Token.Text
}

func (p *Parser) TokenType() token.Type {
	return p.src.Token.Type
}

func (p *Parser) Location() *token.Location {
	return p.src.Token.Source
}

func (p *Parser) PeekType() token.Type {
	return p.src.Peek.Type
}

func (p *Parser) PeekLocation() *token.Location {
	return p.src.Peek.Source
}

func (p *Parser) String(s string) *lisp.LVal {
	return p.tokenLVal(lisp.String(s))
}

func (p *Parser) Symbol(sym string) *lisp.LVal {
	return p.tokenLVal(lisp.Symbol(sym))
}

func (p *Parser) Int(x int) *lisp.LVal {
	return p.tokenLVal(lisp.Int(x))
}

func (p *Parser) Float(x float64) *lisp.LVal {
	return p.tokenLVal(lisp.Float(x))
}

func (p *Parser) Quote(v *lisp.LVal) *lisp.LVal {
	if v.Type == lisp.LError {
		return v
	}
	return p.tokenLVal(lisp.Quote(v))
}

func (p *Parser) SExpr(cells []*lisp.LVal) *lisp.LVal {
	return p.tokenLVal(lisp.SExpr(cells))
}

func (p *Parser) QExpr(cells []*lisp.LVal) *lisp.LVal {
	return p.tokenLVal(lisp.QExpr(cells))
}

func (p *Parser) tokenLVal(v *lisp.LVal) *lisp.LVal {
	v.Source = p.Location()
	return v
}

func (p *Parser) Accept(typ ...token.Type) bool {
	return p.src.AcceptType(typ...)
}

func (p *Parser) errorf(condition string, format string, v ...interface{}) *lisp.LVal {
	err := lisp.ErrorConditionf(condition, format, v...)
	err.Source = p.Location()
	return err
}
