package rdparser

import (
	"io"
	"strconv"
	"strings"

	"bitbucket.org/luthersystems/elps/lisp"
	"bitbucket.org/luthersystems/elps/parser/lexer"
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
	lex  *lexer.Lexer
	curr *token.Token
	peek *token.Token
}

// New initializes and returns a new Parser that reads tokens from scanner.
func New(scanner *token.Scanner) *Parser {
	p := &Parser{
		lex: lexer.New(scanner),
	}
	p.initTokens()
	return p
}

func (p *Parser) initTokens() {
	// Setup the peek token so the parser is in the proper state when the first
	// parse function is called.
	p.ReadToken()
}

func (p *Parser) ParseProgram() ([]*lisp.LVal, error) {
	var exprs []*lisp.LVal

	for {
		for p.expect(token.COMMENT) {
		}
		if p.expect(token.EOF) {
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
	for p.expect(token.COMMENT) {
	}
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
		return p.errorf("scan-error", p.Token().Text)
	default:
		p.ReadToken()
		return p.errorf("unexpected-token", "%s unexpected %s", p.Token().Source, p.Token().Type)
	}
}

func (p *Parser) ParseLiteralInt() *lisp.LVal {
	if !p.expect(token.INT) {
		return p.errorf("parse-error", "invalid integer literal: %v", p.PeekType())
	}
	text := p.Token().Text
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
	if !p.expect(token.FLOAT) {
		return p.errorf("parse-error", "invalid float literal: %v", p.PeekType())
	}
	text := p.Token().Text
	x, err := strconv.ParseFloat(text, 64)
	if err != nil {
		return p.errorf("invalid-float", "invalid floating point literal: %v", text)
	}
	return p.Float(x)
}

func (p *Parser) ParseLiteralString() *lisp.LVal {
	if !p.expect(token.STRING) {
		return p.errorf("parse-error", "invalid string literal: %v", p.PeekType())
	}
	text := p.Token().Text
	s, err := strconv.Unquote(text)
	if err != nil {
		return p.errorf("invalid-float", "invalid string literal: %v", text)
	}
	return p.String(s)
}

func (p *Parser) ParseLiteralStringRaw() *lisp.LVal {
	if !p.expect(token.STRING_RAW) {
		return p.errorf("parse-error", "invalid raw string literal: %v", p.PeekType())
	}
	text := p.Token().Text
	if len(text) < 6 {
		panic("short raw string")
	}
	return p.String(text[3 : len(text)-3])
}

func (p *Parser) ParseQuote() *lisp.LVal {
	if !p.expect(token.QUOTE) {
		return p.errorf("parse-error", "invalid quote: %v", p.PeekType())
	}
	return p.Quote(p.ParseExpression())
}

func (p *Parser) ParseUnbound() *lisp.LVal {
	if !p.expect(token.UNBOUND) {
		return p.errorf("parse-error", "invalid quote: %v", p.PeekType())
	}
	expr := p.ParseExpression()
	if expr.Type == lisp.LError {
		return expr
	}
	// Ensure that the expression doesn't contain nested cons expressions.
	for _, c := range expr.Cells {
		if c.Type == lisp.LSExpr && !c.Quoted {
			return p.errorf("unbound-expression-error", "unbound expression cannot contain nested expressions")
		}
	}
	return lisp.SExpr([]*lisp.LVal{lisp.Symbol("lisp:expr"), expr})
}

func (p *Parser) ParseNegative() *lisp.LVal {
	if !p.expect(token.NEGATIVE) {
		return p.errorf("parse-error", "invalid negative: %v", p.PeekType())
	}
	switch p.PeekType() {
	case token.INT, token.FLOAT, token.SYMBOL:
		p.peek.Source = p.curr.Source
		p.peek.Text = "-" + p.peek.Text
	}
	return p.ParseExpression()
}

func (p *Parser) ParseSymbol() *lisp.LVal {
	if !p.expect(token.SYMBOL) {
		return p.errorf("parse-error", "invalid symbol: %v", p.PeekType())
	}
	tok := p.Token()
	sym := p.Symbol(tok.Text)
	if p.expect(token.QUALIFY) {
		if !p.expect(token.SYMBOL) {
			p.ReadToken()
			return p.errorf("unexpected-token", "%s unexpected %s", p.Token().Source, p.Token().Type)
		}
		sym.Str += ":" + p.Token().Text
	}
	sym.Source = tok.Source
	return sym
}

func (p *Parser) ParseKeyword() *lisp.LVal {
	if !p.expect(token.QUALIFY) {
		return p.errorf("parse-error", "invalid symbol: %v", p.PeekType())
	}
	tok := p.Token()
	if !p.expect(token.SYMBOL) {
		p.ReadToken()
		return p.errorf("unexpected-token", "%s unexpected %s", p.Token().Source, p.Token().Type)
	}
	sym := lisp.Symbol(":" + p.Token().Text)
	sym.Source = tok.Source
	return sym
}

func (p *Parser) ParseConsExpression() *lisp.LVal {
	if !p.expect(token.PAREN_L) {
		return p.errorf("parse-error", "invalid symbol: %v", p.PeekType())
	}
	open := p.Token()
	expr := lisp.SExpr(nil)
	for {
		if p.expect(token.EOF) {
			return p.errorf("unmatched-syntax", "unmatched %s", open.Text)
		}
		if p.expect(token.PAREN_R) {
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
	if !p.expect(token.BRACE_L) {
		return p.errorf("parse-error", "invalid symbol: %v", p.PeekType())
	}
	open := p.Token()
	expr := lisp.QExpr(nil)
	for {
		if p.expect(token.EOF) {
			return p.errorf("unmatched-syntax", "unmatched %s", open.Text)
		}
		if p.expect(token.BRACE_R) {
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

func (p *Parser) ReadToken() *token.Token {
	p.curr = p.peek
	p.peek = p.lex.NextToken()
	return p.curr
}

func (p *Parser) Token() *token.Token {
	return p.curr
}

func (p *Parser) Peek() *token.Token {
	return p.peek
}

func (p *Parser) PeekType() token.Type {
	return p.peek.Type
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

func (p *Parser) tokenLVal(v *lisp.LVal) *lisp.LVal {
	v.Source = p.Token().Source
	return v
}

func (p *Parser) expect(typ ...token.Type) bool {
	peekType := p.peek.Type
	if len(typ) == 0 {
		return peekType != token.EOF
	}
	for _, typ := range typ {
		if typ == peekType {
			p.ReadToken()
			return true
		}
	}
	return false
}

func (p *Parser) errorf(condition string, format string, v ...interface{}) *lisp.LVal {
	err := lisp.ErrorConditionf(condition, format, v...)
	err.Source = p.Token().Source
	return err
}
