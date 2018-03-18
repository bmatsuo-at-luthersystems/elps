/*
Package parser provides a lisp parser.

	expr   := '(' <expr>* ')' | <number> | <string> | <symbol>
	number := /[+-]?[0-9]+/ <fraction>? <exponent>?
	fraction := '.' /[0-9]+/
	exponent := e /[0-9]+/
	string := '"' <strcontent> '"'
	strcontent := /[^"]+/ | '\' '"'
	symbol := /[^[:space:]]+/
*/
package parser

import (
	"fmt"
	"io"
	"strconv"
	"strings"

	"bitbucket.org/luthersystems/elps/lisp"
	parsec "github.com/prataprc/goparsec"
)

const (
	nodeInvalid nodeType = iota
	nodeTerm
	nodeItem
	nodeItems
	nodeList
	nodeSExpr
	nodeQExpr
)

var nodeTypeStrings = []string{
	nodeInvalid: "INVALID",
	nodeTerm:    "TERM",
	nodeItem:    "ITEM",
	nodeItems:   "ITEMS",
	nodeList:    "LIST",
	nodeSExpr:   "SEXPR",
	nodeQExpr:   "QEXPR",
}

// Parse parses a lisp expression.
func Parse(env *lisp.LEnv, print bool, text []byte) (bool, error) {
	s := parsec.NewScanner(text)
	parser := newParsecParser()

	evaled := false
	root, s := parser(s)
	for root != nil {
		evaled = evalParsecRoot(env, print, root)
		root, s = parser(s)
	}
	return evaled, nil
}

// ParseLVal parses LVal values from text and returns them.  The number of
// bytes read is returned along with any error that was encountered in parsing.
func ParseLVal(text []byte) ([]*lisp.LVal, int, error) {
	var v []*lisp.LVal
	s := parsec.NewScanner(text)
	parser := newParsecParser()
	root, s := parser(s)
	for root != nil {
		v = append(v, getLVal(root))
		root, s = parser(s)
	}
	if !s.Endof() {
		return v, s.GetCursor(), io.ErrUnexpectedEOF
	}
	return v, s.GetCursor(), nil
}

func newParsecParser() parsec.Parser {
	openP := parsec.Atom("(", "OPENP")
	closeP := parsec.Atom(")", "CLOSEP")
	q := parsec.Atom("'", "QUOTE")
	comment := parsec.Token(`;([^\n]*[^\s])?`, "COMMENT")
	decimal := parsec.Token(`[+-]?[0-9]+([.][0-9]+)?([eE][+-]?[0-9]+)?`, "DECIMAL")
	//symbol := parsec.Token(`[^\s()']+`, "SYMBOL")
	symbol := parsec.Token(`(?:\pL|[_+\-*/\=<>!&~%])(?:\pL|[0-9]|[_+\-*/\=<>!&~%])*`, "SYMBOL")
	//qsymbol := parsec.And(nil, q, symbol)
	term := parsec.OrdChoice(astNode(nodeTerm), // terminal token
		parsec.String(),
		decimal,
		symbol, // symbol comes last because it swallows anything
	)
	var expr parsec.Parser // forward declaration allows for recursive parsing
	exprList := parsec.Kleene(nil, &expr)
	sexpr := parsec.And(astNode(nodeSExpr), openP, exprList, closeP)
	qexpr := parsec.And(astNode(nodeQExpr), q, &expr)
	expr = parsec.OrdChoice(nil, comment, term, sexpr, qexpr)
	return expr
}

type nodeType uint

func (t nodeType) String() string {
	if int(t) >= len(nodeTypeStrings) {
		return "INVALID"
	}
	return nodeTypeStrings[t]
}

type ast struct {
	typ      nodeType
	children []parsec.ParsecNode
}

func newAST(typ nodeType, nodes []parsec.ParsecNode) parsec.ParsecNode {
	nodes = cleanParsecNodeList(nodes)
	switch typ {
	case nodeTerm:
		var lval *lisp.LVal
		switch term := nodes[0].(type) {
		case string:
			lval = lisp.String(unquoteString(term))
		case *parsec.Terminal:
			switch term.Name {
			case "DECIMAL":
				if strings.ContainsAny(term.Value, ".eE") {
					f, err := strconv.ParseFloat(term.Value, 64)
					if err != nil {
						// FIXME:  error location metadata totally escapes here
						lval = lisp.Errorf("bad number: %v (%s)", err, term.Value)
					} else {
						lval = lisp.Float(f)
					}
				} else {
					x, err := strconv.Atoi(term.Value)
					if err != nil {
						// FIXME:  error location metadata totally escapes here
						lval = lisp.Errorf("bad number: %v (%s)", err, term.Value)
					} else {
						lval = lisp.Int(x)
					}
				}
			case "SYMBOL":
				lval = lisp.Symbol(term.Value)
			}
		}
		return lval
	case nodeSExpr:
		lval := lisp.SExpr()
		// We don't want terminal parsec nodes '(' and ')'
		for _, c := range nodes {
			switch c.(type) {
			case *lisp.LVal:
				lval.Cells = append(lval.Cells, c.(*lisp.LVal))
			}
		}
		return lval
	case nodeQExpr:
		lval := lisp.QExpr()
		// We don't want terminal parsec nodes "'(" and ")"
		c := nodes[1].(*lisp.LVal)
		lval = lisp.Quote(c)
		return lval
	default:
		panic(fmt.Sprintf("unknown nodeType: %s (%d)", typ, typ))
	}
}

func cleanParsecNodeList(lis []parsec.ParsecNode) []parsec.ParsecNode {
	var nodes []parsec.ParsecNode
	for _, n := range lis {
		switch node := n.(type) {
		case []parsec.ParsecNode:
			nodes = append(nodes, cleanParsecNodeList(node)...)
		default:
			nodes = append(nodes, node)
		}
	}
	return nodes
}

func dumpAST(t *ast, indent string) {
	line := fmt.Sprintf("%sAST NODE type=%v", indent, t.typ)
	fmt.Println(line)
	for _, c := range t.children {
		dumpParsecNode(c, indent+"  ")
	}
}

func astNode(t nodeType) parsec.Nodify {
	return func(nodes []parsec.ParsecNode) parsec.ParsecNode {
		return newAST(t, nodes)
	}
}

func dumpParsecNode(node parsec.ParsecNode, indent string) {
	switch node := node.(type) {
	case *ast:
		dumpAST(node, indent)
	case *parsec.Terminal:
		fmt.Printf("%s%s %s\n", indent, node.GetName(), node.GetValue())
	case []parsec.ParsecNode:
		for _, node := range node {
			dumpParsecNode(node, indent+"  ")
		}
	case *lisp.LVal:
		fmt.Println(node)
	default:
		fmt.Printf("%T\n", node)
	}
}

func getLVal(root parsec.ParsecNode) *lisp.LVal {
	nodes := cleanParsecNodeList([]parsec.ParsecNode{root})
	if len(nodes) == 0 {
		// we can be here if there is only whitespace on a line
		return nil
	}
	lval, ok := nodes[0].(*lisp.LVal)
	if !ok {
		// we can be here if there is only a comment on a line
		return nil
	}
	return lval
}

func evalParsecRoot(env *lisp.LEnv, print bool, root parsec.ParsecNode) bool {
	v := getLVal(root)
	if v == nil {
		return false
	}
	if print {
		fmt.Println(env.Eval(v))
	}
	return true
}

func unquoteString(s string) string {
	return s[1 : len(s)-1]
}
