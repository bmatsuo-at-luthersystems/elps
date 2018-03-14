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
	"strconv"

	"github.com/bmatsuo/somelisp/lisp"
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
	openP := parsec.Atom("(", "OPENP")
	closeP := parsec.Atom(")", "CLOSEP")
	q := parsec.Atom("'", "QUOTE")
	number := parsec.Token(`[+-]?[0-9]+`, "NUMBER")
	//symbol := parsec.Token(`[^\s()']+`, "SYMBOL")
	symbol := parsec.Token(`(?:\pL|[_+\-*/\=<>!&])+`, "SYMBOL")
	//qsymbol := parsec.And(nil, q, symbol)
	term := parsec.OrdChoice(astNode(nodeTerm), // terminal token
		number,
		symbol, // symbol comes last because it swallows anything
		//qsymbol,
	)
	var expr parsec.Parser // forward declaration allows for recursive parsing
	exprList := parsec.Kleene(nil, &expr)
	sexpr := parsec.And(astNode(nodeSExpr), openP, exprList, closeP)
	qexpr := parsec.And(astNode(nodeQExpr), q, &expr)
	expr = parsec.OrdChoice(nil, term, sexpr, qexpr)

	evaled := false
	root, s := expr(s)
	for root != nil {
		evaled = true
		// TODO:  Only dump the root if a verbosity flag is set
		//dumpParsecNode(root, "")
		evalParsecRoot(env, print, root)
		root, s = expr(s)
	}
	return evaled, nil
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
		term := nodes[0].(*parsec.Terminal)
		switch term.Name {
		case "NUMBER":
			x, err := strconv.Atoi(term.Value)
			if err != nil {
				// FIXME:  error location metadata totally escapes here
				lval = lisp.Errorf("bad number")
			} else {
				lval = lisp.Number(x)
			}
		case "SYMBOL":
			lval = lisp.Symbol(term.Value)
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
	/*
		if t.typ == nodeTerm {
			for _, node := range t.children {
				line += fmt.Sprintf(" %v %v", node.(*parsec.Terminal).GetName(), node.(*parsec.Terminal).GetValue())
			}
		}
	*/
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

func evalParsecRoot(env *lisp.LEnv, print bool, root parsec.ParsecNode) {
	nodes := cleanParsecNodeList([]parsec.ParsecNode{root})
	lval, ok := nodes[0].(*lisp.LVal)
	if !ok {
		panic("did not get an lval")
	}
	if print {
		fmt.Println(env.Eval(lval))
	}
}
