package parser

import (
	"bitbucket.org/luthersystems/elps/lisp"
	"bitbucket.org/luthersystems/elps/parser/rdparser"
)

// NewReader returns a new lisp.Reader
func NewReader() lisp.Reader {
	return rdparser.NewReader()
}
