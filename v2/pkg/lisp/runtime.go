package lisp

import "github.com/luthersystems/elps/v2/pkg/symbol"

// TODO: Remove this?
type Runtime interface {
	SourceLocation() SourceLocation
	IsKeyword(sym symbol.ID) bool
	Symbol(sym symbol.ID) (string, bool)
	GetGlobal(sym symbol.ID) LVal
	PutGlobal(sym symbol.ID, v LVal) LVal
	Error(err interface{}) LVal
}
