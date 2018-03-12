package lisp

import (
	"bytes"
	"fmt"
	"sort"
	"strconv"
)

// LValType is the type of an LVal
type LValType uint

// Possible LValType values
const (
	LInvalid LValType = iota
	LNumber
	LError
	LSymbol
	LQSymbol
	LSExpr
	LQExpr
	LFun
)

var lvalTypeStrings = []string{
	LInvalid: "INVALID",
	LNumber:  "number",
	LError:   "error",
	LSymbol:  "symbol",
	LQSymbol: "qsymbol",
	LSExpr:   "sexpr",
	LQExpr:   "qexpr",
	LFun:     "function",
}

func (t LValType) String() string {
	if int(t) >= len(lvalTypeStrings) {
		return lvalTypeStrings[LInvalid]
	}
	return lvalTypeStrings[t]
}

// Errno is an error code
type Errno int

// Posible Errno values
const (
	ErrnoPanic Errno = iota
	ErrnoDivZero
	ErrnoNoSym
	ErrnoBadNum
)

var errnoStrings = []string{
	ErrnoPanic:   "PANIC",
	ErrnoDivZero: "division by zero",
	ErrnoNoSym:   "no such symbol",
	ErrnoBadNum:  "bad number",
}

func (n Errno) String() string {
	if int(n) >= len(errnoStrings) {
		return errnoStrings[ErrnoPanic]
	}
	return errnoStrings[n]
}

// LBuiltin is a function that performs executes a lisp function.
type LBuiltin func(env *LEnv, args *LVal) *LVal

// LBuiltinDef is a built-in function
// XXX: LBuiltinDef ... LBuiltInDef ... ?
type LBuiltinDef interface {
	Name() string
	Eval(env *LEnv, args *LVal) *LVal
}

// LVal is a lisp value
type LVal struct {
	Type  LValType
	Num   int
	Sym   string
	Err   error
	Cells []*LVal

	// Variables needed for function values
	Builtin LBuiltin
	Env     *LEnv
	Formals *LVal
	Body    *LVal
}

// Number returns an LVal representing the number x.
func Number(x int) *LVal {
	return &LVal{
		Type: LNumber,
		Num:  x,
	}
}

// Symbol returns an LVal resprenting the symbol s
func Symbol(s string) *LVal {
	return &LVal{
		Type: LSymbol,
		Sym:  s,
	}
}

// QSymbol returns an LVal resprenting the quoted symbol
func QSymbol(s string) *LVal {
	return &LVal{
		Type: LQSymbol,
		Sym:  s,
	}
}

// Nil returns an LVal representing nil, an empty list, an absent value.
func Nil() *LVal {
	return QExpr()
}

// SExpr returns an LVal representing an S-expression, a symbolic expression.
func SExpr() *LVal {
	return &LVal{
		Type: LSExpr,
	}
}

// QExpr returns an LVal representing an Q-expression, a quoted expression, a
// list.
func QExpr() *LVal {
	return &LVal{
		Type: LQExpr,
	}
}

// Fun returns an LVal representing a function
func Fun(fn LBuiltin) *LVal {
	return &LVal{
		Type:    LFun,
		Builtin: fn,
	}
}

// Lambda returns anonymous function that has formals as arguments and the
// given body, which may reference symbols specified in the list of formals.
func Lambda(formals *LVal, body *LVal) *LVal {
	// XXX: Does this need error checking?
	v := &LVal{
		Type:    LFun,
		Env:     NewEnv(nil),
		Formals: formals,
		Body:    body,
	}
	return v
}

// Error returns an LVal representing the error corresponding to err.
func Error(err error) *LVal {
	return &LVal{
		Type: LError,
		Err:  err,
	}
}

// Errorf returns an LVal representing with a formatted error message.
func Errorf(format string, v ...interface{}) *LVal {
	return &LVal{
		Type: LError,
		Err:  fmt.Errorf(format, v...),
	}
}

// Copy creates a deep copy of the receiver.
func (v *LVal) Copy() *LVal {
	if v == nil {
		return nil
	}
	cp := &LVal{}
	*cp = *v                 // shallow copy of all fields
	cp.Cells = v.copyCells() // deep copy of v.Cells
	cp.Env = v.Env.Copy()    // deepish copy of v.Env
	cp.Formals = v.Formals.Copy()
	cp.Body = v.Body.Copy()
	return cp
}

func (v *LVal) copyCells() []*LVal {
	if len(v.Cells) == 0 {
		return nil
	}
	cells := make([]*LVal, len(v.Cells))
	for i := range cells {
		cells[i] = v.Cells[i].Copy()
	}
	return cells
}

func (v *LVal) String() string {
	switch v.Type {
	case LNumber:
		return strconv.Itoa(v.Num)
	case LError:
		return v.Err.Error()
	case LSymbol:
		return v.Sym
	case LQSymbol:
		return "'" + v.Sym
	case LSExpr:
		return exprString(v, "(", ")")
	case LQExpr:
		return exprString(v, "'(", ")")
	case LFun:
		if v.Builtin != nil {
			return "<builtin>"
		}
		vars := lambdaVars(v.Formals, boundVars(v))
		return fmt.Sprintf("(lambda %v %v)", vars, v.Body)
	default:
		return fmt.Sprintf("%#v", v)
	}
}

func lambdaVars(formals *LVal, bound *LVal) *LVal {
	q := QExpr()
	q.Cells = []*LVal{formals, bound}
	return builtinConcat(nil, q)
}

func boundVars(v *LVal) *LVal {
	if v.Env == nil {
		return Nil()
	}
	keys := make([]string, 0, len(v.Env.Scope))
	for k := range v.Env.Scope {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	bound := QExpr()
	for i := range keys {
		q := QExpr()
		q.Cells = append(q.Cells, Symbol(keys[i]))
		q.Cells = append(q.Cells, v.Env.Get(Symbol(keys[i])))
		bound.Cells = append(bound.Cells, q)
	}
	return bound
}

func exprString(v *LVal, left string, right string) string {
	if len(v.Cells) == 0 {
		return left + right
	}
	var buf bytes.Buffer
	buf.WriteString(left)
	for i, c := range v.Cells {
		if i > 0 {
			buf.WriteString(" ")
		}
		buf.WriteString(c.String())
	}
	buf.WriteString(right)
	return buf.String()
}
