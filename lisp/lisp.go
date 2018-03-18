package lisp

import (
	"bytes"
	"fmt"
	"sort"
	"strconv"
)

// LType is the type of an LVal
type LType uint

// Possible LValType values
const (
	LInvalid LType = iota
	LInt
	LFloat
	LError
	LSymbol
	LQSymbol
	LSExpr
	LFun
	LQuote
	LString
	LMarkTailRec
	LMarkMacExpand
)

var lvalTypeStrings = []string{
	LInvalid:       "INVALID",
	LInt:           "int",
	LFloat:         "float",
	LError:         "error",
	LSymbol:        "symbol",
	LQSymbol:       "qsymbol",
	LSExpr:         "sexpr",
	LFun:           "function",
	LQuote:         "quoted",
	LString:        "string",
	LMarkTailRec:   "marker-tail-recursion",
	LMarkMacExpand: "marker-macro-expansion",
}

func (t LType) String() string {
	if int(t) >= len(lvalTypeStrings) {
		return lvalTypeStrings[LInvalid]
	}
	return lvalTypeStrings[t]
}

// LFunType denotes special functions, either macros or special operators.
type LFunType uint

// LFunType constants.  LFunNone indicates a normal function.
const (
	LFunNone = iota
	LFunMacro
	LFunSpecialOp
)

// LVal is a lisp value
type LVal struct {
	Type     LType
	Int      int
	Float    float64
	Str      string
	Err      error
	Cells    []*LVal
	Quoted   bool // flag indicating a single level of quoting
	Terminal bool // LVal is the terminal expression in a function call

	// Variables needed for function values
	Macro   bool
	FunType LFunType
	Builtin LBuiltin
	Env     *LEnv
	Formals *LVal
	Body    *LVal
	FID     string
}

// Bool returns an LVal with truthiness identical to b.
func Bool(b bool) *LVal {
	if b {
		return Symbol("t")
	}
	return Nil()
}

// Int returns an LVal representing the number x.
func Int(x int) *LVal {
	return &LVal{
		Type: LInt,
		Int:  x,
	}
}

// Float returns an LVal representation of the number x
func Float(x float64) *LVal {
	return &LVal{
		Type:  LFloat,
		Float: x,
	}
}

// String returns an LVal representing the string str.
func String(str string) *LVal {
	return &LVal{
		Type: LString,
		Str:  str,
	}
}

// Symbol returns an LVal resprenting the symbol s
func Symbol(s string) *LVal {
	return &LVal{
		Type: LSymbol,
		Str:  s,
	}
}

// QSymbol returns an LVal resprenting the quoted symbol
func QSymbol(s string) *LVal {
	return &LVal{
		Type: LQSymbol,
		Str:  s,
	}
}

// Nil returns an LVal representing nil, an empty list, an absent value.
func Nil() *LVal {
	return SExpr()
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
		Type:   LSExpr,
		Quoted: true,
	}
}

// Fun returns an LVal representing a function
func Fun(fid string, fn LBuiltin) *LVal {
	return &LVal{
		Type:    LFun,
		Builtin: fn,
		FID:     fid,
	}
}

// Macro returns an LVal representing a macro
func Macro(fid string, fn LBuiltin) *LVal {
	return &LVal{
		Type:    LFun,
		FunType: LFunMacro,
		Builtin: fn,
		FID:     fid,
	}
}

// SpecialOp returns an LVal representing a special operator.  Special
// operators are function which receive unevaluated results, like macros.
// However values returned by special operations do not require further
// evaluation, unlike macros.
func SpecialOp(fid string, fn LBuiltin) *LVal {
	return &LVal{
		Type:    LFun,
		FunType: LFunSpecialOp,
		Builtin: fn,
		FID:     fid,
	}
}

// Lambda returns anonymous function that has formals as arguments and the
// given body, which may reference symbols specified in the list of formals.
func Lambda(formals *LVal, body *LVal) *LVal {
	if formals.Type != LSExpr {
		return Errorf("formals is not a list of symbols: %v", formals.Type)
	}
	if formals.Type != LSExpr {
		return Errorf("body is not a list: %v", body.Type)
	}
	env := NewEnv(nil)
	return &LVal{
		Type:    LFun,
		Env:     env,
		Formals: formals,
		Body:    body,
		FID:     env.getFID(),
	}
}

// Error returns an LVal representing the error corresponding to err.
func Error(err error) *LVal {
	return &LVal{
		Type: LError,
		Err:  err,
	}
}

// Quote quotes v and returns the quoted value.  The LVal v is modified.
func Quote(v *LVal) *LVal {
	if !v.Quoted {
		v.Quoted = true
		return v
	}
	quote := &LVal{
		Type:   LQuote,
		Quoted: true,
		Body:   v,
	}
	return quote
}

// Errorf returns an LVal representing with a formatted error message.
func Errorf(format string, v ...interface{}) *LVal {
	return &LVal{
		Type: LError,
		Err:  fmt.Errorf(format, v...),
	}
}

func markTailRec(npop int, fun *LVal, args *LVal) *LVal {
	return &LVal{
		Type:  LMarkTailRec,
		Cells: []*LVal{Int(npop), fun, args},
	}
}

func markMacExpand(expr *LVal) *LVal {
	return &LVal{
		Type:  LMarkMacExpand,
		Cells: []*LVal{expr},
	}
}

// IsSpecialFun returns true if v is a special function.  IsSpecialFun doesn't
// actually check v.Type, only v.FunType.
func (v *LVal) IsSpecialFun() bool {
	return v.FunType != LFunNone
}

// IsMacro returns true if v is a macro function.  IsMacro doesn't
// actually check v.Type, only v.FunType.
func (v *LVal) IsMacro() bool {
	return v.FunType == LFunMacro
}

// IsSpecialOp returns true if v is a special operator.  IsMacro doesn't
// actually check v.Type, only v.FunType.
func (v *LVal) IsSpecialOp() bool {
	return v.FunType == LFunSpecialOp
}

// IsNil returns true if v represents a nil value.
func (v *LVal) IsNil() bool {
	switch v.Type {
	case LSExpr:
		return len(v.Cells) == 0
	}
	return false
}

// IsNumeric returns true if v has a primitive numeric type (int, float64).
func (v *LVal) IsNumeric() bool {
	switch v.Type {
	case LInt:
		return true
	case LFloat:
		return true
	}
	return false
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
	const QUOTE = `'`
	if v.Type == LQuote {
		return QUOTE + v.Body.str(true)
	}
	return v.str(false)
}

func (v *LVal) str(onTheRecord bool) string {
	const QUOTE = `'`
	// All types which may evaluate to things other than themselves must check
	// v.Quoted.
	quote := ""
	if onTheRecord {
		quote = QUOTE
	}
	switch v.Type {
	case LInt:
		return quote + strconv.Itoa(v.Int)
	case LFloat:
		return quote + strconv.FormatFloat(v.Float, 'g', -1, 64)
	case LString:
		return quote + fmt.Sprintf("%q", v.Str)
	case LError:
		return quote + v.Err.Error()
	case LSymbol:
		if v.Quoted {
			quote = QUOTE
		}
		return quote + v.Str
	case LSExpr:
		if v.Quoted {
			quote = QUOTE
		}
		return exprString(v, quote+"(", ")")
	case LFun:
		if v.Quoted {
			quote = QUOTE
		}
		if v.Builtin != nil {
			return fmt.Sprintf("%s<builtin>", quote)
		}
		vars := lambdaVars(v.Formals, boundVars(v))
		return fmt.Sprintf("%s(lambda %v %v)", quote, vars, v.Body)
	case LQuote:
		// TODO: make more efficient
		return QUOTE + v.Body.str(true)
	case LMarkTailRec:
		return quote + fmt.Sprintf("<tail-recursion frames=%d (%s %s)>", v.Cells[0].Int, v.Cells[1], v.Cells[2])
	case LMarkMacExpand:
		return quote + fmt.Sprintf("<macro-expansion %s)>", v.Cells[0])
	default:
		return quote + fmt.Sprintf("<%s %#v>", v.Type, v)
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
