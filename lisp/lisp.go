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
	LBytes
	LSortMap
	LNative
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
	LBytes:         "bytes",
	LSortMap:       "sortmap",
	LNative:        "native",
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
type LFunType uint8

// LFunType constants.  LFunNone indicates a normal function.
const (
	LFunNone = iota
	LFunMacro
	LFunSpecialOp
)

// LVal is a lisp value
type LVal struct {
	// Type is the native type for a value in lisp.
	Type LType

	// Fields used for numeric types
	Int   int
	Float float64

	// Str used by LSymbol and LString values
	Str string

	// Bytes used by LBytes values
	Bytes []byte

	// Package name for symbols and functions.
	Package string

	// Cells used by many values as a storage space for lisp objects.
	Cells []*LVal

	// Native value for language embedding and writing custom DSLs.
	Native interface{}

	// Map used for LSortMap values.
	Map map[interface{}]*LVal

	// Stack set for LError values.
	//
	// TODO:  Make the stack a first class type (or some composite type) so
	// that it could be inspected during a ``catch'' (which doesn't exist yet).
	Stack *CallStack

	// Variables needed for LFun values
	// NOTE:  Cells are used to store the list of formal function arguments in
	// index 0 and the body of non-builtin functions in the remaining cells.
	FID     string
	Env     *LEnv
	Builtin LBuiltin
	FunType LFunType

	Quoted   bool // flag indicating a single level of quoting
	Spliced  bool // denote the value as needing to be spliced into a parent value
	Terminal bool // LVal is the terminal expression in a function body
}

// Value conveniently converts v to an LVal.  Types which can be represented
// directly in lisp will be converted to the appropriate LVal.  All other types
// will be turned into a Native LVal.  Value is the GoValue function.
func Value(v interface{}) *LVal {
	switch v := v.(type) {
	case string:
		return String(v)
	case []byte:
		return Bytes(v)
	case int:
		return Int(v)
	case float64:
		return Float(v)
	case []*LVal:
		return QExpr(v)
	default:
		return Native(v)
	}
}

// Bool returns an LVal with truthiness identical to b.
func Bool(b bool) *LVal {
	if b {
		return Symbol("true")
	}
	return Symbol("false")
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

// Bytes returns an LVal representing binary data b.
func Bytes(b []byte) *LVal {
	return &LVal{
		Type:  LBytes,
		Bytes: b,
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
	return SExpr(nil)
}

// Native returns an LVal containng a native Go value.
func Native(v interface{}) *LVal {
	return &LVal{
		Type:   LNative,
		Native: v,
	}
}

// SExpr returns an LVal representing an S-expression, a symbolic expression.
func SExpr(cells []*LVal) *LVal {
	return &LVal{
		Type:  LSExpr,
		Cells: cells,
	}
}

// QExpr returns an LVal representing an Q-expression, a quoted expression, a
// list.
func QExpr(cells []*LVal) *LVal {
	return &LVal{
		Type:   LSExpr,
		Quoted: true,
		Cells:  cells,
	}
}

// SortedMap returns an LVal represented a sorted map
func SortedMap() *LVal {
	return &LVal{
		Type: LSortMap,
		Map:  make(map[interface{}]*LVal),
	}
}

// Fun returns an LVal representing a function
func Fun(fid string, formals *LVal, fn LBuiltin) *LVal {
	return &LVal{
		Type:    LFun,
		Builtin: fn,
		FID:     fid,
		Cells:   []*LVal{formals},
	}
}

// Macro returns an LVal representing a macro
func Macro(fid string, formals *LVal, fn LBuiltin) *LVal {
	return &LVal{
		Type:    LFun,
		FunType: LFunMacro,
		Builtin: fn,
		FID:     fid,
		Cells:   []*LVal{formals},
	}
}

// SpecialOp returns an LVal representing a special operator.  Special
// operators are function which receive unevaluated results, like macros.
// However values returned by special operations do not require further
// evaluation, unlike macros.
func SpecialOp(fid string, formals *LVal, fn LBuiltin) *LVal {
	return &LVal{
		Type:    LFun,
		FunType: LFunSpecialOp,
		Builtin: fn,
		FID:     fid,
		Cells:   []*LVal{formals},
	}
}

// Lambda returns anonymous function that has formals as arguments and the
// given body, which may reference symbols specified in the list of formals.
func Lambda(formals *LVal, body []*LVal) *LVal {
	if formals.Type != LSExpr {
		return Errorf("formals is not a list of symbols: %v", formals.Type)
	}
	/*
		if formals.Type != LSExpr {
			return Errorf("body is not a list: %v", body.Type)
		}
	*/
	cells := make([]*LVal, 0, len(body)+1)
	cells = append(cells, formals)
	cells = append(cells, body...)
	env := NewEnv(nil)
	return &LVal{
		Type:  LFun,
		Env:   env,
		FID:   env.getFID(),
		Cells: cells,
	}
}

// Error returns an LVal representing the error corresponding to err.  Errors
// store their message in the Str field and their type in their Cells[0].
// Errors generated during expression evaluation typically have a non-nil Stack
// field.  The Env.Error() method is typically the preferred method for
// creating error LVal objects.
func Error(err error) *LVal {
	// TODO:  Consider storing the original error in the Native field so that
	// the embedding program can extract it verbatim..
	return &LVal{
		Type:  LError,
		Str:   err.Error(),
		Cells: []*LVal{Symbol("error")},
	}
}

// Errorf returns an LVal representing with a formatted error message. Errors
// store their message in the Str field and their type in their Cells[0].
// Errors generated during expression evaluation typically have a non-nil Stack
// field.  The Env.Errorf() method is typically the preferred method for
// creating formatted error LVal objects.
func Errorf(format string, v ...interface{}) *LVal {
	return &LVal{
		Type:  LError,
		Str:   fmt.Sprintf(format, v...),
		Cells: []*LVal{Symbol("error")},
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
		Cells:  []*LVal{v},
	}
	return quote
}

// Formals returns an LVal reprsenting a function's formal argument list
// containing symbols with the given names.
func Formals(argSymbols ...string) *LVal {
	s := QExpr(make([]*LVal, len(argSymbols)))
	for i, name := range argSymbols {
		if name == VarArgSymbol {
			if i != len(argSymbols)-2 {
				return Errorf("invalid formal arguments: misplaced %s", VarArgSymbol)
			}
		}
		s.Cells[i] = Symbol(name)
	}
	return s
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

// Len returns the length of the list v.
func (v *LVal) Len() int {
	switch v.Type {
	case LString:
		return len(v.Str)
	case LBytes:
		return len(v.Bytes)
	case LSExpr:
		return len(v.Cells)
	default:
		return -1
	}
}

// MapKeys returns a list of keys in the map.  MapKeys panics if v.Type is not
// LSortMap.
func (v *LVal) MapKeys() *LVal {
	if v.Type != LSortMap {
		panic("not sortmap: " + v.Type.String())
	}
	list := QExpr(sortedMapKeys(v))
	return list
}

// MapGet returns the value corresponding to k in v or an LError if k is not
// present in v.  MapGet panics if v.Type is not LSortMap.
func (v *LVal) MapGet(k interface{}) *LVal {
	if v.Type != LSortMap {
		panic("not sortmap: " + v.Type.String())
	}
	switch k := k.(type) {
	case *LVal:
		return mapGet(v, k, nil)
	case string:
		return mapGet(v, String(k), nil)
	// numerics unsupported
	default:
		return Errorf("invalid key type: %T", k)
	}
}

// MapSet sets k to val in v.  MapSet panics if v.Type is not LSortMap.  If k
// is a string (not LString) then MapSet will try to associate either an
// existing LString key or an existing LSymbol key with val before associating
// a new LString key with val.
func (v *LVal) MapSet(k interface{}, val *LVal) *LVal {
	if v.Type != LSortMap {
		panic("not sortmap: " + v.Type.String())
	}
	switch k := k.(type) {
	case *LVal:
		return mapSet(v, k, val, false)
	case string:
		return mapSet(v, String(k), val, true)
	// numerics unsupported
	default:
		return Errorf("invalid key type: %T", k)
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

// Equal returns a non-nil value if v and other are logically equal, under the
// rules used by the ``equal?'' function.
//
// BUG:  sorted-map comparison is not implemented
func (v *LVal) Equal(other *LVal) *LVal {
	if v.Type != other.Type {
		return Bool(false)
	}
	if v.IsNumeric() {
		return v.equalNum(other)
	}
	switch v.Type {
	case LString, LSymbol:
		return Bool(v.Str == other.Str)
	case LSExpr:
		if v.Len() != other.Len() {
			return Bool(false)
		}
		for i := range v.Cells {
			if !True(v.Cells[i].Equal(other.Cells[i])) {
				return Bool(false)
			}
		}
		return Bool(true)
	case LSortMap:
		if len(v.Map) != len(other.Map) {
			return Bool(false)
		}

		return Bool(false)
	}
	return Bool(false)
}

func (v *LVal) EqualNum(other *LVal) *LVal {
	if !v.IsNumeric() {
		return Errorf("receiver is not a number: %v", v.Type)
	}
	if !other.IsNumeric() {
		return Errorf("argument is not a number: %s", other.Type)
	}
	return v.equalNum(other)
}

func (v *LVal) equalNum(other *LVal) *LVal {
	if bothInt(v, other) {
		return Bool(v.Int == other.Int)
	}

	// This may not be correct
	return Bool(toFloat(v) == toFloat(other))
}

// Copy creates a deep copy of the receiver.
func (v *LVal) Copy() *LVal {
	if v == nil {
		return nil
	}
	cp := &LVal{}
	*cp = *v                 // shallow copy of all fields including Map
	cp.Cells = v.copyCells() // deep copy of v.Cells
	cp.Env = v.Env.Copy()    // deepish copy of v.Env
	return cp
}

func (v *LVal) copyMap() map[interface{}]*LVal {
	if v.Map == nil {
		return nil
	}
	m := make(map[interface{}]*LVal, len(v.Map))
	for k, v := range v.Map {
		// Is v.Copy() really necessary here? It seems like things get copied
		// on mapGet anyway..
		m[k] = v.Copy()
	}
	return m
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
		return QUOTE + v.Cells[0].str(true)
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
	case LBytes:
		return quote + fmt.Sprint(v.Bytes)
	case LError:
		return quote + v.Str
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
		vars := lambdaVars(v.Cells[0], boundVars(v))
		return fmt.Sprintf("%s(lambda %v%v)", quote, vars, bodyStr(v.Cells[1:]))
	case LQuote:
		// TODO: make more efficient
		return QUOTE + v.Cells[0].str(true)
	case LSortMap:
		return quote + sortedMapString(v)
	case LNative:
		return fmt.Sprintf("<native value: %T>", v.Native)
	case LMarkTailRec:
		return quote + fmt.Sprintf("<tail-recursion frames=%d (%s %s)>", v.Cells[0].Int, v.Cells[1], v.Cells[2])
	case LMarkMacExpand:
		return quote + fmt.Sprintf("<macro-expansion %s)>", v.Cells[0])
	default:
		return quote + fmt.Sprintf("<%s %#v>", v.Type, v)
	}
}

func bodyStr(exprs []*LVal) string {
	var buf bytes.Buffer
	for i := range exprs {
		buf.WriteString(" ")
		buf.WriteString(exprs[i].String())
	}
	return buf.String()
}

func lambdaVars(formals *LVal, bound *LVal) *LVal {
	s := SExpr([]*LVal{formals, bound})
	s = builtinConcat(nil, s)
	s.Quoted = false
	return s
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
	bound := SExpr(nil)
	for i := range keys {
		q := SExpr([]*LVal{
			Symbol(keys[i]),
			v.Env.Get(Symbol(keys[i])),
		})
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
