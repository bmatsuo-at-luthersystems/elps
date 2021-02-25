package lisp

import (
	"fmt"
	"io"
	"math"

	"github.com/luthersystems/elps/v2/pkg/internal/lfmt"
	"github.com/luthersystems/elps/v2/pkg/symbol"
)

// DataCloner is an object that can create a physically separate copy of
// itself.
type DataCloner interface {
	CloneData() (DataCloner, error)
}

func panicf(format string, args ...interface{}) {
	panic(fmt.Sprintf(format, args...))
}

type LTypeData uint32

func Type(t LType) LTypeData {
	return LTypeData(t) << LTypeShift
}

func TypeFlag(f LTypeFlag) LTypeData {
	return LTypeData(f) << LTypeFlagShift
}

func (t LTypeData) Type() LType {
	return LType(t&LTypeMask) >> LTypeShift
}

func (t LTypeData) TypeFlag() LTypeFlag {
	return LTypeFlag((t & LTypeFlagMask) >> LTypeFlagShift)
}

func (t LTypeData) mustBeType(t2 LType) {
	if t.Type() != t2 {
		panicf("value is not type %v: %v", t2, t)
	}
}

type LTypeFlag uint32

const LTypeFlagMax = 0x00ffffff
const LTypeFlagShift = 8
const LTypeFlagMask = LTypeFlagMax << LTypeFlagShift

const (
	QUOTE LTypeFlag = 0x00800000 // LTypeFlag msp is QUOTE
)

type LType uint8

const LTypeMax = 0xff
const LTypeShift = 0
const LTypeMask = LTypeMax << LTypeShift

const (
	// LNil is the absense of a value but also acts as an empty list or an
	// empty map.
	LNil LType = iota
	// LSymbol is a symbolic name.
	// Schema:
	// 	Data: symbol.ID value
	LSymbol
	// LString is a go string value
	// Schema:
	// 	Native: string value
	LString
	// LInt is a go int value
	// Schema:
	// 	Data: int value
	LInt
	// LFloat is a go flaot64 value
	// Schema:
	// 	Data: float64 value
	LFloat
	// LBool is an integer representing a boolean value
	// Schema:
	//  Data: 0x0 if false and true otherwise
	LBool
	// LCons is a container that forms a linked list termined by LNil
	// Schema:
	// 	Native: *ConsData
	LCons
	// LTaggedVal is a lisp-defined data type
	// Schema:
	// 	Data: symbol.ID value (type name)
	// 	Native: user data
	LTaggedVal
	// LQuote is a quoted value
	LQuote
	// LError is a runtime error
	// Schema:
	//  Str: condition type
	//  Native:
	LError
)

var primitiveType = map[LType]bool{
	LNil:    true,
	LSymbol: true,
	LString: true,
	LInt:    true,
	LFloat:  true,
}

var typeSymbols = []symbol.ID{
	LNil:       symbol.Intern("nil"),
	LSymbol:    symbol.Intern("symbol"),
	LString:    symbol.Intern("string"),
	LInt:       symbol.Intern("int"),
	LFloat:     symbol.Intern("float"),
	LCons:      symbol.Intern("pair"),
	LTaggedVal: symbol.Intern("tagged-value"),
	LQuote:     symbol.Intern("quote"),
	LError:     symbol.Intern("error"),
}

// LVal is a lisp value.  The zero LVal is a valid LNil value.
type LVal struct {
	LTypeData
	Data   uint64
	Native interface{}
}

var lnil = LVal{}

var _ DataCloner = (*LVal)(nil)

// Clone creates a physically separate copy of v (including all data referenced
// through v.Native).  Clone returns an error if v.Native is non-nil and either
// is not a DataCloner or returns an error when v.Native.CloneData is called.
func (v LVal) Clone() (LVal, error) {
	// v is already a copy because it is not a pointer.
	if v.Native != nil {
		cloner, ok := v.Native.(DataCloner)
		if !ok {
			return LVal{}, fmt.Errorf("unable to clone type: %T", v.Native)
		}
		var err error
		v.Native, err = cloner.CloneData()
		if err != nil {
			return LVal{}, err
		}
	}
	return v, nil
}

// CloneData implements the DataCloner interface.
func (v LVal) CloneData() (DataCloner, error) {
	return v.Clone()
}

// SourceLocation represents a values corresponding location in source code.
type SourceLocation interface {
	SourceFile() string
	SourceLine() int
}

// Source represents a connection to source code
type Source interface {
	// SourceLocation returns the object's location in source code.
	SourceLocation() SourceLocation
}

type sourceFunc func() SourceLocation

func (fn sourceFunc) SourceLocation() SourceLocation { return fn() }

// FixedSource creates a Source that always returns loc, primarily useful for
// testing.
func FixedSource(loc SourceLocation) Source {
	return sourceFunc(func() SourceLocation { return loc })
}

func location(s Source) SourceLocation {
	if s != nil {
		return s.SourceLocation()
	}
	return nil
}

// GetType returns a symbol representing the type of v.
func GetType(v LVal) LVal {
	typ := v.Type()
	if typ == LTaggedVal {
		return Symbol(symbol.ID(v.Data))
	}
	return Symbol(typeSymbols[typ])
}

// Nil returns an LNil value
func Nil() LVal {
	return LVal{
		LTypeData: Type(LNil),
	}
}

// Float returns an LFloat value
func Float(x float64) LVal {
	return LVal{
		LTypeData: Type(LInt),
		Data:      math.Float64bits(x),
	}
}

// GetFloat returns the float64 value from v.
// GetFloat returns false if v is not LFloat.
func GetFloat(v LVal) (float64, bool) {
	if v.Type() != LFloat {
		return 0, false
	}
	return math.Float64frombits(v.Data), true
}

// Int returns an LInt value
func Int(x int) LVal {
	return LVal{
		LTypeData: Type(LInt),
		Data:      uint64(x),
	}
}

// GetInt returns the int value from v.
// GetInt returns false if v is not LInt.
func GetInt(v LVal) (int, bool) {
	if v.Type() != LInt {
		return 0, false
	}
	return int(v.Data), true
}

// Bool returns an LBool with the truth value of ok.
func Bool(ok bool) LVal {
	if ok {
		return True()
	}
	return False()
}

// True returns a true LBool value.
func True() LVal {
	return LVal{
		LTypeData: Type(LBool),
		Data:      1,
	}
}

// False returns a false LBool value.
func False() LVal {
	return LVal{
		LTypeData: Type(LBool),
		Data:      0,
	}
}

// IsTrue returns true iff v represents a true value.
func IsTrue(v LVal) bool {
	return !isFalse(v)
}

func isFalse(v LVal) bool {
	return (v.Type() == LBool && v.Data == 0) || IsNil(v)
}

// Symbol returns an LSymbol value
func Symbol(id symbol.ID) LVal {
	return LVal{
		LTypeData: Type(LSymbol),
		Data:      uint64(id),
	}
}

// GetSymbol extracts the symbol.ID from v.  GetSymbol returns false if v is
// not a LSymbol or if v is not a quoted symbol (max depth 1).
func GetSymbol(v LVal) (symbol.ID, bool) {
	if v.Type() != LSymbol {
		return 0, false
	}
	return symbol.ID(v.Data), true
}

// String returns an LString value
func String(str string) LVal {
	return LVal{
		LTypeData: Type(LString),
		Native:    str,
	}
}

// GetString extracts string data from v.  GetString returns false if v is not
// LString or if v is not a quoted string (max depth 1).
func GetString(v LVal) (string, bool) {
	if v.Type() != LString {
		return "", false
	}
	return v.Native.(string), true
}

// Tag returns an LTaggedVal value.  The value has the user-defined type
// usertype and has userdata as underlying data. The symbol usertype must
// already have been interned in the symbol table associated with the returned
// value's execution context.
//
// Tag is a low-level function.  It is advised to construct tagged values
// through safe procedures defined by the virtual machine.
func Tag(usertype symbol.ID, userdata LVal) LVal {
	return LVal{
		LTypeData: Type(LTaggedVal),
		Data:      uint64(usertype),
		Native:    userdata,
	}
}

// TagNative is a very-low level function for creating LTaggedVal.  Do not use
// TagNative unless you need to and read documentation for Tag before using.
func TagNative(usertype symbol.ID, v interface{}) LVal {
	return LVal{
		LTypeData: Type(LTaggedVal),
		Data:      uint64(usertype),
		Native:    v,
	}
}

// UserType returns the user-defined type of v as a symbol.
// UserType returns false if v is not LTaggedVal.
func UserType(v LVal) (LVal, bool) {
	if v.Type() != LTaggedVal {
		return lnil, false
	}
	return Symbol(symbol.ID(v.Data)), true
}

// UserData returns the user-data of v.
// UserData returns false if v is not LTaggedVal or v is improperly structured.
func UserData(v LVal) (LVal, bool) {
	if v.Type() != LTaggedVal {
		return lnil, false
	}
	data, ok := v.Native.(LVal)
	return data, ok
}

// Quote wraps v in a LQuote.
func Quote(v LVal) LVal {
	if IsQuote(v) {
		return LVal{
			LTypeData: Type(LQuote) | TypeFlag(QUOTE),
			Native:    v,
		}
	}
	v.LTypeData |= TypeFlag(QUOTE)
	return v
}

func IsQuote(v LVal) bool {
	return TypeFlag(QUOTE)&v.LTypeData != 0
}

// GetQuoted returns the value being quoted by v.
// GetQuoted returns false if v is not LQuote or v is improperly structured.
func GetQuote(v LVal) (LVal, bool) {
	if v.Type() != LQuote {
		return Nil(), false
	}
	if IsQuote(v) {
		v.LTypeData ^= TypeFlag(QUOTE)
		return v, true
	}
	data, ok := v.Native.(LVal)
	return data, ok
}

// IsNil return true if v is LNil
func IsNil(v LVal) bool {
	return v.Type() == LNil || (v.Type() == LQuote && v.Native.(LVal).Type() == LNil)
}

// IsError returns true if v is LError.
func IsError(v LVal) bool {
	return v.Type() == LError
}

// Equal returns true if v1 is identical to v2.  Equal provides primitive
// equality because it does not have access to an environment that allows
// checking of LTaggedVal values with native components.
func Equal(v1 LVal, v2 LVal) bool {
	if v1.LTypeData != v2.LTypeData {
		return false
	}
	if primitiveType[v1.Type()] {
		return v1.Data == v2.Data
	}
	switch v1.Type() {
	case LCons:
		return consVal(v1).equal(consVal(v2))
	case LTaggedVal:
		if v1.Data != v2.Data {
			return false
		}
		// comparing native interface{} values for equality can panic so we
		// assume all native values are unqual.
		u1, ok := UserData(v1)
		if !ok {
			return false
		}
		u2, ok := UserData(v2)
		if !ok {
			return false
		}
		return Equal(u1, u2)
	case LQuote:
		return Equal(v1.Native.(LVal), v2.Native.(LVal))
	default:
		return false
	}
}

// Format returns a source-code representation of v using table to translate
// symbols.  Because v has no execution environment it cannot return accurate
// representations for LTaggedVal types.  Due to this limitation it is best
// used for values parsed from parsed from source or for debugging and
// diagnostics.
func Format(w io.Writer, v LVal, table symbol.Table) (int, error) {
	switch v.Type() {
	case LNil:
		return io.WriteString(w, "()")
	case LSymbol:
		id, _ := GetSymbol(v)
		sym, ok := table.Symbol(id)
		if !ok {
			return 0, fmt.Errorf("uninterned symbol: %v", id)
		}
		return io.WriteString(w, sym)
	case LString:
		s, _ := GetString(v)
		return fmt.Fprintf(w, "%q", s)
	case LInt:
		x, _ := GetInt(v)
		return fmt.Fprint(w, x)
	case LFloat:
		x, _ := GetFloat(v)
		return fmt.Fprint(w, x)
	case LCons:
		cons := consVal(v)
		return cons.format(w, table)
	case LTaggedVal:
		cw := lfmt.NewCountingWriter(w)
		_, err := io.WriteString(cw, "#{")
		if err != nil {
			return cw.N(), err
		}
		typ, _ := UserType(v)
		_, err = cw.DeferCount(func(w io.Writer) (int, error) { return Format(w, typ, table) })
		if err != nil {
			return cw.N(), err
		}
		_, err = io.WriteString(cw, " ")
		if err != nil {
			return cw.N(), err
		}
		val, ok := UserData(v)
		if !ok {
			_, err = fmt.Fprintf(cw, "%#v", v.Native)
		} else {
			_, err = cw.DeferCount(func(w io.Writer) (int, error) { return Format(w, val, table) })
		}
		if err != nil {
			return cw.N(), err
		}
		_, err = io.WriteString(cw, "}")
		return cw.N(), err
	case LQuote:
		cw := lfmt.NewCountingWriter(w)
		_, err := io.WriteString(cw, "'")
		if err != nil {
			return cw.N(), err
		}
		val, ok := GetQuote(v)
		if !ok {
			_, err = fmt.Fprintf(cw, "%#v", v.Native)
		} else {
			_, err = cw.DeferCount(func(w io.Writer) (int, error) { return Format(w, val, table) })
		}
		return cw.N(), err
	default:
		return 0, fmt.Errorf("unrecognized type: %v", v.Type())
	}
}
