package lisp

/*

// Int returns an LInt with value x.
func Int(x int) LVal {
	return LVal{
		LTypeData: Type(LInt),
		Int:       x,
	}
}

// NumericVal can be used as either an int or a flaot64.
type NumericVal interface {
	LInt() LVal
	LFloat() LVal
}

// IntVal wraps LInt values and provides convenience methods.
type IntVal struct {
	LVal
}

// MustInt wraps v as IntVal.
// MustInt panics if v is not an LInt.
func MustInt(v LVal) IntVal {
	v.mustBeType(LInt)
	return IntVal{v}
}

// Value returns the int value of x
func (x IntVal) Value() int {
	return x.Int
}

// LInt is a noop conversion so that LInt acts like a numeric value.
func (x IntVal) LInt() LVal {
	return x.LVal
}

// LFloat is a float64 conversion function so that LInt acts like a numeric value.
func (x IntVal) LFloat() LVal {
	return Float(float64(x.Int))
}

// Float returns an LFloat with value x.
func Float(x float64) LVal {
	return LVal{
		LTypeData: Type(LFloat),
		Float:     x,
	}
}

// FloatVal wraps LFloat values and provides convenience methods.
type FloatVal struct {
	LVal
}

// MustFloat wraps v as IntVal.
// MustFloat panics if v is not an LFloat.
func MustFloat(v LVal) FloatVal {
	v.mustBeType(LFloat)
	return FloatVal{v}
}

// Value returns the float64 value of x
func (x FloatVal) Value() float64 {
	return x.Float
}

// LInt is a LInt conversion function so that LFlaot acts like a numeric value.
func (x FloatVal) LInt() LVal {
	return x.LVal
}

// LFloat is a noop conversion so that LFlaot acts like a numeric value.
func (x FloatVal) LFloat() LVal {
	return Float(float64(x.Int))
}
*/
