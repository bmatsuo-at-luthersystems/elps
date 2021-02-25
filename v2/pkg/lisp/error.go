package lisp

import "fmt"

type ErrorData struct {
	GoError   error
	LispError LVal
}

// BUG:  Error always uses the same symbol (0).
func Error(err interface{}) LVal {
	return ErrorCondition(Symbol(0), err)
}

func ErrorCondition(condition LVal, err interface{}) LVal {
	data := &ErrorData{}
	if condition.Type() != LSymbol {
		condition = Symbol(0)
	}
	switch err := err.(type) {
	case error:
		data.GoError = err
	case LVal:
		data.LispError = err
	default:
		data.GoError = fmt.Errorf("invalid error data: %T", err)
	}
	return LVal{
		LTypeData: Type(LError),
		Data:      condition.Data,
		Native:    data,
	}
}
