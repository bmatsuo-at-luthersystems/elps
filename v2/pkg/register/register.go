package register

import (
	"github.com/luthersystems/elps/v2/pkg/lisp"
	"github.com/luthersystems/elps/v2/pkg/symbol"
)

// Register provides named storage for a single lisp value.
type Register struct {
	Value lisp.LVal
	Name  symbol.ID
	Trace func(name symbol.ID, oldval lisp.LVal, newval lisp.LVal)
}

// New initializes and returns a register with the given name.
// New initializes the value within the register to initial.
func New(initial lisp.LVal, name symbol.ID) *Register {
	return (&Register{}).init(initial, name)
}

func MakeSlice(initial lisp.LVal, names ...symbol.ID) []Register {
	storage := make([]Register, len(names))
	for i := range storage {
		storage[i].init(initial, names[i])
	}
	return storage
}

func (r *Register) init(initial lisp.LVal, name symbol.ID) *Register {
	*r = Register{Name: name, Value: initial}
	return r
}

// SetValue stores v in the register.  If s.Trace is non-nil SetValue will call
// s.Trace.
func (r *Register) SetValue(v lisp.LVal) {
	if r.Trace != nil {
		r.Trace(r.Name, r.Value, v)
	}
	r.Value = v
}

// GetValue returns the value stored in the register.
func (r *Register) GetValue() lisp.LVal {
	return r.Value
}
