package register

import (
	"fmt"

	"github.com/luthersystems/elps/v2/pkg/lisp"
	"github.com/luthersystems/elps/v2/pkg/symbol"
)

// Arch is a register architecture that defines a register array and allows
// advanced lookup of register index values for fast, constant time register
// access at runtime.
type Arch interface {
	// Registers returns the ordered list of registers defined by the
	// architecture.
	Registers() []symbol.ID
	// GetRegisterIndex returns the index of the named register within the
	// architecture's defined array.  If the named register is not defined in
	// the Arch then GetRegisterIndex must return the pair (-1, false).
	GetRegisterIndex(name symbol.ID) (int, bool)
	// MakeRegisterArray returns a new register array with the property that
	// GetRegister(i) returns a unique register for each unqiue non-negative
	// value returned by GetRegisterIndex
	MakeRegisterArray(initial lisp.LVal) Array
}

// MakeArch returns creates a new Arch that defines registers for each name
// given in identical order.  If the same name is given in more than one
// position an error will be returned.
//
// If MakeArch is passed a slice then its contents must not be modified while
// the returned Arch is in use.
func MakeArch(name ...symbol.ID) (Arch, error) {
	a := registerArch(name)
	if !a.namesAreUnique() {
		return nil, fmt.Errorf("register names are not unique")
	}
	return a, nil
}

// MustArch is the same as MakeArch but a runtime panic is issued when MakeArch
// would return an error.
func MustArch(name ...symbol.ID) Arch {
	a, err := MakeArch(name...)
	if err != nil {
		panic("unable to construct register architecture: " + err.Error())
	}
	return a
}

type registerArch []symbol.ID

var _ Arch = registerArch(nil)

func (a registerArch) namesAreUnique() bool {
	if len(a) == 0 {
		return true
	}
	m := make(map[symbol.ID]bool, len(a))
	for _, name := range a {
		if m[name] {
			return false
		}
		m[name] = true
	}
	return true
}

func (a registerArch) Registers() []symbol.ID {
	lis := make([]symbol.ID, len(a))
	copy(lis, []symbol.ID(a))
	return lis
}

func (a registerArch) MakeRegisterArray(initial lisp.LVal) Array {
	return makeRegisterArray(initial, []symbol.ID(a)...)
}

// TODO: Make registerArch a struct and fix slow index lookup
func (a registerArch) GetRegisterIndex(name symbol.ID) (int, bool) {
	for i, rname := range a {
		if rname == name {
			return i, true
		}
	}
	return -1, false
}

// An Array is a set of registers with constant time access using a numerical
// index 0 <= i < n for some n.
type Array interface {
	// Arch returns an arch describing the array.
	Arch() Arch
	// GetRegister returns a pointer the register at index i in the array or an
	// error if there is no such register.  If GetRegister(i) returns a non-nil
	// register then successive calls to GetRegister(i) must return a reference
	// to the same register.
	GetRegister(i int) (*Register, error)
	GetRegisterValue(i int) (lisp.LVal, error)
	SetRegisterValue(i int, v lisp.LVal) error
}

// MakeArray returns an array of registers which are physically adjacent in
// memory.
func MakeArray(initial lisp.LVal, names ...symbol.ID) Array {
	return makeRegisterArray(initial, names...)
}

type registerArray []Register

var _ Array = registerArray(nil)

func makeRegisterArray(initial lisp.LVal, names ...symbol.ID) registerArray {
	return registerArray(MakeSlice(initial, names...))
}

// Arch implements Array
func (a registerArray) Arch() Arch {
	arch := make(registerArch, len(a))
	for i := range a {
		arch[i] = a[i].Name
	}
	return arch
}

func (a registerArray) GetRegister(i int) (*Register, error) {
	if 0 <= i && i < len(a) {
		return &a[i], nil
	}
	return nil, fmt.Errorf("invalid register index: %d", i)
}

func (a registerArray) GetRegisterValue(i int) (lisp.LVal, error) {
	if 0 <= i && i < len(a) {
		return a[i].Value, nil
	}
	return lisp.Nil(), fmt.Errorf("invalid register index: %d", i)
}

func (a registerArray) SetRegisterValue(i int, v lisp.LVal) error {
	if 0 <= i && i < len(a) {
		a[i].SetValue(v)
		return nil
	}
	return fmt.Errorf("invalid register index: %d", i)
}

// Lookup is an analogue to the Map.LookupRegister method for Arrays.  Lookup
// will panic if v.Arch().GetRegisterIndex returns an index which cases
// v.GetRegister to return an error.
func Lookup(v Array, name symbol.ID) (*Register, bool) {
	i, ok := v.Arch().GetRegisterIndex(name)
	if !ok {
		return nil, false
	}
	r, err := v.GetRegister(i)
	if err != nil {
		panic("inconsistent machine register array: " + err.Error())
	}
	return r, true
}

// Map is a set of registers without a defined order.
type Map interface {
	Array
	LookupRegister(name symbol.ID) (*Register, bool)
}

// MakeMap constructs a register map that contains the registers defined by
// arch.
func MakeMap(initial lisp.LVal, arch Arch) Map {
	return newRegisterMap(initial, arch)
}

type registerMap struct {
	arch Arch
	Array
}

var _ Map = (*registerMap)(nil)

func newRegisterMap(initial lisp.LVal, arch Arch) *registerMap {
	return &registerMap{arch, arch.MakeRegisterArray(initial)}
}

// Arch implements Map.
func (m *registerMap) Arch() Arch {
	return m.arch
}

// LookupRegister implements Map.
func (m *registerMap) LookupRegister(name symbol.ID) (*Register, bool) {
	i, ok := m.arch.GetRegisterIndex(name)
	if !ok {
		return nil, false
	}
	r, err := m.GetRegister(i)
	if err != nil {
		panic("invalid Arch implementation: " + err.Error())
	}
	return r, true
}
