// Package langasm implements the machine language for elps.
package langasm

import (
	"github.com/luthersystems/elps/v2/pkg/elpslang"
	"github.com/luthersystems/elps/v2/pkg/lisp"
	"github.com/luthersystems/elps/v2/pkg/machine"
	"github.com/luthersystems/elps/v2/pkg/register"
	"github.com/luthersystems/elps/v2/pkg/rmach"
	"github.com/luthersystems/elps/v2/pkg/symbol"
)

// RegisterMachine is an abstraction for a register machine architecture built
// on elpslang.VM.  The Assembler type in this package is one possible
// implementation.
type RegisterMachine interface {
	// VM provides access to the underlyng virtual machine.
	VM() *elpslang.VM
	// Thread wraps a machine thread as a Thread.
	Thread(*machine.T) rmach.Thread
	// GlobalRegister returns a global machine register (shared by all
	// threads).  GlobalRegister returns false if the specified register does
	// not exist.
	GlobalRegister(symbol.ID) (*register.Register, bool)
	// ThreadRegisterAccessor returns a RegisterAccessor function that can be
	// used in machine code to quickly index a thread register.
	ThreadRegisterAccessor(symbol.ID) (RegisterAccessor, bool)
	// Op returns the specified machine operator or false if the operator is
	// not defined.
	Op(symbol.ID) (*elpslang.Operator, bool)
}

// RegisterAccessor allows a reference to a register to be constructed without
// a fixed RegisterSet (e.g. Thread).  Any VM framework must ensure that a
// RegisterAccessor always matches the RegisterSet it is being passed.
type RegisterAccessor func(register.Array) (*register.Register, error)

// ThreadExecutor is analogous to machine.Executor for a VM Arch.
type ThreadExecutor func(thd rmach.Thread) error

// LValExecutor is like a ThreadExecutor but returns a lisp value.
type LValExecutor func(thd rmach.Thread) (lisp.LVal, error)

// ArgExecutor is like LValExecutor but the value it produces is appended to a
// slice which is retured along with any error.
type ArgExecutor func(dest []lisp.LVal, thd rmach.Thread) ([]lisp.LVal, error)
