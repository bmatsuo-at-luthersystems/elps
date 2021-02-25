// Package rmach defines a structure for implementing register-based machine
// architectures.
package rmach

import (
	"fmt"

	"github.com/luthersystems/elps/v2/pkg/lisp"
	"github.com/luthersystems/elps/v2/pkg/machine"
	"github.com/luthersystems/elps/v2/pkg/register"
	"github.com/luthersystems/elps/v2/pkg/symbol"
)

// Assembler can create and link executable code for a register machine.
type Assembler interface {
	// Assemble creates an unlinked object program.
	Assemble(programText lisp.LVal) (Object, error)
}

// Object is validated assembly code, ready to be linked to a machine.
type Object interface {
	// Link installs assembled and linked machine code into m under the name
	// specified.  Before installing code to the machine Link must verify that
	// the given machine's architecture is compatible with the assembler which
	// created it.  Link is not required to check the symbol table of m but it
	// should be ensured that m's symbol table be a superset of the one used to
	// parse the original program text.  It is unspecified what happens if Link
	// is called multiple times, with or without the same argument.
	Link(m Machine, name symbol.ID) error
}

// LUninitializedRegister is an empty singleton type that is in each register
// before the first call to SetValue.
var LUninitializedRegister = symbol.Intern("unitialized-register")

func uninit() lisp.LVal { return lisp.TagNative(LUninitializedRegister, nil) }

// MachineArch describes the the registers and operators of a register machine.
type MachineArch interface {
	// MachineRegisters returns the machine's global register architecture.  These
	// registers are shared among threads.
	MachineRegisters() register.Arch
	// ThreadRegisters returns the machine's thread register architecture.  Each
	// machine thread must have a distinct array of these registers.
	ThreadRegisters() register.Arch
	// ThreadInit is a function that executes once when each thread is created.
	ThreadInit() func(t Thread) error

	machineArch()
}

// NewArch returns a new register machine architecture.  NewArch requires
// register architectures for global machine registers and thread registers.
// In addition to these required arguments any number of ArchOption
// configurations may be provided and are documented separately.
func NewArch(m register.Arch, t register.Arch, opt ...ArchOption) (MachineArch, error) {
	if m == nil {
		return nil, fmt.Errorf("nil machine register architecture")
	}
	if t == nil {
		return nil, fmt.Errorf("nil thread register architecture")
	}
	return newMArch(m, t, opt...)
}

func MustArch(m register.Arch, t register.Arch, opt ...ArchOption) MachineArch {
	arch, err := NewArch(m, t, opt...)
	if err != nil {
		panic("rmach.Arch construction failed: " + err.Error())
	}
	return arch
}

// ArchOption is an opaque type which can be passed to NewArch to configure a
// register machine architecture.
type ArchOption archOption

type archOption func(*march) error

/*
// WithSymbols will use table when the returned Option is passed to NewArch.
func WithSymbols(table symbol.Exporter) ArchOption {
	return func(a *march) error {
		if a.symbols != nil {
			return fmt.Errorf("two symbol tables provided for MachineArch")
		}
		a.symbols = table
		return nil
	}
}

// WithOpMap will install the given map operations when the returned Option is
// passed to NewArch.  For convenience and to reduce confusion errors returned
// by each Op will have there messaged prefixed by their name.
func WithOpMap(ops map[symbol.ID]Op) ArchOption {
	return func(a *march) error {
		for name, fn := range ops {
			name := name
			fn := fn
			_, ok := a.op[name]
			if ok {
				return fmt.Errorf("machine operator defined twice: %v", symbol.String(name, a.Symbols()))
			}
			a.op[name] = func(v ...lisp.LVal) (lisp.LVal, error) {
				w, err := fn(v...)
				if err == nil {
					return w, nil
				}
				return w, fmt.Errorf("%s: %w", symbol.String(name, a.Symbols()), err)
			}
		}
		return nil
	}
}
*/

// WithThreadInit will run fn for each created thread when the returned
// ArchOption is passed to NewArch.  Passing multiple WithThreadInit options to
// NewArch will run each of their functions in order when a thread is created.
func WithThreadInit(fn func(Thread) error) ArchOption {
	return func(a *march) error {
		a.tinit = append(a.tinit, fn)
		return nil
	}
}

type march struct {
	symbols symbol.Exporter
	m       register.Arch
	t       register.Arch
	op      OpMap
	tinit   []func(Thread) error
}

func newMArch(m register.Arch, t register.Arch, opt ...ArchOption) (*march, error) {
	a := &march{
		m:  m,
		t:  t,
		op: make(map[symbol.ID]Op),
	}
	for _, fn := range opt {
		err := fn(a)
		if err != nil {
			return nil, err
		}
	}
	return a, nil
}

func (a *march) Symbols() symbol.Exporter {
	if a.symbols == nil {
		return symbol.DefaultGlobalTable
	}
	return a.symbols
}

func (a *march) MachineRegisters() register.Arch {
	return a.m
}

func (a *march) ThreadRegisters() register.Arch {
	return a.t
}

func (a *march) ThreadInit() func(Thread) error {
	if len(a.tinit) == 0 {
		return nil
	}
	return func(thd Thread) error {
		for _, fn := range a.tinit {
			err := fn(thd)
			if err != nil {
				return err
			}
		}
		return nil
	}
}

func (a *march) Op(name symbol.ID) (Op, bool) {
	fn, ok := a.op[name]
	return fn, ok
}

// machineArch implements MachineArch
func (a *march) machineArch() {}

// Op is a primitive machine operator
type Op func(...lisp.LVal) (lisp.LVal, error)

type OpMap map[symbol.ID]Op

// Machine is an extension of machine.M that adds register-based data storage
// and the concept of primitive machine operations.
type Machine interface {
	// Op returns a specifies machine operator and returns false if no such
	// operator exists.
	Op(name symbol.ID) (Op, bool)
	// M provides access to the underlyng machine.M
	M() *machine.M
	// NewThread is like M().NewThread but it initializes the machine thread as
	// a Thread and invokes any ThreadInit function provided by the machine
	// architecture.
	NewThread() (*machine.T, error)
	// Machines acts as an unordered collection of registers.
	register.Map
	// MachineArch returns the describes the machine.
	MachineArch() MachineArch

	machine()
}

// rmachine implements Machine
type rmachine struct {
	m *machine.M
	register.Map
	arch MachineArch
	op   OpMap
}

var _ Machine = (*rmachine)(nil)

// NewMachine initializes and returns a Machine than implements the given
// machine architecture.
func NewMachine(arch MachineArch, table symbol.Table, op OpMap) (Machine, error) {
	m := &rmachine{
		m:    machine.New(),
		Map:  register.MakeMap(uninit(), arch.MachineRegisters()),
		arch: arch,
		op:   make(OpMap, len(op)),
	}
	if table == nil {
		table = symbol.NewTable(symbol.DefaultGlobalTable.Export()...)
	}
	m.m.Symbols = table
	for name, fn := range op {
		name := name
		fn := fn
		m.op[name] = func(v ...lisp.LVal) (lisp.LVal, error) {
			w, err := fn(v...)
			if err == nil {
				return w, nil
			}
			return w, fmt.Errorf("%s: %w", symbol.String(name, table), err)
		}
	}
	return m, nil
}

func (m *rmachine) Op(name symbol.ID) (Op, bool) {
	fn, ok := m.op[name]
	return fn, ok
}

// M implements Machine
func (m *rmachine) M() *machine.M {
	return m.m
}

// MachineArch implements Machine
func (m *rmachine) MachineArch() MachineArch {
	return m.arch
}

// NewThread implements Machine
func (m *rmachine) NewThread() (*machine.T, error) {
	t, err := m.M().NewThread()
	if err != nil {
		return nil, err
	}
	err = TInit(t, uninit(), m.MachineArch().ThreadRegisters())
	if err != nil {
		return nil, err
	}
	fn := m.MachineArch().ThreadInit()
	if fn != nil {
		err = fn(t.Data.(Thread))
		if err != nil {
			return nil, err
		}
	}
	return t, nil
}

// machine implements Machine
func (m *rmachine) machine() {}

// Thread is an abstraction of a machine thread where machine.T.Data holds a
// register array.
type Thread interface {
	// A thread contains a continuous array of registers.
	register.Array
	// OpArgs returns a slice with capacity at least n which can be used inside
	// a machine.Executor for temporary storage of operands.  The caller must
	// not retain a refences to the slice backing storage after the
	// machine.Executor returns and the caller must zero out any appended data
	// before returning from the machine.Executor.
	OpArgs(n int) []lisp.LVal
	// T returns the underlying machine thread.
	T() *machine.T
	thread()
}
type thread struct {
	register.Array
	opargs []lisp.LVal
	thd    *machine.T
}

var _ Thread = (*thread)(nil)

// T returns the underlying machine thread.
func (thd *thread) T() *machine.T {
	return thd.thd
}

func (thd *thread) OpArgs(n int) []lisp.LVal {
	if n > cap(thd.opargs) {
		thd.opargs = make([]lisp.LVal, 0, 2*n)
	}
	return thd.opargs
}

func (thd *thread) thread() {}

// GetThread extracts a *Thread from a machine thread previously initialized
// with TInit.
func GetThread(thd *machine.T) (Thread, bool) {
	_thd, ok := thd.Data.(*thread)
	return _thd, ok
}

// TInit initializes thd.Data to contain a Thread.
func TInit(thd *machine.T, initial lisp.LVal, arch register.Arch) error {
	if thd.Data != nil {
		return fmt.Errorf("thread is already initialized")
	}
	thd.Data = &thread{arch.MakeRegisterArray(initial), nil, thd}
	return nil
}

// GetThreadRegister returns the specified register from thd.  An assembler
// should not use this function in an Executor because it resolves the id to a
// register index at during execution time.
func GetThreadRegister(thd *machine.T, id symbol.ID) (*register.Register, error) {
	ethd, ok := GetThread(thd)
	if !ok {
		return nil, fmt.Errorf("unexpected machine thread data: %T", thd.Data)
	}
	i, ok := ethd.Arch().GetRegisterIndex(id)
	if !ok {
		return nil, fmt.Errorf("invalid register: %v", id)
	}
	return ethd.GetRegister(i)
}

// Executor is a rmach-equivalent of machine.Executor.
type Executor func(Thread) error

func (fn Executor) Executor() machine.Executor {
	return func(t *machine.T) error {
		thd, ok := GetThread(t)
		if !ok {
			return fmt.Errorf("machine thread data is not rmach.Thread: %T", t.Data)
		}
		return fn(thd)
	}
}
