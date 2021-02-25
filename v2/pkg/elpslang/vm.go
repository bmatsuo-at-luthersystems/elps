package elpslang

import (
	"fmt"

	"github.com/luthersystems/elps/v2/pkg/lisp"
	"github.com/luthersystems/elps/v2/pkg/machine"
	"github.com/luthersystems/elps/v2/pkg/register"
	"github.com/luthersystems/elps/v2/pkg/symbol"
)

// Assembler translates machine assembly instructions into executable machine
// code, returning a program that can be subsequently installed in a machine.
type Assembler interface {
	// VMInit is called once when the machine is first installed in the machine
	// and allows the assembler to allocate and initialize machine registers
	// and operations.
	VMInit(m *VM) error
	// ThreadInit is called once for each thread created in the machine and
	// allows the assembler to initialize thread-local values in t.Data.
	ThreadInit(t *machine.T) error
	// Assemble is called whenever new program code will be installed on the
	// machine.  The assembler produces a Program that will be installed into m
	// but m should not be modified during program assembly.
	Assemble(m *VM, name symbol.ID, program lisp.LVal) (*machine.Program, error)
}

// Operator is a primitive machine operation that operates on register values.
type Operator struct {
	Name symbol.ID
	Fun  OpFun
}

// OpFun is passed list of values and returns a result.  If an OpFun returns a
// non-nil error it will halt execution on the thread.
//
// An OpFun must be safe to call concurrently.
type OpFun func(...lisp.LVal) (lisp.LVal, error)

// VM is a register-based virtual machine that uses lisp.LVal as the primitive
// data type for registers and operators.
type VM struct {
	Symbols   symbol.Table
	Assembler Assembler
	// Reg contains global machine registers.  Assemblers must not cause
	// threads to modify these registers with SetValue at runtime.  Mutating
	// the value returned by GetValue must be synchronized by the assembler.
	Reg map[symbol.ID]*register.Register
	Op  map[symbol.ID]*Operator
	M   *machine.M
}

// NewVM initialize and retus a new VM.  If asm not nil, then InstallAssembler
// will be called before returning and any error encountered will be passed
// through to the caller.
func NewVM(asm Assembler) (*VM, error) {
	vm := &VM{
		Reg: make(map[symbol.ID]*register.Register),
		Op:  make(map[symbol.ID]*Operator),
		M:   machine.New(),
	}
	if asm != nil {
		err := vm.InstallAssembler(asm)
		if err != nil {
			return nil, err
		}
	}
	return vm, nil
}

// InstallAssembler sets m's assembler to asm and calls asm.InitializeMachine
// to prepare for program assembly.
func (m *VM) InstallAssembler(asm Assembler) error {
	err := asm.VMInit(m)
	if err != nil {
		return err
	}
	m.Assembler = asm
	return nil
}

var LUninit = symbol.Intern("UNINITIALIZED")
var LPC = symbol.Intern("pc")
var LStack = symbol.Intern("stack")

// AllocReg creates a new register referenced by the given name.
// AllocReg returns an error if id was defined previously.
func (m *VM) AllocReg(regid symbol.ID) error {
	_, ok := m.Reg[regid]
	if ok {
		return fmt.Errorf("register allocated multiple times: %s", m.symbolString(regid))
	}
	m.Reg[regid] = register.New(lisp.Symbol(LUninit), regid)
	return nil
}

// DefineOp binds opid to fun so it may be referenced by instructions at
// assembly time.
func (m *VM) DefineOp(opid symbol.ID, fun OpFun) error {
	_, ok := m.Op[opid]
	if ok {
		return fmt.Errorf("operation defined multiple times: %s", m.symbolString(opid))
	}
	m.Op[opid] = &Operator{
		Name: opid,
		Fun:  fun,
	}
	return nil
}

// AssembleAndInstall is a convenience function which invokes the machine's
// assembler and installs the produced object program.
func (m *VM) AssembleAndInstall(name symbol.ID, program lisp.LVal) error {
	if m.Assembler == nil {
		return fmt.Errorf("no program assembler")
	}
	p, err := m.Assembler.Assemble(m, name, program)
	if err != nil {
		return err
	}
	return m.M.Install(p)
}

// NewThread creates and initializes a new machine thread.  NewThread returns
// an erro if it failed to create the thread or if the assembler failed to
// initialize the thread.
//
// It is the callers responsibility to call DelThread when the thread is no
// longer needed.
func (m *VM) NewThread() (*machine.T, error) {
	if m.Assembler == nil {
		return nil, fmt.Errorf("no assembler")
	}
	t, err := m.M.NewThread()
	if err != nil {
		return nil, fmt.Errorf("thread create: %w", err)
	}
	err = m.Assembler.ThreadInit(t)
	if err != nil {
		err := t.Stop(false)
		if err != nil {
			// should not be possible
			panic(err)
		}
		return nil, fmt.Errorf("thread init: %w", err)
	}
	return t, nil
}

func (m *VM) symbols() symbol.Table {
	if m.Symbols != nil {
		return m.Symbols
	}
	return symbol.DefaultGlobalTable
}

func (m *VM) symbolString(id symbol.ID) string {
	if m.Symbols != nil {
		return symbol.String(id, m.Symbols)
	}
	return id.String()
}

// AllocReg is a convenience to allocate a sequence of registers in m.
func AllocReg(m *VM, regids ...symbol.ID) error {
	for _, id := range regids {
		err := m.AllocReg(id)
		if err != nil {
			return err
		}
	}
	return nil
}
