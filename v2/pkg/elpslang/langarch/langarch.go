// Pacakge langarch declares a register machine architecture and provides a
// virtual machine implementation.
package langarch

import (
	"fmt"

	"github.com/luthersystems/elps/v2/pkg/elpslang/langop"
	"github.com/luthersystems/elps/v2/pkg/elpslang/stack"
	"github.com/luthersystems/elps/v2/pkg/environ"
	"github.com/luthersystems/elps/v2/pkg/lisp"
	"github.com/luthersystems/elps/v2/pkg/register"
	"github.com/luthersystems/elps/v2/pkg/rmach"
	"github.com/luthersystems/elps/v2/pkg/symbol"
)

const (
	rstack = iota
	rcontinue
	rval
	renv
	runev
	rflag
	rcondition
	rargl
	rproc
	rprogram
	rexp

	rmax
)

// Valid elps assembly registers
var (
	// RStack references the Stack register
	RStack = symbol.Intern("stack")
	// RContinue references the Continue register
	RContinue = symbol.Intern("continue")
	// RVal references the Val register
	RVal = symbol.Intern("val")
	// REnv references the Env register
	REnv = symbol.Intern("env")
	// RUnev references the Unev register
	RUnev = symbol.Intern("unev")
	// RFlag references the Flag register
	RFlag = symbol.Intern("flag")
	// RCondition references the Condition register
	RCondition = symbol.Intern("condition")
	// RArgL references the ArgL register
	RArgL = symbol.Intern("argl")
	// RProc references the Proc register
	RProc = symbol.Intern("proc")
	// RProgram references the Program register
	RProgram = symbol.Intern("program")
	// RExp references the Exp register
	RExp = symbol.Intern("exp")

	elpsRegisterOrder = []symbol.ID{
		rstack:     RStack,
		rcontinue:  RContinue,
		rval:       RVal,
		renv:       REnv,
		runev:      RUnev,
		rflag:      RFlag,
		rcondition: RCondition,
		rargl:      RArgL,
		rproc:      RProc,
		rprogram:   RProgram,
		rexp:       RExp,
	}
)

var (
	machineRegisters = register.MustArch()
	threadRegisters  = register.MustArch(elpsRegisterOrder...)
)

// Arch returns the register machine architecture for elps.
func Arch() rmach.MachineArch {
	return rmach.MustArch(
		machineRegisters,
		threadRegisters,
		rmach.WithThreadInit(func(thd rmach.Thread) error {
			for k, v := range map[symbol.ID]lisp.LVal{
				RStack:     stack.New().LVal(nil, RStack),
				RFlag:      lisp.False(),
				RCondition: lisp.Nil(),
			} {
				i, ok := threadRegisters.GetRegisterIndex(k)
				if !ok {
					return fmt.Errorf("unable to find the %v register", k)
				}
				r, err := thd.GetRegister(i)
				if err != nil {
					return fmt.Errorf("register architecture bug: %w", err)
				}
				r.SetValue(v)
			}
			return nil
		}),
	)
}

// NewMachine is a shorthand to create a new rmach.Machine with the elps
// machine architecture.
func NewMachine(p langop.Printer, table symbol.Table, env *environ.Environ) (rmach.Machine, error) {
	return rmach.NewMachine(Arch(), table, langop.InterpOps(p, table, env))
}
