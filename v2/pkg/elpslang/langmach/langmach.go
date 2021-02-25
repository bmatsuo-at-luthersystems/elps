// Package langmach combines all of the pieces for a virtual machine running
// elps.
package langmach

import (
	"fmt"

	"github.com/luthersystems/elps/v2/pkg/elpslang/langarch"
	"github.com/luthersystems/elps/v2/pkg/elpslang/langasm"
	"github.com/luthersystems/elps/v2/pkg/elpslang/langop"
	"github.com/luthersystems/elps/v2/pkg/environ"
	"github.com/luthersystems/elps/v2/pkg/lisp"
	"github.com/luthersystems/elps/v2/pkg/machine"
	"github.com/luthersystems/elps/v2/pkg/rmach"
	"github.com/luthersystems/elps/v2/pkg/symbol"
)

type EvalOption func(Machine, rmach.Thread) error

func WithThreadTraceFunc(fn func(labels []symbol.ID, code machine.Code)) EvalOption {
	return func(m Machine, thd rmach.Thread) error {
		thd.T().Trace = fn
		return nil
	}
}

func WithRegisterTraceFunc(regid symbol.ID, fn func(symbol.ID, lisp.LVal, lisp.LVal)) EvalOption {
	return func(m Machine, thd rmach.Thread) error {
		r, err := rmach.GetThreadRegister(thd.T(), regid)
		if err != nil {
			return fmt.Errorf("register trace: %w", err)
		}
		r.Trace = fn
		return nil
	}
}

func NewThread(m Machine, opt ...EvalOption) (*machine.T, error) {
	thd, err := m.NewThread()
	if err != nil {
		return nil, err
	}
	rthd, ok := rmach.GetThread(thd)
	if !ok {
		thd.Stop(false)
		return nil, fmt.Errorf("invalid thread data: %v", thd.Data)
	}
	for _, opt := range opt {
		err := opt(m, rthd)
		if err != nil {
			thd.Stop(false)
			return nil, fmt.Errorf("eval option: %w", err)
		}
	}
	return thd, nil
}

func Apply(thd *machine.T, fn lisp.LVal, args lisp.LVal) (lisp.LVal, error) {
	table := thd.M().Symbols
	pc, err := thd.M().LocatePC(table.Intern("main"), table.Intern("funcall"))
	if err != nil {
		return lisp.Nil(), err
	}
	err = thd.Jump(pc)
	if err != nil {
		return lisp.Nil(), err
	}
	rexp, err := rmach.GetThreadRegister(thd, langarch.RExp)
	if err != nil {
		return lisp.Nil(), err
	}
	rval, err := rmach.GetThreadRegister(thd, langarch.RVal)
	if err != nil {
		return lisp.Nil(), err
	}
	rexp.SetValue(lisp.Cons(fn, args))
	err = thd.Proceed()
	if err != nil {
		return lisp.Nil(), fmt.Errorf("execution error: %w", err)
	}
	return rval.GetValue(), nil
}

func Evaluate(m Machine, expr lisp.LVal, opt ...EvalOption) (lisp.LVal, error) {
	thd, err := NewThread(m, opt...)
	if err != nil {
		return lisp.Nil(), err
	}
	defer thd.Stop(false)
	rprogram, err := rmach.GetThreadRegister(thd, langarch.RProgram)
	if err != nil {
		return lisp.Nil(), err
	}
	rval, err := rmach.GetThreadRegister(thd, langarch.RVal)
	if err != nil {
		return lisp.Nil(), err
	}
	rprogram.SetValue(expr)
	err = thd.Start()
	if err != nil {
		return lisp.Nil(), err
	}
	return rval.GetValue(), nil
}

// AssembleAndLink assembles the list of assembly instructions instlist into a
// machine object and installs it under the given name.
func AssembleAndLink(m Machine, name symbol.ID, instlist lisp.LVal) error {
	obj, err := m.Assemble(instlist)
	if err != nil {
		return fmt.Errorf("assembly error: %w", err)
	}
	return obj.Link(m, name)
}

// Machine is a virtual machine which can run elps programs.
type Machine interface {
	rmach.Machine
	rmach.Assembler
	GlobalEnviron() *environ.Environ
	machine()
}

// NewMachine initializes and returns a new machine
func NewMachine(p langop.Printer, table symbol.Table, env *environ.Environ) (Machine, error) {
	return newMachine(p, table, env)
}

type lispMachine struct {
	rmach.Machine
	rmach.Assembler
	global *environ.Environ
}

func newMachine(p langop.Printer, table symbol.Table, env *environ.Environ) (*lispMachine, error) {
	m, err := langarch.NewMachine(p, table, env)
	if err != nil {
		return nil, err
	}
	a := langasm.NewAssembler(m.MachineArch())
	obj, err := a.Assemble(interpText)
	if err != nil {
		return nil, err
	}
	err = obj.Link(m, table.Intern("main"))
	if err != nil {
		return nil, err
	}
	return &lispMachine{m, a, env}, nil
}

func (m *lispMachine) GlobalEnviron() *environ.Environ {
	return m.global
}

func (m *lispMachine) machine() {}
