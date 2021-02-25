package langasm

import (
	"fmt"
	"log"
	"strings"

	"github.com/luthersystems/elps/v2/pkg/asm"
	"github.com/luthersystems/elps/v2/pkg/elpslang"
	"github.com/luthersystems/elps/v2/pkg/elpslang/langarch"
	"github.com/luthersystems/elps/v2/pkg/elpslang/stack"
	"github.com/luthersystems/elps/v2/pkg/lisp"
	"github.com/luthersystems/elps/v2/pkg/machine"
	"github.com/luthersystems/elps/v2/pkg/register"
	"github.com/luthersystems/elps/v2/pkg/rmach"
	"github.com/luthersystems/elps/v2/pkg/symbol"
)

type labelMap map[symbol.ID]machine.PC

type executor func(m rmach.Machine, labels labelMap) (rmach.Executor, error)

func (fn executor) Executor(m rmach.Machine, labels labelMap) (machine.Executor, error) {
	rexec, err := fn(m, labels)
	if err != nil {
		return nil, err
	}
	return rexec.Executor(), nil
}

type linkerCode struct {
	text *asm.Instruction
	executor
}

type programLinker struct {
	march  register.Arch
	tarch  register.Arch
	ops    []symbol.ID
	labels map[symbol.ID]int
	code   []linkerCode
}

var _ rmach.Object = (*programLinker)(nil)

// Link implements rmach.Object
func (ln *programLinker) Link(m rmach.Machine, pname symbol.ID) error {
	err := ln.checkArch(m)
	if err != nil {
		return err
	}
	mp := m.M().NewProgram(pname)
	labels := make(labelMap, len(ln.labels))
	for label, i := range ln.labels {
		pc, err := mp.DefineLabel(label, i)
		if err != nil {
			return fmt.Errorf("label error: %w", err)
		}
		labels[label] = pc
	}
	for i := range ln.code {
		fn, err := ln.code[i].Executor(m, labels)
		if err != nil {
			return fmt.Errorf("linking error for instruction %d: %w: %v", i, err, ln.code[i].text)
		}
		mp.Append(ln.code[i].text, fn)
	}
	err = m.M().Install(mp)
	if err != nil {
		return fmt.Errorf("installation error: %w", err)
	}
	return nil
}

func (ln *programLinker) checkArch(m rmach.Machine) error {
	mdesc := m.MachineArch()
	if !equalArch(ln.march, mdesc.MachineRegisters()) {
		return fmt.Errorf("machine register architecture does not match")
	}
	if !equalArch(ln.tarch, mdesc.ThreadRegisters()) {
		return fmt.Errorf("machine thread architecture does not match")
	}
	return nil
}

func equalArch(a, b register.Arch) bool {
	asym := a.Registers()
	bsym := b.Registers()
	if len(asym) != len(bsym) {
		return false
	}
	for i := range asym {
		if asym[i] != bsym[i] {
			return false
		}
	}
	return true
}

type assembler struct {
	arch rmach.MachineArch
}

var _ rmach.Assembler = (*assembler)(nil)

func (z *assembler) Assemble(program lisp.LVal) (rmach.Object, error) {
	ln := &programLinker{
		march: z.arch.MachineRegisters(),
		tarch: z.arch.ThreadRegisters(),
	}
	p := newProgram(z.arch)
	err := p.assemble(ln, program)
	if err != nil {
		return nil, err
	}
	return ln, nil
}

func NewAssembler(arch rmach.MachineArch) rmach.Assembler {
	return &assembler{arch}
}

// program holds the state of a program assembly
type program struct {
	z      rmach.MachineArch
	ops    map[symbol.ID]bool
	labels map[symbol.ID]int
}

func newProgram(z rmach.MachineArch) *program {
	return &program{
		z:      z,
		ops:    make(map[symbol.ID]bool),
		labels: make(map[symbol.ID]int),
	}
}

func (p *program) threadRegisterAccessor(id symbol.ID) (RegisterAccessor, error) {
	acsor, ok := GetRegisterAccessor(id, p.z.ThreadRegisters())
	if !ok {
		return nil, fmt.Errorf("invalid register: %v", p.z.ThreadRegisters().Registers())
	}
	return acsor, nil
}

func (p *program) threadRegisterIndex(id symbol.ID) (int, error) {
	i, ok := p.z.ThreadRegisters().GetRegisterIndex(id)
	if !ok {
		return i, fmt.Errorf("invalid register: %v", p.z.ThreadRegisters().Registers())
	}
	return i, nil
}

func (p *program) assemble(mp *programLinker, text lisp.LVal) error {
	insts, err := p.extractLabels(mp, text)
	if err != nil {
		return fmt.Errorf("program text error: %w", err)
	}
	// p.labels now contains all the program's labels
	for i, inst := range insts {
		exec, err := p.assembleInstruction(inst)
		if err != nil {
			return fmt.Errorf("program text error: line %d: %w", i, err)
		}
		mp.code = append(mp.code, linkerCode{inst, exec})
	}
	mp.labels = p.labels
	for name := range p.ops {
		mp.ops = append(mp.ops, name)
	}
	return nil
}

func (p *program) extractLabels(mp *programLinker, text lisp.LVal) ([]*asm.Instruction, error) {
	i := 0
	var insts []*asm.Instruction
	for !lisp.IsNil(text) {
		cons, ok := lisp.GetCons(text)
		if !ok {
			return insts, fmt.Errorf("program text is not a list: %v", symbol.ID(lisp.GetType(text).Data))
		}
		itext := cons.CAR()
		text = cons.CDR()
		if id, ok := lisp.GetSymbol(itext); ok {
			/*
				pc, err := mp.DefineLabel(id, i)
				if err != nil {
					return insts, err
				}
			*/
			p.labels[id] = i
			continue
		}
		inst, err := asm.ParseInstruction(itext)
		if err != nil {
			w := &strings.Builder{}
			_, _err := lisp.Format(w, itext,
				symbol.ResolveUnknown("#<UNKNOWN-SYMBOL %#x>", symbol.DefaultGlobalTable))
			if _err != nil {
				log.Printf("failed to format value: %v", _err)
			}
			return insts, fmt.Errorf("failed to parse instruction at index %d: %w: %v", i, err, w.String())
		}
		insts = append(insts, inst)
		i += 1
	}
	return insts, nil
}

func (p *program) assembleInstruction(inst *asm.Instruction) (executor, error) {
	err := validateInstruction(inst)
	if err != nil {
		return nil, err
	}
	var exec executor
	switch inst.Type {
	case IAssign:
		exec, err = p.assembleAssign(inst.Params)
	case ITest:
		exec, err = p.assembleTest(inst.Params)
	case IBranch:
		exec, err = p.assembleBranch(inst.Params)
	case IBranchCondition:
		exec, err = p.assembleBranchCondition(inst.Params)
	case IGoto:
		exec, err = p.assembleGoto(inst.Params)
	case ISave:
		exec, err = p.assembleSave(inst.Params)
	case IRestore:
		exec, err = p.assembleRestore(inst.Params)
	case IPerform:
		exec, err = p.assemblePerform(inst.Params)
	default:
		err = fmt.Errorf("unhandled instruction")
	}
	if err != nil {
		err = fmt.Errorf("%w: %v", err, instructionString(inst))
		return nil, err
	}
	return exec, nil
}

// assembleAssign assign a value to a specified register.
//		(assign (reg val) (reg env))
//		(assign (reg continue) (label x))
//		(assign (reg val) (op foo) (reg val))
//
func (p *program) assembleAssign(params []asm.Param) (executor, error) {
	if len(params) < 2 {
		return nil, fmt.Errorf("assign requires at least two parameters")
	}
	if params[0].Type != PReg {
		return nil, fmt.Errorf("invalid assign parameter")
	}
	regid, ok := lisp.GetSymbol(params[0].Value)
	if !ok {
		return nil, fmt.Errorf("invalid assign parameter value")
	}
	ireg, err := p.threadRegisterIndex(regid)
	if err != nil {
		return nil, err
	}
	icondition, err := p.threadRegisterIndex(langarch.RCondition)
	if err != nil {
		return nil, err
	}
	vexec, err := p.assembleValue(params[1:])
	if err != nil {
		return nil, fmt.Errorf("invalid assign: %w", err)
	}
	exec := func(m rmach.Machine, labels labelMap) (rmach.Executor, error) {
		vexec, err := vexec(m, labels)
		if err != nil {
			return nil, err
		}
		exec := func(thd rmach.Thread) error {
			v, err := vexec(thd)
			if err != nil {
				return err
			}
			if lisp.IsError(v) {
				err := thd.SetRegisterValue(icondition, v)
				if err != nil {
					return err
				}
			}
			thd.T().AdvancePC()
			return thd.SetRegisterValue(ireg, v)
		}
		return exec, nil
	}
	return exec, nil
}

// assembleTest performs an operation and sets the 'flag register based on the
// result.  A true result is stored as a 1 and a false result is stored as a 0.
//		(test (op foo) (reg x))
func (p *program) assembleTest(params []asm.Param) (executor, error) {
	if len(params) == 0 {
		return nil, fmt.Errorf("test requires at least one parameters")
	}
	if params[0].Type != POp {
		return nil, fmt.Errorf("invalid test parameter")
	}
	iflag, err := p.threadRegisterIndex(langarch.RFlag)
	if err != nil {
		return nil, err
	}
	icondition, err := p.threadRegisterIndex(langarch.RCondition)
	if err != nil {
		return nil, err
	}
	vexec, err := p.assembleOpVal(params)
	if err != nil {
		return nil, fmt.Errorf("invalid test: %w", err)
	}
	exec := func(m rmach.Machine, labels labelMap) (rmach.Executor, error) {
		vexec, err := vexec(m, labels)
		if err != nil {
			return nil, err
		}
		exec := func(thd rmach.Thread) error {
			v, err := vexec(thd)
			if err != nil {
				return err
			}
			if lisp.IsError(v) {
				err := thd.SetRegisterValue(icondition, v)
				if err != nil {
					return err
				}
			}
			thd.T().AdvancePC()
			return thd.SetRegisterValue(iflag, v)
		}
		return exec, nil
	}
	return exec, nil
}

// assembleBranch creates an executor that checks the 'flag register and jumps
// to the specified PC if 'flag is true.
//		(branch (label x))
//		(branch (reg val))
func (p *program) assembleBranch(params []asm.Param) (executor, error) {
	if len(params) != 1 {
		return nil, fmt.Errorf("branch requires one parameter")
	}
	if params[0].Type != PReg && params[0].Type != PLabel {
		return nil, fmt.Errorf("invalid branch parameter value")
	}
	movePC, err := p.assembleBranchExecutor(&params[0])
	if err != nil {
		return nil, err
	}
	iflag, err := p.threadRegisterIndex(langarch.RFlag)
	if err != nil {
		return nil, fmt.Errorf("branch requires %s register", langarch.RFlag)
	}
	exec := func(m rmach.Machine, labels labelMap) (rmach.Executor, error) {
		movePC, err := movePC(m, labels)
		if err != nil {
			return nil, err
		}
		exec := func(thd rmach.Thread) error {
			f, err := thd.GetRegister(iflag)
			if err != nil {
				return err
			}
			if lisp.IsTrue(f.GetValue()) {
				return movePC(thd)
			}
			thd.T().AdvancePC()
			return nil
		}
		return exec, nil
	}
	return exec, nil
}

// assembleBranchCondition creates an executor that checks the 'condition
// register and jumps to the specified PC if it is non-nil.
func (p *program) assembleBranchCondition(params []asm.Param) (executor, error) {
	if len(params) != 1 {
		return nil, fmt.Errorf("branch-conditon requires one parameter")
	}
	if params[0].Type != PReg && params[0].Type != PLabel {
		return nil, fmt.Errorf("invalid branch-condition parameter value")
	}
	movePC, err := p.assembleBranchExecutor(&params[0])
	if err != nil {
		return nil, err
	}
	icondition, err := p.threadRegisterIndex(langarch.RCondition)
	if err != nil {
		return nil, fmt.Errorf("branch-condition requires %s register", langarch.RCondition)
	}
	exec := func(m rmach.Machine, labels labelMap) (rmach.Executor, error) {
		movePC, err := movePC(m, labels)
		if err != nil {
			return nil, err
		}
		exec := func(thd rmach.Thread) error {
			c, err := thd.GetRegister(icondition)
			if err != nil {
				return err
			}
			if lisp.IsNil(c.GetValue()) {
				thd.T().AdvancePC()
				return nil
			}
			return movePC(thd)
		}
		return exec, nil
	}
	return exec, nil
}

func (p *program) assembleBranchExecutor(param *asm.Param) (executor, error) {
	id, ok := lisp.GetSymbol(param.Value)
	if !ok {
		return nil, fmt.Errorf("invalid parameter value")
	}
	switch param.Type {
	case PLabel:
		movePC := func(m rmach.Machine, labels labelMap) (rmach.Executor, error) {
			pc, ok := labels[id]
			if !ok {
				return nil, fmt.Errorf("unable to link label: %v", id)
			}
			return func(thd rmach.Thread) error { return thd.T().Jump(pc) }, nil
		}
		return movePC, nil
	case PReg:
		ireg, err := p.threadRegisterIndex(id)
		if err != nil {
			return nil, err
		}
		movePC := func(m rmach.Machine, labels labelMap) (rmach.Executor, error) {
			exec := func(thd rmach.Thread) error {
				r, err := thd.GetRegister(ireg)
				if err != nil {
					return err
				}
				pc, ok := machine.GetPC(r.GetValue())
				if !ok {
					return fmt.Errorf("register does not contain a pc")
				}
				return thd.T().Jump(pc)
			}
			return exec, nil
		}
		return movePC, nil
	default:
		return nil, fmt.Errorf("unexpected parameter type")
	}
}

// assembleGoto creates an executor that unconditionally jumps to the specified
// PC.
//		(goto (label x))
//		(goto (reg continue))
func (p *program) assembleGoto(params []asm.Param) (executor, error) {
	if len(params) != 1 {
		return nil, fmt.Errorf("goto requires one parameter")
	}
	if params[0].Type != PReg && params[0].Type != PLabel {
		return nil, fmt.Errorf("invalid goto parameter type")
	}
	id, ok := lisp.GetSymbol(params[0].Value)
	if !ok {
		return nil, fmt.Errorf("invalid goto parameter value")
	}
	var getPC func(rmach.Machine, labelMap) (func(rmach.Thread) (machine.PC, error), error)
	switch params[0].Type {
	case PLabel:
		getPC = func(m rmach.Machine, labels labelMap) (func(rmach.Thread) (machine.PC, error), error) {
			pc, ok := labels[id]
			if !ok {
				return nil, fmt.Errorf("unable to link label: %v", id)
			}
			return func(_ rmach.Thread) (machine.PC, error) { return pc, nil }, nil
		}
	case PReg:
		ireg, err := p.threadRegisterIndex(id)
		if err != nil {
			return nil, err
		}
		getPC = func(m rmach.Machine, labels labelMap) (func(rmach.Thread) (machine.PC, error), error) {
			exec := func(thd rmach.Thread) (machine.PC, error) {
				r, err := thd.GetRegister(ireg)
				if err != nil {
					return machine.PC{}, err
				}
				pc, ok := machine.GetPC(r.GetValue())
				if !ok {
					return pc, fmt.Errorf("register does not contain a pc")
				}
				return pc, nil
			}
			return exec, nil
		}
	default:
		return nil, fmt.Errorf("unexpected parameter type")
	}
	exec := func(m rmach.Machine, labels labelMap) (rmach.Executor, error) {
		getPC, err := getPC(m, labels)
		if err != nil {
			return nil, err
		}
		exec := func(thd rmach.Thread) error {
			pc, err := getPC(thd)
			if err != nil {
				return err
			}
			return thd.T().Jump(pc)
		}
		return exec, nil
	}
	return exec, nil
}

// assembleSave creates an executor that pushes the value in the specified
// register onto the stack.
//		(save (reg env))
func (p *program) assembleSave(params []asm.Param) (executor, error) {
	if len(params) != 1 {
		return nil, fmt.Errorf("save requires one parameter")
	}
	param := params[0]
	if param.Type != PReg {
		return nil, fmt.Errorf("save parameter is not %s", PReg)
	}
	id, ok := lisp.GetSymbol(param.Value)
	if !ok {
		return nil, fmt.Errorf("invalid save parameter")
	}
	ireg, err := p.threadRegisterIndex(id)
	if err != nil {
		return nil, err
	}
	istk, err := p.threadRegisterIndex(langarch.RStack)
	if err != nil {
		return nil, fmt.Errorf("save requires %s register", langarch.RStack)
	}
	exec := func(m rmach.Machine, labels labelMap) (rmach.Executor, error) {
		exec := func(thd rmach.Thread) error {
			s, err := thd.GetRegister(istk)
			if err != nil {
				return err
			}
			r, err := thd.GetRegister(ireg)
			if err != nil {
				return err
			}
			err = stack.Push(s.GetValue(), r.GetValue())
			if err != nil {
				return err
			}
			thd.T().AdvancePC()
			return nil
		}
		return exec, nil
	}
	return exec, nil
}

// assembleRestore creates an executor that pops a value from the stack and
// stores it is the specified register.
//		(restore (reg continue))
func (p *program) assembleRestore(params []asm.Param) (executor, error) {
	if len(params) != 1 {
		return nil, fmt.Errorf("restore requires one parameter")
	}
	param := params[0]
	if param.Type != PReg {
		return nil, fmt.Errorf("restore parameter is not %s", PReg)
	}
	id, ok := lisp.GetSymbol(param.Value)
	if !ok {
		return nil, fmt.Errorf("invalid restore parameter")
	}
	ireg, err := p.threadRegisterIndex(id)
	if err != nil {
		return nil, err
	}
	istk, err := p.threadRegisterIndex(langarch.RStack)
	if err != nil {
		return nil, fmt.Errorf("restore requires %s register", langarch.RStack)
	}
	exec := func(m rmach.Machine, labels labelMap) (rmach.Executor, error) {
		exec := func(thd rmach.Thread) error {
			s, err := thd.GetRegister(istk)
			if err != nil {
				return err
			}
			v, err := stack.Pop(s.GetValue())
			if err != nil {
				return err
			}
			thd.T().AdvancePC()
			return thd.SetRegisterValue(ireg, v)
		}
		return exec, nil
	}
	return exec, nil
}

// assemblePerform creates an executor that performs an operation and ignores
// the result.
//		(perform (op side-effecty) (reg val) (reg env))
func (p *program) assemblePerform(params []asm.Param) (executor, error) {
	if len(params) == 0 {
		return nil, fmt.Errorf("perform requires at least one parameter")
	}
	if params[0].Type != POp {
		return nil, fmt.Errorf("perform parameter is not %s", POp)
	}
	lexec, err := p.assembleOpVal(params)
	if err != nil {
		return nil, err
	}
	icondition, err := p.threadRegisterIndex(langarch.RCondition)
	if err != nil {
		return nil, fmt.Errorf("perform requires %s register", langarch.RCondition)
	}
	exec := func(m rmach.Machine, labels labelMap) (rmach.Executor, error) {
		lexec, err := lexec(m, labels)
		if err != nil {
			return nil, err
		}
		exec := func(thd rmach.Thread) error {
			v, err := lexec(thd)
			if err != nil {
				return err
			}
			if lisp.IsError(v) {
				err := thd.SetRegisterValue(icondition, v)
				if err != nil {
					return err
				}
			}
			thd.T().AdvancePC()
			return nil
		}
		return exec, nil
	}
	return exec, nil
}

type lvalExecutor func(rmach.Machine, labelMap) (LValExecutor, error)

func (p *program) assembleValue(params []asm.Param) (lvalExecutor, error) {
	if len(params) == 0 {
		return nil, fmt.Errorf("not enough parameters")
	}
	switch params[0].Type {
	case PConst:
		if len(params) != 1 {
			return nil, fmt.Errorf("too many instruction parameters given")
		}
		return p.assembleConstVal(&params[0]), nil
	case PReg:
		if len(params) != 1 {
			return nil, fmt.Errorf("too many instruction parameters given")
		}
		return p.assembleRegVal(&params[0])
	case PLabel:
		if len(params) != 1 {
			return nil, fmt.Errorf("too many instruction parameters given")
		}
		return p.assembleLabelVal(&params[0])
	case POp:
		return p.assembleOpVal(params)
	default:
		return nil, fmt.Errorf("invalid parameter type")
	}
}

// argExecutor appends a value to ThreadData.OpArgs
type argExecutor func(m rmach.Machine, labels labelMap) (ArgExecutor, error)

func (p *program) assembleArg(param *asm.Param) (argExecutor, error) {
	switch param.Type {
	case PConst:
		return p.assembleConstArg(param), nil
	case PReg:
		return p.assembleRegArg(param)
	default:
		return nil, fmt.Errorf("invalid op argument type")
	}
}

func (p *program) assembleConstVal(param *asm.Param) lvalExecutor {
	// p.Type assumed to be PConst
	exec := func(m rmach.Machine, labels labelMap) (LValExecutor, error) {
		exec := func(_ rmach.Thread) (lisp.LVal, error) {
			return param.Value.Clone()
		}
		return exec, nil
	}
	return exec
}

func (p *program) assembleConstArg(param *asm.Param) argExecutor {
	// p.Type assumed to be PConst
	exec := func(m rmach.Machine, labels labelMap) (ArgExecutor, error) {
		exec := func(args []lisp.LVal, _ rmach.Thread) ([]lisp.LVal, error) {
			v, err := param.Value.Clone()
			return append(args, v), err
		}
		return exec, nil
	}
	return exec
}

func (p *program) assembleRegVal(param *asm.Param) (lvalExecutor, error) {
	// p.Type assumed to be PReg
	id, ok := lisp.GetSymbol(param.Value)
	if !ok {
		return nil, fmt.Errorf("invalid %s parameter", PReg)
	}
	ireg, err := p.threadRegisterIndex(id)
	if err != nil {
		return nil, err
	}
	exec := func(m rmach.Machine, labels labelMap) (LValExecutor, error) {
		exec := func(thd rmach.Thread) (lisp.LVal, error) {
			return thd.GetRegisterValue(ireg)
		}
		return exec, nil
	}
	return exec, nil
}

func (p *program) assembleRegArg(param *asm.Param) (argExecutor, error) {
	// p.Type assumed to be PReg
	id, ok := lisp.GetSymbol(param.Value)
	if !ok {
		return nil, fmt.Errorf("invalid %s parameter", PReg)
	}
	ireg, err := p.threadRegisterIndex(id)
	if err != nil {
		return nil, err
	}
	exec := func(m rmach.Machine, labels labelMap) (ArgExecutor, error) {
		exec := func(args []lisp.LVal, thd rmach.Thread) ([]lisp.LVal, error) {
			v, err := thd.GetRegisterValue(ireg)
			return append(args, v), err
		}
		return exec, nil
	}
	return exec, nil
}

func (p *program) assembleLabelVal(param *asm.Param) (lvalExecutor, error) {
	// p.Type assumed to be PLabel
	id, ok := lisp.GetSymbol(param.Value)
	if !ok {
		return nil, fmt.Errorf("invalid %s parameter", PLabel)
	}
	exec := func(m rmach.Machine, labels labelMap) (LValExecutor, error) {
		pc, ok := labels[id]
		if !ok {
			return nil, fmt.Errorf("unable to link label: %v", id)
		}
		lpc := pc.LVal(nil, elpslang.LPC)
		exec := func(thd rmach.Thread) (lisp.LVal, error) { return lpc, nil }
		return exec, nil
	}
	return exec, nil
}

func (p *program) assembleOpVal(params []asm.Param) (lvalExecutor, error) {
	// params[0].Type assumed to be POp
	opid, ok := lisp.GetSymbol(params[0].Value)
	if !ok {
		return nil, fmt.Errorf("invalid %s parameter", POp)
	}
	p.ops[opid] = true
	argParams := params[1:]
	var args []argExecutor
	for i := range argParams {
		exec, err := p.assembleArg(&argParams[i])
		if err != nil {
			return nil, fmt.Errorf("invalid op argument: %w", err)
		}
		args = append(args, exec)
	}
	exec := func(m rmach.Machine, labels labelMap) (LValExecutor, error) {
		op, ok := m.Op(opid)
		if !ok {
			return nil, fmt.Errorf("invalid op: %v", symbol.String(opid, m.M().Symbols))
		}
		var err error
		_args := make([]ArgExecutor, len(args))
		for i := range args {
			_args[i], err = args[i](m, labels)
			if err != nil {
				return nil, err
			}
		}
		argexec := wrapArgExecutors(_args)
		exec := func(thd rmach.Thread) (lisp.LVal, error) {
			var err error
			opargs, err := argexec(thd.OpArgs(len(_args)), thd)
			if err != nil {
				return lisp.Nil(), err
			}
			v, err := op(opargs...)
			for i := range opargs {
				// zero out opargs before returning to allow any referenced memory
				// to be garbage collected.
				opargs[i] = lisp.LVal{}
			}
			return v, err
		}
		return exec, nil
	}
	return exec, nil
}

func wrapArgExecutors(execs []ArgExecutor) ArgExecutor {
	if len(execs) == 0 {
		return func(dest []lisp.LVal, _ rmach.Thread) ([]lisp.LVal, error) {
			return dest, nil
		}
	}
	if len(execs) == 1 {
		return execs[0]
	}
	if len(execs) == 2 {
		e1 := execs[0]
		e2 := execs[1]
		return func(dest []lisp.LVal, thd rmach.Thread) ([]lisp.LVal, error) {
			dest, err := e1(dest, thd)
			if err == nil {
				return e2(dest, thd)
			}
			return dest, err
		}
	}
	return func(dest []lisp.LVal, thd rmach.Thread) ([]lisp.LVal, error) {
		var err error
		for i := range execs {
			dest, err = execs[i](dest, thd)
			if err != nil {
				return dest, err
			}
		}
		return dest, nil
	}
}

func safeSymbolTable(table symbol.Table) symbol.Table {
	if table == nil {
		table = symbol.DefaultGlobalTable
	}
	return symbol.ResolveUnknown("#<SYMBOL %#x>", table)
}

// TODO:  Fix symbol lookup in this function
func instructionString(inst *asm.Instruction) string {
	table := safeSymbolTable(nil)
	repr, err := asm.InstructionString(inst, table)
	if err != nil {
		return fmt.Sprintf("%#v", inst)
	}
	return repr
}

// TODO:  Fix symbol lookup in this function
func paramString(p asm.Param) string {
	table := safeSymbolTable(nil)
	repr, err := asm.ParamString(p, table)
	if err != nil {
		return fmt.Sprintf("%#v", p)
	}
	return repr
}

// TODO:  Fix symbol lookup in this function
func validateInstruction(inst *asm.Instruction) error {
	if !instructionTypes[inst.Type] {
		return fmt.Errorf("invalid instruction type: %v", instructionString(inst))
	}
	for i := range inst.Params {
		if !paramTypes[inst.Params[i].Type] {
			return fmt.Errorf("invalid param type: %v", instructionString(inst))
		}
	}
	return nil
}
