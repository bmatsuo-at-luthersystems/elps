package langasm

import (
	"fmt"

	"github.com/luthersystems/elps/v2/pkg/machine"
	"github.com/luthersystems/elps/v2/pkg/register"
	"github.com/luthersystems/elps/v2/pkg/rmach"
	"github.com/luthersystems/elps/v2/pkg/symbol"
)

// Valid elps assembly instruction types.
var (
	// IAssign assigns a value to a register.
	//		(assign (reg val) (reg unev))
	//		(assign (reg continue) (label x))
	//		(assign (reg argl) (const '()))
	//		(assign (reg val) (op +) (reg val) (const 1))
	IAssign = symbol.Intern("assign")
	// ITest evaluates an operator on arguments and sets 'flag to the result.
	// If the operator returns an LError then 'condition will be set instead.
	//		(test (op >) (const 0) (reg val))
	ITest = symbol.Intern("test")
	// IBranch performs a conditional jump depending on the outcome of the
	// last test (in the 'flag register).
	//		(branch (label x))
	//		(branch (reg val))
	IBranch = symbol.Intern("branch")
	// IBranchCondition performs a conditional jump depending on whether an
	// operation put an error in the 'condition register.
	//		(branch-condition (label x))
	//		(branch-condition (reg val))
	IBranchCondition = symbol.Intern("branch-condition")
	// IBranchConditionSymbol performs a conditional jump depending on whether an
	// operation put an error in the 'condition register.
	//		(branch-condition-symbol (label x) (const 'type-error))
	//		(branch-condition-symbol (reg val) (const 'argument-error))
	IBranchConditionSymbol = symbol.Intern("branch-condition-symbol")
	// IGoto is an unconditional jump.
	//		(goto (label x))
	//		(goto (reg continue))
	IGoto = symbol.Intern("goto")
	// ISave saves the value of a register on the machine stack.
	//		(save (reg env))
	ISave = symbol.Intern("save")
	// IRestore restores a stack value to a named register.
	//		(restore (reg env))
	IRestore = symbol.Intern("restore")
	// IPerform applies an operator to the given operands.  If the operator
	// returns LError it is stored in the 'condition register.
	//		(perform (op print-stack-statistics))
	IPerform = symbol.Intern("perform")

	instructionTypes = map[symbol.ID]bool{
		IAssign:          true,
		ITest:            true,
		IBranch:          true,
		IBranchCondition: true,
		IGoto:            true,
		ISave:            true,
		IRestore:         true,
		IPerform:         true,
	}
)

// Valid elps assembly instruction parameter types.
var (
	// PLabel references a label -- a pointer to machine code.
	PLabel = symbol.Intern("label")
	// PReg references a machine register.
	PReg = symbol.Intern("reg")
	// POp references an machine operator.
	POp = symbol.Intern("op")
	// PConst references a constant value.
	PConst = symbol.Intern("const")

	paramTypes = map[symbol.ID]bool{
		PLabel: true,
		PReg:   true,
		POp:    true,
		PConst: true,
	}

	paramPrimitive = map[symbol.ID]bool{}
)

func registerArrayAccessor(i int) RegisterAccessor {
	return func(rs register.Array) (*register.Register, error) { return rs.GetRegister(i) }
}

// GetRegisterAccessor precomputes a register index from the regs architecture
// and returns an accessor function to retrieve that from a register array.
func GetRegisterAccessor(id symbol.ID, regs register.Arch) (RegisterAccessor, bool) {
	i, ok := regs.GetRegisterIndex(id)
	if !ok {
		return nil, false
	}
	return registerArrayAccessor(i), true
}

// GetThreadRegister returns the specified register from thd.  An assembler
// should not use this function in an Executor because it resolves the id to a
// register index at during execution time.
//
// TODO: this function is duplicated in the rmach package and this copy should
// be deleted.
func GetThreadRegister(thd *machine.T, id symbol.ID) (*register.Register, error) {
	ethd, ok := rmach.GetThread(thd)
	if !ok {
		return nil, fmt.Errorf("unexpected machine thread data: %T", thd.Data)
	}
	i, ok := ethd.Arch().GetRegisterIndex(id)
	if !ok {
		return nil, fmt.Errorf("invalid register: %v", id)
	}
	return ethd.GetRegister(i)
}
