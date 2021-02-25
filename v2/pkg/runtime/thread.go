package runtime

import (
	"fmt"
	"sync/atomic"

	"github.com/luthersystems/elps/v2/pkg/lisp"
	"github.com/luthersystems/elps/v2/pkg/symbol"
)

// Program represents a sequence of expressions.
type Program interface {
	// NextExpression returns the next expression in the program sequence.  If
	// the program sequence is exhausted (EOF) then a false value is returned.
	NextExpression() (expr lisp.LVal, ok bool)
}

// A thread is a lisp execution context with a runtime and a stack.
type Thread struct {
	Source     lisp.SourceLocation
	Runtime    *Runtime
	sig        Signal
	Program    Program
	ParamStack []lisp.LVal
	//CallStack *callstack.Stack
}

var _ lisp.Runtime = (*Thread)(nil)

func (t *Thread) SourceLocation() lisp.SourceLocation {
	return t.Source
}

// IsKeyword implements lisp.Runtime
func (t *Thread) IsKeyword(id symbol.ID) bool {
	return t.Runtime.IsKeyword(id)
}

// PutGlobal implements lisp.Runtime
func (t *Thread) PutGlobal(id symbol.ID, v lisp.LVal) lisp.LVal {
	return t.Runtime.PutGlobal(id, v)
}

// GetGlobal implements lisp.Runtime
func (t *Thread) GetGlobal(id symbol.ID) lisp.LVal {
	return t.Runtime.GetGlobal(id)
}

// Symbol implements lisp.Runtime
func (t *Thread) Symbol(id symbol.ID) (string, bool) {
	return t.Runtime.Symbols.Symbol(id)
}

// Send signals t.
func (t *Thread) Send(sig Signal) {
	atomic.StoreUint32((*uint32)(&t.sig), uint32(sig))
}

// Error implements lisp.Runtime
func (t *Thread) Error(err interface{}) lisp.LVal {
	return lisp.Error(err)
}

// ClearSignal clears the current signal in t.  If current doesn't match the
// thread's signal ClearSignal will have no effect.  ClearSignal returns true
// if the signal was cleared.
func (t *Thread) ClearSignal(current Signal) bool {
	return atomic.CompareAndSwapUint32((*uint32)(&t.sig), uint32(current), uint32(SIG_NONE))
}

// CheckSignal returns the last received, uncleared signal or SIG_NONE if no
// signal has been received.
func (t *Thread) CheckSignal() Signal {
	return Signal(atomic.LoadUint32((*uint32)(&t.sig)))
}

// Execute runs the thread's program and leaves the result on the top of
// t.ParamStack.  If execution resulted in a runtime error an error is returned
// and the LError value will be on the top of t.ParamStack.
func (t *Thread) Execute() error {
	for {
		expr, ok := t.Program.NextExpression()
		if !ok {
			return nil
		}
		v := t.Eval(expr)
		t.ParamStack = append(t.ParamStack, v)
		if lisp.IsError(v) {
			return fmt.Errorf("runtime error encountered: %v", v)
		}
	}
}

// Eval evaluates the expression.
func (t *Thread) Eval(expr lisp.LVal) lisp.LVal {
	//t.Source = expr.SourceLocation
	switch sig := t.CheckSignal(); sig {
	case SIG_NONE:
		break
	case SIG_INTERRUPT:
		return t.Error(SignalError(SIG_INTERRUPT))
	case SIG_DUMPSTACK:
		// TODO
	default:
		return t.Error(fmt.Errorf("unrecognized signal: %v", sig))
	}
	switch expr.Type() {
	default:
		return expr
	}
}
