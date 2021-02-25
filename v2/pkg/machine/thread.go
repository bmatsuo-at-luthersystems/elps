package machine

import (
	"fmt"
	"sync"
	"sync/atomic"

	"github.com/luthersystems/elps/v2/pkg/symbol"
)

// KilledError is returned by Start/Proceed if a thread was halted with Kill.
type KilledError struct{}

func (err KilledError) Error() string { return "machine was killed" }

// TState represents the state of a thread.
type TState int64

const (
	// TReady means the thread is ready to Start or Proceed.
	TReady TState = iota
	// TRunning means the thread is executing instructions.
	TRunning
	// TStopped means the thread has been terminated and cannot Start or
	// Proceed again
	TStopped
)

// T is a machine thread.
type T struct {
	// Data is thread-local storage that may be used by instructions without
	// synchronization.
	Data interface{}

	// Trace is called before each instruction is execution if tracing is
	// enabled by called EnableTrace.  Before the program terminates Trace will
	// be called if any labels that reference to the EOF pseudo-instruction, in
	// which case inst will be nil.
	Trace func(labels []symbol.ID, code Code)

	id      int
	kill    *signal
	numStep int64
	state   TState

	// mut guards pc and Data from concurrent executions
	mut sync.Mutex
	pc  PC
	// pause is used solely to allow Stop to wait for a thread to complete.
	pause sync.Cond
}

func (t *T) State() TState {
	return TState(atomic.LoadInt64((*int64)(&t.state)))
}

func (t *T) setState(s TState) {
	atomic.StoreInt64((*int64)(&t.state), int64(s))
}

func (t *T) swapState(sold, snew TState) (swapped bool) {
	return atomic.CompareAndSwapInt64((*int64)(&t.state), int64(sold), int64(snew))
}

// NumStep returns the number of instructions execution since t.Start was
// called the thread (including any instruction that returned an error).
// NumStep may be called concurrently with Start, Proceed, and Step.
func (t *T) NumStep() int {
	return int(atomic.LoadInt64(&t.numStep))
}

// M returns the machine that owns T.
func (t *T) M() *M {
	return t.pc.m
}

// AdvancePC advances the thread PC to the next instruction in sequnece.
func (t *T) AdvancePC() {
	t.pc = t.pc.Advance()
}

// Jump sets the thread's PC to pc.  Jump returns an error if t and pc belong
// to different machines.
func (t *T) Jump(pc PC) error {
	if pc.m != t.pc.m {
		return fmt.Errorf("pc belongs to a different machine")
	}
	t.pc = pc
	return nil
}

// Kill signals t to halt and avoid executing any more instructions.  Kill may
// be called concurrently with Start, Proceed, and Step and execution will halt
// after the instruction currently being processed.
func (t *T) Kill() {
	t.kill.Send()
	t.pause.Signal()
}

// Start moves the PC to the first instruction of m.Objects[0] and begins the
// exuction of program instructions.  Start must not be called concurrently
// with Proceed, Step, or other calls to Start.
func (t *T) Start() error {
	return t.run(func() error {
		var err error
		t.pc, err = t.pc.m.EntryPoint()
		if err != nil {
			return err
		}
		atomic.StoreInt64(&t.numStep, 0)
		return t.execLoop()
	})
}

// Proceed starts/resumes execution of m's program from the current PC
// instruction.  If an error is encounted it will be returned.  If the machine
// encounted a breakpoint the returned error will be a BreakpointError and the
// machine may resume execution by calling Proceed again.  Proceed must not be
// called concurrently with Start, Step, or other calls to Proceed.
func (t *T) Proceed() error {
	return t.run(func() error {
		return t.execLoop()
	})
}

// Step executes the next machine instruction and returns.  Step must not be
// called concurrently with Start, Proceed, or other calls to Step.
//
// BUG:  Step does not print labels if it reaches EOF and the program has
// labels preceeding the EOF pseudo-instruction.  It's unclear if it should be
// printing such labels.
func (t *T) Step() error {
	return t.run(func() error {
		if t.pc.IsEOF() {
			return nil
		}
		return t.step()
	})
}

// Stop must be called when the thread is no longer needed to free resources in
// the machine.
func (t *T) Stop(wait bool) error {
	if !wait {
		// without waiting we just try once and give up if we fail.
		if !t.swapState(TReady, TStopped) {
			return fmt.Errorf("thread is cannot be stopped: %v", t.State())
		}
		t.M().rmThread(t)
		return nil
	}
	t.pause.L.Lock()
	defer t.pause.L.Unlock()
	for !t.swapState(TReady, TStopped) {
		if t.State() == TStopped {
			return fmt.Errorf("stopped by another goroutine")
		}
		t.pause.Wait()
	}
	t.M().rmThread(t)
	t.pause.Signal()
	return nil
}

func (t *T) run(fn func() error) error {
	if !t.swapState(TReady, TRunning) {
		return fmt.Errorf("thread is not runnable: %v", t.State())
	}
	defer t.pause.Signal()              // wake up any Stop waiting on pause when we're done
	defer t.swapState(TRunning, TReady) // step may set this to TStopped
	return fn()
}

func (t *T) execLoop() error {
	looped := false
	for !t.pc.IsEOF() {
		looped = true
		err := t.step()
		if err != nil {
			return err
		}
	}
	if t.Trace != nil && looped {
		labels := t.pc.Labels()
		if len(labels) > 0 {
			t.Trace(labels, Code{})
		}
	}
	return nil
}

func (t *T) step() error {
	if t.kill.Received() {
		return KilledError{}
	}
	code, ok := t.pc.code()
	if !ok {
		return fmt.Errorf("invalid pc: %v", t.pc)
	}
	if t.Trace != nil {
		t.Trace(t.pc.Labels(), code.Code)
	}
	if len(code.breakpoints) > 0 {
		return &BreakpointError{code.breakpoints, t.pc.m.symbols()}
	}
	err := code.Exec(t)
	atomic.AddInt64(&t.numStep, 1)
	if err != nil {
		// This thread seems unfit for future use because its Data
		// field is in an uncertain state.  It is unsafe to call Start
		// because it may depend on Data being left with a particular
		// value following program termination.  And it is unsafe to
		// call Proceed because this code partially executed.
		// The only option appears to be death.
		t.kill.Send()
		t.setState(TStopped)
		t.M().rmThread(t)
		return err
	}
	return nil
}
