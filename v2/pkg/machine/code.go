package machine

import (
	"fmt"
	"io"
	"strings"
	"sync"

	"github.com/luthersystems/elps/v2/pkg/asm"
	"github.com/luthersystems/elps/v2/pkg/symbol"
)

// Breakpoint represents a machine breakpoint.  A mark that causes machine
// execution to pause so that registers may be inspected.
//
// TODO: Resolve how breakpoints should ideally work with multiple threads.
type Breakpoint struct {
	Program symbol.ID
	Label   symbol.ID
	Offset  int
}

// Format writes a human readable representation of the breakpoint to w.
func (bp Breakpoint) Format(w io.Writer, t symbol.Table) (int, error) {
	return fmt.Fprintf(w, "%s:%s[%+d]", symbol.String(bp.Program, t), symbol.String(bp.Label, t), bp.Offset)
}

// BreakpointError is returned when machine execution is paused by a
// breakpoint.
type BreakpointError struct {
	breakpoints []Breakpoint
	table       symbol.Table
}

// Error implements the error interface.
func (err *BreakpointError) Error() string {
	buf := &strings.Builder{}
	for i := range err.breakpoints {
		if i > 0 {
			io.WriteString(buf, " ")
		}
		// It doesn't seem like a big deal if the error message doesn't have
		// the proper symbol table.  The application inserted the breakpoints
		// in the first place so they should be handling them without needing
		// to render the error message.
		err.breakpoints[i].Format(buf, err.table)
	}
	return fmt.Sprintf("encountered breakpoints: %v", buf.String())
}

// Executor performs one instruction on a machine thread.
type Executor func(*T) error

// Code represents an indivisible unit of computation and typically corresponds
// to one assembly instruction.
type Code struct {
	// Text is the source instruction which produced Exec during the assembly
	// process.
	Text *asm.Instruction
	// Exec performs the instruction within a machine.  If Exec is nil the
	// instruction is considered a no-op.
	Exec Executor
}

type codeInternal struct {
	Code
	// Breakpoints mark the instruction as a breakpoint that will cause a
	// machine to pause for inspection.
	breakpoints []Breakpoint
	mut         sync.RWMutex
}

func (c *codeInternal) isBreakpointSet() bool {
	c.mut.RLock()
	defer c.mut.RUnlock()
	return len(c.breakpoints) != 0
}

func (c *codeInternal) setBreakpoint(bp Breakpoint) bool {
	c.mut.Lock()
	defer c.mut.Unlock()
	for i := range c.breakpoints {
		if bp == c.breakpoints[i] {
			return false
		}
	}
	c.breakpoints = append(c.breakpoints, bp)
	return true
}

func (c *codeInternal) cancelBreakpoint(bp Breakpoint) bool {
	c.mut.Lock()
	defer c.mut.Unlock()
	for i := range c.breakpoints {
		if bp == c.breakpoints[i] {
			c.rmBreakpoint(i)
			return true
		}
	}
	c.breakpoints = append(c.breakpoints, bp)
	return false
}

func (c *codeInternal) rmBreakpoint(i int) {
	if len(c.breakpoints) == 1 {
		c.breakpoints = nil
		return
	}
	bps := make([]Breakpoint, len(c.breakpoints)-1)
	copy(bps, c.breakpoints[:i])
	copy(bps, c.breakpoints[i+1:])
	c.breakpoints = bps
}

func (c *codeInternal) cancelAllBreakpoints() {
	c.mut.Lock()
	defer c.mut.Unlock()
	c.breakpoints = nil
}
