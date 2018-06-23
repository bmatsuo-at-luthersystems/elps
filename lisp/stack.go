package lisp

import (
	"bytes"
	"fmt"
	"io"
	"log"
	"os"
)

// CallStack is a function call stack.
type CallStack struct {
	Frames []CallFrame
}

// CallFrame is one frame in the CallStack
type CallFrame struct {
	FID      string
	Package  string
	Name     string
	Terminal bool
	TROBlock bool // Stop tail-recursion optimization from collapsing this frame
}

// QualifiedFunName returns the qualified name for the function on the top of
// the stack.  If ignore is non-empty QualifiedFunName returns unqualified
// names for functions in the given packages.
func (f *CallFrame) QualifiedFunName(ignore ...string) string {
	if f == nil {
		return ""
	}
	name := f.Name
	if f.Name == "" {
		name = f.FID
	}
	if f.Package == "" {
		return name
	}
	for _, pkg := range ignore {
		if pkg == f.Package {
			return name
		}
	}
	var buf bytes.Buffer
	buf.WriteString(f.Package)
	buf.WriteString(":")
	buf.WriteString(name)
	return buf.String()
}

// Copy creates a copy of the current stack so that it can be attach to a
// runtime error.
func (s *CallStack) Copy() *CallStack {
	frames := make([]CallFrame, len(s.Frames))
	copy(frames, s.Frames)
	return &CallStack{frames}
}

// Top returns the CallFrame at the top of the stack or nil if none exists.
func (s *CallStack) Top() *CallFrame {
	if s == nil || len(s.Frames) == 0 {
		return nil
	}
	return &s.Frames[len(s.Frames)-1]
}

// TerminalFID determines if a chain of terminal stack frames that ends with
// fid (i.e. fid is a candidate for tail-recursion optimization) and returns
// the number of frames in the shortest such chain.  If no such chain of
// terminal frames can be found then 0 is returned.
//
// If a stack frame with TROBlock is found then the search for a terminal chain
// is prematurely terminated as a failure.
//
// NOTE:  If tail-recursion optimization is working then the chain of calls
// found by TerminalFID is unique.
func (s *CallStack) TerminalFID(fid string) int {
	top := s.Top()
	if top == nil {
		return 0
	}
	for i := len(s.Frames) - 1; i >= 0; i-- {
		if !s.Frames[i].Terminal {
			// A non-terminal frame before finding a terminal fid frame.. no
			return 0
		}
		if s.Frames[i].TROBlock {
			// This stack frame contains critical state which cannot be
			// collapsed even if it is a terminal call.  This is not an
			// expected state and signals a problem with the implementation of
			// a builtin or the primary evaluation routine.
			s.DebugPrint(os.Stderr)
			// It's unclear if a panic is the correct action here.  While the
			// existence of the TROBlock should prevent severe harm using the
			// standard language builtins if someone wrote
			// alternative/replacement builtins incorrectly their improper use
			// of teminal expressions may lead to incorrect evaluation, runtime
			// errors, or later panics.
			log.Panicf("tail-recursion-optimization is blocked -- inconsistent stack")
			return 0
		}
		if s.Frames[i].FID == fid {
			// We found the chain.  We want the returned length to be 1 when i
			// is on the first iteration.
			return len(s.Frames) - i
		}
	}
	return 0
}

// PushFID pushes a new stack frame with the given FID onto s.
func (s *CallStack) PushFID(fid, pkg, name string) {
	s.Frames = append(s.Frames, CallFrame{FID: fid, Package: pkg, Name: name})
}

// Pop removes the top CallFrame from the stack and returns it.  If the stack
// is empty Pop returns nil.
func (s *CallStack) Pop() CallFrame {
	if len(s.Frames) < 1 {
		panic("pop called on an empty stack")
	}
	f := s.Frames[len(s.Frames)-1]
	s.Frames[len(s.Frames)-1] = CallFrame{}
	s.Frames = s.Frames[:len(s.Frames)-1]
	return f
}

// DebugPrint prints s
func (s *CallStack) DebugPrint(w io.Writer) (int, error) {
	n, err := fmt.Fprintf(w, "Stack Trace [%d frames -- entrypoint last]:\n", len(s.Frames))
	if err != nil {
		return n, err
	}
	indent := "  "
	for i := len(s.Frames) - 1; i >= 0; i-- {
		f := s.Frames[i]
		var mod bytes.Buffer
		if f.Terminal {
			mod.WriteString(" [terminal]")
		}
		if f.TROBlock {
			mod.WriteString(" [tro-blocked]")
		}
		name := f.FID
		if f.Name != "" {
			name = f.Name
		}
		if f.Package != "" {
			name = f.Package + ":" + name
		}
		_n, err := fmt.Fprintf(w, "%sheight %d: %s%s\n", indent, i, name, mod.String())
		n += _n
		if err != nil {
			return n, err
		}
	}
	return n, nil
}
