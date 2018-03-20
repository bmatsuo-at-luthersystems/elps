package lisp

import (
	"bytes"
	"fmt"
	"io"
)

// CallStack is a function call stack.
type CallStack struct {
	Frames []CallFrame
}

// CallFrame is one frame in the CallStack
type CallFrame struct {
	FID      string
	Name     string
	Terminal bool
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
	if len(s.Frames) == 0 {
		return nil
	}
	return &s.Frames[len(s.Frames)-1]
}

// TerminalFID determines if a chain of terminal stack frames ends with fid
// (i.e. fid is a candidate for tail-recursion optimization) and returns the
// number of frames in the shortest such chain.  If no such chain of terminal
// frames can be found then 0 is returned.
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
		if s.Frames[i].FID == fid {
			// We found the chain.  We want the returned length to be 1 when i
			// is on the first iteration.
			return len(s.Frames) - i
		}
	}
	return 0
}

// PushFID pushes a new stack frame with the given FID onto s.
func (s *CallStack) PushFID(fid string, name string) {
	s.Frames = append(s.Frames, CallFrame{FID: fid, Name: name})
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
		name := f.FID
		if f.Name != "" {
			name = f.Name
		}
		_n, err := fmt.Fprintf(w, "%sheight %d: %s%s\n", indent, i, name, mod.String())
		n += _n
		if err != nil {
			return n, err
		}
	}
	return n, nil
}
