package stack

import (
	"bytes"
	"fmt"
	"io"

	"github.com/luthersystems/elps/v2/pkg/lisp"
	"github.com/luthersystems/elps/v2/pkg/symbol"
)

var errStackEmpty = fmt.Errorf("read on an empty stack")

// Stack is a stack of LVals
type Stack struct {
	// Values contains stack values.  Index 0 is the bottom of the stack.
	Values   []lisp.LVal
	MaxDepth int
	NumPush  int
}

// New initializes and returns a new stack
func New() *Stack {
	return &Stack{}
}

func (s *Stack) Len() int {
	return len(s.Values)
}

// Push places v at the top of the stack.
func (s *Stack) Push(v lisp.LVal) {
	s.Values = append(s.Values, v)
	if len(s.Values) > s.MaxDepth {
		s.MaxDepth = len(s.Values)
	}
	s.NumPush += 1
}

// Peek returns the value at the top of the stack.
// Peek does not modify the stack.
func (s *Stack) Peek() (lisp.LVal, error) {
	if len(s.Values) == 0 {
		return lisp.Nil(), errStackEmpty
	}
	return s.Values[len(s.Values)-1], nil
}

// Pop removes the value at the top of the stack and returns it.
func (s *Stack) Pop() (lisp.LVal, error) {
	if len(s.Values) == 0 {
		return lisp.Nil(), errStackEmpty
	}
	v := s.Values[len(s.Values)-1]
	s.Values = s.Values[:len(s.Values)-1]
	return v, nil
}

// FormatStatistics writes statics about the stack to w
func (s *Stack) FormatStatistics(w io.Writer) (int, error) {
	return fmt.Fprintf(w,
		"MaxDepth  = %d -- Depth = %d -- NumPushes = %d",
		s.MaxDepth, len(s.Values), s.NumPush)
}

// Initialize purges any existing stack values and leaves s as an empty stack.
func (s *Stack) Initialize() {
	s.Values = nil
	s.MaxDepth = 0
	s.NumPush = 0
}

// LVal returns a lisp value containing s.
func (s *Stack) LVal(source lisp.Source, typ symbol.ID) lisp.LVal {
	return lisp.TagNative(typ, s)
}

// GetStack retrieves a Stack from v.  GetStack returns false if v does not
// contain a stack.
func GetStack(v lisp.LVal) (*Stack, bool) {
	if v.Type() != lisp.LTaggedVal {
		return nil, false
	}
	s, ok := v.Native.(*Stack)
	return s, ok
}

// Initialize calls the Initialize method on the stack contained in s.
func Initialize(s lisp.LVal) error {
	stack, ok := GetStack(s)
	if !ok {
		return fmt.Errorf("first argument is not a stack: %v", lisp.GetType(s))
	}
	stack.Initialize()
	return nil
}

// Push calls the Push method on the stack contained in s.
func Push(s lisp.LVal, v lisp.LVal) error {
	stack, ok := GetStack(s)
	if !ok {
		return fmt.Errorf("first argument is not a stack: %v", lisp.GetType(s))
	}
	stack.Push(v)
	return nil
}

// Pop calls the Pop method on the stack contained in s.
func Pop(s lisp.LVal) (lisp.LVal, error) {
	stack, ok := GetStack(s)
	if !ok {
		return lisp.Nil(), fmt.Errorf("first argument is not a stack: %v", lisp.GetType(s))
	}
	return stack.Pop()
}

// FormatStatistics calls the FormatStatistics method on the stack contained in s.
func FormatStatistics(s lisp.LVal) (string, error) {
	stack, ok := GetStack(s)
	if !ok {
		return "", fmt.Errorf("first argument is not a stack: %v", lisp.GetType(s))
	}
	buf := &bytes.Buffer{}
	_, err := stack.FormatStatistics(buf)
	if err != nil {
		// really don't expect this to happen ever
		return "", fmt.Errorf("formatting error: %v", err)
	}
	return buf.String(), nil
}
