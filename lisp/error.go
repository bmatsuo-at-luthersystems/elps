package lisp

import (
	"bufio"
	"fmt"
	"io"
)

// ErrorVal implements the error interface so that errors can be first class lisp
// objects.  The error message is stored in the Str field while contextual
// information (e.g. call stack) can be stored in the Cells slice.
type ErrorVal LVal

// Error implements the error interface.
func (e *ErrorVal) Error() string {
	return e.Str
}

// FullError returns a message that details where the function was created.
func (e *ErrorVal) FullError() string {
	if e.Stack == nil {
		return e.Str
	}
	return fmt.Sprintf("%s: %s", e.Stack.Top().Name, e.Str)
}

// WriteTrace writes the error and a stack trace to w
func (e *ErrorVal) WriteTrace(w io.Writer) (int, error) {
	bw := bufio.NewWriter(w)
	var n int
	var err error
	wrote := func(_n int, _err error) bool {
		n += _n
		err = _err
		return err == nil
	}
	if !wrote(bw.WriteString(e.FullError())) {
		return n, err
	}
	if !wrote(bw.WriteString("\n")) {
		return n, err
	}
	if e.Stack != nil {
		if !wrote(e.Stack.DebugPrint(bw)) {
			return n, err
		}
	}
	return n, bw.Flush()
}
