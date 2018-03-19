package lisp

// ErrorVal implements the error interface so that errors can be first class lisp
// objects.  The error message is stored in the Str field while contextual
// information (e.g. call stack) can be stored in the Cells slice.
type ErrorVal LVal

// Error implements the error interface.
func (e *ErrorVal) Error() string {
	return e.Str
}
