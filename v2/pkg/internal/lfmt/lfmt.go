package lfmt

import "io"

// WriteOp is a function that looks like w.Write but may involve many calls to
// w.Write and aggregate the result.
type WriteOp func(w io.Writer) (int, error)

// CountingWriter is an io.Writer that tracks the total number of bytes written
// across all calls to the Write method.
type CountingWriter interface {
	io.Writer
	// Returns the total number of bytes written.
	N() int
	// DeferCount passes the underlying io.Writer to the WriteOp and counts the
	// reported number of bytes using the WriteOp's return value.  DeferCount
	// is used as an optimization when one would otherwise pass a
	// CountingWriter into a function that may call the Write method many times
	// and would cause the counter to get updated many times instead of once.
	DeferCount(WriteOp) (int, error)
}

// NewCountingWriter wraps w as a CountingWriter.  If w implements
// io.StringWriter then the returned CountingWriter will also implement
// io.StringWriter.
func NewCountingWriter(w io.Writer) CountingWriter {
	sw, ok := w.(io.StringWriter)
	if ok {
		return &countingStringWriter{countingWriter{w: w}, sw}
	}
	return &countingWriter{w: w}
}

type counter struct {
	n int
}

func (c *counter) count(n int, err error) (int, error) {
	c.n += n
	return n, err
}

// N implements CountingWriter
func (c *counter) N() int {
	return c.n
}

// countingWriter is the default implementation of CountingWriter
type countingWriter struct {
	counter
	w io.Writer
}

var _ CountingWriter = (*countingWriter)(nil)

// Write implements io.Writer
func (w *countingWriter) Write(b []byte) (int, error) {
	return w.count(w.w.Write(b))
}

// DeferCount implements CountingWriter
func (w *countingWriter) DeferCount(fn WriteOp) (int, error) {
	return w.count(fn(w.w))
}

// countingStringWriter is an implementation of CountingWriter that implements
// io.StringWriter as well.
type countingStringWriter struct {
	countingWriter
	w io.StringWriter
}

var _ CountingWriter = (*countingStringWriter)(nil)
var _ io.StringWriter = (*countingStringWriter)(nil)

// Write implements io.Writer
func (w *countingStringWriter) Write(b []byte) (int, error) {
	return w.countingWriter.Write(b) // no need to call w.count -- its being counted
}

// WriteString implements io.StringWriter
func (w *countingStringWriter) WriteString(s string) (int, error) {
	return w.count(w.w.WriteString(s))
}
