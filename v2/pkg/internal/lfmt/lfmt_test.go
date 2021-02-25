package lfmt

import (
	"fmt"
	"io"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestCounter(t *testing.T) {
	c := &counter{}
	n, err := c.count(0, nil)
	assert.Equal(t, 0, n)
	assert.Equal(t, 0, c.N())
	assert.NoError(t, err)
	n, err = c.count(1, nil)
	assert.Equal(t, 1, n)
	assert.Equal(t, 1, c.N())
	assert.NoError(t, err)
	n, err = c.count(2, nil)
	assert.Equal(t, 2, n)
	assert.Equal(t, 3, c.N())
	assert.NoError(t, err)
	n, err = c.count(3, fmt.Errorf("an error occurred"))
	assert.Equal(t, 3, n)
	assert.Equal(t, 6, c.N())
	assert.Error(t, err)
}

type mockWriter struct {
	counter
	w func([]byte) (int, error)
}

var _ io.Writer = (*mockWriter)(nil)

func (w *mockWriter) Write(b []byte) (int, error) {
	if w.w == nil {
		w.counter.n += len(b)
		return len(b), nil
	}
	return w.count(w.w(b))
}

type mockStringWriter struct {
	mockWriter
	ws func(string) (int, error)
}

var _ io.Writer = (*mockStringWriter)(nil)
var _ io.StringWriter = (*mockStringWriter)(nil)

func (w *mockStringWriter) WriteString(s string) (int, error) {
	if w.ws == nil {
		w.counter.n += len(s)
		return len(s), nil
	}
	return w.count(w.ws(s))
}

func TestCountingWriter(t *testing.T) {
	mock := mockWriter{}
	w := NewCountingWriter(&mock)
	_, ok := w.(io.StringWriter)
	assert.False(t, ok)
	n, err := w.Write([]byte("hello"))
	assert.Equal(t, 5, n)
	assert.Equal(t, 5, w.N())
	assert.Equal(t, 5, mock.N())
	assert.NoError(t, err)
	n, err = w.Write([]byte("yo"))
	assert.Equal(t, 2, n)
	assert.Equal(t, 7, w.N())
	assert.Equal(t, 7, mock.N())
	assert.NoError(t, err)
	mock.w = func(b []byte) (int, error) { return len(b) / 2, nil }
	n, err = w.Write([]byte("hi"))
	assert.Equal(t, 1, n)
	assert.Equal(t, 8, w.N())
	assert.Equal(t, 8, mock.N())
	assert.NoError(t, err)
	mock.w = func(b []byte) (int, error) { return len(b) / 2, fmt.Errorf("bad write") }
	n, err = w.Write([]byte("ohno"))
	assert.Equal(t, 2, n)
	assert.Equal(t, 10, w.N())
	assert.Equal(t, 10, mock.N())
	assert.Error(t, err)
	n, err = w.DeferCount(func(w io.Writer) (int, error) {
		assert.NotNil(t, w)
		return 10, fmt.Errorf("failed")
	})
	assert.Equal(t, 10, n)
	assert.Equal(t, 20, w.N())
	assert.Equal(t, 10, mock.N()) // DeferCount allows the WriteOp to lie and that's OK
	assert.Error(t, err)
}

func TestCountingStringWriter(t *testing.T) {
	mock := mockStringWriter{}
	w := NewCountingWriter(&mock)
	ws, ok := w.(io.StringWriter)
	assert.True(t, ok)
	n, err := w.Write([]byte("hello"))
	assert.Equal(t, 5, n)
	assert.Equal(t, 5, w.N())
	assert.Equal(t, 5, mock.N())
	assert.NoError(t, err)
	n, err = ws.WriteString("yo")
	assert.Equal(t, 2, n)
	assert.Equal(t, 7, w.N())
	assert.Equal(t, 7, mock.N())
	assert.NoError(t, err)
	mock.ws = func(s string) (int, error) { return len(s) / 2, nil }
	n, err = ws.WriteString("hi")
	assert.Equal(t, 1, n)
	assert.Equal(t, 8, w.N())
	assert.Equal(t, 8, mock.N())
	assert.NoError(t, err)
	mock.ws = func(s string) (int, error) { return len(s) / 2, fmt.Errorf("bad write") }
	n, err = ws.WriteString("ohno")
	assert.Equal(t, 2, n)
	assert.Equal(t, 10, w.N())
	assert.Equal(t, 10, mock.N())
	assert.Error(t, err)
	n, err = w.DeferCount(func(w io.Writer) (int, error) {
		assert.NotNil(t, w)
		return 10, fmt.Errorf("failed")
	})
	assert.Equal(t, 10, n)
	assert.Equal(t, 20, w.N())
	assert.Equal(t, 10, mock.N()) // DeferCount allows the WriteOp to lie and that's OK
	assert.Error(t, err)
}
