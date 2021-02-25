package environ

import (
	"testing"

	"github.com/luthersystems/elps/v2/pkg/lisp"
	"github.com/luthersystems/elps/v2/pkg/symbol"
	"github.com/stretchr/testify/assert"
)

func AssertIntEqual(t *testing.T, expect int, v lisp.LVal) {
	t.Helper()
	x, ok := lisp.GetInt(v)
	if assert.True(t, ok) {
		assert.Equal(t, expect, x)
	}
}

func AssertNoLispError(t *testing.T, v lisp.LVal) {
	t.Helper()
	assert.NotEqual(t, lisp.LError, v.Type())
}

func AssertLispError(t *testing.T, v lisp.LVal) {
	t.Helper()
	assert.Equal(t, lisp.LError, v.Type())
}

func RequireNoLispError(t *testing.T, v lisp.LVal) {
	t.Helper()
	assert.NotEqual(t, lisp.LError, v.Type())
}

func TestRoot(t *testing.T) {
	vara := symbol.Intern("a")
	varb := symbol.Intern("b")
	env := New(nil, nil)
	assert.Equal(t, 0, env.Len())
	env.Put(vara, lisp.Int(1))
	_, ok := env.Get(varb)
	assert.False(t, ok)
	v, ok := env.Get(vara)
	assert.True(t, ok)
	x, ok := lisp.GetInt(v)
	if assert.True(t, ok) {
		assert.Equal(t, 1, x)
	}
}

func TestChild(t *testing.T) {
	vara := symbol.Intern("a")
	varb := symbol.Intern("b")
	root := New(nil, nil)
	assert.Equal(t, 0, root.Len())
	root.Put(vara, lisp.Int(1))
	root.Put(varb, lisp.Int(2))
	env := New(root, nil)
	assert.Equal(t, 0, env.Len())
	env.Put(varb, lisp.Int(3))
	v, ok := env.Get(vara)
	if assert.True(t, ok) {
		AssertIntEqual(t, 1, v)
	}
	v, ok = env.Get(varb)
	if assert.True(t, ok) {
		AssertIntEqual(t, 3, v)
	}
	v, ok = root.Get(varb)
	if assert.True(t, ok) {
		AssertIntEqual(t, 2, v)
	}
}

func TestAddress(t *testing.T) {
	vara := symbol.Intern("a")
	varb := symbol.Intern("b")
	varc := symbol.Intern("c")
	root := New(nil, nil)
	root.Put(vara, lisp.Int(1))
	root.Put(varb, lisp.Int(2))
	env := New(root, nil)
	env.Put(varc, lisp.Int(3))
	v, err := env.GetAddress(0, 0)
	if assert.NoError(t, err) {
		AssertIntEqual(t, 3, v)
	}
	v, err = env.GetAddress(1, 0)
	if assert.NoError(t, err) {
		AssertIntEqual(t, 1, v)
	}
	v, err = env.GetAddress(1, 1)
	if assert.NoError(t, err) {
		AssertIntEqual(t, 2, v)
	}
	assert.NoError(t, env.PutAddress(1, 0, lisp.Int(4)))
	assert.NoError(t, env.PutAddress(1, 1, lisp.Int(5)))
	assert.NoError(t, env.PutAddress(0, 0, lisp.Int(6)))
	v, ok := env.Get(vara)
	if assert.True(t, ok) {
		AssertIntEqual(t, 4, v)
	}
	v, ok = env.Get(varb)
	if assert.True(t, ok) {
		AssertIntEqual(t, 5, v)
	}
	v, ok = env.Get(varc)
	if assert.True(t, ok) {
		AssertIntEqual(t, 6, v)
	}
	_, err = env.GetAddress(0, 10)
	assert.Error(t, err)
	_, err = env.GetAddress(2, 0)
	assert.Error(t, err)
	assert.Error(t, env.PutAddress(0, 10, lisp.Int(10)))
	assert.Error(t, env.PutAddress(2, 0, lisp.Int(10)))
}
