package environ

import (
	"testing"

	"github.com/luthersystems/elps/v2/pkg/lisp"
	"github.com/luthersystems/elps/v2/pkg/symbol"
	"github.com/stretchr/testify/assert"
)

func TestBindings(t *testing.T) {
	myvar := symbol.Intern("myvar")
	s := newBindings(0)
	assert.Equal(t, 0, s.Len())
	v, ok := s.Get(myvar)
	assert.False(t, ok)
	assert.True(t, lisp.IsNil(v))
	s.Put(myvar, lisp.Int(1))
	assert.Equal(t, 1, s.Len())
	v, ok = s.Get(myvar)
	assert.True(t, ok)
	x, ok := lisp.GetInt(v)
	if assert.True(t, ok) {
		assert.Equal(t, 1, x)
	}
	v = s.GetIndex(0)
	x, ok = lisp.GetInt(v)
	if assert.True(t, ok) {
		assert.Equal(t, 1, x)
	}
	s.PutIndex(0, lisp.Int(2))
	v, ok = s.Get(myvar)
	assert.True(t, ok)
	x, ok = lisp.GetInt(v)
	if assert.True(t, ok) {
		assert.Equal(t, 2, x)
	}
	s.Put(myvar, lisp.Int(3))
	v = s.GetIndex(0)
	x, ok = lisp.GetInt(v)
	if assert.True(t, ok) {
		assert.Equal(t, 3, x)
	}
}

func TestBindingsZipCons(t *testing.T) {
	vara := symbol.Intern("a")
	varb := symbol.Intern("b")
	varc := symbol.Intern("c")
	vars := lisp.List(nil,
		lisp.Symbol(vara),
		lisp.Symbol(varb),
		lisp.Symbol(varc),
	)
	_, err := NewBindingsZipCons(vars, lisp.Nil())
	assert.Error(t, err)
	_, err = NewBindingsZipCons(lisp.Nil(), lisp.List(nil,
		lisp.Int(1),
	))
	assert.Error(t, err)
	_, err = NewBindingsZipCons(vars, lisp.List(nil,
		lisp.Int(1),
		lisp.Int(2),
	))
	assert.Error(t, err)
	_, err = NewBindingsZipCons(vars, lisp.List(nil,
		lisp.Int(1),
		lisp.Int(2),
		lisp.Int(3),
		lisp.Int(4),
	))
	assert.Error(t, err)
	s, err := NewBindingsZipCons(vars, lisp.List(nil,
		lisp.Int(1),
		lisp.Int(2),
		lisp.Int(3),
	))
	assert.NoError(t, err)
	v, ok := s.Get(vara)
	if assert.True(t, ok) {
		x, ok := lisp.GetInt(v)
		if assert.True(t, ok) {
			assert.Equal(t, 1, x)
		}
	}
	v, ok = s.Get(varb)
	if assert.True(t, ok) {
		x, ok := lisp.GetInt(v)
		if assert.True(t, ok) {
			assert.Equal(t, 2, x)
		}
	}
	v, ok = s.Get(varc)
	if assert.True(t, ok) {
		x, ok := lisp.GetInt(v)
		if assert.True(t, ok) {
			assert.Equal(t, 3, x)
		}
	}
	if assert.Equal(t, 3, s.Len()) {
		o, ok := s.(OrderedBindings)
		assert.True(t, ok)
		v := o.GetIndex(1)
		x, ok := lisp.GetInt(v)
		if assert.True(t, ok) {
			assert.Equal(t, 2, x)
		}
	}
}
