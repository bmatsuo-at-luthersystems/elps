package lisp

import (
	"errors"
	"testing"

	"github.com/stretchr/testify/assert"
)

// TODO:  Add a test for env.Errorf.

func TestErrors(t *testing.T) {
	testerr := errors.New("test error message")
	lerr := Error(testerr)
	msg := GoError(lerr).Error()
	assert.Equal(t, testerr.Error(), msg)

	lerr = Errorf("test error message")
	msg = GoError(lerr).Error()
	assert.Equal(t, "test error message", msg)
}

func TestRuntimeErrors(t *testing.T) {
	env := NewEnv(nil)
	lerr := InitializeUserEnv(env)
	if GoError(lerr) != nil {
		t.Fatal(GoError(lerr))
	}
	var testsrc *LVal
	testsrc = SExpr([]*LVal{
		Symbol("error"),
		Quote(Symbol("test-error")),
		String("test error message"),
	})
	lerr = env.Eval(testsrc)
	msg := GoError(lerr).Error()
	assert.Equal(t, "test-error: test error message", msg)
}
