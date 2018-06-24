package lisp_test

import (
	"errors"
	"testing"

	"bitbucket.org/luthersystems/elps/lisp"
	"bitbucket.org/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
)

// TODO:  Add a test for env.Errorf.

func TestErrors(t *testing.T) {
	testerr := errors.New("test error message")
	lerr := lisp.Error(testerr)
	msg := lisp.GoError(lerr).Error()
	assert.Equal(t, testerr.Error(), msg)

	lerr = lisp.Errorf("test error message")
	msg = lisp.GoError(lerr).Error()
	assert.Equal(t, "test error message", msg)
}

func TestRuntimeErrors(t *testing.T) {
	env := lisp.NewEnv(nil)
	lerr := lisp.InitializeUserEnv(env)
	if lisp.GoError(lerr) != nil {
		t.Fatal(lisp.GoError(lerr))
	}
	var testsrc *lisp.LVal
	testsrc = lisp.SExpr([]*lisp.LVal{
		lisp.Symbol("error"),
		lisp.Quote(lisp.Symbol("test-error")),
		lisp.String("test error message"),
	})
	lerr = env.Eval(testsrc)
	msg := lisp.GoError(lerr).Error()
	assert.Equal(t, "test-error: test error message", msg)
}

func TestLoadErrors(t *testing.T) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	lerr := lisp.InitializeUserEnv(env)
	if lisp.GoError(lerr) != nil {
		t.Fatal(lisp.GoError(lerr))
	}

	var testsrc *lisp.LVal
	testsrc = lisp.SExpr([]*lisp.LVal{
		lisp.Symbol("load-string"),
		lisp.String("("),
	})
	lerr = env.Eval(testsrc)
	msg := lisp.GoError(lerr).Error()
	assert.Equal(t, "lisp:load-string: unmatched \"(\" starting: (", msg)

	testsrc = lisp.SExpr([]*lisp.LVal{
		lisp.Symbol("load-string"),
		lisp.String("(((foo bar) ()"),
	})
	lerr = env.Eval(testsrc)
	msg = lisp.GoError(lerr).Error()
	assert.Equal(t, "lisp:load-string: unmatched \"(\" starting: ((foo bar)...", msg)

	testsrc = lisp.SExpr([]*lisp.LVal{
		lisp.Symbol("load-string"),
		lisp.String("([(foo bar) ()"),
	})
	lerr = env.Eval(testsrc)
	msg = lisp.GoError(lerr).Error()
	assert.Equal(t, "lisp:load-string: unmatched \"[\" starting: [(foo bar)...", msg)

	testsrc = lisp.SExpr([]*lisp.LVal{
		lisp.Symbol("load-string"),
		lisp.String(`(error 'test-error "test error message")`),
	})
	lerr = env.Eval(testsrc)
	msg = lisp.GoError(lerr).Error()
	assert.Equal(t, "test-error: test error message", msg)
}
