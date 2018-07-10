package lisp_test

import (
	"errors"
	"testing"

	"bitbucket.org/luthersystems/elps/lisp"
	"bitbucket.org/luthersystems/elps/parser/rdparser"
	"github.com/stretchr/testify/assert"
)

// TODO:  Add a test for env.Errorf.

func TestGoError(t *testing.T) {
	testerr := errors.New("test error message")
	lerr := lisp.Error(testerr)
	msg := lisp.GoError(lerr).Error()
	assert.Equal(t, "<native code>: "+testerr.Error(), msg)

	lerr = lisp.Errorf("test error message")
	msg = lisp.GoError(lerr).Error()
	assert.Equal(t, "<native code>: test error message", msg)
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
	assert.Equal(t, "<native code>: test-error: test error message", msg)
}

func TestLoadErrors(t *testing.T) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = rdparser.NewReader()
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
	assert.Equal(t, "<native code>: lisp:load-string: load-string:1: unmatched-syntax: unmatched (", msg)

	testsrc = lisp.SExpr([]*lisp.LVal{
		lisp.Symbol("load-string"),
		lisp.String("(((foo bar) ()"),
		lisp.Symbol(":name"),
		lisp.String("test.lisp"),
	})
	lerr = env.Eval(testsrc)
	msg = lisp.GoError(lerr).Error()
	assert.Equal(t, "<native code>: lisp:load-string: test.lisp:1: unmatched-syntax: unmatched (", msg)

	testsrc = lisp.SExpr([]*lisp.LVal{
		lisp.Symbol("load-string"),
		lisp.String("([(foo bar) ()"),
		lisp.Symbol(":name"),
		lisp.String("test.lisp"),
	})
	lerr = env.Eval(testsrc)
	msg = lisp.GoError(lerr).Error()
	assert.Equal(t, "<native code>: lisp:load-string: test.lisp:1: unmatched-syntax: unmatched [", msg)

	testsrc = lisp.SExpr([]*lisp.LVal{
		lisp.Symbol("load-string"),
		lisp.String("())"),
		lisp.Symbol(":name"),
		lisp.String("test.lisp"),
	})
	lerr = env.Eval(testsrc)
	msg = lisp.GoError(lerr).Error()
	assert.Equal(t, "<native code>: lisp:load-string: test.lisp:1: unexpected-token: unexpected )", msg)

	testsrc = lisp.SExpr([]*lisp.LVal{
		lisp.Symbol("load-string"),
		lisp.String("(foo)) (defun bar () (baz))"),
		lisp.Symbol(":name"),
		lisp.String("test.lisp"),
	})
	lerr = env.Eval(testsrc)
	msg = lisp.GoError(lerr).Error()
	assert.Equal(t, "<native code>: lisp:load-string: test.lisp:1: unexpected-token: unexpected )", msg)

	testsrc = lisp.SExpr([]*lisp.LVal{
		lisp.Symbol("load-string"),
		lisp.String("((foo) ^(defun bar () (baz)))"),
		lisp.Symbol(":name"),
		lisp.String("test.lisp"),
	})
	lerr = env.Eval(testsrc)
	msg = lisp.GoError(lerr).Error()
	assert.Equal(t, msg, "<native code>: lisp:load-string: test.lisp:1: scan-error: unexpected text starting with '^'", msg)

	testsrc = lisp.SExpr([]*lisp.LVal{
		lisp.Symbol("load-string"),
		lisp.String(`(error 'test-error "test error message")`),
		lisp.Symbol(":name"),
		lisp.String("test.lisp"),
	})
	lerr = env.Eval(testsrc)
	msg = lisp.GoError(lerr).Error()
	assert.Equal(t, "test.lisp:1: test-error: test error message", msg)
}
