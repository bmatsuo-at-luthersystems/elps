package lisptest

import (
	"testing"

	"github.com/bmatsuo/elps/lisp"
	"github.com/bmatsuo/elps/parser"
)

func TestEval_simple(t *testing.T) {
	type testexpr []struct {
		expr   string
		result string
	}
	tests := []struct {
		testexpr
	}{
		{testexpr{
			{"3", "3"},
			// a single quote on a self-evaluating expression does not show up.
			{"'3", "3"},
			// a double quotes on a self-evaluating expression both show up.
			{"''3", "''3"},
		}},
		{testexpr{
			{"()", "()"},
			{"'t", "'t"},
			{"t", "t"},
			// A bit brittle, but it's ok for now. Replace with a more robust
			// test later if problematic.
			{"a", "unbound symbol: a"},
		}},
		{testexpr{
			{"'()", "'()"},
			{"'(1 2 3)", "'(1 2 3)"},
		}},
		{testexpr{
			// arithmetic functions w/o args
			{"(+)", "0"},
			{"(-)", "0"},
			{"(*)", "1"},
			{"(/)", "1"},
			// arithmetic functions w/ one arg
			{"(+ 2)", "2"},
			{"(+ 2.0)", "2"},
			{"(- 2)", "-2"},
			{"(- 2.0)", "-2"},
			{"(* 2)", "2"},
			{"(* 2.0)", "2"},
			{"(/ 2)", "0.5"},
			{"(/ 2.0)", "0.5"},
			// arithmetic functions w/ two args
			{"(* 2 0.75)", "1.5"},
			{"(+ 1 2 3)", "6"},
			{"(+ 1 (* 2 3))", "7"},
			{"(+ 1 1.5)", "2.5"},
			{"(- 0.5 1)", "-0.5"},
			{"(* 2 0.75)", "1.5"},
		}},
		{testexpr{
			{"(cons 1 (cons 2 (cons 3 ())))", "'(1 2 3)"},
			{"(list 1 2 3)", "'(1 2 3)"},
			{"(concat (list 1 2) (list 3))", "'(1 2 3)"},
		}},
		{testexpr{
			{"(cons 1 (cons 2 (cons 3 ())))", "'(1 2 3)"},
			{"(list 1 2 3)", "'(1 2 3)"},
			{"(concat (list 1 2) (list 3))", "'(1 2 3)"},
		}},
		{testexpr{
			{"((lambda () '(+ 1 1)))", "2"},
			{"((lambda '(n) '(+ n 1)) 1)", "2"},
			{"((lambda '(x y) '(+ x y)) 1 2)", "3"},
		}},
		{testexpr{
			// defun macro
			{"(defun fn0 () (+ 1 1))", "()"},
			{"(defun fn1 (n) (+ n 1))", "()"},
			{"(defun fn2 (x y) (+ x y))", "()"},
			{"(fn0)", "2"},
			{"(fn1 1)", "2"},
			{"(fn2 1 2)", "3"},
		}},
		{testexpr{
			// if operator
			{"(if () 1 2)", "2"},
			{"(if t 1 2)", "1"},
			{"(if t (set 'x 1) (set 'x 2))", "1"},
			{"x", "1"},
			{"(if () (set 'x 1) (set 'x 2))", "2"},
			{"x", "2"},
			{"(if '(()) 1 2)", "1"},
			{`(if "false" 1 2)`, "1"},
		}},
		{testexpr{
			// simple lexical scoping tests
			{"(let ((x 1)) x)", "1"},
			{"x", "unbound symbol: x"},
			{"(set 'x 1)", "1"},
			{"(let ((x 2)) x)", "2"},
			{"x", "1"},
			{"(let ((x 3)) (defun fn (y) (+ x y)))", "()"},
			{"(let ((x 2)) (fn 2))", "5"},
			{"(((lambda '(x) '(lambda () '(+ x 2))) 3))", "5"},
		}},
		{testexpr{
			{"(defmacro m0 () (quasiquote (+ 1 1)))", "()"},
			{"(defmacro m1 (x) (quasiquote (+ (unquote x) 1)))", "()"},
			{"(defmacro m2 (x y) (quasiquote (+ (unquote x) (unquote y))))", "()"},
			{"(m0)", "2"},
			{"(m1 1)", "2"},
			{"(m2 1 2)", "3"},
		}},
		{testexpr{
			{"(defmacro m1 (x) (quasiquote (let ((y (+ (unquote x) 1))) (+ y y))))", "()"},
			{"(set 'z 1)", "1"},
			{"(m1 z)", "4"},
		}},
		{testexpr{
			{"(defmacro m1 (x) (quasiquote (let ((y (+ (unquote x) 1))) (+ y y))))", "()"},
			{"(set 'z 1)", "1"},
			{"(m1 z)", "4"},
		}},
	}
	for i, test := range tests {
		env := lisp.NewEnv(nil)
		env.AddBuiltins()
		env.AddMacros()
		for j, expr := range test.testexpr {
			v, _, err := parser.ParseLVal([]byte(expr.expr))
			if err != nil {
				t.Errorf("test %d: expr %d: parse error: %v", i, j, err)
				continue
			}
			if len(v) == 0 {
				t.Errorf("test %d: expr %d: no expression parsed", i, j)
				continue
			}
			if len(v) != 1 {
				t.Errorf("test %d: expr %d: more than one expression parsed (%d)", i, j, len(v))
				continue
			}
			result := env.Eval(v[0]).String()
			if result != expr.result {
				t.Errorf("test %d: expr %d: expected result %s (got %s )", i, j, expr.result, result)
			}
		}
	}
}
