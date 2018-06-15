package elpstest

import (
	"testing"
)

func TestEval(t *testing.T) {
	tests := TestSuite{
		{"raw strings", TestSequence{
			{`"""a raw string"""`, `"a raw string"`},
			{`"""a raw
string"""`, `"a raw\nstring"`},
			{`"""""a raw
string"""`, `"\"\"a raw\nstring"`},
		}},
		{"quotes", TestSequence{
			{"3", "3"},
			// a single quote on a self-evaluating expression does not show up.
			{"'3", "3"},
			// a double quotes on a self-evaluating expression both show up.
			{"''3", "''3"},
		}},
		{"symbols", TestSequence{
			{"()", "()"},
			{"'true", "'true"},
			{"true", "true"},
			{"'false", "'false"},
			{"false", "false"},
			// A bit brittle, but it's ok for now. Replace with a more robust
			// test later if problematic.
			{"a", "unbound symbol: a"},
		}},
		{"lists basics", TestSequence{
			{"'()", "'()"},
			{"'(1 2 3)", "'(1 2 3)"},
		}},
		{"function basics", TestSequence{
			{"(lambda (x) x)", "(lambda (x) x)"},
			{"((lambda (x) x) 1)", "1"},
			{"(lambda (x) (+ x 1))", "(lambda (x) (+ x 1))"},
			{"((lambda () (+ 1 1)))", "2"},
			{"((lambda (n) (+ n 1)) 1)", "2"},
			{"((lambda (x y) (+ x y)) 1 2)", "3"},
		}},
		{"partial evaluation", TestSequence{
			{"((lambda (x y) (+ x y)) 1)", "(lambda (y (x 1)) (+ x y))"},
			{"(((lambda (x y) (+ x y)) 1) 2)", "3"},
		}},
		{"lists", TestSequence{
			{"(cons 1 (cons 2 (cons 3 ())))", "'(1 2 3)"},
			{"(list 1 2 3)", "'(1 2 3)"},
			{"(concat 'list (list 1 2) (list 3))", "'(1 2 3)"},
			{"(cons 1 (cons 2 (cons 3 ())))", "'(1 2 3)"},
			{"(list 1 2 3)", "'(1 2 3)"},
			{"(reverse 'list (list 1 2 3))", "'(3 2 1)"},
			{"(reverse 'list (list 1 2))", "'(2 1)"},
			{"(reverse 'vector (list 1 2 3))", "(vector 3 2 1)"},
			{"(reverse 'vector (list 1 2))", "(vector 2 1)"},
			{"(reverse 'list (vector 1 2 3))", "'(3 2 1)"},
			{"(reverse 'list (vector 1 2))", "'(2 1)"},
			{"(reverse 'vector (vector 1 2 3))", "(vector 3 2 1)"},
			{"(reverse 'vector (vector 1 2))", "(vector 2 1)"},
			{"(concat 'list (list 1 2) (list 3))", "'(1 2 3)"},
			{"(slice 'list '(0 1 2 3 4) 1 3)", "'(1 2)"},
		}},
		{"make-sequence", TestSequence{
			{"(make-sequence 0 5)", "'(0 1 2 3 4)"},
			{"(make-sequence 0 5 2)", "'(0 2 4)"},
		}},
		{"filtering", TestSequence{
			{"(select 'list (expr (< % 3)) '())", "'()"},
			{"(select 'list (expr (< % 3)) '(0 1 2 3 4 5))", "'(0 1 2)"},
			{"(select 'list (expr (< % 3)) '(3 4 5 6))", "'()"},
			{"(reject 'list (expr (< % 3)) '())", "'()"},
			{"(reject 'list (expr (< % 3)) '(0 1 2 3 4 5))", "'(3 4 5)"},
			{"(reject 'list (expr (< % 3)) '(0 1 1 -1 2 2))", "'()"},
			{"(select 'list (expr (< % 3)) (vector))", "'()"},
			{"(select 'list (expr (< % 3)) (vector 0 1 2 3 4 5))", "'(0 1 2)"},
			{"(select 'list (expr (< % 3)) (vector 3 4 5 6))", "'()"},
			{"(reject 'list (expr (< % 3)) (vector ))", "'()"},
			{"(reject 'list (expr (< % 3)) (vector 0 1 2 3 4 5))", "'(3 4 5)"},
			{"(reject 'list (expr (< % 3)) (vector 0 1 1 -1 2 2))", "'()"},
			{"(select 'vector (expr (< % 3)) '())", "(vector)"},
			{"(select 'vector (expr (< % 3)) '(0 1 2 3 4 5))", "(vector 0 1 2)"},
			{"(select 'vector (expr (< % 3)) '(3 4 5 6))", "(vector)"},
			{"(reject 'vector (expr (< % 3)) '())", "(vector)"},
			{"(reject 'vector (expr (< % 3)) '(0 1 2 3 4 5))", "(vector 3 4 5)"},
			{"(reject 'vector (expr (< % 3)) '(0 1 1 -1 2 2))", "(vector)"},
		}},
		{"defun", TestSequence{
			// defun macro
			{"(defun fn0 () (+ 1 1))", "()"},
			{"(defun fn1 (n) (+ n 1))", "()"},
			{"(defun fn2 (x y) (+ x y))", "()"},
			{"(fn0)", "2"},
			{"(fn1 1)", "2"},
			{"(fn2 1 2)", "3"},
		}},
		{"errors", TestSequence{
			{`(list 1 2 (error 'test-error "test message") 4)`, "test message"},
		}},
	}
	RunTestSuite(t, tests)
}
