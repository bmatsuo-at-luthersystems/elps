package elpstest

import (
	"testing"
)

func TestEval(t *testing.T) {
	tests := TestSuite{
		{"quotes", TestSequence{
			{"3", "3"},
			// a single quote on a self-evaluating expression does not show up.
			{"'3", "3"},
			// a double quotes on a self-evaluating expression both show up.
			{"''3", "''3"},
		}},
		{"symbols", TestSequence{
			{"()", "()"},
			{"'t", "'t"},
			{"t", "t"},
			// A bit brittle, but it's ok for now. Replace with a more robust
			// test later if problematic.
			{"a", "unbound symbol: a"},
		}},
		{"lists basics", TestSequence{
			{"'()", "'()"},
			{"'(1 2 3)", "'(1 2 3)"},
		}},
		{"function basics", TestSequence{
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
			{"(concat (list 1 2) (list 3))", "'(1 2 3)"},
			{"(cons 1 (cons 2 (cons 3 ())))", "'(1 2 3)"},
			{"(list 1 2 3)", "'(1 2 3)"},
			{"(reverse (list 1 2 3))", "'(3 2 1)"},
			{"(reverse (list 1 2))", "'(2 1)"},
			{"(concat (list 1 2) (list 3))", "'(1 2 3)"},
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
	}
	RunTestSuite(t, tests)
}
