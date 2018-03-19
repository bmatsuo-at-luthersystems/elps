package elpstest

import "testing"

func TestScope(t *testing.T) {
	tests := TestSuite{
		{"lexical scope", TestSequence{
			// simple lexical scoping tests
			{"(let ((x 1)) x)", "1"},
			{"x", "unbound symbol: x"},
			{"(set 'x 1)", "1"},
			{"(let ((x 2)) x)", "2"},
			{"x", "1"},
			{"(let ((x 3)) (defun fn (y) (+ x y)))", "()"},
			{"(let ((x 2)) (fn 2))", "5"},
			{"(((lambda (x) (lambda () (+ x 2))) 3))", "5"},
		}},
		{"docs", TestSequence{
			// simple lexical scoping tests
			{"(let ((x 1) (y 2)) (defun add-y (x) (+ x y)))", "()"},
			{"(add-y 3)", "5"},
		}},
	}
	RunTestSuite(t, tests)
}
