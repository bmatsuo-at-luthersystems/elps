package elpstest

import "testing"

func TestPackages(t *testing.T) {
	tests := TestSuite{
		{"basic namespace evaluation", TestSequence{
			// The lisp package contains all default builtins
			{"(lisp:+ 2 3)", "5"},
			{"(+ 2 3)", "5"},
			{"(lisp:list 1 2 3)", "'(1 2 3)"},
			{"(list 1 2 3)", "'(1 2 3)"},
		}},
		{"in-package", TestSequence{
			// Switch into a new package and define a function
			{"(in-package 'new-package)", "()"},
			{"(lisp:defun ++ (x) (lisp:+ 1 x))", "()"},
			{`(++ 2)`, `3`},
		}},
		{"shadowing", TestSequence{
			// Switch into a new package and define a function
			{"(in-package 'new-package)", "()"},
			{"(defun + (x y) (list x y))", "()"},
			{`(+ 1 2)`, `'(1 2)`},
		}},
	}
	RunTestSuite(t, tests)
}
