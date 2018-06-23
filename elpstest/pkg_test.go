package elpstest

import "testing"

func TestPackages(t *testing.T) {
	tests := TestSuite{
		{"basic namespace evaluation", TestSequence{
			// The lisp package contains all default builtins
			{"(lisp:+ 2 3)", "5", ""},
			{"(+ 2 3)", "5", ""},
			{"(lisp:list 1 2 3)", "'(1 2 3)", ""},
			{"(list 1 2 3)", "'(1 2 3)", ""},
		}},
		{"in-package", TestSequence{
			// Switch into a new package and define a function
			{"(in-package 'new-package)", "()", ""},
			{"(defun fun (x) (+ x 1))", "()", ""},
			{"(in-package 'other-package)", "()", ""},
			{"(defun fun (x) (- x 1))", "()", ""},
			{"(in-package 'user)", "()", ""},
			{`(new-package:fun 2)`, `3`, ""},
			{`(other-package:fun 2)`, `1`, ""},
		}},
		{"use-package", TestSequence{
			// Define fun in a new-package
			{"(in-package 'new-package)", "()", ""},
			{"(defun fun (x) (internal x))", "()", ""},
			{"(defun internal (x) (+ x 1))", "()", ""},
			{"(export 'fun)", "()", ""},
			// Also define fun in other-package
			{"(in-package 'other-package)", "()", ""},
			{"(defun fun (x) (internal x))", "()", ""},
			{"(defun internal (x) (- x 1))", "()", ""},
			{"(export 'fun)", "()", ""},
			// Use new-package and check the result of an unqualified fun call.
			{"(in-package 'user)", "()", ""},
			{"(use-package 'new-package)", "()", ""},
			{`(fun 2)`, `3`, ""},
			{`(other-package:fun 2)`, `1`, ""},
		}},
	}
	RunTestSuite(t, tests)
}
