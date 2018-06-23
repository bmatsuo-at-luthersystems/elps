package elpstest

import "testing"

func TestFP(t *testing.T) {
	tests := TestSuite{
		{"curry", TestSequence{
			{"(defun f (a b) (+ a b))", "()", ""},
			{"((curry-function 'f 1) 2)", "3", ""},
			// BUG:  This error message has to be tested with defun and can't
			// be tested in an earlier test (say eval_test.go) because the
			// function id used in the error messages of anonymous functions
			// is not deterministic.
			{"(f 1 2 3)", "f: invalid number of arguments: 3", ""},
		}},
		{"map-reduce", TestSequence{
			{"(map 'list (lambda (x) (+ x x)) '(1 2 3))", "'(2 4 6)", ""},
			{"(map 'vector (lambda (x) (+ x x)) '(1 2 3))", "(vector 2 4 6)", ""},
			{"(map 'list (lambda (x) (+ x x)) (vector 1 2 3))", "'(2 4 6)", ""},
			{"(map 'vector (lambda (x) (+ x x)) (vector 1 2 3))", "(vector 2 4 6)", ""},
			{"(defun flip (fn x y) (fn y x))", "()", ""},
			{"(foldl (flip cons) () '(1 2 3))", "'(3 2 1)", ""},
			{"(foldr cons () '(1 2 3))", "'(1 2 3)", ""},
		}},
		{"unpack", TestSequence{
			{"(defun f (a b) (+ a b))", "()", ""},
			{"(unpack f '(1 2))", "3", ""},
			{"(unpack (f 1) '(2))", "3", ""},
			{"(unpack cons '(1 '(2)))", "'(1 2)", ""},
			{"(unpack (cons 1) '('(2)))", "'(1 2)", ""},
		}},
		{"flip", TestSequence{
			{"((flip +) 1 2)", "3", ""},
			{"((flip <) 1 2)", "false", ""},
			{"(((flip cons) '(2 3)) 1)", "'(1 2 3)", ""},
		}},
		{"zip", TestSequence{
			{"(zip 'list '(1 2 3) '('a 'b 'c))", "'('(1 'a) '(2 'b) '(3 'c))", ""},
			{"(unpack (zip 'list) (zip 'list '(1 2 3) '('a 'b 'c)))", "'('(1 2 3) '('a 'b 'c))", ""},
			{"(zip 'vector '(1 2 3) '('a 'b 'c))", "(vector (vector 1 'a) (vector 2 'b) (vector 3 'c))", ""},
			{"(unpack (zip 'vector) (map 'list identity (zip 'vector '(1 2 3) '('a 'b 'c))))", "(vector (vector 1 2 3) (vector 'a 'b 'c))", ""},
		}},
		{"simple composition", TestSequence{
			{"(defun f (y) (+ y 1))", "()", ""},
			{"(defun g (x) (* x 2))", "()", ""},
			{"(compose f g)", "(lambda (x) (lisp:funcall (lambda (y) (+ y 1)) (lisp:apply (lambda (x) (* x 2)) x ())))", ""},
			{"((compose f g) 2)", "5", ""},
		}},
		{"complex composition", TestSequence{
			{"(defun g (x &rest xs) (cons x xs))", "()", ""},
			{"(compose (reverse 'list) g)", "(lambda (x &rest xs) (lisp:funcall <builtin> (lisp:apply (lambda (x &rest xs) (cons x xs)) x xs)))", ""},
			{"((compose (reverse 'list) list) 1 2 3)", "'(3 2 1)", ""},
			{"((compose (reverse 'list) g) 1 2 3)", "'(3 2 1)", ""},
		}},
	}
	RunTestSuite(t, tests)
}
