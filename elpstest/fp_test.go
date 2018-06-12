package elpstest

import "testing"

func TestFP(t *testing.T) {
	tests := TestSuite{
		{"map-reduce", TestSequence{
			{"(map (lambda (x) (+ x x)) '(1 2 3))", "'(2 4 6)"},
			{"(defun flip (fn x y) (fn y x))", "()"},
			{"(foldl (flip cons) () '(1 2 3))", "'(3 2 1)"},
			{"(foldr cons () '(1 2 3))", "'(1 2 3)"},
		}},
		{"unpack", TestSequence{
			{"(defun f (a b) (+ a b))", "()"},
			{"(unpack f '(1 2))", "3"},
			{"(unpack (f 1) '(2))", "3"},
			{"(unpack cons '(1 '(2)))", "'(1 2)"},
			{"(unpack (cons 1) '('(2)))", "'(1 2)"},
		}},
		{"flip", TestSequence{
			{"((flip +) 1 2)", "3"},
			{"((flip <) 1 2)", "()"},
			{"(((flip cons) '(2 3)) 1)", "'(1 2 3)"},
		}},
		{"zip", TestSequence{
			{"(zip '(1 2 3) '('a 'b 'c))", "'('(1 'a) '(2 'b) '(3 'c))"},
			{"(unpack zip (zip '(1 2 3) '('a 'b 'c)))", "'('(1 2 3) '('a 'b 'c))"},
		}},
		{"simple composition", TestSequence{
			{"(defun f (x) (+ x 1))", "()"},
			{"(defun g (x) (* x 2))", "()"},
			{"(compose f g)", "(lambda (x) ((lambda (x) (+ x 1)) ((lambda (x) (* x 2)) x)))"},
			{"((compose f g) 2)", "5"},
		}},
		{"complex composition", TestSequence{
			{"(defun g (x & xs) (cons x xs))", "()"},
			{"(compose reverse g)", "(lambda (x & xs) (<builtin> (lisp:unpack (lambda (x & xs) (cons x xs)) (lisp:concat '(x) xs))))"},
			{"((compose reverse list) 1 2 3)", "'(3 2 1)"},
			{"((compose reverse g) 1 2 3)", "'(3 2 1)"},
		}},
	}
	RunTestSuite(t, tests)
}
