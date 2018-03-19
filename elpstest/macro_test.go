package elpstest

import "testing"

func TestMacros(t *testing.T) {
	tests := TestSuite{
		{"quasiquote", TestSequence{
			{"(quasiquote (reverse '(1 2 3)))", "'(reverse '(1 2 3))"},
			{"(quasiquote (unquote (reverse '(1 2 3))))", "'(3 2 1)"},
			{"(quasiquote (unquote '(reverse '(1 2 3))))", "'(reverse '(1 2 3))"},
			{"(quasiquote (1 2 (unquote-splicing '(3 4)) 5))", "'(1 2 3 4 5)"},
			{"(let ((xs '(2 1))) (quasiquote (concat '(1 2) (unquote xs))))", "'(concat '(1 2) '(2 1))"},
		}},
		{"defmacro", TestSequence{
			{"(defmacro m0 () (quasiquote (+ 1 1)))", "()"},
			{"(defmacro m1 (x) (quasiquote (+ (unquote x) 1)))", "()"},
			{"(defmacro m2 (x y) (quasiquote (+ (unquote x) (unquote y))))", "()"},
			{"(m0)", "2"},
			{"(m1 1)", "2"},
			{"(m2 1 2)", "3"},
		}},
		{"defmacro advanced 1", TestSequence{
			{"(defmacro m1 (x) (quasiquote (let ((y (+ (unquote x) 1))) (+ y y))))", "()"},
			{"(set 'z 1)", "1"},
			{"(m1 z)", "4"},
		}},
		{"defmacro advanced 2", TestSequence{
			{"(defmacro m1 (x) (quasiquote (let ((y (+ (unquote x) 1))) (+ y y))))", "()"},
			{"(set 'z 1)", "1"},
			{"(m1 z)", "4"},
		}},
		//{"defmacro advanced 3", TestSequence{
		//	{`(defmacro ~>> (x & chain)
		//		(let ((c chain))
		//			(if c
		//				(let ((head ((eval (car c)) x)))
		//					(cons ~>> (cons head (cdr c))))
		//				x)))`, "()"},
		//	{`(defun addn (n) (lambda (y) (+ y n)))`, "()"},
		//	{"(~>> 0 (addn 1) (addn 1) (addn 1))", "3"},
		//}},
	}
	RunTestSuite(t, tests)
}
