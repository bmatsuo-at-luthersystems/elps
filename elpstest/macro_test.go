package elpstest

import "testing"

func TestMacros(t *testing.T) {
	tests := TestSuite{
		{"test-trace", TestSequence{
			{"(set 'x 0)", "0", ""},
			{"(defun fun () (set 'x (+ x 1)))", "()", ""},
			{"(trace (fun))", "1", "\"TRACE\" 1\n"},
			{`(trace (fun) "TRACE2")`, "2", "\"TRACE2\" 2\n"},
		}},
		{"quasiquote", TestSequence{
			{"(quasiquote (reverse 'list '(1 2 3)))", "'(reverse 'list '(1 2 3))", ""},
			{"(quasiquote (unquote (reverse 'list '(1 2 3))))", "'(3 2 1)", ""},
			{"(quasiquote (unquote '(reverse 'list '(1 2 3))))", "'(reverse 'list '(1 2 3))", ""},
			{"(quasiquote (1 2 (unquote-splicing '(3 4)) 5))", "'(1 2 3 4 5)", ""},
			{"(let ((xs '(2 1))) (quasiquote (concat 'list '(1 2) (unquote xs))))", "'(concat 'list '(1 2) '(2 1))", ""},
			{"(quasiquote (unquote test-symbol))", "test:1: lisp:quasiquote: unbound symbol: test-symbol", ""},
			{"(quasiquote (list (unquote-splicing test-symbol)))", "test:1: lisp:quasiquote: unbound symbol: test-symbol", ""},
			{"(quasiquote (unquote-splicing '(+ 2 3)))", "test:1: lisp:quasiquote: unquote-splicing used in an invalid context", ""},
		}},
		{"defmacro", TestSequence{
			{"(defmacro m0 () (quasiquote (+ 1 1)))", "()", ""},
			{"(defmacro m1 (x) (quasiquote (+ (unquote x) 1)))", "()", ""},
			{"(defmacro m2 (x y) (quasiquote (+ (unquote x) (unquote y))))", "()", ""},
			{"(m0)", "2", ""},
			{"(m1 1)", "2", ""},
			{"(m2 1 2)", "3", ""},
			{`(macroexpand '(m0))`, `'(+ 1 1)`, ""},
			{`(macroexpand '(m1 (* 2 3)))`, `'(+ (* 2 3) 1)`, ""},
			{`(macroexpand '(m2 (* 2 3) (m0)))`, `'(+ (* 2 3) (m0))`, ""},
			{`(macroexpand-1 '(m0))`, `'(+ 1 1)`, ""},
			{`(macroexpand-1 '(m1 (* 2 3)))`, `'(+ (* 2 3) 1)`, ""},
			{`(macroexpand-1 '(m2 (* 2 3) (m0)))`, `'(+ (* 2 3) (m0))`, ""},
		}},
		{"macroexpand", TestSequence{
			{`(defmacro // (&rest x) (quasiquote (or (unquote-splicing x))))`, `()`, ""},
			{`(defmacro m (&rest x) (quasiquote (// (unquote (car x)) (m (unquote-splicing (cdr x))))))`, `()`, ""},
			{`(macroexpand '(m 1 2 3))`, `'(or 1 (m 2 3))`, ""},
			{`(macroexpand-1 '(m 1 2 3))`, `'(// 1 (m 2 3))`, ""},
		}},
		{"defmacro advanced 1", TestSequence{
			{"(defmacro m1 (x) (quasiquote (let ((y (+ (unquote x) 1))) (+ y y))))", "()", ""},
			{"(set 'z 1)", "1", ""},
			{"(m1 z)", "4", ""},
		}},
		{"defmacro advanced 2", TestSequence{
			{"(defmacro m1 (x) (quasiquote (let ((y (+ (unquote x) 1))) (+ y y))))", "()", ""},
			{"(set 'z 1)", "1", ""},
			{"(m1 z)", "4", ""},
		}},
		{"defmacro repeated expansion", TestSequence{
			{`(defmacro mlen (&rest xs) (if (nil? xs) 0 (quasiquote (+ 1 (mlen (unquote-splicing (cdr xs)))))))`, `()`, ``},
			{`(macroexpand '(mlen))`, `0`, ``},
			{`(mlen)`, `0`, ``},
			{`(macroexpand '(mlen a))`, `'(+ 1 (mlen))`, ``},
			{`(mlen a)`, `1`, ``},
			{`(macroexpand '(mlen a b))`, `'(+ 1 (mlen b))`, ``},
			{`(mlen a b)`, `2`, ``},
		}},
		{"defmacro optional args", TestSequence{
			{"(defmacro mrest (&rest xs) (quasiquote (- (unquote-splicing xs))))", "()", ""},
			{"(defmacro mopt (&optional x) (let ([sym (gensym)]) (quasiquote (let ([(unquote sym) (unquote x)]) (if (unquote sym) (mrest (unquote sym)) (mrest 0))))))", "()", ""},
			{"(defmacro mopt1 (&optional x) (let ([x (if (nil? x) 0 x)]) (quasiquote (if (unquote x) (mrest (unquote x)) (mrest 0)))))", "()", ""},
			{"(mopt 1)", "-1", ""},
			{"(mopt)", "0", ""},
			{"(mopt (+ 1 1))", "-2", ""},
			{"(mrest (mopt (+ 1 1)))", "2", ""},
			{"(mopt1 1)", "-1", ""},
			{"(mopt1)", "0", ""},
			{"(mopt1 (+ 1 1))", "-2", ""},
			{"(mrest (mopt1 (+ 1 1)))", "2", ""},
		}},
		{"my-defun", TestSequence{
			{"(defmacro my-defun (name formals &rest exprs) (quasiquote (defun (unquote name) (unquote formals) (unquote-splicing exprs))))", "()", ""},
			{"(my-defun test-fun (x y) (debug-print x) (debug-print y) (+ x y))", "()", ""},
			{"(test-fun 1 2)", "3", "1\n2\n"},
			{"test-fun", "(lambda (x y) (debug-print x) (debug-print y) (+ x y))", ""},
		}},
	}
	RunTestSuite(t, tests)
}
