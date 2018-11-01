package lisp_test

import (
	"testing"

	"bitbucket.org/luthersystems/elps/elpstest"
)

func TestSpecialOp(t *testing.T) {
	debugstack := `Stack Trace [4 frames -- entrypoint last]:
  height 3: test:4: lisp:debug-stack
  height 2: test:4: lisp:progn
  height 1: test:2: lisp:let
  height 0: test:1: lisp:let [terminal]
`
	tests := elpstest.TestSuite{
		{"if", elpstest.TestSequence{
			// if operator
			{"(if () 1 2)", "2", ""},
			{"(if true 1 2)", "1", ""},
			{"(if true (set 'x 1) (set 'x 2))", "1", ""},
			{"x", "1", ""},
			{"(if () (set 'x 1) (set 'x 2))", "2", ""},
			{"x", "2", ""},
			{"(if '(()) 1 2)", "1", ""},
			{`(if "false" 1 2)`, "1", ""},
		}},
		{"let", elpstest.TestSequence{
			{`(let ())`, "()", ""},
			{`(let ((x 1)) x)`, "1", ""},
			{`(let ([x 1]) x)`, "1", ""},
			{`(let ([x 1] [y 2]) (+ x y))`, "3", ""},
			{`(let ([x 0])
				(let (
						[x 1]
						[y (+ (progn (debug-stack) x) 1)])
					(+ x y)))`, "2", debugstack},
		}},
		{"define", elpstest.TestSequence{
			{`(define x 1)`, `()`, ``},
			{`x`, `1`, ``},
			{`(define x 2)`, `()`, ``},
			{`x`, `2`, ``},
			{`(define (f x) (+ x x))`, `()`, ``},
			{`(f 1)`, `2`, ``},
			{`(define (f)
				(define x 5)
				(debug-print x))`, `()`, ``},
			{`(f)`, `()`, "5\n"},
			{`x`, `2`, ``},
			{`(define (f &rest xs) (apply 'list 1 xs))`, `()`, ``},
			{`(f 2 3 4)`, `'(1 2 3 4)`, ``},
			{`(define false 0)`, `test:1: lisp:define: cannot rebind constant: false`, ``},
			{`(define () (debug-print 2))`, `test:1: lisp:define: invalid definition form: ()`, ``},
		}},
		{"set!", elpstest.TestSequence{
			{`(set 'x 1)`, `1`, ``},
			{`(set! x 2)`, `()`, ``},
			{`x`, `2`, ``},
			{`(let ([x 'a]) (set! x 'b) x)`, `'b`, ``},
			{`x`, `2`, ``},
			{`(set! false x)`, `test:1: lisp:set!: cannot rebind constant: false`, ``},
			{`(set! foo 3)`, `test:1: lisp:set!: symbol not bound: foo`, ``},
		}},
		{"let*", elpstest.TestSequence{
			{`(let* ())`, "()", ""},
			{`(let* ((x 1)) x)`, "1", ""},
			{`(let* ([x 1]) x)`, "1", ""},
			{`(let* ([x 1] [y 2]) (+ x y))`, "3", ""},
			{`(let* ([x 0]) (let* ([x 1] [y (+ x 1)]) (+ x y)))`, "3", ""},
			{`(let ((bar 0)) (let* ((foo (lambda () bar)) (bar 1)) (foo)))`, `0`, ``},
			{`(let* ((f (lambda (x)
						(if (= 0 x) 0 (f (- x 1))))))
				(f 10))`, `test:2: lisp:if: unbound symbol: f`, ``},
		}},
		{"flet", elpstest.TestSequence{
			{`(flet [])`, "()", ""},
			{`(flet ([f (x) x]) (f 2))`, "2", ""},
			{`(flet ([f (x y) (if (= x 0) y (f (- x 1) (+ y 1)))]) (f 3 2))`, "5", ""},
			{`(defun orig () 1)`, "()", ""},
			{`(flet ([orig () 2] [f () (orig)]) (f))`, "1", ""},
		}},
		{"labels", elpstest.TestSequence{
			{`(labels [])`, "()", ""},
			{`(labels ([f (x) x]) (f 2))`, "2", ""},
			{`(labels ([f (x y) (if (= x 0) y (f (- x 1) (+ y 1)))]) (f 3 2))`, "5", ""},
			{`(defun orig () 1)`, "()", ""},
			{`(labels ([orig () 2] [f () (orig)]) (f))`, "2", ""},
			{`(labels ([f () (orig)] [orig () 2]) (f))`, "2", ""},
		}},
		{"cond", elpstest.TestSequence{
			{`(cond)`, "()", ""},
			{`(cond (else 1))`, "1", ""},
			{`(cond (:else 1))`, "1", ""},
			{`(cond (true 2) (else 1))`, "2", ""},
			{`(cond (true) (else 1))`, "()", ""},
			{`(cond (true 1 (+ 1 2)) (else 1))`, "3", ""},
			{`(cond ((< 1 2) 3) (else 1))`, "3", ""},
		}},
		{"expr", elpstest.TestSequence{
			{`((expr ()))`, "()", ""},
			{`((expr "hello"))`, `"hello"`, ""},
			{`((expr %) 123)`, "123", ""},
			{`((expr %2) 'a 'b)`, "'b", ""},
			{`((expr (reverse 'list %&rest)) 1 2 3)`, "'(3 2 1)", ""},
			{`((expr %&optional))`, "()", ""},
			{`((expr %&optional) 1)`, "1", ""},
			{`((expr (cons 1 %&optional)))`, "'(1)", ""},
			{`((expr (cons 1 %&optional)) '(2))`, "'(1 2)", ""},
		}},
		{"#^", elpstest.TestSequence{
			{`(#^())`, "()", ""},
			{`(#^"hello")`, `"hello"`, ""},
			{`(#^% 123)`, "123", ""},
			{`(#^%2 'a 'b)`, "'b", ""},
			{`(#^(reverse 'list %&rest) 1 2 3)`, "'(3 2 1)", ""},
			{`(#^%&optional)`, "()", ""},
			{`(#^%&optional 1)`, "1", ""},
			{`(#^(cons 1 %&optional))`, "'(1)", ""},
			{`(#^(cons 1 %&optional) '(2))`, "'(1 2)", ""},
			{`(#^'(cons 1 %))`, "'(cons 1 %)", ""},
			{`(#^(list 1 '%))`, "'(1 '%)", ""},
		}},
		{"threading", elpstest.TestSequence{
			{`(thread-last 1 (+ 2) (< 2))`, `true`, ""},
			{`(thread-last 1 (+ 2) (> 2))`, `false`, ""},
			{`(thread-first 1 (+   2) (<   2))`, `false`, ""},
			{`(thread-first 1 (+   2) (>   2))`, `true`, ""},
			{`(map 'list (lambda (x) (thread-last x (+ 2) (< 2))) '(1 -1))`, `'(true false)`, ""},
			{`(map 'list (lambda (x) (thread-first x (+ 2) (< 2))) '(1 -1))`, `'(false true)`, ""},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}
