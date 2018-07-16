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
		{"let*", elpstest.TestSequence{
			{`(let* ())`, "()", ""},
			{`(let* ((x 1)) x)`, "1", ""},
			{`(let* ([x 1]) x)`, "1", ""},
			{`(let* ([x 1] [y 2]) (+ x y))`, "3", ""},
			{`(let* ([x 0]) (let* ([x 1] [y (+ x 1)]) (+ x y)))`, "3", ""},
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