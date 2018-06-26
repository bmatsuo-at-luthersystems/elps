package elpstest

import "testing"

func TestStack(t *testing.T) {
	tests := TestSuite{
		{"basic tail recursion optimization", TestSequence{
			// One of the most trivial tail recursive functions.
			{`(defun tr1 (x) (if (< 0 x) (tr1 (- x 1)) (debug-stack)))`, `()`, ``},
			{`(let ([x 10]) (tr1 x))`, `()`, `Stack Trace [4 frames -- entrypoint last]:
  height 3: lisp:debug-stack
  height 2: lisp:if [terminal]
  height 1: user:tr1 [terminal]
  height 0: lisp:let [terminal]
`},
			// The funcall builtin is capable of triggering tail recursion optimization.
			{`(defun tr2 (x) (if (< 0 x) (funcall tr2 (- x 1)) (debug-stack)))`, `()`, ``},
			{`(let ([x 10]) (tr2 x))`, `()`, `Stack Trace [4 frames -- entrypoint last]:
  height 3: lisp:debug-stack
  height 2: lisp:if [terminal]
  height 1: user:tr2 [terminal]
  height 0: lisp:let [terminal]
`},
			// Even if there is a let that binds lexical variables before the
			// tail-recursive call tail recursion can succeed because argument
			// values are evaluated before the stack is collapsed.
			{`(defun tr3 (x) (if (< 0 x) (let ([y (- x 1)]) (tr3 y)) (debug-stack)))`, `()`, ``},
			{`(let ([x 10]) (tr3 x))`, `()`, `Stack Trace [4 frames -- entrypoint last]:
  height 3: lisp:debug-stack
  height 2: lisp:if [terminal]
  height 1: user:tr3 [terminal]
  height 0: lisp:let [terminal]
`},
			// No tail recursion optimization in this final test.  The
			// non-terminal (TROBlock) handler-bind frame prevents tail
			// recursion
			{`(defun tr4 (x) (if (< 0 x) (handler-bind ([condition (lambda (&rest x))])  (tr4 (- x 1))) (debug-stack)))`, `()`, ``},
			{`(let ([x 1]) (tr4 x))`, `()`, `Stack Trace [7 frames -- entrypoint last]:
  height 6: lisp:debug-stack
  height 5: lisp:if [terminal]
  height 4: user:tr4 [terminal]
  height 3: lisp:handler-bind [tro-blocked]
  height 2: lisp:if [terminal]
  height 1: user:tr4 [terminal]
  height 0: lisp:let [terminal]
`},
		}},
	}
	RunTestSuite(t, tests)
}