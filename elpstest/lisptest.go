package elpstest

import (
	"testing"

	"bitbucket.org/luthersystems/elps/lisp"
	"bitbucket.org/luthersystems/elps/parser"
)

// TestSequence is a sequence of lisp expressions which are evaluated sequentially
// by a lisp.LEnv.
type TestSequence []struct {
	Expr   string // a lisp expression
	Result string // the evaluated result
}

// TestSuite is a set of named TestSequences
type TestSuite []struct {
	Name string
	TestSequence
}

// RunTestSuite runs each TestSequence in tests on isolated lisp.LEnvs.
func RunTestSuite(t *testing.T, tests TestSuite) {
	for i, test := range tests {
		env := lisp.NewEnv(nil)
		lisp.InitializeUserEnv(env)
		env.Reader = parser.NewReader()
		for j, expr := range test.TestSequence {
			//log.Printf("test %d %q: expr %d evaluating", i, test.Name, j)
			v, _, err := parser.ParseLVal([]byte(expr.Expr))
			if err != nil {
				t.Errorf("test %d %q: expr %d: parse error: %v", i, test.Name, j, err)
				continue
			}
			if len(v) == 0 {
				t.Errorf("test %d %q: expr %d: no expression parsed", i, test.Name, j)
				continue
			}
			if len(v) != 1 {
				t.Errorf("test %d %q: expr %d: more than one expression parsed (%d)", i, test.Name, j, len(v))
				continue
			}
			result := env.Eval(v[0]).String()
			if result != expr.Result {
				t.Errorf("test %d %q: expr %d: expected result %s (got %s)", i, test.Name, j, expr.Result, result)
			}
		}
	}
}
