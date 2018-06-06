package elpstest

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"path/filepath"
	"testing"

	"bitbucket.org/luthersystems/elps/lisp"
	"bitbucket.org/luthersystems/elps/lisp/lisplib"
	"bitbucket.org/luthersystems/elps/lisp/lisplib/libtesting"
	"bitbucket.org/luthersystems/elps/parser"
)

// Runner is a test runner.
type Runner struct {
	// Loader is the package loader used to initialize the test environment.
	// When Loader is nil lisplib.LoadLibrary is used.
	Loader func(*lisp.LEnv) *lisp.LVal
}

func (r *Runner) NewEnv() (*lisp.LEnv, error) {
	env := lisp.NewEnv(nil)
	lerr := lisp.InitializeUserEnv(env)
	if lerr.Type == lisp.LError {
		return nil, fmt.Errorf("Failed to initialize lisp environment: %v", lerr)
	}
	env.InPackage(lisp.String(lisp.DefaultUserPackage))
	env.Reader = parser.NewReader()
	loader := r.Loader
	if loader == nil {
		loader = lisplib.LoadLibrary
	}
	lerr = loader(env)
	if lerr.Type == lisp.LError {
		return nil, fmt.Errorf("Failed to load package library: %v", lerr)
	}
	lerr = env.InPackage(lisp.String(lisp.DefaultUserPackage))
	if lerr.Type == lisp.LError {
		return nil, fmt.Errorf("Failed to switch into user package: %v", lerr)
	}

	return env, nil
}

func (r *Runner) RunTestFile(t *testing.T, path string) {
	source, err := ioutil.ReadFile(path)
	if err != nil {
		t.Errorf("Unable to read test file: %v", err)
		return
	}

	var names []string
	ok := t.Run("$load", func(t *testing.T) {
		env, err := r.NewEnv()
		if err != nil {
			t.Error(err.Error())
			return
		}

		lerr := env.Load(filepath.Base(path), bytes.NewReader(source))
		if lerr.Type == lisp.LError {
			t.Error(lerr.String())
			if lerr.Stack != nil {
				var buf bytes.Buffer
				lerr.Stack.DebugPrint(&buf)
				t.Error(buf.String())
			}
		}
		suite := libtesting.EnvTestSuite(env)
		if suite == nil {
			t.Errorf("unable to locate test suite")
			return
		}
		names = make([]string, suite.Len())
		for i := range names {
			names[i] = suite.Test(i).Name
		}
	})
	if !ok {
		return
	}

	for i := range names {
		// We don't check the result of t.Run here because we want all
		// independent tests to run during a single run of the suite.  An
		// assertion failure within a tests prevents futher evaluation of
		// expressions in that test, but does not halt the execution of the
		// suite as a whole.
		t.Run(names[i], func(t *testing.T) {
			env, err := r.NewEnv()
			if err != nil {
				t.Error(err.Error())
				return
			}

			lerr := env.Load(filepath.Base(path), bytes.NewReader(source))
			if lerr.Type == lisp.LError {
				t.Error(lerr.String())
				if lerr.Stack != nil {
					var buf bytes.Buffer
					lerr.Stack.DebugPrint(&buf)
					t.Error(buf.String())
					return
				}
			}
			suite := libtesting.EnvTestSuite(env)
			if suite == nil {
				t.Errorf("unable to locate test suite")
				return
			}
			ltest := suite.Test(i)
			lerr = env.Eval(lisp.SExpr([]*lisp.LVal{ltest.Fun}))
			if lerr.Type == lisp.LError {
				t.Errorf("%s: %v", ltest.Name, lerr.String())
				return
			}
		})
	}
}

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
		env.InPackage(lisp.String(lisp.DefaultUserPackage))
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
