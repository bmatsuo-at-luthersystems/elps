package libtesting

import (
	"fmt"

	"bitbucket.org/luthersystems/elps/lisp"
	"bitbucket.org/luthersystems/elps/lisp/lisplib/internal/libutil"
)

// DeafultPackageName is the package name used by LoadPackage.
const DefaultPackageName = "testing"

const DefaultSuiteSymbol = "test-suite"

// LoadPackage adds the time package to env
func LoadPackage(env *lisp.LEnv) *lisp.LVal {
	name := lisp.Symbol(DefaultPackageName)
	e := env.DefinePackage(name)
	if !e.IsNil() {
		return e
	}
	e = env.InPackage(name)
	if !e.IsNil() {
		return e
	}
	suite := NewTestSuite()
	env.PutGlobal(lisp.Symbol(DefaultSuiteSymbol), lisp.Native(suite))
	for _, fn := range suite.Ops() {
		env.AddSpecialOps(true, fn)
	}
	for _, fn := range suite.Macros() {
		env.AddMacros(true, fn)
	}
	return lisp.Nil()
}

var builtins = []*libutil.Builtin{}

// TestSuite is an ordered set of named tests.
type TestSuite struct {
	tests map[string]*Test
	order []string
}

func NewTestSuite() *TestSuite {
	return &TestSuite{
		tests: make(map[string]*Test),
	}
}

func (s *TestSuite) Add(t *Test) error {
	if s.tests[t.Name] != nil {
		return fmt.Errorf("test with the same name already defined: %v", t.Name)
	}
	s.order = append(s.order, t.Name)
	s.tests[t.Name] = t
	return nil
}

func (s *TestSuite) Len() int {
	return len(s.order)
}

func (s *TestSuite) Test(i int) *Test {
	return s.tests[s.order[i]]
}

func (s *TestSuite) Macros() []*libutil.Builtin {
	return []*libutil.Builtin{
		libutil.Function("test-let", lisp.Formals("name", "bindings", lisp.VarArgSymbol, "expers"), s.MacroTestLet),
		libutil.Function("test-let*", lisp.Formals("name", "bindings", lisp.VarArgSymbol, "expers"), s.MacroTestLetSeq),
	}
}

func (s *TestSuite) Ops() []*libutil.Builtin {
	return []*libutil.Builtin{
		libutil.Function("test", lisp.Formals("name", lisp.VarArgSymbol, "exprs"), s.OpTest),
	}
}

func (s *TestSuite) MacroTestLet(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return s.macroTestLet(env, args, "let")
}

func (s *TestSuite) MacroTestLetSeq(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return s.macroTestLet(env, args, "let*")
}

func (s *TestSuite) macroTestLet(env *lisp.LEnv, args *lisp.LVal, let string) *lisp.LVal {
	name, binds, exprs := args.Cells[0], args.Cells[1], args.Cells[2:]
	_, _, _ = name, binds, exprs
	if name.Type != lisp.LString {
		return env.Errorf("first argument is not a string: %v", name.Type)
	}
	if binds.Type != lisp.LSExpr {
		return env.Errorf("second argument is not a list: %v", binds.Type)
	}
	for _, v := range binds.Cells {
		if v.Type != lisp.LSExpr {
			return env.Errorf("second argument is not a list of pairs: %v", v.Type)
		}
		if v.Len() != 2 {
			return env.Errorf("second argument is not a list of pairs: length %d", v.Len())
		}
	}
	letCells := make([]*lisp.LVal, 0, 2+len(exprs))
	letCells = append(letCells, lisp.Symbol(env.Registry.Lang+":"+let), binds)
	letCells = append(letCells, exprs...)
	letExpr := lisp.SExpr(letCells)
	return lisp.SExpr([]*lisp.LVal{
		lisp.Symbol(DefaultPackageName + ":test"),
		name,
		letExpr,
	})
}

func (s *TestSuite) OpTest(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	name, exprs := args.Cells[0], args.Cells[1:]
	if name.Type != lisp.LString {
		return env.Errorf("first argument is not a string: %v", name.Type)
	}
	fun := env.Lambda(lisp.Nil(), exprs)
	test := &Test{
		Name: name.Str,
		Fun:  fun,
	}
	err := s.Add(test)
	if err != nil {
		return env.Error(err)
	}
	return lisp.Nil()
}

type Test struct {
	Name string
	Fun  *lisp.LVal
}

func EnvTestSuite(env *lisp.LEnv) *TestSuite {
	lsuite := env.Registry.Packages[DefaultPackageName].Get(lisp.Symbol(DefaultSuiteSymbol))
	if lsuite.Type != lisp.LNative {
		return nil
	}
	suite, _ := lsuite.Native.(*TestSuite)
	return suite
}