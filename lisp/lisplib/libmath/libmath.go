package libmath

import (
	"math"

	"bitbucket.org/luthersystems/elps/lisp"
	"bitbucket.org/luthersystems/elps/lisp/lisplib/internal/libutil"
)

// DeafultPackageName is the package name used by LoadPackage.
const DefaultPackageName = "math"

// LoadPackage adds the math package to env
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
	env.PutGlobal(lisp.Symbol("inf"), lisp.Float(math.Inf(1)))
	env.PutGlobal(lisp.Symbol("-inf"), lisp.Float(math.Inf(-1)))
	for _, fn := range builtins {
		env.AddBuiltins(true, fn)
	}
	return lisp.Nil()
}

var builtins = []*libutil.Builtin{
	libutil.Function("ceil", lisp.Formals("number"), builtinCeil),
	libutil.Function("floor", lisp.Formals("number"), builtinFloor),
	libutil.Function("sqrt", lisp.Formals("number"), builtinSqrt),
	libutil.Function("exp", lisp.Formals("number"), builtinExp),
	libutil.Function("ln", lisp.Formals("number"), builtinLn),
	libutil.Function("log", lisp.Formals("base", "number"), builtinLog),
}

func builtinCeil(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	x := args.Cells[0]
	if !x.IsNumeric() {
		return env.Errorf("argument is not a number: %v", x.Type)
	}
	if x.Type == lisp.LInt {
		return x
	}
	return lisp.Float(math.Ceil(x.Float))
}

func builtinFloor(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	x := args.Cells[0]
	if !x.IsNumeric() {
		return env.Errorf("argument is not a number: %v", x.Type)
	}
	if x.Type == lisp.LInt {
		return x
	}
	return lisp.Float(math.Floor(x.Float))
}

func builtinSqrt(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	x := args.Cells[0]
	if !x.IsNumeric() {
		return env.Errorf("argument is not a number: %v", x.Type)
	}
	return lisp.Float(math.Sqrt(toFloat(x)))
}

func builtinExp(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	x := args.Cells[0]
	if !x.IsNumeric() {
		return env.Errorf("argument is not a number: %v", x.Type)
	}
	return lisp.Float(math.Exp(toFloat(x)))
}

func builtinLn(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	x := args.Cells[0]
	if !x.IsNumeric() {
		return env.Errorf("argument is not a number: %v", x.Type)
	}
	return lisp.Float(math.Log(toFloat(x)))
}

func builtinLog(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	b, x := args.Cells[0], args.Cells[1]
	if !b.IsNumeric() {
		return env.Errorf("argument is not a number: %v", b.Type)
	}
	if !x.IsNumeric() {
		return env.Errorf("argument is not a number: %v", x.Type)
	}
	return lisp.Float(math.Log(toFloat(x)) / math.Log(toFloat(b)))
}

func toFloat(x *lisp.LVal) float64 {
	if x.Type == lisp.LFloat {
		return x.Float
	}
	return float64(x.Int)
}
