package libosio

import (
	"io/ioutil"
	"os"

	"bitbucket.org/luthersystems/elps/lisp"
	"bitbucket.org/luthersystems/elps/lisp/lisplib/internal/libutil"
)

// DeafultPackageName is the package name used by LoadPackage.
const DefaultPackageName = "os/io"

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
	for _, fn := range builtins {
		env.AddBuiltins(true, fn)
	}
	return lisp.Nil()
}

var builtins = []*libutil.Builtin{
	libutil.Function("read-file", lisp.Formals("path"), BuiltinReadFile),
	libutil.Function("write-file", lisp.Formals("path", "data", lisp.KeyArgSymbol, "mode"), BuiltinWriteFile),
}

func BuiltinReadFile(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	path := args.Cells[0]
	if path.Type != lisp.LString {
		return env.Errorf("argument is not a string: %v", path.Type)
	}
	b, err := ioutil.ReadFile(path.Str)
	if err != nil {
		return env.Error(err)
	}
	return lisp.Bytes(b)
}

func BuiltinWriteFile(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	path, data, mode := args.Cells[0], args.Cells[1], args.Cells[2]
	if path.Type != lisp.LString && path.Type != lisp.LBytes {
		return env.Errorf("argument is not a string or bytes: %v", path.Type)
	}
	if !mode.IsNil() && mode.Type != lisp.LInt {
		return env.Errorf("mode argument is not an int: %v", mode.Type)
	}
	perm := os.FileMode(0644)
	if mode.Type == lisp.LInt {
		perm = os.FileMode(mode.Int)
	}
	var b []byte
	if data.Type == lisp.LBytes {
		b = data.Bytes()
	} else {
		// TODO:  rewrite this so that we don't allocate a copy of the whole
		// string
		b = []byte(data.Str)
	}
	err := ioutil.WriteFile(path.Str, b, perm)
	if err != nil {
		return env.Error(err)
	}
	return lisp.Nil()
}
