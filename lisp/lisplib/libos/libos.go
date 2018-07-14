package libos

import (
	"os"

	"bitbucket.org/luthersystems/elps/lisp"
	"bitbucket.org/luthersystems/elps/lisp/lisplib/internal/libutil"
)

// DeafultPackageName is the package name used by LoadPackage.
const DefaultPackageName = "os"

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
	libutil.Function("getenv", lisp.Formals("key"), BuiltinGetenv),
	libutil.Function("rename", lisp.Formals("source-path", "destination-path"), BuiltinMove),
	libutil.Function("remove", lisp.Formals("path", lisp.KeyArgSymbol, "recursive"), BuiltinRemove),
	libutil.Function("mkdir", lisp.Formals("path", lisp.KeyArgSymbol, "recursive", "mode"), BuiltinMkdir),
	libutil.Function("chdir", lisp.Formals("path"), BuiltinChdir),
	libutil.Function("work-dir", lisp.Formals(), BuiltinWorkDir),
	libutil.Function("exists?", lisp.Formals("path"), BuiltinExists),
	libutil.Function("dir?", lisp.Formals("path"), BuiltinIsDir),
}

func BuiltinGetenv(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	key := args.Cells[0]
	if key.Type != lisp.LString && key.Type != lisp.LSymbol {
		return env.Errorf("argument not a string or symbol: %v", key.Type)
	}
	val, ok := os.LookupEnv(key.Str)
	if !ok {
		return lisp.Nil()
	}
	return lisp.String(val)
}

func BuiltinMove(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	src, dst := args.Cells[0], args.Cells[1]
	if src.Type != lisp.LString {
		return env.Errorf("first argument is not a string: %v", src.Type)
	}
	if dst.Type != lisp.LString {
		return env.Errorf("first argument is not a string: %v", src.Type)
	}
	err := os.Rename(src.Str, dst.Str)
	if err != nil {
		return env.Error(err)
	}
	return lisp.Nil()
}

func BuiltinRemove(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	path, recursive := args.Cells[0], args.Cells[1]
	if path.Type != lisp.LString {
		return env.Errorf("first argument is not a string: %v", path.Type)
	}
	var err error
	if lisp.True(recursive) {
		err = os.RemoveAll(path.Str)
	} else {
		err = os.Remove(path.Str)
	}
	if err != nil {
		return env.Error(err)
	}
	return lisp.Nil()
}

func BuiltinMkdir(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	path, recursive, mode := args.Cells[0], args.Cells[1], args.Cells[2]
	if path.Type != lisp.LString {
		return env.Errorf("first argument is not a string: %v", path.Type)
	}
	osmode := os.FileMode(0755)
	if !mode.IsNil() && mode.Type != lisp.LInt {
		return env.Errorf("mode argument is not an integer: %v", mode.Type)
	}
	if mode.Type == lisp.LInt {
		osmode = os.FileMode(mode.Int)
	}
	var err error
	if lisp.True(recursive) {
		err = os.MkdirAll(path.Str, osmode)
	} else {
		err = os.Mkdir(path.Str, osmode)
	}
	if err != nil {
		return env.Error(err)
	}
	return lisp.Nil()
}

func BuiltinChdir(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	path := args.Cells[0]
	if path.Type != lisp.LString {
		return env.Errorf("argument is not a string: %v", path.Type)
	}
	err := os.Chdir(path.Str)
	if err != nil {
		return env.Error(err)
	}
	return lisp.Nil()
}

func BuiltinWorkDir(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	dir, err := os.Getwd()
	if err != nil {
		return env.Error(err)
	}
	return lisp.String(dir)
}

func BuiltinExists(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	path := args.Cells[0]
	if path.Type != lisp.LString {
		return env.Errorf("first argument is not a string: %v", path.Type)
	}
	_, err := os.Stat(path.Str)
	if os.IsNotExist(err) {
		return lisp.Bool(false)
	}
	if err != nil {
		return env.Error(err)
	}
	return lisp.Bool(true)
}

func BuiltinIsDir(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	path := args.Cells[0]
	if path.Type != lisp.LString {
		return env.Errorf("first argument is not a string: %v", path.Type)
	}
	stat, err := os.Stat(path.Str)
	if os.IsNotExist(err) {
		return lisp.Bool(false)
	}
	if err != nil {
		return env.Error(err)
	}
	return lisp.Bool(stat.IsDir())
}
