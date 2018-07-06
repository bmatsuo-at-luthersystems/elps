package libregexp

import (
	"regexp"

	"bitbucket.org/luthersystems/elps/lisp"
	"bitbucket.org/luthersystems/elps/lisp/lisplib/internal/libutil"
)

// DeafultPackageName is the package name used by LoadPackage.
const DefaultPackageName = "regexp"

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
	libutil.Function("regexp-compile", lisp.Formals("pattern"), BuiltinCompile),
	libutil.Function("regexp-pattern", lisp.Formals("re"), BuiltinPattern),
	libutil.Function("regexp-match?", lisp.Formals("re", "text"), BuiltinIsMatch),
}

func BuiltinCompile(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	patt := args.Cells[0]
	if patt.Type != lisp.LString {
		return env.Errorf("argument is not a string: %v", patt.Type)
	}
	re, err := regexp.Compile(patt.Str)
	if err != nil {
		return env.ErrorCondition("invalid-regexp-pattern", err)
	}
	return lisp.Native(re)
}

func BuiltinPattern(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	lre := args.Cells[0]
	re, lerr := getRegexp(env, lre)
	if lerr != nil {
		return lerr
	}
	return lisp.String(re.String())
}

func BuiltinIsMatch(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	lre, text := args.Cells[0], args.Cells[1]
	re, lerr := getRegexp(env, lre)
	if lerr != nil {
		return lerr
	}
	switch text.Type {
	case lisp.LString:
		return lisp.Bool(re.MatchString(text.Str))
	case lisp.LBytes:
		return lisp.Bool(re.Match(text.Bytes))
	default:
		return env.Errorf("argument is not a string or bytes: %v", text.Type)
	}
}

func getRegexp(env *lisp.LEnv, v *lisp.LVal) (re *regexp.Regexp, lerr *lisp.LVal) {
	if v.Type != lisp.LNative {
		return nil, env.Errorf("argument is not a regexp: %v", v.Type)
	}
	re, ok := v.Native.(*regexp.Regexp)
	if !ok {
		return nil, env.Errorf("argument is not a regexp: %v", v)
	}
	return re, nil
}
