package libbase64

import (
	"encoding/base64"

	"bitbucket.org/luthersystems/elps/lisp"
	"bitbucket.org/luthersystems/elps/lisp/lisplib/internal/libutil"
)

// DeafultPackageName is the package name used by LoadPackage.
const DefaultPackageName = "base64"

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
	libutil.Function("encode", lisp.Formals("data"), builtinEncode),
	libutil.Function("decode", lisp.Formals("base64-data"), builtinDecode),
}

func builtinEncode(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	v := args.Cells[0]
	switch v.Type {
	case lisp.LString:
		// TODO:  Consider using a base64.Encoder here.
		b := make([]byte, base64.StdEncoding.EncodedLen(len(v.Str)))
		base64.StdEncoding.Encode(b, []byte(v.Str))
		return lisp.Bytes(b)
	case lisp.LBytes:
		b := make([]byte, base64.StdEncoding.EncodedLen(len(v.Bytes())))
		base64.StdEncoding.Encode(b, v.Bytes())
		return lisp.Bytes(b)
	default:
		return env.Errorf("argument is not a string: %v", v.Type)
	}
}

func builtinDecode(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	v := args.Cells[0]
	switch v.Type {
	case lisp.LString:
		b, err := base64.StdEncoding.DecodeString(v.Str)
		if err != nil {
			return env.Error(err)
		}
		return lisp.Bytes(b)
	case lisp.LBytes:
		b := make([]byte, base64.StdEncoding.DecodedLen(len(v.Bytes())))
		n, err := base64.StdEncoding.Decode(b, v.Bytes())
		if err != nil {
			return env.Error(err)
		}
		return lisp.Bytes(b[:n])
	default:
		return env.Errorf("argument is not a string: %v", v.Type)
	}
}
