// Package lisplib is used to conveniently load the standard library for the
// elps environment
package lisplib

import (
	"bitbucket.org/luthersystems/elps/lisp"
	"bitbucket.org/luthersystems/elps/lisp/lisplib/libtime"
)

// LoadLibrary loads the standard library into env and returns env to the
// default user package.
func LoadLibrary(env *lisp.LEnv) *lisp.LVal {
	e := libtime.LoadPackage(env)
	if !e.IsNil() {
		return e
	}
	e = env.InPackage(lisp.Symbol(lisp.DefaultUserPackage))
	if !e.IsNil() {
		return e
	}
	return lisp.Nil()
}
