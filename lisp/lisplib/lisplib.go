// Package lisplib is used to conveniently load the standard library for the
// elps environment
package lisplib

import (
	"bitbucket.org/luthersystems/elps/lisp"
	"bitbucket.org/luthersystems/elps/lisp/lisplib/libbase64"
	"bitbucket.org/luthersystems/elps/lisp/lisplib/libgolang"
	"bitbucket.org/luthersystems/elps/lisp/lisplib/libjson"
	"bitbucket.org/luthersystems/elps/lisp/lisplib/libmath"
	"bitbucket.org/luthersystems/elps/lisp/lisplib/libos"
	"bitbucket.org/luthersystems/elps/lisp/lisplib/libos/libosio"
	"bitbucket.org/luthersystems/elps/lisp/lisplib/libregexp"
	"bitbucket.org/luthersystems/elps/lisp/lisplib/libstring"
	"bitbucket.org/luthersystems/elps/lisp/lisplib/libtesting"
	"bitbucket.org/luthersystems/elps/lisp/lisplib/libtime"
)

var loaders = []lisp.Loader{
	libtime.LoadPackage,
	libgolang.LoadPackage,
	libmath.LoadPackage,
	libstring.LoadPackage,
	libbase64.LoadPackage,
	libjson.LoadPackage,
	libos.LoadPackage,
	libosio.LoadPackage,
	libregexp.LoadPackage,
	libtesting.LoadPackage,
}

// LoadLibrary loads the standard library into env and returns env to the
// default user package.
func LoadLibrary(env *lisp.LEnv) *lisp.LVal {
	for _, fn := range loaders {
		lerr := fn(env)
		if !lerr.IsNil() {
			return lerr
		}
	}
	lerr := env.InPackage(lisp.Symbol(lisp.DefaultUserPackage))
	if !lerr.IsNil() {
		return lerr
	}
	return lisp.Nil()
}
