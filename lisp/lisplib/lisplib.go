// Package lisplib is used to conveniently load the standard library for the
// elps environment
package lisplib

import (
	"bitbucket.org/luthersystems/elps/lisp"
	"bitbucket.org/luthersystems/elps/lisp/lisplib/libbase64"
	"bitbucket.org/luthersystems/elps/lisp/lisplib/libgolang"
	"bitbucket.org/luthersystems/elps/lisp/lisplib/libjson"
	"bitbucket.org/luthersystems/elps/lisp/lisplib/libmath"
	"bitbucket.org/luthersystems/elps/lisp/lisplib/libregexp"
	"bitbucket.org/luthersystems/elps/lisp/lisplib/libstring"
	"bitbucket.org/luthersystems/elps/lisp/lisplib/libtesting"
	"bitbucket.org/luthersystems/elps/lisp/lisplib/libtime"
)

// LoadLibrary loads the standard library into env and returns env to the
// default user package.
func LoadLibrary(env *lisp.LEnv) *lisp.LVal {
	e := libtime.LoadPackage(env)
	if !e.IsNil() {
		return e
	}
	e = libgolang.LoadPackage(env)
	if !e.IsNil() {
		return e
	}
	e = libmath.LoadPackage(env)
	if !e.IsNil() {
		return e
	}
	e = libstring.LoadPackage(env)
	if !e.IsNil() {
		return e
	}
	e = libbase64.LoadPackage(env)
	if !e.IsNil() {
		return e
	}
	e = libjson.LoadPackage(env)
	if !e.IsNil() {
		return e
	}
	e = libregexp.LoadPackage(env)
	if !e.IsNil() {
		return e
	}
	e = libtesting.LoadPackage(env)
	if !e.IsNil() {
		return e
	}
	e = env.InPackage(lisp.Symbol(lisp.DefaultUserPackage))
	if !e.IsNil() {
		return e
	}
	return lisp.Nil()
}
