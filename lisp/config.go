package lisp

import "io"

// Config is a function that configures a root environment or its runtime.
type Config func(env *LEnv) *LVal

// WithMaximumEffectiveStackHeight returns a Config that will prevent an
// execution environment from allowing the effective stack height to exceed n.
// The effective height of the stack is the stacks physical height plus any the
// number of stack frames which have been ellided due to tail recursive call
// optimizations.
func WithMaximumEffectiveStackHeight(n int) Config {
	return func(env *LEnv) *LVal {
		env.Runtime.Stack.MaxHeightEffective = n
		return Nil()
	}
}

// WithLoader returns a Config that executes fn and ensures that the
// environment's working package is reset following execution of fn.  Despite
// fn having the same signature as a Config WithLoader allows a Loader to
// function more like the LEnv methods LoadFile, LoadString, etc.
func WithLoader(fn Loader) Config {
	return func(env *LEnv) (lerr *LVal) {
		pkg := env.Runtime.Package.Name
		defer func() {
			e := env.InPackage(Symbol(pkg))
			if e.Type == LError && lerr.Type != LError {
				lerr = e
			}
		}()
		return fn(env)
	}
}

// WithReader returns a Config that makes environments use r to parse source
// streams.  There is no default Reader for an environment.
func WithReader(r Reader) Config {
	return func(env *LEnv) *LVal {
		env.Runtime.Reader = r
		return Nil()
	}
}

// WithStderr returns a Config that makes environments write debugging output
// to w instead of the default, os.Stderr.
func WithStderr(w io.Writer) Config {
	return func(env *LEnv) *LVal {
		env.Runtime.Stderr = w
		return Nil()
	}
}
