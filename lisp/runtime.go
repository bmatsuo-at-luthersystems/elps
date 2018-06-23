package lisp

import (
	"fmt"
	"io"
	"os"
	"sync/atomic"
)

// Runtime is an object underlying a family of tree of LEnv values.  It is
// responsible for holding shared environment state, generating identifiers,
// and writing debugging output to a stream (typically os.Stderr).
type Runtime struct {
	Registry *PackageRegistry
	Package  *Package
	Stderr   io.Writer
	Stack    *CallStack
	numenv   atomicCounter
	numsym   atomicCounter
}

// StandardRuntime returns a new Runtime with an empty package registry and
// Stderr set to os.Stderr.
func StandardRuntime() *Runtime {
	return &Runtime{
		Registry: NewRegistry(),
		Stderr:   os.Stderr,
		Stack:    &CallStack{},
	}
}

func (r *Runtime) GenEnvID() uint {
	return r.getEnvID()
}

func (r *Runtime) GenSym() string {
	return fmt.Sprintf("gen%08d", r.gensym())
}

func (r *Runtime) getEnvID() uint {
	return r.numenv.Add(1)
}

func (r *Runtime) gensym() uint {
	return r.numsym.Add(1)
}

type atomicCounter uint64

func (c *atomicCounter) Add(n uint) uint {
	return uint(atomic.AddUint64((*uint64)(c), uint64(1)))
}
