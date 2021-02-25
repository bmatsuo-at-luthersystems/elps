package environ

import (
	"fmt"

	"github.com/luthersystems/elps/v2/pkg/lisp"
	"github.com/luthersystems/elps/v2/pkg/symbol"
)

// Environ is an lexical environment.  Environ contains local symbol bindings
// and a parent environment.  Environ is in the scope of its parent's bindings.
type Environ struct {
	parent   *Environ
	root     *Environ
	bindings Bindings
}

// New returns a new environment.  If parent is nil a root Environ will be
// returned.
func New(parent *Environ, bindings Bindings) *Environ {
	if bindings == nil {
		bindings = NewBindings(0)
	}
	env := &Environ{
		parent:   parent,
		bindings: bindings,
	}
	if parent != nil {
		env.root = parent.Root()
	}
	return env
}

// ExtendZipCons returns a new environment populated with bindings created by
// NewBindingsZipCons.
func ExtendZipCons(parent *Environ, vars, vals lisp.LVal) (*Environ, error) {
	bindings, err := NewBindingsZipCons(vars, vals)
	if err != nil {
		return nil, err
	}
	return New(parent, bindings), nil
}

func (env *Environ) Parent() *Environ {
	return env.parent
}

func (env *Environ) Root() *Environ {
	if env.root != nil {
		return env.root
	}
	return env
}

// Len returns the number of local bindings in env.
func (env *Environ) Len() int {
	return env.bindings.Len()
}

func (env *Environ) ancestor(i int) (*Environ, bool) {
	for i > 0 {
		if env.parent == nil {
			return nil, false
		}
		env = env.parent
		i--
	}
	return env, true
}

// GetAddress returns the value at index j of env's i-th ancestor.  If i is 0
// then GetAddress returns the value bound to the variable at index j of env.
// GetAddress return an error if the specified environment does not contain
// OrderedBindings.
func (env *Environ) GetAddress(i, j int) (lisp.LVal, error) {
	if i < 0 || j < 0 {
		return lisp.Nil(), fmt.Errorf("negative index in lexical address")
	}
	env, ok := env.ancestor(i)
	if !ok {
		return lisp.Nil(), fmt.Errorf("environment referenced by address does not exist")
	}
	bindings, ok := env.bindings.(OrderedBindings)
	if !ok {
		return lisp.Nil(), fmt.Errorf("environment does not support addressing")
	}
	if j >= env.Len() {
		return lisp.Nil(), fmt.Errorf("variable referenced by address does not exist")
	}
	return bindings.GetIndex(j), nil
}

// PutAddress stores v at index j of env's i-th ancestor.  If i is 0 then
// PutAddress stores v at index j of env.  PutAddress returns an error if the
// specified environment does not contain OrdererBindings.
func (env *Environ) PutAddress(i, j int, v lisp.LVal) error {
	if i < 0 || j < 0 {
		return fmt.Errorf("negative index in lexical address")
	}
	env, ok := env.ancestor(i)
	if !ok {
		return fmt.Errorf("environment referenced by address does not exist")
	}
	bindings, ok := env.bindings.(OrderedBindings)
	if !ok {
		return fmt.Errorf("environment does not support lexical bindings")
	}
	if j >= env.Len() {
		return fmt.Errorf("variable referenced by address does not exist")
	}
	bindings.PutIndex(j, v)
	return nil
}

// Get returns the value bound to the given symbol.
// Get returns an error if sym is not a symbol.
func (env *Environ) Get(id symbol.ID) (lisp.LVal, bool) {
	v, ok := env.bindings.Get(id)
	if ok {
		return v, true
	}
	for env.parent != nil {
		env = env.parent
		v, ok := env.bindings.Get(id)
		if ok {
			return v, true
		}
	}
	return lisp.Nil(), false
}

// Put binds id to v in env.
func (env *Environ) Put(id symbol.ID, v lisp.LVal) {
	env.bindings.Put(id, v)
}

// LVal wraps env as an LVal.
func (env *Environ) LVal(source lisp.Source, typ symbol.ID) lisp.LVal {
	return lisp.TagNative(typ, env)
}

// GetEnviron extracts an Environ from v.  GetEnviron returns false if v is not
// LTaggedVal or does not contain an Environ.
func GetEnviron(v lisp.LVal) (*Environ, bool) {
	if v.Type() != lisp.LTaggedVal {
		return nil, false
	}
	env, ok := v.Native.(*Environ)
	return env, ok
}
