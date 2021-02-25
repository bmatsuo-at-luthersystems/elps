package lisp

/*

type Env struct {
	Parent   *Env
	Bindings map[string]LVal
}

// NewEnv returns Env that is a child of parent.  If parent is nil then the
// returned Env is a root environment.
func NewEnv(parent *Env) *Env {
	env := &Env{
		Parent:   parent,
		Bindings: make(map[string]LVal),
	}
	return env
}

// LispEval evaluates primitive lisp data types.  The nontrivial function of
// LispEval is to resolve symbol bindings.
func (env *Env) LispEval(v LVal) LVal {
	switch v.Type() {
	case LString:
		return v
	case LSymbol:
		return env.Get(v)
	case LInt:
		return v
	case LFloat:
		return v
	case LBool:
		return v
	default:
		return env.ArgumentErrorf("environment cannot evaluate type: %v", v.Type())
	}
}

// LispString returns a source-code representation of v.
func (env *Env) LispString(v LVal) string {
	// TODO:  Implement this function.
	switch v.Type() {
	case LCons:
		return MustCons(v).LispString(env)
	default:
		return fmt.Sprint(v)
	}
}

// Get resolves the value bound to the symbol key.  Get returns an error if key
// is not an LSymbol value.
func (env *Env) Get(key LVal) LVal {
	if key.Type() != LSymbol {
		return env.ArgumentErrorf("argument is not a string: %v", key.Type())
	}
	for {
		v, ok := env.Bindings[key.Str]
		if ok {
			return v
		}
		if env.Parent != nil {
			env = env.Parent
			continue
		}
		return Nil()
	}
}

// Put binds key to val in env.  Put returns an error if key is not an LSymbol
// value.
func (env *Env) Put(key, val LVal) LVal {
	if key.Type() != LSymbol {
		return env.ArgumentErrorf("argument is not a string: %v", key.Type())
	}
	env.Bindings[key.Str] = val
	return Nil()
}

// ArgumentErrorf returns an LError value with the condition "argument-error".
func (env *Env) ArgumentErrorf(format string, args ...interface{}) LVal {
	lerr := Error(fmt.Errorf(format, args...))
	lerr.Str = "argument-error"
	return lerr
}
*/
