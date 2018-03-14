package lisp

import "fmt"

// LEnv is a lisp environment.
type LEnv struct {
	Scope  map[string]*LVal
	Parent *LEnv
}

// NewEnv returns initializes and returns a new LEnv.
func NewEnv(parent *LEnv) *LEnv {
	return &LEnv{
		Scope:  make(map[string]*LVal),
		Parent: parent,
	}
}

// Copy returns a new LEnv with a copy of env.Scope but a shared parent (not
// quite a deep copy).
func (env *LEnv) Copy() *LEnv {
	if env == nil {
		return nil
	}
	cp := &LEnv{}
	*cp = *env
	cp.Scope = make(map[string]*LVal, len(env.Scope))
	for k, v := range env.Scope {
		cp.Scope[k] = v
	}
	return cp
}

// Get takes an LSymbol k and returns the LVal it is bound to in env.
func (env *LEnv) Get(k *LVal) *LVal {
	// LQSymbols are allowed...
	if k.Type != LSymbol && k.Type != LQSymbol {
		return Nil()
	}
	if k.Str == "t" {
		return Symbol("t")
	}
	v, ok := env.Scope[k.Str]
	if ok {
		return v.Copy()
	}
	if env.Parent != nil {
		return env.Parent.Get(k)
	}
	return Errorf("unbound symbol: %v", k)
}

// Put takes an LSymbol k and binds it to v in env.
func (env *LEnv) Put(k, v *LVal) {
	if k.Type != LSymbol && k.Type != LQSymbol {
		return
	}
	if k.Str == "t" {
		panic("constant value")
	}
	if v == nil {
		panic("nil value")
	}
	env.Scope[k.Str] = v.Copy()
}

// GetGlobal takes LSymbol k and returns the value it is bound to in the root
// environment (global scope).
func (env *LEnv) GetGlobal(k *LVal) *LVal {
	return env.root().Get(k)
}

// PutGlobal takes an LSymbol k and binds it to v in root environment (global
// scope).
func (env *LEnv) PutGlobal(k, v *LVal) {
	env.root().Put(k, v)
}

func (env *LEnv) root() *LEnv {
	for env.Parent != nil {
		env = env.Parent
	}
	return env
}

// AddMacros binds the given macros to their names in env.  When called with no
// arguments AddMacros adds the DefaultMacros to env.
func (env *LEnv) AddMacros(macs ...LBuiltinDef) {
	if len(macs) == 0 {
		macs = DefaultMacros()
	}
	for i, mac := range macs {
		k := Symbol(mac.Name())
		exist := env.Get(k)
		if !exist.IsNil() && exist.Type != LError { // LError is ubound symbol
			panic(fmt.Sprintf("macro already defined: %v (= %v)", k, exist))
		}
		env.Put(k, Macro(macs[i].Eval))
	}
}

// AddBuiltins binds the given funs to their names in env.  When called with no
// arguments AddBuiltins adds the DefaultBuiltins to env.
func (env *LEnv) AddBuiltins(funs ...LBuiltinDef) {
	if len(funs) == 0 {
		funs = DefaultBuiltins()
	}
	for i := range funs {
		k := Symbol(funs[i].Name())
		v := Fun(funs[i].Eval)
		env.Put(k, v)
	}
}

// Eval evaluates v in the context (scope) of env and returns the resulting
// LVal.  Eval does not unquote v during evaluation -- a difference between
// Eval and the ``eval'' builtin function.
func (env *LEnv) Eval(v *LVal) *LVal {
	if v.Quoted {
		return v
	}
	switch v.Type {
	case LSymbol:
		return env.Get(v)
	case LSExpr:
		return env.EvalSExpr(v)
	case LQuote:
		// this quote was unquoted... eval the underlying value
		return env.Eval(v.Body)
	default:
		return v
	}
}

// EvalSExpr evaluates s and returns the resulting LVal.
func (env *LEnv) EvalSExpr(s *LVal) *LVal {
	if s.Type != LSExpr {
		return Errorf("not an s-expression")
	}
	if len(s.Cells) == 0 {
		return Nil()
	}

	f := env.Eval(s.Cells[0])
	if f.Type == LError {
		return f
	}
	if f.Type != LFun {
		return Errorf("first element of expression is not a function: %v", f)
	}
	if f.Macro {
		// Argument to a macro a not evaluated but they aren't quoted either.
		// This behavior is what allows ``unquote'' to properly resolve macro
		// arguments symbols during and still produce valid code during macro
		// expansion.  That is, if x is a macro argument then what do the
		// following expressions return?
		//		(quasiquote (unquote x))             	  => {expression bound to x}
		//		(quasiquote (unquote '(if 1 '(1) '(2))))  => '(1)
		// If the value given to x was quoted by eval then ``unquote'' would
		// have to undo that quoting.  But unquote is not supposed to unquote
		// the value returned by (if 1 '(1) '(2)), it merely evaluates the
		// expression and produces '(1).
	} else {
		// Evaluate arguments before invoking f.
		for i := 1; i < len(s.Cells); i++ {
			s.Cells[i] = env.Eval(s.Cells[i])
		}
		for _, c := range s.Cells[1:] {
			if c.Type == LError {
				return c
			}
		}
		if len(s.Cells) == 1 {
			// I guess ``(x)'' is the same as ``x''?  Is this implying currying?
			return s.Cells[0]
		}
	}
	s.Cells = s.Cells[1:]
	r := env.Call(f, s)
	if r.Type == LError {
		return r
	}
	if !f.Macro {
		return r
	}
	// TODO:  Turn macro expansion into a loop instead of a recursive process.
	// A real program will probably exhaust system memory with the call stack
	// when expanding macros.
	r.Quoted = false
	return env.Eval(r)
}

// Call invokes LFun fun with the list args.
func (env *LEnv) Call(fun *LVal, args *LVal) *LVal {
	if fun.Builtin != nil {
		return fun.Builtin(env, args)
	}
	// FIXME:  A shallow copy is probably correct here.  We don't want to copy
	// the Env so that updates to the global scope are reflected.
	fun = fun.Copy()
	nargs := len(fun.Formals.Cells) // only used when not vargs
	for i, v := range args.Cells {
		if len(fun.Formals.Cells) == 0 {
			return Errorf("function expects %d arguments (got %d)",
				nargs, len(args.Cells))
		}
		argSym := fun.Formals.Cells[0]
		if argSym.Str == "&" {
			if len(fun.Formals.Cells) == 1 {
				return Errorf("function argument format list ends with symbol ``&''")
			}
			argSym = fun.Formals.Cells[1]
			q := QExpr()
			q.Cells = args.Cells[i:]
			fun.Env.Put(argSym, q)
			break
		}
		fun.Formals.Cells = fun.Formals.Cells[1:]
		fun.Env.Put(argSym, v)
	}
	if len(fun.Formals.Cells) != 0 {
		if fun.Formals.Cells[0].Str != "&" {
			return fun
		}
		if len(fun.Formals.Cells) != 2 {
			return Errorf("function argument format list ends with symbol ``&''")
		}
		fun.Formals.Cells = nil
	}
	// NOTE: The bugs suggestion of chaining env here seems like dynamic
	// scoping.
	//		fun.Env.Parent = env
	s := SExpr()
	s.Cells = append(s.Cells, fun.Body.Cells...)
	return fun.Env.Eval(s)
}
