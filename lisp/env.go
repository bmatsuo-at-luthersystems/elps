package lisp

// LEnv is a lisp environment.
type LEnv struct {
	Count  int
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
	if k.Type != LSymbol {
		return Nil()
	}
	v, ok := env.Scope[k.Sym]
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
	if k.Type != LSymbol {
		return
	}
	if v == nil {
		panic("nil value")
	}
	env.Scope[k.Sym] = v.Copy()
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
// LVal.
func (env *LEnv) Eval(v *LVal) *LVal {
	switch v.Type {
	case LQSymbol:
		return Symbol(v.Sym)
	case LSymbol:
		return env.Get(v)
	case LSExpr:
		return env.EvalSExpr(v)
	default:
		return v
	}
}

// EvalSExpr evaluates s and returns the resulting LVal.
func (env *LEnv) EvalSExpr(s *LVal) *LVal {
	if s.Type != LSExpr {
		return Errorf("not an s-expression")
	}
	for i := range s.Cells {
		s.Cells[i] = env.Eval(s.Cells[i])
	}
	for i := range s.Cells {
		if s.Cells[i].Type == LError {
			return s.Cells[i]
		}
	}
	if len(s.Cells) == 0 {
		return Nil()
	}
	if len(s.Cells) == 1 {
		// I guess ``(x)'' is the same as ``x''?  Is this implying currying?
		return s.Cells[0]
	}
	f := s.Cells[0]
	if f.Type != LFun {
		return Errorf("first element of expression is not a function: %v", f.Type)
	}
	s.Cells = s.Cells[1:]
	return env.Call(f, s)
}

// Call invokes LFun fun with the list args.
func (env *LEnv) Call(fun *LVal, args *LVal) *LVal {
	if fun.Builtin != nil {
		return fun.Builtin(env, args)
	}
	nargs := len(fun.Formals.Cells) // only used when not vargs
	for i, v := range args.Cells {
		if len(fun.Formals.Cells) == 0 {
			return Errorf("function expects %d arguments (got %d)",
				nargs, len(args.Cells))
		}
		argSym := fun.Formals.Cells[0]
		if argSym.Sym == "&" {
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
		if fun.Formals.Cells[0].Sym != "&" {
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
