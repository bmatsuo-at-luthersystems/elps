package lisp

var langMacros = []*langBuiltin{
	{"defun", macroDefun},
	{"if", macroIf},
}

// DefaultMacros returns the default set of LMacroDefs added to LEnv objects
// when LEnv.AddMacros is called without arguments.
func DefaultMacros() []LBuiltinDef {
	macros := make([]LBuiltinDef, len(langMacros))
	for i := range macros {
		macros[i] = langMacros[i]
	}
	return macros
}

func macroDefun(env *LEnv, args *LVal) *LVal {
	if len(args.Cells) != 3 {
		return berrf("defun", "three arguments expected (got %d)", len(args.Cells))
	}
	sym := args.Cells[0]
	if sym.Type != LSymbol {
		return berrf("defun", "first argument is not a symbol: %s", sym.Type)
	}
	v := env.GetGlobal(sym)
	if v.Type != LError {
		return berrf("defun", "symbol ``%v'' is already defined: %v", sym, v)
	}
	fun := Lambda(args.Cells[1], args.Cells[2])
	if fun.Type == LError {
		return fun
	}
	fun.Env.Parent = env // function definitions get a lexical scope
	env.PutGlobal(sym, fun)
	return Nil()
}

// (if test-form then-form else-form)
func macroIf(env *LEnv, s *LVal) *LVal {
	if len(s.Cells) != 3 {
		return berrf("if", "three arguments expected (got %d)", len(s.Cells))
	}
	r := env.Eval(s.Cells[0])
	if r.Type == LError {
		return r
	}
	ok := r.IsNil()
	if ok {
		// test-form evaluated to nil (false)
		return s.Cells[2]
	}
	// test-form evaluated to something non-nil (true)
	return s.Cells[1]
}
