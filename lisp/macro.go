package lisp

var langMacros = []*langBuiltin{
	{"defmacro", macroDefmacro},
	{"defun", macroDefun},
	{"quote", macroQuote},
	{"quasiquote", macroQuasiquote},
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

func macroDefmacro(env *LEnv, args *LVal) *LVal {
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
	fun.Macro = true     // evaluate as a macro
	fun.Env.Parent = env // function definitions get a lexical scope
	env.PutGlobal(sym, fun)
	return Nil()
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

func macroQuote(env *LEnv, args *LVal) *LVal {
	if len(args.Cells) != 1 {
		return berrf("quote", "one argument expected (got %d)", len(args.Cells))
	}
	// NOTE:  Racket seems to detect nested (quote ...) expressions when
	// quoting things.  We could try to dig into the quoted arguments to
	// determine if that were possible but it is unclear whether it's possible
	// for ``quote'' to resolve differently or for this macro to be called
	// under a different name.
	return args.Cells[0]
}

func macroQuasiquote(env *LEnv, args *LVal) *LVal {
	if len(args.Cells) != 1 {
		return berrf("quasiquote", "one argument expected (got %d)", len(args.Cells))
	}
	// We need to find and unquote values in args.Cells[0] (possibly
	// args.Cells[0] itself).
	return findAndUnquote(env, args.Cells[0])
}

func findAndUnquote(env *LEnv, v *LVal) *LVal {
	if v.Type != LSExpr {
		return v
	}
	if len(v.Cells) > 0 && v.Cells[0].Type == LSymbol && v.Cells[0].Sym == "unquote" {
		// The v looks like ``(unquote EXPR)''
		v.Cells = v.Cells[1:]
		if len(v.Cells) != 1 {
			return berrf("unquote", "one argument expected (got %d)", len(v.Cells))
		}
		// This feels... wrong
		unquote := v.Cells[0]
		unquote.Quoted = false
		return env.Eval(unquote)
	}
	// findAndUnquote all child expressions
	for i := range v.Cells {
		v.Cells[i] = findAndUnquote(env, v.Cells[i])
	}
	return v
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
		//s.Cells[2].Quoted = false
		return s.Cells[2]
	}
	// test-form evaluated to something non-nil (true)
	//s.Cells[1].Quoted = false
	return s.Cells[1]
}
