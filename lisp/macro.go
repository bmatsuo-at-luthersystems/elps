package lisp

import (
	"fmt"
	"log"
	"os"
)

var langSpecialOps = []*langBuiltin{
	{"quasiquote", macroQuasiquote},
	{"lambda", builtinLambda},
	{"let*", macroLetSeq},
	{"let", macroLet},
	{"progn", macroProgn},
	{"if", macroIf},
	{"or", opOr},
	{"and", opAnd},
}

var langMacros = []*langBuiltin{
	{"defmacro", macroDefmacro},
	{"defun", macroDefun},
	{"quote", macroQuote},
	{"trace", macroTrace},
}

// DefaultMacros returns the default set of LBuiltinDef added to LEnv objects
// when LEnv.AddMacros is called without arguments.
func DefaultMacros() []LBuiltinDef {
	macros := make([]LBuiltinDef, len(langMacros))
	for i := range macros {
		macros[i] = langMacros[i]
	}
	return macros
}

// DefaultSpecialOps returns the default set of LBuiltinDef added to LEnv
// objects when LEnv.AddSpecialOps is called without arguments.
func DefaultSpecialOps() []LBuiltinDef {
	macros := make([]LBuiltinDef, len(langSpecialOps))
	for i := range macros {
		macros[i] = langSpecialOps[i]
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
	fun.FunType = LFunMacro // evaluate as a macro
	fun.Env.Parent = env    // function definitions get a lexical scope
	fun.Env.Stack = env.Stack
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
	fun.Env.Stack = env.Stack
	env.PutGlobal(sym, fun)
	return Nil()
}

func macroQuote(env *LEnv, args *LVal) *LVal {
	if len(args.Cells) != 1 {
		return berrf("quote", "one argument expected (got %d)", len(args.Cells))
	}
	// NOTE:  Racket seems to detect nested (quote ...) expressions when
	// quoting things.  That is, (quote (quote 3)) in Racket evaluates to ''3,
	// not '(quote 3).  We could try to dig into the quoted arguments to
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

	// NOTE:  This isUnquote check is really strange.  But expressions like
	// (quasiquote (unquote ...)) do not seem to evaluate correctly without it.
	// It goes with some checking in LVal.EvalSExpr that doesn't really seem
	// right, all in all.
	quote := !isUnquote(args.Cells[0])
	result := findAndUnquote(env, args.Cells[0])
	if quote {
		return Quote(result)
	}
	return result
}

func isUnquote(v *LVal) bool {
	if v.Type != LSExpr {
		return false
	}
	if len(v.Cells) < 1 {
		return false
	}
	if v.Cells[0].Type != LSymbol {
		return false
	}
	return v.Cells[0].Str == "unquote" || v.Cells[0].Str == "unquote-splicing"
}

// NOTE:  There are almost surely bugs with this implementation un unquoting,
// especially when dealing with complex macros using multiple levels of
// quasiquotes/unquotes.  I need to find these examples, understand how they
// are supposed to work, and then fix these test cases.
func findAndUnquote(env *LEnv, v *LVal) *LVal {
	if v.Type != LSExpr {
		return v
	}
	if isUnquote(v) {
		spliceType := v.Cells[0].Str
		v.Cells = v.Cells[1:]
		if len(v.Cells) != 1 {
			return berrf(v.Cells[0].Str, "one argument expected (got %d)", len(v.Cells))
		}
		// The v looks like ``(unquote EXPR)'' or ``(unquote-splicing expr)''
		var x *LVal
		unquote := v.Cells[0]
		// Try to unwrap a quoted expression into x.  If x is not quoted
		// then we have to evaluate unquote
		if unquote.Type == LQuote {
			x = unquote.Body
		} else if unquote.Quoted {
			unquote.Quoted = false
			x = unquote
		} else {
			x = env.Eval(unquote)
		}
		if spliceType == "unquote-splicing" {
			x.Spliced = true
		}
		return findAndUnquote(env, x)
	}
	// findAndUnquote all child expressions
	for i := range v.Cells {
		v.Cells[i] = findAndUnquote(env, v.Cells[i])
	}
	// splice in grandchildren of children that were unquoted with
	// ``unquote-splicing''
	for i := len(v.Cells) - 1; i >= 0; i-- {
		if v.Cells[i].Spliced {
			log.Printf("splice detected")
			splice := v.Cells[i]
			if splice.Type != LSExpr {
				// TODO:  I believe it is incorrect to error out here.  But
				// splicing non-lists is not a major concern at the moment.
				return berrf("unquote-splicing", "cannot splice not list: %s", splice.Type)
			}
			// TODO:  Be clever and don't force allocation here.  Grow newcells and shift v.Cells[i+1:]
			newcells := make([]*LVal, 0, len(v.Cells)+len(splice.Cells))
			newcells = append(newcells, v.Cells[:i]...)
			newcells = append(newcells, splice.Cells...)
			newcells = append(newcells, v.Cells[i+1:]...)
			v.Cells = newcells
		}
	}
	return v
}

func macroLetSeq(env *LEnv, args *LVal) *LVal {
	letenv := NewEnv(env)
	bindlist := args.Cells[0]
	args.Cells = args.Cells[1:] // decap so we can call builtinProgn on args.
	if bindlist.Type != LSExpr {
		return berrf("let*", "first argument is not a list: %s", bindlist.Type)
	}
	for _, bind := range bindlist.Cells {
		if bind.Type != LSExpr {
			return berrf("let*", "first argument is not a list of pairs")
		}
		if len(bind.Cells) != 2 {
			return berrf("let*", "first argument is not a list of pairs")
		}
		val := letenv.Eval(bind.Cells[1])
		if val.Type == LError {
			return val
		}
		letenv.Put(bind.Cells[0], val)
	}
	return macroProgn(letenv, args)
}

func macroLet(env *LEnv, args *LVal) *LVal {
	letenv := NewEnv(env)
	bindlist := args.Cells[0]
	args.Cells = args.Cells[1:] // decap so we can call builtinProgn on args.
	if bindlist.Type != LSExpr {
		return berrf("let", "first argument is not a list: %s", bindlist.Type)
	}
	vals := make([]*LVal, len(bindlist.Cells))
	for i, bind := range bindlist.Cells {
		if bind.Type != LSExpr {
			return berrf("let", "first argument is not a list of pairs")
		}
		if len(bind.Cells) != 2 {
			return berrf("let", "first argument is not a list of pairs")
		}
		vals[i] = letenv.Eval(bind.Cells[1])
		if vals[i].Type == LError {
			return vals[i]
		}
	}
	for i, bind := range bindlist.Cells {
		letenv.Put(bind.Cells[0], vals[i])
	}
	return macroProgn(letenv, args)
}

func macroTrace(env *LEnv, args *LVal) *LVal {
	if len(args.Cells) != 1 {
		return berrf("trace", "one argument expected (got %d)", len(args.Cells))
	}
	fmt.Fprintln(os.Stderr, args.Cells[0])
	return args.Cells[0]
}

func macroProgn(env *LEnv, args *LVal) *LVal {
	if len(args.Cells) == 0 {
		return Nil()
	}
	args.Cells[len(args.Cells)-1].Terminal = true
	var val *LVal
	for _, c := range args.Cells {
		val = env.Eval(c)
	}
	return val
}

func opOr(env *LEnv, s *LVal) *LVal {
	if len(s.Cells) > 0 {
		s.Cells[len(s.Cells)-1].Terminal = true
	}
	for _, c := range s.Cells {
		r := env.Eval(c)
		if r.Type == LError {
			return r
		}
		ok := r.IsNil()
		if ok {
			return Bool(true)
		}
	}
	return Bool(false)
}

func opAnd(env *LEnv, s *LVal) *LVal {
	if len(s.Cells) > 0 {
		s.Cells[len(s.Cells)-1].Terminal = true
	}
	for _, c := range s.Cells {
		r := env.Eval(c)
		if r.Type == LError {
			return r
		}
		ok := r.IsNil()
		if !ok {
			return Bool(false)
		}
	}
	return Bool(true)
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
		s.Cells[2].Terminal = true
		return env.Eval(s.Cells[2])
	}
	// test-form evaluated to something non-nil (true)
	s.Cells[1].Terminal = true
	return env.Eval(s.Cells[1])
}
