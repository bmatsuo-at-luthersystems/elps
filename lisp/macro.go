package lisp

import (
	"fmt"
	"os"
)

var userMacros []*langBuiltin
var langMacros = []*langBuiltin{
	{"defmacro", Formals("name", "formals", "expr"), macroDefmacro},
	{"defun", Formals("name", "formals", VarArgSymbol, "expr"), macroDefun},
	{"quote", Formals("expr"), macroQuote},
	{"trace", Formals("expr"), macroTrace},
}

// RegisterDefaultMacro adds the given function to the list returned by
// DefaultMacros.
func RegisterDefaultMacro(name string, formals *LVal, fn LBuiltin) {
	userMacros = append(userMacros, &langBuiltin{name, formals.Copy(), fn})
}

// DefaultMacros returns the default set of LBuiltinDef added to LEnv objects
// when LEnv.AddMacros is called without arguments.
func DefaultMacros() []LBuiltinDef {
	ops := make([]LBuiltinDef, len(langMacros)+len(userMacros))
	for i := range langMacros {
		ops[i] = langMacros[i]
	}
	offset := len(langMacros)
	for i := range userMacros {
		ops[offset+i] = langMacros[i]
	}
	return ops
}

func macroDefmacro(env *LEnv, args *LVal) *LVal {
	sym := args.Cells[0]
	if sym.Type != LSymbol {
		return env.Errorf("first argument is not a symbol: %s", sym.Type)
	}
	v := env.GetGlobal(sym)
	if v.Type != LError {
		return env.Errorf("symbol ``%v'' is already defined: %v", sym, v)
	}
	fun := Lambda(args.Cells[1], []*LVal{args.Cells[2]})
	if fun.Type == LError {
		fun.Stack = env.Stack.Copy()
		return fun
	}
	fun.FunType = LFunMacro // evaluate as a macro
	fun.Package = env.root().Package.Name
	fun.Env.Parent = env // function definitions get a lexical scope
	fun.Env.Stack = env.Stack
	env.PutGlobal(sym, fun)
	return Nil()
}

func macroDefun(env *LEnv, args *LVal) *LVal {
	sym := args.Cells[0]
	if sym.Type != LSymbol {
		return env.Errorf("first argument is not a symbol: %s", sym.Type)
	}
	v := env.GetGlobal(sym)
	if v.Type != LError {
		return env.Errorf("symbol ``%v'' is already defined: %v", sym, v)
	}
	fun := Lambda(args.Cells[1], args.Cells[2:])
	if fun.Type == LError {
		fun.Stack = env.Stack.Copy()
		return fun
	}
	fun.Env.Parent = env // function definitions get a lexical scope
	fun.Package = env.root().Package.Name
	fun.Env.Stack = env.Stack
	env.PutGlobal(sym, fun)
	return Nil()
}

func macroQuote(env *LEnv, args *LVal) *LVal {
	if len(args.Cells) != 1 {
		return env.Errorf("one argument expected (got %d)", len(args.Cells))
	}
	// NOTE:  Racket seems to detect nested (quote ...) expressions when
	// quoting things.  That is, (quote (quote 3)) in Racket evaluates to ''3,
	// not '(quote 3).  We could try to dig into the quoted arguments to
	// determine if that were possible but it is unclear whether it's possible
	// for ``quote'' to resolve differently or for this macro to be called
	// under a different name.
	return args.Cells[0]
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
			return env.Errorf("%v: one argument expected (got %d)", v.Cells[0].Str, len(v.Cells))
		}
		// The v looks like ``(unquote EXPR)'' or ``(unquote-splicing expr)''
		var x *LVal
		unquote := v.Cells[0]
		// Try to unwrap a quoted expression into x.  If x is not quoted
		// then we have to evaluate unquote
		if unquote.Type == LQuote {
			x = unquote.Cells[0]
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
			splice := v.Cells[i]
			if splice.Type != LSExpr {
				// TODO:  I believe it is incorrect to error out here.  But
				// splicing non-lists is not a major concern at the moment.
				return env.Errorf("%s: cannot splice non-list: %s", "unquote-splicing", splice.Type)
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

func macroTrace(env *LEnv, args *LVal) *LVal {
	if len(args.Cells) != 1 {
		return env.Errorf("one argument expected (got %d)", len(args.Cells))
	}
	fmt.Fprintln(os.Stderr, args.Cells[0])
	return args.Cells[0]
}
