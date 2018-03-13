package lisp

import (
	"fmt"
)

// LBuiltin is a function that performs executes a lisp function.
type LBuiltin func(env *LEnv, args *LVal) *LVal

// LBuiltinDef is a built-in function
// XXX: LBuiltinDef ... LBuiltInDef ... ?
type LBuiltinDef interface {
	Name() string
	Eval(env *LEnv, args *LVal) *LVal
}

type langBuiltin struct {
	name string
	fun  LBuiltin
}

func (fun *langBuiltin) Name() string {
	return fun.name
}

func (fun *langBuiltin) Eval(env *LEnv, args *LVal) *LVal {
	return fun.fun(env, args)
}

var langBuiltins = []*langBuiltin{
	{"set", builtinSet},
	{"lambda", builtinLambda},
	{"eval", builtinEval},
	{"car", builtinCAR},
	{"cdr", builtinCDR},
	{"concat", builtinConcat},
	{"list", builtinList},
	{"not", builtinNot},
	{"+", builtinAdd},
	{"-", builtinSub},
	{"/", builtinDiv},
	{"*", builtinMul},
}

// DefaultBuiltins returns the default set of LBuiltinDefs added to LEnv
// objects when LEnv.AddBuiltins is called without arguments.
func DefaultBuiltins() []LBuiltinDef {
	funs := make([]LBuiltinDef, len(langBuiltins))
	for i := range funs {
		funs[i] = langBuiltins[i]
	}
	return funs
}

func builtinSet(env *LEnv, v *LVal) *LVal {
	if len(v.Cells) < 2 {
		return berrf("set", "too few arguments provided: %d", len(v.Cells))
	}
	if len(v.Cells) > 2 {
		return berrf("set", "too many arguments provided: %d", len(v.Cells))
	}
	if v.Cells[0].Type != LSymbol {
		return berrf("set", "first argument is not a symbol: %v", v.Cells[0].Type)
	}

	env.PutGlobal(v.Cells[0], v.Cells[1])
	return env.GetGlobal(v.Cells[0])
}

func builtinLambda(env *LEnv, v *LVal) *LVal {
	if len(v.Cells) < 2 {
		return berrf("lambda", "too few arguments provided: %d", len(v.Cells))
	}
	if len(v.Cells) > 2 {
		return berrf("lambda", "too many arguments provided: %d", len(v.Cells))
	}
	for _, q := range v.Cells {
		if q.Type != LSExpr {
			return berrf("lambda", "argument is not a list: %v", q.Type)
		}
	}

	formals := v.Cells[0]
	body := v.Cells[1]

	for _, sym := range formals.Cells {
		if sym.Type != LSymbol {
			return berrf("lambda", "first argument contains a non-symbol: %v", sym.Type)
		}
	}

	// Construct the LVal and add env to the LEnv chain to get lexical scoping
	// (I think... -bmatsuo)
	fun := Lambda(formals, body)
	fun.Env.Parent = env

	return fun
}

func builtinEval(env *LEnv, args *LVal) *LVal {
	if len(args.Cells) > 1 {
		return berrf("eval", "too many arguments provided: %d", len(args.Cells))
	}
	v := args.Cells[0]
	if v.Type == LQuote {
		return v.Body
	}
	v.Quoted = false
	return env.Eval(v)
}

func builtinCAR(env *LEnv, v *LVal) *LVal {
	if len(v.Cells) != 1 {
		return berrf("car", "too many arguments provided: %d", len(v.Cells))
	}
	if v.Cells[0].Type != LSExpr {
		return berrf("car", "argument is not a list: %v", v.Cells[0].Type)
	}
	if len(v.Cells[0].Cells) == 0 {
		// Maybe this should just return v?
		return berrf("car", "argument is empty")
	}
	return v.Cells[0].Cells[0]
}

func builtinCDR(env *LEnv, v *LVal) *LVal {
	if len(v.Cells) != 1 {
		return berrf("cdr", "too many arguments provided: %d", len(v.Cells))
	}
	if v.Cells[0].Type != LSExpr {
		return berrf("cdr", "argument is not a list %v", v.Cells[0].Type)
	}
	if len(v.Cells[0].Cells) == 0 {
		// Maybe this should just return v?
		return berrf("cdr", "argument is empty")
	}
	q := QExpr()
	q.Cells = v.Cells[0].Cells[1:]
	return q
}

func builtinConcat(env *LEnv, v *LVal) *LVal {
	q := QExpr()
	for _, c := range v.Cells {
		if c.Type != LSExpr {
			return berrf("concat", "argument is not a list: %v", c.Type)
		}
		q.Cells = append(q.Cells, c.Cells...)
	}
	return q
}

func builtinList(env *LEnv, v *LVal) *LVal {
	q := QExpr()
	q.Cells = v.Cells
	return q
}

func builtinNot(env *LEnv, v *LVal) *LVal {
	if len(v.Cells) != 1 {
		return berrf("not", "too many arguments provided: %d", len(v.Cells))
	}
	switch v.Cells[0].Type {
	case LSExpr:
		if len(v.Cells[0].Cells) == 0 {
			// v.Cells[0] is nil
			return Symbol("t")
		}
	}
	return Nil()
}

func builtinAdd(env *LEnv, v *LVal) *LVal {
	sum := 0
	for _, c := range v.Cells {
		if c.Type != LNumber {
			return berrf("+", "argument is not a number: %v", c.Type)
		}
		sum += c.Num
	}
	return Number(sum)
}

func builtinSub(env *LEnv, v *LVal) *LVal {
	diff := 0
	if len(v.Cells) > 1 {
		if v.Cells[0].Type != LNumber {
			return berrf("-", "argument is not a number: %v", v.Cells[0].Type)
		}
		diff = v.Cells[0].Num
		v.Cells = v.Cells[1:]
	}
	for _, c := range v.Cells {
		if c.Type != LNumber {
			return berrf("-", "argument is not a number: %v", c.Type)
		}
		diff -= c.Num
	}
	return Number(diff)
}

func builtinDiv(env *LEnv, v *LVal) *LVal {
	div := 1
	if len(v.Cells) > 1 {
		if v.Cells[0].Type != LNumber {
			return berrf("/", "argument is not a number: %v", v.Cells[0].Type)
		}
		div = v.Cells[0].Num
		v.Cells = v.Cells[1:]
	}
	for _, c := range v.Cells {
		if c.Type != LNumber {
			return berrf("/", "argument is not a number: %v", c.Type)
		}
		if c.Num == 0 {
			return berrf("/", "argument is not a number: %v", c.Type)
		}
		div /= c.Num
	}
	return Number(div)
}

func builtinMul(env *LEnv, v *LVal) *LVal {
	prod := 1
	for _, c := range v.Cells {
		if c.Type != LNumber {
			return berrf("*", "argument is not a number: %v", c.Type)
		}
		prod *= c.Num
	}
	return Number(prod)
}

func berrf(bname string, format string, v ...interface{}) *LVal {
	return Errorf("<builtin ``%s''>: %v", bname, fmt.Sprintf(format, v...))
}
