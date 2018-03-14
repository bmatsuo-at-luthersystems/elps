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
	{"map", builtinMap},
	{"foldl", builtinFoldLeft},
	{"foldr", builtinFoldRight},
	{"list", builtinList},
	{"not", builtinNot},
	{">", builtinGT},
	{"<", builtinLT},
	{"=", builtinEqNum},
	{"+", builtinAdd},
	{"-", builtinSub},
	{"/", builtinDiv},
	{"*", builtinMul},
	{"debug-print", builtinDebugPrint},
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

func builtinMap(env *LEnv, args *LVal) *LVal {
	if len(args.Cells) != 2 {
		return berrf("map", "too many arguments provided: %d", len(args.Cells))
	}
	f := args.Cells[0]
	if f.Type != LFun {
		return berrf("map", "first argument is not a function: %s", f.Type)
	}
	lis := args.Cells[1]
	if lis.Type != LSExpr {
		return berrf("map", "second argument is not a list: %s", lis.Type)
	}
	for i, c := range lis.Cells {
		fargs := QExpr()
		fargs.Cells = []*LVal{c}
		fret := env.Call(f, fargs)
		if fret.Type == LError {
			return fret
		}
		lis.Cells[i] = fret
	}
	return lis
}

func builtinFoldLeft(env *LEnv, args *LVal) *LVal {
	if len(args.Cells) != 3 {
		return berrf("foldl", "too many arguments provided: %d", len(args.Cells))
	}
	f := args.Cells[0]
	if f.Type != LFun {
		return berrf("foldl", "first argument is not a function: %s", f.Type)
	}
	acc := args.Cells[1]
	lis := args.Cells[2]
	if lis.Type != LSExpr {
		return berrf("foldl", "third argument is not a list: %s", lis.Type)
	}
	for _, c := range lis.Cells {
		fargs := QExpr()
		fargs.Cells = []*LVal{
			// args reversed from foldr function invocation
			acc,
			c,
		}
		fret := env.Call(f, fargs)
		if fret.Type == LError {
			return fret
		}
		acc = fret
	}
	return acc
}

func builtinFoldRight(env *LEnv, args *LVal) *LVal {
	if len(args.Cells) != 3 {
		return berrf("foldr", "too many arguments provided: %d", len(args.Cells))
	}
	f := args.Cells[0]
	if f.Type != LFun {
		return berrf("foldr", "first argument is not a function: %s", f.Type)
	}
	acc := args.Cells[1]
	lis := args.Cells[2]
	if lis.Type != LSExpr {
		return berrf("foldr", "third argument is not a list: %s", lis.Type)
	}
	for i := len(lis.Cells) - 1; i >= 0; i-- {
		c := lis.Cells[i]
		fargs := QExpr()
		fargs.Cells = []*LVal{
			// args reversed from foldl function invocation
			c,
			acc,
		}
		fret := env.Call(f, fargs)
		if fret.Type == LError {
			return fret
		}
		acc = fret
	}
	return acc
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

func builtinLT(env *LEnv, args *LVal) *LVal {
	if len(args.Cells) != 2 {
		berrf("<", "two arguments expected (got %d)", len(args.Cells))
	}
	a, b := args.Cells[0], args.Cells[1]
	if a.IsNumeric() {
		berrf("<", "first argument is not a number: %s", a.Type)
	}
	if b.IsNumeric() {
		berrf("<", "second argument is not a number: %s", b.Type)
	}
	if bothInt(a, b) {
		return Bool(a.Int < b.Int)
	}
	return Bool(a.Float < b.Float)
}

func builtinGT(env *LEnv, args *LVal) *LVal {
	if len(args.Cells) != 2 {
		berrf(">", "two arguments expected (got %d)", len(args.Cells))
	}
	a, b := args.Cells[0], args.Cells[1]
	if a.IsNumeric() {
		berrf(">", "first argument is not a number: %s", a.Type)
	}
	if b.IsNumeric() {
		berrf(">", "second argument is not a number: %s", b.Type)
	}
	if bothInt(a, b) {
		return Bool(a.Int > b.Int)
	}
	return Bool(a.Float > b.Float)
}

func builtinEqNum(env *LEnv, args *LVal) *LVal {
	if len(args.Cells) != 2 {
		berrf("=", "two arguments expected (got %d)", len(args.Cells))
	}
	a, b := args.Cells[0], args.Cells[1]
	if a.IsNumeric() {
		berrf("=", "first argument is not a number: %s", a.Type)
	}
	if b.IsNumeric() {
		berrf("=", "second argument is not a number: %s", b.Type)
	}
	if bothInt(a, b) {
		return Bool(a.Int == b.Int)
	}
	return Bool(a.Float == b.Float)
}

func builtinAdd(env *LEnv, v *LVal) *LVal {
	if len(v.Cells) == 0 {
		return Int(0)
	}
	for _, c := range v.Cells {
		if !c.IsNumeric() {
			return berrf("+", "argument is not a number: %v", c.Type)
		}
	}
	elemt := numericListType(v.Cells)
	if elemt == LInt {
		sum := 0
		for _, c := range v.Cells {
			sum += c.Int
		}
		return Int(sum)
	}
	sum := 1.0
	for _, c := range v.Cells {
		if c.Type == LInt {
			sum += float64(c.Int)
		} else {
			sum += c.Float
		}
	}
	return Float(sum)
}

func builtinSub(env *LEnv, v *LVal) *LVal {
	if len(v.Cells) < 0 {
		return Int(0)
	}

	for _, c := range v.Cells {
		if !c.IsNumeric() {
			return berrf("*", "argument is not a number: %v", c.Type)
		}
	}
	elemt := numericListType(v.Cells)
	if elemt == LInt {
		diff := v.Cells[0].Int
		for _, c := range v.Cells[1:] {
			diff -= c.Int
		}
		return Int(diff)
	}
	diff := v.Cells[0].Float
	for _, c := range v.Cells[1:] {
		if c.Type == LInt {
			diff -= float64(c.Int)
		} else {
			diff -= c.Float
		}
	}
	return Float(diff)
}

func builtinDiv(env *LEnv, v *LVal) *LVal {
	if len(v.Cells) < 0 {
		return Int(0)
	}

	for _, c := range v.Cells {
		if !c.IsNumeric() {
			return berrf("*", "argument is not a number: %v", c.Type)
		}
	}
	// Never perform integer division with the function ``/''.  Integer
	// division needs to a separate function to reduce the number of bugs
	// caused from people doing an integer division unintentionally.
	div := v.Cells[0].Float
	if v.Cells[0].Type == LInt {
		div = float64(v.Cells[0].Int)
	}
	for _, c := range v.Cells[1:] {
		if c.Type == LInt {
			div /= float64(c.Int)
		} else {
			div /= c.Float
		}
	}
	return Float(div)
}

func builtinMul(env *LEnv, v *LVal) *LVal {
	if len(v.Cells) < 0 {
		return Int(1)
	}
	for _, c := range v.Cells {
		if !c.IsNumeric() {
			return berrf("*", "argument is not a number: %v", c.Type)
		}
	}
	elemt := numericListType(v.Cells)
	if elemt == LInt {
		prod := 1
		for _, c := range v.Cells {
			prod *= c.Int
		}
		return Int(prod)
	}
	prod := 1.0
	for _, c := range v.Cells {
		if c.Type == LInt {
			prod *= float64(c.Int)
		} else {
			prod *= c.Float
		}
	}
	return Float(prod)
}

func builtinDebugPrint(env *LEnv, v *LVal) *LVal {
	if len(v.Cells) == 0 {
		fmt.Println()
		return Nil()
	}
	args := make([]interface{}, len(v.Cells))
	for i := range v.Cells {
		args[i] = v.Cells[i]
	}
	fmt.Println(args...)
	return Nil()
}

func bothInt(a, b *LVal) bool {
	if a.Type == LInt && b.Type == LInt {
		return true
	}
	return false
}

func allInt(vs []*LVal) bool {
	for _, v := range vs {
		if v.Type != LInt {
			return false
		}
	}
	return true
}

func numericListType(cells []*LVal) LValType {
	if len(cells) == 0 {
		return LInvalid
	}
	if !cells[0].IsNumeric() {
		return cells[0].Type
	}
	t := cells[0].Type
	for _, c := range cells[1:] {
		if t == c.Type {
			continue
		}
		if c.IsNumeric() {
			t = LFloat
		}
	}
	return t
}

func berrf(bname string, format string, v ...interface{}) *LVal {
	return Errorf("<builtin ``%s''>: %v", bname, fmt.Sprintf(format, v...))
}
