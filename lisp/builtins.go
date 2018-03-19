package lisp

import (
	"fmt"
	"math"
)

// LBuiltin is a function that performs executes a lisp function.
type LBuiltin func(env *LEnv, args *LVal) *LVal

// LBuiltinDef is a built-in function
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
	{"eval", builtinEval},
	{"car", builtinCAR},
	{"cdr", builtinCDR},
	{"map", builtinMap},
	{"foldl", builtinFoldLeft},
	{"foldr", builtinFoldRight},
	{"compose", builtinCompose},
	{"unpack", builtinUnpack},
	{"assoc!", builtinAssocMutate},
	{"get", builtinGet},
	{"sorted-map", builtinSortedMap},
	{"concat", builtinConcat},
	{"reverse", builtinReverse},
	{"list", builtinList},
	{"cons", builtinCons},
	{"not", builtinNot},
	{">=", builtinGEq},
	{">", builtinGT},
	{"<=", builtinLEq},
	{"<", builtinLT},
	{"=", builtinEqNum},
	{"**", builtinPow},
	{"%", builtinMod},
	{"+", builtinAdd},
	{"-", builtinSub},
	{"/", builtinDiv},
	{"*", builtinMul},
	{"debug-print", builtinDebugPrint},
	{"debug-stack", builtinDebugStack},
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

// NOTE: Compose requires concat and unpack in order to work with varargs.
func builtinCompose(env *LEnv, args *LVal) *LVal {
	if len(args.Cells) != 2 {
		return berrf("compose", "two argument expected (got %d)", len(args.Cells))
	}
	f := args.Cells[0]
	g := args.Cells[1]
	if f.Type != LFun {
		return berrf("compose", "first argument is not a function: %s", f.Type)
	}
	if g.Type != LFun {
		return berrf("compose", "second argument is not a function: %s", g.Type)
	}
	formals := g.Formals.Copy()
	body := args // body.Cells[0] is already set to f
	gcall := SExpr()
	gcall.Cells = make([]*LVal, 0, len(formals.Cells)+1)
	gcall.Cells = append(gcall.Cells, g)
	var restSym *LVal
	for i, argSym := range formals.Cells {
		if argSym.Type != LSymbol {
			// This should not happen.  The list of formals should be checked
			// when the g function was created.
			return berrf("compose", "invalid list of formals: %s", formals)
		}
		if argSym.Str == "&" {
			if len(formals.Cells) != i+2 {
				// This should not happen.  The list of formals should be checked
				// when the g function was created.
				return berrf("compose", "invalid list of formals: %s", formals)
			}
			restSym = formals.Cells[i+1]
			break
		}
		gcall.Cells = append(gcall.Cells, argSym)
	}
	if restSym != nil {
		concatPrefix := QExpr()
		concatPrefix.Cells = gcall.Cells[1:]
		concatCall := SExpr()
		concatCall.Cells = append(concatCall.Cells, Symbol("concat"))
		concatCall.Cells = append(concatCall.Cells, concatPrefix)
		concatCall.Cells = append(concatCall.Cells, restSym.Copy())
		unpackCall := SExpr()
		unpackCall.Cells = append(unpackCall.Cells, Symbol("unpack"))
		unpackCall.Cells = append(unpackCall.Cells, g)
		unpackCall.Cells = append(unpackCall.Cells, concatCall)
		gcall = unpackCall
	}
	body.Cells[1] = gcall
	newfun := Lambda(formals, body)
	newfun.Env.Parent = env
	return newfun
}

func builtinUnpack(env *LEnv, args *LVal) *LVal {
	if len(args.Cells) != 2 {
		return berrf("unpack", "two argument expected (got %d)", len(args.Cells))
	}
	if args.Cells[0].Type != LFun {
		return berrf("unpack", "first argument is not a function: %s", args.Cells[0].Type)
	}
	if args.Cells[1].Type != LSExpr {
		return berrf("unpack", "second argument is not a list: %s", args.Cells[1].Type)
	}
	args.Cells = append(args.Cells[:1], args.Cells[1].Cells...)
	return env.Eval(args)
}

func builtinAssocMutate(env *LEnv, args *LVal) *LVal {
	if len(args.Cells) != 3 {
		return berrf("assoc!", "three arguments expected (got %d)", len(args.Cells))
	}
	m := args.Cells[0]
	k := args.Cells[1]
	v := args.Cells[2]
	if m.IsNil() {
		m = SortedMap()
	} else if m.Type != LSortMap {
		return berrf("assoc!", "first argument is not a map: %s", m.Type)
	}
	err := mapSet(m, k, v, false)
	if !err.IsNil() {
		return berrf("assoc!", "%s", err)
	}
	return m
}

func builtinGet(env *LEnv, args *LVal) *LVal {
	if len(args.Cells) != 2 {
		return berrf("get", "two arguments expected (got %d)", len(args.Cells))
	}
	m := args.Cells[0]
	k := args.Cells[1]
	if m.Type != LSortMap {
		return berrf("get", "first argument is not a map: %s", m.Type)
	}
	return mapGet(m, k)
}

func builtinSortedMap(env *LEnv, args *LVal) *LVal {
	m := SortedMap()
	if len(args.Cells)%2 != 0 {
		return berrf("sorted-map", "uneven number of arguments: %d", len(args.Cells))
	}
	for len(args.Cells) >= 2 {
		k := args.Cells[0]
		v := args.Cells[1]
		err := mapSet(m, k, v, false)
		if !err.IsNil() {
			return err
		}
		args.Cells = args.Cells[2:]
	}
	return m
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

func builtinReverse(env *LEnv, args *LVal) *LVal {
	if len(args.Cells) != 1 {
		return berrf("reverse", "one argument expected (got %d)", len(args.Cells))
	}
	if args.Cells[0].Type != LSExpr {
		return berrf("reverse", "first argument is not a list: %v", args.Cells[0].Type)
	}
	q := QExpr()
	q.Cells = args.Cells[0].Cells
	for i := 0; i < len(q.Cells)-1; i++ {
		q.Cells[i], q.Cells[len(q.Cells)-1-i] = q.Cells[len(q.Cells)-1-i], q.Cells[i]
	}
	return q
}

func builtinList(env *LEnv, v *LVal) *LVal {
	q := QExpr()
	q.Cells = v.Cells
	return q
}

func builtinCons(env *LEnv, args *LVal) *LVal {
	if len(args.Cells) != 2 {
		return berrf("cons", "two arguments expected (got %d)", len(args.Cells))
	}
	if args.Cells[1].Type != LSExpr {
		return berrf("cons", "second argument is not a list: %s", args.Cells[1].Type)
	}
	args.Cells = append(args.Cells[:1], args.Cells[1].Cells...)
	args.Quoted = true
	return args
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

func builtinLEq(env *LEnv, args *LVal) *LVal {
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
		return Bool(a.Int <= b.Int)
	}
	return Bool(toFloat(a) <= toFloat(b))
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
	return Bool(toFloat(a) < toFloat(b))
}

func builtinGEq(env *LEnv, args *LVal) *LVal {
	if len(args.Cells) != 2 {
		berrf(">=", "two arguments expected (got %d)", len(args.Cells))
	}
	a, b := args.Cells[0], args.Cells[1]
	if a.IsNumeric() {
		berrf(">=", "first argument is not a number: %s", a.Type)
	}
	if b.IsNumeric() {
		berrf(">=", "second argument is not a number: %s", b.Type)
	}
	if bothInt(a, b) {
		return Bool(a.Int >= b.Int)
	}
	return Bool(toFloat(a) >= toFloat(b))
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
	return Bool(toFloat(a) > toFloat(b))
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

	// This may not be correct
	return Bool(toFloat(a) == toFloat(b))
}

func builtinPow(env *LEnv, args *LVal) *LVal {
	if len(args.Cells) != 2 {
		berrf("**", "two arguments expected (got %d)", len(args.Cells))
	}
	a, b := args.Cells[0], args.Cells[1]
	if a.IsNumeric() {
		berrf("**", "first argument is not a number: %s", a.Type)
	}
	if b.IsNumeric() {
		berrf("**", "second argument is not a number: %s", b.Type)
	}
	if bothInt(a, b) {
		return powInt(a.Int, b.Int)
	}
	return Float(math.Pow(toFloat(a), toFloat(b)))
}

func powInt(a, b int) *LVal {
	if b == 0 {
		return Int(1)
	}
	if b < 0 {
		return Float(math.Pow(float64(a), float64(b)))
	}
	n := 1
	atob := a
	for 2*n < b {
		atob *= atob
		n *= 2
	}
	for n < b {
		atob *= a
		n++
	}
	return Int(atob)
}

func builtinMod(env *LEnv, args *LVal) *LVal {
	if len(args.Cells) != 2 {
		berrf("%", "two arguments expected (got %d)", len(args.Cells))
	}
	a, b := args.Cells[0], args.Cells[1]
	if a.Type != LInt {
		berrf("%", "first argument is not an int: %s", a.Type)
	}
	if b.Type != LInt {
		berrf("%", "second argument is not an int: %s", b.Type)
	}
	return Int(a.Int % b.Int)
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
	sum := 0.0
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
	if len(v.Cells) == 0 {
		return Int(0)
	}
	if len(v.Cells) == 1 {
		v.Cells[0].Int = -v.Cells[0].Int
		v.Cells[0].Float = -v.Cells[0].Float
		return v.Cells[0]
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
	if len(v.Cells) == 0 {
		return Int(1)
	}
	if len(v.Cells) == 1 {
		if v.Cells[0].Type == LInt {
			return Float(1 / float64(v.Cells[0].Int))
		}
		v.Cells[0].Float = 1 / v.Cells[0].Float
		return v.Cells[0]
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
	if len(v.Cells) == 0 {
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

func builtinDebugPrint(env *LEnv, args *LVal) *LVal {
	if len(args.Cells) == 0 {
		fmt.Println()
		return Nil()
	}
	fmtargs := make([]interface{}, len(args.Cells))
	for i := range args.Cells {
		fmtargs[i] = args.Cells[i]
	}
	fmt.Println(fmtargs...)
	return Nil()
}

func builtinDebugStack(env *LEnv, args *LVal) *LVal {
	if len(args.Cells) != 0 {
		return berrf("debug-stack", "no arguments expected (got %d)", len(args.Cells))
	}
	env.Stack.DebugPrint()
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

func numericListType(cells []*LVal) LType {
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

func toFloat(x *LVal) float64 {
	if !x.IsNumeric() {
		panic("toFloat called with non-numeric argument: " + x.String())
	}
	if x.Type == LInt {
		return float64(x.Int)
	}
	return x.Float
}

func berrf(bname string, format string, v ...interface{}) *LVal {
	return Errorf("<builtin ``%s''>: %v", bname, fmt.Sprintf(format, v...))
}
