package lisp

import (
	"fmt"
	"math"
	"os"
)

// LBuiltin is a function that performs executes a lisp function.
type LBuiltin func(env *LEnv, args *LVal) *LVal

// LBuiltinDef is a built-in function
type LBuiltinDef interface {
	Name() string
	Formals() *LVal
	Eval(env *LEnv, args *LVal) *LVal
}

type langBuiltin struct {
	name    string
	formals *LVal
	fun     LBuiltin
}

func (fun *langBuiltin) Name() string {
	return fun.name
}

func (fun *langBuiltin) Formals() *LVal {
	return fun.formals
}

func (fun *langBuiltin) Eval(env *LEnv, args *LVal) *LVal {
	return fun.fun(env, args)
}

var userBuiltins []*langBuiltin
var langBuiltins = []*langBuiltin{
	{"set", Formals("sym", "val"), builtinSet},
	{"eval", Formals("expr"), builtinEval},
	{"error", Formals(VarArgSymbol, "args"), builtinError},
	{"car", Formals("lis"), builtinCAR},
	{"cdr", Formals("lis"), builtinCDR},
	{"map", Formals("fn", "lis"), builtinMap},
	{"foldl", Formals("fn", "z", "lis"), builtinFoldLeft},
	{"foldr", Formals("fn", "z", "lis"), builtinFoldRight},
	{"compose", Formals("f", "g"), builtinCompose},
	{"unpack", Formals("f", "lis"), builtinUnpack},
	{"assoc", Formals("m", "k", "v"), builtinAssoc},
	{"assoc!", Formals("m", "k", "v"), builtinAssocMutate},
	{"get", Formals("m", "k"), builtinGet},
	{"sorted-map", Formals(VarArgSymbol, "args"), builtinSortedMap},
	{"concat", Formals(VarArgSymbol, "args"), builtinConcat},
	{"reverse", Formals("lis"), builtinReverse},
	{"list", Formals(VarArgSymbol, "args"), builtinList},
	{"length", Formals("lis"), builtinLength},
	{"cons", Formals("head", "tail"), builtinCons},
	{"not", Formals("expr"), builtinNot},
	{">=", Formals("a", "b"), builtinGEq},
	{">", Formals("a", "b"), builtinGT},
	{"<=", Formals("a", "b"), builtinLEq},
	{"<", Formals("a", "b"), builtinLT},
	{"=", Formals("a", "b"), builtinEqNum},
	{"**", Formals("a", "b"), builtinPow},
	{"%", Formals("a", "b"), builtinMod},
	{"+", Formals(VarArgSymbol, "x"), builtinAdd},
	{"-", Formals(VarArgSymbol, "x"), builtinSub},
	{"/", Formals(VarArgSymbol, "x"), builtinDiv},
	{"*", Formals(VarArgSymbol, "x"), builtinMul},
	{"debug-print", Formals(VarArgSymbol, "args"), builtinDebugPrint},
	{"debug-stack", Formals(), builtinDebugStack},
}

// RegisterDefaultBuiltin adds the given function to the list returned by
// DefaultBuiltins.
func RegisterDefaultBuiltin(name string, formals *LVal, fn LBuiltin) {
	userBuiltins = append(userBuiltins, &langBuiltin{name, formals.Copy(), fn})
}

// DefaultBuiltins returns the default set of LBuiltinDefs added to LEnv
// objects when LEnv.AddBuiltins is called without arguments.
func DefaultBuiltins() []LBuiltinDef {
	ops := make([]LBuiltinDef, len(langBuiltins)+len(userBuiltins))
	for i := range langBuiltins {
		ops[i] = langBuiltins[i]
	}
	offset := len(langBuiltins)
	for i := range userBuiltins {
		ops[offset+i] = langBuiltins[i]
	}
	return ops
}

func builtinSet(env *LEnv, v *LVal) *LVal {
	if v.Cells[0].Type != LSymbol {
		return env.Errorf("first argument is not a symbol: %v", v.Cells[0].Type)
	}

	env.PutGlobal(v.Cells[0], v.Cells[1])
	return env.GetGlobal(v.Cells[0])
}

func builtinEval(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	if v.Type == LQuote {
		return v.Cells[0]
	}
	v.Quoted = false
	return env.Eval(v)
}

func builtinError(env *LEnv, args *LVal) *LVal {
	iargs := make([]interface{}, args.Len())
	for i, arg := range args.Cells {
		iargs[i] = arg
	}
	return env.Error(iargs...)
}

func builtinCAR(env *LEnv, v *LVal) *LVal {
	if v.Cells[0].Type != LSExpr {
		return env.Errorf("argument is not a list: %v", v.Cells[0].Type)
	}
	if len(v.Cells[0].Cells) == 0 {
		// Maybe this should just return v?
		return env.Errorf("argument is empty")
	}
	return v.Cells[0].Cells[0]
}

func builtinCDR(env *LEnv, v *LVal) *LVal {
	if v.Cells[0].Type != LSExpr {
		return env.Errorf("argument is not a list %v", v.Cells[0].Type)
	}
	if len(v.Cells[0].Cells) == 0 {
		// Maybe this should just return v?
		return env.Errorf("argument is empty")
	}
	q := QExpr(v.Cells[0].Cells[1:])
	return q
}

func builtinMap(env *LEnv, args *LVal) *LVal {
	f := args.Cells[0]
	if f.Type != LFun {
		return env.Errorf("first argument is not a function: %s", f.Type)
	}
	lis := args.Cells[1]
	if lis.Type != LSExpr {
		return env.Errorf("second argument is not a list: %s", lis.Type)
	}
	for i, c := range lis.Cells {
		fargs := QExpr([]*LVal{c})
		fret := env.Call(f, fargs)
		if fret.Type == LError {
			return fret
		}
		lis.Cells[i] = fret
	}
	return lis
}

func builtinFoldLeft(env *LEnv, args *LVal) *LVal {
	f := args.Cells[0]
	if f.Type != LFun {
		return env.Errorf("first argument is not a function: %s", f.Type)
	}
	acc := args.Cells[1]
	lis := args.Cells[2]
	if lis.Type != LSExpr {
		return env.Errorf("third argument is not a list: %s", lis.Type)
	}
	for _, c := range lis.Cells {
		fargs := QExpr([]*LVal{
			// args reversed from foldr function invocation
			acc,
			c,
		})
		fret := env.Call(f, fargs)
		if fret.Type == LError {
			return fret
		}
		acc = fret
	}
	return acc
}

func builtinFoldRight(env *LEnv, args *LVal) *LVal {
	f := args.Cells[0]
	if f.Type != LFun {
		return env.Errorf("first argument is not a function: %s", f.Type)
	}
	acc := args.Cells[1]
	lis := args.Cells[2]
	if lis.Type != LSExpr {
		return env.Errorf("third argument is not a list: %s", lis.Type)
	}
	for i := len(lis.Cells) - 1; i >= 0; i-- {
		c := lis.Cells[i]
		fargs := QExpr([]*LVal{
			// args reversed from foldl function invocation
			c,
			acc,
		})
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
	f := args.Cells[0]
	g := args.Cells[1]
	if f.Type != LFun {
		return env.Errorf("first argument is not a function: %s", f.Type)
	}
	if g.Type != LFun {
		return env.Errorf("second argument is not a function: %s", g.Type)
	}
	formals := g.Cells[0].Copy()
	body := args // body.Cells[0] is already set to f
	body.Quoted = false
	gcall := SExpr(make([]*LVal, 0, len(formals.Cells)+1))
	gcall.Cells = append(gcall.Cells, g)
	var restSym *LVal
	for i, argSym := range formals.Cells {
		if argSym.Type != LSymbol {
			// This should not happen.  The list of formals should be checked
			// when the g function was created.
			return env.Errorf("invalid list of formals: %s", formals)
		}
		if argSym.Str == VarArgSymbol {
			if len(formals.Cells) != i+2 {
				// This should not happen.  The list of formals should be checked
				// when the g function was created.
				return env.Errorf("invalid list of formals: %s", formals)
			}
			restSym = formals.Cells[i+1]
			break
		}
		gcall.Cells = append(gcall.Cells, argSym)
	}
	if restSym != nil {
		concatPrefix := QExpr(gcall.Cells[1:])
		concatCall := SExpr(nil)
		concatCall.Cells = append(concatCall.Cells, Symbol("concat"))
		concatCall.Cells = append(concatCall.Cells, concatPrefix)
		concatCall.Cells = append(concatCall.Cells, restSym.Copy())
		unpackCall := SExpr(nil)
		unpackCall.Cells = append(unpackCall.Cells, Symbol("unpack"))
		unpackCall.Cells = append(unpackCall.Cells, g)
		unpackCall.Cells = append(unpackCall.Cells, concatCall)
		gcall = unpackCall
	}
	body.Cells[1] = gcall
	newfun := Lambda(formals, []*LVal{body})
	newfun.Env.Parent = env
	return newfun
}

func builtinUnpack(env *LEnv, args *LVal) *LVal {
	if args.Cells[0].Type != LFun {
		return env.Errorf("first argument is not a function: %s", args.Cells[0].Type)
	}
	if args.Cells[1].Type != LSExpr {
		return env.Errorf("second argument is not a list: %s", args.Cells[1].Type)
	}
	args.Cells = append(args.Cells[:1], args.Cells[1].Cells...)
	args.Quoted = false
	return env.Eval(args)
}

func builtinAssoc(env *LEnv, args *LVal) *LVal {
	m := args.Cells[0]
	k := args.Cells[1]
	v := args.Cells[2]
	if m.IsNil() {
		m = SortedMap()
	} else if m.Type != LSortMap {
		return env.Errorf("first argument is not a map: %s", m.Type)
	} else {
		m = m.Copy()
		m.Map = m.copyMap()
	}
	err := mapSet(m, k, v, false)
	if !err.IsNil() {
		return env.Errorf("%s", err)
	}
	return m
}

func builtinAssocMutate(env *LEnv, args *LVal) *LVal {
	m := args.Cells[0]
	k := args.Cells[1]
	v := args.Cells[2]
	if m.IsNil() {
		return env.Errorf("first argument is nil", m.Type)
	} else if m.Type != LSortMap {
		return env.Errorf("first argument is not a map: %s", m.Type)
	}
	err := mapSet(m, k, v, false)
	if !err.IsNil() {
		return env.Error(err.String())
	}
	return m
}

func builtinGet(env *LEnv, args *LVal) *LVal {
	m := args.Cells[0]
	k := args.Cells[1]
	if m.Type != LSortMap {
		return env.Errorf("first argument is not a map: %s", m.Type)
	}
	return mapGet(m, k)
}

func builtinSortedMap(env *LEnv, args *LVal) *LVal {
	m := SortedMap()
	if len(args.Cells)%2 != 0 {
		return env.Errorf("uneven number of arguments: %d", len(args.Cells))
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
	q := QExpr(nil)
	for _, c := range v.Cells {
		if c.Type != LSExpr {
			return env.Errorf("argument is not a list: %v", c.Type)
		}
		q.Cells = append(q.Cells, c.Cells...)
	}
	return q
}

func builtinReverse(env *LEnv, args *LVal) *LVal {
	if args.Cells[0].Type != LSExpr {
		return env.Errorf("first argument is not a list: %v", args.Cells[0].Type)
	}
	q := QExpr(args.Cells[0].Cells)
	for i := 0; i < len(q.Cells)-1; i++ {
		q.Cells[i], q.Cells[len(q.Cells)-1-i] = q.Cells[len(q.Cells)-1-i], q.Cells[i]
	}
	return q
}

func builtinList(env *LEnv, v *LVal) *LVal {
	return QExpr(v.Cells)
}

func builtinLength(env *LEnv, args *LVal) *LVal {
	lis := args.Cells[0]
	if lis.Type != LSExpr {
		return env.Errorf("first argument is not a list: %v", lis.Type)
	}
	return Int(lis.Len())
}

func builtinCons(env *LEnv, args *LVal) *LVal {
	if args.Cells[1].Type != LSExpr {
		return env.Errorf("second argument is not a list: %s", args.Cells[1].Type)
	}
	args.Cells = append(args.Cells[:1], args.Cells[1].Cells...)
	args.Quoted = true
	return args
}

func builtinNot(env *LEnv, v *LVal) *LVal {
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
	a, b := args.Cells[0], args.Cells[1]
	if a.IsNumeric() {
		env.Errorf("first argument is not a number: %s", a.Type)
	}
	if b.IsNumeric() {
		env.Errorf("second argument is not a number: %s", b.Type)
	}
	if bothInt(a, b) {
		return Bool(a.Int <= b.Int)
	}
	return Bool(toFloat(a) <= toFloat(b))
}

func builtinLT(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if a.IsNumeric() {
		env.Errorf("first argument is not a number: %s", a.Type)
	}
	if b.IsNumeric() {
		env.Errorf("second argument is not a number: %s", b.Type)
	}
	if bothInt(a, b) {
		return Bool(a.Int < b.Int)
	}
	return Bool(toFloat(a) < toFloat(b))
}

func builtinGEq(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if a.IsNumeric() {
		env.Errorf("first argument is not a number: %s", a.Type)
	}
	if b.IsNumeric() {
		env.Errorf("second argument is not a number: %s", b.Type)
	}
	if bothInt(a, b) {
		return Bool(a.Int >= b.Int)
	}
	return Bool(toFloat(a) >= toFloat(b))
}

func builtinGT(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if a.IsNumeric() {
		env.Errorf("first argument is not a number: %s", a.Type)
	}
	if b.IsNumeric() {
		env.Errorf("second argument is not a number: %s", b.Type)
	}
	if bothInt(a, b) {
		return Bool(a.Int > b.Int)
	}
	return Bool(toFloat(a) > toFloat(b))
}

func builtinEqNum(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if a.IsNumeric() {
		env.Errorf("first argument is not a number: %s", a.Type)
	}
	if b.IsNumeric() {
		env.Errorf("second argument is not a number: %s", b.Type)
	}
	if bothInt(a, b) {
		return Bool(a.Int == b.Int)
	}

	// This may not be correct
	return Bool(toFloat(a) == toFloat(b))
}

func builtinPow(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if a.IsNumeric() {
		env.Errorf("first argument is not a number: %s", a.Type)
	}
	if b.IsNumeric() {
		env.Errorf("second argument is not a number: %s", b.Type)
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
	a, b := args.Cells[0], args.Cells[1]
	if a.Type != LInt {
		env.Errorf("first argument is not an int: %s", a.Type)
	}
	if b.Type != LInt {
		env.Errorf("second argument is not an int: %s", b.Type)
	}
	return Int(a.Int % b.Int)
}

func builtinAdd(env *LEnv, v *LVal) *LVal {
	if len(v.Cells) == 0 {
		return Int(0)
	}
	for _, c := range v.Cells {
		if !c.IsNumeric() {
			return env.Errorf("argument is not a number: %v", c.Type)
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
			return env.Errorf("argument is not a number: %v", c.Type)
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
			return env.Errorf("argument is not a number: %v", c.Type)
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
			return env.Errorf("argument is not a number: %v", c.Type)
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
	env.Stack.DebugPrint(os.Stdout)
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
