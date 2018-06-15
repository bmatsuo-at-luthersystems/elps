package lisp

import (
	"bytes"
	"fmt"
	"math"
	"os"
	"sort"
	"strconv"
	"strings"
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
	{"load-string", Formals("source-code"), builtinLoadString},
	{"load-bytes", Formals("source-code"), builtinLoadBytes},
	{"in-package", Formals("package-name"), builtinInPackage},
	{"use-package", Formals(VarArgSymbol, "package-name"), builtinUsePackage},
	{"export", Formals(VarArgSymbol, "symbol"), builtinExport},
	{"set", Formals("sym", "val"), builtinSet},
	{"gensym", Formals(), builtinGensym},
	{"identity", Formals("value"), builtinIdentity},
	{"to-string", Formals("value"), builtinToString},
	{"to-int", Formals("value"), builtinToInt},
	{"to-float", Formals("value"), builtinToFloat},
	{"eval", Formals("expr"), builtinEval},
	{"error", Formals(VarArgSymbol, "args"), builtinError},
	{"car", Formals("lis"), builtinCAR},
	{"cdr", Formals("lis"), builtinCDR},
	{"first", Formals("seq"), builtinFirst},
	{"second", Formals("seq"), builtinSecond},
	{"nth", Formals("seq", "n"), builtinNth},
	{"map", Formals("type-specifier", "fn", "seq"), builtinMap},
	{"foldl", Formals("fn", "z", "seq"), builtinFoldLeft},
	{"foldr", Formals("fn", "z", "seq"), builtinFoldRight},
	{"compose", Formals("f", "g"), builtinCompose},
	{"unpack", Formals("f", "lis"), builtinUnpack},
	{"flip", Formals("binary-function"), builtinFlip},
	{"assoc", Formals("map", "key", "value"), builtinAssoc},
	{"assoc!", Formals("map", "key", "value"), builtinAssocMutate},
	{"get", Formals("map", "key", VarArgSymbol, "default"), builtinGet},
	{"keys", Formals("map"), builtinKeys},
	{"sorted-map", Formals(VarArgSymbol, "args"), builtinSortedMap},
	{"concat", Formals("type-specifier", VarArgSymbol, "args"), builtinConcat},
	{"insert-index", Formals("type-specifier", "seq", "index", "item"), builtinInsertIndex},
	{"sort", Formals("less-predicate", "list", VarArgSymbol, "key-fun"), builtinSort},
	{"insert-sorted", Formals("type-specifier", "list", "predicate", "item", VarArgSymbol, "key-fun"), builtinInsertSorted},
	{"search-sorted", Formals("n", "predicate"), builtinSearchSorted},
	{"select", Formals("type-specifier", "predicate", "seq"), builtinSelect},
	{"reject", Formals("type-specifier", "predicate", "seq"), builtinReject},
	{"zip", Formals("type-specifier", "list", VarArgSymbol, "lists"), builtinZip},
	{"make-sequence", Formals("start", "stop", VarArgSymbol, "step"), builtinMakeSequence},
	{"format-string", Formals("format", VarArgSymbol, "values"), builtinFormatString},
	{"reverse", Formals("type-specifier", "seq"), builtinReverse},
	{"slice", Formals("type-specifier", "seq", "start", "end"), builtinSlice},
	{"list", Formals(VarArgSymbol, "args"), builtinList},
	{"vector", Formals(VarArgSymbol, "args"), builtinVector},
	{"aref", Formals("a", VarArgSymbol, "indices"), builtinARef},
	{"length", Formals("lis"), builtinLength},
	{"cons", Formals("head", "tail"), builtinCons},
	{"not", Formals("expr"), builtinNot},
	{"nil?", Formals("expr"), builtinIsNil},
	{"list?", Formals("expr"), builtinIsList},
	{"sorted-map?", Formals("expr"), builtinIsSortedMap},
	{"array?", Formals("expr"), builtinIsArray},
	{"vector?", Formals("expr"), builtinIsVector},
	{"string?", Formals("expr"), builtinIsString},
	{"equal?", Formals("a", "b"), builtinEqual},
	{"all?", Formals("predicate", "seq"), builtinAllP},
	{"any?", Formals("predicate", "seq"), builtinAnyP},
	{"max", Formals("real", VarArgSymbol, "rest"), builtinMax},
	{"min", Formals("real", VarArgSymbol, "rest"), builtinMin},
	{"string>=", Formals("a", "b"), builtinStringGEq},
	{"string>", Formals("a", "b"), builtinStringGT},
	{"string<=", Formals("a", "b"), builtinStringLEq},
	{"string<", Formals("a", "b"), builtinStringLT},
	{"string=", Formals("a", "b"), builtinStringEq},
	{">=", Formals("a", "b"), builtinGEq},
	{">", Formals("a", "b"), builtinGT},
	{"<=", Formals("a", "b"), builtinLEq},
	{"<", Formals("a", "b"), builtinLT},
	{"=", Formals("a", "b"), builtinEqNum},
	{"pow", Formals("a", "b"), builtinPow},
	{"mod", Formals("a", "b"), builtinMod},
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

func builtinLoadString(env *LEnv, args *LVal) *LVal {
	source := args.Cells[0]
	if source.Type != LString {
		return env.Errorf("first argument is not a string: %v", source.Type)
	}
	return env.root().LoadString("load-string", source.Str)
}

func builtinLoadBytes(env *LEnv, args *LVal) *LVal {
	source := args.Cells[0]
	if source.Type != LBytes {
		return env.Errorf("first argument is not bytes: %v", source.Type)
	}
	return env.root().Load("load-bytes", bytes.NewReader(source.Bytes))
}

func builtinInPackage(env *LEnv, args *LVal) *LVal {
	if args.Cells[0].Type != LSymbol && args.Cells[0].Type != LString {
		return env.Errorf("first argument is not a symbol or a string: %v", args.Cells[0].Type)
	}
	name := args.Cells[0].Str
	root := env.root()
	pkg := root.Registry.Packages[name]
	newpkg := false
	if pkg == nil {
		newpkg = true
		root.Registry.DefinePackage(name)
		pkg = root.Registry.Packages[name]
	}
	root.Package = pkg
	if newpkg && root.Registry.Lang != "" {
		// For now, all packages use the lisp package.  The ``in-package''
		// builtin doesn't provide syntax to simply use lisp (calling
		// ``lisp:use-package'' from a package that doesn't use lisp hard to
		// remember).
		root.UsePackage(Symbol(root.Registry.Lang))
	}
	return Nil()
}

func builtinUsePackage(env *LEnv, args *LVal) *LVal {
	if args.Cells[0].Type != LSymbol && args.Cells[0].Type != LString {
		return env.Errorf("first argument is not a symbol or a string: %v", args.Cells[0].Type)
	}
	return env.UsePackage(args.Cells[0])
}

func builtinExport(env *LEnv, args *LVal) *LVal {
	for _, arg := range args.Cells {
		switch {
		case arg.Type == LSymbol || arg.Type != LString:
			env.root().Package.Exports(arg.Str)
		case arg.Type == LSExpr:
			builtinExport(env, arg)
		default:
			return env.Errorf("argument is not a symbol, a string, or a list of valid types: %v", arg.Type)
		}
	}
	return Nil()
}

func builtinSet(env *LEnv, v *LVal) *LVal {
	if v.Cells[0].Type != LSymbol {
		return env.Errorf("first argument is not a symbol: %v", v.Cells[0].Type)
	}

	env.PutGlobal(v.Cells[0], v.Cells[1])
	return env.GetGlobal(v.Cells[0])
}

func builtinGensym(env *LEnv, args *LVal) *LVal {
	return env.GenSym()
}

func builtinIdentity(env *LEnv, args *LVal) *LVal {
	return args.Cells[0]
}

func builtinToString(env *LEnv, args *LVal) *LVal {
	val := args.Cells[0]
	switch val.Type {
	case LString:
		return val
	case LSymbol:
		return String(val.Str)
	case LBytes:
		return String(string(val.Bytes))
	case LInt:
		return String(strconv.Itoa(val.Int))
	case LFloat:
		return String(strconv.FormatFloat(val.Float, 'g', -1, 64))
	default:
		return env.Errorf("cannot convert type to string: %v", val.Type)
	}
}

func builtinToInt(env *LEnv, args *LVal) *LVal {
	val := args.Cells[0]
	switch val.Type {
	case LString:
		x, err := strconv.Atoi(val.Str)
		if err != nil {
			return env.Error(err)
		}
		return Int(x)
	case LInt:
		return val
	case LFloat:
		return Int(int(val.Float))
	default:
		return env.Errorf("cannot convert type to string: %v", val.Type)
	}
}

func builtinToFloat(env *LEnv, args *LVal) *LVal {
	val := args.Cells[0]
	switch val.Type {
	case LString:
		x, err := strconv.ParseFloat(val.Str, 64)
		if err != nil {
			return env.Error(err)
		}
		return Float(x)
	case LInt:
		return Float(float64(val.Int))
	case LFloat:
		return val
	default:
		return env.Errorf("cannot convert type to string: %v", val.Type)
	}
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
		return Nil()
	}
	return v.Cells[0].Cells[0]
}

func builtinCDR(env *LEnv, v *LVal) *LVal {
	if v.Cells[0].Type != LSExpr {
		return env.Errorf("argument is not a list %v", v.Cells[0].Type)
	}
	if len(v.Cells[0].Cells) == 0 {
		return Nil()
	}
	q := QExpr(v.Cells[0].Cells[1:])
	return q
}

func builtinFirst(env *LEnv, args *LVal) *LVal {
	list := args.Cells[0]
	if !isSeq(list) {
		return env.Errorf("argument is not a proper sequence: %v", list.Type)
	}
	cells := seqCells(list)
	if len(cells) == 0 {
		return Nil()
	}
	return cells[0]
}

func builtinSecond(env *LEnv, args *LVal) *LVal {
	list := args.Cells[0]
	if !isSeq(list) {
		return env.Errorf("argument is not a proper sequence: %v", list.Type)
	}
	cells := seqCells(list)
	if len(cells) < 2 {
		return Nil()
	}
	return cells[1]
}

func builtinNth(env *LEnv, args *LVal) *LVal {
	list, n := args.Cells[0], args.Cells[1]
	if !isSeq(list) {
		return env.Errorf("first argument is not a proper sequence: %v", list.Type)
	}
	if n.Type != LInt {
		return env.Errorf("second argument is not an integer: %v", n.Type)
	}
	cells := seqCells(list)
	if len(cells) < n.Int {
		return Nil()
	}
	return cells[n.Int]
}

func builtinMap(env *LEnv, args *LVal) *LVal {
	typespec, f, lis := args.Cells[0], args.Cells[1], args.Cells[2]
	nilReturn := false
	if typespec.IsNil() {
		nilReturn = true
	} else if typespec.Type != LSymbol {
		return env.Errorf("first argument is not a valid type specification: %v", typespec.Type)
	}
	if f.Type != LFun {
		return env.Errorf("second argument is not a function: %s", f.Type)
	}
	if !isSeq(lis) {
		return env.Errorf("third argument is not a proper sequence: %s", lis.Type)
	}
	var v *LVal
	var cells []*LVal
	if nilReturn {
		v = Nil()
	} else {
		switch typespec.Str {
		case "vector":
			v = Array(QExpr([]*LVal{Int(lis.Len())}), nil)
			cells = v.Cells[1:]
		case "list":
			cells = make([]*LVal, lis.Len())
			v = QExpr(cells)
		default:
			return env.Errorf("type specifier is invalid: %v", typespec)
		}
	}
	for i, c := range seqCells(lis) {
		fargs := QExpr([]*LVal{c})
		fret := env.Call(f, fargs)
		if fret.Type == LError {
			return fret
		}
		if !nilReturn {
			cells[i] = fret
		}
	}
	return v
}

func builtinFoldLeft(env *LEnv, args *LVal) *LVal {
	f := args.Cells[0]
	if f.Type != LFun {
		return env.Errorf("first argument is not a function: %s", f.Type)
	}
	acc := args.Cells[1]
	lis := args.Cells[2]
	if !isSeq(lis) {
		return env.Errorf("third argument is not a proper sequence: %s", lis.Type)
	}
	for _, c := range seqCells(lis) {
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
	if !isSeq(lis) {
		return env.Errorf("third argument is not a proper sequence: %s", lis.Type)
	}
	cells := seqCells(lis)
	for i := len(cells) - 1; i >= 0; i-- {
		c := cells[i]
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
		concatCall.Cells = append(concatCall.Cells, Symbol("lisp:concat"))
		concatCall.Cells = append(concatCall.Cells, Quote(Symbol("list")))
		concatCall.Cells = append(concatCall.Cells, concatPrefix)
		concatCall.Cells = append(concatCall.Cells, restSym.Copy())
		unpackCall := SExpr(nil)
		unpackCall.Cells = append(unpackCall.Cells, Symbol("lisp:unpack"))
		unpackCall.Cells = append(unpackCall.Cells, g)
		unpackCall.Cells = append(unpackCall.Cells, concatCall)
		gcall = unpackCall
	}
	body.Cells[1] = gcall
	newfun := Lambda(formals, []*LVal{body})
	newfun.Env.Parent = env
	newfun.Package = env.root().Package.Name
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

func builtinFlip(env *LEnv, args *LVal) *LVal {
	fn := args.Cells[0]
	if fn.Type != LFun {
		return env.Errorf("argument is not a function: %s", fn.Type)
	}
	formals := fn.Cells[0]
	if len(formals.Cells) < 2 && formals.Cells[0].Str != VarArgSymbol {
		// TODO:  Check if varargs
		return env.Errorf("argument is not a function of two arguments: %s", formals)
	}
	call := SExpr([]*LVal{fn, Symbol("y"), Symbol("x")})
	return env.Lambda(Formals("x", "y"), []*LVal{call})
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
	m, k, _def := args.Cells[0], args.Cells[1], args.Cells[2:]
	if m.Type != LSortMap {
		return env.Errorf("first argument is not a map: %s", m.Type)
	}
	if len(_def) > 1 {
		return env.Errorf("too many arguments provided")
	}
	var def *LVal
	if len(_def) > 0 {
		def = _def[0]
	}
	return mapGet(m, k, def)
}

func builtinKeys(env *LEnv, args *LVal) *LVal {
	m := args.Cells[0]
	if m.Type != LSortMap {
		return env.Errorf("first argument is not a map: %s", m.Type)
	}
	return m.MapKeys()
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

func builtinConcat(env *LEnv, args *LVal) *LVal {
	typespec, rest := args.Cells[0], args.Cells[1:]
	if typespec.Type != LSymbol {
		return env.Errorf("first argument is not a valid type specification: %v", typespec.Type)
	}
	size := 0
	for _, v := range rest {
		if !isSeq(v) {
			return env.Errorf("argument is not a proper sequence: %v", v.Type)
		}
		size += v.Len()
	}
	var ret *LVal
	var cells []*LVal
	switch typespec.Str {
	case "vector":
		ret = Array(QExpr([]*LVal{Int(size)}), nil)
		cells = seqCells(ret)
		cells = cells[0:0:size]
	case "list":
		ret = QExpr(make([]*LVal, size))
		cells = ret.Cells[0:0:size]
	default:
		return env.Errorf("type specifier is not valid: %v", typespec)
	}
	for _, v := range rest {
		cells = append(cells, seqCells(v)...)
	}
	return ret
}

func builtinSort(env *LEnv, args *LVal) *LVal {
	less, list, optArgs := args.Cells[0], args.Cells[1], args.Cells[2:]
	var keyFun *LVal
	if less.Type != LFun {
		return env.Errorf("first argument is not a function: %v", less.Type)
	}
	if list.Type != LSExpr {
		return env.Errorf("second arument is not a list: %v", list.Type)
	}
	if len(optArgs) > 1 {
		return env.Errorf("too many optional arguments provided")
	}
	if len(optArgs) > 0 {
		keyFun = optArgs[0]
		if keyFun.Type != LFun {
			return env.Errorf("third argument is not a function: %v", keyFun.Type)
		}
	}
	sortErr := Nil()
	sort.Slice(list.Cells, func(i, j int) bool {
		if !sortErr.IsNil() {
			return false
		}
		a, b := list.Cells[i], list.Cells[j]
		// Functions are always copied when being invoked. But the arguments
		// are not copied in general.
		var expr *LVal
		if keyFun == nil {
			expr = SExpr([]*LVal{less, a.Copy(), b.Copy()})
		} else {
			expr = SExpr([]*LVal{less,
				SExpr([]*LVal{keyFun, a.Copy()}),
				SExpr([]*LVal{keyFun, b.Copy()}),
			})
		}
		ok := env.Eval(expr)
		if ok.Type == LError {
			sortErr = ok
			return false
		}
		return True(ok)
	})
	if !sortErr.IsNil() {
		return sortErr
	}
	return list
}

func builtinInsertIndex(env *LEnv, args *LVal) *LVal {
	typespec, list, index, item := args.Cells[0], args.Cells[1], args.Cells[2], args.Cells[3]
	if typespec.Type != LSymbol {
		return env.Errorf("first argument is not a valid type specifier: %v", typespec.Type)
	}
	if !isSeq(list) {
		return env.Errorf("second argument is not a proper sequence: %v", list.Type)
	}
	if index.Type != LInt {
		return env.Errorf("third arument is not a integer: %v", index.Type)
	}
	if index.Int > list.Len() {
		return env.Errorf("index out of bounds")
	}
	var v *LVal
	var cells []*LVal
	switch typespec.Str {
	case "vector":
		v = Array(QExpr([]*LVal{Int(1 + list.Len())}), nil)
		cells = v.Cells[1:]
	case "list":
		cells = make([]*LVal, 1+list.Len())
		v = QExpr(cells)
	default:
		return env.Errorf("type specifier is invalid: %v", typespec)
	}
	inCells := seqCells(list)
	copy(cells[:index.Int], inCells[:index.Int])
	copy(cells[index.Int+1:], inCells[index.Int:])
	cells[index.Int] = item
	return v
}

func builtinInsertSorted(env *LEnv, args *LVal) *LVal {
	typespec, list, p, item, optArgs := args.Cells[0], args.Cells[1], args.Cells[2], args.Cells[3], args.Cells[4:]
	var keyFun *LVal
	if typespec.Type != LSymbol {
		return env.Errorf("first argument is not a valid type specifier: %v", typespec.Type)
	}
	if !isSeq(list) {
		return env.Errorf("second argument is not a proper sequence: %v", list.Type)
	}
	if p.Type != LFun {
		return env.Errorf("third arument is not a function: %v", p.Type)
	}
	if len(optArgs) > 1 {
		return env.Errorf("too many optional arguments provided")
	}
	if len(optArgs) > 0 {
		keyFun = optArgs[0]
		if keyFun.Type != LFun {
			return env.Errorf("last argument is not a function: %v", keyFun.Type)
		}
	}
	sortErr := Nil()
	inCells := seqCells(list)
	i := sort.Search(len(inCells), func(i int) bool {
		var expr *LVal
		if keyFun == nil {
			expr = SExpr([]*LVal{p, item.Copy(), inCells[i].Copy()})
		} else {
			expr = SExpr([]*LVal{
				p,
				SExpr([]*LVal{
					keyFun,
					item.Copy(),
				}),
				SExpr([]*LVal{
					keyFun,
					inCells[i].Copy(),
				}),
			})
		}
		ok := env.Eval(expr)
		if ok.Type == LError {
			sortErr = ok
			return false
		}
		return True(ok)
	})
	if !sortErr.IsNil() {
		return sortErr
	}
	var v *LVal
	var cells []*LVal
	switch typespec.Str {
	case "vector":
		v = Array(QExpr([]*LVal{Int(1 + list.Len())}), nil)
		cells = v.Cells[1:]
	case "list":
		cells = make([]*LVal, 1+list.Len())
		v = QExpr(cells)
	default:
		return env.Errorf("type specifier is invalid: %v", typespec)
	}
	copy(cells[:i], inCells[:i])
	copy(cells[i+1:], inCells[i:])
	cells[i] = item
	return v
}

func builtinSearchSorted(env *LEnv, args *LVal) *LVal {
	n, p := args.Cells[0], args.Cells[1]
	if n.Type != LInt {
		return env.Errorf("first argument is not an integer: %v", n.Type)
	}
	if p.Type != LFun {
		return env.Errorf("second arument is not a function: %v", p.Type)
	}
	sortErr := Nil()
	i := sort.Search(n.Int, func(i int) bool {
		expr := SExpr([]*LVal{p, Int(i)})
		ok := env.Eval(expr)
		if ok.Type == LError {
			sortErr = ok
			return false
		}
		return True(ok)
	})
	if !sortErr.IsNil() {
		return sortErr
	}
	return Int(i)
}

func builtinSelect(env *LEnv, args *LVal) *LVal {
	typespec, pred, list := args.Cells[0], args.Cells[1], args.Cells[2]
	if typespec.Type != LSymbol {
		return env.Errorf("first argument is not a valid type specifier: %v", typespec.Type)
	}
	if pred.Type != LFun {
		return env.Errorf("second argument is not a function: %v", pred.Type)
	}
	if !isSeq(list) {
		return env.Errorf("third argument is not a proper sequence: %v", list.Type)
	}
	var v *LVal
	var cells []*LVal
	switch typespec.Str {
	case "vector":
		v = Array(QExpr([]*LVal{Int(list.Len())}), nil)
		cells = v.Cells[1:]
		cells = cells[0:0:list.Len()]
	case "list":
		v = QExpr(nil)
	default:
		return env.Errorf("type specifier is invalid: %v", typespec)
	}
	for _, v := range seqCells(list) {
		expr := SExpr([]*LVal{pred, v})
		ok := env.Eval(expr)
		if ok.Type == LError {
			return ok
		}
		if True(ok) {
			cells = append(cells, v)
		}
	}
	switch typespec.Str {
	case "list":
		v.Cells = cells
	case "vector":
		v.Cells[0].Cells[0].Int = len(cells)
		v.Cells = v.Cells[:1+len(cells)]
	}
	return v
}

func builtinReject(env *LEnv, args *LVal) *LVal {
	typespec, pred, list := args.Cells[0], args.Cells[1], args.Cells[2]
	if typespec.Type != LSymbol {
		return env.Errorf("first argument is not a valid type specifier: %v", typespec.Type)
	}
	if pred.Type != LFun {
		return env.Errorf("second argument is not a function: %v", pred.Type)
	}
	if !isSeq(list) {
		return env.Errorf("third argument is not a proper sequence: %v", list.Type)
	}
	var v *LVal
	var cells []*LVal
	switch typespec.Str {
	case "vector":
		v = Array(QExpr([]*LVal{Int(list.Len())}), nil)
		cells = v.Cells[1:]
		cells = cells[0:0:list.Len()]
	case "list":
		v = QExpr(nil)
	default:
		return env.Errorf("type specifier is invalid: %v", typespec)
	}
	for _, v := range seqCells(list) {
		expr := SExpr([]*LVal{pred, v})
		ok := env.Eval(expr)
		if ok.Type == LError {
			return ok
		}
		if !True(ok) {
			cells = append(cells, v)
		}
	}
	switch typespec.Str {
	case "list":
		v.Cells = cells
	case "vector":
		v.Cells[0].Cells[0].Int = len(cells)
		v.Cells = v.Cells[:1+len(cells)]
	}
	return v
}

func builtinZip(env *LEnv, args *LVal) *LVal {
	typespec, lists := args.Cells[0], args.Cells[1:]
	if typespec.Type != LSymbol {
		return env.Errorf("first argument is not a valid type specifier: %v", typespec.Type)
	}
	n := 0
	for _, list := range lists {
		if !isSeq(list) {
			return env.Errorf("argument is not a proper list: %v", list.Type)
		}
		m := list.Len()
		if n == 0 || m < n {
			n = m
		}
	}
	var v *LVal
	var cells []*LVal
	switch typespec.Str {
	case "vector":
		v = Array(QExpr([]*LVal{Int(n)}), nil)
		cells = v.Cells[1 : 1+n]
	case "list":
		cells = make([]*LVal, n)
		v = QExpr(cells)
	default:
		return env.Errorf("type specifier is invalid: %v", typespec)
	}
	for i := range cells {
		var elem *LVal
		var elemCells []*LVal
		switch typespec.Str {
		case "vector":
			elem = Array(QExpr([]*LVal{Int(len(lists))}), nil)
			elemCells = elem.Cells[1 : 1+len(lists)]
		case "list":
			elemCells = make([]*LVal, len(lists))
			elem = QExpr(elemCells)
		default:
			return env.Errorf("type specifier is invalid: %v", typespec)
		}
		for j, list := range lists {
			elemCells[j] = seqCells(list)[i]
		}
		cells[i] = elem
	}
	return v
}

func builtinMakeSequence(env *LEnv, args *LVal) *LVal {
	start, stop := args.Cells[0], args.Cells[1]
	if !start.IsNumeric() {
		return env.Errorf("first argument is not numeric: %v", start.Type)
	}
	if !stop.IsNumeric() {
		return env.Errorf("second argument is not numeric: %v", stop.Type)
	}
	var step *LVal
	if len(args.Cells) == 2 {
		if start.Type == LInt {
			step = Int(1)
		} else {
			step = Float(1.0)
		}
	} else {
		if len(args.Cells) > 3 {
			return env.Errorf("too many arguments provided")
		}
		step = args.Cells[2]
		if !step.IsNumeric() {
			return env.Errorf("third argument is not numeric: %v", step.Type)
		}
		if lessNumeric(step, Float(0)) {
			return env.Errorf("third argument is negative")
		}
	}
	list := QExpr(nil)
	for x := start; lessNumeric(x, stop); x = addNumeric(x, step) {
		list.Cells = append(list.Cells, x.Copy())
	}
	return list
}

func builtinReverse(env *LEnv, args *LVal) *LVal {
	typespec, list := args.Cells[0], args.Cells[1]
	if typespec.Type != LSymbol {
		return env.Errorf("first argument is not a valid type specifier: %v", typespec.Type)
	}
	if !isSeq(list) {
		return env.Errorf("first argument is not a proper sequence: %v", args.Cells[0].Type)
	}
	var v *LVal
	var cells []*LVal
	switch typespec.Str {
	case "vector":
		v = Array(QExpr([]*LVal{Int(list.Len())}), nil)
		cells = v.Cells[1 : 1+list.Len()]
	case "list":
		cells = make([]*LVal, list.Len())
		v = QExpr(cells)
	default:
		return env.Errorf("type specifier is invalid: %v", typespec)
	}
	for i, v := range seqCells(list) {
		cells[len(cells)-1-i] = v
	}
	return v
}

func builtinSlice(env *LEnv, args *LVal) *LVal {
	typespec, list, start, end := args.Cells[0], args.Cells[1], args.Cells[2], args.Cells[3]
	if typespec.Type != LSymbol {
		return env.Errorf("first argument is not a valid type specifier: %v", typespec.Type)
	}
	if !isSeq(list) {
		return env.Errorf("second argument is not a proper sequence: %v", list.Type)
	}
	if start.Type != LInt {
		return env.Errorf("third argument is not an integer: %v", start.Type)
	}
	if end.Type != LInt {
		return env.Errorf("forth argument is not an integer: %v", end.Type)
	}
	n := list.Len()
	i := start.Int
	j := end.Int
	if i < 0 {
		return env.Errorf("index out of range")
	}
	if i > n {
		return env.Errorf("index out of range")
	}
	if j < 0 {
		return env.Errorf("index out of range")
	}
	if j > n {
		return env.Errorf("index out of range")
	}
	if i > j {
		return env.Errorf("end before start")
	}
	switch typespec.Str {
	case "list":
		return QExpr(seqCells(list)[i:j])
	case "vector":
		cells := seqCells(list)[i:j]
		return Array(QExpr([]*LVal{Int(len(cells))}), cells)
	default:
		return env.Errorf("type specifier is not valid: %v", typespec)
	}
}

func builtinList(env *LEnv, v *LVal) *LVal {
	return QExpr(v.Cells)
}

func builtinVector(env *LEnv, args *LVal) *LVal {
	return Array(nil, args.Cells)
}

func builtinARef(env *LEnv, args *LVal) *LVal {
	array, indices := args.Cells[0], args.Cells[1:]
	if array.Type != LArray {
		return env.Errorf("first argument is not an array: %v", array.Type)
	}
	v := array.ArrayIndex(indices...)
	if v.Type == LError {
		return env.Error(v)
	}
	return v
}

func builtinLength(env *LEnv, args *LVal) *LVal {
	seq := args.Cells[0]
	n := seq.Len()
	if n < 0 {
		return env.Errorf("first argument is not a list, bytes, or a string: %v", seq.Type)
	}
	return Int(n)
}

func builtinCons(env *LEnv, args *LVal) *LVal {
	if args.Cells[1].Type != LSExpr {
		return env.Errorf("second argument is not a list: %s", args.Cells[1].Type)
	}
	args.Cells = append(args.Cells[:1], args.Cells[1].Cells...)
	args.Quoted = true
	return args
}

func builtinNot(env *LEnv, args *LVal) *LVal {
	return Bool(Not(args.Cells[0]))
}

func builtinIsNil(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	if v.IsNil() {
		return Bool(true)
	}
	return Bool(false)
}

func builtinIsList(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	return Bool(v.Type == LSExpr)
}

func builtinIsSortedMap(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	return Bool(v.Type == LSortMap)
}

func builtinIsArray(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	return Bool(v.Type == LArray)
}

func builtinIsVector(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	return Bool(v.Type == LArray && v.Cells[0].Len() == 1)
}

func builtinIsString(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	return Bool(v.Type == LString)
}

func builtinEqual(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	return a.Equal(b)
}

func builtinAllP(env *LEnv, args *LVal) *LVal {
	pred, list := args.Cells[0], args.Cells[1]
	if pred.Type != LFun {
		return env.Errorf("first argument is not a function: %v", pred.Type)
	}
	if !isSeq(list) {
		return env.Errorf("second argument is not a proper sequence: %v", list.Type)
	}
	for _, v := range seqCells(list) {
		expr := SExpr([]*LVal{pred, v})
		ok := env.Eval(expr)
		if ok.Type == LError {
			return ok
		}
		if !True(ok) {
			return Bool(false)
		}
	}
	return Bool(true)
}

func builtinAnyP(env *LEnv, args *LVal) *LVal {
	pred, list := args.Cells[0], args.Cells[1]
	if pred.Type != LFun {
		return env.Errorf("first argument is not a function: %v", pred.Type)
	}
	if !isSeq(list) {
		return env.Errorf("second argument is not a list: %v", list.Type)
	}
	for _, v := range seqCells(list) {
		expr := SExpr([]*LVal{pred, v})
		ok := env.Eval(expr)
		if ok.Type == LError {
			return ok
		}
		if True(ok) {
			return ok
		}
	}
	return Bool(false)
}

func builtinMax(env *LEnv, args *LVal) *LVal {
	max := args.Cells[0]
	if !max.IsNumeric() {
		return env.Errorf("argument is not a number: %s", max.Type)
	}
	for _, x := range args.Cells[1:] {
		if !x.IsNumeric() {
			return env.Errorf("argument is not a number: %s", x.Type)
		}
		if lessNumeric(max, x) {
			max = x
		}
	}
	return max
}

func builtinMin(env *LEnv, args *LVal) *LVal {
	min := args.Cells[0]
	if !min.IsNumeric() {
		return env.Errorf("argument is not a number: %s", min.Type)
	}
	for _, x := range args.Cells[1:] {
		if !x.IsNumeric() {
			return env.Errorf("argument is not a number: %s", x.Type)
		}
		if lessNumeric(x, min) {
			min = x
		}
	}
	return min
}

func builtinStringLEq(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if a.Type != LString {
		return env.Errorf("first argument is not a string: %s", a.Type)
	}
	if b.Type != LString {
		return env.Errorf("second argument is not a string: %s", b.Type)
	}
	return Bool(a.Str <= b.Str)
}

func builtinStringLT(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if a.Type != LString {
		return env.Errorf("first argument is not a string: %s", a.Type)
	}
	if b.Type != LString {
		return env.Errorf("second argument is not a string: %s", b.Type)
	}
	return Bool(a.Str < b.Str)
}

func builtinStringGEq(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if a.Type != LString {
		return env.Errorf("first argument is not a string: %s", a.Type)
	}
	if b.Type != LString {
		return env.Errorf("second argument is not a string: %s", b.Type)
	}
	return Bool(a.Str >= b.Str)
}

func builtinStringGT(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if a.Type != LString {
		return env.Errorf("first argument is not a string: %s", a.Type)
	}
	if b.Type != LString {
		return env.Errorf("second argument is not a string: %s", b.Type)
	}
	return Bool(a.Str > b.Str)
}

func builtinStringEq(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if a.Type != LString {
		return env.Errorf("first argument is not a string: %s", a.Type)
	}
	if b.Type != LString {
		return env.Errorf("second argument is not a string: %s", b.Type)
	}
	return Bool(a.Str == b.Str)
}

func builtinLEq(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if !a.IsNumeric() {
		return env.Errorf("first argument is not a number: %s", a.Type)
	}
	if !b.IsNumeric() {
		return env.Errorf("second argument is not a number: %s", b.Type)
	}
	if bothInt(a, b) {
		return Bool(a.Int <= b.Int)
	}
	return Bool(toFloat(a) <= toFloat(b))
}

func builtinLT(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if !a.IsNumeric() {
		return env.Errorf("first argument is not a number: %s", a.Type)
	}
	if !b.IsNumeric() {
		return env.Errorf("second argument is not a number: %s", b.Type)
	}
	if bothInt(a, b) {
		return Bool(a.Int < b.Int)
	}
	return Bool(toFloat(a) < toFloat(b))
}

func builtinGEq(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if !a.IsNumeric() {
		return env.Errorf("first argument is not a number: %s", a.Type)
	}
	if !b.IsNumeric() {
		return env.Errorf("second argument is not a number: %s", b.Type)
	}
	if bothInt(a, b) {
		return Bool(a.Int >= b.Int)
	}
	return Bool(toFloat(a) >= toFloat(b))
}

func builtinGT(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if !a.IsNumeric() {
		return env.Errorf("first argument is not a number: %s", a.Type)
	}
	if !b.IsNumeric() {
		return env.Errorf("second argument is not a number: %s", b.Type)
	}
	if bothInt(a, b) {
		return Bool(a.Int > b.Int)
	}
	return Bool(toFloat(a) > toFloat(b))
}

func builtinEqNum(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if !a.IsNumeric() {
		return env.Errorf("first argument is not a number: %s", a.Type)
	}
	if !b.IsNumeric() {
		return env.Errorf("second argument is not a number: %s", b.Type)
	}
	return a.equalNum(b)
}

func builtinPow(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if !a.IsNumeric() {
		return env.Errorf("first argument is not a number: %s", a.Type)
	}
	if !b.IsNumeric() {
		return env.Errorf("second argument is not a number: %s", b.Type)
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
	for _, c := range v.Cells {
		if !c.IsNumeric() {
			return env.Errorf("argument is not a number: %v", c.Type)
		}
	}

	if len(v.Cells) == 0 {
		return Int(0)
	}

	if len(v.Cells) == 1 {
		v.Cells[0].Int = -v.Cells[0].Int
		v.Cells[0].Float = -v.Cells[0].Float
		return v.Cells[0]
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

	for _, c := range v.Cells {
		if !c.IsNumeric() {
			return env.Errorf("argument is not a number: %v", c.Type)
		}
	}

	if len(v.Cells) == 1 {
		if v.Cells[0].Type == LInt {
			return Float(1 / float64(v.Cells[0].Int))
		}
		v.Cells[0].Float = 1 / v.Cells[0].Float
		return v.Cells[0]
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

func addNumeric(a, b *LVal) *LVal {
	if bothInt(a, b) {
		return Int(a.Int + b.Int)
	}
	return Float(toFloat(a) + toFloat(b))
}

func lessNumeric(a, b *LVal) bool {
	if bothInt(a, b) {
		return a.Int < b.Int
	}
	return toFloat(a) < toFloat(b)
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

func builtinFormatString(env *LEnv, args *LVal) *LVal {
	format := args.Cells[0]
	fvals := args.Cells[1:]
	if format.Type != LString {
		return env.Errorf("first argument is not a string")
	}
	parts, err := parseFormatString(format.Str)
	if err != nil {
		return env.Error(err)
	}
	var buf bytes.Buffer
	anonIndex := 0
	for _, p := range parts {
		if strings.HasPrefix(p, "{") && strings.HasSuffix(p, "}") {
			p = strings.Join(strings.Fields(p), "")
			// TODO:  Allow non-empty formatting directives
			if p != "{}" {
				return env.Errorf("formatting direcives must be empty")
			}
			if anonIndex >= len(fvals) {
				return env.Errorf("too many formatting direcives for supplied values")
			}
			val := fvals[anonIndex]
			if val.Type == LString {
				buf.WriteString(val.Str)
			} else {
				buf.WriteString(val.String())
			}
			anonIndex++
		} else {
			buf.WriteString(p)
		}
	}

	return String(buf.String())
}

func parseFormatString(f string) ([]string, error) {
	var s []string
	tokens := tokenizeFormatString(f)
	for len(tokens) > 0 {
		tok := tokens[0]
		if tok.typ == formatText {
			s = append(s, tok.text)
			tokens = tokens[1:]
			continue
		}
		if tok.typ == formatClose {
			if len(tokens) == 0 || tokens[0].typ != formatClose {
				return nil, fmt.Errorf("unexpected closing brace '}' outside of formatting direcive")
			}
			s = append(s, "}")
			tokens = tokens[2:]
		}
		if len(tokens) < 2 {
			return nil, fmt.Errorf("unclosed formatting directive")
		}
		switch tokens[1].typ {
		case formatOpen:
			s = append(s, "{")
			tokens = tokens[2:]
			continue
		case formatClose:
			s = append(s, "{}")
			tokens = tokens[2:]
			continue
		case formatText:
			if len(tokens) < 3 {
				return nil, fmt.Errorf("unclosed formatting directive")
			}
			if tokens[2].typ != formatClose {
				return nil, fmt.Errorf("invalid formatting directive")
			}
			s = append(s, "{"+tokens[1].text+"}")
			tokens = tokens[3:]
			continue
		default:
			panic("unknown type")
		}
	}
	return s, nil
}

func tokenizeFormatString(f string) []formatToken {
	var tokens []formatToken
	for {
		i := strings.IndexAny(f, "{}")
		if i < 0 {
			tokens = append(tokens, formatToken{formatText, f})
			return tokens
		}
		if i > 0 {
			tokens = append(tokens, formatToken{formatText, f[:i]})
			f = f[i:]
		}
		if f[0] == '{' {
			tokens = append(tokens, formatToken{formatOpen, "{"})
			f = f[1:]
		} else {
			tokens = append(tokens, formatToken{formatClose, "}"})
			f = f[1:]
		}
	}
}

type formatTokenType uint

const (
	formatText formatTokenType = iota
	formatOpen
	formatClose
)

type formatToken struct {
	typ  formatTokenType
	text string
}
