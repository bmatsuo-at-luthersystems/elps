package lisp

import (
	"fmt"
	"io"
	"log"
	"os"
	"strings"
	"sync/atomic"

	"bitbucket.org/luthersystems/elps/parser/token"
)

// DefaultLangPackage is the name of default language package
const DefaultLangPackage = "lisp"

// DefaultUserPackage is the name of the entry point package for interpreting
// user code.
const DefaultUserPackage = "user"

var envCount uint64

func getEnvID() uint {
	return uint(atomic.AddUint64(&envCount, 1))
}

var symCount uint64

func gensym() uint {
	return uint(atomic.AddUint64(&symCount, 1))
}

// InitializeUserEnv creates the default user environment.
func InitializeUserEnv(env *LEnv) *LVal {
	env.Runtime.Registry.DefinePackage(DefaultLangPackage)
	env.Runtime.Registry.Lang = DefaultLangPackage
	env.Runtime.Package = env.Runtime.Registry.Packages[env.Runtime.Registry.Lang]
	env.AddMacros(true)
	env.AddSpecialOps(true)
	env.AddBuiltins(true)
	env.Runtime.Registry.DefinePackage(DefaultUserPackage)
	rc := env.InPackage(Symbol(DefaultUserPackage))
	if GoError(rc) != nil {
		return rc
	}
	return env.UsePackage(Symbol(env.Runtime.Registry.Lang))
}

// LEnv is a lisp environment.
type LEnv struct {
	ID      uint
	Loc     *token.Location
	Scope   map[string]*LVal
	FunName map[string]string
	Parent  *LEnv
	Runtime *Runtime
}

// NewEnvRuntime initializes a new LEnv, like NewEnv, but it explicitly
// specifies the runtime to use.  NewEnvRuntime is only suitable for creating
// root LEnv object, so it does not take a parent argument.  When rt is nil
// StandardRuntime() called to create a new Runtime for the returned LEnv.  It
// is an error to use the same runtime object in multiple calls to
// NewEnvRuntime if the two envs are not in the same tree and doing so will
// have unspecified results.
func NewEnvRuntime(rt *Runtime) *LEnv {
	if rt == nil {
		rt = StandardRuntime()
	}
	env := &LEnv{
		ID:      rt.GenEnvID(),
		Scope:   make(map[string]*LVal),
		FunName: make(map[string]string),
		Runtime: rt,
	}
	return env

}

// NewEnv returns initializes and returns a new LEnv.
func NewEnv(parent *LEnv) *LEnv {
	var runtime *Runtime
	if parent != nil {
		runtime = parent.Runtime
	} else {
		runtime = StandardRuntime()
	}
	env := &LEnv{
		ID:      runtime.GenEnvID(),
		Scope:   make(map[string]*LVal),
		FunName: make(map[string]string),
		Parent:  parent,
		Runtime: runtime,
	}
	return env
}

func (env *LEnv) getFID() string {
	return fmt.Sprintf("_fun%d", env.ID)
}

func (env *LEnv) GenSym() *LVal {
	return Symbol(env.Runtime.GenSym())
}

func (env *LEnv) DefinePackage(name *LVal) *LVal {
	if name.Type != LSymbol && name.Type != LString {
		return env.Errorf("argument cannot be converted to string: %v", name.Type)
	}
	env.Runtime.Registry.DefinePackage(name.Str)
	return Nil()
}

func (env *LEnv) InPackage(name *LVal) *LVal {
	if name.Type != LSymbol && name.Type != LString {
		return env.Errorf("argument cannot be converted to string: %v", name.Type)
	}
	pkg := env.Runtime.Registry.Packages[name.Str]
	if pkg == nil {
		return env.Errorf("unknown package: %v", name.Str)
	}
	env.Runtime.Package = pkg
	return Nil()
}

func (env *LEnv) UsePackage(name *LVal) *LVal {
	if name.Type != LSymbol && name.Type != LString {
		return env.Errorf("argument cannot be converted to string: %v", name.Type)
	}
	pkg := env.Runtime.Registry.Packages[name.Str]
	if pkg == nil {
		return env.Errorf("unknown package: %v", name.Str)
	}
	for _, sym := range pkg.Externals {
		v := pkg.Get(Symbol(sym))
		if v.Type == LError {
			return env.Errorf("package %s: %v", name.Str, v)
		}
		env.Runtime.Package.Put(Symbol(sym), v)
	}
	return Nil()
}

func (env *LEnv) LoadString(name, exprs string) *LVal {
	return env.Load(name, strings.NewReader(exprs))
}

// Load reads LVals from r and evaluates them as if in a progn.  The value
// returned by the last evaluated LVal will be retured.  If env.Reader has not
// been set then an error will be returned.
func (env *LEnv) Load(name string, r io.Reader) *LVal {
	if env.Runtime.Reader == nil {
		return env.Errorf("no reader for environment runtime")
	}
	exprs, err := env.Runtime.Reader.Read(name, r)
	if err != nil {
		return env.Error(err)
	}
	if len(exprs) == 0 {
		return Nil()
	}
	ret := Nil()
	for _, expr := range exprs {
		ret = env.Eval(expr)
		if ret.Type == LError {
			return ret
		}
	}
	return ret
}

// Copy returns a new LEnv with a copy of env.Scope but a shared parent and
// stack (not quite a deep copy).
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
	v := env.get(k)
	if v.Type == LFun {
		// Set the function's name here in case the same function is defined
		// with multiple names.  We want to try and use the name the programmer
		// used.  The name may even come from a higher scope.
		env.FunName[v.FID] = k.Str
	}
	return v
}

// GetFun returns a function referenced by the given LVal.  If fun is already
// an LFun, then fun is returned.  If fun is a symbol then GetFun looks for a
// function bound to the symbol.  If fun does reference a symbol then an error
// is returned.
//
// GetFun is a suitable for backing an implementation of functional programing
// constructs, like funcall, map, reduce, etc.
func (env *LEnv) GetFun(fun *LVal) *LVal {
	if fun.Type == LSymbol {
		f := env.Get(fun)
		if f.Type == LError {
			return f
		}
		if f.Type != LFun {
			return env.Errorf("symbol %s not bound to a function: %v", fun, f.Type)
		}
		return f
	} else if fun.Type != LFun {
		return env.Errorf("first argument is not a function: %v", fun.Type)
	}
	return fun
}

func (env *LEnv) get(k *LVal) *LVal {
	// LQSymbols are allowed...
	if k.Type != LSymbol && k.Type != LQSymbol {
		return Nil()
	}
	if k.Str == TrueSymbol {
		return Symbol(TrueSymbol)
	}
	if k.Str == FalseSymbol {
		return Symbol(FalseSymbol)
	}
	pieces := strings.Split(k.Str, ":")
	switch len(pieces) {
	case 1:
		break
	case 2:
		if pieces[0] == "" {
			// keyword
			return k.Copy()
		}
		pkg := env.Runtime.Registry.Packages[pieces[0]]
		if pkg == nil {
			return env.Errorf("unknown package: %q", pieces[0])
		}
		lerr := pkg.Get(Symbol(pieces[1]))
		if lerr.Type == LError {
			lerr.Stack = env.Runtime.Stack.Copy()
		}
		return lerr
	default:
		return env.Errorf("illegal symbol: %q", k.Str)
	}
	v, ok := env.Scope[k.Str]
	if ok {
		if v.Type == LFun {
			// Set the function's name here in case the same function is
			// defined with multiple names.  We want to try and use the name
			// the programmer used.
			env.FunName[v.FID] = k.Str
		}
		return v.Copy()
	}
	if env.Parent != nil {
		return env.Parent.Get(k)
	}
	return env.packageGet(k)
}

func (env *LEnv) packageGet(k *LVal) *LVal {
	lerr := env.Runtime.Package.Get(k)
	if lerr.Type == LError {
		lerr.Stack = env.Runtime.Stack.Copy()
	}
	return lerr
}

// GetFunName returns the function name (if any) known to be bound to the given
// FID.
func (env *LEnv) GetFunName(f *LVal) string {
	pkgname := f.Package
	if pkgname == "" {
		log.Printf("unknown package for function %s", f.FID)
		return ""
	}
	pkg := env.Runtime.Registry.Packages[pkgname]
	if pkg == nil {
		log.Printf("failed to find package %q", pkgname)
		return ""
	}
	return pkg.FunNames[f.FID]
}

// Put takes an LSymbol k and binds it to v in env.
func (env *LEnv) Put(k, v *LVal) {
	if k.Type != LSymbol && k.Type != LQSymbol {
		return
	}
	if k.Str == TrueSymbol {
		panic("constant value")
	}
	if k.Str == FalseSymbol {
		panic("constant value")
	}
	if v == nil {
		panic("nil value")
	}
	if v.Type == LFun {
		env.FunName[v.FID] = k.Str
	}
	env.Scope[k.Str] = v.Copy()
}

// GetGlobal takes LSymbol k and returns the value it is bound to in the
// current package.
func (env *LEnv) GetGlobal(k *LVal) *LVal {
	return env.Runtime.Package.Get(k)
}

// PutGlobal takes an LSymbol k and binds it to v in current package.
func (env *LEnv) PutGlobal(k, v *LVal) {
	env.Runtime.Package.Put(k, v)
}

// Lambda returns a new Lambda with fun.Env and fun.Package set automatically.
func (env *LEnv) Lambda(formals *LVal, body []*LVal) *LVal {
	if formals.Type != LSExpr {
		return env.Errorf("formals is not a list of symbols: %v", formals.Type)
	}
	cells := make([]*LVal, 0, len(body)+1)
	cells = append(cells, formals)
	cells = append(cells, body...)
	fenv := NewEnv(env)
	fun := &LVal{
		Type:  LFun,
		Env:   fenv,
		FID:   fenv.getFID(),
		Cells: cells,
	}
	fun.Package = env.Runtime.Package.Name
	return fun
}

func (env *LEnv) root() *LEnv {
	for env.Parent != nil {
		env = env.Parent
	}
	return env
}

// AddMacros binds the given macros to their names in env.  When called with no
// arguments AddMacros adds the DefaultMacros to env.
func (env *LEnv) AddMacros(external bool, macs ...LBuiltinDef) {
	if len(macs) == 0 {
		macs = DefaultMacros()
	}
	pkg := env.Runtime.Package
	for _, mac := range macs {
		k := Symbol(mac.Name())
		exist := pkg.Get(k)
		if !exist.IsNil() && exist.Type != LError { // LError is ubound symbol
			panic(fmt.Sprintf("macro already defined: %v (= %v)", k, exist))
		}
		id := fmt.Sprintf("<builtin-macro ``%s''>", mac.Name())
		fn := Macro(id, mac.Formals(), mac.Eval)
		fn.Package = pkg.Name
		pkg.Put(k, fn)
		if external {
			pkg.Externals = append(pkg.Externals, k.Str)
		}
	}
}

// AddSpecialOps binds the given special operators to their names in env.  When
// called with no arguments AddSpecialOps adds the DefaultSpecialOps to env.
func (env *LEnv) AddSpecialOps(external bool, ops ...LBuiltinDef) {
	if len(ops) == 0 {
		ops = DefaultSpecialOps()
	}
	pkg := env.Runtime.Package
	for _, op := range ops {
		k := Symbol(op.Name())
		exist := pkg.Get(k)
		if !exist.IsNil() && exist.Type != LError { // LError is ubound symbol
			panic(fmt.Sprintf("macro already defined: %v (= %v)", k, exist))
		}
		id := fmt.Sprintf("<special-op ``%s''>", op.Name())
		fn := SpecialOp(id, op.Formals(), op.Eval)
		fn.Package = pkg.Name
		pkg.Put(k, fn)
		if external {
			pkg.Externals = append(pkg.Externals, k.Str)
		}
	}
}

// AddBuiltins binds the given funs to their names in env.  When called with no
// arguments AddBuiltins adds the DefaultBuiltins to env.
func (env *LEnv) AddBuiltins(external bool, funs ...LBuiltinDef) {
	if len(funs) == 0 {
		funs = DefaultBuiltins()
	}
	pkg := env.Runtime.Package
	for _, f := range funs {
		k := Symbol(f.Name())
		exist := pkg.Get(k)
		if exist.Type != LError {
			panic("symbol already defined: " + f.Name())
		}
		id := fmt.Sprintf("<builtin-function ``%s''>", f.Name())
		v := Fun(id, f.Formals(), f.Eval)
		v.Package = pkg.Name
		pkg.Put(k, v)
		if external {
			pkg.Externals = append(pkg.Externals, k.Str)
		}
	}
}

// Error returns an LError value with an error message given by rendering msg.
//
// Error may be called either with an error or with any number of *LVal values.
// It is invalid to pass an error argument with any other values and doing so
// will result in a runtime panic.
//
// Unlike the exported function, the Error method returns LVal with a copy
// env.Runtime.Stack.
func (env *LEnv) Error(msg ...interface{}) *LVal {
	return env.ErrorCondition("error", msg...)
}

// ErrorCondition returns an LError the given condition type and an error
// message computed by rendering msg.
//
// ErrorCondition may be called either with an error or with any number of
// *LVal values.  It is invalid to pass ErrorCondition an error argument with
// any other values and doing so will result in a runtime panic.
//
// Unlike the exported function, the ErrorCondition method returns an LVal with
// a copy env.Runtime.Stack.
func (env *LEnv) ErrorCondition(condition string, v ...interface{}) *LVal {
	//log.Printf("stack %v", env.Runtime.Stack.Copy())

	narg := len(v)
	cells := make([]*LVal, 0, len(v))
	for _, v := range v {
		switch v := v.(type) {
		case *LVal:
			cells = append(cells, v)
		case error:
			if narg > 1 {
				panic("invalid error argument")
			}
			return &LVal{
				Type:  LError,
				Str:   condition,
				Stack: env.Runtime.Stack.Copy(),
				Cells: []*LVal{Native(v)},
			}
		case string:
			cells = append(cells, String(v))
		default:
			cells = append(cells, Native(v))
		}
	}
	return &LVal{
		Type:   LError,
		Source: env.Loc,
		Str:    condition,
		Stack:  env.Runtime.Stack.Copy(),
		Cells:  cells,
	}
}

// Errorf returns an LError value with a formatted error message.
//
// Unlike the exported function, the Errorf method returns an LVal with a copy
// env.Runtime.Stack.
func (env *LEnv) Errorf(format string, v ...interface{}) *LVal {
	return env.ErrorConditionf("error", format, v...)
}

// ErrorConditionf returns an LError value with the given condition type and a
// a formatted error message rendered using fmt.Sprintf.
//
// Unlike the exported function, the ErrorConditionf method returns an LVal
// with a copy env.Runtime.Stack.
func (env *LEnv) ErrorConditionf(condition string, format string, v ...interface{}) *LVal {
	return &LVal{
		Source: env.Loc,
		Type:   LError,
		Str:    condition,
		Stack:  env.Runtime.Stack.Copy(),
		Cells:  []*LVal{String(fmt.Sprintf(format, v...))},
	}
}

// Eval evaluates v in the context (scope) of env and returns the resulting
// LVal.
//
// NOTE:  Eval shouldn't unquote v during evaluation -- a difference between
// Eval and the ``eval'' builtin function, but it does.  For some reason macros
// won't work without this unquoting.
func (env *LEnv) Eval(v *LVal) *LVal {
eval:
	if v.Spliced {
		return env.Errorf("spliced value used as expression")
	}
	env.Loc = v.Source
	// Builtins signal that they are executing their final expression (which
	// will be returned verbatim) by evaluating an LVal marked as terminal.
	// When this happens we want to mark the stack as terminal so tail
	// recursion optimization may seek past the frame at top of the stack (and
	// clear the LVal's terminal field for good measure).  We don't clear the
	// frame's Terminal flag when v.Terminal is false because a terminal
	// expression likely needs to have args evaluated (for which a TROBlock
	// prevents recursion optimization).  And the arguments to the terminal
	// expression will not themselves be terminal expressions.
	if v.Terminal {
		v.Terminal = false
		top := env.Runtime.Stack.Top()
		if top != nil {
			top.Terminal = true
		}
	}
	if v.Quoted {
		return v
	}
	switch v.Type {
	case LSymbol:
		pieces := strings.Split(v.Str, ":")
		switch len(pieces) {
		case 1:
			return env.Get(v)
		case 2:
			if pieces[0] == "" {
				return v
			}
			pkg := env.root().Runtime.Registry.Packages[pieces[0]]
			if pkg == nil {
				return env.Errorf("unknown package: %q", pieces[0])
			}
			lerr := pkg.Get(Symbol(pieces[1]))
			if lerr.Type == LError {
				lerr.Stack = env.Runtime.Stack.Copy()
			}
			return lerr
		default:
			return env.Errorf("illegal symbol: %q", v.Str)
		}
	case LSExpr:
		res := env.EvalSExpr(v)
		if res.Type == LMarkMacExpand {
			// A macro was just expanded and returned an unevaluated
			// expression.  We have to evaluate the result before we return.
			v = res.Cells[0]
			goto eval
		}
		if res.Type == LError {
			if res.Source == nil {
				res.Source = env.Loc
			}
			if res.Stack == nil {
				res.Stack = env.Runtime.Stack.Copy()
			}
		}
		return res
	case LQuote:
		// this quote was unquoted... eval the underlying value
		v = v.Cells[0]
		goto eval
	default:
		return v
	}
}

// EvalSExpr evaluates s and returns the resulting LVal.
func (env *LEnv) EvalSExpr(s *LVal) *LVal {
	if s.Type != LSExpr {
		return env.Errorf("not an s-expression")
	}
	if len(s.Cells) == 0 {
		return Nil()
	}
	call := env.evalSExprCells(s)
	if call.Type == LError {
		if call.Stack == nil {
			call.Stack = env.Runtime.Stack.Copy()
		}
		return call
	}
	fun := call.Cells[0] // call is not an empty expression -- fun is known LFun
	args := call
	args.Cells = args.Cells[1:]

	switch fun.FunType {
	case LFunNone:
		return env.FunCall(fun, args)
	case LFunSpecialOp:
		return env.SpecialOpCall(fun, args)
	case LFunMacro:
		return env.MacroCall(fun, args)
	default:
		panic(fmt.Sprintf("invalid function type %#v", fun.FunType))
	}
}

// MacroCall invokes macro fun with argument list args.
func (env *LEnv) MacroCall(fun, args *LVal) *LVal {
	if fun.Type != LFun {
		return env.Errorf("not a special function: %v", fun.Type)
	}
	if !fun.IsMacro() {
		return env.Errorf("not a special function: %v", fun.FunType)
	}

	// Push a frame onto the stack to represent the function's execution.
	env.Runtime.Stack.PushFID(env.Loc, fun.FID, fun.Package, env.GetFunName(fun))
	defer env.Runtime.Stack.Pop()
	// Macros can't participate in tail-recursion optimization at all.  Enable
	// the TROBlock on the stack fram so TerminalFID never seeks past the
	// macro's callsite.
	env.Runtime.Stack.Top().TROBlock = true

	r := env.call(fun, args, false)
	if r == nil {
		env.Runtime.Stack.DebugPrint(os.Stderr)
		panic("nil LVal returned from function call")
	}
	if r.Type == LError {
		return r
	}

	// NOTE:  There should be no need to check for LMarkTailRec objects because
	// we block all tail-recursion for macro calls.

	// This is a lazy unquote.  Unquoting in this way appears to allow the
	// upcoming evaluation to produce the correct value for user defined
	// macros, which are typically using quasiquote.  Builtin macros can be
	// massaged to return a proper value.  I'm sure there is a bug where
	// something is unintentionally unquoted.  I will deal with
	// implementing a proper system for special operators at that point.
	r.Quoted = false
	return markMacExpand(r)
}

// SpecialOpCall invokes special operator fun with the argument list args.
func (env *LEnv) SpecialOpCall(fun, args *LVal) *LVal {
	if fun.Type != LFun {
		return env.Errorf("not a special function: %v", fun.Type)
	}
	if !fun.IsSpecialOp() {
		return env.Errorf("not a special function: %v", fun.FunType)
	}

	// Push a frame onto the stack to represent the function's execution.
	env.Runtime.Stack.PushFID(env.Loc, fun.FID, fun.Package, env.GetFunName(fun))
	defer env.Runtime.Stack.Pop()

	// Special functions in general cannot be candidates for tail-recursion
	// optimization because they receive unevaluated arguments.  As such,
	// unwinding the stack would put them at risk of losing bindings to a
	// symbol which still has yet to be evaluated (after normal functions would
	// have all of their argument evaled already).  Furthermore, special
	// operators like ``let'' define a lexical scope which cannot be collapsed
	// by tail-recursion-optimization.

callf:
	r := env.call(fun, args, false)
	if r == nil {
		env.Runtime.Stack.DebugPrint(os.Stderr)
		panic("nil LVal returned from function call")
	}
	if r.Type == LError {
		return r
	}

	if r.Type == LMarkTailRec {
		// Tail recursion optimization is occurring.
		if decrementMarkTailRec(r) {
			fun, args = extractMarkTailRec(r)
			goto callf
		}
		return r
	}

	return r
}

func (env *LEnv) FunCall(fun, args *LVal) *LVal {
	return env.funCall(fun, args, true)
}

// FunCall invokes regular function fun with the argument list args.
func (env *LEnv) funCall(fun, args *LVal, curry bool) *LVal {
	if fun.Type != LFun {
		return env.Errorf("not a function: %v", fun.Type)
	}
	if fun.IsSpecialFun() {
		return env.Errorf("not a regular function: %v", fun.FunType)
	}

	// Check for possible tail recursion before pushing to avoid hitting s when
	// checking.  But push FID onto the stack before popping to simplify
	// book-keeping.
	var npop int
	npop = env.Runtime.Stack.TerminalFID(fun.FID)

	// Push a frame onto the stack to represent the function's execution.
	env.Runtime.Stack.PushFID(env.Loc, fun.FID, fun.Package, env.GetFunName(fun))
	defer env.Runtime.Stack.Pop()

	if npop > 0 {
		return markTailRec(npop, fun, args)
	}

callf:
	r := env.call(fun, args, curry)
	if r == nil {
		env.Runtime.Stack.DebugPrint(os.Stderr)
		panic("nil LVal returned from function call")
	}
	if r.Type == LError {
		return r
	}

	if r.Type == LMarkTailRec {
		// Tail recursion optimization is occurring.
		done := decrementMarkTailRec(r)
		if done {
			fun, args = extractMarkTailRec(r)
			goto callf
		}
	}

	return r
}

func extractMarkTailRec(mark *LVal) (fun, args *LVal) {
	return mark.Cells[1], mark.Cells[2]
}

// Decrement the tail recursion counter until it indicates 0 additional
// stack frames should be popped.  When that happens we can jump into the
// next call.
//
// mark must be LMarkTailRec
func decrementMarkTailRec(mark *LVal) (done bool) {
	if len(mark.Cells) != 3 {
		panic("invalid mark")
	}
	mark.Cells[0].Int--
	if mark.Cells[0].Int <= 0 {
		return true
	}
	return false
}

func (env *LEnv) evalSExprCells(s *LVal) *LVal {
	loc := env.Loc
	defer func() { env.Loc = loc }()

	cells := s.Cells
	newCells := make([]*LVal, 1, len(s.Cells))
	if env.Runtime.Stack.Top() != nil {
		// Avoid tail recursion during argument evaluation by temporarily
		// resetting Terminal.  We don't want to push anything on the stack
		// here because that would causes improper error messages/stack-dumps
		// if an error is encountered while evaluating the arguments to a
		// function.
		if env.Runtime.Stack.Top().Terminal {
			env.Runtime.Stack.Top().Terminal = false
			defer func() { env.Runtime.Stack.Top().Terminal = true }()
		}
	}
	f := env.Eval(cells[0])
	cells = cells[1:]
	if f.Type == LError {
		return f
	}
	if f.Type != LFun {
		return env.Errorf("first element of expression is not a function: %v", f)
	}
	if f.Type == LMarkTailRec {
		env.Runtime.Stack.DebugPrint(os.Stderr)
		log.Panicf("tail-recursion optimization attempted during argument evaluation: %v", f.Cells)
	}

	newCells[0] = f
	if f.IsSpecialFun() {
		// Arguments to a macro are not evaluated but they aren't quoted
		// either.  This behavior is what allows ``unquote'' to properly
		// resolve macro argument symbols during and still produce valid code
		// during macro expansion.  That is, if x is a macro argument then what
		// do the following expressions return?
		//		(quasiquote (unquote x))             	  => {expression bound to x}
		//		(quasiquote (unquote '(if 1 '(1) '(2))))  => '(1)
		// If the value given to x was quoted by eval then ``unquote'' would
		// have to undo that quoting.  But unquote is not supposed to unquote
		// the value returned by (if 1 '(1) '(2)), it merely evaluates the
		// expression and produces '(1).
		newCells = append(newCells, cells...)
		return SExpr(newCells)
	}
	// Evaluate arguments before invoking f.
	for _, expr := range cells {
		newCells = append(newCells, env.Eval(expr))
	}
	for _, v := range newCells {
		if v.Type == LError {
			return v
		}
		if v.Type == LMarkTailRec {
			env.Runtime.Stack.DebugPrint(os.Stderr)
			log.Panicf("tail-recursion optimization attempted during argument evaluation: %v", v.Cells)
		}
	}
	return SExpr(newCells)
}

// call invokes LFun fun with the list args.  In general it is not safe to call
// env.call bacause the stack must be setup for tail recursion optimization.
func (env *LEnv) call(fun *LVal, args *LVal, curry bool) *LVal {
	// FIXME:  A shallow copy is probably correct here.(?)  We don't want to
	// copy the Env so that updates to the global scope are reflected.
	fun = fun.Copy()
	result := env.bindFormals(fun, args, curry)
	if result.Type == LError {
		return result
	}
	if result.Type == LFun {
		return result
	}
	if !result.IsNil() {
		panic("unexpected result of binding formal arguments")
	}

	// NOTE:  The book's suggestion of chaining env here seems like dynamic
	// scoping.

	if fun.Builtin != nil {
		// TODO:  It might be cool to implement a special value (like
		// LTerminal) which a builtin could return.  If the LEnv encounters
		// LTerminal values returned from functions then they immediately mark
		// the top terminal and evaluate before popping the stack (but saving a
		// Go stack frame).

		// FIXME:  I think fun.Env is probably correct here.  But it wouldn't
		// surprise me if at least one builtin breaks when it switches.
		return fun.Builtin(env, QExpr(fun.Cells[1:]))
	}

	// With formal arguments bound, we can switch into the function's package
	// namespace for the duration of the call.
	//
	// BUG:  This package-swap should occur for builtins as well but there is a
	// bootstrapping problem, where ``set'' (as well as defun/defmacro) needs
	// to modify the *package* namespace and not the "lisp" namespace.  Dynamic
	// variables may be required in order to work through this completely.
	outer := env.Runtime.Package
	if outer.Name != fun.Package {
		inner := env.Runtime.Registry.Packages[fun.Package]
		if inner != nil {
			env.Runtime.Package = inner
			defer func() {
				env.Runtime.Package = outer
			}()
		}
	}

	body := fun.Cells[1:]
	if len(body) == 0 {
		return Nil()
	}
	if !fun.IsMacro() {
		body[len(body)-1].Terminal = true
	}
	var ret *LVal
	for i := range body {
		ret = fun.Env.Eval(body[i])
		if ret.Type == LError {
			return ret
		}
	}
	return ret
}

// bindFormalArguments may return an error, if an error was encountered during
// the binding process, a function if required arguments remain to be bound, or
// nil if all argument symbols were bound to values.
//
// If bindFormals returns nil any argument symbols not bound to values in args
// will be bound to a nil value.
//
// NOTE:  The process of binding formals is currently a destructive one which
// modifies fun.  Typically, before calling bindFormals fun should be copied to
// ensure that the function may be called again in the future.
func (env *LEnv) bindFormals(fun, args *LVal, curry bool) *LVal {
	narg := len(args.Cells)
	putArg := func(k, v *LVal) {
		fun.Env.Put(k, v)
	}
	putVarArg := func(k *LVal, v *LVal) {
		fun.Env.Put(k, v)
	}
	if fun.Env == nil {
		// FIXME?: Builtins don't have lexical envs.  We just store the args in
		// the cells for builtin functions.  A side effect of this is that
		// bindFormalNext is required to make put() calls in the order args are
		// defined.
		putArg = func(k, v *LVal) {
			fun.Cells = append(fun.Cells, v)
		}
		putVarArg = func(k *LVal, v *LVal) {
			fun.Cells = append(fun.Cells, v.Cells...)
		}
	}
	formals := fun.Cells[0]
	nformal := len(formals.Cells)
	for len(formals.Cells) != 0 {
		ret := env.bindFormalNext(fun, formals, args, putArg, putVarArg)
		if ret.Type == LError {
			return ret
		}
		if ret.Type == LFun {
			if curry {
				return ret
			}
			return env.Errorf("invalid number of arguments: %v", narg)
		}
		if !ret.IsNil() {
			panic("unexpected formal binding state")
		}
		if len(formals.Cells) == nformal {
			panic("no progress binding")
		}
		nformal = len(formals.Cells)
	}
	if len(args.Cells) > 0 {
		return env.Errorf("invalid number of arguments: %d", narg)
	}
	return Nil()
}

type bindfunc func(k, v *LVal)

func (env *LEnv) bindFormalNext(fun, formals, args *LVal, put, putVarArgs bindfunc) *LVal {
	argSym := formals.Cells[0]
	switch {
	case argSym.Str == KeyArgSymbol:
		if len(formals.Cells) < 2 {
			return env.Errorf("function formal argument list contains a control symbol at an invalid location: %v", argSym.Str)
		}
		keyCells := formals.Cells[1:]
		keymap := make(map[string]*LVal)
		var keys []string
		if len(args.Cells)%2 != 0 {
			return env.Errorf("function called with an odd number of keyword arguments")
		}
		for i := 0; i < len(args.Cells); i += 2 {
			key := args.Cells[i]
			val := args.Cells[i+1]
			if key.Type != LSymbol {
				return env.Errorf("argument is not a keyword: %v", key.Type)
			}
			if !isKeyword(key.Str) {
				return env.Errorf("argument is not a keyword: %v", key.Str)
			}
			keymap[key.Str[1:]] = val
			keys = append(keys, key.Str[1:])
		}
		for _, key := range keyCells {
			if strings.HasPrefix(key.Str, MetaArgPrefix) {
				return env.Errorf("function formal argument list contains a control symbol at an invalid location: %v", argSym.Str)
			}
			val, ok := keymap[key.Str]
			if !ok {
				put(key, Nil())
				continue
			}
			delete(keymap, key.Str)
			put(key, val)
		}
		if len(keymap) > 0 {
			// Scan through keys in the order they were given to provide a
			// logical, deterministic error message.
			for _, k := range keys {
				_, ok := keymap[k]
				if ok {
					return env.Errorf("unrecognized keyword argument: %v", k)
				}
			}
		}
		formals.Cells = nil
		args.Cells = nil
		return Nil()
	case argSym.Str == OptArgSymbol:
		if len(formals.Cells) < 2 {
			return env.Errorf("function formal argument list contains a control symbol at an invalid location: %v", argSym.Str)
		}
		if strings.HasPrefix(formals.Cells[1].Str, MetaArgPrefix) {
			// The next formal is a control symbol so we want to just pop the
			// OptArgSymbol off of formals and return without binding anything.
			// We will deal with the control symbol in the next call.
			formals.Cells = formals.Cells[1:]
			return Nil()
		}
		argSym := formals.Cells[1]
		if len(formals.Cells) == 2 {
			formals.Cells = nil
		} else {
			s := formals.Cells[0] // OptArgSymbol
			formals.Cells = formals.Cells[1:]
			formals.Cells[0] = s
		}
		if len(args.Cells) == 0 {
			// No arguments left so we bind the optional arg to nil.
			put(argSym, Nil())
			return Nil()
		}
		put(argSym, args.Cells[0])
		args.Cells = args.Cells[1:]
		return Nil()
	case argSym.Str == VarArgSymbol:
		if len(formals.Cells) != 2 {
			return env.Errorf("function formal argument list contains a control symbol at an invalid location: %v", argSym.Str)
		}
		argSym := formals.Cells[1]
		if strings.HasPrefix(argSym.Str, MetaArgPrefix) {
			return env.Errorf("function formal argument list contains a control symbol at an invalid location: %v", argSym.Str)
		}
		putVarArgs(argSym, QExpr(args.Cells))
		formals.Cells = nil
		args.Cells = nil
		return Nil()
	case strings.HasPrefix(argSym.Str, MetaArgPrefix):
		return env.Errorf("function formal argument list contains invalid control symbol ``%s''", argSym.Str)
	default:
		if len(args.Cells) == 0 {
			return fun
		}

		// This is a normal (required) argument symbol.  Pull a value out of
		// args and bind it.
		formals.Cells = formals.Cells[1:]
		put(argSym, args.Cells[0])
		args.Cells = args.Cells[1:]
		return Nil()
	}
}

func isKeyword(sym string) bool {
	// TODO:  Fix this terrible test.
	return strings.HasPrefix(sym, ":")
}
