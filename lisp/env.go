package lisp

import (
	"bytes"
	"fmt"
	"io"
	"log"
	"strings"
	"sync/atomic"
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

// InitializeUserEnv creates the default user environment.
func InitializeUserEnv(env *LEnv) *LVal {
	env.Registry.DefinePackage(DefaultLangPackage)
	env.Registry.Lang = DefaultLangPackage
	env.Package = env.Registry.Packages[env.Registry.Lang]
	env.AddMacros(true)
	env.AddSpecialOps(true)
	env.AddBuiltins(true)
	env.Registry.DefinePackage(DefaultUserPackage)
	rc := env.InPackage(Symbol(DefaultUserPackage))
	if !rc.IsNil() {
		return rc
	}
	return env.UsePackage(Symbol(env.Registry.Lang))
}

// LEnv is a lisp environment.
type LEnv struct {
	ID       uint
	Scope    map[string]*LVal
	FunName  map[string]string
	Parent   *LEnv
	Stack    *CallStack
	Package  *Package
	Registry *PackageRegistry
	Reader   Reader
}

// NewEnv returns initializes and returns a new LEnv.
func NewEnv(parent *LEnv) *LEnv {
	env := &LEnv{
		ID:      getEnvID(),
		Scope:   make(map[string]*LVal),
		FunName: make(map[string]string),
		Parent:  parent,
	}
	if parent != nil {
		env.Stack = parent.Stack
	} else {
		env.Registry = NewRepository()
		env.Stack = &CallStack{}
	}
	return env
}

func (env *LEnv) getFID() string {
	return fmt.Sprintf("anon%d", env.ID)
}

func (env *LEnv) DefinePackage(name *LVal) *LVal {
	if name.Type != LSymbol && name.Type != LString {
		return env.Errorf("argument cannot be converted to string: %v", name.Type)
	}
	env = env.root()
	env.Registry.DefinePackage(name.Str)
	return Nil()
}

func (env *LEnv) InPackage(name *LVal) *LVal {
	if name.Type != LSymbol && name.Type != LString {
		return env.Errorf("argument cannot be converted to string: %v", name.Type)
	}
	root := env.root()
	pkg := root.Registry.Packages[name.Str]
	if pkg == nil {
		return env.Errorf("unknown package: %v", name.Str)
	}
	root.Package = pkg
	return Nil()
}

func (env *LEnv) UsePackage(name *LVal) *LVal {
	if name.Type != LSymbol && name.Type != LString {
		return env.Errorf("argument cannot be converted to string: %v", name.Type)
	}
	root := env.root()
	pkg := root.Registry.Packages[name.Str]
	if pkg == nil {
		return env.Errorf("unknown package: %v", name.Str)
	}
	for _, sym := range pkg.Externals {
		v := pkg.Get(Symbol(sym))
		if v.Type == LError {
			return env.Errorf("package %s: %v", name.Str, v)
		}
		root.Package.Put(Symbol(sym), v)
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
	if env.Reader == nil {
		return Errorf("no reader for environment")
	}
	exprs, err := env.Reader.Read(name, r)
	if err != nil {
		return Error(err)
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

func (env *LEnv) get(k *LVal) *LVal {
	// LQSymbols are allowed...
	if k.Type != LSymbol && k.Type != LQSymbol {
		return Nil()
	}
	if k.Str == "t" {
		return Symbol("t")
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
	return env.Package.Get(k)
}

// GetFunName returns the function name (if any) known to be bound to the given
// FID.
func (env *LEnv) GetFunName(f *LVal) string {
	pkgname := f.Package
	if pkgname == "" {
		log.Printf("unknown package for function %s", f.FID)
		return ""
	}
	pkg := env.root().Registry.Packages[pkgname]
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
	if k.Str == "t" {
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
	return env.root().Package.Get(k)
}

// PutGlobal takes an LSymbol k and binds it to v in current package.
func (env *LEnv) PutGlobal(k, v *LVal) {
	root := env.root()
	root.Package.Put(k, v)
}

// Lambda returns a new Lambda with fun.Env and fun.Package set automatically.
func (env *LEnv) Lambda(formals *LVal, body []*LVal) *LVal {
	fun := Lambda(formals, body)
	if fun.Type == LError {
		return fun
	}
	fun.Env.Parent = env
	fun.Package = env.root().Package.Name
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
	pkg := env.root().Package
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
	pkg := env.root().Package
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
	pkg := env.root().Package
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
// Error may be called either with an error or with any number of *LVal values.
// It is invalid to pass an error argument with any other values and doing so
// will result in a runtime panic.  The returned LVal contains a copy
// env.Stack.
func (env *LEnv) Error(msg ...interface{}) *LVal {
	fullMsg := ""
	var buf bytes.Buffer
	for i, v := range msg {
		switch v := v.(type) {
		case *LVal:
			if i > 0 {
				buf.WriteString(" ")
			}
			if v.Type == LString {
				buf.WriteString(v.Str)
			} else {
				buf.WriteString(v.String())
			}
		case error:
			if len(msg) > 1 {
				panic("invalid error argument")
			}
			// TODO:  Add somewhere to store the original error so that the embedding
			// program can potentially pick it out and inspect it.
			fullMsg = v.Error()
		}
	}
	if fullMsg == "" {
		fullMsg = buf.String()
	}
	//log.Printf("stack %v", env.Stack.Copy())
	return &LVal{
		Type:  LError,
		Str:   fullMsg,
		Stack: env.Stack.Copy(),
	}
}

// Errorf returns an LError value with a formatted error message.  The returned
// LVal contains a copy env.Stack.
func (env *LEnv) Errorf(format string, v ...interface{}) *LVal {
	return &LVal{
		Type:  LError,
		Str:   fmt.Sprintf(format, v...),
		Stack: env.Stack.Copy(),
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
		return Errorf("spliced value used as expression")
	}
	if v.Terminal {
		v.Terminal = false
		top := env.Stack.Top()
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
			pkg := env.root().Registry.Packages[pieces[0]]
			if pkg == nil {
				return env.Errorf("unknown package: %q", pieces[0])
			}
			return pkg.Get(Symbol(pieces[1]))
		default:
			return env.Errorf("illegal symbol: %q", v.Str)
		}
	case LSExpr:
		res := env.EvalSExpr(v)
		if res.Type == LMarkMacExpand {
			v = res.Cells[0]
			goto eval
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

	f := env.Eval(s.Cells[0])
	s.Cells = s.Cells[1:]
	if f.Type == LError {
		return f
	}
	if f.Type != LFun {
		return env.Errorf("first element of expression is not a function: %v", f)
	}

	// Check for possible tail recursion before pushing to avoid hitting s when
	// checking.  But push FID onto the stack before popping to simplify
	// book-keeping.
	npop := env.Stack.TerminalFID(f.FID)
	// Push onto the stack here so that we don't trigger tail recursion while
	// evaluating the arguments to f -- f has to be the recursive call, if
	// there is recursion at all.
	env.Stack.PushFID(f.FID, f.Package, env.GetFunName(f))
	defer env.Stack.Pop()

	if f.IsSpecialFun() {
		// Argument to a macro a not evaluated but they aren't quoted either.
		// This behavior is what allows ``unquote'' to properly resolve macro
		// arguments symbols during and still produce valid code during macro
		// expansion.  That is, if x is a macro argument then what do the
		// following expressions return?
		//		(quasiquote (unquote x))             	  => {expression bound to x}
		//		(quasiquote (unquote '(if 1 '(1) '(2))))  => '(1)
		// If the value given to x was quoted by eval then ``unquote'' would
		// have to undo that quoting.  But unquote is not supposed to unquote
		// the value returned by (if 1 '(1) '(2)), it merely evaluates the
		// expression and produces '(1).
	} else {
		// Evaluate arguments before invoking f.
		for i := range s.Cells {
			s.Cells[i] = env.Eval(s.Cells[i])
		}
		for i := range s.Cells {
			if s.Cells[i].Type == LError {
				return s.Cells[i]
			}
		}
	}
	// The call stack allows for tail recursion optimization.  However certain
	// functions like ``let'' can't utilize optimization because the scope
	// defined by the outer ``let'' would be lost.  (let ([x 1]) (let ([y x])
	// (+ y 1))) The stack analysis marks the inner let as an optimization
	// candidate.  But unwinding the stack would result in x being unbound.
	// The simple solution is to avoid unwinding for functions like let (e.g.
	// all builtin functions).  Alternatively builtin functions that don't
	// allow tail recursion might mark the stack to indicate frames which
	// cannot be unwound to for tail recursion optimization.  It's unclear if
	// builtin functions benefit from tail recursion so it is much simpler to
	// just avoid unwinding for tail recursion in any buitlin function..
	if npop > 0 && f.Builtin == nil {
		return markTailRec(npop, f, s)
	}
callf:
	r := env.Call(f, s)
	if r.Type == LError {
		return r
	}
	if r.Type == LMarkTailRec {
		if len(r.Cells) != 3 {
			panic("invalid mark")
		}
		r.Cells[0].Int--
		if r.Cells[0].Int <= 0 {
			f = r.Cells[1]
			s = r.Cells[2]
			goto callf
		}
		return r
	}
	if !f.IsMacro() {
		if r.Type == LSExpr && !r.IsNil() {
			// This has a really bad smell to it.  But I can't get quasiquote
			// to work as expected without checking for unquoted s-expressions
			// here.
			r.Quoted = true
		}
		return r
	}
	// This is a lazy unquote.  Unquoting in this way appears to allow the
	// upcoming evaluation to produce the correct value for user defined
	// macros, which are typically using quasiquote.  Builtin macros can be
	// massaged to return a proper value.  I'm sure there is a bug where
	// something is unintentionally unquoted.  I will deal with
	// implementing a proper system for special operators at that point.
	r.Quoted = false
	return markMacExpand(r)
}

// Call invokes LFun fun with the list args.
func (env *LEnv) Call(fun *LVal, args *LVal) *LVal {
	// FIXME:  A shallow copy is probably correct here.(?)  We don't want to
	// copy the Env so that updates to the global scope are reflected.

	fun = fun.Copy()
	putArg := func(k, v *LVal) {
		fun.Env.Put(k, v)
	}
	putVarArg := func(k *LVal, v *LVal) {
		fun.Env.Put(k, v)
	}
	if fun.Env == nil {
		// FIXME?: Builtins don't have lexical envs.  We just store the args in
		// the cells for builtin functions.
		putArg = func(k, v *LVal) {
			fun.Cells = append(fun.Cells, v)
		}
		putVarArg = func(k *LVal, v *LVal) {
			fun.Cells = append(fun.Cells, v.Cells...)
		}
	}
	formals := fun.Cells[0]
	nargs := len(formals.Cells) // only used when not vargs
	for i, v := range args.Cells {
		if len(formals.Cells) == 0 {
			return env.Errorf("%s: function expects %d arguments (got %d)", fun.FID, nargs, len(args.Cells))
		}
		argSym := formals.Cells[0]
		if argSym.Str == VarArgSymbol {
			if len(formals.Cells) == 1 {
				return env.Errorf("%s: function argument format list ends with symbol ``%s''", fun.FID, VarArgSymbol)
			}
			argSym = formals.Cells[1]
			q := QExpr(args.Cells[i:])
			formals.Cells = nil
			putVarArg(argSym, q)
			break
		}
		formals.Cells = formals.Cells[1:]
		putArg(argSym, v)
	}
	if len(formals.Cells) != 0 {
		if formals.Cells[0].Str != VarArgSymbol {
			// Return the partially bound, unevaluated function so additional
			// arguments can be curried.
			return fun
		}
		if len(formals.Cells) != 2 {
			return env.Errorf("function argument format list ends with symbol ``%s''", VarArgSymbol)
		}
		// We never bound the final argument to a value so we do it here.
		putVarArg(formals.Cells[1], Nil())
		formals.Cells = nil
	}

	// NOTE:  The book's suggestion of chaining env here seems like dynamic
	// scoping.
	// NOTE:  This is indeed where you would chain environments to create
	// dynamic scope.
	//		fun.Env.Parent = env

	if fun.Builtin != nil {
		// We are definitely working with a non-empty stack here so there is no
		// nil check.
		env.Stack.Top().Terminal = true

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
	root := env.root()
	outer := root.Package
	if outer.Name != fun.Package {
		inner := root.Registry.Packages[fun.Package]
		if inner != nil {
			root.Package = inner
			defer func() {
				root.Package = outer
			}()
		}
	}

	body := fun.Cells[1:]
	if len(body) > 0 {
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
