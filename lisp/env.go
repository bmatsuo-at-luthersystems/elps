package lisp

import (
	"bytes"
	"fmt"
	"log"
	"sync/atomic"
)

var envCount uint64

func getEnvID() uint {
	return uint(atomic.AddUint64(&envCount, 1))
}

// LEnv is a lisp environment.
type LEnv struct {
	ID      uint
	Scope   map[string]*LVal
	FunName map[string]string
	Parent  *LEnv
	Stack   *CallStack
}

// NewEnv returns initializes and returns a new LEnv.
func NewEnv(parent *LEnv) *LEnv {
	var stack *CallStack
	if parent != nil {
		stack = parent.Stack
	} else {
		stack = &CallStack{}
	}
	return &LEnv{
		ID:      getEnvID(),
		Scope:   make(map[string]*LVal),
		FunName: make(map[string]string),
		Parent:  parent,
		Stack:   stack,
	}
}

func (env *LEnv) getFID() string {
	return fmt.Sprintf("anon%d", env.ID)
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
	return Errorf("unbound symbol: %v", k)
}

// GetFunName returns the function name (if any) known to be bound to the given
// FID.
func (env *LEnv) GetFunName(fid string) string {
	name, ok := env.FunName[fid]
	if ok {
		return name
	}
	if env.Parent != nil {
		return env.Parent.GetFunName(fid)
	}
	return ""
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

// GetGlobal takes LSymbol k and returns the value it is bound to in the root
// environment (global scope).
func (env *LEnv) GetGlobal(k *LVal) *LVal {
	return env.root().Get(k)
}

// PutGlobal takes an LSymbol k and binds it to v in root environment (global
// scope).
func (env *LEnv) PutGlobal(k, v *LVal) {
	root := env.root()
	root.Put(k, v)
}

func (env *LEnv) root() *LEnv {
	for env.Parent != nil {
		env = env.Parent
	}
	return env
}

// AddMacros binds the given macros to their names in env.  When called with no
// arguments AddMacros adds the DefaultMacros to env.
func (env *LEnv) AddMacros(macs ...LBuiltinDef) {
	if len(macs) == 0 {
		macs = DefaultMacros()
	}
	for _, mac := range macs {
		k := Symbol(mac.Name())
		exist := env.Get(k)
		if !exist.IsNil() && exist.Type != LError { // LError is ubound symbol
			panic(fmt.Sprintf("macro already defined: %v (= %v)", k, exist))
		}
		id := fmt.Sprintf("<builtin-macro ``%s''>", mac.Name())
		env.Put(k, Macro(id, mac.Formals(), mac.Eval))
	}
}

// AddSpecialOps binds the given special operators to their names in env.  When
// called with no arguments AddSpecialOps adds the DefaultSpecialOps to env.
func (env *LEnv) AddSpecialOps(ops ...LBuiltinDef) {
	if len(ops) == 0 {
		ops = DefaultSpecialOps()
	}
	for _, op := range ops {
		k := Symbol(op.Name())
		exist := env.Get(k)
		if !exist.IsNil() && exist.Type != LError { // LError is ubound symbol
			panic(fmt.Sprintf("macro already defined: %v (= %v)", k, exist))
		}
		id := fmt.Sprintf("<special-op ``%s''>", op.Name())
		env.Put(k, SpecialOp(id, op.Formals(), op.Eval))
	}
}

// AddBuiltins binds the given funs to their names in env.  When called with no
// arguments AddBuiltins adds the DefaultBuiltins to env.
func (env *LEnv) AddBuiltins(funs ...LBuiltinDef) {
	if len(funs) == 0 {
		funs = DefaultBuiltins()
	}
	for _, f := range funs {
		k := Symbol(f.Name())
		exist := env.Get(k)
		if exist.Type != LError {
			panic("symbol already defined: " + f.Name())
		}
		id := fmt.Sprintf("<builtin-function ``%s''>", f.Name())
		v := Fun(id, f.Formals(), f.Eval)
		env.Put(k, v)
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
	log.Printf("stack %v", env.Stack.Copy())
	return &LVal{
		Type:  LError,
		Str:   fullMsg,
		Stack: env.Stack.Copy(),
	}
}

// Errorf returns an LError value with a formatted error message.  The returned
// LVal contains a copy env.Stack.
func (env *LEnv) Errorf(format string, v ...interface{}) *LVal {
	log.Printf("stack %v", env.Stack.Copy())
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
		return env.Get(v)
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
		return Errorf("not an s-expression")
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
		return Errorf("first element of expression is not a function: %v", f)
	}

	// Check for possible tail recursion before pushing to avoid hitting s when
	// checking.  But push FID onto the stack before popping to simplify
	// book-keeping.
	npop := env.Stack.TerminalFID(f.FID)
	// Push onto the stack here so that we don't trigger tail recursion while
	// evaluating the arguments to f -- f has to be the recursive call, if
	// there is recursion at all.
	env.Stack.PushFID(f.FID, env.GetFunName(f.FID))
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
	if npop > 0 {
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
			return Errorf("%s: function expects %d arguments (got %d)", fun.FID, nargs, len(args.Cells))
		}
		argSym := formals.Cells[0]
		if argSym.Str == VarArgSymbol {
			if len(formals.Cells) == 1 {
				return Errorf("%s: function argument format list ends with symbol ``%s''", fun.FID, VarArgSymbol)
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
			return Errorf("function argument format list ends with symbol ``%s''", VarArgSymbol)
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
