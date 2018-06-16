package lisp

import (
	"errors"
	"fmt"
	"strconv"
	"strings"
)

var userSpecialOps []*langBuiltin
var langSpecialOps = []*langBuiltin{
	{"assert", Formals("expr", VarArgSymbol, "message-format-args"), opAssert},
	{"quote", Formals("expr"), opQuote},
	{"quasiquote", Formals("expr"), opQuasiquote},
	{"lambda", Formals("formals", VarArgSymbol, "expr"), opLambda},
	{"expr", Formals("pattern"), opExpr},
	{"thread-first", Formals("value", VarArgSymbol, "exprs"), opThreadFirst},
	{"thread-last", Formals("value", VarArgSymbol, "exprs"), opThreadLast},
	{"let*", Formals("bindings", VarArgSymbol, "expr"), opLetSeq},
	{"let", Formals("bindings", VarArgSymbol, "expr"), opLet},
	{"progn", Formals(VarArgSymbol, "expr"), opProgn},
	{"ignore-errors", Formals(VarArgSymbol, "exprs"), opIgnoreErrors},
	{"cond", Formals(VarArgSymbol, "branch"), opCond},
	{"if", Formals("condition", "then", "else"), opIf},
	{"or", Formals(VarArgSymbol, "expr"), opOr},
	{"and", Formals(VarArgSymbol, "expr"), opAnd},
}

// RegisterDefaultSpecialOp adds the given function to the list returned by
// DefaultSpecialOps.
func RegisterDefaultSpecialOp(name string, formals *LVal, fn LBuiltin) {
	userSpecialOps = append(userSpecialOps, &langBuiltin{name, formals.Copy(), fn})
}

// DefaultSpecialOps returns the default set of LBuiltinDef added to LEnv
// objects when LEnv.AddSpecialOps is called without arguments.
func DefaultSpecialOps() []LBuiltinDef {
	ops := make([]LBuiltinDef, len(langSpecialOps)+len(userSpecialOps))
	for i := range langSpecialOps {
		ops[i] = langSpecialOps[i]
	}
	offset := len(langSpecialOps)
	for i := range userSpecialOps {
		ops[offset+i] = langSpecialOps[i]
	}
	return ops
}

func opAssert(env *LEnv, args *LVal) *LVal {
	test := args.Cells[0]
	var formatStr *LVal
	var formatArgs []*LVal
	if len(args.Cells) > 1 {
		formatStr = args.Cells[1]
		formatArgs = args.Cells[2:]
		if formatStr.Type != LString {
			return env.Errorf("second argument is not a string: %v", formatStr.Type)
		}
	}
	ok := env.Eval(test.Copy())
	if ok.Type == LError {
		return ok
	}
	if True(ok) {
		return Nil()
	}
	if formatStr == nil {
		return env.Errorf("assertion failure: %s", test)
	}
	for i := range formatArgs {
		formatArgs[i] = env.Eval(formatArgs[i])
		if formatArgs[i].Type == LError {
			return formatArgs[i]
		}
	}
	msg := builtinFormatString(env, SExpr(args.Cells[1:]))
	if msg.Type == LError {
		return msg
	}
	return env.Error(errors.New(msg.Str))
}

func opQuote(env *LEnv, args *LVal) *LVal {
	if len(args.Cells) != 1 {
		return env.Errorf("one argument expected (got %d)", len(args.Cells))
	}
	// NOTE:  Racket seems to detect nested (quote ...) expressions when
	// quoting things.  That is, (quote (quote 3)) in Racket evaluates to ''3,
	// not '(quote 3).  We could try to dig into the quoted arguments to
	// determine if that were possible but it is unclear whether it's possible
	// for ``quote'' to resolve differently or for this macro to be called
	// under a different name.
	return Quote(args.Cells[0])
}

func opQuasiquote(env *LEnv, args *LVal) *LVal {
	if len(args.Cells) != 1 {
		return env.Errorf("one argument expected (got %d)", len(args.Cells))
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

func opLambda(env *LEnv, v *LVal) *LVal {
	if len(v.Cells) < 2 {
		return env.Errorf("too few arguments provided: %d", len(v.Cells))
	}
	/*
		if len(v.Cells) > 2 {
			return env.Errorf("too many arguments provided: %d", len(v.Cells))
		}
		for _, q := range v.Cells {
			if q.Type != LSExpr {
				return env.Errorf("argument is not a list: %v", q.Type)
			}
		}
	*/

	formals := v.Cells[0]
	body := v.Cells[1:]
	body[len(body)-1].Terminal = true

	for _, sym := range formals.Cells {
		if sym.Type != LSymbol {
			return env.Errorf("first argument contains a non-symbol: %v", sym.Type)
		}
	}

	// Construct the LVal and add env to the LEnv chain to get lexical scoping
	// (I think... -bmatsuo)
	fun := Lambda(formals, body)
	fun.Env.Parent = env
	fun.Env.Stack = env.Stack
	fun.Package = env.root().Package.Name

	return fun
}

func opExpr(env *LEnv, args *LVal) *LVal {
	if args.Len() != 1 {
		return env.Errorf("one argument expected (got %d)", args.Len())
	}
	body := args.Cells[0]
	body.Terminal = true
	n, short, nopt, vargs, err := countExprArgs(body)
	if err != nil {
		return env.Errorf("%s", err)
	}
	formals := SExpr(nil)
	if short {
		formals.Cells = make([]*LVal, 1, 3)
		formals.Cells[0] = Symbol("%")
	} else {
		formals.Cells = make([]*LVal, n, n+2)
		for i := range formals.Cells {
			formals.Cells[i] = Symbol(fmt.Sprintf("%%%d", i+1))
		}
	}
	if nopt == 1 {
		formals.Cells = append(formals.Cells, Symbol(OptArgSymbol), Symbol("%"+OptArgSymbol))
	} else {
		// multple optional args aren't supported currently
	}
	if vargs {
		formals.Cells = append(formals.Cells, Symbol(VarArgSymbol), Symbol("%"+VarArgSymbol))
	}
	fun := Lambda(formals, []*LVal{body})
	fun.Env.Parent = env
	fun.Env.Stack = env.Stack
	fun.Package = env.root().Package.Name

	return fun
}

func countExprArgs(expr *LVal) (nargs int, short bool, nopt int, vargs bool, err error) {
	switch expr.Type {
	case LSymbol:
		if !strings.HasPrefix(expr.Str, "%") {
			return 0, false, 0, false, nil
		}
		numStr := expr.Str[1:]
		if numStr == "" {
			return 1, true, 0, false, nil
		}
		if numStr == VarArgSymbol {
			return 0, false, 0, true, nil
		}
		if numStr == OptArgSymbol {
			// multple optional args aren't supported currently
			return 0, false, 1, false, nil
		}
		num, err := strconv.Atoi(numStr)
		if err != nil {
			return 0, false, 0, false, fmt.Errorf("invalid expr argument symbol: %s", expr.Str)
		}
		return num, false, 0, false, nil
	case LSExpr:
		short := false
		for _, cell := range expr.Cells {
			if !strings.HasPrefix(cell.Str, "%") {
				continue
			}
			numStr := cell.Str[1:]
			if numStr == "" {
				if !short {
					if nargs > 0 {
						err := fmt.Errorf("invalid mixing of expr argument symbols: %s and %s",
							fmt.Sprintf("%%%d", nargs),
							cell.Str)
						return 0, false, 0, false, err
					}
					short = true
				}
				continue
			}
			if numStr == OptArgSymbol {
				nopt = 1 // multple optional args aren't supported currently
				continue
			}
			if numStr == VarArgSymbol {
				vargs = true
				continue
			}
			if strings.HasPrefix(numStr, MetaArgPrefix) {
				return 0, false, 0, false, fmt.Errorf("invalid expr argument symbol: %v", expr.Str)
			}
			num, err := strconv.Atoi(numStr)
			if err != nil {
				return 0, false, 0, false, fmt.Errorf("invalid expr argument symbol: %s", expr.Str)
			}
			if short {
				err := fmt.Errorf("invalid mix of expr argument symbols: %s and %s", "%", cell.Str)
				return 0, false, 0, false, err
			}
			if num > nargs {
				nargs = num
			}
		}
		return nargs, short, nopt, vargs, nil
	case LInt, LFloat, LString:
		return 0, false, 0, false, nil
	default:
		return 0, false, 0, false, fmt.Errorf("invalid internal expression type: %s", expr.Type)
	}
}

func opThreadLast(env *LEnv, args *LVal) *LVal {
	val, exprs := args.Cells[0], args.Cells[1:]
	for _, expr := range exprs {
		if expr.Type != LSExpr || expr.Quoted {
			return env.Errorf("expression argument is not a function call")
		}
		if expr.Len() < 1 {
			return env.Errorf("expression argument is nil")
		}
	}
	for _, expr := range exprs {
		expr.Cells = append(expr.Cells, val)
		val = env.Eval(expr)
		if val.Type == LError {
			return val
		}
	}
	return val
}

func opThreadFirst(env *LEnv, args *LVal) *LVal {
	val, exprs := args.Cells[0], args.Cells[1:]
	for _, expr := range exprs {
		if expr.Type != LSExpr || expr.Quoted {
			return env.Errorf("expression argument is not a function call")
		}
		if expr.Len() < 1 {
			return env.Errorf("expression argument is nil")
		}
	}
	for _, expr := range exprs {
		cells := make([]*LVal, len(expr.Cells)+1)
		cells[0] = expr.Cells[0]
		cells[1] = val
		copy(cells[2:], expr.Cells[1:])
		val = env.Eval(SExpr(cells))
		if val.Type == LError {
			return val
		}
	}
	return val
}

func opLet(env *LEnv, args *LVal) *LVal {
	letenv := NewEnv(env)
	bindlist := args.Cells[0]
	args.Cells = args.Cells[1:] // decap so we can call builtinProgn on args.
	if bindlist.Type != LSExpr {
		return env.Errorf("first argument is not a list: %s", bindlist.Type)
	}
	vals := make([]*LVal, len(bindlist.Cells))
	for i, bind := range bindlist.Cells {
		if bind.Type != LSExpr {
			return env.Errorf("first argument is not a list of pairs")
		}
		if len(bind.Cells) != 2 {
			return env.Errorf("first argument is not a list of pairs")
		}
		vals[i] = letenv.Eval(bind.Cells[1])
		if vals[i].Type == LError {
			return vals[i]
		}
	}
	for i, bind := range bindlist.Cells {
		letenv.Put(bind.Cells[0], vals[i])
	}
	return opProgn(letenv, args)
}

func opLetSeq(env *LEnv, args *LVal) *LVal {
	letenv := NewEnv(env)
	bindlist := args.Cells[0]
	args.Cells = args.Cells[1:] // decap so we can call builtinProgn on args.
	if bindlist.Type != LSExpr {
		return env.Errorf("first argument is not a list: %s", bindlist.Type)
	}
	for _, bind := range bindlist.Cells {
		if bind.Type != LSExpr {
			return env.Errorf("first argument is not a list of pairs")
		}
		if len(bind.Cells) != 2 {
			return env.Errorf("first argument is not a list of pairs")
		}
		val := letenv.Eval(bind.Cells[1])
		if val.Type == LError {
			return val
		}
		letenv.Put(bind.Cells[0], val)
	}
	return opProgn(letenv, args)
}

func opProgn(env *LEnv, args *LVal) *LVal {
	if len(args.Cells) == 0 {
		return Nil()
	}
	args.Cells[len(args.Cells)-1].Terminal = true
	var val *LVal
	for _, c := range args.Cells {
		val = env.Eval(c)
		if val.Type == LError {
			return val
		}
	}
	return val
}

func opIgnoreErrors(env *LEnv, args *LVal) *LVal {
	if len(args.Cells) == 0 {
		return Nil()
	}
	args.Cells[len(args.Cells)-1].Terminal = true
	var val *LVal
	for _, c := range args.Cells {
		val = env.Eval(c)
		if val.Type == LError {
			return Nil()
		}
	}
	return val
}

// (cond (test-form then-form)*)
func opCond(env *LEnv, args *LVal) *LVal {
	last := len(args.Cells) - 1
	for i, branch := range args.Cells {
		if branch.Type != LSExpr {
			return env.Errorf("argument is not a list: %v", branch.Type)
		}
		if len(branch.Cells) != 2 {
			return env.Errorf("argument is not a pair (length %d)", len(branch.Cells))
		}
		var test *LVal
		if branch.Cells[0].Type == LSymbol && branch.Cells[0].Str == "else" {
			if i != last {
				return env.Errorf("invalid syntax: else")
			}
			test = branch.Cells[0] // the value here doesn't matter as long as it isn't nil
		} else {
			test = env.Eval(branch.Cells[0])
		}
		if test.Type == LError {
			return test
		}
		if Not(test) {
			continue
		}
		branch.Cells[1].Terminal = true
		return env.Eval(branch.Cells[1])
	}
	return Nil()
}

// (if test-form then-form else-form)
func opIf(env *LEnv, s *LVal) *LVal {
	if len(s.Cells) != 3 {
		return env.Errorf("three arguments expected (got %d)", len(s.Cells))
	}
	r := env.Eval(s.Cells[0])
	if r.Type == LError {
		return r
	}
	if Not(r) {
		// test-form evaluated to nil (false)
		s.Cells[2].Terminal = true
		return env.Eval(s.Cells[2])
	}
	// test-form evaluated to something non-nil (true)
	s.Cells[1].Terminal = true
	return env.Eval(s.Cells[1])
}

func opOr(env *LEnv, s *LVal) *LVal {
	if len(s.Cells) == 0 {
		return Nil()
	}
	s.Cells[len(s.Cells)-1].Terminal = true
	var r *LVal
	for _, c := range s.Cells {
		r = env.Eval(c)
		if r.Type == LError {
			return r
		}
		if True(r) {
			return r
		}
	}
	// In the common lisp standard the ``or'' function returns the evaluated
	// result of its final argument if no arguments evaluated true.
	//		(or) == nil
	//		(or x) == x
	//		(or x1 x2 ... xn) == (cond (x1 x1) (x2 x2) ... (t xn))
	return r
}

func opAnd(env *LEnv, s *LVal) *LVal {
	if len(s.Cells) == 0 {
		// The identity for and is a true value.
		return Bool(true)
	}
	s.Cells[len(s.Cells)-1].Terminal = true
	var r *LVal
	for _, c := range s.Cells {
		r = env.Eval(c)
		if r.Type == LError {
			return r
		}
		if !True(r) {
			// In the common lisp standard short circuiting the ``and''
			// function always causes a nil return, even if r is the symbol
			// false.
			return Nil()
		}
	}
	// In the common lisp standard the ``and'' function returns the evaluated
	// result of its final argument if all arguments evaluated true.
	//		(and) == nil
	//		(and x) == x
	//		(and x1 x2 ... xn) == (cond
	//		                       ((not x1) nil)
	//		                       ((not x2) nil)
	//		                       ...
	//		                       (t xn))
	return r
}
