package lisp

import (
	"fmt"
	"strconv"
	"strings"
)

var userSpecialOps []*langBuiltin
var langSpecialOps = []*langBuiltin{
	{"quasiquote", Formals("expr"), opQuasiquote},
	{"lambda", Formals("formals", VarArgSymbol, "expr"), opLambda},
	{"expr", Formals("pattern"), opExpr},
	{"let*", Formals("bindings", VarArgSymbol, "expr"), opLetSeq},
	{"let", Formals("bindings", VarArgSymbol, "expr"), opLet},
	{"progn", Formals(VarArgSymbol, "expr"), opProgn},
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

func opQuasiquote(env *LEnv, args *LVal) *LVal {
	if len(args.Cells) != 1 {
		return berrf("quasiquote", "one argument expected (got %d)", len(args.Cells))
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
		return berrf("lambda", "too few arguments provided: %d", len(v.Cells))
	}
	/*
		if len(v.Cells) > 2 {
			return berrf("lambda", "too many arguments provided: %d", len(v.Cells))
		}
		for _, q := range v.Cells {
			if q.Type != LSExpr {
				return berrf("lambda", "argument is not a list: %v", q.Type)
			}
		}
	*/

	formals := v.Cells[0]
	body := v.Cells[1:]
	body[len(body)-1].Terminal = true

	for _, sym := range formals.Cells {
		if sym.Type != LSymbol {
			return berrf("lambda", "first argument contains a non-symbol: %v", sym.Type)
		}
	}

	// Construct the LVal and add env to the LEnv chain to get lexical scoping
	// (I think... -bmatsuo)
	fun := Lambda(formals, body)
	fun.Env.Parent = env
	fun.Env.Stack = env.Stack

	return fun
}

func opExpr(env *LEnv, args *LVal) *LVal {
	if args.Len() != 1 {
		return berrf("expr", "one argument expected (got %d)", args.Len())
	}
	body := args.Cells[0]
	body.Terminal = true
	n, short, vargs, err := countExprArgs(body)
	if err != nil {
		return berrf("expr", "%s", err)
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
	if vargs {
		formals.Cells = append(formals.Cells, Symbol(VarArgSymbol), Symbol("%&"))
	}
	fun := Lambda(formals, []*LVal{body})
	fun.Env.Parent = env
	fun.Env.Stack = env.Stack
	return fun
}

func countExprArgs(expr *LVal) (nargs int, short bool, vargs bool, err error) {
	switch expr.Type {
	case LSymbol:
		if !strings.HasPrefix(expr.Str, "%") {
			return 0, false, false, nil
		}
		numStr := expr.Str[1:]
		if numStr == "" {
			return 1, true, false, nil
		}
		if numStr == VarArgSymbol {
			return 0, false, true, nil
		}
		num, err := strconv.Atoi(numStr)
		if err != nil {
			return 0, false, false, fmt.Errorf("invalid expr argument symbol: %s", expr.Str)
		}
		return num, false, false, nil
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
						return 0, false, false, err
					}
					short = true
				}
				continue
			}
			if numStr == VarArgSymbol {
				vargs = true
				continue
			}
			num, err := strconv.Atoi(numStr)
			if err != nil {
				return 0, false, false, fmt.Errorf("invalid expr argument symbol: %s", expr.Str)
			}
			if short {
				err := fmt.Errorf("invalid mix of expr argument symbols: %s and %s", "%", cell.Str)
				return 0, false, false, err
			}
			if num > nargs {
				nargs = num
			}
		}
		return nargs, short, vargs, nil
	case LInt, LFloat, LString:
		return 0, false, false, nil
	default:
		return 0, false, false, fmt.Errorf("invalid internal expression type: %s", expr.Type)
	}
}

func opLet(env *LEnv, args *LVal) *LVal {
	letenv := NewEnv(env)
	bindlist := args.Cells[0]
	args.Cells = args.Cells[1:] // decap so we can call builtinProgn on args.
	if bindlist.Type != LSExpr {
		return berrf("let", "first argument is not a list: %s", bindlist.Type)
	}
	vals := make([]*LVal, len(bindlist.Cells))
	for i, bind := range bindlist.Cells {
		if bind.Type != LSExpr {
			return berrf("let", "first argument is not a list of pairs")
		}
		if len(bind.Cells) != 2 {
			return berrf("let", "first argument is not a list of pairs")
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
		return berrf("let*", "first argument is not a list: %s", bindlist.Type)
	}
	for _, bind := range bindlist.Cells {
		if bind.Type != LSExpr {
			return berrf("let*", "first argument is not a list of pairs")
		}
		if len(bind.Cells) != 2 {
			return berrf("let*", "first argument is not a list of pairs")
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
	}
	return val
}

// (cond (test-form then-form)*)
func opCond(env *LEnv, args *LVal) *LVal {
	last := len(args.Cells) - 1
	for i, branch := range args.Cells {
		if branch.Type != LSExpr {
			return berrf("cond", "argument is not a list: %v", branch.Type)
		}
		if len(branch.Cells) != 2 {
			return berrf("cond", "argument is not a pair (length %d)", len(branch.Cells))
		}
		var test *LVal
		if branch.Cells[0].Type == LSymbol && branch.Cells[0].Str == "else" {
			if i != last {
				return berrf("cond", "invalid syntax: else")
			}
			test = branch.Cells[0] // the value here doesn't matter as long as it isn't nil
		} else {
			test = env.Eval(branch.Cells[0])
		}
		if test.Type == LError {
			return test
		}
		if test.IsNil() {
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
		return berrf("if", "three arguments expected (got %d)", len(s.Cells))
	}
	r := env.Eval(s.Cells[0])
	if r.Type == LError {
		return r
	}
	ok := r.IsNil()
	if ok {
		// test-form evaluated to nil (false)
		s.Cells[2].Terminal = true
		return env.Eval(s.Cells[2])
	}
	// test-form evaluated to something non-nil (true)
	s.Cells[1].Terminal = true
	return env.Eval(s.Cells[1])
}

func opOr(env *LEnv, s *LVal) *LVal {
	if len(s.Cells) > 0 {
		s.Cells[len(s.Cells)-1].Terminal = true
	}
	for _, c := range s.Cells {
		r := env.Eval(c)
		if r.Type == LError {
			return r
		}
		ok := r.IsNil()
		if ok {
			return Bool(true)
		}
	}
	return Bool(false)
}

func opAnd(env *LEnv, s *LVal) *LVal {
	if len(s.Cells) > 0 {
		s.Cells[len(s.Cells)-1].Terminal = true
	}
	for _, c := range s.Cells {
		r := env.Eval(c)
		if r.Type == LError {
			return r
		}
		ok := r.IsNil()
		if !ok {
			return Bool(false)
		}
	}
	return Bool(true)
}
