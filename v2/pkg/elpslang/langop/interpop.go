package langop

import (
	"fmt"
	"strings"

	"github.com/luthersystems/elps/v2/pkg/elpslang/langproc"
	"github.com/luthersystems/elps/v2/pkg/elpslang/stack"
	"github.com/luthersystems/elps/v2/pkg/environ"
	"github.com/luthersystems/elps/v2/pkg/internal/lisputil"
	"github.com/luthersystems/elps/v2/pkg/lisp"
	"github.com/luthersystems/elps/v2/pkg/symbol"
)

type Printer interface {
	Print(...interface{})
}

var (
	symSet     = symbol.Intern("set")
	symIf      = symbol.Intern("if")
	symLambda  = symbol.Intern("lambda")
	symProgn   = symbol.Intern("progn")
	symEnviron = symbol.Intern("environ")
	symProc    = symbol.Intern("procedure")
	symRest    = symbol.Intern("&rest")
)

func carIsSymbol(v lisp.LVal, sym symbol.ID) lisp.LVal {
	v, ok := lisp.GetCAR(v)
	if !ok {
		return lisp.False()
	}
	return lisp.Bool(v.Type() == lisp.LSymbol && symbol.ID(v.Data) == sym)
}

func argTypeErr(v lisp.LVal, expect string) error {
	return fmt.Errorf("argument is not a %s: %v", expect, lisp.GetType(v))
}

var errZeroArgs = fmt.Errorf("requires 1 arguments")
var errOneArg = fmt.Errorf("requires 1 arguments")
var errTwoArgs = fmt.Errorf("requires 2 arguments")
var errThreeArgs = fmt.Errorf("requires 3 arguments")

func _car(v lisp.LVal, ok bool) (lisp.LVal, bool) {
	if !ok {
		return lisp.Nil(), false
	}
	return lisp.GetCAR(v)
}

func _cdr(v lisp.LVal, ok bool) (lisp.LVal, bool) {
	if !ok {
		return lisp.Nil(), false
	}
	return lisp.GetCDR(v)
}

func listwrap(v lisp.LVal, ok bool) (lisp.LVal, error) {
	if !ok {
		return lisp.Nil(), argTypeErr(v, "list")
	}
	return v, nil
}
func car(v lisp.LVal) (lisp.LVal, error)    { return listwrap(_car(v, true)) }
func cdr(v lisp.LVal) (lisp.LVal, error)    { return listwrap(_cdr(v, true)) }
func cddr(v lisp.LVal) (lisp.LVal, error)   { return listwrap(_cdr(_cdr(v, true))) }
func cadr(v lisp.LVal) (lisp.LVal, error)   { return listwrap(_car(_cdr(v, true))) }
func caddr(v lisp.LVal) (lisp.LVal, error)  { return listwrap(_car(_cdr(_cdr(v, true)))) }
func cadddr(v lisp.LVal) (lisp.LVal, error) { return listwrap(_car(_cdr(_cdr(_cdr(v, true))))) }

//  (op initialize-stack) (reg stack)
func InitializeStack(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 1 {
		return lisp.Nil(), errOneArg
	}
	s, ok := stack.GetStack(args[0])
	if !ok {
		return lisp.Nil(), argTypeErr(args[0], "stack")
	}
	s.Initialize()
	return lisp.Nil(), nil
}

//  (op print-stack-statistics) (reg stack)
func PrintStackStatistics(p Printer) func(args ...lisp.LVal) (lisp.LVal, error) {
	return func(args ...lisp.LVal) (lisp.LVal, error) {
		if len(args) != 1 {
			return lisp.Nil(), errOneArg
		}
		s, ok := stack.GetStack(args[0])
		if !ok {
			return lisp.Nil(), argTypeErr(args[0], "stack")
		}
		w := &strings.Builder{}
		_, err := s.FormatStatistics(w)
		if err != nil {
			return lisp.Nil(), err
		}
		p.Print(w.String())
		return lisp.Nil(), nil
	}
}

//  (op get-global-environment)
func GetGlobalEnvironment(env *environ.Environ) func(args ...lisp.LVal) (lisp.LVal, error) {
	return func(args ...lisp.LVal) (lisp.LVal, error) {
		if len(args) != 0 {
			return lisp.Nil(), errZeroArgs
		}
		return env.LVal(nil, symEnviron), nil
	}
}

//  (op self-evaluating?) (reg exp)
func IsSelfEvaluating(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 1 {
		return lisp.Nil(), errOneArg
	}
	return lisp.Bool(args[0].Type() != lisp.LCons && args[0].Type() != lisp.LSymbol), nil
}

//  (op variable?) (reg exp)
func IsVariable(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 1 {
		return lisp.Nil(), errOneArg
	}
	return lisp.Bool(args[0].Type() == lisp.LSymbol), nil
}

//  (op quoted?) (reg exp)
func IsQuoted(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 1 {
		return lisp.Nil(), errOneArg
	}
	return lisp.Bool(args[0].Type() == lisp.LQuote), nil
}

//  (op assignment?) (reg exp)
func IsAssignment(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 1 {
		return lisp.Nil(), errOneArg
	}
	return carIsSymbol(args[0], symSet), nil
}

//  (op if?) (reg exp)
func IsIf(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 1 {
		return lisp.Nil(), errOneArg
	}
	return carIsSymbol(args[0], symIf), nil
}

//  (op lambda?) (reg exp)
func IsLambda(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 1 {
		return lisp.Nil(), errOneArg
	}
	return carIsSymbol(args[0], symLambda), nil
}

//  (op progn?) (reg exp)
func IsProgn(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 1 {
		return lisp.Nil(), errOneArg
	}
	return carIsSymbol(args[0], symProgn), nil
}

//  (op application?) (reg exp)
func IsApplication(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 1 {
		return lisp.Nil(), errOneArg
	}
	return lisp.Bool(args[0].Type() == lisp.LCons), nil
}

//  (op lookup-variable-value) (reg exp) (reg env)
func LookupVariableValue(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 2 {
		return lisp.Nil(), errTwoArgs
	}
	name, ok := lisp.GetSymbol(args[0])
	if !ok {
		return lisp.Nil(), argTypeErr(args[0], "symbol")
	}
	env, ok := environ.GetEnviron(args[1])
	if !ok {
		return lisp.Nil(), argTypeErr(args[1], "environ")
	}
	v, ok := env.Get(name)
	if !ok {
		// TODO:  This error message needs to include the string representation
		// of the symbol
		return lisp.Nil(), fmt.Errorf("unbound symbol")
	}
	return v, nil
}

//  (op set-variable-value!) (reg unev) (reg val) (reg env)
func SetVariableValue(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 3 {
		return lisp.Nil(), errThreeArgs
	}
	name, ok := lisp.GetSymbol(args[0])
	if !ok {
		return lisp.Nil(), argTypeErr(args[0], "symbol")
	}
	env, ok := environ.GetEnviron(args[2])
	if !ok {
		return lisp.Nil(), argTypeErr(args[2], "environ")
	}
	env.Root().Put(name, args[1])
	return lisp.Nil(), nil
}

//  (op extend-environment) (reg unev) (reg argl) (reg env)
func ExtendEnvironment(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 3 {
		return lisp.Nil(), errThreeArgs
	}
	env, ok := environ.GetEnviron(args[2])
	if !ok {
		return lisp.Nil(), argTypeErr(args[2], "environ")
	}
	argl, ok := GetArgList(args[1])
	if !ok {
		return lisp.Nil(), argTypeErr(args[0], "arg-list")
	}
	n, _ := lisputil.ConsLen(args[0])
	env = environ.New(env, environ.NewBindings(n))
	err := extendAny(env,
		lisp.NewListIterator(args[0]),
		lisp.NewListIterator(argl.List()))
	// TOOD:  These failures should be LError
	if err != nil {
		return lisp.Nil(), err
	}
	return env.LVal(nil, symEnviron), nil
}

func extendAny(env *environ.Environ, formals *lisp.ListIterator, args *lisp.ListIterator) error {
	for {
		if !formals.Next() {
			if formals.Err() != nil {
				return fmt.Errorf("invalid procedure formal arguments: %w", formals.Err())
			}
			if lisp.IsNil(args.Rest()) {
				return nil
			}
			return fmt.Errorf("too many arguments provided")
		}
		sym, ok := lisp.GetSymbol(formals.Value())
		if !ok {
			return fmt.Errorf("formal argument is not a symbol")
		}
		if sym == symRest {
			return extendRest(env, formals, args)
		}
		if args.Next() {
			env.Put(sym, args.Value())
			continue
		}
		if args.Err() != nil {
			return fmt.Errorf("argument list: %w", args.Err())
		}
		return fmt.Errorf("not enough arguments provided")
	}
}

func extendRest(env *environ.Environ, formals *lisp.ListIterator, args *lisp.ListIterator) error {
	if formals.Next() {
		sym, ok := lisp.GetSymbol(formals.Value())
		if !ok {
			return fmt.Errorf("formal argument is not a symbol")
		}
		env.Put(sym, args.Rest())
		return nil
	}
	if formals.Err() != nil {
		return fmt.Errorf("formal argument list: %w", formals.Err())
	}
	return fmt.Errorf("formal argument list: %s not followed by a symbol", symRest)
}

//  (op runtime-error?) (reg val)
func IsRuntimeError(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 1 {
		return lisp.Nil(), errOneArg
	}
	return lisp.Bool(args[0].Type() == lisp.LError), nil
}

//  (op text-of-quotation) (reg exp)
func TextOfQuotation(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 1 {
		return lisp.Nil(), errOneArg
	}
	v, ok := lisp.GetQuote(args[0])
	if !ok {
		return lisp.Nil(), argTypeErr(args[0], "quote")
	}
	return v, nil
}

//  (op lambda-parameters) (reg exp)
func LambdaParameters(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 1 {
		return lisp.Nil(), errOneArg
	}
	cons, ok := lisp.GetCons(args[0])
	if !ok {
		return lisp.Nil(), argTypeErr(args[0], "lambda expression")
	}
	cons, ok = lisp.GetCons(cons.CDR())
	if !ok {
		return lisp.Nil(), argTypeErr(args[0], "lambda expression")
	}
	return cons.CAR(), nil
}

//  (op lambda-body) (reg exp)
func LambdaBody(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 1 {
		return lisp.Nil(), errOneArg
	}
	cons, ok := lisp.GetCons(args[0])
	if !ok {
		return lisp.Nil(), argTypeErr(args[0], "lambda expression")
	}
	cons, ok = lisp.GetCons(cons.CDR())
	if !ok {
		return lisp.Nil(), argTypeErr(args[0], "lambda expression")
	}
	return cons.CDR(), nil
}

//  (op make-procedure) (reg unev) (reg exp) (reg env)
func MakeProcedure(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 3 {
		return lisp.Nil(), errThreeArgs
	}
	env, ok := environ.GetEnviron(args[2])
	if !ok {
		return lisp.Nil(), fmt.Errorf("third argument is not an environ")
	}
	proc, err := langproc.NewCompound(args[0], lisp.Nil(), args[1], env)
	if err != nil {
		return lisp.Nil(), err
	}
	return proc.LVal(nil, langproc.LCompoundProc), nil
}

//  (op compound-procedure?) (reg proc)
func IsCompoundProcedure(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 1 {
		return lisp.Nil(), errOneArg
	}
	_, ok := langproc.GetCompound(args[0])
	return lisp.Bool(ok), nil
}

//  (op procedure-parameters) (reg proc)
func ProcedureParameters(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 1 {
		return lisp.Nil(), errOneArg
	}
	p, ok := langproc.GetCompound(args[0])
	if !ok {
		return lisp.Nil(), fmt.Errorf("return error argument is not a compound-procedure")
	}
	return p.FormalArgs(), nil
}

//  (op procedure-body) (reg proc)
func ProcedureBody(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 1 {
		return lisp.Nil(), errOneArg
	}
	p, ok := langproc.GetCompound(args[0])
	if !ok {
		return lisp.Nil(), fmt.Errorf("return error argument is not a compound-procedure")
	}
	return p.Body(), nil
}

//  (op procedure-environment) (reg proc)
func ProcedureEnvironment(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 1 {
		return lisp.Nil(), errOneArg
	}
	p, ok := langproc.GetCompound(args[0])
	if !ok {
		return lisp.Nil(), fmt.Errorf("return error argument is not a compound-procedure")
	}
	return p.Environ().LVal(nil, symEnviron), nil
}

//  (op operands) (reg exp)
func Operands(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 1 {
		return lisp.Nil(), errOneArg
	}
	data, ok := lisp.GetConsData(args[0])
	if !ok {
		return lisp.Nil(), argTypeErr(args[0], "list")
	}
	return data.CDR, nil
}

//  (op operator) (reg exp)
func Operator(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 1 {
		return lisp.Nil(), errOneArg
	}
	data, ok := lisp.GetConsData(args[0])
	if !ok {
		return lisp.Nil(), argTypeErr(args[0], "list")
	}
	return data.CAR, nil
}

//  (op empty-arglist)
func EmptyArglist(args ...lisp.LVal) (lisp.LVal, error) {
	return (&ArgList{}).LVal(), nil
}

//  (op no-operands?) (reg unev)
func NoOperands(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 1 {
		return lisp.Nil(), errOneArg
	}
	return lisp.Bool(lisp.IsNil(args[0])), nil
}

//  (op first-operand) (reg unev)
func FirstOperand(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 1 {
		return lisp.Nil(), errOneArg
	}
	v, ok := lisp.GetCAR(args[0])
	if !ok {
		return lisp.Nil(), argTypeErr(args[0], "pair")
	}
	return v, nil
}

//  (op rest-operands) (reg unev)
func RestOperands(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 1 {
		return lisp.Nil(), errOneArg
	}
	v, ok := lisp.GetCDR(args[0])
	if !ok {
		return lisp.Nil(), argTypeErr(args[0], "pair")
	}
	return v, nil
}

//  (op last-operand?) (reg unev)
func IsLastOperand(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 1 {
		return lisp.Nil(), errOneArg
	}
	data, ok := lisp.GetConsData(args[0])
	if !ok {
		return lisp.Nil(), argTypeErr(args[0], "list")
	}
	return lisp.Bool(lisp.IsNil(data.CDR)), nil
}

//  (op adjoin-arg) (reg val) (reg argl)
func AdjoinArg(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 2 {
		return lisp.Nil(), errTwoArgs
	}
	argl, ok := GetArgList(args[1])
	if !ok {
		return lisp.Nil(), fmt.Errorf("argument is not an arg-list")
	}
	argl.Append(args[0])
	return args[1], nil
}

//  (op primitive-procedure?) (reg proc)
func IsPrimitiveProcedure(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 1 {
		return lisp.Nil(), errOneArg
	}
	_, ok := langproc.GetPrimitive(args[0])
	return lisp.Bool(ok), nil
}

//  (op primitive-procedure-parameters) (reg proc)
func PrimitiveProcedureParameters(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 1 {
		return lisp.Nil(), errOneArg
	}
	proc, ok := langproc.GetPrimitive(args[0])
	if !ok {
		return lisp.Nil(), argTypeErr(args[0], "primitive-procedure")
	}
	return proc.FormalArgs(), nil
}

//  (op apply-primitive-procedure) (reg proc) (reg env)
func ApplyPrimitiveProcedure(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 2 {
		return lisp.Nil(), errTwoArgs
	}
	proc, ok := langproc.GetPrimitive(args[0])
	if !ok {
		return lisp.Nil(), argTypeErr(args[0], "primitive-procedure")
	}
	env, ok := environ.GetEnviron(args[1])
	if !ok {
		return lisp.Nil(), argTypeErr(args[1], "environ")
	}
	return proc.Apply(&procEnviron{env}), nil
}

type procEnviron struct {
	env *environ.Environ
}

func (env *procEnviron) Len() int {
	return env.env.Len()
}

func (env *procEnviron) GetAddress(j int) lisp.LVal {
	v, err := env.env.GetAddress(0, j)
	if err != nil {
		panic(err)
	}
	return v
}

func (env *procEnviron) Get(id symbol.ID) (lisp.LVal, bool) {
	return env.env.Get(id)
}

func (env *procEnviron) PutGlobal(id symbol.ID, v lisp.LVal) {
	// TODO: implement this
}

func (env *procEnviron) Print(v ...interface{}) {
	// TODO: implement this
}

//  (op progn-actions) (reg exp)
func PrognActions(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 1 {
		return lisp.Nil(), errOneArg
	}
	return cdr(args[0])
}

//  (op first-exp) (reg unev)
func FirstExp(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 1 {
		return lisp.Nil(), errOneArg
	}
	return car(args[0])
}

//  (op rest-exps) (reg unev)
func RestExps(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 1 {
		return lisp.Nil(), errOneArg
	}
	return cdr(args[0])
}

//  (op last-exp?) (reg unev)
func IsLastExp(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 1 {
		return lisp.Nil(), errOneArg
	}
	tail, err := cdr(args[0])
	if err != nil {
		return lisp.Nil(), err
	}
	return lisp.Bool(lisp.IsNil(tail)), nil
}

//  (op if-predicate) (reg exp)
func IfPredicate(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 1 {
		return lisp.Nil(), errOneArg
	}
	return cadr(args[0])
}

//  (op if-consequent) (reg exp)
func IfConsequent(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 1 {
		return lisp.Nil(), errOneArg
	}
	return caddr(args[0])
}

//  (op if-alternative) (reg exp)
func IfAlternative(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 1 {
		return lisp.Nil(), errOneArg
	}
	return cadddr(args[0])
}

//  (op true?) (reg val)
func IsTrue(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 1 {
		return lisp.Nil(), errOneArg
	}
	return lisp.Bool(lisp.IsTrue(args[0])), nil
}

//  (op assignment-variable) (reg exp)
func AssignmentVariable(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 1 {
		return lisp.Nil(), errOneArg
	}
	return cadr(args[0])
}

//  (op assignment-value) (reg exp)
func AssignmentValue(args ...lisp.LVal) (lisp.LVal, error) {
	if len(args) != 1 {
		return lisp.Nil(), errOneArg
	}
	return caddr(args[0])
}

//  (op user-print) (reg val)
func UserPrint(p Printer, table symbol.Table) func(args ...lisp.LVal) (lisp.LVal, error) {
	return func(args ...lisp.LVal) (lisp.LVal, error) {
		if len(args) != 1 {
			return lisp.Nil(), errOneArg
		}
		w := &strings.Builder{}
		_, err := lisp.Format(w, args[0], table)
		if err != nil {
			return lisp.Nil(), err
		}
		p.Print(w.String())
		return lisp.Nil(), nil
	}
}

var LArgList = symbol.Intern("arg-list")

type ArgList struct {
	lisp.ListBuilder
}

func (args *ArgList) LVal() lisp.LVal {
	return lisp.TagNative(LArgList, args)
}

func IsArgList(v lisp.LVal) bool {
	return v.Type() == lisp.LTaggedVal && symbol.ID(v.Data) == LArgList
}

func GetArgList(v lisp.LVal) (*ArgList, bool) {
	if v.Type() != lisp.LTaggedVal {
		return nil, false
	}
	args, ok := v.Native.(*ArgList)
	return args, ok
}
