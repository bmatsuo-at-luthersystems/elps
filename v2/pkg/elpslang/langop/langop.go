// Package langop defines machine operators for elpslang
package langop

import (
	"github.com/luthersystems/elps/v2/pkg/environ"
	"github.com/luthersystems/elps/v2/pkg/rmach"
	"github.com/luthersystems/elps/v2/pkg/symbol"
)

var Ops = rmach.OpMap{
	symbol.Intern("="):   EQ,
	symbol.Intern("<="):  LTE,
	symbol.Intern(">="):  GTE,
	symbol.Intern("<"):   LT,
	symbol.Intern(">"):   GT,
	symbol.Intern("+"):   Add,
	symbol.Intern("-"):   Sub,
	symbol.Intern("*"):   Mul,
	symbol.Intern("/"):   Div,
	symbol.Intern("mod"): Mod,
}

func InterpOps(p Printer, table symbol.Table, global *environ.Environ) rmach.OpMap {
	return rmach.OpMap{
		symbol.Intern("initialize-stack"):               InitializeStack,
		symbol.Intern("print-stack-statistics"):         PrintStackStatistics(p),
		symbol.Intern("get-global-environment"):         GetGlobalEnvironment(global),
		symbol.Intern("self-evaluating?"):               IsSelfEvaluating,
		symbol.Intern("variable?"):                      IsVariable,
		symbol.Intern("quoted?"):                        IsQuoted,
		symbol.Intern("assignment?"):                    IsAssignment,
		symbol.Intern("if?"):                            IsIf,
		symbol.Intern("lambda?"):                        IsLambda,
		symbol.Intern("progn?"):                         IsProgn,
		symbol.Intern("application?"):                   IsApplication,
		symbol.Intern("lookup-variable-value"):          LookupVariableValue,
		symbol.Intern("set-variable-value!"):            SetVariableValue,
		symbol.Intern("extend-environment"):             ExtendEnvironment,
		symbol.Intern("runtime-error?"):                 IsRuntimeError,
		symbol.Intern("text-of-quotation"):              TextOfQuotation,
		symbol.Intern("lambda-parameters"):              LambdaParameters,
		symbol.Intern("lambda-body"):                    LambdaBody,
		symbol.Intern("make-procedure"):                 MakeProcedure,
		symbol.Intern("compound-procedure?"):            IsCompoundProcedure,
		symbol.Intern("procedure-parameters"):           ProcedureParameters,
		symbol.Intern("procedure-body"):                 ProcedureBody,
		symbol.Intern("procedure-environment"):          ProcedureEnvironment,
		symbol.Intern("operands"):                       Operands,
		symbol.Intern("operator"):                       Operator,
		symbol.Intern("empty-arglist"):                  EmptyArglist,
		symbol.Intern("no-operands?"):                   NoOperands,
		symbol.Intern("first-operand"):                  FirstOperand,
		symbol.Intern("rest-operands"):                  RestOperands,
		symbol.Intern("last-operand?"):                  IsLastOperand,
		symbol.Intern("adjoin-arg"):                     AdjoinArg,
		symbol.Intern("primitive-procedure?"):           IsPrimitiveProcedure,
		symbol.Intern("primitive-procedure-parameters"): PrimitiveProcedureParameters,
		symbol.Intern("apply-primitive-procedure"):      ApplyPrimitiveProcedure,
		symbol.Intern("progn-actions"):                  PrognActions,
		symbol.Intern("first-exp"):                      FirstExp,
		symbol.Intern("rest-exps"):                      RestExps,
		symbol.Intern("last-exp?"):                      IsLastExp,
		symbol.Intern("if-predicate"):                   IfPredicate,
		symbol.Intern("if-consequent"):                  IfConsequent,
		symbol.Intern("if-alternative"):                 IfAlternative,
		symbol.Intern("true?"):                          IsTrue,
		symbol.Intern("assignment-variable"):            AssignmentVariable,
		symbol.Intern("assignment-value"):               AssignmentValue,
		symbol.Intern("user-print"):                     UserPrint(p, table),
	}
}
