package langmach

import (
	"github.com/luthersystems/elps/v2/pkg/internal/bootstrapparser"
	"github.com/luthersystems/elps/v2/pkg/lisp"
	"github.com/luthersystems/elps/v2/pkg/symbol"
)

func init() {
	program, err := bootstrapparser.ParseProgramString(symbol.Intern("main"), symbol.DefaultGlobalTable, interpAsm)
	if err != nil {
		panic("failed to parse interpreter: " + err.Error())
	}
	interpText = program
}

var interpText lisp.LVal

const interpAsm = `
  (goto (label execute-program))

;;read-eval-print-loop
;;  (perform (op initialize-stack) (reg stack))
;;  (perform (op prompt-for-input) (const ";;; EC-Eval input:"))
;;  (assign (reg exp) (op read))
;;  (assign (reg env) (op get-global-environment))
;;  (assign (reg continue) (label print-result))
;;  (goto (label eval-dispatch))
;;print-result
;;  (perform (op announce-output) (const ";;; EC-Eval value:"))
;;  (perform (op user-print) (reg val))
;;  (goto (label read-eval-print-loop))

execute-program
  (perform (op initialize-stack) (reg stack))
  (assign (reg unev) (reg program))
  (assign (reg env) (op get-global-environment))
  (assign (reg continue) (label done))
  (save (reg continue))
  (goto (label ev-sequence))

funcall
  (perform (op initialize-stack) (reg stack))
  (assign (reg env) (op get-global-environment))
  (assign (reg continue) (label done))
  (goto (label ev-application))

eval-dispatch
  (test (op self-evaluating?) (reg exp))
  (branch (label ev-self-eval))
  (test (op variable?) (reg exp))
  (branch (label ev-variable))
  (test (op quoted?) (reg exp))
  (branch (label ev-quoted))
  (test (op assignment?) (reg exp))
  (branch (label ev-assignment))
  ;;(test (op definition?) (reg exp))
  ;;(branch (label ev-definition))
  (test (op if?) (reg exp))
  (branch (label ev-if))
  (test (op lambda?) (reg exp))
  (branch (label ev-lambda))
  (test (op progn?) (reg exp))
  (branch (label ev-progn))
  (test (op application?) (reg exp))
  (branch (label ev-application))
  (goto (label unknown-expression-type))
ev-self-eval
  (assign (reg val) (reg exp))
  (goto (reg continue))
ev-variable
  (assign (reg val) (op lookup-variable-value) (reg exp) (reg env))
  (branch-condition (label ev-variable-error))
  (goto (reg continue))
ev-variable-error
  ; TODO associate stack with the error in (reg val)
  (goto (reg continue))
ev-quoted
  (assign (reg val) (op text-of-quotation) (reg exp))
  (goto (reg continue))
ev-lambda
  (assign (reg unev) (op lambda-parameters) (reg exp))
  (assign (reg exp) (op lambda-body) (reg exp))
  (assign (reg val) (op make-procedure) (reg unev) (reg exp) (reg env))
  (branch-condition (label ev-lambda-error))
  (goto (reg continue))
ev-lambda-error
  ; TODO associate stack with the error in (reg val)
  (goto (reg continue))
ev-application
  ;; setup evaluation of the compound operator
  (save (reg continue))
  (save (reg env))
  (assign (reg unev) (op operands) (reg exp))
  (save (reg unev))
  (assign (reg exp) (op operator) (reg exp))
  (assign (reg continue) (label ev-appl-did-operator))
  (goto (label eval-dispatch))
ev-appl-did-operator
  ;; store the evaluated operator and enter operand evaluation loop
  (branch-condition (label ev-appl-did-operator-error))
  (restore (reg unev))  ; the operands
  (restore (reg env))
  (assign (reg argl) (op empty-arglist))
  (assign (reg proc) (reg val)) ; the operator
  (test (op no-operands?) (reg unev))
  (branch (label ev-apply-dispatch))
  (save (reg proc))
ev-appl-operand-loop
  ;; setup an operand to be evaluated
  (save (reg argl))
  (assign (reg exp) (op first-operand) (reg unev))
  (test (op last-operand?) (reg unev))
  (branch (label ev-appl-last-arg))
  (save (reg env))
  (save (reg unev))
  (assign (reg continue) (label ev-appl-accumulate-arg))
  (goto (label eval-dispatch))
ev-appl-did-operator-error
  ; TODO associate stack with the error in (reg val)
  (restore (reg unev)) ; restore registers to allow proper unwinding
  (restore (reg env))
  (goto (reg continue))
ev-appl-accumulate-arg
  ;; append evaluated operand to the end of argl and start the iteration
  ;; of the operand eval loop.
  (branch-condition (label ev-appl-accumulate-arg-error))
  (restore (reg unev))
  (restore (reg env))
  (restore (reg argl))
  (assign (reg argl) (op adjoin-arg) (reg val) (reg argl))
  (assign (reg unev) (op rest-operands) (reg unev))
  (goto (label ev-appl-operand-loop))
ev-appl-accumulate-arg-error
  ; TODO associate stack with the error in (reg val)
  (restore (reg unev)) ; restore registers to allow proper unwinding
  (restore (reg env))
  (restore (reg argl))
  (goto (reg continue))
ev-appl-last-arg
  ;; analogous to ev-appl-operand-loop
  (assign (reg continue) (label ev-appl-accum-last-arg))
  (goto (label eval-dispatch))
ev-appl-accum-last-arg
  ;; analogous to ev-appl-accumulate-arg but jumps to apply-dispatch
  (branch-condition (label ev-appl-accum-last-arg-error))
  (restore (reg argl))
  (assign (reg argl) (op adjoin-arg) (reg val) (reg argl))
  (restore (reg proc))
  (goto (label ev-apply-dispatch))
ev-appl-accum-last-arg-error
  ; TODO associate stack with the error in (reg val)
  (restore (reg argl)) ; restore registers to allow proper unwinding
  (restore (reg proc))
  (goto (reg continue))

ev-apply-dispatch
  ;; select the correct application label based on the the type of procedure
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-apply))
  (test (op compound-procedure?) (reg proc))
  (branch (label compound-apply))
  (goto (label unknown-procedure-type))
primitive-apply
  ;; procedure implementation is not implemented as machine instructions.
  ;; just assign the result to val and return to caller
  (assign (reg env) (op get-global-environment)) ; TODO fixme
  (assign (reg unev) (op primitive-procedure-parameters) (reg proc))
  (assign (reg env) (op extend-environment) (reg unev) (reg argl) (reg env))
  (assign (reg val) (op apply-primitive-procedure) (reg proc) (reg env))
  (branch-condition (label primitive-apply-error))
  (restore (reg continue))
  (goto (reg continue))
primitive-apply-error
  ; TODO associate stack with the error in (reg val)
  (restore (reg continue))
  (goto (reg continue))
compound-apply
  ;; extend env by binding extracted paramaters to argl values the start
  ;; the function body evalutaion. at this point continue has been set in
  ;; ev-application
  (assign (reg unev) (op procedure-parameters) (reg proc))
  (assign (reg env) (op procedure-environment) (reg proc))
  (assign (reg env) (op extend-environment) (reg unev) (reg argl) (reg env))
  (assign (reg unev) (op procedure-body) (reg proc))
  (goto (label ev-sequence))

ev-progn
  ;; an explicit begin that has to extract actions and set continue by
  ;; entering ev-sequence
  (assign (reg unev) (op progn-actions) (reg exp))
  (save (reg continue))
  (goto (label ev-sequence))

ev-sequence
  ;; while expressions remain we have to setup continuing the loop before
  ;; entering eval-dispatch
  (assign (reg exp) (op first-exp) (reg unev))
  (test (op last-exp?) (reg unev))
  (branch (label ev-sequence-last-exp))
  (save (reg unev))
  (save (reg env))
  (assign (reg continue) (label ev-sequence-continue))
  (goto (label eval-dispatch))
ev-sequence-continue
  (branch-condition (label ev-sequence-error)) ; test error before restore to preserve error stack
  (restore (reg env))
  (restore (reg unev))
  (assign (reg unev) (op rest-exps) (reg unev))
  (goto (label ev-sequence))
ev-sequence-error
  ; TODO associate stack with the error in (reg val)
  (restore (reg env)) ; restore registers to allow proper unwinding
  (restore (reg unev))
  (goto (reg continue))
ev-sequence-last-exp
  ;; restore jump label setup either by ev-application as the return
  ;; point after ev-begin.  avoiding unnecessary extra save/restore of
  ;; unev and env saves stack space in a tail-recursive function (this
  ;; was called a terminal expression in elps1 but is probably more
  ;; efficient because elps still pushed to the stack but did a premature
  ;; pop/return).
  (restore (reg continue))
  (goto (label eval-dispatch))

ev-if
  (save (reg exp)) ; save expression for after predicate evaluation
  (save (reg env))
  (save (reg continue))
  (assign (reg continue) (label ev-if-decide))
  (assign (reg exp) (op if-predicate) (reg exp))
  (goto (label eval-dispatch)) ; evaluate the predicate
ev-if-decide
  (branch-condition (label ev-if-decide-error)) ; test error before restore to preserve error stack
  (restore (reg continue))
  (restore (reg env))
  (restore (reg exp))
  (test (op true?) (reg val))
  (branch (label ev-if-consequent))
ev-if-alternative
  (assign (reg exp) (op if-alternative) (reg exp))
  (goto (label eval-dispatch))
ev-if-consequent
  (assign (reg exp) (op if-consequent) (reg exp))
  (goto (label eval-dispatch))
ev-if-decide-error
  ; TODO associate stack with the error in (reg val)
  (restore (reg continue)) ; restore registers to allow proper unwinding
  (restore (reg env))
  (restore (reg exp))
  (goto (reg continue))

ev-assignment
  (assign (reg unev) (op assignment-variable) (reg exp))
  (save (reg unev)) ; save variable for after value evaluation
  (assign (reg exp) (op assignment-value) (reg exp))
  (save (reg continue))
  (assign (reg continue) (label ev-assignment-1))
  (goto (label eval-dispatch))
ev-assignment-1
  (restore (reg continue))
  (restore (reg unev))
  (perform (op set-variable-value!) (reg unev) (reg val) (reg env))
  (assign (reg val) (const 'ok))
  (goto (reg continue))

;;ev-definition
;;  (assign (reg unev) (op definition-variable) (reg exp))
;;  (save (reg unev)) ; save variable for after value evaluation
;;  (assign (reg exp) (op definition-value) (reg exp))
;;  (save (reg env))
;;  (save (reg continue))
;;  (assign (reg continue) (label ev-definition-1))
;;  (goto (label eval-dispatch))
;;ev-definition-1
;;  (restore (reg continue))
;;  (restore (reg env))
;;  (restore (reg unev))
;;  (perform (op define-variable!) (reg unev) (reg val) (reg env))
;;  (assign (reg val) (const 'ok))
;;  (goto (reg continue))

unknown-expression-type
  (assign (reg val) (const unknown-expression-type-error))
  (goto (label signal-error))

unknown-procedure-type
  (restore (reg continue)) ; clean up stack from apply-dispatch
  (goto (label signal-error))

signal-error
  (perform (op user-print) (reg val))
  (goto (label done))

done
  (perform (op print-stack-statistics) (reg stack))
`
