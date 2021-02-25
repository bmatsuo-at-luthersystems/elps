(in-package 'evaluator)
(use-package 'lisp2)
(use-package 'adt)
(use-package 'runtime-error)
(use-package 'environ)
(use-package 'primitive)
(use-package 'procedure)
(use-package 'machine)

(export 'self-evaluating?)
(defun self-evaluating? (exp)
  (or (nil? exp)
      (int? exp)
      (float? exp)))
(export 'variable?)
(defun variable? (exp) (symbol? exp))
(export 'quoted?)
(defun quoted? (exp) (and (pair? exp) (equal? 'quote (car exp))))
(export 'assignment?)
(defun assignment? (exp) (equal? 'set! (car exp)))
(export 'definition?)
(defun definition? (exp) (equal? 'define (car exp)))
(export 'if?)
(defun if? (exp) (equal? 'if (car exp)))
(export 'lambda?)
(defun lambda? (exp) (equal? 'lambda (car exp)))
(export 'begin?)
(defun begin? (exp) (equal? 'begin (car exp)))
(export 'application?)
(defun application? (exp) (pair? exp))

(export 'operator)
(defun operator (exp) (car exp))
(export 'operands)
(defun operands (exp) (cdr exp))

(export 'text-of-quotation)
(defun text-of-quotation (exp) (cadr exp))

(export 'lambda-parameters)
(defun lambda-parameters (exp) (cadr exp))
(export 'lambda-body)
(defun lambda-body (exp) (cddr exp))

(export 'begin-actions)
(defun begin-actions (exp) (cdr exp))

(export 'first-exp)
(defun first-exp (exp) (car exp))
(export 'last-exp?)
(defun last-exp? (exp) (nil? (cdr exp)))
(export 'rest-exps)
(defun rest-exps (exp) (cdr exp))

(export 'if-predicate)
(defun if-predicate (exp) (cadr exp))
(export 'if-consequent)
(defun if-consequent (exp) (caddr exp))
(export 'if-alternative)
(defun if-alternative (exp) (cadddr exp))

(export 'assignment-variable)
(defun assignment-variable (exp) (cadr exp))
(export 'assignment-value)
(defun assignment-value (exp) (caddr exp))

(export 'definition-variable)
(defun definition-variable (exp) (caadr exp))
(export 'definition-value)
(defun definition-value (exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (concat (qlist lambda) (cons (cdadr exp) (cddr exp)))))

(export 'make-procedure)
(defun make-procedure (parameters body env)
  (new procedure parameters body env))
(export 'primitive-procedure?)
(defun primitive-procedure? (proc) (primitive? proc))
;; compound-procedure? is defined by package 'procedure
(export 'empty-arglist)
(defun empty-arglist () nil)
(export 'adjoin-arg)
(defun adjoin-arg (arg arg-list) (append arg-list arg))
(export 'apply-primitive-procedure)
(defun apply-primitive-procedure (proc args)
  (apply (primitive-implementation proc) args))

(export 'no-operands?)
(defun no-operands? (operands) (nil? operands))
(export 'first-operand)
(defun first-operand (operands) (car operands))
(export 'last-operand?)
(defun last-operand? (operands) (nil? (cdr operands)))
(export 'rest-operands)
(defun rest-operands (operands) (cdr operands))

(export 'eceval)
(defun eceval (global-environment)
  (make-machine
    (new table
         ;; syntax ops
         (cons 'self-evaluating? self-evaluating?)
         (cons 'variable? variable?)
         (cons 'quoted? quoted?)
         (cons 'assignment? assignment?)
         (cons 'definition? definition?)
         (cons 'if? if?)
         (cons 'lambda? lambda?)
         (cons 'begin? begin?)
         (cons 'application? application?)
         (cons 'text-of-quotation text-of-quotation)
         (cons 'lambda-parameters lambda-parameters)
         (cons 'lambda-body lambda-body)
         (cons 'operator operator)
         (cons 'operands operands)
         (cons 'begin-actions begin-actions)
         (cons 'if-predicate if-predicate)
         (cons 'if-consequent if-consequent)
         (cons 'if-alternative if-alternative)
         (cons 'assignment-variable assignment-variable)
         (cons 'assignment-value assignment-value)
         (cons 'definition-variable definition-variable)
         (cons 'definition-value definition-value)

         ;; somewhere inbetween
         (cons 'no-operands? no-operands?)
         (cons 'first-operand first-operand)
         (cons 'last-operand? last-operand?)
         (cons 'rest-operands rest-operands)
         (cons 'first-exp first-exp)
         (cons 'last-exp? last-exp?)
         (cons 'rest-exps rest-exps)

         ;; procedure ops
         (cons 'primitive-procedure? primitive-procedure?)
         (cons 'apply-primitive-procedure apply-primitive-procedure)
         (cons 'compound-procedure? compound-procedure?)
         (cons 'make-procedure make-procedure)
         (cons 'procedure-parameters procedure-parameters)
         (cons 'procedure-environment procedure-environment)
         (cons 'procedure-body procedure-body)
         (cons 'empty-arglist empty-arglist)
         (cons 'adjoin-arg adjoin-arg)

         ;; error-handling ops
         (cons 'runtime-error? runtime-error?)
         (cons 'runtime-error-condition runtime-error-condition)
         (cons 'runtime-error-value runtime-error-value)
         (cons 'runtime-error-stack runtime-error-stack)
         (cons 'set-runtime-error-stack! set-runtime-error-stack!)

         ;; utility ops
         (cons 'true? true?) ; the lisp2 version of true?
         (cons 'cons cons)

         ;; environ ops
         (cons 'get-global-environment (lambda () the-global-environment))
         (cons 'lookup-variable-value lookup-variable-value)
         (cons 'set-variable-value! set-variable-value!)
         (cons 'extend-environment extend-environment)
         (cons 'define-variable! define-variable!)

         ;; repl ops
         (cons 'prompt-for-input debug-print)
         (cons 'read (lambda () (error 'unimplemented "read operation not implemented")))
         (cons 'announce-output debug-print)
         (cons 'user-print debug-print)
         )
    (qlist
      (goto (label execute-program))

      read-eval-print-loop
        (perform (op initialize-stack))
        (perform (op prompt-for-input) (const ";;; EC-Eval input:"))
        (assign exp (op read))
        (assign env (op get-global-environment))
        (assign continue (label print-result))
        (goto (label eval-dispatch))
      print-result
        (perform (op announce-output) (const ";;; EC-Eval value:"))
        (perform (op user-print) (reg val))
        (goto (label read-eval-print-loop))

      execute-program
        (perform (op initialize-stack))
        (assign unev (reg program))
        (assign env (op get-global-environment))
        (assign continue (label done))
        (save continue)
        (goto (label ev-sequence))

      eval-dispatch
        (test (op self-evaluating?) (reg exp))
        (branch (label ev-self-eval))
        (test (op variable?) (reg exp))
        (branch (label ev-variable))
        (test (op quoted?) (reg exp))
        (branch (label ev-quoted))
        (test (op assignment?) (reg exp))
        (branch (label ev-assignment))
        (test (op definition?) (reg exp))
        (branch (label ev-definition))
        (test (op if?) (reg exp))
        (branch (label ev-if))
        (test (op lambda?) (reg exp))
        (branch (label ev-lambda))
        (test (op begin?) (reg exp))
        (branch (label ev-begin))
        (test (op application?) (reg exp))
        (branch (label ev-application))
        (goto (label unknown-expression-type))
      ev-self-eval
        (assign val (reg exp))
        (goto (reg continue))
      ev-variable
        (assign val (op lookup-variable-value) (reg exp) (reg env))
        (test (op runtime-error?) (reg val))
        (branch (label ev-variable-error))
        (goto (reg continue))
      ev-variable-error
        ; TODO associate stack with the error in (reg val)
        (goto (reg continue))
      ev-quoted
        (assign val (op text-of-quotation) (reg exp))
        (goto (reg continue))
      ev-lambda
        (assign unev (op lambda-parameters) (reg exp))
        (assign exp (op lambda-body) (reg exp))
        (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
        (test (op runtime-error?) (reg val))
        (branch (label ev-lambda-error))
        (goto (reg continue))
      ev-lambda-error
        ; TODO associate stack with the error in (reg val)
        (goto (reg continue))
      ev-application
        ;; setup evaluation of the compound operator
        (save continue)
        (save env)
        (assign unev (op operands) (reg exp))
        (save unev)
        (assign exp (op operator) (reg exp))
        (assign continue (label ev-appl-did-operator))
        (goto (label eval-dispatch))
      ev-appl-did-operator
        ;; store the evaluated operator and enter operand evaluation loop
        (test (op runtime-error?) (reg val))
        (branch (label ev-appl-did-operator-error))
        (restore unev)  ; the operands
        (restore env)
        (assign argl (op empty-arglist))
        (assign proc (reg val)) ; the operator
        (test (op no-operands?) (reg unev))
        (branch (label ev-apply-dispatch))
        (save proc)
      ev-appl-operand-loop
        ;; setup an operand to be evaluated
        (save argl)
        (assign exp (op first-operand) (reg unev))
        (test (op last-operand?) (reg unev))
        (branch (label ev-appl-last-arg))
        (save env)
        (save unev)
        (assign continue (label ev-appl-accumulate-arg))
        (goto (label eval-dispatch))
      ev-appl-did-operator-error
        ; TODO associate stack with the error in (reg val)
        (restore unev) ; restore registers to allow proper unwinding
        (restore env)
        (goto (reg continue))
      ev-appl-accumulate-arg
        ;; append evaluated operand to the end of argl and start the iteration
        ;; of the operand eval loop.
        (test (op runtime-error?) (reg val))
        (branch (label ev-appl-accumulate-arg-error))
        (restore unev)
        (restore env)
        (restore argl)
        (assign argl (op adjoin-arg) (reg val) (reg argl))
        (assign unev (op rest-operands) (reg unev))
        (goto (label ev-appl-operand-loop))
      ev-appl-accumulate-arg-error
        ; TODO associate stack with the error in (reg val)
        (restore unev) ; restore registers to allow proper unwinding
        (restore env)
        (restore argl)
        (goto (reg continue))
      ev-appl-last-arg
        ;; analogous to ev-appl-operand-loop
        (assign continue (label ev-appl-accum-last-arg))
        (goto (label eval-dispatch))
      ev-appl-accum-last-arg
        ;; analogous to ev-appl-accumulate-arg but jumps to apply-dispatch
        (test (op runtime-error?) (reg val))
        (branch (label ev-appl-accum-last-arg-error))
        (restore argl)
        (assign argl (op adjoin-arg) (reg val) (reg argl))
        (restore proc)
        (goto (label ev-apply-dispatch))
      ev-appl-accum-last-arg-error
        ; TODO associate stack with the error in (reg val)
        (restore argl) ; restore registers to allow proper unwinding
        (restore proc)
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
        (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
        (test (op runtime-error?) (reg val))
        (branch (label primitive-apply-error))
        (restore continue)
        (goto (reg continue))
      primitive-apply-error
        ; TODO associate stack with the error in (reg val)
        (restore continue)
        (goto (reg continue))
      compound-apply
        ;; extend env by binding extracted paramaters to argl values the start
        ;; the function body evalutaion. at this point continue has been set in
        ;; ev-application
        (assign unev (op procedure-parameters) (reg proc))
        (assign env (op procedure-environment) (reg proc))
        (assign env (op extend-environment) (reg unev) (reg argl) (reg env))
        (assign unev (op procedure-body) (reg proc))
        (goto (label ev-sequence))
      ev-begin
        ;; an explicit begin that has to extract actions and set continue by
        ;; entering ev-sequence
        (assign unev (op begin-actions) (reg exp))
        (save continue)
        (goto (label ev-sequence))

      ev-sequence
        ;; while expressions remain we have to setup continuing the loop before
        ;; entering eval-dispatch
        (assign exp (op first-exp) (reg unev))
        (test (op last-exp?) (reg unev))
        (branch (label ev-sequence-last-exp))
        (save unev)
        (save env)
        (assign continue (label ev-sequence-continue))
        (goto (label eval-dispatch))
      ev-sequence-continue
        ;; restore registers after eval and enter the next iteration.
        (test (op runtime-error?) (reg val)) ; test error before restore to preserve error stack
        (branch (label ev-sequence-error))
        (restore env)
        (restore unev)
        (assign unev (op rest-exps) (reg unev))
        (goto (label ev-sequence))
      ev-sequence-error
        ; TODO associate stack with the error in (reg val)
        (restore env) ; restore registers to allow proper unwinding
        (restore unev)
        (goto (reg continue))
      ev-sequence-last-exp
        ;; restore jump label setup either by ev-application as the return
        ;; point after ev-begin.  avoiding unnecessary extra save/restore of
        ;; unev and env saves stack space in a tail-recursive function (this
        ;; was called a terminal expression in elps1 but is probably more
        ;; efficient because elps still pushed to the stack but did a premature
        ;; pop/return).
        (restore continue)
        (goto (label eval-dispatch))

      ev-if
        (save exp) ; save expression for after predicate evaluation
        (save env)
        (save continue)
        (assign continue (label ev-if-decide))
        (assign exp (op if-predicate) (reg exp))
        (goto (label eval-dispatch)) ; evaluate the predicate
      ev-if-decide
        (test (op runtime-error?) (reg val)) ; test error before restore to preserve error stack
        (branch (label ev-if-decide-error))
        (restore continue)
        (restore env)
        (restore exp)
        (test (op true?) (reg val))
        (branch (label ev-if-consequent))
      ev-if-alternative
        (assign exp (op if-alternative) (reg exp))
        (goto (label eval-dispatch))
      ev-if-consequent
        (assign exp (op if-consequent) (reg exp))
        (goto (label eval-dispatch))
      ev-if-decide-error
        ; TODO associate stack with the error in (reg val)
        (restore continue) ; restore registers to allow proper unwinding
        (restore env)
        (restore exp)
        (goto (reg continue))

      ev-assignment
        (assign unev (op assignment-variable) (reg exp))
        (save unev) ; save variable for after value evaluation
        (assign exp (op assignment-value) (reg exp))
        (save env)
        (save continue)
        (assign continue (label ev-assignment-1))
        (goto (label eval-dispatch))
      ev-assignment-1
        (restore continue)
        (restore env)
        (restore unev)
        (perform (op set-variable-value!) (reg unev) (reg val) (reg env))
        (assign val (const 'ok))
        (goto (reg continue))

      ev-definition
        (assign unev (op definition-variable) (reg exp))
        (save unev) ; save variable for after value evaluation
        (assign exp (op definition-value) (reg exp))
        (save env)
        (save continue)
        (assign continue (label ev-definition-1))
        (goto (label eval-dispatch))
      ev-definition-1
        (restore continue)
        (restore env)
        (restore unev)
        (perform (op define-variable!) (reg unev) (reg val) (reg env))
        (assign val (const 'ok))
        (goto (reg continue))

      unknown-expression-type
        (assign val (const unknown-expression-type-error))
        (goto (label signal-error))

      unknown-procedure-type
        (restore continue) ; clean up stack from apply-dispatch
        (goto (label signal-error))

      signal-error
        (perform (op user-print) (reg val))
        (goto (label read-eval-print-loop))

      done
        (perform (op print-stack-statistics))
      )))

(defun init-eceval! (eceval-machine)
  (load-program! eceval-machine nil))

(export 'make-eceval-machine)
(defun make-eceval-machine ()
  (let ([m (eceval (setup-environment))])
    (init-eceval! m)
    m))

(export 'load-program!)
(defun load-program! (eceval-machine program)
  (set-register-contents! eceval-machine 'program program))


(export 'exec-program)
(defun exec-program (program &optional trace)
  (let ([m (make-eceval-machine)])
    (load-program! m program)
    ;;(call-method (get-register m 'continue) 'trace-on)
    ;;(call-method (get-register m 'unev) 'trace-on)
    ;;(call-method (get-register m 'argl) 'trace-on)
    ;;(call-method (get-register m 'val) 'trace-on)
    (if trace
      (call-method m 'trace-on)
      ())
    (start m)
    (get-register-contents m 'val)))

(assert (equal? 3 (exec-program (qlist (+ 1 2)) true)))
(assert (equal? 4 (exec-program (qlist
                                  (define (double n)
                                    (+ n n))
                                  (double 2)) true)))
;;(assert (equal? 2 (exec-program (qlist
;;                                  (define (fib n)
;;                                    (if (< n 2)
;;                                      n
;;                                      (+ (fib (- n 1)) (fib (- n 2)))))
;;                                  (fib 3)))))
(assert (equal? 3 (exec-program (qlist
                                  (define (fib n)
                                    (if (< n 2)
                                      n
                                      (+ (fib (- n 1)) (fib (- n 2)))))
                                  (fib 4)))))
;;(assert (equal? 5 (exec-program (qlist
;;                                  (define (fib n)
;;                                    (if (< n 2)
;;                                      n
;;                                      (+ (fib (- n 1)) (fib (- n 2)))))
;;                                  (fib 5)))))

;;(assert (equal? 120 (exec-program (qlist
;;                                    (define (fact n)
;;                                      (if (< n 2)
;;                                        n
;;                                        (* n (fact (- n 1)))))
;;                                    (fact 5)))))
;;
;;(assert (equal? 120 (exec-program (qlist
;;                                    (define (factr n i acc)
;;                                      (if (>= i n)
;;                                        (* i acc)
;;                                        (factr n (+ i 1) (* i acc))))
;;                                    (define (fact n)
;;                                      (if (<= n 0)
;;                                        0
;;                                        (factr n 1 1)))
;;                                    (fact 5)))))

;;(assert (equal? 3628800 (exec-program (qlist
;;                                        (define (fact n)
;;                                          (if (< n 2)
;;                                            n
;;                                            (* n (fact (- n 1)))))
;;                                        (fact 10)))))
;;
;;(assert (equal? 3628800 (exec-program (qlist
;;                                        (define (factr n i acc)
;;                                          (if (>= i n)
;;                                            (* i acc)
;;                                            (factr n (+ i 1) (* i acc))))
;;                                        (define (fact n)
;;                                          (if (<= n 0)
;;                                            0
;;                                            (factr n 1 1)))
;;                                        (fact 10)))))
