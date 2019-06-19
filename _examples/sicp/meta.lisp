(in-package 'sicp/meta)

(use-package 'testing)

; well... it looks like we want linked lists after all...
(set 'nil '())
(defun cons (a b)
  (labels ([set-car! (x) (set! a x)]
           [set-cdr! (x) (set! b x)])
  (lisp:list 'cons
             (lambda (op)
               (cond ((equal? op 'car) a)
                     ((equal? op 'cdr) b)
                     ((equal? op 'set-car!) set-car!)
                     ((equal? op 'set-cdr!) set-cdr!)
                     (:else (error 'invalid-operator "invalid operator -- CONS" op)))))))

(defun car (pair) (funcall (lisp:nth pair 1) 'car)) ; first
(defun cdr (pair) (funcall (lisp:nth pair 1) 'cdr))
(defun set-car! (pair a) (funcall (funcall (lisp:nth pair 1) 'set-car!) a))
(defun set-cdr! (pair b) (funcall (funcall (lisp:nth pair 1) 'set-cdr!) b))

(defun list? (val)
  (cond ((nil? val) true)
        ((lisp:list? val)
          (equal? 'cons (lisp:car val)))
        (:else false)))
(defun tagged-list? (exp tag)
  (if (list? exp)
    (equal? (car exp) tag)
    false))
(defun list (&rest xs)
  (if (nil? xs)
    nil
    (cons (lisp:car xs) (lisp:apply list (lisp:cdr xs))))) ; inefficient w/o slicing

(defun cons-for-each (proc pair)
  (if (nil? pair)
    nil
    (progn (funcall proc (car pair))
           (cons-for-each proc (cdr pair)))))
(defun cons-map (proc pair)
  (if (nil? pair)
    nil
    (cons (funcall proc (car pair))
          (cons-map proc (cdr pair)))))
(defun cons-debug (pair &optional prefix)
  (cond ((nil? pair) nil)
        ((list? (car pair))
          (debug-print (if prefix prefix ""))
          (cons-debug (car pair) (concat 'string prefix "-"))
          (cons-debug (cdr pair) prefix))
        (:else
          (if prefix
            (debug-print prefix (car pair))
            (debug-print "" (car pair)))
          (cons-debug (cdr pair) prefix))))

(defun cadr (pair) (car (cdr pair))) ; second
(defun caadr (pair) (car (car (cdr pair))))
(defun cdadr (pair) (cdr (cadr pair)))
(defun cddr (pair) (cdr (cdr pair)))
(defun caddr (pair) (car (cdr (cdr pair)))) ; third
(defun cadddr (pair) (car (cdr (cdr (cdr pair))))) ; fourth

(defun cons-length (pair &optional prefix-length)
  (if (nil? pair)
    (if (nil? prefix-length) 0 prefix-length)
    (cons-length (cdr pair) (+ 1 (if (nil? prefix-length) 0 prefix-length)))))

(assert= 4 (cons-length (list 1 1 1 1)))
(assert= 4 (cadr (cons-map #^(* % %) (list 1 2 3 4))))

(defun eval (exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
          (make-procedure (lambda-parameters exp)
                          (lambda-body exp)
                          env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
          (apply (eval (operator exp) env)
                 (list-of-values (operands exp) env)))
        (:else (error 'invalid-expression
                      "unrecognized expression type -- EVAL" exp))))

(defun apply (procedure arguments)
  (cond ((primitive-procedure? procedure)
          (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
          (eval-sequence
            (procedure-body procedure)
            (extend-environment
              (procedure-parameters procedure)
              arguments
              (procedure-environment procedure))))
        (:else (error 'invalid-procedure
                      "unrecognized procedure type -- APPLY" procedure))))

(defun analyze-application (exp)
  (let ([get-proc (analyze (operator exp))]
        [arg-procs (cons-map 'analyze (operands exp))])
    (lambda (env)
      (execute-application (funcall get-proc env)
                           (cons-map (lambda (arg-proc) (funcall arg-proc env))
                                     arg-procs)))))

(defun execute-application (proc args)
  (cond ((primitive-procedure? proc)
          (apply-primitive-procedure proc args))
        ((compond-procedure? proc)
          (funcall (procedure-body proc)
                   (extend-environment (procedure-parameters proc)
                                       args
                                       (procedure-environment proc))))
    (:else (error 'invalid-procedure "unrecognized procedure type -- EXECUTE-APPLICATION"))))

(defun list-of-values (exps env)
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(defun eval-if (exp env)
  (if (true? (eval (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

(defun analyze-if (exp)
  (let ([predicate (analyze (if-predicate exp))]
        [consequent (analyze (if-consequent exp))]
        [alternative (analyze (if-alternative exp))])
    (lambda (env)
      (if (true? (funcall predicate env))
        (funcall consequent env)
        (funcall alternative env)))))

(defun eval-sequence (exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (:else (eval (first-exp exps) env)
               (eval-sequence (rest-exps exps) env))))

(defun analyze-sequence (exps)
  (labels ([in-sequence (proc1 proc2)
              (lambda (env)
                (funcall proc1 env)
                (funcall proc2 env))]
           [loop (first-proc rem-procs)
              (if (nil? rem-procs)
                first-proc
                (loop (in-sequence fisrt-proc (car rem-procs))
                      (cdr rem-procs)))])
    (let ([procs (cons-map analyze procs)])
      (if (nil? procs)
        (error 'empty-sequence "empty sequnce -- ANALYZE")
        (loop (car procs) (cdr procs))))))

(defun eval-assignment (exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(defun analyze-assignment (exp)
  (let ([var (assignment-variable exp)]
        [val-fun (analyze (assignment-value exp))])
    (lambda (env)
      (set-variable-value var (funcall val-fun))
      'ok)))

(defun eval-definition (exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

(defun analyze-definition (exp)
  (let ([var (definition-variable exp)]
        [val-fun (analyze (definition-value exp))])
    (lambda (env)
      (define-variable! var (funcall val-fun env) env)
      'ok)))

(defun self-evaluating? (exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (:else false)))

(defun variable? (exp) (symbol? exp))

(defun quoted? (exp) (tagged-list? exp 'quote))
(defun text-of-quotation (exp) (second exp)) ; ?

(defun assignment? (exp) (tagged-list? exp 'set!))
(defun assignment-variable (exp) (cadr exp))
(defun assignment-value (exp) (caddr exp))

(defun definition? (exp) (tagged-list? exp 'define))
(defun definition-variable (exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))
(defun definition-value (exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp)
                 (cddr exp))))

(defun lambda? (exp) (tagged-list? exp 'lambda))
(defun lambda-parameters (exp) (cadr exp))
(defun lambda-body (exp) (cddr exp))

(defun make-lambda (parameters body)
  (list 'lambda parameters body))

(defun if? (exp) (tagged-list? exp 'if))
(defun if-predicate (exp) (cadr exp))
(defun if-consequent (exp) (caddr exp))
(defun if-alternative (exp)
  (if (> 4 (cons-length exp))
    'false
    (cadddr exp)))

(defun make-if (predicate consequent alternative)
  (list 'if predicate consequent alternative))

(defun begin? (exp) (tagged-list? exp 'begin))
(defun begin-actions (exp) (cdr exp))

(defun last-exp? (exp) (nil? (cdr exp)))
(defun first-exp (exp) (car exp))
(defun rest-exp (exp) (cdr exp))

(defun sequence->exp (seq)
  (cond ((nil? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (:else (make-begin seq))))

(defun application? (exp) (list? exp))
(defun operator (exp) (car exp))
(defun operands (exp) (cdr exp))

(defun no-operands? (ops) (nil? ops))
(defun first-operand (ops) (car ops))
(defun rest-operands (ops) (cdr ops))

(defun cond? (exp) (tagged-list? exp 'cond))
(defun cond-clauses (exp) (cdr exp))
(defun cond-else-clause? (clause) (equal? (cond-predicate clause) 'else))
(defun cond-predicate (clause) (car clause))
(defun cond-actions (clause) (cdr clause))

(defun cond->if (exp)
  (expand-clauses (cond-clauses exp)))

(defun expand-clauses (clauses)
  (if (nil? clauses)
    'false
    (let ([first (car clauses)]
          [rest (cdr clauses)])
      (if (cond-else-clause? first)
        (if (nil? rest)
          (sequence->exp (cond-actions first))
          (error 'premature-else "ELSE clause isn't the last -- COND->IF" clauses))
        (make-if (cond-predicate first)
                 (sequence->exp (cond-actions first))
                 (expand-clauses rest))))))

(defun true? (x)
  (not (equal? x false)))

(defun false? (x)
  (equal? x false))

(defun make-procedure (parameters body env)
  (list 'procedure parameters body env))

(defun analyze-lambda (exp)
  (let ([vars       (lambda-parameters exp)]
        [body-proc  (analyze-sequence (lambda-body exp))])
    (lambda (env) (make-procedure vars body-proc env))))

(defun compound-procedure? (p)
  (tagged-list? p 'procedure))
(defun procedure-parameters (p) (cadr p))
(defun procedure-body (p) (caddr p))
(defun procedure-environment (p) (cadddr p))

(defun lookup-variable-value (var env)
  (labels ([env-loop (env)
              (if (equal? env the-empty-environment)
                (error 'unbound-variable "unbound variable" var)
                (frame-locate-binding (first-frame env) var
                                      (lambda (bindings) (bound-value (car bindings)))
                                      (lambda () (env-loop (envclosing-environment env)))))])
    (env-loop env)))

(defun extend-environment (variables values base-env)
  (if (= (cons-length variables) (cons-length values))
    (cons (make-frame variables values) base-env)
    (if (< (cons-length variables) (cons-length values))
      (error 'invalid-arguments "too many arguments supplied" variables values)
      (error 'invalid-arguments "too few arguments supplied" variables values))))

(defun set-variable-value! (var value env)
  (labels ([env-loop (env)
              (if (equal? env the-empty-environment)
                (error 'unbound-variable "unbound variable" var)
                (frame-locate-binding (first-frame env) var
                                      (lambda (bindings) (set-car! bindings (cons var val)))
                                      (lambda () (env-loop (envclosing-environment env)))))])
    (env-loop env)))

(defun define-variable! (var val env)
  (let ([frame (first-frame env)])
    (frame-locate-binding frame var
                          (lambda (bindings) (set-car! bindings (cons var val)))
                          (lambda () (add-binding-to-frame! var val frame)))))

(defun enclosing-environment (env) (cdr env))
(defun first-frame (env) (car env))
(set 'the-empty-environment '())

(defun make-binding (variable value) (cons variable value))
(defun make-binding-list (variables values)
  (if (nil? variables)
    (if (nil? values)
      nil
      (error 'invalid-arguments "too many arguments supplied" variables values))
    (if (nil? values)
      (error 'invalid-arguments "too few arguments supplied" variables values)
      (cons (cons (car variables)
                  (car values))
            (make-binding-list (cdr variables)
                               (cdr values))))))

(defun bound-variable (bind) (car bind))
(defun bound-value (bind) (cdr bind))

(defun make-frame (variables values)
  (let* ([bindings (make-binding-list variables values)]
         [add-binding! (lambda (variable value)
                         (set! bindings (cons (make-binding variable value)
                                              bindings))
                         'ok)])
    (lambda (op)
      (cond ((equal? op 'bindings) bindings)
            ((equal? op 'add-binding!) add-binding!)
            (:else (error 'invalid-op "unknown operation -- FRAME"))))))
(defun frame-bindings (frame) (frame 'bindings))
(defun add-binding-to-frame! (variable value frame)
  (funcall (funcall frame 'add-binding!) variable value))

(defun frame-locate-binding (frame var proc-found proc-not-found)
  (labels ([scan (bindings)
              (cond
                ((nil? bindings)
                  (funcall proc-not-found))
                ((equal? var (bound-variable (car bindings)))
                  (funcall proc-found bindings))
                (:else
                  (scan (cdr bindings))))])
    (scan (frame-bindings frame))))

(set 'primitive-procedures
     (list (list 'car car)
           (list 'cdr cdr)
           (list 'cons cons)
           (list 'nil? nil?)))

(defun primitive-procedure-names ()
  (cons-map 'car primitive-procedures))
(defun primitive-procedure-objects ()
  (cons-map
       (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(defun primitive-procedure? (proc)
  (tagged-list? proc 'primitive))

(defun primitive-implementation (proc)
  (cadr proc))

(defun apply-primitive-procedure (proc args)
  (let ([argv (vector)])
    (labels ([push-cons (x)
                (if (nil? x)
                  nil
                  (progn
                    (set! argv (append! argv (car x)))
                    (push-cons (cdr x))))])
      (push-cons args)
      (lisp:apply (primitive-implementation proc)
              (map 'list identity argv)))))

(defun setup-environment ()
  (let ([initial-env (extend-environment (primitive-procedure-names)
                                         (primitive-procedure-objects)
                                         the-empty-environment)])
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(set 'the-global-environment (setup-environment))

; note: standard quoted-list syntax '(a b c) does not work here because we are
; using a custom cons to form linked lists.
(assert= 3 (eval 3 the-global-environment))
(assert-equal 'ok (eval (list 'define 'x 1) the-global-environment))
(assert= 1 (eval 'x the-global-environment))
(assert= 1 (eval (list 'if (list 'nil? 'x) 2 'x) the-global-environment))

(defun analyze (exp)
  (cond ((self-evaluating? exp) (lambda (env) exp))
        ((variable? exp)        (lambda (env) (lookup-variable-value exp env)))
        ((quoted? exp)          (lambda (env) (text-of-quotation exp)))
        ((assignment? exp)      (analyze-assignment exp))
        ((definition? exp)      (analyze-definition exp))
        ((if? exp)              (analyze-if exp))
        ((lambda? exp)          (analyze-lambda exp))
        ((begin? exp)           (analyze-sequence (begin-actions exp)))
        ((application? exp)     (analyze-application exp))
        (:else (error 'unrecognized-expression "invalid expression -- ANALYZE"))))

; shadow the previously defined `eval`.
(defun eval (exp env)
  (funcall (analyze exp) env))

(assert= 3 (eval 3 the-global-environment))
(assert-equal 'ok (eval (list 'define 'x 1) the-global-environment))
(assert= 1 (eval 'x the-global-environment))
(assert= 1 (eval (list 'if (list 'nil? 'x) 2 'x) the-global-environment))

; registers

(defun make-register (register-name)
  (let ([contents 'UNASSIGNED])
    (labels ([get () contents]
             [set (value) (set! contents value)]
             [dispatch (sym) (cond ((equal? sym 'get) get)
                                   ((equal? sym 'put) set)
                                   (:else (error 'invalid-message sym)))])
      dispatch)))

(defun get-contents (register)
  (funcall (register 'get)))

(defun set-contents! (register value)
  (funcall (register 'set) value))

; stacks

(defun make-stack ()
  (let ([s nil])
    (labels ([push (x) (set! s (cons x s))]
             [pop ()
                  (if (nil? s)
                    (error 'empty-stack "pop called on an empty stack")
                    (let ([head (car s)])
                      (set! s (cdr s))
                      head))]
             [initialize ()
                         (set! s nil)
                         'done]
             [dispatch (message) (cond ((equal? message 'push) push)
                                       ((equal? message 'pop) pop)
                                       ((equal? message 'initialize) initialize)
                                       (:else (error 'invalid-message message)))]])
      dispatch)))

(defun push! (stack x)
  (funcall (stack 'push) x))

(defun pop! (stack x)
  (funcall (stack 'pop) x))

; machines

(defun make-machine (register-names ops controller)
  (let ([machine (make-new-machine)])
    (cons-for-each (lambda (register-name)
                     (funcall (machine 'allocate-register) register-name))
                   register-names)
    (funcall (machine 'install-operations) ops)
    (funcall (machine 'install-instruction-sequence)
             (assemble controller machine))
    machine))

(defun start! (machine)
  (funcall (machine 'start)))

(defun get-register (machine)
  (funcall (machine 'get-register)))

(defun get-register-contents (machine register-name)
  (get-contents (get-register machine register-name)))

(defun set-register-contents! (machine register-name value)
  (set-contents! (get-register machine register-name) value))

(defun make-new-machine ()
  (let ([pc (make-register 'pc)]
        [flag (make-register 'flag)]
        [stack (make-stack)]
        [instruction-sequence nil])
    (let ([ops (sorted-map 'initialize-stack (lambda () (funcall (stack 'initialize))))]
          [register-table (sorted-map 'pc pc
                                      'flag flag)])
      (labels ([allocate-register! (name)
                  (if (key? register-table name)
                    (error 'allocate-register-duplicate name)
                    (assoc! register-table name (make-register name)))]
               [lookup-register (name)
                  (let ([val (get register-table name)])
                    (or val (error 'unknown-register name)))]
               [install-instructions! (seq)
                  (set! instruction-sequence seq)]
               [install-operations! (pairs)
                  (cons-for-each (lambda (pair) (assoc! ops (car pair) (cdr pair)))
                                 pairs)]
               [start! ()
                  (set-contents! pc instruction-sequence)
                  (execute!)]
               [execute! ()
                  (let ([insts (get-contents pc)])
                    (if (nil? insts)
                      'done
                      (progn
                        (funcall (instruction-execution-proc (car insts)))
                        (execute!))))]
               [dispatch (msg)
                  (cond ((equal? msg 'start!) start!)
                        ((equal? msg 'install-instruction-sequence!) install-instructions!)
                        ((equal? msg 'install-operations!) install-operations!)
                        ((equal? msg 'allocate-register!) allocate-register!)
                        ((equal? msg 'get-register) lookup-register)
                        ((equal? msg 'stack) (lambda () stack))
                        ((equal? msg 'operations) (lambda () ops))
                        (:else (error 'invalid-message msg)))])
        dispatch))))
