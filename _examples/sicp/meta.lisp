(in-package 'sicp/meta)

(use-package 'testing)

(defmacro send (receiver message &rest args)
  (quasiquote (funcall (funcall (unquote receiver)
                                (unquote message))
                       (unquote-splicing args))))

; well... it looks like we want linked lists after all...
(set 'nil '())
(defun cons (a b)
  (labels ([set-car! (x) (set! a x)]
           [set-cdr! (x) (set! b x)]
           [string-internal ()
                            (let ([a (cond ((nil? a) "()")
                                           ((list? a) (send (lisp:nth a 1) 'string))
                                           (:else a))])
                              (cond ((nil? b) (format-string "{}" a))
                                    ((list? b) (format-string "{} {}" a (send (lisp:nth b 1) 'string-internal)))
                                    (:else (format-string "{} . {}" a b))))]
           [string () (format-string "({})" (string-internal))])
  (lisp:list 'cons
             (lambda (op)
               (cond ((equal? op 'car) (lambda () a))
                     ((equal? op 'cdr) (lambda () b))
                     ((equal? op 'set-car!) set-car!)
                     ((equal? op 'set-cdr!) set-cdr!)
                     ((equal? op 'string) string)
                     ((equal? op 'string-internal) string-internal)
                     (:else (error 'invalid-operator "invalid operator -- CONS" op)))))))

(defun format-value (x)
  (cond ((nil? x) "'()")
        ((list? x) (format-cons x))
        (:else (format-string "{}" x))))

(defun equal-cons? (a b)
  (cond ((and (nil? a) (nil? b)) true)
        ((and (list? a) (list? b))
         (and (not (or (nil? a) (nil? b)))
              (equal-cons? (car a) (car b))
              (equal-cons? (cdr a) (cdr b))))
        (:else (equal? a b))))
(defun format-cons (pair) (send (lisp:nth pair 1) 'string))
(defun car (pair) (send (lisp:nth pair 1) 'car)) ; first
(defun cdr (pair) (send (lisp:nth pair 1) 'cdr))
(defun set-car! (pair a) (send (lisp:nth pair 1) 'set-car! a))
(defun set-cdr! (pair b) (send (lisp:nth pair 1) 'set-cdr! b))

(defun drop-while (predicate lis)
  (cond ((nil? lis) nil)
        ((funcall predicate (car lis)) (drop-while (cdr lis)))
        (:else lis)))

(defun reject! (pred lis)
  (cond ((nil? lis) '())
        ((funcall pred (car lis)) (cdr lis))
        (:else
          (set-cdr! lis (reject! pred (cdr lis)))
          lis)))

(defun insert! (lis elem n skip)
  (cond ((nil? lis) (error 'invalid-list "nil list"))
        ((funcall skip (car lis))
         (set-cdr! lis (insert! (cdr lis) elem n skip))
         lis)
        ((< 0 n)
         (set-cdr! lis (insert! (cdr lis) elem (- n 1) skip))
         lis)
        ((= 0 n)
         (let ([tail (cons (car lis) (cdr lis))])
           (set-car! lis elem)
           (set-cdr! lis tail)
           lis))
        (:else (error 'invalid-index n))))

(defun inspect (x)
  (cond ((nil? x) (format-string "{}" ()))
        ((list? x) (send (lisp:nth x 1) 'string))
        (:else (format-string "{}" x))))

(defun make-lisp-list (pair)
  (let ([v (vector)])
    (labels ( [make (pair)
                  (cond ((nil? pair) v)
                        ((list? (cdr pair))
                          (append! v (car pair))
                          (make (cdr pair)))
                        (:else
                          (append! v (car pair))
                          (append! v (cdr pair))))])
      (lisp:map 'list identity (make pair)))))
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
(defun tree (&rest xs)
  (lisp:apply list (map 'list
                        (lambda (x)
                          (cond ((nil? x) nil)
                                ((lisp:list? x) (lisp:apply tree x))
                                (:else x)))
                        xs)))
(defmacro qcons (&rest xs)
  (let ([lis (lisp:apply tree xs)])
    (quasiquote (lisp:list (unquote-splicing lis)))))

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
(defun assoc-get (pair key)
  (let ([item (car pair)])
    (if (equal? key (car item))
      item
      (assoc-get (cdr pair)))))

(defun cadr (pair) (car (cdr pair))) ; second
(defun caadr (pair) (car (car (cdr pair))))
(defun cdadr (pair) (cdr (cadr pair)))
(defun cddr (pair) (cdr (cdr pair)))
(defun caddr (pair) (car (cdr (cdr pair)))) ; third
(defun cadddr (pair) (car (cdr (cdr (cdr pair))))) ; fourth

(assert-equal '+ (car (car (qcons (+ 1 1) abc x))))
(assert-equal 'abc (cadr (car (cdr (qcons (+ 1 1) (1 abc) x)))))

(defun cons-length (pair &optional prefix-length)
  (if (nil? pair)
    (if (nil? prefix-length) 0 prefix-length)
    (cons-length (cdr pair) (+ 1 (if (nil? prefix-length) 0 prefix-length)))))

(assert= 4 (cons-length (list 1 1 1 1)))
(assert= 4 (cadr (cons-map #^(* % %) (list 1 2 3 4))))
(debug-print (format-cons (qcons (+ 1 1) (1 abc) x)))

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
                      "unrecognized expression type -- EVAL" (inspect exp)))))

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
                      "unrecognized procedure type -- APPLY" (inspect procedure)))))

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
  (send frame 'add-binding! variable value))

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
  (send analyze exp env))

(assert= 3 (eval 3 the-global-environment))
(assert-equal 'ok (eval (list 'define 'x 1) the-global-environment))
(assert= 1 (eval 'x the-global-environment))
(assert= 1 (eval (list 'if (list 'nil? 'x) 2 'x) the-global-environment))

; registers

(defun make-register (register-name)
  (let ([contents 'UNASSIGNED]
        [tracing false])
    (labels ([get () contents]
             [put! (value)
                (if tracing
                  (debug-print 'register register-name (format-value contents) '-> (format-value value) )
                  ())
                (set! contents value)]
             [trace-on! () (set! tracing true)]
             [trace-off! () (set! tracing false)]
             [dispatch (sym) (cond ((equal? sym 'get) get)
                                   ((equal? sym 'put!) put!)
                                   ((equal? sym 'trace-on!) trace-on!)
                                   ((equal? sym 'trace-off!) trace-off!)
                                   (:else (error 'invalid-message sym)))])
      dispatch)))

(defun get-contents (register)
  (send register 'get))

(defun set-contents! (register value)
  (send register 'put! value))

; stacks

(defun make-stack ()
  (let ([s nil]
        [num-pushes 0]
        [max-depth 0]
        [current-depth 0])
    (labels ([push (x)
               (set! s (cons x s))
               (set! num-pushes (+ 1 num-pushes))
               (set! current-depth (+ 1 current-depth))
               (set! max-depth (max current-depth max-depth))]
             [pop ()
               (if (nil? s)
                 (error 'empty-stack "pop called on an empty stack")
                 (let ([head (car s)])
                   (set! s (cdr s))
                   (set! current-depth (- current-depth 1))
                   head))]
             [initialize ()
               (set! s nil)
               (set! num-pushes 0)
               (set! max-depth 0)
               (set! current-depth 0)
               'done]
             [print-statistics ()
               (debug-print)
               (debug-print 'total-pushes '= num-pushes)
               (debug-print 'maximum-depth '= max-depth)]
             [dispatch (message)
               (cond ((equal? message 'push) push)
                     ((equal? message 'pop) pop)
                     ((equal? message 'initialize) initialize)
                     ((equal? message 'print-statistics) print-statistics)
                     (:else (error 'invalid-message message)))])
      dispatch)))

(defun push! (stack x)
  (send stack 'push x))

(defun pop! (stack)
  (send stack 'pop))

; machines

(defun make-machine (register-names ops controller)
  (let ([machine (make-new-machine)])
    (cons-for-each (lambda (register-name)
                     (send machine 'allocate-register! register-name))
                   register-names)
    (send machine 'install-operations! ops)
    (send machine 'install-instruction-sequence!
          (assemble controller machine))
    machine))

(defun install-breakpoint! (machine label n)
  (send machine 'install-breakpoint! label n))

(defun cancel-breakpoint! (machine label n)
  (send machine 'cancel-breakpoint! label n))

(defun cancel-all-breakpoints! (machine label n)
  (send machine 'cancel-all-breakpoints! label n))

(defun start! (machine)
  (send machine 'start!))

(defun proceed-machine! (machine)
  (send machine 'proceed!))

(defun trace-on! (machine)
  (send machine 'trace-on!))

(defun trace-off! (machine)
  (send machine 'trace-on!))

(defun get-register (machine register-name)
  (send machine 'get-register register-name))

(defun get-register-contents (machine register-name)
  (get-contents (get-register machine register-name)))

(defun set-register-contents! (machine register-name value)
  (set-contents! (get-register machine register-name) value))

(defun trace-on-register! (machine register-name)
  (send (get-register machine register-name) 'trace-on!))

(defun trace-off-register! (machine register-name)
  (send (get-register machine register-name) 'trace-off!))

(defun make-new-machine ()
  (let ([pc (make-register 'pc)]
        [flag (make-register 'flag)]
        [ca (make-register 'ca)]
        [cd (make-register 'cd)]
        [stack (make-stack)]
        [instruction-sequence nil]
        [instruction-execution-count 0]
        [break false]
        [tracing false])

    (let ([ops (sorted-map 'initialize-stack! (lambda () (send stack 'initialize))
                           'print-stack-statistics (lambda () (send stack 'print-statistics))
                           'print-execution-statistics (lambda ()
                                                         (debug-print)
                                                         (debug-print 'instructions-executed '=
                                                                      instruction-execution-count)))]
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
                  (cons-for-each (lambda (pair) (assoc! ops (car pair) (cadr pair)))
                                 pairs)]
               [install-breakpoint! (label n)
                  (if (<= n 0)
                    (error 'invalid-breakpoint-offset n)
                    (let ([pos (drop-while (lambda (inst) (not (equal? label (instruction-text inst))))
                                           instruction-sequence)])
                      (if (nil? pos)
                        (error 'label-does-not-exist label)
                        (let ([inst (make-phony-instruction (list 'break label n))]
                              [execution-proc (lambda ()
                                                (set! break true)
                                                (advance-pc! pc))])
                          (set-instruction-execution-proc! inst execution-proc)
                          (insert! pos inst n phony-instruction?)))))]
               [cancel-breakpoint! (label n)
                  (reject! (lambda (inst) (equal-cons? (instruction-text inst)
                                                       (list 'break label n)))
                           instruction-sequence)]
               [cancel-all-breakpoints! ()
                  (reject! (lambda (inst)
                             (let ([text (instruction-text inst)])
                               (and (list? text)
                                    (equal? 'break (car text)))))
                           instruction-sequence)]
               [proceed! ()
                  (if break
                    (progn
                      (set! break false)
                      (execute!))
                    (error 'no-break "machine is idle"))]
               [start! ()
                  (reset-execution-statistics!)
                  (set-contents! pc instruction-sequence)
                  (execute!)]
               [trace-instruction (inst)
                  (let ([text (instruction-text inst)])
                    (if (list? text)
                      (debug-print (format-cons text))
                      (debug-print text)))]
               [execute! ()
                  (let ([insts (get-contents pc)])
                    (if (nil? insts)
                      'done
                      (let* ([inst (car insts)]
                             [proc (instruction-execution-proc inst)])
                        (if tracing (trace-instruction inst) ())
                        (if proc
                          (progn
                            (funcall (instruction-execution-proc inst))
                            (set! instruction-execution-count (+ 1 instruction-execution-count)))
                          (advance-pc! pc)) ; pseudo-instructions can't otherwise advance the pc.
                        (if break
                          'break
                          (execute!)))))]
               [print-execution-statistics ()
                  (debug-print)
                  (debug-print 'instructions-executed '= instruction-execution-count)]
               [reset-execution-statistics! ()
                  (set! instruction-execution-count 0)]
               [trace-on! ()
                  (set! tracing true)]
               [trace-off! ()
                  (set! tracing false)]
               [dispatch (msg)
                  (cond ((equal? msg 'start!) start!)
                        ((equal? msg 'proceed!) proceed!)
                        ((equal? msg 'install-instruction-sequence!) install-instructions!)
                        ((equal? msg 'install-operations!) install-operations!)
                        ((equal? msg 'install-breakpoint!) install-breakpoint!)
                        ((equal? msg 'cancel-breakpoint!) cancel-breakpoint!)
                        ((equal? msg 'cancel-all-breakpoints!) cancel-all-breakpoints!)
                        ((equal? msg 'allocate-register!) allocate-register!)
                        ((equal? msg 'get-register) lookup-register)
                        ((equal? msg 'stack) (lambda () stack))
                        ((equal? msg 'operations) (lambda () ops))
                        ((equal? msg 'print-execution-statistics) print-execution-statistics)
                        ((equal? msg 'reset-execution-statistics!) reset-execution-statistics!)
                        ((equal? msg 'trace-on!) trace-on!)
                        ((equal? msg 'trace-off!) trace-off!)
                        (:else (error 'invalid-message msg)))])
        dispatch))))

(defun update-insts! (insts labels machine)
  (let ([pc (get-register machine 'pc)]
        [flag (get-register machine 'flag)]
        [stack (send machine 'stack)]
        [ops (send machine 'operations)])
    (cons-for-each (lambda (inst)
                     (let ([proc (make-execution-procedure (instruction-text inst)
                                                           labels
                                                           machine
                                                           pc
                                                           flag
                                                           stack
                                                           ops)])
                       (set-instruction-execution-proc! inst proc)))
                   insts)))

; assembler

(defun assemble (controller-definition machine)
  (extract-labels controller-definition
                  (lambda (insts labels)
                    (update-insts! insts labels machine)
                    insts)))

(defun extract-labels (text receive)
  (if (nil? text)
    (funcall receive nil (make-jumptable))
    (extract-labels (cdr text)
                    (lambda (insts labels)
                      (let* ([next-inst (car text)]
                             [insts (cons (make-instruction next-inst) insts)])
                        (if (symbol? next-inst)
                            (progn
                              (set-instruction-phony! (car insts))
                              (send labels 'insert! next-inst insts))
                            ())
                        (receive insts labels))))))

; instruction

(defun make-instruction (text)
  (list text false))

(defun make-phony-instruction (text)
  (list text true))

(defun instruction-text (inst)
  (car inst))

(defun instruction-execution-proc (inst)
  (cddr inst))

(defun phony-instruction? (inst)
  (cadr inst))

(defun set-instruction-phony! (inst)
  (set-car! (cdr inst) true))

(defun set-instruction-execution-proc! (inst proc)
  (set-cdr! (cdr inst) proc))

; label

(defun make-jumptable ()
  (let ([table (sorted-map)])
    (labels ([insert! (label-name insts)
                (if (key? table label-name)
                  (error 'duplicate-label-definition label-name)
                  (assoc! table label-name insts))]
             [lookup (label-name)
                (if (key? table label-name)
                  (get table label-name)
                  (error 'unknown-label label-name))]
             [dispatch (msg)
                (cond ((equal? msg 'lookup) lookup)
                      ((equal? msg 'insert!) insert!)
                      (:else (error 'invalid-message msg)))])
      dispatch)))

(defun make-execution-procedure (inst labels machine pc flag stack ops)
  (cond ((symbol? inst) nil) ; labels are noop pseudo-instructions used only for tracing
        ((equal? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((equal? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((equal? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((equal? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((equal? (car inst) 'save)
         (make-save inst machine stack pc))
        ((equal? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((equal? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (:else (error 'unknown-instruction-type (inspect inst)))))

(defun advance-pc! (pc)
  (set-contents! pc (cdr (get-contents pc))))

(defun make-assign (inst machine labels operations pc)
  (let ([target (get-register machine (assign-reg-name inst))]
        [value-exp (assign-value-exp inst)])
    (let ([value-proc
            (if (operation-exp? value-exp)
              (make-operation-exp value-exp machine labels operations)
              (make-primitive-exp (car value-exp) machine labels))])
      (lambda ()
        (set-contents! target (funcall value-proc))
        (advance-pc! pc)))))

(defun assign-reg-name (assign-instruction)
  (cadr assign-instruction))

(defun assign-value-exp (assign-instruction)
  (cddr assign-instruction))

(defun make-test (inst machine labels operations flag pc)
  (let ([condition (test-condition inst)])
    (if (operation-exp? condition)
      (let ([condition-proc (make-operation-exp condition machine labels operations)])
        (lambda ()
          (set-contents! flag (funcall condition-proc))
          (advance-pc! pc)))
      (error 'bad-test-instruction (inspect inst)))))

(defun test-condition (test-instruction)
  (cdr test-instruction))

(defun make-branch (inst machine labels flag pc)
  (let ([dest (branch-dest inst)])
    (if (label-exp? dest)
      (let ([insts (send labels 'lookup (label-exp-label dest))])
        (lambda ()
          (if (get-contents flag)
            (set-contents! pc insts)
            (advance-pc! pc))))
      (error 'bad-branch-instruction (inspect inst)))))

(defun branch-dest (branch-instruction)
  (cadr branch-instruction))

(defun make-goto (inst machine labels pc)
  (let ([dest (goto-dest inst)])
    (cond ((label-exp? dest)
           (let ([insts (send labels 'lookup (label-exp-label dest))])
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ([reg (get-register machine (register-exp-reg dest))])
             (lambda () (set-contents! pc (get-contents reg)))))
          (:else (error 'bad-goto-instruction (inspect inst))))))

(defun goto-dest (goto-instruction)
  (cadr goto-instruction))

(defun make-save (inst machine stack pc)
  (let ([reg (get-register machine (stack-inst-reg-name inst))])
    (lambda ()
      (push! stack (get-contents reg))
      (advance-pc! pc))))

(defun make-restore (inst machine stack pc)
  (let ([reg (get-register machine (stack-inst-reg-name inst))])
    (lambda ()
      (set-contents! reg (pop! stack))
      (advance-pc! pc))))

(defun stack-inst-reg-name (stack-instruction)
  (cadr stack-instruction))

(defun make-perform (inst machine labels operations pc)
  (let ([action (perform-action inst)])
    (if (operation-exp? action)
      (let ([action-proc (make-operation-exp action machine labels operations)])
        (lambda ()
          (funcall action-proc)
          (advance-pc! pc)))
      (error 'bad-perform-instruction (inspect inst)))))

(defun perform-action (perform-instruction)
  (cdr perform-instruction))

(defun make-primitive-exp (exp machine labels)
  (cond ((constant-exp? exp)
         (let ([c (constant-exp-value exp)])
           (lambda () c)))
        ((label-exp? exp)
         (let ([insts (send labels 'lookup (label-exp-label exp))])
           (lambda () insts)))
        ((register-exp? exp)
         (let ([r (get-register machine (register-exp-reg exp))])
           (lambda () (get-contents r))))
        (:else (error 'unknown-expression-type (inspect exp)))))

(defun register-exp? (exp) (tagged-list? exp 'reg))
(defun register-exp-reg (exp) (cadr exp))
(defun constant-exp? (exp) (tagged-list? exp 'const))
(defun constant-exp-value (exp) (cadr exp))
(defun label-exp? (exp) (tagged-list? exp 'label))
(defun label-exp-label (exp) (cadr exp))

(defun make-operation-exp (exp machine labels operations)
  (let ([op (lookup-prim (operation-exp-op exp) operations)]
        [aprocs (cons-map #^(make-primitive-exp % machine labels)
                          (operation-exp-operands exp))])
    (lambda ()
      ; NOTE: The version of apply defined by the sicp in an earlier section
      ; does not work with implementation of a machine
      (lisp:apply op (make-lisp-list (cons-map funcall aprocs))))))

(defun operation-exp? (exp)
  (and (list? exp) (tagged-list? (car exp) 'op)))

(defun operation-exp-op (operation-exp)
  (cadr (car operation-exp)))

(defun operation-exp-operands (operation-exp)
  (cdr operation-exp))

(defun lookup-prim (symbol operations)
  (if (key? operations symbol)
    (get operations symbol)
    (error 'unknown-operation symbol)))

(defun gcd-machine ()
     (make-machine
       (qcons a b t)
       (list (list 'rem mod) (list '= =))
       (qcons test-b
              (test (op =) (reg b) (const 0))
              (branch (label gcd-done))
              (assign t (op rem) (reg a) (reg b))
              (assign a (reg b))
              (assign b (reg t))
              (goto (label test-b))
              gcd-done)))

(defun gcd (a b)
  (let ([gcd (gcd-machine)])
    (set-register-contents! gcd 'a a)
    (set-register-contents! gcd 'b b)
    (start! gcd)
    (get-register-contents gcd 'a)))

(assert= 10 (gcd 40 630))
(assert= 5 (gcd 735 635))

(defun factorial-machine ()
  (make-machine
    (qcons n val continue)
    (list (list '= =) (list '- -) (list '* *))
    (qcons (assign continue (label fact-done))
           fact-loop
           (test (op =) (reg n) (const 1))
           (branch (label base-case))
           (save continue)
           (save n)
           (assign n (op -) (reg n) (const 1))
           (assign continue (label after-fact))
           (goto (label fact-loop))
           after-fact
           (restore n)
           (restore continue)
           (assign val (op *) (reg n) (reg val))
           (goto (reg continue))
           base-case
           (assign val (const 1))
           (goto (reg continue))
           fact-done
           (perform (op print-stack-statistics))
           (perform (op print-execution-statistics)))))

(defun factorial (n)
  (let ([fact (factorial-machine)])
    (set-register-contents! fact 'n n)
    (start! fact)
    (get-register-contents fact 'val)))

(assert= 6 (factorial 3))
(assert= 120 (factorial 5))
(assert= 5040 (factorial 7))
(assert= 1 (factorial 1))

(let ([gcd (gcd-machine)])
  (set-register-contents! gcd 'a 15)
  (set-register-contents! gcd 'b 9)
  (trace-on! gcd)
  (install-breakpoint! gcd 'test-b 4)
  (debug-print (start! gcd))
  (debug-print (get-register-contents gcd 'a))
  (cancel-breakpoint! gcd 'test-b 4)
  (debug-print (proceed-machine! gcd))
  (debug-print (get-register-contents gcd 'a)))

(defun evaluator-machine ()
  (make-machine
    (qcons exp env val continue proc arg1 unev)
    (list)
    (qcons)))
