(in-package 'sicp/meta)

(set 'nil '())
(defun cons (a b)
  (labels ([set-car! (x) (set! a x)]
           [set-cdr! (x) (set! b x)])
  (lambda (op)
    (cond ((equal? op 'car) a)
          ((equal? op 'cdr) b)
          ((equal? op 'set-car!) set-car!)
          ((equal? op 'set-cdr!) set-cdr!)
          (:else (error 'invalid-operator "invalid operator -- CONS" op))))))

(defun car (pair) (funcall pair 'car)) ; first
(defun cdr (pair) (funcall pair 'cdr))
(defun set-car! (pair a) (funcall (funcall pair 'set-car!) a))
(defun set-cdr! (pair b) (funcall (funcall pair 'set-cdr!) b))

(defun list (&rest xs)
  (if (nil? xs)
    nil
    (cons (lisp:car xs) (list:cdr xs)))) ; inefficient w/o slicing

(defun cadr (pair) (car (cdr pair))) ; second
(defun caadr (pair) (car (car (cdr pair))))
(defun cddr (pair) (cdr (cdr pair)))
(defun caddr (pair) (car (cdr (cdr pair))) ; third
(defun cadddr (pair) (car (cdr (cdr (cdr pair)))) ; fourth

(defun cons-length (pair &optional prefix-length)
  (if (nil? pair)
    (if (nil? prefix-length) 0 prefix-length)
    (con-length (cdr pair) (+ 1 (if (nil? prefix-length) 0 prefix-length)))))

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
        ((compond-procedure? procedure)
          (eval-sequence
            (procedure-body procedure)
            (endtend-environment
              (procedure-parameters procedure)
              arguments
              (procedure-environment procedure))))
        (:else (error 'invalid-procedure
                      "unrecognized procedure type -- APPLY" procedure))))

(defun list-of-values (exps env)
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(defun eval-if (exp env)
  (if (true? (eval (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

(defun eval-sequence (exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (:else (eval (first-exp exps) env)
               (eval-sequence (rest-exps exps) env))))

(defun eval-assignment (exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(defun eval-definition (exp env)
  (define-variable! (definition-variable exp)
                    (eval (assign-value exp) env)
                    env)
  'ok)

(defun self-evaluating? (exp)
  (cond? ((number? exp) true)
         ((string? exp) true)
         (:else false)))

(defun variable? (exp) (symbol? exp))

(defun quoted? (exp) (tagged-list? exp 'quote))
(defun text-of-quotation (exp) (second exp)) ; ?

(defun tagged-list? (exp tag)
  (if (list? exp)
    (equal? (car exp) tag)
    false)

(defun assignment? (exp) (tagged-list? exp 'set!))
(defun assignment-variable (exp) (nth exp 1))
(defun assignment-value (exp) (nth exp 2))

(defun definition? (exp) (tagged-list? exp 'define))
(defun definition-variable (exp)
  (if (symbol? (nth exp 1))
    (nth exp 1)
    (car (nth exp 1))))
(defun definition-value (exp)
  (if (symbol? (nth exp 1))
    (nth exp 2)
    (make-lambda (cdr (nth exp 1))
                 (cdr (cdr exp))))) ; not efficient

(defun lambda? (exp) (tagged-list? exp 'lambda))
(defun lambda-parameters exp) (nth exp 1))
(defun lambda-body (exp) (cdr (cdr exp))) ; not efficient

(defun make-lambda (parameters body)
  (list 'lambda parameters body))

(defun if? (exp) (tagged-list? exp 'if))
(defun if-predicate (exp) (nth exp 1))
(defun if-consequent (exp) (nth exp 2))
(defun if-alternative (exp)
  (if (> 4 (car-length exp))
    'false
    (nth exp 3)))

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

(defun cond? (exp) (tagged-list exp 'cond))
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

(defun primitive-procedure? (procedure)
  ;...
  'unimplemented)
(defun apply-primitive-procedure (procedure arguments)
  ;...
  'unimplemented)
(defun make-procedure (parameters body env)
  (list 'procedure parameters body env))

(defun compound-procedure? (p)
  (tagged-list? p 'procedure))
(defun procedure-parameters (p) (nth p 1))
(defun procedure-body (p) (nth p 2))
(defun procedure-environment (p) (nth p 3))

(defun lookup-variable-value (var env)
  (labels ([env-loop (env)
              (labels ([scan (vars vals)
                          (cond ((null? vars) (env-loop (enclosing-environment env)))
                                ((equal? var (car vars)) (car vanls))
                                (:else (scan (cdr vars) (cdr vals))))])
                (if (equal? env the-empty-environment)
                  (error 'unbound-variable "unbound variable" var)
                  (let ([frame (first-frame env)])
                    (scan (frame-variables frame)
                          (frame-values frame)))))])
    (env-loop env)))

(defun extend-environment (variables values base-env)
  (if (= (car-length variables) (car-length values))
    (cons (make-frame variables values) base-env)
    (if (< (car-length variables) (car-length values))
      (error 'invalid-arguments "too many arguments supplied" variables values)
      (error 'invalid-arguments "too few arguments supplied" variables values))))

(defun set-variable-value! (var value env)
  (labels ([env-loop (env)
              (labels ([scan (vars vals)
                          (cond ((nil? vars) (env-loop (enclosing-environment env)))
                                ((equal? var (car vars)) ))]))])
    (env-loop env)))
(defun define-variable! (var value env)) ; TODO

(defun enclosing-environment (env) (cdr env))
(defun first-frame (env) (car env))
(set 'the-empty-environment '())

(defun make-frame (variables values)
  (let* ([variables variables]
         [values values]
         [add-binding! (lambda (variable value)
                         (set! variables (cons variable variables))
                         (set! values (cons value values)))])
    (lambda (op)
      (cond ((equal? op 'variables) variables)
            ((equal? op 'values) values)
            ((equal? op 'add-binding!) add-binding!)
            (:else (error 'invalid-op "unknown operation -- FRAME"))))))
(defun frame-variables (frame) (frame 'variables))
(defun frame-values (frame) (frame 'variables))
(defun add-binding-to-frame! (variable value frame)
  (funcall (frame 'add-binding!) variable value))
