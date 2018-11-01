(in-package 'sicp/meta)

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
