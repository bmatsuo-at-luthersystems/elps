(in-package 'compile)
(use-package 'errors)
(use-package 'lisp2)
(use-package 'procedure)
(use-package 'evaluator)

(export 'cond?)
(defun cond? (exp)
  (equal? 'cond (car exp)))

(export 'instruction-sequence)
(deftype instruction-sequence (needs modifies statements)
  (list needs modifies statements))

(export 'empty-instruction-sequence)
(defun empty-instruction-sequence ()
  (new instruction-sequence nil nil nil))

(export 'registers-needed)
(defun registers-needed (s)
  (if (symbol? s) nil (car (user-data s))))

(export 'registers-modified)
(defun registers-modified (s)
  (if (symbol? s) nil (cadr (user-data s))))

(export 'statements)
(defun statements (s)
  (if (symbol? s) (list s) (caddr (user-data s))))

(defun member? (x s)
  (any? (lambda (z) (equal? x z)) s))

(export 'needs-register?)
(defun needs-register? (seq reg)
  (member? reg (registers-needed seq)))

(export 'modifies-register?)
(defun modifies-register? (seq reg)
  (member? reg (registers-modified seq)))

(export 'preserving)
;; concatenates the sequences seq1 and seq2 while inserting stack instructions
;; to ensure that all of the given registers needed by seq2 have their value
;; from before seq1 executed at the time when seq2 begins execution.  for
;; example, if regs is all-regs then the execution of seq1 in the concatenated
;; sequence should not effect the execution of seq2
(defun preserving (regs seq1 seq2)
  (if (nil? regs)
    (append-instruction-sequences seq1 seq2)
    (let ([first-reg (car regs)])
      ;; wrap seq1 in save/restore instructions to preserve first-reg if
      ;; first-reg is needed by seq2 but will be modified by s1. afterwards,
      ;; continue presversing the remaining registers
      (if (and (needs-register? seq2 first-reg)
               (modifies-register? seq1 first-reg))
        (preserving (cdr regs)
                    (new instruction-sequence
                         (list-union (list first-reg)
                                     (registers-needed seq1))
                         (list-difference (registers-modified seq1)
                                          (list first-reg))
                         (concat (quasiquote-list ((save (unquote first-reg))))
                                 (concat (statements seq1)
                                         (quasiquote-list ((restore (unquote first-reg)))))))
                    seq2)
        (preserving (cdr regs) seq1 seq2)))))

(export 'tack-on-instruction-sequence)
;; concatenates the sequences seq and body-seq but discards register
;; constraints from body-seq because body-seq is not executed following seq but
;; is executed via goto.
(defun tack-on-instruction-sequence (seq body-seq)
  (new instruction-sequence
       (registers-needed seq)
       (registers-modified seq)
       (concat (statements seq)
               (statements body-seq))))

(export 'parallel-instruction-sequences)
;; concatenates the sequences seq1 and seq2 with the union of needed and
;; modified registers.  seq2 can use the values in any registers modified by
;; seq1
(defun parallel-instruction-sequences (seq1 seq2)
  (new instruction-sequence
       (list-union (registers-needed seq1)
                   (registers-needed seq2))
       (list-union (registers-modified seq1)
                   (registers-modified seq2))
       (concat (statements seq1) (statements seq2))))

(export 'append-instruction-sequences)
;; concatenates the given sequences.  the resulting sequence needs registers r
;; required by any seqs except those needed only by seqs following a sequence
;; that modifies r.  the resulting sequence modifies registers r that are
;; modified by any seqs.  sequences can use register values set by earlier
;; sequences.
(defun append-instruction-sequences (&rest seqs)
  ;; must convert from lisp:list to a cons list
  (append-seq-list (lisp:apply list seqs)))

(defun append-seq-list (seqs)
  (if (nil? seqs)
    (empty-instruction-sequence)
    (append-2-sequences (car seqs)
                        (append-seq-list (cdr seqs)))))

(defun append-2-sequences (seq1 seq2)
  (new instruction-sequence
       (list-union (registers-needed seq1)
                   (list-difference (registers-needed seq2)
                                    (registers-modified seq1)))
       (list-union (registers-modified seq1)
                   (registers-modified seq2))
       (concat (statements seq1) (statements seq2))))

(defun list-union (s1 s2)
  (cond ((nil? s1) s2)
        ((member? (car s1) s2) (list-union (cdr s1) s2))
        (:else (cons (car s1) (list-union (cdr s1) s2)))))

(defun list-difference (s1 s2)
  (cond ((nil? s1) nil)
        ((member? (car s1) s2) (list-difference (cdr s1) s2))
        (:else (cons (car s1) (list-difference (cdr s1) s2)))))



(export 'compile)
(defun compile (exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp)
         (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assignment exp target linkage))
        ((definition? exp)
         (compile-definition exp target linkage))
        ((if? exp)
         (compile-if exp target linkage))
        ((lambda? exp)
         (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage))
        ((cond? exp)
         (compile (cond->if exp) target linkage))
        ((application? exp)
         (compile-application exp target linkage))
        (:else (error 'unknown-expression-type
                      (format-string "unrecognized expression type: {}" (format-list exp))))))

(export 'compile-self-evaluating)
(defun compile-self-evaluating (exp target linkage)
  (end-with-linkage linkage
                    ;;(new instruction-sequence nil (qlist target) (list (concat (qlist assign)
                    ;;                                                           (list target)
                    ;;                                                           (list (concat (qlist const)
                    ;;                                                                         (list exp))))))))
                    (new instruction-sequence
                         nil
                         (list target)
                         (quasiquote-list ((assign (unquote target)
                                                   (const (unquote exp))))))))
(export 'compile-quoted)
(defun compile-quoted (exp target linkage)
  (end-with-linkage linkage
                    (new instruction-sequence
                         nil
                         (list target)
                         (quasiquote-list ((assign (unquote target)
                                                   (const (unquote (text-of-quotation exp)))))))))
(export 'compile-variable)
(defun compile-variable (exp target linkage)
  (end-with-linkage linkage
                    (new instruction-sequence
                         (qlist env)
                         (list target)
                         (quasiquote-list ((assign (unquote target)
                                                   (op lookup-variable-value)
                                                   (const (unquote exp))
                                                   (reg env)))))))
(export 'compile-assignment)
(defun compile-assignment (exp target linkage)
  (let ([var (assignment-variable exp)]
        [get-value-code (compile (assignment-value exp) 'val 'next)])
    (end-with-linkage linkage
                      (preserving (qlist env)
                                  get-value-code
                                  (new instruction-sequence
                                       (qlist env val)
                                       (list target)
                                       (quasiquote-list ((perform (op set-variable-value!)
                                                                  (const (unquote var))
                                                                  (reg val)
                                                                  (reg env))
                                                         (assign (unquote target) (const ok)))))))))

(export 'compile-definition)
(defun compile-definition (exp target linkage)
  (let ([var (definition-variable exp)]
        [get-value-code (compile (definition-value exp) 'val 'next)])
    (end-with-linkage linkage
                      (preserving (qlist env)
                                  get-value-code
                                  (new instruction-sequence
                                       (qlist env val)
                                       (list target)
                                       (quasiquote-list ((perform (op define-variable!)
                                                                  (const (unquote var))
                                                                  (reg val)
                                                                  (reg env))
                                                         (assign (unquote target) (const ok)))))))))

(set 'label-counter 0)

(defun new-label-number ()
  (set 'label-counter (+ 1 label-counter))
  label-counter)

(defun make-label (s)
  (if (symbol? s)
    (to-symbol (format-string "{}{}" (to-string s) (new-label-number)))
    (type-error "argument is not a string:" s)))

;;(debug-print (make-label 'test-label))
;;(debug-print (make-label 'test-label))
;;(debug-print (make-label 'test-label))

(export 'compile-if)
(defun compile-if (exp target linkage)
  (let ([after-if (make-label 'after-if)]
        [f-branch (make-label 'false-branch)]
        [t-branch (make-label 'true-branch)])
    (let ([consequent-linkage (if (equal? 'next linkage) after-if linkage)])
      (let ([a-code (compile (if-alternative exp) target linkage)]
            [c-code (compile (if-consequent exp) target consequent-linkage)]
            [p-code (compile (if-predicate exp) 'val 'next)])
        (preserving (qlist env continue)
                    p-code
                    (append-instruction-sequences
                      (new instruction-sequence
                           (qlist val)
                           nil
                           (quasiquote-list ((test (op false?) (reg val))
                                             (branch (label (unquote f-branch))))))
                      (parallel-instruction-sequences
                        (append-instruction-sequences t-branch c-code)
                        (append-instruction-sequences f-branch a-code))
                      after-if))))))

(export 'compile-sequence)
(defun compile-sequence (exps target linkage)
  (if (last-exp? exps)
    (compile (first-exp exps) target linkage)
    (preserving (qlist env continue)
                (compile (first-exp exps) target 'next)
                (compile-sequence (rest-exps exps) target 'next))))

(export 'compile-lambda)
(defun compile-lambda (exp target linkage)
  (let ([after-lambda (make-label 'after-lambda)]
        [proc-entry (make-label 'entry)])
    (let ([lambda-linkage (if (equal? linkage 'next) after-lambda linkage)])
      (append-instruction-sequences
        (tack-on-instruction-sequence
          (end-with-linkage lambda-linkage
                            (new instruction-sequence
                                 (qlist env)
                                 (list target)
                                 (quasiquote-list ((assign (unquote target)
                                                           (op make-compiled-procedure)
                                                           (label (unquote proc-entry))
                                                           (reg env))))))
          (compile-lambda-body exp proc-entry))
        after-lambda))))

(defun compile-lambda-body (exp proc-entry)
  (let ([formals (lambda-parameters exp)])
    (append-instruction-sequences
      (new instruction-sequence
           (qlist env proc argl)
           (qlist env)
           (quasiquote-list ((unquote proc-entry)
                             (assign env (op compiled-procedure-env) (reg proc))
                             (assign env
                                     (op extend-environment)
                                     (const (unquote formals))
                                     (reg argl)
                                     (reg env)))))
      (compile-sequence (lambda-body exp) 'val 'return))))

(export 'compile-application)
(defun compile-application (exp target linkage)
  (let ([operand-codes (map (lambda (operand) (compile operand 'val 'next))
                            (operands exp))]
        [proc-code (compile (operator exp) 'proc 'next)])
    (preserving (qlist env continue)
                proc-code
                (preserving (qlist proc continue)
                            (construct-arglist operand-codes)
                            (compile-procedure-call target linkage)))))

;; returns code to evaluate operands and construct the argument list in argl
(defun construct-arglist (operand-codes)
  (let ([operand-codes (reverse operand-codes)])
    (if (nil? operand-codes)
      (new instruction-sequence nil (qlist argl)
           (qlist (assign argl (const ()))))
      (let ([code-to-get-last-arg (append-instruction-sequences
                                    (car operand-codes)
                                    (new instruction-sequence (qlist val) (qlist argl)
                                         (qlist (assign argl (op list) (reg val)))))])
        (if (nil? (cdr operand-codes))
          code-to-get-last-arg
          (preserving (qlist env)
                      code-to-get-last-arg
                      (code-to-get-rest-args (cdr operand-codes))))))))

(defun code-to-get-rest-args (operand-codes)
  (let ([code-for-next-arg (preserving (qlist argl)
                                       (car operand-codes)
                                       (new instruction-sequence (qlist val argl) (qlist argl)
                                            (qlist (assign argl (op cons) (reg val) (reg arg)))))])
    (if (nil? (cdr operand-codes))
      code-for-next-arg
      (preserving (qlist env)
                  code-for-next-arg
                  (code-to-get-rest-args (cdr operand-codes))))))

(defun compile-procedure-call (target linkage)
  (let ([after-call (make-label 'after-call)]
        [compiled-branch (make-label 'compiled-branch)]
        [primitive-branch (make-label 'primitive-branch)])
    (let ([compiled-linkage (if (equal? 'next linkage) after-call linkage)])
      (append-instruction-sequences
        (new instruction-sequence (qlist proc) nil
             (quasiquote-list ((test (op primitive-procedure?) (reg proc))
                               (branch (label (unquote primitive-branch))))))
        (parallel-instruction-sequences
          (append-instruction-sequences
            compiled-branch
            (compile-proc-appl target compiled-linkage))
          (append-instruction-sequences
            primitive-branch
            (end-with-linkage linkage
                              (new instruction-sequence (qlist proc argl) (list target)
                                   (quasiquote-list ((assign (unquote target)
                                                             (op apply-primitive-procedure)
                                                             (reg proc)
                                                             (reg argl))))))))
        after-call))))

(set 'all-regs (qlist env proc val argl continue))

(defun compile-proc-appl (target linkage)
  (cond ((and (equal? target 'val) (not (equal? linkage 'return)))
         (new instruction-sequence (qlist proc) all-regs
              (quasiquote-list ((assign continue (label (unquote linkage)))
                                (assign val (op compiled-procedure-entry) (reg proc))
                                (goto (reg val))))))
        ((and (not (equal? 'val target))
              (not (equal? 'return linkage)))
         (let ([proc-return (make-label 'proc-return)])
           (new instruction-sequence (qlist proc) all-regs
                (quasiquote-list ((assign continue (label (unquote proc-return)))
                                  (assign val (op compiled-procedure-entry))
                                  (goto (reg val))
                                  (unquote proc-return)
                                  (assign (unquote target) (reg val))
                                  (goto (label (unquote linkage))))))))
        ((and (equal? 'val target) (equal? 'return linkage))
         (new instruction-sequence (qlist proc continue) all-regs
              (qlist (assign val (op compiled-procedure-entry) (reg proc))
                     (goto (reg val)))))
        ((and (not (equal? 'val target)) (equal? linkage 'return))
         (error 'compile-error (format-string "return linkage, target not val: {}" (format-list val))))
        (:else
         (error 'unreachable "this should not be reachable"))))

(export 'end-with-linkage)
(defun end-with-linkage (linkage sequence)
  (preserving (qlist continue)
              sequence
              (compile-linkage linkage)))

(export 'compile-linkage)
(defun compile-linkage (linkage)
  (cond ((equal? linkage 'return)
         (new instruction-sequence (qlist continue) nil (list (make-goto-reg (symbol continue)))))
        ((equal? linkage 'next)
         (empty-instruction-sequence))
        (:else
         (new instruction-sequence nil nil (list (make-goto-label linkage))))))

(defun make-goto-reg (reg)
  (concat
    (qlist goto)
    (list (concat
            (qlist reg)
            (list reg)))))

(defun make-goto-label (label)
  (concat
    (qlist goto)
    (list (concat
            (qlist label)
            (list label)))))

(debug-list (compile-linkage 'return))
(debug-list (compile-linkage 'next))
(debug-list (compile-linkage (symbol foo)))

(defun print-statements (seq)
  (map (lambda (st)
         (if (symbol? st)
           (debug-print st)
           (debug-print (format-string "  {}" (format-list st)))))
       (statements seq)))

(debug-print "ADD")
(print-statements
  (compile (qlist + 1 (- 3 2))
           'val 'next))

(debug-print "FACTORIAL-ALT")
(print-statements
  (compile (qlist define (factorial-alt n) (if (= n 1) 1 (* (factorial-alt (- n 1)) n)))
           'val 'next))

;;(debug-print "FACTORIAL")
;;(print-statements
;;  (compile (qlist define (factorial n)
;;                  (define (iter product counter)
;;                    (if (> counter n)
;;                      product
;;                      (iter (* counter product)
;;                            (+ counter 1))))
;;                  (iter 1 1))
;;           'val 'next))
