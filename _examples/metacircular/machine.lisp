(in-package 'machine)
(use-package 'lisp2)
(use-package 'adt)

(export 'make-machine)
(defun make-machine (ops controller-text)
  (let ([machine (new machine)])
    (call-method machine 'install-operations ops)
    (assemble-program (new assembler machine) controller-text)
    machine))

(export 'machine)
(deftype machine ()
  (let ([pc (new register 'pc)]
        [flag (new register 'flag)]
        [the-cars (new register 'the-cars)]
        [the-cdrs (new register 'the-cdrs)]
        [free (new register 'free)]
        [stack (new stack)]
        [the-instruction-sequence nil]
        [sorted-instructions nil]
        [entry-points nil]
        [label-definitions nil]
        [save-registers nil]
        [restore-registers nil]
        [register-sources (new table)]
        [instruction-count 0]
        [instruction-trace-enabled false])
    (set-contents! free 0)
    (set-contents! the-cars (new vector 100))
    (set-contents! the-cdrs (new vector 100))
    (let ([the-ops (new table
                        (cons 'vector-ref vector-ref)
                        (cons 'vector-set! vector-set!)
                        (cons 'initialize-stack (lambda () (call-method stack 'initialize)))
                        (cons 'print-stack-statistics (lambda () (call-method stack 'print-statistics))))]
          [register-table (new table
                               (cons 'pc pc)
                               (cons 'flag flag)
                               (cons 'the-cars the-cars)
                               (cons 'the-cdrs the-cdrs)
                               (cons 'free free))])
      (labels ([allocate-register (name)
                  (if (table-assoc register-table name)
                    (error 'duplicate-register (format-string "register defined multiple times: {}" name))
                    (table-insert! register-table (cons name (new register name))))]
               [lookup-register (name)
                  (let ([val (table-assoc register-table name)])
                    (if val
                      (cdr val)
                      (error 'unknown-register name)))]
               [execute (can-break?)
                  (let ([insts (get-contents pc)])
                    (if (nil? insts)
                      'done
                      (let* ([inst (car insts)])
                        (if instruction-trace-enabled
                          (if (symbol? (instruction-text inst))
                            ;; BUG:  If controller text has adjacent labels A
                            ;; followed by B then goto B will not print a trace
                            ;; line for label A.
                            (debug-print 'LABEL (format-list (instruction-text inst)))
                            (debug-print 'STEP (format-list (instruction-text inst))))
                          ())
                        (if (execute-inst inst can-break?)
                          (execute true)
                          'breakpoint))))]
               [execute-inst (inst can-break?)
                  (let ([proc (instruction-execution-proc inst)])
                    (if (nil? proc)
                      (advance-pc pc)
                      (if (or (nil? (instruction-breakpoints inst))
                              (not can-break?))
                        (progn
                          (funcall (instruction-execution-proc inst))
                          (set! instruction-count (+ instruction-count 1))
                          true)
                        (progn
                          (for-each (lambda (breakpoint)
                                      (debug-print 'BREAKPOINT (format-breakpoint breakpoint)))
                                    (instruction-breakpoints inst))
                          false))))]
               [start ()
                  (set-contents! pc the-instruction-sequence)
                  (execute true)]
               [proceed ()
                  (execute false)]
               [install-instruction-sequence (seq)
                  (set! the-instruction-sequence seq)
                  ;;(debug-print "INSTRUCTIONS")
                  ;;(for-each (lambda (inst) (debug-print (format-list (user-data inst))))
                  ;;          the-instruction-sequence)]
                  ()]
               [install-operations (ops)
                  (table-for-each (lambda (op) (table-insert! the-ops op))
                                  ops)
                  ;;(debug-print "OPERATIONS")
                  ;;(table-print the-ops)
                  ()]
               [trace-on () (set! instruction-trace-enabled true)]
               [trace-off () (set! instruction-trace-enabled false)]
               [find-breakpoint (breakpoint)
                  (let ([rem (thread-first the-instruction-sequence
                                           (find-label (breakpoint-label breakpoint))
                                           (find-offset (breakpoint-offset breakpoint)
                                                        (lambda (inst) (symbol? (instruction-text inst)))))])
                    (if (nil? rem)
                      (error 'invalid-breakpoint
                             (format-string "breakpoint is invalid: {}"
                                            (format-breakpoint breakpoint)))
                      (car rem)))]
               [set-breakpoint (label offset)
                  (let ([breakpoint (new breakpoint label offset)])
                    (set-instruction-breakpoint! (find-breakpoint breakpoint) breakpoint))]
               [cancel-breakpoint (label offset)
                  (let ([breakpoint (new breakpoint label offset)])
                    (cancel-instruction-breakpoint! (find-breakpoint breakpoint) breakpoint))]
               [cancel-all-breakpoints ()
                  (for-each (cancel-all-instruction-breakpoints! inst)
                            the-instruction-sequence)]
               [set-entry-points (lis) (set! entry-points lis)]
               [set-label-definitions (lis) (set! label-definitions lis)]
               [set-save-registers (lis) (set! save-registers lis)]
               [set-restore-registers (lis) (set! restore-registers lis)]
               [print-instruction-count ()
                  (debug-print 'instruction-count '= instruction-count)
                  (set! instruction-count 0)]
               [describe-machine ()
                  (debug-print "===")
                  (debug-print "MACHINE REGISTERS")
                  (debug-list (table-keys register-table))
                  (debug-print "MACHINE ENTRY POINTS")
                  (debug-list entry-points)
                  (debug-print "MACHINE SAVE REGISTERS")
                  (debug-list save-registers)
                  (debug-print "MACHINE RESTORE REGISTERS")
                  (debug-list restore-registers)
                  (debug-print "===")
                  ]
               [dispatch (message)
                  (cond ((equal? message 'start) start)
                        ((equal? message 'proceed) proceed)
                        ((equal? message 'install-instruction-sequence) install-instruction-sequence)
                        ((equal? message 'allocate-register)  allocate-register)
                        ((equal? message 'get-register) lookup-register)
                        ((equal? message 'install-operations) install-operations)
                        ((equal? message 'trace-on) trace-on)
                        ((equal? message 'trace-off) trace-off)
                        ((equal? message 'set-breakpoint) set-breakpoint)
                        ((equal? message 'cancel-breakpoint) cancel-breakpoint)
                        ((equal? message 'cancel-all-breakpoints) cancel-all-breakpoints)
                        ((equal? message 'set-entry-points) set-entry-points)
                        ((equal? message 'set-save-registers) set-save-registers)
                        ((equal? message 'set-restore-registers) set-restore-registers)
                        ((equal? message 'stack) (lambda () stack))
                        ((equal? message 'operations) (lambda () the-ops))
                        ((equal? message 'print-instruction-count) print-instruction-count)
                        ((equal? message 'describe-machine) describe-machine)
                        (:else (error 'unknown-method (format-string "machine: unknown request: {}" message))))])
        dispatch))))

(export 'start)
(defun start (machine)
  (call-method machine 'start))

(export 'proceed-machine)
(defun proceed-machine (machine)
  (call-method machine 'proceed))

(export 'set-breakpoint!)
(defun set-breakpoint! (machine label offset)
  (call-method machine 'set-breakpoint label offset))

(export 'cancel-breakpoint!)
(defun cancel-breakpoint! (machine label offset)
  (call-method machine 'cancel-breakpoint label offset))

(export 'cancel-all-breakpoints!)
(defun cancel-all-breakpoints! (machine)
  (call-method machine 'cancel-all-breakpoints))

(export 'describe-machine)
(defun describe-machine (m) (call-method m 'describe-machine))

(export 'get-register)
(defun get-register (machine register-name)
  (call-method machine 'get-register register-name))

(export 'get-register-contents)
(defun get-register-contents (machine register-name)
  (get-contents (get-register machine register-name)))

(export 'set-register-contents!)
(defun set-register-contents! (machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

(deftype assembler (machine)
  (let ([instruction-sequence nil]
        [known-registers (new table)]
        [pc (get-register machine 'pc)]
        [flag (get-register machine 'flag)]
        [stack (call-method machine 'stack)]
        [operations (call-method machine 'operations)]
        [jump-labels nil])
    (labels ([touch-register! (register-name)
                (if (table-assoc known-registers register-name)
                  ()
                  (progn (table-insert! known-registers (cons register-name nil))
                         (call-method machine 'allocate-register register-name)))]
             [update-insts (insts extracted-labels)
                (set! jump-labels extracted-labels)
                (set! instruction-sequence insts)
                (let ([registers (thread-last insts
                                              (map instruction-text)
                                              (reject symbol?)
                                              (select (lambda (text) (equal? 'save (car text))))
                                              (map cadr)
                                              (sort (to-string< identity))
                                              (unique equal?))])
                  (call-method machine 'set-save-registers registers))
                (let ([registers (thread-last insts
                                              (map instruction-text)
                                              (reject symbol?)
                                              (select (lambda (text) (equal? 'restore (car text))))
                                              (map cadr)
                                              (sort (to-string< identity))
                                              (unique equal?))])
                  (call-method machine 'set-restore-registers registers))
                (let ([registers (thread-last insts
                                              (map instruction-text)
                                              (reject symbol?)
                                              (select (lambda (text) (and (equal? 'goto (car text))
                                                                          (equal? 'reg (caadr text)))))
                                              (map (lambda (text) (cadadr text)))
                                              (sort (to-string< identity))
                                              (unique list-equal?))])
                  (call-method machine 'set-entry-points registers))
                (for-each (lambda (inst)
                            (let ([proc (assemble-instruction (instruction-text inst))])
                              (set-instruction-execution-proc! inst proc)))
                          insts)]

             [assemble-instruction (inst)
                (if (symbol? inst)
                  nil
                  (let ([code (car inst)])
                    (cond ((equal? code 'label)
                           (assemble-label inst))
                          ((equal? code 'assign)
                           (assemble-assign inst))
                          ((equal? code 'test)
                           (assemble-test inst))
                          ((equal? code 'branch)
                           (assemble-branch inst))
                          ((equal? code 'goto)
                           (assemble-goto inst))
                          ((equal? code 'save)
                           (assemble-save inst))
                          ((equal? code 'restore)
                           (assemble-restore inst))
                          ((equal? code 'perform)
                           (assemble-perform inst))
                          (:else (error 'unknown-instruction inst)))))]

             [assemble-label (inst) nil]

             [assemble-assign (inst)
                (touch-register! (assign-reg-name inst))
                (let ([target (get-register machine (assign-reg-name inst))]
                      [value-exp (assign-value-exp inst)])
                  (let ([value-proc (if (operation-exp? value-exp)
                                      (assemble-operation-exp value-exp)
                                      (assemble-primitive-exp (car value-exp)))])
                    (lambda ()
                      (set-contents! target (funcall value-proc))
                      (advance-pc pc))))]

             [assemble-test (inst)
                (let ([condition (test-condition inst)])
                  (if (operation-exp? condition)
                    (let ([condition-proc (assemble-operation-exp condition)])
                      (lambda ()
                        (set-contents! flag (funcall condition-proc))
                        (advance-pc pc)))
                    (error 'invalid-test-instruction (format-string "test condition is not an operation: {}" (format-list inst)))))]

             [assemble-branch (inst)
                (let ([dest (branch-dest inst)])
                  (if (label-exp? dest)
                    (let ([insts (lookup-label jump-labels (label-exp-label dest))])
                      (lambda ()
                        (if (and (get-contents flag) (not (nil? (get-contents flag))))
                          (set-contents! pc insts)
                          (advance-pc pc))))
                    (error 'invalid-branch-instruction (format-string "branch destination is not a label {}" (format-list inst)))))]

             [assemble-goto (inst)
                (let ([dest (goto-dest inst)])
                  (cond ((label-exp? dest)
                         (let ([insts (lookup-label jump-labels (label-exp-label dest))])
                           (lambda () (set-contents! pc insts))))
                        ((register-exp? dest)
                         (let ([register-name (register-exp-reg dest)])
                           (touch-register! register-name)
                           (let ([reg (get-register machine register-name)])
                             (lambda () (set-contents! pc (get-contents reg))))))
                        (:else (error 'invalid-goto-instruction "goto destination is not a label or register"))))]

             [assemble-save (inst)
                (let ([register-name (stack-inst-reg-name inst)])
                  (touch-register! register-name)
                  (let ([reg (get-register machine register-name)])
                    (lambda ()
                      (push stack (get-contents reg))
                      (advance-pc pc))))]

             [assemble-restore (inst)
                (let ([register-name (stack-inst-reg-name inst)])
                  (touch-register! register-name)
                  (let ([reg (get-register machine register-name)])
                    (lambda ()
                      (set-contents! reg (pop stack))
                      (advance-pc pc))))]

             [assemble-perform (inst)
                (let ([action (perform-action inst)])
                  (if (operation-exp? action)
                    (let ([action-proc (assemble-operation-exp action)])
                      (lambda ()
                        (action-proc)
                        (advance-pc pc)))
                    (error 'invalid-perform-instruction "perform action is not an operation"))) ]

             [assemble-operation-exp (exp)
                (let ([op (lookup-prim (operation-exp-op exp) operations)]
                      [aprocs (map (lambda (e) (assemble-primitive-operand-exp e))
                                   (operation-exp-operands exp))])
                  (lambda ()
                    (apply op (map funcall aprocs))))]

             [assemble-primitive-exp (exp)
                (cond ((constant-exp? exp)
                       (let ([c (constant-exp-value exp)])
                         (lambda () c)))
                      ((register-exp? exp)
                       (let ([register-name (register-exp-reg exp)])
                         (touch-register! register-name)
                         (let ([r (get-register machine register-name)])
                           (lambda () (get-contents r)))))
                      ((label-exp? exp)
                       (let ([insts (lookup-label jump-labels (label-exp-label exp))])
                         (lambda () insts)))
                      (:else (error 'unknown-expression-type
                                    (format-string "cannot determine expression type for {}" (format-list exp)))))]

             [assemble-primitive-operand-exp (exp)
                (cond ((constant-exp? exp)
                       (let ([c (constant-exp-value exp)])
                         (lambda () c)))
                      ((register-exp? exp)
                       (let ([register-name (register-exp-reg exp)])
                         (touch-register! register-name)
                         (let ([r (get-register machine register-name)])
                           (lambda () (get-contents r)))))
                      ((label-exp? exp)
                       (error 'invalid-expression-type "label expressions cannot be used as operands"))
                      (:else (error 'unknown-expression-type
                                    (format-string "cannot determine expression type for {}" (format-list exp)))))]

             [install-instruction-sequence ()
                (call-method machine 'install-instruction-sequence instruction-sequence)]

             [dispatch (message)
                (cond ((equal? message 'update-insts) update-insts)
                      ((equal? message 'install-instruction-sequence) install-instruction-sequence)
                      (:else (error 'unknown-method (format-string "assembler: unknown request {}" message))))])
      dispatch)))

(export 'assemble-program)
(defun assemble-program (assembler controller-text)
  (extract-labels controller-text (funcall (user-data assembler) 'update-insts))
  (call-method assembler 'install-instruction-sequence))

(defun extract-labels (text receive)
  (if (nil? text)
    (funcall receive nil (new table))
    (let ([seen (new table)])
      (extract-labels (cdr text)
                      (lambda (insts labels)
                        (let ([next-inst (car text)])
                          (if (symbol? next-inst)
                            (let ([new-inst (new instruction next-inst)])
                              (if (table-assoc seen next-inst)
                                (error 'repeated-label (format-string "multiply defined label {}" next-inst))
                                (table-insert! seen (cons next-inst nil)))
                              (table-insert! labels (make-label-entry next-inst (cons new-inst insts)))
                              (receive (cons new-inst insts) labels))
                            (receive (cons (new instruction next-inst)
                                           insts)
                                     labels))))))))

(defun to-string< (fun)
  (lambda (a b)
    (string< (to-string (funcall fun a))
             (to-string (funcall fun b)))))

(deftype breakpoint (label offset) (cons label offset))

(defun breakpoint-label (breakpoint)
  (car (user-data breakpoint)))

(defun breakpoint-offset (breakpoint)
  (cdr (user-data breakpoint)))

(defun breakpoint-equal? (b1 b2)
  (list-equal? b1 b2))

(defun format-breakpoint (breakpoint)
  (format-string "{} {}" (car (user-data breakpoint)) (cdr (user-data breakpoint))))

(export 'instruction)
(deftype instruction (text) (cons text (cons nil nil)))

(export 'instruction-text)
(defun instruction-text (inst) (car (user-data inst)))

(export 'instruction-breakpoints)
(defun instruction-breakpoints (inst) (cadr (user-data inst)))

(export 'set-instruction-breakpoint!)
(defun set-instruction-breakpoint! (inst breakpoint)
  (update-instruction-breakpoints! inst
    (lambda (breakpoints)
      (if (any? (lambda (b) (breakpoint-equal? breakpoint b))
                breakpoints)
        (error 'duplicate-breakpoint
               (format-string "breakpoint already set on instruction: {}" breakpoint))
        (cons breakpoint breakpoints)))))


(export 'cancel-instruction-breakpoint!)
(defun cancel-instruction-breakpoint! (inst breakpoint)
  (update-instruction-breakpoints! inst
    (lambda (breakpoints) (reject (lambda (b) (breakpoint-equal? breakpoint b)) breakpoints))))

(export 'cancel-all-instruction-breakpoints!)
(defun cancel-all-instruction-breakpoints! (inst breakpoint)
  (update-instruction-breakpoints! inst (lambda (breakpoints) nil)))

(defun update-instruction-breakpoints! (inst fun)
  (set-car! (cdr (user-data inst))
            (funcall fun (instruction-breakpoints inst))))

(export 'instruction-execution-proc)
(defun instruction-execution-proc (inst) (cddr (user-data inst)))

(export 'set-instruction-execution-proc!)
(defun set-instruction-execution-proc! (inst proc) (set-cdr! (cdr (user-data inst)) proc))

(defun find-offset (insts offset skip?)
  (if (> 0 offset)
    (error 'negative-offset "list offset is netgative")
    (if (nil? insts)
      nil
      (if (skip? (car insts))
        (find-offset (cdr insts) offset skip?)
        (if (>= 1 offset)
          insts
          (find-offset (cdr insts) (- offset 1) skip?))))))

(defun find-label (insts label)
  (cond ((pair? insts)
         (if (and (symbol? (instruction-text (car insts)))
                  (equal? label (instruction-text (car insts))))
           insts
           (find-label (cdr insts) label)))
        ((nil? insts) nil)
        (:else (type-error "argument is not a list:" insts))))

(defun make-label-entry (label-name insts)
  (cons label-name insts))

(defun lookup-label (labels label-name)
  (let ([val (table-assoc labels label-name)])
    (if val
      (cdr val)
      (error 'undefined-label label-name))))


(defun advance-pc (pc)
  (set-contents! pc (cdr (get-contents pc)))
  nil)

(defun assign-reg-name (assign-instruction)
  (cadr assign-instruction))

(defun assign-value-exp (assign-instruction)
  (cddr assign-instruction))

(defun test-condition (test-instruction)
  (cdr test-instruction))

(defun branch-dest (branch-instruction)
  (cadr branch-instruction))

(defun goto-dest (goto-instruction)
  (cadr goto-instruction))

(defun stack-inst-reg-name (stack-instruction)
  (cadr stack-instruction))

(defun perform-action (inst)
  (cdr inst))

(defun register-exp? (exp)
  (equal? 'reg (car exp)))
(defun register-exp-reg (exp)
  (cadr exp))

(defun constant-exp? (exp)
  (equal? 'const (car exp)))
(defun constant-exp-value (exp)
  (cadr exp))

(defun label-exp? (exp)
  (equal? 'label (car exp)))
(defun label-exp-label (exp)
  (cadr exp))

(defun operation-exp? (exp)
  (and (pair? exp)
       (equal? 'op (caar exp))))

(defun operation-exp-op (operation-exp)
  (cadar operation-exp))

(defun operation-exp-operands (operation-exp)
  (cdr operation-exp))

(defun lookup-prim (symbol operations)
  (let ([val (table-assoc operations symbol)])
    (if val
      (cdr val)
      (error 'unknown-operation (format-string "operation not found: {}" symbol)))))

(defun gcd-machine ()
  (make-machine
    (new table
         (cons 'rem mod)
         (cons '= =))
    (qlist
      test-b
       (test (op =) (reg b) (const 0))
       (branch (label gcd-done))
       (assign t (op rem) (reg a) (reg b))
       (assign a (reg b))
       (assign b (reg t))
       (goto (label test-b))
      gcd-done)))

(defun gdc (a b)
  (let ([m (gcd-machine)])
    (call-method (get-register m 'b) 'trace-on)
    (set-register-contents! m 'a 24)
    (set-register-contents! m 'b 32)
    (call-method m 'trace-on)
    (start m)
    (let ([result (get-register-contents m 'a)])
      (call-method m 'print-instruction-count)
      result)))

(describe-machine (gcd-machine))
(assert (= 8 (gdc 32 32)))

(defun fib-machine ()
  (make-machine
    (new table
         (cons '< <)
         (cons '- -)
         (cons '+ +))
    (qlist
        (assign continue (label fib-done))
      fib-loop
        (test (op <) (reg n) (const 2))
        (branch (label immediate-answer))
        ;; set up to compute f(n-1)
        (save continue)
        (assign continue (label afterfib-n-1))
        (save n)
        (assign n (op -) (reg n) (const 1))
        (goto (label fib-loop))
      afterfib-n-1
        (restore n)
        (restore continue)
        ;; set up to compute f(n-2)
        (assign n (op -) (reg n) (const 2))
        (save continue)
        (assign continue (label afterfib-n-2))
        (save val)
        (goto (label fib-loop))
      afterfib-n-2
        (assign n (reg val))
        (restore val)
        (restore continue)
        (assign val (op +) (reg val) (reg n))
        (goto (reg continue))
      immediate-answer
        (assign val (reg n))
        (goto (reg continue))
      fib-done
        (perform (op print-stack-statistics)))))

(defun fib (n)
  (let ([m (fib-machine)])
    (set-register-contents! m 'n n)
    (start m)
    (let ([result (get-register-contents m 'val)])
      (call-method m 'print-instruction-count)
      result)))

(describe-machine (fib-machine))
(assert (= 0 (fib 0)))
(assert (= 1 (fib 1)))
(assert (= 1 (fib 2)))
(assert (= 2 (fib 3)))
(assert (= 3 (fib 4)))
(assert (= 5 (fib 5)))
(assert (= 8 (fib 6)))

(let ([m (gcd-machine)])
  (set-breakpoint! m 'test-b 4)
  (call-method m 'trace-on)
  (call-method (get-register m 'b) 'trace-on)
  (set-register-contents! m 'a 24)
  (set-register-contents! m 'b 32)
  (debug-print (start m))
  (debug-print 'a (get-register-contents m 'a))
  (debug-print 'b (get-register-contents m 'b))
  (debug-print (proceed-machine m))
  (debug-print 'a (get-register-contents m 'a))
  (debug-print 'b (get-register-contents m 'b))
  (debug-print (proceed-machine m))
  (debug-print 'a (get-register-contents m 'a))
  (debug-print 'b (get-register-contents m 'b))
  (debug-print (proceed-machine m))
  (debug-print 'a (get-register-contents m 'a))
  (debug-print 'b (get-register-contents m 'b))
  (assert (= 8 (get-register-contents m 'a))))
