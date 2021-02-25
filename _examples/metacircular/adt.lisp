(in-package 'adt)
(use-package 'lisp2)

(export 'vector)
(deftype vector (n)
  (let ([data (lisp:vector)])
    (lisp:dotimes (i n)
      (lisp:append! data nil))
    data))

(export 'vector-ref)
(defun vector-ref (v n)
  (lisp:aref (user-data v) n))

(export 'vector-set!)
(defun vector-set! (v n x)
  (lisp:set-aref! (user-data v) x n))

(export 'table)
(deftype table (&rest pairs)
  (let ([records nil])
    (labels ([lookup (key)
                (let ([record (assoc key)])
                  (if record
                    (cdr record)
                    false))]
             [assoc-inner (records key)
                (cond ((nil? records) false)
                      ((equal? key (caar records)) (car records))
                      (:else (assoc-inner (cdr records) key)))]
             [assoc (key) (assoc-inner records key)]
             [insert (record)
                (if (not (pair? record))
                  (type-error "argument is not a pair:" record)
                  ())
                (let ([pair (assoc (car record))])
                  (if pair
                    (set-cdr! pair (cdr record))
                    (set! records (cons record records))))]
             [append (record)
                (set! records (cons:append records record))]
             [_for-each (fun)
                (for-each fun records)]
             [print ()
                (for-each (lambda (row) (debug-print (format-list row)))
                          records)]
             [keys () (map car records)]
             [dispatch (message)
                (cond ((equal? message 'lookup) lookup)
                      ((equal? message 'assoc) assoc)
                      ((equal? message 'insert) insert)
                      ((equal? message 'insert) insert)
                      ((equal? message 'for-each) _for-each)
                      ((equal? message 'keys) keys)
                      ((equal? message 'print) print)
                      (:else (error 'unrecognized-method (to-string message))))])
      (for-each insert (lisp:apply list pairs))
      dispatch)))

(export 'table-lookup)
(defun table-lookup (table key)
  (call-method table 'lookup key))

(export 'table-assoc)
(defun table-assoc (table key)
  (call-method table 'assoc key))

(export 'table-insert!)
(defun table-insert! (table pair)
  (call-method table 'insert pair))

(export 'table-for-each)
(defun table-for-each (fun table)
  (call-method table 'for-each fun))

(export 'table-print)
(defun table-print (table)
  (call-method table 'print))

(export 'table-keys)
(defun table-keys (table)
  (call-method table 'keys))

(export 'register)
(deftype register (name)
  (let ([contents 'UNASSIGNED]
        [trace-enabled false])
    (labels ([get () contents]
             [set (value)
                  (if trace-enabled
                    (debug-print 'REGISTER name '-- 'old '= (format-list contents) 'new '= (format-list value))
                    ())
                  (set! contents value)]
             [trace-on () (set! trace-enabled true)]
             [trace-off () (set! trace-enabled false)]
             [dispatch (message)
                (cond ((equal? message 'get) get)
                      ((equal? message 'set) set)
                      ((equal? message 'trace-on) trace-on)
                      ((equal? message 'trace-off) trace-off)
                      (:else (error 'unknown-method (format-string "register: unknown request: {}" message))))])
      dispatch)))

(export 'get-contents)
(defun get-contents (register)
  (call-method register 'get))

(export 'set-contents!)
(defun set-contents! (register value)
  (call-method register 'set value))

(export 'stack)
(deftype stack ()
  (let ([s nil]
        [num-pushes 0]
        [max-depth 0]
        [current-depth 0])
    (labels ([copy-contents () s] ;; immutable linked list doesn't need a copy
             [push (x)
                   (set! num-pushes (+ num-pushes 1))
                   (set! current-depth (+ current-depth 1))
                   (if (< max-depth current-depth)
                     (set! max-depth current-depth)
                     ())
                   (set! s (cons x s))]
             [pop () (if (nil? s)
                       (error 'pop-empty-stack "pop: empty stack")
                       (let ([top (car s)])
                         (set! s (cdr s))
                         (set! current-depth (- current-depth 1))
                         top))]

             [print-statistics ()
                               (debug-print 'num-pushes '= num-pushes)
                               (debug-print 'max-depth '= max-depth)]
             [initialize () (set! s nil) 'done]
             [dispatch (message)
                (cond ((equal? message 'copy-contents) copy-contents)
                      ((equal? message 'push) push)
                      ((equal? message 'pop) pop)
                      ((equal? message 'initialize) initialize)
                      ((equal? message 'print-statistics) print-statistics)
                      (:else (error 'unknown-method (format-string "stack: unknown request: {}" message))))])
      dispatch)))

(export 'pop)
(defun pop (stack)
  (call-method stack 'pop))

(export 'push)
(defun push (stack value)
  (call-method stack 'push value))
