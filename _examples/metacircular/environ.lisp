(in-package 'environ)
(use-package 'lisp2)
(use-package 'adt)
(use-package 'primitive)

(defun init-bindings! (table variables values)
  (if (or (nil? variables) (nil? values))
    (if (and (nil? variables) (nil? values))
      ()
      (error 'invalid-bindings (format-string "variable length mismatch: {} {}" (format-list variables) (format-list values))))
    (progn
      (table-insert! table (cons (car variables) (car values)))
      (init-bindings! table (cdr variables) (cdr values)))))

(deftype environ (variables values parent)
  (let ([bindings (new table)])
    (init-bindings! bindings variables values)
    (labels ([lookup-value (name)
                (let ([record (table-assoc bindings name)])
                  (cond (record (cdr record))
                        ((nil? parent) (error 'unknown-variable (format-string "symbol is unbound: {}" name)))
                        (:else (lookup-variable-value name parent))))]
             [set-value (name value)
                (let ([record (table-assoc bindings name)])
                  (cond (record (set-cdr! record value))
                        ((nil? parent) (error 'unknown-variable (format-string "symbol is unbound: {}" name)))
                        (:else (set-variable-value! parent))))]
             [define-variable (name value)
                (let ([record (table-assoc bindings name)])
                  (if record (set-cdr! record value)
                    (table-insert! bindings (cons name value))))]
             [dispatch (message) (cond ((equal? message 'define-variable) define-variable)
                                       ((equal? message 'lookup-variable-value) lookup-value)
                                       ((equal? message 'set-variable-value) set-value)
                                       (:else (unknown-method-error 'environ message)))])
      dispatch)))

(export 'extend-environment)
(defun extend-environment (vars vals env)
  (new environ vars vals env))

(set 'primitive-procedures
     (list (list 'cons cons)
           (list 'car car)
           (list 'cdr cdr)
           (list 'nil? nil?)
           (list '+ +)
           (list '- -)
           (list '* *)
           (list '/ /)
           (list '< <)
           (list '<= <=)
           (list '>= >=)
           (list 'equal? list-equal?)))

(set 'primitive-procedure-names (map car primitive-procedures))
(set 'primitive-procedure-objects (map (lambda (proc) (new primitive (cadr proc)))
                                       primitive-procedures))

;; bind name to value in env.  if name has already been defined it will be
;; rebound to value.
(export 'define-variable!)
(defun define-variable! (name value env)
  (call-method env 'define-variable name value))

;; find the value bound to name in env or in its enclosing environments.
(export 'lookup-variable-value)
(defun lookup-variable-value (name env)
  (call-method env 'lookup-variable-value name))

;; set the value of a previously defined, named variable to value.  if name is
;; not defined in env then the value will be set in the enclosing environment.
(export 'set-variable-value!)
(defun set-variable-value! (name value env)
  (call-method env 'set-variable-value name value))

(export 'setup-environment)
(defun setup-environment ()
  (let ([initial-env (new environ
                          primitive-procedure-names
                          primitive-procedure-objects
                          nil)])
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(export 'the-global-environment)
(set 'the-global-environment (setup-environment))
