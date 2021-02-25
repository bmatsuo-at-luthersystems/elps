(in-package 'procedure)
(use-package 'lisp2)

(export 'procedure)
(deftype procedure (parameters body env)
  (list parameters body env))

(export 'compound-procedure?)
(defun compound-procedure? (v) (type? procedure v))

(export 'procedure-parameters)
(defun procedure-parameters (p)
  (car (user-data p)))

(export 'procedure-body)
(defun procedure-body (p)
  (cadr (user-data p)))

(export 'procedure-environment)
(defun procedure-environment (p)
  (caddr (user-data p)))

(export 'compiled-procedure)
(deftype compiled-procedure (entry env)
  (list entry env))

(export 'compiled-procedure?)
(defun compiled-procedure? (proc)
  (type? compiled-procedure proc))

(export 'compiled-procedure-entry)
(defun compiled-procedure-entry (proc)
  (car (user-data proc)))

(export 'compiled-procedure-env)
(defun compiled-procedure-env (proc)
  (cadr (user-data proc)))
