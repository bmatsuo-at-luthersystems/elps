(in-package 'runtime-error)
(use-package 'lisp2)
(use-package 'adt)

(deftype runtime-error (condition value stack) (list condition value stack))

(export 'make-runtime-error)
(defun make-runtime-error (condition value stack)
  (new runtime-error condition value stack))

(export 'runtime-error?)
(defun runtime-error? (err)
  (type? runtime-error err))

(defun get-runtime-error (err)
  (if (type? runtime-error err)
    (user-data err)
    (type-error "argument is not a runtime-error:" err)))

(export 'runtime-error-condition)
(defun runtime-error-condition (err)
  (car (get-runtime-error err)))

(export 'runtime-error-value)
(defun runtime-error-value (err)
  (cadr (get-runtime-error err)))

(export 'runtime-error-stack)
(defun runtime-error-stack (err)
  (caddr (get-runtime-error err)))

(export 'set-runtime-error-stack!)
(defun set-runtime-error-stack! (err stack)
  (set-car! (cddr (get-runtime-error)) stack))
