(in-package 'primitive)
(use-package 'lisp2)

(export 'primitive)
(deftype primitive (fun) fun)

(export 'primitive?)
(defun primitive? (v) (type? primitive v))

(export 'primitive-implementation)
(defun primitive-implementation (v)
  (user-data v))
