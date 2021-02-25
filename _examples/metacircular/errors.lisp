(in-package 'errors)

(export 'type-error)
(defmacro type-error (msg val)
  (quasiquote (lisp:error 'type-error
                          (lisp:concat 'string
                                       (to-string (unquote msg))
                                       " "
                                       (lisp:to-string (type (unquote val)))))))

(export 'unknown-method-error)
(defmacro unknown-method-error (typ method)
  (quasiquote (lisp:error 'unknown-method
                          (string:join (lisp:list "unrecognized method for type "
                                                  (lisp:to-string (unquote typ))
                                                  ": "
                                                  (lisp:to-string (unquote method)))
                                       ""))))
