(load-file "complex.lisp")
(use-package 'sicp/complex)
(use-package 'math)
(use-package 'testing)

;  Don't blow the dispatch table away if we reload the file
(set 'dispatch-table (or (ignore-errors dispatch-table) (sorted-map)))

(defun dispatch-put (op-symbol operand-type operator)
  (let ([op-table (or (get dispatch-table op-symbol) (sorted-map))])
    (assoc! op-table (first operand-type) operator)
    (assoc! dispatch-table op-symbol op-table)))

(defun dispatch-get (op-symbol operand-type)
  (let [(op-table (get dispatch-table op-symbol))]
    (and op-table
         (get op-table operand-type))))

(defun install-rectangular-package ()
  (labels ([real-part (z) (first z)]
           [imag-part (z) (second z)]
           [make-from-real-imag (x y) (list x y)]
           [magnitude (z)
              (sqrt (+ (square (real-part z))
                       (square (imag-part z))))]
           [angle (z)
              (let ([x (real-part z)]
                    [y (imag-part z)])
                (atan (imag-part z)
                      (real-part z)))]
           [make-from-mag-ang (r a)
              (if (= 0 r)
                (list 0 0)
                (list (* r (cos a)) (* r (sin a))))]
           [tag (x) (attach-tag 'rectangular x)])
    (dispatch-put 'real-part '(rectangular) real-part)
    (dispatch-put 'imag-part '(rectangular) imag-part)
    (dispatch-put 'magnitude '(rectangular) magnitude)
    (dispatch-put 'angle '(rectangular) angle)
    (dispatch-put 'make-from-real-imag '(rectangular)
         (lambda (x y) (tag (make-from-real-imag x y))))
    (dispatch-put 'make-from-mag-ang '(rectangular)
         (lambda (r a) (tag (make-from-mag-ang r a))))
    'done))

(defun install-polar-package ()
  (labels ([magnitude (z) (first z)]
           [angle (z) (second z)]
           [make-from-mag-ang (r a) (list r a)]
           [real-part (z)
              (* (magnitude z) (cos (angle z)))]
           [imag-part (z)
              (* (magnitude z) (sin (angle z)))]
           [make-from-real-imag (x y)
              (list (sqrt (+ (square (real-part z))
                             (square (imag-part z))))
                    (atan y x))]
           [tag (x) (attach-tag 'polar x)])
    (dispatch-put 'real-part '(polar) real-part)
    (dispatch-put 'imag-part '(polar) imag-part)
    (dispatch-put 'magnitude '(polar) magnitude)
    (dispatch-put 'angle '(polar) angle)
    (dispatch-put 'make-from-real-imag '(polar)
                  (lambda (x y) (tag (make-from-real-imag x y))))
    (dispatch-put 'make-from-mag-ang '(polar)
                  (lambda (r a) (tag (make-from-mag-ang r a))))
    'done))

(defun apply-generic (op &rest args)
  (let* ([type-tags (map 'list 'type-tag args)]
         [proc (dispatch-get op (first type-tags))])
    (if proc
      (apply proc (map 'list 'contents args))
      (error 'invalid-method "no operation for types" (list op type-tags)))))

(defun real-part (z) (apply-generic 'real-part z))
(defun imag-part (z) (apply-generic 'imag-part z))
(defun magnitude (z) (apply-generic 'magnitude z))
(defun angle (z) (apply-generic 'angle z))

(defun make-from-real-imag (x y)
  (let ([f (dispatch-get 'make-from-real-imag 'rectangular)])
    (f x y)))

(defun make-from-mag-ang (x y)
  (let ([f (dispatch-get 'make-from-mag-ang 'rectangular)])
    (f x y)))


(install-rectangular-package)
(install-polar-package)

(assert= 0 (angle (make-from-real-imag 0 0)))
(assert= 0 (angle (make-from-real-imag 1 0)))
(assert= (/ math:pi 2) (angle (make-from-real-imag 0 1)))
(assert= (/ math:pi 4) (angle (make-from-real-imag 1 1)))
(assert= 1 (magnitude (make-from-real-imag 1 0)))
(assert= 1 (magnitude (make-from-real-imag 0 1)))
(assert= (sqrt 2) (magnitude (make-from-real-imag 1 1)))

(assert= 0 (angle (make-from-mag-ang 1 0)))
(assert= 0 (angle (make-from-mag-ang 0 (/ math:pi 2))))  ; angle ignored
(assert= (/ math:pi 2) (angle (make-from-mag-ang 1 (/ math:pi 2))))
(assert= 1 (real-part (make-from-mag-ang 1 0)))
(assert= 0 (imag-part (make-from-mag-ang 0 1)))
(assert= -1 (imag-part (make-from-mag-ang 1 (/ (* 3 math:pi) 2))))
(assert= -1 (real-part (make-from-mag-ang 1 math:pi)))

(defun add (x y) (apply-generic 'add x y))
(defun sub (x y) (apply-generic 'sub x y))
(defun mul (x y) (apply-generic 'mul x y))
(defun div (x y) (apply-generic 'div x y))
