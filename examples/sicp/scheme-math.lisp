(load-file "complex.lisp")
(use-package 'sicp/complex)

;  Don't blow the dispatch table away if we reload the file
(set 'dispatch-table (or (ignore-errors dispatch-table) (sorted-map)))

(defun dispatch-put (op-symbol operand-type operator)
  (let ([op-table (or (get dispatch-table op-symbol) (sorted-map))])
    (assoc! op-table operand-type operator)
    (assoc! dispatch-table op-symbol op-table)))

(defun dispatch-get (op-symbol operand-type)
  (let [(op-table (get dispatch-table op-symbol))]
    (and op-table
         (get op-table operand-type))))

(defun install-rectangular-package ()
  (labels ([real-part (z) (first z)]
           [imag-part (z) (second z)]
           [make-from-real-imag (x y) (list x y)]
           [magnitude (z) (sqrt (+ (square (real-part z))
                                           (square (imag-part z))))]
           [angle     (z)])))

(defun add (x y) (apply-generic 'add x y))
(defun sub (x y) (apply-generic 'sub x y))
(defun mul (x y) (apply-generic 'mul x y))
(defun div (x y) (apply-generic 'div x y))
