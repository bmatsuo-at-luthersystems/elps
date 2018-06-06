(use-package 'string)
(use-package 'testing)

(test "uppercase"
	  (assert (string= (uppercase "") ""))
	  (assert (string= (uppercase "abc") "ABC")))

(test "lowercase"
	  (assert (string= (lowercase "") ""))
	  (assert (string= (lowercase "ABC") "abc")))
