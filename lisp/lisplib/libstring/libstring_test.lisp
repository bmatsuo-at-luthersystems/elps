(use-package 'string)
(use-package 'testing)

(test "uppercase"
      (assert (string= (uppercase "") ""))
      (assert (string= (uppercase "abc") "ABC")))

(test "lowercase"
      (assert (string= (lowercase "") ""))
      (assert (string= (lowercase "ABC") "abc")))

(test "split"
      (assert (equal? (split "abc:def" ":")
                      '("abc" "def"))
              "unexpected {}" (split "abc:def" ":"))
      (assert (equal? (split "abc" ":")
                      '("abc")))
      (assert (equal? (split "" ":")
                      '("")))
      (assert (equal? (split ":" ":")
                      '("" ""))))
