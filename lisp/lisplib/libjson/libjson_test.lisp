(use-package 'testing)

(test "mashal"
    (assert (string= """null""" (json:dump-string ())))
    (assert (string= """true""" (json:dump-string t)))
    (assert (string= """12""" (json:dump-string 12)))
    (assert (string= """[1,2]""" (json:dump-string '(1 2))))
    (assert (string= """[1,2]""" (json:dump-string [1 2])))
    (assert (string= """{}""" (json:dump-string (sorted-map))))
    (assert (string= """{"a":1}""" (json:dump-string (sorted-map "a" 1))))
    (assert (string= """{"a":1,"b":2}""" (json:dump-string (sorted-map "a" 1 "b" 2))))
    (assert (string= """{"a":{"b":"c"}}""" (json:dump-string (sorted-map "a" (sorted-map "b" "c")))))
    )
