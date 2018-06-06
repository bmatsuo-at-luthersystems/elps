(use-package 'string)

(assert (string= (uppercase "abc") "ABC"))
(assert (string= (lowercase "ABC") "abc"))
(assert (string= (titlecase "abc def") "Abc def"))
