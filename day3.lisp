#!/usr/bin/env -S sbcl --script
(load "~/quicklisp/setup.lisp")
(ql:quickload "uiop")

; Part 1

(defparameter *binaries* (uiop:read-file-lines "inputs/day2-1.txt"))

; (defun get-gamma (bins)
;   ()
; )

(defun transpose (list-of-lists) (apply #'mapcar #'list list-of-lists))

(defun flip-bits (bin)
  (map 'string (lambda (c)
    (cond
      ((equal c #\1 ) #\0)
      ((equal c #\0 ) #\1)
    )
  )
  (string bin))
)

(defun int-product (a b) (* (parse-integer a :radix 2) (parse-integer b :radix 2)))

(print (int-product "1000" (flip-bits "1100")))
(print (flip-bits "11110000"))
(print  (mapcar #'list (list "123" "abc" "xyz")))
