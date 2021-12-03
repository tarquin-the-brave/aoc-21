#!/usr/bin/env -S sbcl --script
(load "~/quicklisp/setup.lisp")
(ql:quickload "uiop")

; Part 1

(defvar *binaries* (uiop:read-file-lines "inputs/day3-1.txt"))

(defun get-gamma (bins)
  (coerce (mapcar #'most-popular (transpose (mapcar (lambda (l) (coerce l 'list)) bins))) 'string)
)

(defun most-popular (chars)
  (let ((ones 0) (zeros 0))
    (loop for c in chars
      do (cond
        ((equal c #\1 ) (incf ones))
        ((equal c #\0 ) (incf zeros))
      )
    )
    (cond
      ((> ones zeros) #\1)
      (t #\0)
    )
  )
)

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

(defvar *gamma* (get-gamma *binaries*))
(defvar *epsilon* (flip-bits *gamma*))

(print (int-product *gamma* *epsilon*))
