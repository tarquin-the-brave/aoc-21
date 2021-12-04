#!/usr/bin/env -S sbcl --script
(load "~/quicklisp/setup.lisp")
(ql:quickload "uiop")

; Part 1

(defvar *binaries* (uiop:read-file-lines "inputs/day3-1.txt"))

(defun char-grid (strings) "transpose a list of strings to a grid of chars grouped by their index"
  (transpose (mapcar (lambda (l) (coerce l 'list)) strings))
)

(defun get-gamma (bins)
  (coerce (mapcar #'popular (char-grid bins)) 'string)
)

(defun popular (chars &optional unpopular)
  (let ((ones (count #\1 chars)) (zeros (count #\0 chars)))
    (if unpopular
      (cond
        ((> zeros ones) #\1)
        (t #\0)
      )
      (cond
        ((> zeros ones) #\0)
        (t #\1)
      )
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

; Part 2

(defun popular-at-pos (strings pos &optional unpopular)
  (popular (elt (char-grid strings) pos) unpopular)
)

(defun rating (bins &optional co2)
  (let ((idx 0) (numbers (copy-list bins)))
    (loop while (< 1 (list-length numbers)) do
      (let ((popular-char (popular-at-pos numbers idx co2)))
        (setf numbers (remove-if #'(lambda (num) (not (equal popular-char (elt num idx)))) numbers))
      )
      (incf idx)
    )
    (first numbers)
  )
)

(print (int-product (rating *binaries*) (rating *binaries* t)))

