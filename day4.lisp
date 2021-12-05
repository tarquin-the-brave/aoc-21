#!/usr/bin/env -S sbcl --script
(load "~/quicklisp/setup.lisp")
(ql:quickload "uiop")
(ql:quickload "array-operations")

(defun sum-array (arr)
  (let ((x 0))
    (loop for a across arr do (if a (setf x (+ x a)) ()))
    x
  )
)

(defun sum-2array (A)
  (let ((x 0))
    (aops:nested-loop (i j) (array-dimentions A)
      (if a (setf x (+ x (aref A i j))) ())
    )
    x
  )
)

; Part 1

(defvar *input* (uiop:read-file-lines "inputs/day4-test.txt"))

(defparameter *arr* (make-array '(4) :initial-element 3))
(defparameter *2arr* (make-array '(4 4) :initial-element 3))
; (print *input*)
(print *arr*)
(print (sum-array *arr*))
(print (sum-2array *2arr*))

; Part 2

