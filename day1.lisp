#!/usr/bin/env -S sbcl --script
(load "~/quicklisp/setup.lisp")
(ql:quickload "uiop")

(defparameter *depths*
  (mapcar #'parse-integer (uiop:read-file-lines "inputs/day1-1.txt")))

; todo: Is there a recursive version of this function that is cleaner?
; recursive was my first thought but seeing how nice the "for on" syntax
; is this seems almost a minimal expression.
(defun count-increases (numbers)
  (loop for (a b) on numbers
        while b
        count (> b a)))

(print (count-increases *depths*))

(defun window-sum (numbers)
  (loop for (a b c) on numbers
        while c
        collect (+ a b c)))

(print (count-increases (window-sum *depths*)))
