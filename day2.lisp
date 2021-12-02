#!/usr/bin/env -S sbcl --script
(load "~/quicklisp/setup.lisp")
(ql:quickload "uiop")

; todo
; FYI depth => down is +ve
(defun parse-2vector () ())

(defparameter *movements*
  (mapcar #'parse-2vector (uiop:read-file-lines "inputs/day2-1.txt")))

(defun sum-2vector () ())

(defun xy-product () ())

(print (xy-product (sum-2vector *movements*)))
