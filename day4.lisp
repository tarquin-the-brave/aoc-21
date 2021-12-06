;#!/usr/bin/env -S sbcl --script
(load "~/quicklisp/setup.lisp")
(ql:quickload "uiop")
(ql:quickload "array-operations")

;;;; Plan for day 4 solution:
;;;; Object oriented approach.
;;;; An object for a bingo card:
;;;;   data:
;;;;     a 5x5 grid of nullable ints
;;;;     a count of found values for each row and column.  If any of these reach 5 the card has won.
;;;;   methods:
;;;;     init
;;;;     play number: will nullify the number where it is found in the card, increment the
;;;;                  appropriate row and column counts, if any counts get to five, return a value,
;;;;                  caller can check for a value as a sign of the game being won.
;;;;     score: add up all the non nil values on the card.
;;;; An object to hold lots of cards and play the game.
;;;;  data:
;;;;    a list of bingo cards
;;;;  methods:
;;;;    init
;;;;    play bingo: takes a list of integers.  Returns score of winning card, or nil of not found.


(defun sum-array (arr)
  (let ((x 0))
    (loop for a across arr do (when a (setf x (+ x a))))
    x
  )
)

(defun sum-2array (arr)
  (let (
        (imax (1- (first (array-dimensions arr))))
        (jmax (1- (second (array-dimensions arr))))
        (x 0)
        )
    (loop for i from 0 to imax do (
      (loop for j from 0 to jmax do (
        setf x (+ x (aref arr i j))
      ))
    ))
    x
  )
)

; bingo card object
(defclass card ()
  (
   (grid
    :initarg :grid
    :initform (error "initial value for slot `grid` required")
    :accessor grid)
   (row-counts
    :initform (make-array '(5))
    :accessor row-counts)
   (column-counts
    :initform (make-array '(5))
    :accessor column-counts)
  )
)

(defmethod score ((self card))
  (sum-2array (grid self))
)

(defvar *input* (uiop:read-file-lines "inputs/day4-test.txt"))

(defparameter *arr* (make-array '(4) :initial-element 3))
(defparameter *2arr* (make-array '(4 4) :initial-element 3))
; (print *input*)
; (print *arr*)
(print (sum-array *arr*))
(print (sum-2array *2arr*))
; (defvar *foo* (make-instance 'card :grid *2arr*))
; (print (score *foo*))

; Part 1

; Part 2


; Notes on OO in lisp:
;
; I don't know how to make an initiation method like new(). where I can give arguments
; that initial slot values are derived from.  Perhaps just a wrapper around `make-instance`
; but that wouldn't get associated with the object in the same way...
