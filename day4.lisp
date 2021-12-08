(load "~/quicklisp/setup.lisp")
(ql:quickload "uiop")
(ql:quickload "array-operations")
(ql:quickload "str")

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

;;;
;;; Some Functions
;;;
(defun sum-array (arr)
  (let ((x 0))
    (loop for a across arr do (when a (setf x (+ x a))))
    x))

(defun sum-2array (arr)
  (let*
    ((dims (array-dimensions arr))
     (imax (first dims))
     (jmax (second dims))
     (x 0))
    (loop for i below imax do
      (loop for j below jmax do
        (let ((e (aref arr i j)))
        (when e (incf x e)))))
    x))

(defun contains5 (arr)
  (loop for a across arr do (when (equal 5 a) (return t))))

; credit to the interwebs: https://stackoverflow.com/questions/9549568/common-lisp-convert-between-lists-and-arrays
(defun list-to-2d-array (list)
  (make-array
    (list (length list) (length (first list)))
    :initial-contents list))

;;
;; bingo card object
;;
(defclass card ()
  ((grid
    :initarg :grid
    :initform (error "initial value for slot `grid` required")
    :accessor grid)
   (row-counts
    :initform (make-array '(5))
    :accessor row-counts)
   (column-counts
    :initform (make-array '(5))
    :accessor column-counts)
   (in-play
     :initform t
     :accessor in-play)))

(defmethod score ((self card))
  (sum-2array (grid self)))

(defmethod play ((self card) guess)
  (when
    (loop named outer for i below 5 do
      (loop for j below 5 do
        (when (equal guess (aref (grid self) i j))
          (progn
            (incf (aref (row-counts self) i))
            (incf (aref (column-counts self) j))
            (setf (aref (grid self) i j) nil)
            (return-from outer guess)))))
    (when
      (or (contains5 (row-counts self)) (contains5 (column-counts self)))
      (progn (setf (in-play self) nil) t))))

;;
;; Player Object
;;
(defclass player ()
  ((cards
    :initarg :cards
    :initform (error "initial value for slot `cards` required")
    :accessor cards)))

(defmethod play ((self player) guesses)
  (loop named outer for guess in guesses do
    (loop for card in (cards self) do
      (when (play card guess) (return-from outer (* guess (score card)))))))

(defmethod play-to-lose ((self player) guesses)
  (let
    ((cards-remaining (list-length (cards self)))
     (latest-score 0))
    (loop named outer while (> cards-remaining 0) do
          (let
            ((guess (pop guesses)))
            (loop for card in (cards self) do
              (when (in-play card)
                (when (play card guess)
                  (progn (setf latest-score (* guess (score card))) (decf cards-remaining)))))))
    latest-score))

;;;
;;; Input parsing
;;;
(defun read-cards-from-lines (lines)
  (loop while lines collect
    (let*
      ((blank (pop lines))
       (r0 (pop lines))
       (r1 (pop lines))
       (r2 (pop lines))
       (r3 (pop lines))
       (r4 (pop lines)))
      (make-instance 'card
        :grid (list-to-2d-array
                (mapcar
                  (lambda (rowstr) (mapcar #'parse-integer (str:words rowstr)))
                  (list r0 r1 r2 r3 r4)))))))

;;;
;;; Solution
;;;

; Part 1

(defvar *input* (uiop:read-file-lines "inputs/day4-1.txt"))
(defvar *guesses* (mapcar #'parse-integer (str:split "," (pop *input*))))
(defvar *cards* (read-cards-from-lines *input*))
(defvar *player* (make-instance 'player :cards *cards*))

(print (play *player* *guesses*))

; Part 2
; reset all the variables as some got mutated
(setf *input* (uiop:read-file-lines "inputs/day4-1.txt"))
(setf *guesses* (mapcar #'parse-integer (str:split "," (pop *input*))))
(setf *cards* (read-cards-from-lines *input*))
(setf *player* (make-instance 'player :cards *cards*))

(print (play-to-lose *player* *guesses*))

