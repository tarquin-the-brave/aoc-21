(load "~/quicklisp/setup.lisp")
(ql:quickload "uiop")

(defparameter *input*
  (mapcar
    (lambda (line) (coerce line 'list))
    (uiop:read-file-lines
      ; "inputs/day10-test.txt"
      "inputs/day10-1.txt"
      )))

(defun score-corrupt-char (c)
  (cond
    ((eq #\) c) 3)
    ((eq #\] c) 57)
    ((eq #\} c) 1197)
    ((eq #\> c) 25137)))

(defun score-completion-char (c)
  (cond
    ((eq #\) c) 1)
    ((eq #\] c) 2)
    ((eq #\} c) 3)
    ((eq #\> c) 4)))

(defun walk (line)
  "walk the line.  If corrupted return offending character.
   If incomplete return the completion characters (the stack)."
  (let
    ((stack (list)))
    (loop for c in line do
      (cond
        ((eq #\( c) (push #\) stack))
        ((eq #\[ c) (push #\] stack))
        ((eq #\{ c) (push #\} stack))
        ((eq #\< c) (push #\> stack))
        (t
          (when
            (not (eq (pop stack) c))
            (return-from walk c)))))
    stack))

(defun completion-score (chars)
  (let ((score 0))
    (loop for c in chars do
      (setf score (* 5 score))
      (incf score (score-completion-char c)))
    score))

(defun middle-score (scores)
  (let
    ((n (floor (list-length scores) 2))
     (sorted (sort scores '<)))
    (nth n sorted)))

(defun day10 (input)
  (let ((corrupt-score 0) (comp-scores (list)))
    (loop for line in input do
      (let ((w (walk line)))
        (if
          (typep w 'list)
          (push (completion-score w) comp-scores)
          (incf corrupt-score (score-corrupt-char w)))))

    (list corrupt-score (middle-score comp-scores))))

(print (day10 *input*))
