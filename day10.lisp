(load "~/quicklisp/setup.lisp")
(ql:quickload "uiop")

(defparameter *input*
  (mapcar
    (lambda (line) (coerce line 'list))
    (uiop:read-file-lines
      ; "inputs/day10-test.txt"
      "inputs/day10-1.txt"
      )))

(defun score-char (c)
  (cond
    ((eq #\) c) 3)
    ((eq #\] c) 57)
    ((eq #\} c) 1197)
    ((eq #\> c) 25137)))

(defun corrupt-score (line)
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
            (return-from corrupt-score (score-char c))))))))

;
; Part 1
;
(defun part1 (input)
  (let ((score 0))
    (loop for line in input do
      (let ((s (corrupt-score line)))
        (when s (incf score s))))
    score))

(print (part1 *input*))
