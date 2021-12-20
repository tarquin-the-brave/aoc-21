(load "~/quicklisp/setup.lisp")
(ql:quickload '("str" "uiop"))

(defparameter *input* (uiop:read-file-lines
                        ; "inputs/day14-example.txt"
                        "inputs/day14.txt"
                        ))

(defparameter *template* (pop *input*))
(pop *input*)

(defparameter *rules*
  (let ((rules (make-hash-table :test 'equal)))
    (loop for rule in (mapcar
                        (lambda (line) (str:split " -> " line))
                        *input*) do
          (setf (gethash (first rule) rules) (second rule)))
    rules))

(defun run-step (template rules)
  (let*
    ((t-chars (coerce template 'list))
     (output (format nil "~a" (first t-chars))))
    (loop for (a b) on t-chars
          while b
          do (setf output
                   (format nil "~a~a~a" output (gethash (coerce (list a b) 'string) rules) b)))
    output))

(defun run-steps (template rules n)
  (let ((output template))
    (loop repeat n do (setf output (run-step output rules)))
    output))

(defun min-non-zero (xs)
  (let ((m nil))
    (loop for x in (coerce xs 'list) do
      (when (> x 0)
        (if m
          (when (< x m) (setf m x))
          (setf m x))))
    m))

(defun part-x (template rules n)
  (let
    ((polymer (coerce (run-steps template rules n) 'list))
     (char-counts (make-array '(100) :initial-element 0)))
    (loop for p in polymer do (incf (aref char-counts (char-code p))))
    (- (reduce #'max char-counts) (min-non-zero char-counts))))

;
; Part 1
;
(print (part-x *template* *rules* 10))

;
; Part 2
;
; Part 1 grows at O(2^n)
;
; (print (part-x *template* *rules* 40))
