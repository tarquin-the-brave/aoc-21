(load "~/quicklisp/setup.lisp")
(ql:quickload '("uiop" "str"))

(defparameter *crabs*
  (mapcar #'parse-integer (str:split "," (first (uiop:read-file-lines "inputs/day7-1.txt")))))

(defun triangle (n)
  (if (= n 0) 0 (+ n (triangle (1- n)))))

(defun bin-search (f n nprev)
  (let ((prev (funcall f (1- n))) (val (funcall f n)) (next (funcall f (1+ n))))
    (cond
      ((< prev val) (bin-search f (floor n 2) n))
      ((< next val) (bin-search f (+ n (+ 1 (floor (- nprev n) 2))) n))
      (t val))))
;
; Part 1
;
(defun fuel1 (crabs pos)
  (loop for crab in crabs sum (abs (- pos crab))))

(print (bin-search (lambda (n) (fuel1 *crabs* n)) 500 1000))

;
; Part 2
;
(defun fuel2 (crabs pos)
  (loop for crab in crabs sum (triangle (abs (- pos crab)))))

(print (bin-search (lambda (n) (fuel2 *crabs* n)) 500 1000))

