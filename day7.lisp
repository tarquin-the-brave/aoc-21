(load "~/quicklisp/setup.lisp")
(ql:quickload '("uiop" "str"))

(defparameter *crabs*
  (mapcar #'parse-integer (str:split "," (first (uiop:read-file-lines "inputs/day7-1.txt")))))

(defun fuel (crabs pos)
  (let ((f 0))
  (loop for crab in crabs do (incf f (abs (- pos crab))))
  f))

(defun fuel2 (crabs pos)
  (let ((f 0))
  (loop for crab in crabs do (incf f (triangle (abs (- pos crab)))))
  f))

(defun triangle (n)
  (if (= n 0) 0 (+ n (triangle (1- n)))))

; Part 1
; (loop for x below 1000 do (print (fuel *crabs* x)))
; Part 2
; (loop for x below 1000 do (print (fuel2 *crabs* x)))
; TODO: Do this properly.
; - binary search
;   + start in middle
;   + check values ± 1
;   + if both sides are higher: we've found minimum
;   + else: binary search in direction fuel value falls
;
; Quick attempt below.
; (defun bin-search (f n nprev)
;   (let ((prev (f (1- n))) (val (f n)) (next (f (1+ n))))
;     (cond
;       ((< prev val) (bin-search f (floor n 2) n))
;       ((< next val) (bin-search f (+ n (floor (- nprev n) 2)) n))
;       (t val))))

; (print (bin-search (lambda (n) (fuel2 *crabs* n)) 500 1000))
