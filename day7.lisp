(load "~/quicklisp/setup.lisp")
(ql:quickload '("uiop" "str"))

(defparameter *crabs*
  (mapcar #'parse-integer (str:split "," (first (uiop:read-file-lines "inputs/day7-1.txt")))))

(defun fuel1 (crabs pos)
  (let ((f 0))
  (loop for crab in crabs do (incf f (abs (- pos crab))))
  f))

(defun fuel2 (crabs pos)
  (let ((f 0))
  (loop for crab in crabs do (incf f (triangle (abs (- pos crab)))))
  f))

(defun triangle (n)
  (if (= n 0) 0 (+ n (triangle (1- n)))))

;
; Part 1
;
(defun bin-search-1 (n nprev)
  (let ((prev (fuel1 *crabs* (1- n))) (val (fuel1 *crabs* n)) (next (fuel1 *crabs* (1+ n))))
    (cond
      ((< prev val) (bin-search-1 (floor n 2) n))
      ((< next val) (bin-search-1 (+ n (+ 1 (floor (- nprev n) 2))) n))
      (t val))))

(print (bin-search-1 500 1000))

;
; Part 2
;
(defun bin-search-2 (n nprev)
  (let ((prev (fuel2 *crabs* (1- n))) (val (fuel2 *crabs* n)) (next (fuel2 *crabs* (1+ n))))
    (cond
      ((< prev val) (bin-search-2 (floor n 2) n))
      ((< next val) (bin-search-2 (+ n (+ 1 (floor (- nprev n) 2))) n))
      (t val))))

(print (bin-search-2 500 1000))

;; TODO: combine bin-search-{1,2} by taking the fuel function as a
;; parameter.  This quick attempt below didn't work.
; (defun bin-search (f n nprev)
;   (let ((prev (f (1- n))) (val (f n)) (next (f (1+ n))))
;     (cond
;       ((< prev val) (bin-search f (floor n 2) n))
;       ((< next val) (bin-search f (+ n (floor (- nprev n) 2)) n))
;       (t val))))
; (print (bin-search (lambda (n) (fuel2 *crabs* n)) 500 1000))

