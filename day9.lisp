(load "~/quicklisp/setup.lisp")
(ql:quickload "uiop")

(defun list-to-2d-array (l)
  (make-array
    (list (length l) (length (first l)))
    :initial-contents l))

(defparameter *heights*
  (list-to-2d-array
  (mapcar
    (lambda
      (line)
      (mapcar
        (lambda
          (c)
          (parse-integer (coerce (list c) 'string)))
        (coerce line 'list)))
    (uiop:read-file-lines
      ; "inputs/day9-test.txt"
      "inputs/day9-1.txt"
      ))))

(defun get-surroundings (grid i j imax jmax)
  (let
    ((xs (surr i imax))
     (ys (surr j jmax))
     (surroundings (list)))
    (loop for x in xs do
        (push (aref grid j x) surroundings))
    (loop for y in ys do
        (push (aref grid y i) surroundings))
    surroundings))

(defun surr (i imax)
  (cond
    ((= 0 i) (list 1))
    ((= imax i) (list (1- imax)))
    (t (list (1- i) (1+ i)))))

(defun risk-value (height surroundings)
  (let
    ((not-min (block outer
               (loop for s in surroundings do
                  (when (<= s height) (return-from outer t))))))
    (when (not not-min) (1+ height))))

(defun total-risk (grid)
  (let*
    ((risk 0)
     (dims (array-dimensions grid))
     (jmax (1- (first dims)))
     (imax (1- (second dims))))
    (loop for j from 0 to jmax do
      (loop for i from 0 to imax do
         (let ((r (risk-value (aref grid j i) (get-surroundings grid i j imax jmax))))
           (when r (progn (format t "~d, ~d => ~d~%" i j r) (incf risk r))))))
    risk))

(print (total-risk *heights*))
