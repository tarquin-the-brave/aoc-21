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

(defun surr (i imax)
  (cond
    ((= 0 i) (list 1))
    ((= imax i) (list (1- imax)))
    (t (list (1- i) (1+ i)))))

(defun adjacent-points (i j imax jmax)
  (let
    ((xs (surr i imax))
     (ys (surr j jmax))
     (surroundings (list)))
    (loop for x in xs do
        (push (list x j) surroundings))
    (loop for y in ys do
        (push (list i y) surroundings))
    surroundings))

(defun get-surroundings (grid i j imax jmax)
  (loop for point in (adjacent-points i j imax jmax) collect
        (aref grid (second point) (first point))))

(defun find-minima (grid)
  (let*
    ((minima (list))
     (dims (array-dimensions grid))
     (jmax (1- (first dims)))
     (imax (1- (second dims))))
    (loop for j from 0 to jmax do
      (loop for i from 0 to imax do
         (let ((is-minimum? (less-that-all (aref grid j i) (get-surroundings grid i j imax jmax))))
           (when is-minimum? (push (list i j) minima)))))
    minima))

(defun less-that-all (height surroundings)
  (let
    ((not-min (block outer
               (loop for s in surroundings do
                  (when (<= s height) (return-from outer t))))))
    (not not-min)))

;
; Part 1
;
(defun total-risk (grid)
  (let ((tot 0))
    (loop for minimum in (find-minima grid) do
      (incf tot
        (1+
          (aref grid (second minimum) (first minimum)))))
    tot))

(print (total-risk *heights*))

;
; Part 2
;
