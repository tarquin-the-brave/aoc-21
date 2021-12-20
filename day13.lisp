(load "~/quicklisp/setup.lisp")
(ql:quickload '("str" "uiop"))

(defparameter *points*
  (mapcar (lambda (line) (mapcar #'parse-integer (str:split "," line)))
    (uiop:read-file-lines
      ; "inputs/day13-example-points.txt"
      "inputs/day13-points.txt"
    )))

(defparameter *folds*
  (mapcar (lambda (line) (list (first line) (parse-integer (second line))))
    (mapcar (lambda (line) (str:split "=" line))
      (uiop:read-file-lines
        ; "inputs/day13-example-folds.txt"
        "inputs/day13-folds.txt"
      ))))

(defun do-x-fold (foldAt points)
  (let ((result (list)))
    (loop for point in points do
      (let ((x (first point)) (y (second point)))
        (cond
          ((< x foldAt) (push (list x y) result))
          ((> x foldAt) (push (list (- (* 2 foldAt) x) y) result)))))
    (remove-duplicates result :test 'equal)))

; Part 1 - use REPL, load do-x-fold function.

(defun do-y-fold (foldAt points)
  (let ((result (list)))
    (loop for point in points do
      (let ((x (first point)) (y (second point)))
        (cond
          ((< y foldAt) (push (list x y) result))
          ((> y foldAt) (push (list x (- (* 2 foldAt) y)) result)))))
    (remove-duplicates result :test 'equal)))

(defun do-all-folds (folds points)
  (reduce
    (lambda
      (acc fold)
      (if (equal "x" (first fold))
        (do-x-fold (second fold) acc)
        (do-y-fold (second fold) acc)))
    folds
    :initial-value points))

(defun display-size (points)
  (let ((max-x 0) (max-y 0))
    (loop for point in points do
          (let ((x (first point)) (y (second point)))
            (when (> x max-x) (setf max-x x))
            (when (> y max-y) (setf max-y y))))
    (list (1+ max-y) (1+ max-x))))

(defun make-display (points)
  (let ((display (make-array (display-size points) :initial-element #\.)))
    (loop for point in points do
      (setf (aref display (second point) (first point)) #\#))
    display))

(defun print-display (display)
  (let*
    ((dims (array-dimensions display))
     (imax (second dims))
     (jmax (first dims)))
    (loop for j below jmax do
      (format t "~a~%" (loop for i below imax do (format t "~a" (aref display j i)))))))

(print-display (make-display (do-all-folds *folds* *points*)))
