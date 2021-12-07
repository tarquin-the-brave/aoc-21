(load "~/quicklisp/setup.lisp")
(ql:quickload "uiop")
(ql:quickload "str")

(defun vertical-line (x ymin ymax)
  (loop for y from ymin to ymax collect (list x y)))

(defun horizontal-line (y xmin xmax)
  (loop for x from xmin to xmax collect (list x y)))

(defun line-points (line)
  (let*
    ((words (str:words line))
    (p0 (mapcar #'parse-integer (str:split "," (first words))))
    (p1 (mapcar #'parse-integer (str:split "," (third words))))
    (x0 (first p0))
    (x1 (first p1))
    (y0 (second p0))
    (y1 (second p1)))
  (cond
    ((= x0 x1) ; verticle line
      (if (< y0 y1) (vertical-line x0 y0 y1) (vertical-line x0 y1 y0)))
    ((= y0 y1) ; horizontal line
      (if (< x0 x1) (horizontal-line y0 x0 x1) (horizontal-line y0 x1 x0))))))

(defun get-points (lines)
  (let ((points (make-hash-table :test 'equal)))
    (loop for line in lines do
      (loop for point in (line-points line) do
        (if (gethash point points) (incf (gethash point points)) (setf (gethash point points) 1))))
    points))

(defparameter *points*
  (get-points (uiop:read-file-lines "inputs/day5-1.txt")))

(defun count-gt-2 (points)
  (loop for v being the hash-value in points count (> v 1)))

(print (count-gt-2 *points*))
