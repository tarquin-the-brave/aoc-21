(load "~/quicklisp/setup.lisp")
(ql:quickload "uiop")
(ql:quickload "str")

(defun vertical-line (x ymin ymax)
  (loop for y from ymin to ymax collect (list x y)))

(defun horizontal-line (y xmin xmax)
  (loop for x from xmin to xmax collect (list x y)))

(defun diagonal-line (x0 x1 y0 y1)
  (let (xa xb ya yb)
    (if (< x0 x1) ; first make sure our start point is on the LHS, i.e minimal in x
      (progn (setf xa x0) (setf ya y0) (setf xb x1) (setf yb y1))
      (progn (setf xa x1) (setf ya y1) (setf xb x0) (setf yb y0)))
    (if (< ya yb) ; next determine if the line is pointing up or down
      (upward-horizontal-line xa xb ya yb)
      (downward-horizontal-line xa xb ya))))

(defun upward-horizontal-line (xstart xend ystart yend)
  (loop for x from xstart to xend for y from ystart to yend collect (list x y)))

(defun downward-horizontal-line (xstart xend ystart)
  (let ((y (1+ ystart)))
    (loop for x from xstart to xend collect (list x (decf y)))))

(defun line-points (line &optional diagonals)
  (let*
    ((words (str:words line))
    (p0 (mapcar #'parse-integer (str:split "," (first words))))
    (p1 (mapcar #'parse-integer (str:split "," (third words))))
    (x0 (first p0))
    (x1 (first p1))
    (y0 (second p0))
    (y1 (second p1)))
  (cond
    ((= x0 x1) ; vertical line
      (if (< y0 y1) (vertical-line x0 y0 y1) (vertical-line x0 y1 y0)))
    ((= y0 y1) ; horizontal line
      (if (< x0 x1) (horizontal-line y0 x0 x1) (horizontal-line y0 x1 x0)))
    ((and diagonals (= (abs (- x1 x0)) (abs (- y1 y0))))
      (diagonal-line x0 x1 y0 y1)))))

(defun get-points (lines &optional diagonals)
  (let ((points (make-hash-table :test 'equal)))
    (loop for line in lines do
      (loop for point in (line-points line diagonals) do
        (if (gethash point points) (incf (gethash point points)) (setf (gethash point points) 1))))
    points))

(defun count-gt-2 (points)
  (loop for v being the hash-value in points count (> v 1)))

;
; Part 1
;
(defparameter *points*
  (get-points (uiop:read-file-lines "inputs/day5-1.txt")))

(print (count-gt-2 *points*))

;
; Part 2
;
(defparameter *points2*
  (get-points (uiop:read-file-lines "inputs/day5-1.txt") t))

(print (count-gt-2 *points2*))
