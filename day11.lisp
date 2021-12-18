(load "~/quicklisp/setup.lisp")
(ql:quickload "uiop")

(defun list-to-2d-array (l)
  (make-array
    (list (length l) (length (first l)))
    :initial-contents l))

(defparameter *input*
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
      ; "inputs/day11-example.txt"
      ; "inputs/day11-test.txt"
      "inputs/day11-1.txt"
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
    (loop for x in xs do
      (loop for y in ys do
        (push (list x y) surroundings)))
    surroundings))

(defclass octopuses ()
  ((grid
    :initarg :grid
    :initform (error "initial value for slot `grid` required")
    :accessor grid)
  (imax
    :initarg :imax
    :initform (error "initial value for slot `imax` required")
    :accessor imax)
  (jmax
    :initarg :jmax
    :initform (error "initial value for slot `jmax` required")
    :accessor jmax)
   (steps
     :initform 0
     :accessor steps)
   (flashes
     :initform 0
     :accessor flashes)))

(defun make-octopuses (grid)
  (let*
    ((dims (array-dimensions grid))
     (jmax (1- (first dims)))
     (imax (1- (second dims))))
    (make-instance 'octopuses :grid grid :imax imax :jmax jmax)))

(defmethod run-flashes ((self octopuses))
  (let ((new-flashes 0))
    (loop for j from 0 to (jmax self) do
      (loop for i from 0 to (imax self) do
        (let ((val (aref (grid self) j i)))
          (when
            (and (< 9 val) (< val 100))
            (progn
              (incf (flashes self))
              (incf new-flashes)
              (setf (aref (grid self) j i) 100)
              (loop for point in (adjacent-points i j (imax self) (jmax self)) do
                (incf (aref (grid self) (second point) (first point)))))))))
    new-flashes))

(defmethod plus1 ((self octopuses))
  (loop for j from 0 to (jmax self) do
    (loop for i from 0 to (imax self) do
      (incf (aref (grid self) j i)))))

(defmethod zero ((self octopuses))
  (loop for j from 0 to (jmax self) do
    (loop for i from 0 to (imax self) do
      (when
        (< 99 (aref (grid self) j i))
        (setf (aref (grid self) j i) 0)))))

(defmethod run-step ((self octopuses))
  (plus1 self)
  (loop while (< 0 (run-flashes self)))
  (zero self)
  (incf (steps self)))

(defmethod all-zero? ((self octopuses))
  (loop for j from 0 to (jmax self) do
    (loop for i from 0 to (imax self) do
      (when
        (/= 0 (aref (grid self) j i))
        (return-from all-zero? nil))))
  t)

(defmethod synchronize ((self octopuses))
  (loop while (not (all-zero? self)) do
    (run-step self)))

(defvar *octopuses* (make-octopuses *input*))

(loop repeat 100 do (run-step *octopuses*))

(print (flashes *octopuses*))

(synchronize *octopuses*)

(print (steps *octopuses*))
