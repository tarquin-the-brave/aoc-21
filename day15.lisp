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
      "inputs/day15-example.txt"
      ; "inputs/day15.txt"
      ))))

(defun surr (i imax)
  (cond
    ((= 0 i) (list 1))
    ((= imax i) (list (1- imax)))
    (t (list (1- i) (1+ i)))))

(defun adjacent-points (point imax jmax &optional diagonals)
  (let*
    ((i (first point))
     (j (second point))
     (xs (surr i imax))
     (ys (surr j jmax))
     (surroundings (list)))
    (loop for x in xs do
        (push (list x j) surroundings))
    (loop for y in ys do
        (push (list i y) surroundings))
    (when diagonals
      (loop for x in xs do
        (loop for y in ys do
          (push (list x y) surroundings))))
    surroundings))

(defun display-risks (risks imax jmax)
  (loop for j below jmax do
      (format t "~%"
              (loop for i below imax do
                    (format t "~a"
                            (let ((risk (gethash (list i j) risks)))
                              (if risk risk ".")))))))

(defun run-dijkstra (grid)
  (let*
    ((dims (array-dimensions grid))
     (imax (1- (second dims)))
     (jmax (1- (first dims)))
     (point '(0 0))
     (risks (make-hash-table :test 'equal)))
    (setf (gethash point risks) 0)
    (loop while (< (hash-table-count risks) (array-total-size risks)) do
      (progn
        (display-risks risks imax jmax)
        (let
          ((risk (gethash point risks))
           (neighbours (adjacent-points point imax jmax))

           ; update neighbours risk: risk + grid value of neighbour
           ; move `point` to the neighbour with the lowest risk
        )
      )
    )
    (gethash (list imax jmax) risks)))



