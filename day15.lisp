(load "~/quicklisp/setup.lisp")
(ql:quickload '("uiop" "fset"))

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
      ; "inputs/day15-example-2.txt"
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
  (loop for j from 0 to jmax do
      (format t "~%~a"
              (loop for i from 0 to imax collect
                    (format nil "~a"
                            (let ((risk (gethash (list i j) risks)))
                              (if risk risk ".")))))))

(defun min2 (a b)
  "like min but can deal with a == nil"
  (if a (min a b) b))

(defun sort-points-by-risk (points risks)
  (sort points (lambda (p1 p2) (< (gethash p1 risks) (gethash p2 risks)))))

(defun next-point (points risks visited)
  (let ((points-sorted (sort-points-by-risk points risks)))
    (loop for point in points-sorted do
      (when (not (fset:contains? visited point)) (return-from next-point point)))
    (first points-sorted)))

(defun dijkstra-step (grid risks point visited imax jmax)
  (let*
    ((risk (gethash point risks))
     (neighbours (adjacent-points point imax jmax)))

     ; update neighbours risk: risk + grid value of neighbour
    (loop for neighbour in neighbours do
      (let*
        ((n-risk-curr (gethash neighbour risks))
         (ni (first  neighbour))
         (nj (second  neighbour))
         (n-risk-new (+ risk (aref grid nj ni))))

        (setf (gethash neighbour risks) (min2 n-risk-curr n-risk-new))))

     (next-point neighbours risks visited)
    ))

(defun run-dijkstra (grid)
  (let*
    ((dims (array-dimensions grid))
     (imax (1- (second dims)))
     (jmax (1- (first dims)))
     (point '(0 0))
     (visited (fset:empty-set))
     (risks (make-hash-table :test 'equal)))
    (setf (gethash point risks) 0)
    ; (loop while (< (hash-table-count risks) (array-total-size grid)) do
    (loop repeat 50 do
        (setf visited (fset:with visited point))
        (setf point (dijkstra-step grid risks point visited imax jmax))
        (display-risks risks imax jmax)
        (format t "~%~%point: ~a~%" point)
    )
    ; )
    (gethash (list imax jmax) risks)))



