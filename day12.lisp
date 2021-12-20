(load "~/quicklisp/setup.lisp")
(ql:quickload '("str" "fset" "uiop"))

(defparameter *connections*
  (mapcar (lambda (line) (str:split "-" line))
             (uiop:read-file-lines
               ; "inputs/day12-example-1.txt"
               ; "inputs/day12-example-2.txt"
               ; "inputs/day12-example-3.txt"
               "inputs/day12-1.txt"
               )))

(defun connection-into-graph (graph x y)
  "given a connection from x to y, place entry into graph,
  we leave out connections back to the start or from the end"
  (when
    (and (not (equal x "end")) (not (equal y "start")))
    (if
      (gethash x graph)
      (push y (gethash x graph))
      (setf (gethash x graph) (list y)))))

(defun make-graph (input)
  (let ((graph (make-hash-table :test 'equal)))
    (loop for line in input do
      (let ((a (first line)) (b (second line)))
        (connection-into-graph graph a b)
        (connection-into-graph graph b a)))
    graph))

(defun add-to-end (ps x p)
  "ps.append(x:p)"
  (let ((pc (copy-list p)))
    (push x pc)
    (append ps (list pc))))

(defun not-in (e xs)
  (loop for x in xs do
    (when (equal x e) (return-from not-in nil)))
  t)

(defun count-paths (decider graph)
  (let ((ps (list (list "start"))) (c 0))
    (loop while ps do
      (print (list-length ps))
      (let ((p (pop ps)))
        (loop for x in (gethash (first p) graph) do
          (cond
            ((equal x "end") (incf c))
            ((equal x (string-upcase x)) (setf ps (add-to-end ps x p)))
            ((and (equal x (string-downcase x)) (funcall decider x p))
             (setf ps (add-to-end ps x p)))))))
    c))

;
; Part 1
;
(print (count-paths #'not-in (make-graph *connections*)))

;
; Part 2
;
(defun no-small-duplicates (xs)
  (let ((ls (remove-if (lambda (x) (equal x (string-upcase x))) xs)))
    (=
      (list-length ls)
      (list-length (remove-duplicates ls :test 'equal)))))

(defun part-2-decider (e xs)
  "either e is not in xs, or there are no lowercase duplicates in xs"
  (or
    (not-in e xs)
    (no-small-duplicates xs)))

(print (count-paths #'part-2-decider (make-graph *connections*)))
