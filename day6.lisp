(load "~/quicklisp/setup.lisp")
(ql:quickload '("uiop" "str"))

(defparameter *fish*
  (mapcar #'parse-integer (str:split "," (first (uiop:read-file-lines "inputs/day6-1.txt")))))

(defclass school ()
  ((fish
    :initarg :fish
    :initform (error "initial value for slot `fish` required")
    :accessor fish)))

(defmethod newday ((self school))
  (let
    ((babies 0))
    (setf (fish self) (loop for f in (fish self) collect
      (if (< 0 f) (decf f) (progn (incf babies) (setf f 6)))))
    (loop repeat babies do (push 8 (fish self)))))

;
; Part 1
;
(defvar *school* (make-instance 'school :fish (copy-list *fish*)))
(loop repeat 80 do (newday *school*))
(print (list-length (fish *school*)))

;
; Part 2
;
(defclass school2 ()
  ((fish
    :initarg :fish
    :initform (error "initial value for slot `fish` required")
    :accessor fish)))

(defmethod newday ((self school2))
  (let* ((fs (fish self)) (f0 (aref fs 0)))
    (loop for i below 8 do
      (setf (aref fs i) (aref fs (1+ i))))
    (setf (aref fs 8) f0) ; fish babies
    (setf (aref fs 6) (+ f0 (aref fs 6))))) ; fish rejoining the dating pool

(defmethod count-fish ((self school2))
  (reduce '+ (fish self)))

(defun init-school2 (fish)
  (let ((fish-by-day (make-array '(9))))
    (loop for f in fish do (incf (aref fish-by-day f)))
    (make-instance 'school2 :fish fish-by-day)))

(defvar *school2* (init-school2 (copy-list *fish*)))
(print (fish *school2*))
(loop repeat 256 do (newday *school2*))
(print (count-fish *school2*))
