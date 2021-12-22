(load "~/quicklisp/setup.lisp")
(ql:quickload '("str" "uiop"))

(defparameter *input* (uiop:read-file-lines
                        ; "inputs/day14-example.txt"
                        "inputs/day14.txt"
                        ))

(defparameter *template* (pop *input*))
(pop *input*)

(defparameter *rules*
  (let ((rules (make-hash-table :test 'equal)))
    (loop for rule in (mapcar
                        (lambda (line) (str:split " -> " line))
                        *input*) do
          (setf (gethash (first rule) rules) (second rule)))
    rules))

(defun run-step (template rules)
  (let*
    ((t-chars (coerce template 'list))
     (output (format nil "~a" (first t-chars))))
    (loop for (a b) on t-chars
          while b
          do (setf output
                   (format nil "~a~a~a" output (gethash (coerce (list a b) 'string) rules) b)))
    output))

(defun run-steps (template rules n)
  (let ((output template))
    (loop repeat n do (setf output (run-step output rules)))
    output))

(defun min-non-zero (xs)
  (let ((m nil))
    (loop for x in (coerce xs 'list) do
      (when (> x 0)
        (if m
          (when (< x m) (setf m x))
          (setf m x))))
    m))

(defun part-1 (template rules)
  (let
    ((polymer (coerce (run-steps template rules 10) 'list))
     (char-counts (make-array '(100) :initial-element 0)))
    (loop for p in polymer do (incf (aref char-counts (char-code p))))
    (- (reduce #'max char-counts) (min-non-zero char-counts))))

;
; Part 1
;
(print (part-1 *template* *rules*))

;
; Part 2
;
; Part 1 grows at O(2^n)
;
; Don't try to calculate the polymer, but calculate the number
; of each pair that will exist.
(defun make-pairs (template)
  (let ((pairs (make-hash-table :test 'equal)))
    (loop for (a b) on (coerce template 'list)
          when b
          do (if (gethash (list a b) pairs)
               (incf (gethash (list a b) pairs))
               (setf (gethash (list a b) pairs) 1)))
    pairs))

(defclass polymer ()
  ((pairs
     :initarg :pairs
     :initform (error "slot `pairs` is required")
     :accessor pairs)
   (pstart
     :initarg :pstart
     :initform (error "slot `pstart` is required")
     :accessor pstart)
   (pend
     :initarg :pend
     :initform (error "slot `pend` is required")
     :accessor pend)))

(defun make-polymer (template)
  (let ((t-chars (coerce template 'list)))
    (make-instance
      'polymer
      :pairs (make-pairs template)
      :pstart (char-code (first t-chars))
      :pend (char-code (car (last t-chars))))))

(defmethod counts ((self polymer))
  (let ((cs (make-array '(100) :initial-element 0)))
    (loop for pair
      being the hash-key
      using (hash-value c) of (pairs self) do
        (incf (aref cs (char-code (first pair))) c)
        (incf (aref cs (char-code (second pair))) c))
    (incf (aref cs (pstart self)))
    (incf (aref cs (pend self)))
    (map 'vector (lambda (x) (floor x 2)) cs)))

(defmethod take-step ((self polymer) rules)
  (let ((new-pairs (make-hash-table :test 'equal)))
    (loop for pair
      being the hash-key
      using (hash-value c) of (pairs self) do
        (let ((new-char (first (coerce (gethash (coerce pair 'string) rules) 'list))))
            (if new-char
              (progn (if (gethash (list (first pair) new-char) new-pairs)
                 (incf (gethash (list (first pair) new-char) new-pairs) c)
                 (setf (gethash (list (first pair) new-char) new-pairs) c))
               (if (gethash (list new-char (second pair)) new-pairs)
                 (incf (gethash (list new-char (second pair)) new-pairs) c)
                 (setf (gethash (list new-char (second pair)) new-pairs) c)))
              (if (gethash pair new-pairs)
                (incf (gethash pair new-pairs) c)
                (setf (gethash pair new-pairs) c)))))
    (setf (pairs self) new-pairs)))

(defmethod take-steps ((self polymer) rules n)
  (loop repeat n do
        (take-step self rules)))

(defmethod part-2 ((self polymer) rules n)
  (take-steps self rules n)
  (let ((char-counts (counts self)))
    (- (reduce #'max char-counts) (min-non-zero char-counts))))

(print (part-2 (make-polymer *template*) *rules* 40))
