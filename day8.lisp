(load "~/quicklisp/setup.lisp")
(ql:quickload '("uiop" "str"))

(defun parse-line (line)
  (mapcar #'str:words (str:split " | " line)))

(defparameter *input*
  (mapcar #'parse-line (uiop:read-file-lines "inputs/day8-1.txt")))

(defvar *output-values* (mapcar #'second *input*))

; Digit -> number of segments
; 1 -> 2
; 4 -> 4
; 7 -> 3
; 8 -> 7
(defvar *easy-digit-count*
  (let ((n 0))
    (loop for output in *output-values* do
      (loop for elem in output do
        (when (member (length elem) '(2 3 4 7)) (incf n))))
    n))

(print *easy-digit-count*)

; Decoding:
;
; number of segements -> digits
;
; 2 -> 1
; 3 -> 7
; 4 -> 4
; 5 -> 2 3 5
; 6 -> 0 6 9
; 7 -> 8

; LHS of input lines has all the signals.
; for each segment in the digit we can deduce a rule to find the letter that
; fills it.  We'll call segments i, j, k, l, m, n, o
;
;  iiii
; j    k
; j    k
;  llll
; m    n
; m    n
;  oooo
;
; (i) => segment in 7 and not 1 => difference between 3-letter-code(ikn) and 2-letter-code(kn)
; (i l o) => intersection of all 5 letter codes
;       (l) => intersection of that with 4-letter-code(jkln)
;       (o) => difference ilo '(i l)
; (i j l n o) => intersection of 6 letter codes
; (j n) => difference ijlno ilo
; (n) => intersection jn 3-letter-code(ikn)
; (j) => difference jn 3-letter-code(ikn)
; (m k) => difference 7-letter-code(ijklmno) ijlno
; (k) => intersection mk 3-letter-code(ikn)
; (m) => difference mk 3-letter-code(ikn)
;
; digits -> segments
; 0 -> ijkmno
; 1 -> kn
; 2 -> iklmo
; 3 -> ilkno
; 4 -> jkln
; 5 -> ijlno
; 6 -> ijlmno
; 7 -> ikn
; 8 -> ijklmno
;
; Find the digit values of the line output by matching the sets to the above.
; Match by checking intersection is the same length.
;
