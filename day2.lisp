(load "~/quicklisp/setup.lisp")
(ql:quickload "uiop")
(ql:quickload "str")

; Part 1

(defun parse-2vector (line)
  (let ((instruction (str:words line)))
    (cond
      ((equal (first instruction) "forward") (list 0 (parse-integer (second instruction))))
      ((equal (first instruction) "down") (list (parse-integer (second instruction)) 0))
      ((equal (first instruction) "up") (list (- (parse-integer (second instruction))) 0))
    )
  )
)

(defparameter *movements*
  (mapcar #'parse-2vector (uiop:read-file-lines "inputs/day2-1.txt")))

; Q: Is there any way to do pattern unpacking here?
; A: See part 2.  let expressions allows an equivalent here to make code
;    more understandable.
(defun sum-2vector (vectors)
  (reduce
    (lambda (a b) (list (+ (first a) (first b)) (+ (second a) (second b))))
    vectors
  )
)

(defun xy-product (vec) (* (first vec) (second vec)))

(print (xy-product (sum-2vector *movements*)))

; Part 2

; Oh fffffff.... I made the wrong abstraction in part 1.  In part 1 I parsed the
; instructions into individual vector translations then summed the translations.
; now in part 2, the vector translations the sub undergoes are subject to the
; current "aim" state.  Luckily the instructions to change "aim" ("up" & "down")
; are still orthogonal to the "forward" instruction. This allows us to interpret
; *movements* above in this "aim and move" manner instead and rely on the assumption
; that an instruction is only ever "change aim" or "move", never both... got away
; with that one slightly :S ...
(defun sum-with-aim (instructions)
  (reduce
    (lambda (current instruction)
      (let (
            (aim (first current))
            (pos (second current))
            ; HACK: in this system "aim-change" and "move" are mutually exclusive
            ; this allows us to construct the real x-y vector translation, knowing
            ; that only aim or position are ever changing at once.
            (aim-change (first instruction))
            (move (second instruction))
            )
      (list
        (+ aim aim-change)
        (list
          (+ (first pos) (* aim move))
          (+ (second pos) move)
        )
      )
      ))
    instructions
    :initial-value
    (list 0 (list 0 0))
  )
)

(print (xy-product (second (sum-with-aim *movements*))))

