#|
Ch. 18 : A Few FORMAT Recipes


|#

;; ~$ - print floating point (default round to 2 decimal places)
(format t "~$" pi)

;; ~<n>$ - print floating point (round to n decimal places)
(format t "~5$" pi)

;; ~v$ - print floating point (round to decimal places as specified
;; by next argument)
(format t "~v$" 3 pi)

;; ~#$ - print floating point (round to x decimal places where
;; x = # of args passed in)
(format t "~#$" pi)

;; ~F : another way of printing floating point values but has finer control
;; over how to display
;;
;; takes up to 5 arguments (separated by commas):
;;
;; "~1,2,3,4,5f"
;;
;; 1. total # of number to be printed. defaults to the length of number
;; 2. # of digits to print after decimal. default all
;; 3. # of digits to the left. default: none
;; 4. the special character print when there is display overflow
;; 5. special character - fill character

(format t "~,5f~%" pi)

;; prints integer
(format t "~d~%" 1000000)

;; print integer - adds a comma
(format t "~:d~%" 1000000)

;; prints integer with sign
(format t "~@d~%" 1000000)

;; you can choose to combine all of them.
(format t "~:@d~%" 1000000)

(format t "~x~%" 15) ;; 0xF
(format t "~o~%" 15) ;; 0o17
(format t "~b~%" 15) ;; 0b1111

;; print english representation
(format t "~r" 1234)

;; new-style Roman Numerals
(format t "~@r~%" 1234)

;; old-style Roman Numerals
(format t "~:@r~%" 1234)







;; compliments to Michael Fiano

;;print elements of a list separated by commas
;; =>
;; "1, 2, 3"
(format nil "~{~a~^, ~}" '(1 2 3))

;; same thing, inserting "and" before last element
;; =>
;; "1, 2, and 3"
(format t "~{~a~#[~;, and ~:;, ~]~}" (list 1 2 3))

;; insert new line characters
;; =>
;; "1
;; 2
;; 3"
(format nil "~a~%~a~%~a~%" 1 2 3)

;; ~% : newline == "\n"

;; binary conversion can also take in a sequence
;; =>
;; "(1 10 11 100 101)"
(format t "~b" '(1 2 3 4 5))

;; same thing with hexadecimal, etc
;; =>
;; "(A 14 1E 28 32)"
(format t "~x" '(10 20 30 40 50))

(format t "~c~%" #\a)

(format t "~:c~%" #\ )

(format t "~@c~%" #\a)
(format t "~@c~%" (code-char 97))

;; creating your own directories (compliments of Edi Weitz Common Lisp Recipes Ch.9-6)
(defun twice (stream arg &rest other-args)
  (declare (ignore other-args))
  (format stream "~A~:*~A" arg))

(twice *standard-output* 42)

(format t "~A~/twice/~A~%"
  #\b #\o #\t)
